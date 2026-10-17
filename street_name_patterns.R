# Does a letter, digit, or adjacent character combination in a street name
# correlate with more subsequently reported crashes?
#
# Run from the project directory:
#   Rscript street_name_patterns.R
# Or supply an input CSV and an output directory as the first two arguments.
# In PowerShell, if Rscript is not on PATH:
#   & 'C:/Program Files/R/R-4.6.1/bin/Rscript.exe' street_name_patterns.R
# Required packages: data.table, MASS. No packages are installed automatically.
#
# Unit: a standardized ON STREET NAME within one borough (not a road segment).
# Population: streets recorded in 2021-2023; outcome: crashes in 2024-2025,
# including zero later crashes. Streets never recorded in the baseline are
# outside the study population. Missing borough/name records are excluded.
# A count association is not exposure-adjusted risk or evidence of causation.
#
# Source: City of New York, Motor Vehicle Collisions - Crashes.
# https://catalog.data.gov/dataset/motor-vehicle-collisions-crashes
# MASS model: https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/glm.nb.html
# Holm adjustment: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html

MAX_PATTERN_LENGTH <- 2L  # Increase to 3 to include triples; more tests take longer.
MIN_STREETS <- 50L       # Minimum eligible streets both with and without a pattern.
BASELINE_START <- "2021-01-01"
OUTCOME_START <- "2024-01-01"
OUTCOME_END <- "2025-12-31"

standardize_name <- function(x) {
  x <- toupper(trimws(x))
  x <- gsub("[^A-Z0-9 ]", " ", x)
  # Merge equivalent ordinal forms: 42ND STREET -> 42 ST.
  x <- gsub("\\b([0-9]+)(ST|ND|RD|TH)\\b", "\\1", x, perl = TRUE)
  replacements <- c(
    STREET = "ST", AVENUE = "AVE", BOULEVARD = "BLVD", ROAD = "RD",
    DRIVE = "DR", PLACE = "PL", LANE = "LN", COURT = "CT",
    PARKWAY = "PKWY", HIGHWAY = "HWY", EXPRESSWAY = "EXPY", TERRACE = "TER"
  )
  for (word in names(replacements)) {
    x <- gsub(paste0("\\b", word, "\\b"), replacements[[word]], x, perl = TRUE)
  }
  trimws(gsub("\\s+", " ", x))
}

extract_patterns <- function(name, max_length = MAX_PATTERN_LENGTH) {
  words <- strsplit(name, " +")[[1L]]
  words <- words[nzchar(words)]
  unique(unlist(lapply(words, function(word) {
    unlist(lapply(seq_len(min(max_length, nchar(word))), function(k) {
      starts <- seq_len(nchar(word) - k + 1L)
      substring(word, starts, starts + k - 1L)
    }), use.names = FALSE)
  }), use.names = FALSE))
}

build_street_counts <- function(crashes) {
  # Group across the full window before selecting baseline-eligible streets.
  streets <- crashes[, .(
    baseline_crashes = sum(date < data.table::as.IDate(OUTCOME_START)),
    later_crashes = sum(date >= data.table::as.IDate(OUTCOME_START))
  ), by = .(borough, street)]
  streets <- streets[baseline_crashes > 0L]

  road_codes <- "ST|AVE|BLVD|RD|DR|PL|LN|CT|PKWY|HWY|EXPY|TER|WAY"
  suffix_pattern <- paste0("\\b(", road_codes, ")$")
  streets[, road_type := data.table::fifelse(
    grepl(suffix_pattern, street, perl = TRUE),
    sub(paste0("^.*\\b(", road_codes, ")$"), "\\1", street, perl = TRUE),
    data.table::fifelse(grepl("^AVE\\b", street), "AVE", "OTHER")
  )]
  # Exclude generic road types from the character scan; keep them as controls.
  streets[, name_core := sub(
    paste0("\\s+(", road_codes, ")$"), "", street, perl = TRUE
  )]
  streets[, name_core := trimws(sub("^AVE\\s+", "", name_core))]
  streets <- streets[nzchar(name_core)]
  streets[, `:=`(
    street_id = .I,
    name_length = nchar(gsub(" ", "", name_core, fixed = TRUE)),
    numbered = as.integer(grepl("[0-9]", name_core)),
    borough = factor(borough),
    road_type = factor(road_type)
  )]
  streets
}

test_pattern <- function(pat, streets, membership, model_formula) {
  ids <- membership[pattern == pat, street_id]
  d <- data.table::copy(streets)
  d[, has_pattern := as.integer(street_id %in% ids)]
  answer <- data.table::data.table(
    pattern = pat,
    streets_with = sum(d$has_pattern == 1L),
    streets_without = sum(d$has_pattern == 0L),
    mean_crashes_with = mean(d$later_crashes[d$has_pattern == 1L]),
    mean_crashes_without = mean(d$later_crashes[d$has_pattern == 0L]),
    count_ratio = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
    p_value = NA_real_, status = "fit_failed", model_warnings = ""
  )
  model_warnings <- character()
  fit <- tryCatch(
    withCallingHandlers(
      MASS::glm.nb(model_formula, data = d, control = stats::glm.control(maxit = 100)),
      warning = function(w) {
        model_warnings <<- c(model_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      model_warnings <<- c(model_warnings, conditionMessage(e))
      NULL
    }
  )
  answer[, model_warnings := paste(unique(model_warnings), collapse = " | ")]
  if (is.null(fit)) return(answer)
  # Do not report successful inference for a model that issued warnings.
  if (!isTRUE(fit$converged) || !is.null(fit$th.warn) || length(model_warnings)) {
    answer[, status := "fit_warning_or_nonconvergence"]
    return(answer)
  }
  coefficients <- stats::coef(summary(fit))
  if (!"has_pattern" %in% rownames(coefficients)) {
    answer[, status := "pattern_not_estimable"]
    return(answer)
  }
  beta <- coefficients["has_pattern", "Estimate"]
  se <- coefficients["has_pattern", "Std. Error"]
  if (!is.finite(beta) || !is.finite(se) || se <= 0) return(answer)
  answer[, `:=`(
    count_ratio = exp(beta),
    ci_lower = exp(beta - stats::qnorm(0.975) * se),
    ci_upper = exp(beta + stats::qnorm(0.975) * se),
    p_value = 2 * stats::pnorm(-abs(beta / se)), status = "ok"
  )]
  answer
}

plot_patterns <- function(results, output_file) {
  shown <- data.table::copy(results[status == "ok" & is.finite(count_ratio) &
    is.finite(ci_lower) & is.finite(ci_upper) & ci_lower > 0])
  grDevices::png(output_file, width = 1700, height = 1050, res = 170)
  on.exit(grDevices::dev.off())
  if (!nrow(shown)) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No models produced usable estimates.")
    return(invisible(NULL))
  }
  data.table::setorder(shown, p_adjusted, -count_ratio, na.last = TRUE)
  shown <- head(shown, 15L)
  shown <- shown[order(count_ratio)]
  colors <- ifelse(shown$higher_count_association, "#B84A32", "#477487")
  graphics::par(mar = c(5.5, 5, 5, 2))
  graphics::plot(
    shown$count_ratio, seq_len(nrow(shown)), log = "x", pch = 19,
    col = colors, yaxt = "n", ylab = "Street-name pattern",
    xlab = "Adjusted ratio of recorded crash counts (log scale)",
    xlim = range(c(1, shown$ci_lower, shown$ci_upper)),
    main = "Street-name patterns and subsequent crash counts"
  )
  graphics::segments(shown$ci_lower, seq_len(nrow(shown)),
    shown$ci_upper, seq_len(nrow(shown)), col = colors, lwd = 2)
  graphics::axis(2, at = seq_len(nrow(shown)), labels = shown$pattern, las = 1)
  graphics::abline(v = 1, lty = 2, col = "gray50")
  graphics::mtext("2024-2025 outcomes; streets recorded in 2021-2023", side = 3, line = 0.4)
  graphics::mtext("Intervals: nominal 95%. Rust: higher count, Holm p < 0.05.",
    side = 1, line = 4, cex = 0.72)
}

main <- function() {
  needed <- c("data.table", "MASS")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))
  # Attaching data.table enables its standard data.table expressions in functions.
  suppressPackageStartupMessages(library(data.table))
  args <- commandArgs(trailingOnly = TRUE)
  script_arg <- grep("^--file=", commandArgs(), value = TRUE)
  script_root <- if (length(script_arg)) {
    dirname(normalizePath(sub("^--file=", "", script_arg[1L]), mustWork = TRUE))
  } else getwd()
  csv_path <- if (length(args) >= 1L) args[1L] else file.path(script_root, "data", "motor_vehicle_collisions.csv")
  out_dir <- if (length(args) >= 2L) args[2L] else file.path(script_root, "outputs", "street_name_patterns")
  if (!file.exists(csv_path)) stop("CSV not found: ", csv_path)
  stopifnot(MAX_PATTERN_LENGTH >= 1L, MAX_PATTERN_LENGTH <= 5L, MIN_STREETS >= 2L)

  message("Reading crash date, borough, street name, and collision ID...")
  crashes <- data.table::fread(file = csv_path,
    select = c("COLLISION_ID", "CRASH DATE", "BOROUGH", "ON STREET NAME"),
    colClasses = "character", na.strings = c("", "NA"))
  data.table::setnames(crashes,
    c("COLLISION_ID", "CRASH DATE", "BOROUGH", "ON STREET NAME"),
    c("collision_id", "date", "borough", "street"))
  audit <- list(input_records = nrow(crashes))
  if (anyNA(crashes$collision_id)) stop("Missing collision IDs; resolve before deduplicating.")
  crashes <- unique(crashes, by = "collision_id")
  audit$duplicate_records_removed <- audit$input_records - nrow(crashes)
  crashes[, date := data.table::as.IDate(date, format = "%m/%d/%Y")]
  audit$unparseable_dates <- sum(is.na(crashes$date))
  crashes <- crashes[!is.na(date) & date >= data.table::as.IDate(BASELINE_START) &
    date <= data.table::as.IDate(OUTCOME_END)]
  audit$records_in_window <- nrow(crashes)
  crashes[, `:=`(street = standardize_name(street), borough = toupper(trimws(borough)))]
  audit$missing_or_unrecognized_borough <- sum(!crashes$borough %chin%
    c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND"))
  usable_name <- !is.na(crashes$street) & nzchar(crashes$street) &
    !grepl("^(UNKNOWN|UNSPECIFIED|UNNAMED|NONE|N A)( |$)", crashes$street)
  audit$missing_or_unusable_name <- sum(!usable_name)
  crashes <- crashes[usable_name & borough %chin%
    c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND")]
  audit$usable_records_in_window <- nrow(crashes)
  streets <- build_street_counts(crashes)
  if (!nrow(streets)) stop("No eligible streets in the baseline period.")
  audit$eligible_streets <- nrow(streets)
  audit$eligible_streets_with_zero_later_crashes <- sum(streets$later_crashes == 0)
  audit$baseline_crashes_on_eligible_streets <- sum(streets$baseline_crashes)
  audit$later_crashes_on_eligible_streets <- sum(streets$later_crashes)
  # Reconcile the outcome against the original retained crash events.
  later <- crashes[date >= data.table::as.IDate(OUTCOME_START)]
  expected_later <- nrow(later[streets, on = .(borough, street), nomatch = 0])
  stopifnot(sum(streets$later_crashes) == expected_later,
    !anyDuplicated(streets[, .(borough, street)]), all(streets$baseline_crashes > 0))

  membership <- data.table::rbindlist(lapply(seq_len(nrow(streets)), function(i) {
    data.table::data.table(street_id = streets$street_id[i],
      pattern = extract_patterns(streets$name_core[i]))
  }))
  stopifnot(!anyDuplicated(membership))
  candidates <- membership[, .(streets_with = uniqueN(street_id)), by = pattern]
  candidates[, streets_without := nrow(streets) - streets_with]
  candidates <- candidates[streets_with >= MIN_STREETS & streets_without >= MIN_STREETS]
  if (!nrow(candidates)) stop("No patterns meet MIN_STREETS; lower that setting.")

  controls <- character()
  if (uniqueN(streets$borough) > 1L) controls <- c(controls, "borough")
  if (uniqueN(streets$road_type) > 1L) controls <- c(controls, "road_type")
  if (uniqueN(streets$name_length) > 1L) controls <- c(controls, "log1p(name_length)")
  if (uniqueN(streets$numbered) > 1L) controls <- c(controls, "numbered")
  model_formula <- stats::reformulate(c("has_pattern", controls), response = "later_crashes")
  message("Fitting ", nrow(candidates), " patterns across ", nrow(streets), " eligible streets...")
  rows <- vector("list", nrow(candidates))
  for (i in seq_len(nrow(candidates))) {
    rows[[i]] <- test_pattern(candidates$pattern[i], streets, membership, model_formula)
    if (i %% 25L == 0L || i == nrow(candidates)) message("Models: ", i, "/", nrow(candidates))
  }
  results <- data.table::rbindlist(rows)
  results[, p_adjusted := stats::p.adjust(p_value, method = "holm", n = .N)]
  results[, higher_count_association := status == "ok" & !is.na(p_adjusted) &
    p_adjusted < 0.05 & count_ratio > 1]
  data.table::setorder(results, p_adjusted, -count_ratio, na.last = TRUE)
  audit$patterns_tested <- nrow(results)
  audit$successful_models <- sum(results$status == "ok")
  audit$higher_count_associations <- sum(results$higher_count_association)

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(streets, file.path(out_dir, "street_counts.csv"))
  data.table::fwrite(results, file.path(out_dir, "pattern_results.csv"))
  data.table::fwrite(results[higher_count_association == TRUE], file.path(out_dir, "higher_count_patterns.csv"))
  data.table::fwrite(data.table::data.table(metric = names(audit), value = unlist(audit)),
    file.path(out_dir, "data_audit.csv"))
  plot_patterns(results, file.path(out_dir, "pattern_associations.png"))
  notes <- c(
    "STREET-NAME PATTERNS: EXPLORATORY RESULTS",
    paste("Input:", normalizePath(csv_path, winslash = "/")),
    paste("Baseline:", BASELINE_START, "through 2023-12-31; outcome:", OUTCOME_START, "through", OUTCOME_END),
    paste("Model:", paste(deparse(model_formula), collapse = " ")),
    paste("Eligible streets:", nrow(streets), "| tested patterns:", nrow(results)),
    paste("Successful models:", audit$successful_models, "| higher-count associations:", audit$higher_count_associations),
    "",
    "Files: street_counts.csv (one borough/name unit per row); pattern_results.csv",
    "(all eligible patterns, including failed fits); higher_count_patterns.csv",
    "(successful positive associations with Holm-adjusted p < 0.05); data_audit.csv;",
    "pattern_associations.png (up to 15 patterns ranked by adjusted p, then effect).",
    "",
    "count_ratio is an adjusted count ratio, not a per-trip or per-mile risk ratio.",
    "Confidence intervals are nominal 95%, not multiplicity-adjusted intervals.",
    "The character scan excludes generic road-type endings and normalizes ordinals.",
    "Patterns are adjacent substrings within words; they do not cross spaces.",
    "Each pattern counts at most once per street; patterns overlap across streets.",
    "Missing/invalid borough and street names are excluded, possibly nonrandomly.",
    "A borough/name can combine disconnected roads; aliases may remain unmerged.",
    "Street length, traffic volume, spatial dependence and neighborhood confounding",
    "are unmeasured. These are exploratory associations, not causal findings.",
    "Streets absent from baseline are excluded; this is not a census of NYC roads.",
    "No test result should be described as proving that a character causes crashes.",
    "",
    "Source: City of New York, Motor Vehicle Collisions - Crashes.",
    "https://catalog.data.gov/dataset/motor-vehicle-collisions-crashes",
    "Include this AI conversation in citations if following the challenge rules."
  )
  writeLines(notes, file.path(out_dir, "analysis_notes.txt"))
  capture.output(sessionInfo(), file = file.path(out_dir, "session_info.txt"))
  print(results[, .(pattern, streets_with, mean_crashes_with, mean_crashes_without,
    count_ratio, ci_lower, ci_upper, p_adjusted, status)][1:min(20L, .N)])
  message("Saved outputs to: ", normalizePath(out_dir, winslash = "/"))
  invisible(list(streets = streets, results = results, audit = audit))
}

if (sys.nframe() == 0L) main()
