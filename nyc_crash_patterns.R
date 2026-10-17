# NYC STREET-NAME PATTERNS AND RECORDED CRASH COUNTS
# Version 1.0 | 2026-10-17
#
# Scope: letters/digits in ON STREET NAME, not license plates (not in this table).
# No NYC results are embedded. Supply the actual crash CSV or a data.frame.
#
# Install once: install.packages(c("data.table", "sandwich"))
# source("nyc_crash_patterns.R")
# run_self_tests()
#
# EXAMPLE ONLY: choose three complete periods appropriate to YOUR extract.
# ans <- analyze_crash_patterns(
#   "Motor_Vehicle_Collisions_-_Crashes.csv",
#   boundaries = c("2021-01-01", "2023-01-01", "2024-01-01", "2025-01-01"),
#   complete_extract = TRUE,
#   out_dir = "nyc_pattern_results"
# )
# head(ans$results, 20)
# ans$results[ans$results$replicated_more, ]
#
# Boundaries define [baseline start, discovery start, validation start, end).
# All intervals include the first date and EXCLUDE the next boundary.
# complete_extract=TRUE asserts that the file contains all available crash
# records for the selected dates, without borough/street/injury/sample filters.
# It does not assert that police records capture all real-world collisions.
#
# WHAT IS ESTIMATED
# One row per normalized borough + street name, established in baseline.
# Each feature is fitted separately to counts in each later period, adjusting
# for borough, road-name suffix, and name length. The OPTIONAL predeclared
# sensitivity adjust_baseline_history=TRUE also adjusts baseline crash count.
# A log-link Poisson mean model uses HC3 sandwich standard errors; its inference
# does NOT require Poisson equality of mean and variance. It DOES require a
# suitable conditional mean model and approximately independent street units.
# Default results are adjusted count associations in the baseline cohort.
# With adjust_baseline_history=TRUE they measure association beyond past counts,
# a stricter question that may absorb persistent name-count relationships.
# No traffic-volume/road-length denominator exists here: these are count ratios,
# NOT per-trip risk ratios, causal effects, or results for every NYC street.
#
# PATTERNS
# CHAR:X: one character, GRAM2:XY / GRAM3:XYZ: contiguous within-word substrings;
# NUMBER:123: exact all-digit word of any length. No n-gram crosses a word break.
# Each pattern is binary per street, even if its character repeats in the name.
# Common road-type suffixes are standardized and excluded from pattern text.
# STREET and AVENUE remain different street identities and model controls.
# Arbitrary noncontiguous combinations and license-plate strings are not tested.
#
# REPLICATION AND MULTIPLICITY
# For each eligible pattern k, test H0: beta_k <= 0 in EACH later period.
# p_rep = max(p_discovery, p_validation) is an intersection-union p-value for
# the claim that the association is positive in BOTH periods. This construction
# does not require independence between the two period-specific tests.
# Apply Benjamini-Yekutieli to ALL eligible patterns' p_rep, including failures
# as p=1. BY accommodates dependence among overlapping patterns if individual
# model p-values are valid. It cannot repair confounding or spatial dependence.
# The eligibility rules use baseline names ONLY, never later crash outcomes.
# Confidence intervals are ordinary pointwise 95% intervals, NOT simultaneous.
#
# Important limits: spelling/alias errors; different streets sharing a name;
# missing-location selection; geographic/traffic/road-length confounding;
# dependence across intersecting roads; reporting changes; renaming/closures;
# baseline-selected cohort. Review audits and diagnostics before interpretation.
# Do not keep modifying dates/features/thresholds after inspecting p-values.
#
# REFERENCES (official primary documentation)
# NYC: https://data.cityofnewyork.us/Public-Safety/Motor-Vehicle-Collisions-Crashes/h9gi-nx95
# GLM: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html
# HC3: https://search.r-project.org/CRAN/refmans/sandwich/html/vcovHC.html
# BY:  https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html
#
# VERIFICATION STATUS AT DELIVERY
# R was unavailable in the authoring environment. This file has NOT been
# executed in R there. Self-tests below are supplied for an R installation.
# Independent Python numerical checks of the statistical construction are
# separate from, and do not substitute for, running this R implementation.

require_crash_packages <- function() {
  pkgs <- c("data.table", "sandwich")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install required packages: install.packages(c(",
                           paste(sprintf('"%s"', missing), collapse = ", "), "))")
  # data.table expressions use its exported helpers below.
  suppressPackageStartupMessages(library(data.table))
}

standard_header <- function(x) {
  x <- tolower(trimws(x))
  gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", x))
}

parse_crash_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  x <- trimws(as.character(x))
  # Parse distinct strings only, avoiding millions of identical conversions.
  u <- unique(x)
  ans <- as.Date(rep(NA_character_, length(u)))
  iso <- !is.na(u) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}($|[T ])", u)
  us <- !is.na(u) & grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", u)
  ans[iso] <- as.Date(substr(u[iso], 1, 10), format = "%Y-%m-%d")
  ans[us] <- as.Date(u[us], format = "%m/%d/%Y")
  ans[match(x, u)]
}

normalize_street <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[x %in% c("", "NA", "N/A", "NULL", "UNKNOWN", "UNSPECIFIED",
             "UNAVAILABLE", "NOT AVAILABLE")] <- NA_character_
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = " ")
  # Preserve tokens rather than fabricating n-grams across punctuation.
  x <- gsub("['\u2019]", "", x)
  x <- gsub("[^A-Z0-9]+", " ", x)
  x <- trimws(gsub(" +", " ", x))
  x <- gsub("\\b([0-9]+)(ST|ND|RD|TH)\\b", "\\1", x, perl = TRUE)
  aliases <- c(ST="STREET", STR="STREET", STREET="STREET",
    AV="AVENUE", AVE="AVENUE", AVENUE="AVENUE", RD="ROAD", ROAD="ROAD",
    BLVD="BOULEVARD", BOULEVARD="BOULEVARD", DR="DRIVE", DRIVE="DRIVE",
    LN="LANE", LANE="LANE", PKWY="PARKWAY", PARKWAY="PARKWAY",
    PL="PLACE", PLACE="PLACE", CT="COURT", COURT="COURT",
    TER="TERRACE", TERRACE="TERRACE", HWY="HIGHWAY", HIGHWAY="HIGHWAY",
    EXPY="EXPRESSWAY", EXPRESSWAY="EXPRESSWAY", TPKE="TURNPIKE", TURNPIKE="TURNPIKE")
  last <- sub("^.* ", "", x)
  road_type <- unname(aliases[last])
  known <- !is.na(road_type)
  core <- x
  core[known] <- trimws(sub(" ?[^ ]+$", "", x[known]))
  x[known] <- trimws(paste(core[known], road_type[known]))
  # NYC's "AVENUE A" convention: AVENUE is a type, A is the name.
  prefix <- !is.na(x) & grepl("^(AV|AVE|AVENUE) [A-Z0-9]+$", x)
  core[prefix] <- sub("^[^ ]+ ", "", x[prefix])
  road_type[prefix] <- "AVENUE"
  x[prefix] <- paste("AVENUE", core[prefix])
  road_type[is.na(road_type)] <- "NONE"
  x[is.na(core) | !nzchar(core)] <- NA_character_
  core[is.na(x)] <- NA_character_
  data.frame(street = x, core = core, road_type = road_type,
             stringsAsFactors = FALSE)
}

name_features <- function(core, max_ngram = 3L) {
  if (is.na(core) || !nzchar(core)) return(character())
  words <- strsplit(core, " +")[[1L]]
  out <- character()
  for (word in words) {
    len <- nchar(word)
    for (k in seq_len(min(max_ngram, len))) {
      starts <- seq_len(len - k + 1L)
      label <- if (k == 1L) "CHAR:" else paste0("GRAM", k, ":")
      out <- c(out, paste0(label, substring(word, starts, starts + k - 1L)))
    }
    if (grepl("^[0-9]+$", word)) out <- c(out, paste0("NUMBER:", word))
  }
  unique(out)
}

read_crashes <- function(input) {
  required <- c("collision_id", "crash_date", "borough", "on_street_name")
  if (is.character(input) && length(input) == 1L) {
    if (!file.exists(input)) stop("CSV does not exist: ", input)
    hdr <- names(data.table::fread(file = input, nrows = 0L, check.names = FALSE))
    clean <- standard_header(hdr)
    if (anyDuplicated(clean)) stop("Duplicate column names after normalization.")
    if (!all(required %in% clean)) stop("Missing columns: ",
      paste(setdiff(required, clean), collapse = ", "))
    chosen <- hdr[match(required, clean)]
    dt <- data.table::fread(file = input, select = chosen, colClasses = "character",
                           na.strings = c("", "NA", "NULL"), check.names = FALSE)
    data.table::setnames(dt, names(dt), standard_header(names(dt)))
  } else if (is.data.frame(input)) {
    dt <- data.table::as.data.table(data.table::copy(input))
    clean <- standard_header(names(dt))
    if (anyDuplicated(clean)) stop("Duplicate column names after normalization.")
    data.table::setnames(dt, clean)
    if (!all(required %in% names(dt))) stop("Missing required analysis columns.")
    dt <- dt[, required, with = FALSE]
    dt[, (required) := lapply(.SD, as.character), .SDcols = required]
  } else stop("input must be a CSV filename or a data.frame.")
  dt[, source_row := .I]
  dt
}

clean_crashes <- function(dt) {
  dt <- data.table::copy(dt)
  n_input <- nrow(dt)
  dt[, collision_id := trimws(collision_id)]
  valid_id <- !is.na(dt$collision_id) & grepl("^[0-9]+$", dt$collision_id)
  invalid_id <- sum(!valid_id)
  dt <- dt[valid_id]
  # Resolve identical analysis records only. Never arbitrarily choose between
  # conflicting dates/locations for the same primary key.
  fields <- c("collision_id", "crash_date", "borough", "on_street_name")
  dedup <- unique(dt, by = fields)
  conflicts <- dedup[, .N, by = collision_id][N > 1L, collision_id]
  if (length(conflicts)) stop("Conflicting analysis records for COLLISION_ID; resolve first. Examples: ",
                             paste(head(conflicts, 10L), collapse = ", "))
  removed_duplicates <- nrow(dt) - nrow(dedup)
  dt <- dedup
  dt[, date := parse_crash_date(crash_date)]
  dt[, borough := toupper(trimws(borough))]
  valid_boroughs <- c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND")
  dt[!borough %in% valid_boroughs, borough := NA_character_]
  # Normalize only distinct names for scalability.
  raw_names <- unique(dt$on_street_name)
  mapping <- data.table::as.data.table(normalize_street(raw_names))
  mapping[, raw_name := raw_names]
  idx <- match(dt$on_street_name, mapping$raw_name)
  dt[, `:=`(street = mapping$street[idx], core = mapping$core[idx],
            road_type = mapping$road_type[idx])]
  dt[, usable := !is.na(date) & !is.na(borough) & !is.na(street)]
  audit <- data.frame(metric = c("input_rows", "invalid_or_missing_id_rows",
    "duplicate_analysis_records_removed", "unique_id_rows",
    "invalid_or_missing_date", "invalid_or_missing_borough",
    "missing_or_unusable_street", "usable_rows"),
    value = c(n_input, invalid_id, removed_duplicates, nrow(dt),
              sum(is.na(dt$date)), sum(is.na(dt$borough)),
              sum(is.na(dt$street)), sum(dt$usable)))
  # Missingness reasons overlap; never add these counts as if exclusive.
  list(data = dt, audit = audit, normalization = mapping)
}

prepare_cohort <- function(clean, boundaries, max_ngram = 3L) {
  b <- as.Date(boundaries)
  if (length(b) != 4L || anyNA(b) || any(diff(b) <= 0))
    stop("Provide four strictly increasing ISO dates.")
  periods <- c("baseline", "discovery", "validation")
  dt <- data.table::copy(clean$data)
  dt[, period := NA_character_]
  for (j in seq_len(3L)) dt[!is.na(date) & date >= b[j] & date < b[j+1L], period := periods[j]]
  missingness <- dt[!is.na(period), .(all_id_records = .N,
    usable_records = sum(usable), missing_street = sum(is.na(street)),
    missing_borough = sum(is.na(borough))), by = .(period, borough)]
  # Detect empty dates using all unique-ID records, including missing streets.
  calendar <- data.table::data.table(date = seq(b[1L], b[4L]-1L, by = "day"))
  daily <- dt[!is.na(period), .(records = .N), by = date]
  calendar <- merge(calendar, daily, by = "date", all.x = TRUE, sort = TRUE)
  calendar[is.na(records), records := 0L]
  use <- dt[usable & !is.na(period)]
  if (!nrow(use)) stop("No usable crashes in the requested dates.")
  street_counts <- use[, .(
    baseline = sum(period == "baseline"),
    discovery = sum(period == "discovery"),
    validation = sum(period == "validation")),
    by = .(borough, street, core, road_type)]
  data.table::setorder(street_counts, borough, street)
  street_counts[, street_id := .I]
  # Only baseline membership defines the cohort. Later zeros are preserved.
  cohort <- street_counts[baseline > 0L]
  if (!nrow(cohort)) stop("No baseline streets.")
  all_features <- lapply(street_counts$core, name_features, max_ngram = max_ngram)
  links <- data.table::data.table(
    street_id = rep(street_counts$street_id, lengths(all_features)),
    feature = unlist(all_features, use.names = FALSE))
  baseline_links <- links[street_id %in% cohort$street_id]
  # Descriptive count shares include ALL usable streets in the analysis window.
  linked <- merge(links, street_counts[, .(street_id, baseline, discovery, validation)],
                  by = "street_id", sort = FALSE, allow.cartesian = TRUE)
  descriptive <- linked[, .(streets_observed = .N,
    baseline_crashes = sum(baseline), discovery_crashes = sum(discovery),
    validation_crashes = sum(validation)), by = feature]
  descriptive[, total_crashes := baseline_crashes + discovery_crashes + validation_crashes]
  descriptive[, share_of_usable_crashes := total_crashes / nrow(use)]
  data.table::setorder(descriptive, -total_crashes, feature)
  coverage <- use[, .(usable_crashes = .N,
    crashes_on_baseline_streets = sum(paste(borough, street, sep = "|") %in%
       paste(cohort$borough, cohort$street, sep = "|"))), by = period]
  coverage[, cohort_share := crashes_on_baseline_streets / usable_crashes]
  list(cohort = cohort, links = baseline_links, descriptive = descriptive,
       street_counts = street_counts, missingness = missingness,
       daily_counts = calendar, coverage = coverage, boundaries = b)
}

make_controls <- function(cohort, rare_type_min = 30L, adjust_baseline_history = FALSE) {
  d <- as.data.frame(cohort)
  d$name_length <- nchar(gsub(" ", "", d$core, fixed = TRUE))
  d$log_baseline <- log1p(d$baseline)
  d$borough <- factor(d$borough)
  freq <- table(d$road_type)
  d$road_type <- factor(ifelse(freq[d$road_type] < rare_type_min, "OTHER", d$road_type))
  terms <- character()
  if (nlevels(d$borough) > 1L) terms <- c(terms, "borough")
  if (nlevels(d$road_type) > 1L) terms <- c(terms, "road_type")
  # Natural splines permit curvature; knots/basis are fixed by baseline data.
  continuous <- c("name_length", if (adjust_baseline_history) "log_baseline")
  for (v in continuous) {
    nu <- length(unique(d[[v]]))
    if (nu > 1L) {
      if (nu >= 5L) {
        # Use quantiles of DISTINCT values so many low-count streets do not
        # put several knots at the same boundary.
        knots <- as.numeric(quantile(sort(unique(d[[v]])), c(1/3, 2/3), names = FALSE))
        basis <- splines::ns(d[[v]], knots = knots, Boundary.knots = range(d[[v]]))
        cn <- paste0(v, "_s", seq_len(ncol(basis)))
        d[cn] <- basis
        terms <- c(terms, cn)
      } else terms <- c(terms, v)
    }
  }
  # Remove any exact linear dependencies among controls deterministically.
  control_formula <- if (length(terms)) reformulate(terms) else ~ 1
  mm <- model.matrix(control_formula, data = d)
  qrmm <- qr(mm)
  keep <- sort(qrmm$pivot[seq_len(qrmm$rank)])
  mm <- mm[, keep, drop = FALSE]
  list(data = d, X = mm, control_columns = colnames(mm))
}

fit_pattern <- function(X, y, feature_present) {
  # Feature is appended last; all other columns are nuisance controls.
  design <- cbind(X, feature = as.integer(feature_present))
  bad <- function(reason) list(status = reason, beta = NA_real_, se = NA_real_,
    count_ratio = NA_real_, ci_low = NA_real_, ci_high = NA_real_, p_more = 1,
    dispersion = NA_real_, max_leverage = NA_real_, max_abs_dfbeta = NA_real_)
  if (qr(design)$rank < ncol(design)) return(bad("feature_collinear_with_controls"))
  if (nrow(design) <= ncol(design) + 10L) return(bad("insufficient_residual_degrees_of_freedom"))
  if (sum(y[feature_present]) == 0L || sum(y[!feature_present]) == 0L)
    return(bad("zero_events_in_one_group"))
  warnings_seen <- character()
  attempt <- tryCatch(withCallingHandlers({
    # Explicit design makes spline/control construction identical in both periods.
    fit <- glm(y ~ design - 1, family = poisson(link = "log"),
               control = glm.control(maxit = 100L), x = TRUE, y = TRUE)
    if (!isTRUE(fit$converged) || any(!is.finite(coef(fit)))) stop("nonconverged_fit")
    h <- hatvalues(fit)
    if (any(!is.finite(h)) || max(h) >= 0.999) stop("extreme_leverage")
    V <- sandwich::vcovHC(fit, type = "HC3")
    k <- length(coef(fit))
    beta <- unname(coef(fit)[k])
    se <- sqrt(V[k, k])
    if (!is.finite(se) || se <= 0) stop("invalid_robust_standard_error")
    ratio <- exp(beta)
    lo <- exp(beta - qnorm(0.975) * se)
    hi <- exp(beta + qnorm(0.975) * se)
    if (any(!is.finite(c(ratio, lo, hi)))) stop("unbounded_effect_interval")
    # Approximate GLM influence diagnostic, separate from the robust SE.
    influence <- suppressWarnings(dfbeta(fit))[, k]
    list(status = "ok", beta = beta, se = se, count_ratio = ratio,
      ci_low = lo, ci_high = hi, p_more = pnorm(beta / se, lower.tail = FALSE),
      dispersion = sum(residuals(fit, type = "pearson")^2) / df.residual(fit),
      max_leverage = max(h), max_abs_dfbeta = max(abs(influence), na.rm = TRUE))
  }, warning = function(w) {
    warnings_seen <<- c(warnings_seen, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) bad(conditionMessage(e)))
  # Numerical warnings cannot silently become successful findings.
  if (length(warnings_seen)) return(bad(paste(unique(warnings_seen), collapse = "; ")))
  attempt
}

test_patterns <- function(prepared, min_support = 50L, min_within_borough = 10L,
                          min_overlap_boroughs = 1L, alpha = 0.05,
                          adjust_baseline_history = FALSE) {
  cohort <- prepared$cohort
  controls <- make_controls(cohort, adjust_baseline_history = adjust_baseline_history)
  d <- controls$data
  ids_by_feature <- split(prepared$links$street_id, prepared$links$feature)
  names_sorted <- sort(names(ids_by_feature))
  rows <- vector("list", length(names_sorted))
  for (j in seq_along(names_sorted)) {
    f <- names_sorted[j]
    present <- d$street_id %in% ids_by_feature[[f]]
    n_yes <- sum(present); n_no <- sum(!present)
    tab <- table(d$borough, factor(present, levels = c(FALSE, TRUE)))
    overlap <- sum(rowSums(tab >= min_within_borough) == 2L)
    eligible <- n_yes >= min_support && n_no >= min_support && overlap >= min_overlap_boroughs
    row <- data.frame(feature = f, baseline_streets_with = n_yes,
      baseline_streets_without = n_no, overlap_boroughs = overlap,
      eligible = eligible, stringsAsFactors = FALSE)
    for (period in c("discovery", "validation")) {
      y <- d[[period]]
      row[[paste0(period, "_crashes_with")]] <- sum(y[present])
      row[[paste0(period, "_crashes_without")]] <- sum(y[!present])
      row[[paste0(period, "_mean_with")]] <- mean(y[present])
      row[[paste0(period, "_mean_without")]] <- if (n_no) mean(y[!present]) else NA_real_
      result <- if (eligible) fit_pattern(controls$X, y, present) else
        list(status = "insufficient_baseline_support_or_overlap", beta = NA_real_, se = NA_real_,
          count_ratio = NA_real_, ci_low = NA_real_, ci_high = NA_real_, p_more = 1,
          dispersion = NA_real_, max_leverage = NA_real_, max_abs_dfbeta = NA_real_)
      for (nm in names(result)) row[[paste0(period, "_", nm)]] <- result[[nm]]
    }
    rows[[j]] <- row
  }
  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  if (!nrow(out)) stop("No baseline name patterns exist.")
  out[, p_replicated_more := pmax(discovery_p_more, validation_p_more)]
  out[, q_replicated_more_BY := NA_real_]
  ix <- which(out$eligible)
  if (!length(ix)) warning("No patterns meet the predeclared support/overlap rules; no inferential tests were possible.")
  if (length(ix)) out$q_replicated_more_BY[ix] <- p.adjust(out$p_replicated_more[ix], method = "BY")
  out[, replicated_more := eligible & !is.na(q_replicated_more_BY) &
    q_replicated_more_BY <= alpha & discovery_beta > 0 & validation_beta > 0 &
    discovery_status == "ok" & validation_status == "ok"]
  out[is.na(replicated_more), replicated_more := FALSE]
  out <- out[order(-replicated_more, q_replicated_more_BY, feature, na.last = TRUE)]
  list(results = out, model_controls = controls$control_columns)
}

analyze_crash_patterns <- function(input, boundaries, complete_extract = FALSE,
    out_dir = "nyc_pattern_results", min_support = 50L, max_ngram = 3L,
    min_within_borough = 10L, min_overlap_boroughs = 1L, alpha = 0.05,
    adjust_baseline_history = FALSE) {
  require_crash_packages()
  if (!isTRUE(complete_extract)) stop(
    "Verify the file is a complete, unfiltered extract for the selected dates, then set complete_extract=TRUE. ",
    "Later zero counts are otherwise not interpretable.")
  if (length(max_ngram) != 1L || is.na(max_ngram) || !max_ngram %in% 1:3)
    stop("max_ngram must be 1, 2, or 3.")
  if (!is.numeric(min_support) || length(min_support) != 1L || is.na(min_support) ||
      min_support < 20L || min_support != floor(min_support)) stop("min_support must be an integer >=20.")
  if (length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1) stop("Invalid alpha.")
  if (!is.logical(adjust_baseline_history) || length(adjust_baseline_history) != 1L ||
      is.na(adjust_baseline_history)) stop("adjust_baseline_history must be TRUE or FALSE.")
  if (length(min_within_borough) != 1L || !is.finite(min_within_borough) || min_within_borough < 1 ||
      min_within_borough != floor(min_within_borough)) stop("min_within_borough must be a positive integer.")
  if (length(min_overlap_boroughs) != 1L || !min_overlap_boroughs %in% 1:5)
    stop("min_overlap_boroughs must be an integer from 1 to 5.")
  if (dir.exists(out_dir) && length(list.files(out_dir, all.files = TRUE, no.. = TRUE)))
    stop("Output directory is not empty. Choose a new directory to preserve earlier analyses.")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  cleaned <- clean_crashes(read_crashes(input))
  p <- prepare_cohort(cleaned, boundaries, max_ngram)
  # Write audits before any model. Empty days often signal truncated downloads.
  export <- function(x, name) data.table::fwrite(x, file.path(out_dir, name), na = "NA")
  export(cleaned$audit, "data_quality.csv")
  export(cleaned$normalization, "street_name_normalization.csv")
  export(p$missingness, "missingness_by_period_borough.csv")
  export(p$daily_counts, "daily_record_counts.csv")
  export(p$coverage, "cohort_coverage.csv")
  export(p$descriptive, "descriptive_patterns.csv")
  export(p$street_counts, "all_observed_street_counts.csv")
  export(p$cohort, "baseline_street_cohort.csv")
  if (any(p$daily_counts$records == 0L)) stop(
    "At least one date has no crash records. Audit daily_record_counts.csv and choose verified complete periods. ",
    "Descriptive outputs were saved, but models were not run.")
  period_counts <- colSums(as.matrix(p$cohort[, .(baseline, discovery, validation)]))
  if (any(period_counts < 100L)) stop("Too few cohort crashes in at least one period for this analysis.")
  # Whole-year periods are preferable; warn, rather than silently drop dates.
  if (any(format(p$boundaries, "%m-%d") != "01-01"))
    warning("Periods are not whole calendar years; seasonality may affect comparability.")
  analysis <- test_patterns(p, min_support, min_within_borough, min_overlap_boroughs,
                            alpha, adjust_baseline_history)
  export(analysis$results, "pattern_results.csv")
  export(analysis$results[replicated_more], "replicated_more_crashes.csv")
  settings <- list(boundaries = as.character(p$boundaries), interval = "left-closed/right-open",
    complete_extract_asserted = complete_extract, min_support = min_support,
    max_ngram = max_ngram, min_within_borough = min_within_borough,
    min_overlap_boroughs = min_overlap_boroughs, alpha = alpha,
    adjust_baseline_history = adjust_baseline_history,
    controls = analysis$model_controls, count_model = "Poisson mean with HC3 sandwich covariance",
    multiple_testing = "BY on maximum of two one-sided period p-values",
    input_md5 = if (is.character(input)) unname(tools::md5sum(input)) else NA_character_)
  dput(settings, file = file.path(out_dir, "analysis_settings.R"))
  capture.output(sessionInfo(), file = file.path(out_dir, "session_info.txt"))
  note <- c("Results describe baseline-observed borough-street names, not every NYC road.",
    "Pattern effects are adjusted count ratios, not traffic-exposure risk ratios.",
    paste0("Baseline crash history adjustment enabled: ", adjust_baseline_history,
      ". When TRUE, the question is association beyond past crash counts."),
    "replicated_more=TRUE requires a positive association in both later periods and BY-adjusted p <= alpha.",
    "q_replicated_more_BY is an adjusted p-value, not the probability that this pattern is a false discovery.",
    "All confidence intervals are pointwise 95%; they are not multiplicity-adjusted or causal intervals.",
    "Failed eligible fits remain in the correction with p=1. Inspect status and influence columns.",
    "A large dispersion is permitted by robust inference; it does not prove the mean model is correct.",
    "HC3 does not account for dependence between neighboring streets or unmeasured traffic/geographic confounding.",
    "Each pattern has its own model; related significant substrings are not independent or mutually adjusted effects.",
    "Raw descriptive pattern shares overlap and do not sum to 1.",
    "Zero later counts mean no recorded events in this extract, not proof of zero true collisions.",
    "Do not interpret a non-significant result as proof that no association exists.")
  writeLines(note, file.path(out_dir, "INTERPRETATION.txt"))
  message("Completed. Eligible patterns: ", sum(analysis$results$eligible),
          "; replicated positive associations: ", sum(analysis$results$replicated_more),
          ". Inspect audits before interpreting results.")
  invisible(list(results = analysis$results, descriptive = p$descriptive,
                 cohort = p$cohort, audit = cleaned$audit, settings = settings))
}

# Self-tests: synthetic data ONLY; successful tests do not establish NYC findings.
# Run manually after source(), before using the real CSV.
run_self_tests <- function() {
  require_crash_packages()
  n <- normalize_street(c(" 31st st. ", "31 STREET", "AVENUE A", "AVE A", "UNKNOWN"))
  stopifnot(n$street[1L] == "31 STREET", n$street[1L] == n$street[2L],
            n$core[3L] == "A", n$street[3L] == n$street[4L], is.na(n$street[5L]))
  f <- name_features("AAA 131 A7")
  stopifnot(sum(f == "CHAR:A") == 1L, "GRAM2:A7" %in% f, "NUMBER:131" %in% f,
            !("GRAM2:A1" %in% f), !("NUMBER:13" %in% f))
  dates <- parse_crash_date(c("12/31/2023", "2024-01-01T00:00:00.000", "bad", "02/30/2023"))
  stopifnot(dates[1L] == as.Date("2023-12-31"), dates[2L] == as.Date("2024-01-01"),
            is.na(dates[3L]), is.na(dates[4L]))
  raw <- data.frame(COLLISION_ID = c("1", "1", "2", "3", "4"),
    `CRASH DATE` = c("01/01/2021", "01/01/2021", "01/01/2022", "01/01/2022", "01/01/2023"),
    BOROUGH = "QUEENS", `ON STREET NAME` = c("31 ST", "31 ST", "31 ST", "NEW ROAD", "NEW ROAD"),
    check.names = FALSE)
  cleaned <- clean_crashes(read_crashes(raw))
  stopifnot(nrow(cleaned$data) == 4L)
  p <- prepare_cohort(cleaned, c("2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01"))
  stopifnot(nrow(p$cohort) == 1L, p$cohort$validation == 0L,
            !any(p$cohort$street == "NEW ROAD"))
  # A constant-control cohort must still produce an intercept-only matrix.
  stopifnot(ncol(make_controls(p$cohort)$X) == 1L)
  # A duplicate ID with different analysis fields must fail.
  bad <- raw; bad$`ON STREET NAME`[2L] <- "OTHER ROAD"
  stopifnot(inherits(try(clean_crashes(read_crashes(bad)), silent = TRUE), "try-error"))
  set.seed(1017)
  nn <- 1600L
  pattern <- rep(c(FALSE, TRUE), nn / 2L)
  nuisance <- rnorm(nn)
  X <- cbind(intercept = 1, nuisance = nuisance)
  # Exact balanced counts give an exact count ratio of two.
  paired_counts <- rep(c(2L, 4L, 6L), length.out = nn / 2L)
  exact_y <- rep(paired_counts, each = 2L) * ifelse(pattern, 2L, 1L)
  deterministic <- fit_pattern(matrix(1, nn, 1L), exact_y, pattern)
  stopifnot(deterministic$status == "ok", abs(deterministic$count_ratio - 2) < 1e-6)
  # Negative-binomial simulation deliberately violates Poisson variance.
  mu <- exp(1.8 + 0.25 * nuisance + log(1.8) * pattern)
  y1 <- rnbinom(nn, mu = mu, size = 5)
  y2 <- rnbinom(nn, mu = mu, size = 5)
  a <- fit_pattern(X, y1, pattern); b <- fit_pattern(X, y2, pattern)
  stopifnot(a$status == "ok", b$status == "ok", a$count_ratio > 1.4,
            b$count_ratio > 1.4, max(a$p_more, b$p_more) < 0.001)
  # Null effect: exactly equal group distributions, not a random p-value assertion.
  paired <- rpois(nn / 2L, 5)
  yn <- rep(paired, each = 2L)
  null <- fit_pattern(matrix(1, nn, 1L), yn, pattern)
  stopifnot(null$status == "ok", abs(null$count_ratio - 1) < 1e-6, null$p_more > 0.49)
  # Confounded/constant patterns and all-zero groups must not pass.
  alias <- fit_pattern(cbind(1, as.integer(pattern)), y1, pattern)
  zero <- fit_pattern(matrix(1, nn, 1L), ifelse(pattern, 0, 1), pattern)
  stopifnot(alias$p_more == 1, alias$status != "ok", zero$p_more == 1)
  # Full end-to-end test, including every date, real CSV I/O and all outputs.
  set.seed(1018)
  ns <- 240L
  street <- paste(ifelse(seq_len(ns) %% 2L == 0L, "AX", "BY"), seq_len(ns), "STREET")
  all_dates <- seq(as.Date("2021-01-01"), as.Date("2023-12-31"), by = "day")
  date_index <- rep(seq_along(all_dates), each = 12L)
  sampled_street <- sample(seq_len(ns), length(date_index), replace = TRUE)
  demo <- data.frame(COLLISION_ID = seq_along(date_index),
    `CRASH DATE` = as.character(all_dates[date_index]),
    BOROUGH = c("QUEENS", "BROOKLYN", "BRONX")[(sampled_street %% 3L) + 1L],
    `ON STREET NAME` = street[sampled_street], check.names = FALSE)
  tmp <- tempfile("crash_selftest_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  csv <- file.path(tmp, "synthetic.csv"); data.table::fwrite(demo, csv)
  answer <- analyze_crash_patterns(csv,
    c("2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01"),
    complete_extract = TRUE, out_dir = file.path(tmp, "results"), min_support = 20L)
  stopifnot(nrow(answer$cohort) == ns,
    sum(answer$cohort$baseline) + sum(answer$cohort$discovery) + sum(answer$cohort$validation) == nrow(demo),
    all(answer$results$q_replicated_more_BY[answer$results$eligible] >=
        answer$results$p_replicated_more[answer$results$eligible] - 1e-12),
    file.exists(file.path(tmp, "results", "pattern_results.csv")))
  message("All R self-tests passed. Synthetic tests are not real-data findings.")
  invisible(TRUE)
}
