# Street casualty totals and borough injury/death rates per 1,000 crashes.
# Uses all records in the local CSV; no year/season/time-of-day filtering.
# Run from the project directory, or use:
# & 'C:/Program Files/R/R-4.6.1/bin/Rscript.exe' street_borough_casualties.R
# Packages: data.table, ggplot2. Results are saved in outputs/street_borough_casualties/.

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(ggplot2))
setDTthreads(min(4L, getDTthreads()))

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
project_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1L])))
} else getwd()
csv_path <- file.path(project_dir, "data", "motor_vehicle_collisions.csv")
output_dir <- file.path(project_dir, "outputs", "street_borough_casualties")
top_n <- 20L  # Only limits the street plot; the CSV contains every street name.

# 1. Read only the fields needed for this comparison.
crashes <- fread(
  file = csv_path,
  select = c("COLLISION_ID", "ON STREET NAME", "BOROUGH",
    "NUMBER OF PERSONS INJURED", "NUMBER OF PERSONS KILLED"),
  colClasses = list(character = c("COLLISION_ID", "ON STREET NAME", "BOROUGH")),
  na.strings = c("", "NA")
)
setnames(crashes,
  c("COLLISION_ID", "ON STREET NAME", "BOROUGH", "NUMBER OF PERSONS INJURED", "NUMBER OF PERSONS KILLED"),
  c("collision_id", "street_name", "borough", "injuries", "deaths"))
if (anyNA(crashes$collision_id)) stop("Missing collision IDs; check before grouping.")
crashes <- unique(crashes, by = "collision_id")

# Trim whitespace and standardize case, without guessing street-name aliases.
# Retain missing street names; missing boroughs are excluded from borough rankings below.
clean_location <- function(x) {
  x <- toupper(trimws(gsub("\\s+", " ", x)))
  x[is.na(x) | x == ""] <- "MISSING"
  x
}
crashes[, `:=`(street_name = clean_location(street_name), borough = clean_location(borough))]
sum_recorded <- function(x) if (all(is.na(x))) NA_real_ else as.numeric(sum(x, na.rm = TRUE))

# 2. Sum people, not the number of injury/fatal crashes.
# Identical street names across different boroughs are combined here.
summarize_location <- function(location_column) {
  result <- crashes[, .(
    crashes = .N,
    injuries = sum_recorded(injuries),
    deaths = sum_recorded(deaths),
    missing_injury_records = sum(is.na(injuries)),
    missing_death_records = sum(is.na(deaths))
  ), by = location_column]
  setorderv(result, c("injuries", "deaths"), c(-1L, -1L), na.last = TRUE)
  result
}
street_summary <- summarize_location("street_name")
borough_all <- summarize_location("borough")

# Check that grouping preserves all crashes and recorded casualty counts.
for (table in list(street_summary, borough_all)) {
  stopifnot(sum(table$crashes) == nrow(crashes))
  for (outcome in c("injuries", "deaths")) {
    stopifnot(isTRUE(all.equal(sum_recorded(table[[outcome]]), sum_recorded(crashes[[outcome]]))))
  }
}

# Borough comparison: only the five NYC boroughs; exclude MISSING.
# Rates count people per 1,000 crashes with a known corresponding outcome.
# They are not probabilities or population/trip-adjusted risk estimates.
borough_summary <- borough_all[borough %chin%
  c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND")]
borough_summary[, `:=`(
  known_injury_records = crashes - missing_injury_records,
  known_death_records = crashes - missing_death_records
)]
borough_summary[, `:=`(
  injury_rate_per_1000 = fifelse(known_injury_records > 0,
    injuries / known_injury_records * 1000, NA_real_),
  death_rate_per_1000 = fifelse(known_death_records > 0,
    deaths / known_death_records * 1000, NA_real_)
)]
borough_summary[, `:=`(
  injury_rate_rank = frank(-injury_rate_per_1000, ties.method = "min", na.last = "keep"),
  death_rate_rank = frank(-death_rate_per_1000, ties.method = "min", na.last = "keep")
)]
setorder(borough_summary, injury_rate_rank)
stopifnot(nrow(borough_summary) == 5L, !"MISSING" %chin% borough_summary$borough)

# 3. Compare injuries and deaths in separate panels: their scales differ greatly.
comparison_plot <- function(table, location_column, title, subtitle) {
  chart_data <- copy(table)
  setnames(chart_data, location_column, "location")
  chart_data[, location := factor(location, levels = rev(location))]
  chart_data <- melt(chart_data, id.vars = "location",
    measure.vars = c("injuries", "deaths"), variable.name = "outcome", value.name = "people")
  chart_data[, outcome := factor(outcome, levels = c("injuries", "deaths"), labels = c("Injuries", "Deaths"))]
  ggplot(chart_data, aes(x = people, y = location)) +
    geom_col(fill = "#34758B", width = 0.72) +
    facet_wrap(~outcome, scales = "free_x", nrow = 1) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE),
      expand = expansion(mult = c(0, 0.08))) +
    labs(title = title, subtitle = subtitle, x = "Recorded number of people", y = NULL,
      caption = "Panels use different x-axis scales. Missing outcomes are not confirmed zeros; counts are not exposure-adjusted risks.") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"), plot.title = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0, size = 8))
}
top_streets <- head(street_summary[street_name != "MISSING"], top_n)
street_plot <- comparison_plot(top_streets, "street_name",
  paste("Top", nrow(top_streets), "street names by injuries"),
  "All available records | Same street names pooled across boroughs | Every street is included in the CSV")
borough_chart <- melt(borough_summary, id.vars = "borough",
  measure.vars = c("injury_rate_per_1000", "death_rate_per_1000"),
  variable.name = "outcome", value.name = "rate")
borough_chart[, outcome := factor(outcome,
  levels = c("injury_rate_per_1000", "death_rate_per_1000"),
  labels = c("Injury rate", "Death rate"))]
# Separate labels allow each panel to rank boroughs independently.
setorder(borough_chart, outcome, rate)
borough_chart[, plot_label := paste(borough, outcome, sep = "___")]
borough_chart[, plot_label := factor(plot_label, levels = plot_label)]
borough_plot <- ggplot(borough_chart, aes(x = rate, y = plot_label, fill = outcome)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f", rate)), hjust = -0.15, size = 3.6) +
  facet_wrap(~outcome, scales = "free", nrow = 1) +
  scale_fill_manual(values = c("Injury rate" = "#34758B", "Death rate" = "#B55842")) +
  scale_y_discrete(labels = function(x) sub("___.*$", "", x)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Borough rankings: injuries and deaths per 1,000 crashes",
    subtitle = "Five NYC boroughs | Missing borough excluded | Each panel ranked from highest to lowest",
    x = "Recorded people per 1,000 crashes with a known corresponding outcome", y = NULL,
    caption = "Injuries / known-injury crashes x 1,000; deaths / known-death crashes x 1,000. Not risk per trip; panels have different scales.") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"), plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, size = 8))

# 4. Save the full summaries and quick comparison figures.
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
fwrite(street_summary, file.path(output_dir, "street_summary.csv"))
fwrite(borough_summary, file.path(output_dir, "borough_summary.csv"))
ggsave(file.path(output_dir, "street_comparison.png"), street_plot,
  width = 14, height = 9, dpi = 180, bg = "white")
ggsave(file.path(output_dir, "borough_comparison.png"), borough_plot,
  width = 13, height = 5, dpi = 180, bg = "white")

cat("\nStreet summary: top 20 by injuries (complete table saved to CSV)\n")
print(head(street_summary, 20L))
cat("\nBorough injury-rate ranking (people per 1,000 known-outcome crashes)\n")
print(borough_summary[, .(borough, injuries, injury_rate_per_1000, injury_rate_rank)])
cat("\nBorough death-rate ranking (people per 1,000 known-outcome crashes)\n")
print(borough_summary[order(death_rate_rank), .(borough, deaths, death_rate_per_1000, death_rate_rank)])
cat("\nRecords excluded from borough rankings:", nrow(crashes) - sum(borough_summary$crashes), "\n")
cat("\nMissing injury values:", sum(is.na(crashes$injuries)),
  "| Missing death values:", sum(is.na(crashes$deaths)), "\n")
cat("Missing outcomes were excluded from sums, not treated as confirmed zeros.\n")
cat("Results:", normalizePath(output_dir, winslash = "/"), "\n")

# In RStudio, Source this script to show the plots. You can also run:
# View(street_summary)                 # Every street name
# View(borough_summary)                # Five boroughs, with both rate rankings
# street_summary[order(-deaths)]       # Rank streets by recorded deaths
if (interactive()) {
  print(street_plot)
  print(borough_plot)
}
