# Reproduce the borough crash / vehicle registration comparison.
# Uses the ENTIRE local crash dataset and divides crash counts by 14 years,
# as requested. The file actually spans partial 2012 and partial 2026.
# Registration counts were transcribed from the user's supplied image;
# the image does not identify their reference year or registration categories.
# Borough names describe crash locations, not necessarily vehicle owners' homes.
# These ratios are descriptive comparisons, not individual crash probabilities.
#
# In RStudio: open this file in the project and click Source.
# In PowerShell:
# & 'C:/Program Files/R/R-4.6.1/bin/Rscript.exe' borough_crashes_per_registered_vehicle.R
# Requires data.table. No other project scripts are run or modified.

suppressPackageStartupMessages(library(data.table))
setDTthreads(min(4L, getDTthreads()))

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
project_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1L])))
} else getwd()
data_dir <- file.path(project_dir, "data")
output_dir <- file.path(project_dir, "outputs", "borough_vehicle_comparison")
number_of_years <- 14  # Explicit approximation requested by the user.

# 1. Read the independent registration dataset created from the image.
# The source image's total is not stored as another borough: that would double count.
vehicle_registrations <- fread(file.path(data_dir, "borough_active_vehicle_registrations.csv"))
stopifnot(
  nrow(vehicle_registrations) == 5L,
  !anyDuplicated(vehicle_registrations$borough),
  all(vehicle_registrations$active_registrations > 0),
  sum(vehicle_registrations$active_registrations) == 2124852,
  number_of_years > 0
)

# 2. Count crashes by borough from the original data, without a year filter.
crashes <- fread(file.path(data_dir, "motor_vehicle_collisions.csv"),
  select = c("COLLISION_ID", "CRASH DATE", "BOROUGH"),
  colClasses = "character", na.strings = c("", "NA"))
if (anyNA(crashes$COLLISION_ID)) stop("Missing collision IDs; resolve before counting.")
crashes <- unique(crashes, by = "COLLISION_ID")
crashes[, borough := toupper(trimws(BOROUGH))]
crash_counts <- crashes[, .(total_crashes = .N), by = borough]

# Use exactly the five boroughs matched to the registration dataset.
borough_results <- merge(vehicle_registrations[, .(borough, active_registrations)],
  crash_counts, by = "borough", all.x = TRUE)
if (anyNA(borough_results$total_crashes)) stop("A registration borough has no matching crash group.")
excluded_crashes <- nrow(crashes) - sum(borough_results$total_crashes)

# 3. Calculate the annual average and both representations of its vehicle ratio.
borough_results[, annual_average_crashes := total_crashes / number_of_years]
borough_results[, `:=`(
  annual_crashes_per_1000_vehicles = annual_average_crashes / active_registrations * 1000,
  annual_proportion_percent = annual_average_crashes / active_registrations * 100
)]
setorder(borough_results, -annual_proportion_percent)
setcolorder(borough_results, c("borough", "total_crashes", "annual_average_crashes",
  "active_registrations", "annual_crashes_per_1000_vehicles", "annual_proportion_percent"))

# The total ratio is calculated from summed counts, not averaged borough percentages.
total_crashes <- sum(borough_results$total_crashes)
total_registrations <- sum(borough_results$active_registrations)
total_row <- data.table(
  borough = "TOTAL (known boroughs)",
  total_crashes = total_crashes,
  annual_average_crashes = total_crashes / number_of_years,
  active_registrations = total_registrations,
  annual_crashes_per_1000_vehicles = total_crashes / number_of_years / total_registrations * 1000,
  annual_proportion_percent = total_crashes / number_of_years / total_registrations * 100
)

# 4. Present a standard R data.frame, rounded only for display/export.
# borough_results retains the five boroughs at full precision for further analysis.
borough_table <- as.data.frame(rbind(borough_results, total_row))
display_columns <- c("annual_average_crashes", "annual_crashes_per_1000_vehicles", "annual_proportion_percent")
borough_table[display_columns] <- lapply(borough_table[display_columns], round, digits = 2)
stopifnot(
  nrow(borough_results) == 5L,
  nrow(borough_table) == 6L,
  sum(borough_results$total_crashes) + excluded_crashes == nrow(crashes),
  isTRUE(all.equal(borough_results$annual_crashes_per_1000_vehicles,
    borough_results$annual_proportion_percent * 10))
)

print(borough_table, row.names = FALSE)
dates <- as.Date(unique(crashes$`CRASH DATE`), format = "%m/%d/%Y")
cat("\nActual crash-date coverage:", format(min(dates, na.rm = TRUE)),
  "to", format(max(dates, na.rm = TRUE)), "\n")
cat("Annual average = cumulative crashes /", number_of_years, "years.\n")
cat("Excluded crashes with no matching known borough:", excluded_crashes, "\n")
cat("annual_proportion_percent is expressed as percent (10.77 means 10.77%).\n")
cat("Registration reference year is unspecified; ratios are not crash probabilities.\n")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(borough_table, file.path(output_dir, "borough_crash_vehicle_comparison.csv"), row.names = FALSE)
cat("\nSaved:", file.path(output_dir, "borough_crash_vehicle_comparison.csv"), "\n")

# Source in RStudio to view the data.frame directly.
if (interactive()) View(borough_table, title = "Borough crashes and vehicle registrations")
