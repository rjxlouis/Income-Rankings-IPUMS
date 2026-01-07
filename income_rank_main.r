# ==============================================================================
# INCOME RANKING ANALYSIS - MAIN SCRIPT (CLEANED)
# ==============================================================================
# Purpose: Process IPUMS data to create percentile rank files by birth cohort by year
# ==============================================================================

# SETUP ----
library(tidyverse)
library(data.table)
source('define_rank_income.R')

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Birth cohorts to analyze
COHORT_RANGE <- c(1983:2000)

# Age filters (keep ages ≤17 OR ≥24, exclude 18-23)
AGE_FILTER_LOWER <- 17  # Children: age ≤ this
AGE_FILTER_UPPER <- 24  # Adults: age ≥ this

# Income interval for ranking (dollars)
INTERVAL <- 100  # Percentile ranks computed at $100 increments

# Income definitions to create
INCOME_DEFINITIONS <- tibble(
  name = c("xearn", "xemp", "xinc", "xcominc"),
  formula = c(
    "INCWAGE",
    "INCWAGE + INCBUS00",
    "INCTOT",
    "INCWAGE + INCBUS00 + INCSS + INCRETIR + INCINVST"
  )
)

# IPUMS data files (.csv files with new pointers)
IPUMS_FILES <- c(
  "2000" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2000.csv',
  "2005" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2005.csv',
  "2006" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2006.csv',
  "2007" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2007.csv',
  "2008" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2008.csv',
  "2009" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2009.csv',
  "2010" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2010.csv',
  "2011" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2011.csv',
  "2012" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2012.csv',
  "2013" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2013.csv',
  "2014" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2014.csv',
  "2015" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2015.csv',
  "2016" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2016.csv',
  "2017" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2017.csv',
  "2018" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2018.csv',
  "2019" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2019.csv',
  "2020" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2020.csv',
  "2021" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2021.csv',
  "2022" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2022.csv',
  "2023" = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/IPUMS/Data/acs_2023.csv'
)

# Output directories
OUTPUT_DIR <- '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/Code/Output'
TEST_OUTPUT_DIR <- '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/Code/Test/Output'

# ==============================================================================
# TESTING FUNCTIONS
# ==============================================================================

#' Test 1: Load and prepare data
test_load_and_prep <- function(year="2005") {
  cat("\n=== TEST 1: Load and Prep Data ===\n")
  
  test_year <- year
  raw_data <- load_data_csv(IPUMS_FILES[[test_year]])
  cat("Raw data:", nrow(raw_data), "rows\n")
  
  prepped_data <- prep_for_aggregation(
    dt = raw_data,
    cohort_range = COHORT_RANGE,
    income_table = INCOME_DEFINITIONS,
    age_filter_lower = AGE_FILTER_LOWER,
    age_filter_upper = AGE_FILTER_UPPER,
    sample_n = 5000
  )
  
  cat("Prepped data:", nrow(prepped_data), "rows\n")
  cat("Income columns created:", 
      paste(names(prepped_data)[names(prepped_data) %in% INCOME_DEFINITIONS$name], 
            collapse = ", "), "\n")
  
  return(prepped_data)
}

#' Test 2: Apply aggregations
test_aggregations <- function(year="2005") {
  cat("\n=== TEST 2: Apply Aggregations ===\n")
  
  test_year <- year
  raw_data <- load_data_csv(IPUMS_FILES[[test_year]])
  
  prepped_data <- prep_for_aggregation(
    dt = raw_data,
    cohort_range = COHORT_RANGE,
    income_table = INCOME_DEFINITIONS,
    age_filter_lower = AGE_FILTER_LOWER,
    age_filter_upper = AGE_FILTER_UPPER,
    sample_n = 1000
  )
  
  agg_result <- apply_all_aggregations_optimized(
    prepped_data, 
    INCOME_DEFINITIONS$name
  )
  
  cat("Ego-level data:", nrow(agg_result$egos), "rows\n")
  
  # Show aggregated columns
  agg_cols <- grep("_(household|family|partners_sum|spouses_sum|guardians_sum|guardians_married|ego|partner|spouse)$", 
                   names(agg_result$egos), value = TRUE)
  cat("Aggregation columns (", length(agg_cols), "):\n")
  for (col in agg_cols) {
    pct_na <- mean(is.na(agg_result$egos[[col]])) * 100
    cat(sprintf("  %-30s: %5.1f%% missing\n", col, pct_na))
  }
  
  return(agg_result)
}

#' Test 3: Single year ranking
test_single_ranking <- function(year=2010) {
  cat("\n=== TEST 3: Single Year Ranking ===\n")
  
  test_year <- year
  raw_data <- load_data_csv(IPUMS_FILES[[as.character(test_year)]])
  
  prepped_data <- prep_for_aggregation(
    dt = raw_data,
    cohort_range = COHORT_RANGE,
    income_table = INCOME_DEFINITIONS,
    age_filter_lower = AGE_FILTER_LOWER,
    age_filter_upper = AGE_FILTER_UPPER,
    sample_n = 5000
  )
  
  agg_result <- apply_all_aggregations_optimized(
    prepped_data,
    INCOME_DEFINITIONS$name
  )
  
  # Test ranking
  valid_cohorts <- COHORT_RANGE[sapply(COHORT_RANGE, function(c) {
    age <- test_year - c
    age >= AGE_FILTER_UPPER
  })]
  
  cat("Valid adult cohorts:", paste(valid_cohorts, collapse = ", "), "\n")
  
  test_data <- agg_result$egos %>%
    filter(ego_BIRTHYR %in% valid_cohorts) %>%
    select(ego_BIRTHYR, xearn_EGO, PERWT) %>%
    rename(income = xearn_EGO)
  
  cat("Test data:", nrow(test_data), "rows\n")
  
  # Rank using fast empirical method
  rank_table <- rank_empirical_fast(test_data, "income", "PERWT", INTERVAL)
  
  cat("Rank table:", nrow(rank_table), "rows\n")
  cat("Cohorts:", paste(unique(rank_table$cohort), collapse = ", "), "\n")
  
  # Check monotonicity
  for (coh in head(unique(rank_table$cohort), 3)) {
    cohort_data <- rank_table %>% filter(cohort == coh) %>% arrange(income_value)
    is_monotonic <- all(diff(cohort_data$percentile_rank) >= 0)
    cat(sprintf("Cohort %d: %s\n", coh, ifelse(is_monotonic, "MONOTONIC ✓", "NOT MONOTONIC ✗")))
  }
  
  return(rank_table)
}

#' Test 4: Full pipeline on sample
test_full_pipeline <- function(year="2010") {
  cat("\n=== TEST 4: Full Pipeline (Sample) ===\n")
  
  test_year <- year
  
  results <- process_single_year(
    ipums_filepath = IPUMS_FILES[[test_year]],
    year_val = as.numeric(test_year),
    cohort_range = COHORT_RANGE,
    income_definitions = INCOME_DEFINITIONS,
    interval = INTERVAL,
    age_filter_lower = AGE_FILTER_LOWER,
    age_filter_upper = AGE_FILTER_UPPER,
    sample_n = 5000
  )
  
  cat("\nResults:", nrow(results), "rows\n")
  
  summary_stats <- results %>%
    group_by(income_definition, aggregation_level) %>%
    summarise(
      n_cohorts = n_distinct(cohort),
      n_values = n(),
      .groups = "drop"
    )
  
  print(summary_stats)
  
  # Save test results
  test_file <- file.path(TEST_OUTPUT_DIR, paste0("TEST_income_ranks_", test_year, ".csv"))
  write_csv(results, test_file)
  cat("\nSaved to:", test_file, "\n")
  
  return(results)
}

# ==============================================================================
# PRODUCTION FUNCTIONS
# ==============================================================================

#' Run full production: single year
run_production_single <- function(year=2010, output_dir = NULL) {
  cat("\n========================================\n")
  cat("PRODUCTION RUN: SINGLE YEAR\n")
  cat("========================================\n")
  cat("Year:", year, "\n")
  cat("Output:", OUTPUT_DIR, "\n")
  
  # Confirm
  cat("\nPress [Enter] to continue or [Ctrl+C] to cancel...\n")
  readline()
  
  overall_start <- Sys.time()
  
  if (is.null(output_dir)) {
    output_dir <- getwd()
  }
  
  results <- process_single_year(
    ipums_filepath = IPUMS_FILES[[paste0(year)]],
    year_val = as.numeric(year),
    cohort_range = COHORT_RANGE,
    income_definitions = INCOME_DEFINITIONS,
    interval = INTERVAL,
    age_filter_lower = AGE_FILTER_LOWER,
    age_filter_upper = AGE_FILTER_UPPER,
    sample_n = NULL
    )
  
  overall_end <- Sys.time()
  total_time <- difftime(overall_end, overall_start, units = "mins")
  
  cat("\n========================================\n")
  cat("PRODUCTION COMPLETE!\n")
  cat("Total time:", round(total_time, 1), "minutes\n")
  cat("========================================\n")
  
  # Save results
  output_file <- file.path(output_dir, paste0("income_ranks_", year, ".csv.gz"))
  fwrite(results, output_file)
  
  return(results)
}

#' Run full production: all years in parallel
run_production <- function() {
  cat("\n========================================\n")
  cat("PRODUCTION RUN: ALL YEARS IN PARALLEL\n")
  cat("========================================\n")
  cat("Years:", length(IPUMS_FILES), "\n")
  cat("Output:", OUTPUT_DIR, "\n")
  
  # Confirm
  cat("\nPress [Enter] to continue or [Ctrl+C] to cancel...\n")
  readline()
  
  overall_start <- Sys.time()
  
  results_summary <- process_all_years_parallel(
    ipums_files = IPUMS_FILES,
    output_dir = OUTPUT_DIR,
    n_cores = parallel::detectCores() - 1,
    cohort_range = COHORT_RANGE,
    income_definitions = INCOME_DEFINITIONS,
    interval = INTERVAL,
    age_filter_lower = AGE_FILTER_LOWER,
    age_filter_upper = AGE_FILTER_UPPER
  )
  
  overall_end <- Sys.time()
  total_time <- difftime(overall_end, overall_start, units = "mins")
  
  cat("\n========================================\n")
  cat("PRODUCTION COMPLETE!\n")
  cat("Total time:", round(total_time, 1), "minutes\n")
  cat("========================================\n")
  
  return(results_summary)
}

#' Verify output files
verify_outputs <- function() {
  cat("\n=== VERIFYING OUTPUT FILES ===\n\n")
  
  output_files <- list.files(OUTPUT_DIR, pattern = "^income_ranks_\\d{4}\\.csv", 
                             full.names = TRUE)
  
  cat("Found", length(output_files), "files\n\n")
  
  file_info <- data.frame()
  
  for (file in output_files) {
    year <- gsub(".*_(\\d{4})\\.csv.*", "\\1", basename(file))
    file_size_mb <- file.info(file)$size / 1024^2
    
    tryCatch({
      dt <- fread(file, nrows = 1)
      n_rows <- as.numeric(system(paste("wc -l <", file), intern = TRUE)) - 1
      
      file_info <- rbind(file_info, data.frame(
        year = year,
        n_rows = n_rows,
        size_mb = round(file_size_mb, 1)
      ))
      
      cat(sprintf("  %s: %s rows (%.1f MB) ✓\n", 
                  year, format(n_rows, big.mark = ","), file_size_mb))
      
    }, error = function(e) {
      cat(sprintf("  %s: ERROR ✗\n", year))
    })
  }
  
  cat("\nTotal size:", round(sum(file_info$size_mb), 1), "MB\n")
  
  return(file_info)
}

#' Correlation analysis for a given year
create_correlation_analysis <- function(year_val) {
  cat("\n========================================\n")
  cat("CORRELATION ANALYSIS:", year_val, "\n")
  cat("========================================\n")
  
  # Load results
  results_file <- file.path(OUTPUT_DIR, paste0("income_ranks_", year_val, ".csv.gz"))
  if (!file.exists(results_file)) {
    stop("Results file not found: ", results_file)
  }
  
  results <- fread(results_file)
  cat("Loaded results:", nrow(results), "rows\n")
  
  # Load raw data
  raw_data <- load_ipums_data(IPUMS_FILES[[as.character(year_val)]])
  
  prepped_data <- prep_for_aggregation(
    dt = raw_data,
    cohort_range = COHORT_RANGE,
    income_table = INCOME_DEFINITIONS,
    age_filter_lower = AGE_FILTER_LOWER,
    age_filter_upper = AGE_FILTER_UPPER,
    sample_n = NULL
  )
  
  # Create aggregations
  agg_result <- apply_all_aggregations_optimized(
    prepped_data,
    INCOME_DEFINITIONS$name,
    COHORT_RANGE,
    AGE_FILTER_LOWER,
    AGE_FILTER_UPPER
  )
  
  ego_data <- agg_result$data
  cat("Ego data:", nrow(ego_data), "rows\n")
  
  # Get income-aggregation columns
  income_agg_cols <- grep("_(household|family|partners_sum|married_sum|guardians_sum|guardians_married|ego|partner|spouse)$", 
                          names(ego_data), value = TRUE)
  
  # Reshape to long
  ego_long <- melt(
    ego_data,
    id.vars = c("SERIAL", "FAMUNIT", "PERNUM", "BIRTHYR", "YEAR", "PERWT"),
    measure.vars = income_agg_cols,
    variable.name = "measure",
    value.name = "income_value"
  )
  
  # Parse measure
  ego_long[, ':='(
    income_definition = sub("_.*", "", measure),
    aggregation_level = sub("^[^_]+_", "", measure)
  )]
  
  # Round income
  ego_long[, income_value_rounded := round(income_value / INTERVAL) * INTERVAL]
  
  # Join to results to get percentile ranks
  setkey(ego_long, BIRTHYR, income_definition, aggregation_level, income_value_rounded)
  setkey(results, cohort, income_definition, aggregation_level, income_value)
  
  ego_with_percentiles <- results[ego_long,
                                  .(SERIAL, FAMUNIT, PERNUM, BIRTHYR, measure, 
                                    income_value, percentile_rank),
                                  on = .(cohort = BIRTHYR, 
                                         income_definition, 
                                         aggregation_level, 
                                         income_value = income_value_rounded)]
  
  ego_with_percentiles <- ego_with_percentiles[!is.na(percentile_rank)]
  
  cat("Matched", nrow(ego_with_percentiles), "ego-measure pairs\n")
  
  # Reshape to wide
  ego_wide <- dcast(
    ego_with_percentiles,
    SERIAL + FAMUNIT + PERNUM + BIRTHYR ~ measure,
    value.var = "percentile_rank"
  )
  
  cat("Wide format:", nrow(ego_wide), "egos\n")
  
  # Calculate correlation matrix
  measure_cols <- setdiff(names(ego_wide), c("SERIAL", "FAMUNIT", "PERNUM", "BIRTHYR"))
  cor_matrix <- cor(ego_wide[, ..measure_cols], use = "pairwise.complete.obs")
  
  # Create plot
  library(corrplot)
  
  plot_file <- file.path(OUTPUT_DIR, paste0("correlation_", year_val, ".pdf"))
  
  pdf(plot_file, width = 16, height = 14)
  corrplot(cor_matrix, 
           method = "color",
           type = "upper",
           order = "hclust",
           tl.col = "black",
           tl.cex = 0.55,
           tl.srt = 45,
           title = paste("Income-Aggregation Correlations:", year_val),
           mar = c(0, 0, 4, 0),
           addCoef.col = "black",
           number.cex = 0.35)
  dev.off()
  
  cat("Saved plot to:", plot_file, "\n")
  
  return(list(
    ego_data = ego_wide,
    correlation_matrix = cor_matrix,
    plot_file = plot_file
  ))
}

# ==============================================================================
# USAGE EXAMPLES
# ==============================================================================

# Run tests first:
test_data <- test_load_and_prep("2010")
test_agg <- test_aggregations("2016")
test_ranks <- test_single_ranking(2010)
test_full <- test_full_pipeline("2000")

# Run production (single year):
results <- run_production_single(year = 2010,
                                 output_dir = OUTPUT_DIR)

# Run production (all years in parallel):
results <- run_production()

# Verify outputs:
# file_check <- verify_outputs()

# Correlation analysis:
# cor_analysis <- create_correlation_analysis(2010)

# ==============================================================================
# COMBINE OUTPUT
# ==============================================================================

combine_csv_files <- function(folder_path, output_file, pattern = "\\.csv\\.gz$") {
  
  # Get all files
  files <- list.files(folder_path, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) {
    stop("No files found matching pattern: ", pattern)
  }
  
  cat("Found", length(files), "files to combine\n")
  
  # Process each file
  for (i in seq_along(files)) {
    cat(sprintf("[%d/%d] Processing: %s\n", i, length(files), basename(files[i])))
    
    # Extract year (last 4 digits before .csv.gz)
    year <- sub(".*_(\\d{4})\\.csv\\.gz$", "\\1", basename(files[i]))
    
    # Read file
    dt <- fread(files[i], showProgress = FALSE)
    dt[, year := as.integer(year)]
    
    # Write (append after first file)
    if (i == 1) {
      fwrite(dt, output_file, compress = "gzip")
    } else {
      fwrite(dt, output_file, append = TRUE, compress = "gzip")
    }
    
    # Clear memory
    rm(dt)
    gc()
  }
  
  cat("Successfully combined", length(files), "files into:", output_file, "\n")
  
  # Return summary
  return(data.table(
    n_files = length(files),
    output = output_file
  ))
}

# Use it
result <- combine_csv_files(
  folder_path = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/Code/Output',
  output_file = '/Users/reneelouis/Library/CloudStorage/GoogleDrive-rlouis@stanford.edu/My Drive/Grad_Quarters/RAship/Code/Output/output.csv.gz'
)
