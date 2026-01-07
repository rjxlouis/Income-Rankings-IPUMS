# ==============================================================================
# INCOME RANKING ANALYSIS - HELPER FUNCTIONS (CLEANED)
# ==============================================================================
# This file contains all the core functions for data loading, preparation,
# aggregation, and ranking
#
# Edited to suit utility in RDC, combined some elements from income_rank_main.R
# ==============================================================================

library(data.table)
library(ipumsr)
library(progress)
library(future)
library(furrr)
library(tidyverse)

# ==============================================================================
# GLOBALS 
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

# ==============================================================================
# 1. DATA LOADING
# ==============================================================================

#' Load IPUMS microdata from XML DDI file
#' 
#' @param filepath Path to IPUMS .xml DDI file (e.g., "usa_2010.xml")
#' @return data.table with IPUMS microdata
#' 
#' What it does:
#' - Reads the XML metadata file (.xml)
#' - Automatically finds the corresponding data file (.dat)
#' - Loads all variables and converts to data.table for fast processing
load_ipums_data <- function(filepath) {
  ddi <- read_ipums_ddi(filepath)
  dat_filepath <- sub("\\.xml$", ".dat", filepath)
  data <- read_ipums_micro(ddi, data_file = dat_filepath)
  setDT(data)
  return(data)
}

#' Load IPUMS microdata from .csv file
#' 
#' @param filepath Path to IPUMS .csv file (e.g., "acs_2005.csv")
#' @return data.table with IPUMS microdata
#' 
#' What it does:
#' - Reads the file
#' - Loads all variables and converts to data.table for fast processing
load_data_csv <- function(filepath) {
  data <- read_csv(filepath)
  setDT(data)
  return(data)
}

# ==============================================================================
# 2. DATA PREPARATION
# ==============================================================================

#' Code missing values in income variables
#' 
#' @param dt data.table with IPUMS data
#' @return Same data.table with missing codes replaced by NA
#' 
#' What it does:
#' - IPUMS uses special codes for missing (999999, 999998, etc.)
#' - This converts those codes to proper NA values
#' - Affects: INCWAGE, INCBUS00, INCINVST, INCRETIR, INCSS, INCTOT, etc.
code_missing_income <- function(dt) {
  dt[INCWAGE %in% c(999999, 999998), INCWAGE := NA]
  dt[INCBUS00 == 999999, INCBUS00 := NA]
  dt[INCINVST == 999999, INCINVST := NA]
  dt[INCRETIR == 999999, INCRETIR := NA]
  dt[INCSS == 99999, INCSS := NA]
  # dt[INCSUPP == 99999, INCSUPP := NA]
  # dt[INCWELFR == 99999, INCWELFR := NA]
  # dt[INCOTHER %in% c(99999, 99998), INCOTHER := NA]
  dt[INCTOT %in% c(9999999, 9999998), INCTOT := NA]
  return(dt)
}

#' Create a new income variable from a formula
#' 
#' @param dt data.table with IPUMS data
#' @param formula String expression (e.g., "INCWAGE + INCBUS00")
#' @param new_var_name Name for the new column
#' @return Same data.table with new income column added
#' 
#' What it does:
#' - Evaluates the formula to create a new income measure
#' - Example: "xemp" = INCWAGE + INCBUS00 (wage + business income)
create_income_variable <- function(dt, formula, new_var_name) {
  dt[, (new_var_name) := eval(parse(text = formula))]
  return(dt)
}

#' Create all income definitions at once
#' 
#' @param dt data.table with IPUMS data
#' @param definition_table Tibble with columns: name, formula
#' @return Same data.table with all income columns added
#' 
#' What it does:
#' - Loops through your income definitions table
#' - Creates each income measure (xearn, xemp, xinc, xcominc)
create_all_income_definitions <- function(dt, definition_table) {
  for (i in 1:nrow(definition_table)) {
    create_income_variable(
      dt, 
      formula = definition_table$formula[i], 
      new_var_name = definition_table$name[i]
    )
  }
  return(dt)
}

#' Make data long on ego
#' 
#' @param dt data.table with IPUMS data
#' @return data.table with one copy of all HH information per target ego, retaining all columns
#' What it does:
#' - Identifies a "target ego" and makes all information long.
make_data_long_on_ego <- function(dt) {
  
  # Step 1: Identify the target egos and their household information
  ego_data <- dt[is_ego == TRUE, .(CBSERIAL, ego_PERNUM = CBPERNUM, ego_AGE = AGE, ego_BIRTHYR = BIRTHYR)]
  
  # Step 2: Create copies of all household members for each ego
  long_data <- dt[ , .SD, .SDcols = names(dt)]  # This retains all original columns
  
  # Step 3: Merge ego data back into long_data to duplicate rows accordingly
  long_data <- merge(long_data, ego_data, by = c("CBSERIAL"), allow.cartesian = TRUE)
  
  # Mark if ego is an adult or a child based on the age threshold for all rows
  long_data[, is_child := ifelse(ego_AGE <= AGE_FILTER_LOWER, 1, 0)]
  long_data[, is_adult := ifelse(ego_AGE >= AGE_FILTER_UPPER, 1, 0)]
  
  # Return the long formatted data
  return(long_data)
}

#' Prepare data for aggregation
#' 
#' @param dt data.table with raw IPUMS data
#' @param cohort_range Vector of birth years to analyze (e.g., 1983:2000)
#' @param income_table Table with income definitions
#' @param age_filter_lower Keep ages ≤ this (default 17 for children)
#' @param age_filter_upper Keep ages ≥ this (default 24 for adults)
#' @param sample_n Optional: sample N households for testing
#' @return Filtered and prepared data.table
#' 
#' What it does:
#' 1. Optionally samples households (for testing)
#' 2. Keeps only households containing an "ego" (someone in cohort_range)
#' 3. Codes missing income values as NA
#' 4. Creates all income definitions
#' 5. Makes data long on every ego
#' 
#' Note: Ages 18-23 will be filtered out later during ranking
prep_for_aggregation <- function(dt, cohort_range, income_table, 
                                 age_filter_lower = 17, age_filter_upper = 24,
                                 sample_n = NULL) {
  # Optional sampling for testing
  if (!is.null(sample_n)) {
    cat("Sampling", sample_n, "households...\n")
    sampled_hh <- dt[, .(CBSERIAL = unique(CBSERIAL))][sample(.N, min(sample_n, .N))]
    dt <- dt[sampled_hh, on = "CBSERIAL"]
  }
  
  # Keep only households with an ego
  dt[, has_ego := any(BIRTHYR %in% cohort_range), by = CBSERIAL]
  dt <- dt[has_ego == TRUE]
  dt[, has_ego := NULL]
  
  # Identify target egos
  dt[, is_ego := BIRTHYR %in% COHORT_RANGE]
  
  # Clean income variables
  code_missing_income(dt)
  
  # Create all income measures
  create_all_income_definitions(dt, income_table)
  
  # Makes data long on target ego
  dt <- make_data_long_on_ego(dt)

  return(dt)
}

# ==============================================================================
# 3. AGGREGATION HELPERS
# ==============================================================================

#' Identify family members of target egos
#' @param dt data.table with IPUMS data
#' @return data.table with appended column: FAMILY
#' What it does:
#' - identifies which rows should be included for the FAMILY aggregation
#' - all family members (shares FAMUNIT) of target egos get T, all other rows get F
#' - Note that all target egos' famunit should include at least themselves.
#' - calculate regardless of target ego age.
identify_family <- function(dt) {
  dt[, FAMILY := (FAMUNIT == FAMUNIT[CBPERNUM == ego_PERNUM]),
     by = .(CBSERIAL, ego_PERNUM)]
  return(dt)
}

#' Identify target egos in adult years
#' @param dt data.table with IPUMS data
#' @return data.table with appended column: EGO
#' What it does:
#' - identifies which rows should be included for the EGO aggregation
#' - target egos in adult years get T, all other rows get F
#' - since data structure is ego-level, retain only one copy based on ego_PERNUM which identifies the focal ego for that household copy
identify_egos <- function(dt) {
  dt[, EGO := (CBPERNUM == ego_PERNUM & is_adult == TRUE)]
  return(dt)
}

#' Identify target egos' partners in adult years
#' @param dt data.table with IPUMS data
#' @return data.table with appended column: PARTNER
#' What it does:
#' - identifies which rows should be included for the PARTNER aggregation
#' - every row whose value for SPLOC is ego_PERNUM AND is_adult is true gets T, all else is F
identify_partners <- function(dt) {
  dt[, PARTNER := (SPLOC == ego_PERNUM & is_adult == TRUE)]
  return(dt)
}

#' Identify target egos' spouses in adult years
#' @param dt data.table with IPUMS data (assumes PARTNER exists in the data)
#' @return data.table with appended column: SPOUSE
#' What it does:
#' - identifies which rows should be included for the SPOUSE aggregation
#' - every row whose value for PARTNER is T AND MARTST == 1/2 all gets T, all else is F
identify_spouses <- function(dt) {
  dt[, SPOUSE := (PARTNER == TRUE & MARST %in% 1:2)]
  return(dt)
}

#' Identify target egos' guardians in childhood years
#' @param dt data.table with IPUMS data
#' @return data.table with appended column: GUARDIAN
#' What it does:
#' - identifies which rows should be included for the GUARDIANS and GUARDIANS_SUM aggregations
#' - every row who is identified as MOMLOC/MOMLOC2/POPLOC/POPLOC2 for target ego (via ego_PERNUM) gets T, all else gets F
identify_guardians <- function(dt) {
  # make all guardian pointers the same within household, which identifies the guardians
  # of target ego ONLY
  dt[, NMOMLOC := MOMLOC[CBPERNUM == ego_PERNUM],
     by = .(CBSERIAL, ego_PERNUM)]
  
  dt[, NMOMLOC2 := MOMLOC2[CBPERNUM == ego_PERNUM],
     by = .(CBSERIAL, ego_PERNUM)]
  
  dt[, NPOPLOC := POPLOC[CBPERNUM == ego_PERNUM],
     by = .(CBSERIAL, ego_PERNUM)]
  
  dt[, NPOPLOC2 := POPLOC2[CBPERNUM == ego_PERNUM],
     by = .(CBSERIAL, ego_PERNUM)]
  
  # set TRUE if is a guardian of target ego in childhood
  dt[, GUARDIAN := (CBPERNUM == NMOMLOC | CBPERNUM == NMOMLOC2 | CBPERNUM == NPOPLOC | CBPERNUM == NPOPLOC2) & (is_child == TRUE)]
  
  dt[, `:=`(NMOMLOC = NULL, NMOMLOC2 = NULL, NPOPLOC = NULL, NPOPLOC2 = NULL)]
  
  return(dt)
}

#' Identify target egos' married guardians in childhood years
#' @param dt data.table with IPUMS data, assumes GUARDIAN is a column in the data
#' @return data.table with appended column: MARRIED_GUARDIAN
#' What it does:
#' - identifies which rows should be included for the MARRIED_GUARDIANS and MARRIED_GUARDIANS_SUM aggregations
#' - excludes, from GUARDIANS column, any guardian whose partner is an unmarried HOH and where the ego's RELATED value is "child" only.
identify_married_guardians <- function(dt) {
  # Create a reference table for household-target ego variables
  hoh_info <- dt[RELATED == 101, .(hoh_MARST = unique(MARST), hoh_PERNUM = unique(CBPERNUM)), 
                 by = .(CBSERIAL, ego_PERNUM)] # Gather HOH info
  
  # Merge HOH info back to the original table
  dt[hoh_info, on = .(CBSERIAL, ego_PERNUM), 
     `:=`(hoh_MARST = i.hoh_MARST, hoh_PERNUM = i.hoh_PERNUM)]
  
  # Create household-targetego-level variable which takes on the RELATED value for that target ego
  dt[, ego_RELATED := RELATED[CBPERNUM == ego_PERNUM], by = .(CBSERIAL, ego_PERNUM)]
  
  # Use combined logical operation to create MARRIED_GUARDIAN
  dt[, MARRIED_GUARDIAN := (GUARDIAN) & !(SPLOC == hoh_PERNUM & (!hoh_MARST %in% 1:2) & ego_RELATED == 301)]
  
  dt[, `:=`(hoh_PERNUM = NULL, hoh_MARST = NULL, ego_RELATED = NULL, NPOPLOC2 = NULL)]
  
  return(dt)
}

#' Create all aggregation columns
#'
#' @param dt data.table after running prep_for_aggregation function
#' @return data.table with 7 new aggregation columns
#' 
#' What it does:
#' - applies all the helper functions to identify rows included for each aggregation type
create_aggregation_columns <- function(dt) {
  dt[, HOUSEHOLD := TRUE]
  identify_family(dt)
  identify_egos(dt)
  identify_partners(dt)
  identify_spouses(dt)
  identify_guardians(dt)
  identify_married_guardians(dt)
  
  return(dt)
}

#' Multiply income values by aggregation columns
#' @param dt data.table after running create_aggregation_columns function
#' @param income_def name of current income definition
#' @return data.table with 7 new appended aggregation columns
#' What it does:
#' - applies 7 aggregation types across given income column
#' - handles NAs versus 0s for each aggregation type.
create_income_columns <- function(dt, income_def, cols) {
  # Loop through the specified columns to create new columns based on income_def
  for (col in cols) {
    
    # Check if the column exists
    if (!col %in% names(dt)) {
      warning(paste(col, "not found in the dataset. Skipping."))
      next
    }
    # Create the new column name
    new_col_name <- paste0(income_def, "_", col)
    
    # Create the new binary column based on the specified logic
    dt[, (new_col_name) := ifelse(get(col) == TRUE, 
                                  ifelse(is.na(get(income_def)), 0, get(income_def)), 
                                  NA)]
  }
  
  # Return the modified data.table with the new columns
  return(dt)
}

#' Check if spouse < partner < ego
#' 
#' @param dt data.table of ego-level distribution result
#' @return T/F if check is passed.
#' What it does:
#' - Checks if number of valid spouse values <= valid partner values <= valid ego values
#' - And also if partner_sum == spouses_sum == ego
check_relationship_completeness <- function(dt) {
  # Get column names that contain "SPOUSE", "PARTNER", and "EGO"
  spouse_cols <- names(dt)[grepl("SPOUSE$", names(dt))]
  partner_cols <- names(dt)[grepl("PARTNER$", names(dt))]
  ego_cols <- names(dt)[grepl("EGO", names(dt))]
  spouses_cols <- names(dt)[grepl("SPOUSES", names(dt))]
  partners_cols <- names(dt)[grepl("PARTNERS", names(dt))]
  
  # Count non-NA values for SPOUSE columns
  spouse_non_na_count <- sum(sapply(spouse_cols, function(col) sum(!is.na(dt[[col]]))))
  
  # Count non-NA values for PARTNER columns
  partner_non_na_count <- sum(sapply(partner_cols, function(col) sum(!is.na(dt[[col]]))))
  
  # Count non-NA values for EGO columns
  ego_non_na_count <- sum(sapply(ego_cols, function(col) sum(!is.na(dt[[col]]))))
  
  # Count non-NA values for SPOUSES_SUM columns
  spouses_non_na_count <- sum(sapply(spouses_cols, function(col) sum(!is.na(dt[[col]]))))
  
  # Count non-NA values for PARTNERS_SUM columns
  partners_non_na_count <- sum(sapply(partners_cols, function(col) sum(!is.na(dt[[col]]))))
  
  # Check the conditions
  result <- spouse_non_na_count <= partner_non_na_count && partner_non_na_count <= ego_non_na_count && partners_non_na_count == spouses_non_na_count && spouses_non_na_count == ego_non_na_count
  
  print(paste0("Number of valid ego: ", ego_non_na_count))
  print(paste0("Number of valid partner: ", partner_non_na_count))
  print(paste0("Number of valid spouse: ", spouse_non_na_count))
  print(paste0("Number of valid partners: ", partners_non_na_count))
  print(paste0("Number of valid spouses: ", spouses_non_na_count))
  
  return(result)
}

#' Check if married-guardians <= guardians
#' 
#' @param dtg data.table of guardian-level distribution result
#' @param dtm data.table of married-guardian level distribution result
#' @return T/F if check is passed.
#' What it does:
#' - Checks if number of married-guardians <= guardians
check_guardians_count <- function(dtg, dtm) {
  print(paste0("Number of valid guardians: ", nrow(dtg)))
  print(paste0("Number of valid married guardians: ", nrow(dtm)))
  return(nrow(dtm) <= nrow(dtg))
}

#' Check if guardians_sum == guardians_married_sum
#' 
#' @param dt data.table of ego-level distribution result
#' @return T/F if check is passed.
check_guardians_sum_count <- function(dt) {
  gs_cols <- names(dt)[grepl("GUARDIANS_SUM", names(dt))]
  gs_non_na_count <- sum(sapply(gs_cols, function(col) sum(!is.na(dt[[col]]))))
  
  gm_cols <- names(dt)[grepl("GUARDIANS_MARRIED_SUM", names(dt))]
  gm_non_na_count <- sum(sapply(gm_cols, function(col) sum(!is.na(dt[[col]]))))
  
  print(paste0("Number of valid guardians_sum: ", gs_non_na_count))
  print(paste0("Number of valid guardians_married_sum: ", gm_non_na_count))
  
  return(gs_non_na_count == gm_non_na_count)
}

#' Check if household and family columnes are all complete
#' 
#' @param dt data.table of ego-level distribution result
#' @return T/F if check is passed.
#' What it does:
#' - Checks if there are no NA values in any household or family aggregation
check_complete_cases <- function(dt) {
  hh_cols <- names(dt)[grepl("HOUSEHOLD", names(dt))]
  hh_non_na_count <- sum(sapply(hh_cols, function(col) sum(!is.na(dt[[col]]))))
  
  family_cols <- names(dt)[grepl("FAMILY", names(dt))]
  fam_non_na_count <- sum(sapply(family_cols, function(col) sum(!is.na(dt[[col]]))))
  
  print(paste0("Number of valid households: ", hh_non_na_count))
  print(paste0("Number of valid families: ", fam_non_na_count))
  
  return(dt %>% 
           select(contains("HOUSEHOLD") | contains("FAMILY")) %>% 
           complete.cases() %>% all())
}

#' Check if logic for aggregation outputs are as expected
#' 
#' @param dt.list list of three distribution items to be returned in main aggregation function
#' @return null. Just prints the results of the checks
#' 
#' What it does:
#' - runs some logical checks for the results of the aggregation
check_all_cases <- function(dt.list) {
  guardians <- dt.list[[1]]
  mguardians <- dt.list[[2]]
  egos <- dt.list[[3]]
  
  # household and family
  if(check_complete_cases(egos)){
    cat("✓ Test 1 Passed\n")
  } else {
    cat("✗ Test 1 Failed\n")
  }
  
  # partner(s), spouse(s), ego
  if(check_relationship_completeness(egos)){
    cat("✓ Test 2 Passed\n")
  } else {
    cat("✗ Test 2 Failed\n")
  }
  
  # guardians and married guardians sum
  if(check_guardians_sum_count(egos)){
    cat("✓ Test 3 Passed\n")
  } else {
    cat("✗ Test 3 Failed\n")
  }
  
  # guardians and married guardians dist
  if(check_guardians_count(guardians, mguardians)){
    cat("✓ Test 4 Passed\n")
  } else {
    cat("✗ Test 4 Failed\n")
  }
}

# ==============================================================================
# 4. MAIN AGGREGATION FUNCTION
# ==============================================================================

#' Apply all aggregations efficiently
#' 
#' @param dt Prepared data (output from prep_for_aggregation)
#' @param income_defs Vector of income definition names as strings
#' @return List with: guardians (guardian-level distributions), married_guardians (married-guardians-level distributions), egos (ego-level with all other aggregations)
#' 
#' What it does:
#' For each income definition (xearn, xemp, xinc, xcominc):
#'   - household: sum across entire household
#'   - family: sum within family unit
#'   - ego: individual's own income (adult egos only)
#'   - partner: partner's income (if exists)
#'   - spouse: spouse's income (if married)
#'   - partners_sum: ego + partner
#'   - spouses_sum: ego + spouse
#'   - guardians_sum: sum of all guardian incomes
#'   - guardians_married_sum: sum of guardians incomes excluding non-married partners of HOHs
#'   - guardian: all guardians of target egos
#'   - married_guardian: all guardians of target egos excluding non-married partners of HOHs 
#' 
#' Results in 4 income defs × 11 aggregations = 44 new columns over 3 data.tables
apply_all_aggregations_optimized <- function(dt, income_defs) {
  
  cat("\n=== OPTIMIZED AGGREGATION ===\n")
  
  # create binary aggregation inclusion columns
  create_aggregation_columns(dt)
  
  # create results list
  # primed output of three items; guardians dist, married guardians dist, ego dist,
  guardians_dist <- dt[GUARDIAN == TRUE, .(CBSERIAL, ego_PERNUM, CBPERNUM, ego_BIRTHYR, PERWT)]
  
  mguardians_dist <- dt[MARRIED_GUARDIAN == TRUE, .(CBSERIAL, ego_PERNUM, CBPERNUM, ego_BIRTHYR, PERWT)]
  
  ego_dist <- dt[CBPERNUM == ego_PERNUM, .(CBSERIAL, CBPERNUM, ego_BIRTHYR, PERWT)]
  
  cols <- c("HOUSEHOLD", "FAMILY", "EGO", "PARTNER", "SPOUSE", "GUARDIAN", "MARRIED_GUARDIAN")
  
  # start looping through each income definition
  cat("Processing income definitions...\n")
  pb <- progress_bar$new(
    format = "  [:bar] :current/:total (:percent) eta: :eta",
    total = length(income_defs)
  )
  
  for (income_def in income_defs) {
    # 1: run create_income_columns to multiply aggregation inclusions by income values
    dt <- create_income_columns(dt, income_def, cols)
    
    # 2: process guardians distribution
    current <- dt[!is.na(paste0(income_def, "_GUARDIAN")), 
                  .(CBSERIAL, ego_PERNUM, CBPERNUM, income_value = get(paste0(income_def, "_GUARDIAN")))]
    
    setnames(current, "income_value", paste0(income_def, "_GUARDIAN"))    
    
    guardians_dist <- merge(guardians_dist, current, by = c("CBSERIAL", "ego_PERNUM", "CBPERNUM"), all.x = TRUE)
    
    # 3. process married guardians distribution
    current <- dt[!is.na(paste0(income_def, "_MARRIED_GUARDIAN")), 
                  .(CBSERIAL, ego_PERNUM, CBPERNUM, income_value = get(paste0(income_def, "_MARRIED_GUARDIAN")))]
    
    setnames(current, "income_value", paste0(income_def, "_MARRIED_GUARDIAN"))    
    
    mguardians_dist <- merge(mguardians_dist, current, by = c("CBSERIAL", "ego_PERNUM", "CBPERNUM"), all.x = TRUE)
    
    # 4. process all other ego-level distribution aggregations
    col_names <- paste0(income_def, "_", cols)
    
    current <- dt[, lapply(col_names, function(col) {
      if(all(is.na(get(col)))){ # if no one in the household-ego is to be included, return NA
        return(NA_real_)
      } else{
        # else, sum all valid values for the specified income column
        sum(get(col), na.rm = TRUE)
      }
    }), by = .(CBSERIAL, ego_PERNUM)]

    # Rename columns
    setnames(current, old = names(current)[-c(1, 2)], new = paste0(income_def, "_", cols))
    
    setnames(current, old = c(paste0(income_def, "_GUARDIAN"), paste0(income_def, "_MARRIED_GUARDIAN")),
             new = c(paste0(income_def, "_GUARDIANS_SUM"), paste0(income_def, "_GUARDIANS_MARRIED_SUM")))
    
    setnames(current, old = "ego_PERNUM", new = "CBPERNUM")

    ego_dist <- merge(ego_dist, current, by = c("CBSERIAL", "CBPERNUM"), all.x = TRUE)
    
    # 5. calculate partners_sum and spouses_sum
    ego_column <- paste0(income_def, "_EGO")
    partner_column <- paste0(income_def, "_PARTNER")
    spouse_column <- paste0(income_def, "_SPOUSE")
    
    ego_dist[, 
             paste0(income_def, "_PARTNERS_SUM") := 
               ifelse(
                 is.na(get(ego_column)) & is.na(get(partner_column)), 
                 NA_real_, 
                 rowSums(.SD, na.rm = TRUE)
               ),
             .SD = c(ego_column, partner_column)]
    
    ego_dist[, 
             paste0(income_def, "_SPOUSES_SUM") := 
               ifelse(
                 is.na(get(ego_column)) & is.na(get(spouse_column)), 
                 NA_real_, 
                 rowSums(.SD, na.rm = TRUE)
               ),
             .SD = c(ego_column, spouse_column)]
  }
  
  cat("\nAggregation complete!\n")
  
  # 6. Return results
  results <- list(guardians = guardians_dist,
                  married_guardians = mguardians_dist,
                  egos = ego_dist)
  
  # print if logical checks passed
  check_all_cases(results)

  return(results)
}

# ==============================================================================
# 5. RANKING FUNCTIONS
# ==============================================================================

#' Determine valid aggregations for a given age
#' 
#' @param age_in_year Age of person in the observation year
#' @return Vector of valid aggregation names
#' 
#' What it does:
#' - Ages ≤17 (children): household, family, guardians_sum, guardians, guardians_married
#' - Ages ≥24 (adults): household, family, partners_sum, married_sum, ego, partner, spouse
#' - Ages 18-23: excluded (return empty vector)
get_valid_aggregations <- function(age_in_year) {
  if (age_in_year <= AGE_FILTER_LOWER) {
    return(c("HOUSEHOLD", "FAMILY", "GUARDIANS_SUM", "GUARDIANS_MARRIED_SUM", "GUARDIAN", "MARRIED_GUARDIAN"))
  } else if (age_in_year >= AGE_FILTER_UPPER) {
    return(c("HOUSEHOLD", "FAMILY", "PARTNERS_SUM", "SPOUSES_SUM", "EGO", "PARTNER", "SPOUSE"))
  } else {
    return(character(0))
  }
}

#' Fast empirical ranking using weighted percentiles
#' 
#' @param data data.table with columns: income, ego_BIRTHYR, weights
#' @param income_var Name of income column
#' @param weight_var Name of weight column
#' @param interval Income interval size (dollars)
#' @return data.table: cohort, income_value, percentile_rank
#' 
#' What it does:
#' 1. For each birth cohort separately:
#' 2. Sort individuals by income
#' 3. Calculate cumulative weights to get empirical CDF
#' 4. Create income grid (every $100 from min to max)
#' 5. Interpolate to get percentile rank for each grid point
#' 
#' This is much faster than quantile regression and produces very similar results
rank_empirical_fast <- function(data, income_var, weight_var, interval = 100) {
  dt <- copy(data)
  dt[, `:=`(income_temp = get(income_var), weight_temp = get(weight_var))]
  dt <- dt[!is.na(income_temp)]
  
  if (nrow(dt) == 0) {
    return(data.table(cohort = numeric(), income_value = numeric(), percentile_rank = numeric()))
  }
  
  result <- dt[, {
    # Sort by income
    ord <- order(income_temp)
    inc_sorted <- income_temp[ord]
    wt_sorted <- weight_temp[ord]
    
    # Calculate cumulative percentiles
    cum_wt <- cumsum(wt_sorted)
    total_wt <- sum(wt_sorted)
    pct_rank <- cum_wt / total_wt
    
    # Create income grid
    min_inc <- floor(min(inc_sorted) / interval) * interval
    max_inc <- ceiling(max(inc_sorted) / interval) * interval
    income_grid <- seq(min_inc, max_inc, by = interval)
    
    # Interpolate percentiles for grid points
    grid_percentiles <- approx(
      x = inc_sorted,
      y = pct_rank,
      xout = income_grid,
      method = "linear",
      rule = 2
    )$y
    
    data.table(income_value = income_grid, percentile_rank = grid_percentiles)
    
  }, by = ego_BIRTHYR]
  
  setnames(result, "ego_BIRTHYR", "cohort")
  return(result)
}

#' Rank all income-aggregation combinations for a year
#' 
#' @param agg_result List from apply_all_aggregations_optimized
#' @param year_val Year being processed
#' @param cohort_range Vector of birth years
#' @param income_defs Vector of income definition names
#' @param weight_var Weight variable name
#' @param interval Income interval size
#' @param age_filter_lower Lower age threshold
#' @param age_filter_upper Upper age threshold
#' @return data.table with all results
#' 
#' What it does:
#' 1. Determine which cohort-aggregation combinations are valid
#'    (based on age filters)
#' 2. For each valid income-aggregation combination:
#'    - Extract the relevant data
#'    - Rank within each cohort
#' 3. Combine all results with metadata
#' 
#' Output columns: year, cohort, income_definition, aggregation_level, 
#'                 income_value, percentile_rank
rank_all_combinations_fast <- function(agg_result, year_val, cohort_range, income_defs, 
                                       weight_var, interval = 100,
                                       age_filter_lower = 17, age_filter_upper = 24) {
  
  egos_data <- agg_result$egos
  guardians_data <- agg_result$guardians
  mguardians_data <- agg_result$married_guardians
  
  all_results <- data.table()
  
  # Determine valid cohort-aggregation combinations
  valid_combinations <- data.table()
  
  for (cohort in cohort_range) {
    age_in_year <- year_val - cohort
    valid_aggs <- get_valid_aggregations(age_in_year)
    
    if (length(valid_aggs) > 0) {
      valid_combinations <- rbindlist(list(
        valid_combinations,
        data.table(cohort = cohort, aggregation = valid_aggs)
      ))
    }
  }
  
  # Get all income-aggregation combinations
  income_agg_cols <- CJ(
    income_def = income_defs,
    aggregation = unique(valid_combinations$aggregation)
  )[, col_name := paste0(income_def, "_", aggregation)]
  
  cat("Ranking income distributions...\n")
  pb <- progress_bar$new(
    format = "  [:bar] :current/:total (:percent) eta: :eta",
    total = nrow(income_agg_cols)
  )
  
  for (i in 1:nrow(income_agg_cols)) {
    income_def <- income_agg_cols$income_def[i]
    agg_level <- income_agg_cols$aggregation[i]
    col_name <- income_agg_cols$col_name[i]
    
    pb$tick()
    
    cohorts_for_agg <- valid_combinations[aggregation == agg_level, cohort]
    if (length(cohorts_for_agg) == 0) next
    
    # Special handling for "guardian" and "married_guardian aggregations
    if (agg_level == "GUARDIAN") {
      guardian_data <- guardians_data[[paste0(income_def, "_GUARDIAN")]]
      
      if (is.null(guardian_data) || length(guardian_data)==0) {
        next
      }
      
      data_subset <- guardians_data[ego_BIRTHYR %in% cohorts_for_agg, 
                                    .(ego_BIRTHYR, income = get(col_name), PERWT = PERWT)]
      
    } else if (agg_level == "MARRIED_GUARDIAN") {
      mguardian_data <- mguardians_data[[paste0(income_def, "_MARRIED_GUARDIAN")]]
      
      if (is.null(mguardian_data) || length(mguardian_data)==0) {
        next
      }
      
      data_subset <- mguardians_data[ego_BIRTHYR %in% cohorts_for_agg, 
                                   .(ego_BIRTHYR, income = get(col_name), PERWT = PERWT)]
      
    } else {
      # Regular aggregations (use main ego-level data)
      if (!col_name %in% names(egos_data)) {
        next
      }
      
      data_subset <- egos_data[ego_BIRTHYR %in% cohorts_for_agg, 
                               .(ego_BIRTHYR, income = get(col_name), PERWT = PERWT)]
    }
    
    if (nrow(data_subset) == 0 || all(is.na(data_subset$income))) {
      next
    }
    
    # Rank using fast empirical method
    rank_table <- rank_empirical_fast(data_subset, "income", "PERWT", interval)
    
    if (nrow(rank_table) == 0) {
      next
    }
    
    # Add metadata
    rank_table[, ':='(
      year = year_val,
      income_definition = income_def,
      aggregation_level = agg_level
    )]
    setcolorder(rank_table, c("year", "cohort", "income_definition", "aggregation_level", 
                              "income_value", "percentile_rank"))
    
    all_results <- rbindlist(list(all_results, rank_table))
  }
  
  return(all_results)
}

# ==============================================================================
# 6. WORKFLOW FUNCTIONS
# ==============================================================================

#' Process a single year
#' 
#' @param ipums_filepath Path to IPUMS .csv file
#' @param year_val Year (numeric)
#' @param cohort_range Birth years to analyze
#' @param income_definitions Table with income definitions
#' @param interval Income interval for ranking
#' @param age_filter_lower Lower age threshold
#' @param age_filter_upper Upper age threshold
#' @param sample_n Optional sample size for testing
#' @return data.table with all ranked results for this year
#' 
#' What it does (complete pipeline):
#' 1. Load raw IPUMS data
#' 2. Prepare data (filter, create income variables)
#' 3. Apply all aggregations
#' 4. Rank all combinations
#' 5. Return results
process_single_year <- function(ipums_filepath, year_val, cohort_range, income_definitions, 
                                interval = 100,
                                age_filter_lower = 17, age_filter_upper = 24,
                                sample_n = NULL) {
  cat("\n========================================\n")
  cat("Processing year:", year_val, "\n")
  if (!is.null(sample_n)) {
    cat("TESTING MODE: Sample of", sample_n, "households\n")
  }
  cat("========================================\n")
  
  # Load data
  cat("\nLoading data...\n")
  t1 <- Sys.time()
  raw_data <- load_data_csv(ipums_filepath)
  t2 <- Sys.time()
  cat("Loaded", nrow(raw_data), "rows in", round(difftime(t2, t1, units = "secs"), 1), "sec\n")
  
  # Prepare data
  cat("\nPreparing data...\n")
  t1 <- Sys.time()
  prepped_data <- prep_for_aggregation(raw_data, cohort_range, income_definitions,
                                       age_filter_lower, age_filter_upper, sample_n)
  t2 <- Sys.time()
  cat("Prepared", nrow(prepped_data), "rows in", round(difftime(t2, t1, units = "secs"), 1), "sec\n")
  
  # Apply aggregations
  cat("\n")
  t1 <- Sys.time()
  agg_result <- apply_all_aggregations_optimized(prepped_data, income_definitions$name)
  t2 <- Sys.time()
  cat("Aggregations completed in", round(difftime(t2, t1, units = "secs"), 1), "sec\n")
  
  # Rank all combinations
  cat("\n")
  t1 <- Sys.time()
  results <- rank_all_combinations_fast(
    agg_result = agg_result,
    year_val = year_val,
    cohort_range = cohort_range,
    income_defs = income_definitions$name,
    weight_var = "PERWT",
    interval = interval,
    age_filter_lower = age_filter_lower,
    age_filter_upper = age_filter_upper
  )
  t2 <- Sys.time()
  cat("Ranking completed in", round(difftime(t2, t1, units = "secs"), 1), "sec\n")
  
  cat("\nYear", year_val, "complete! Generated", nrow(results), "rows\n")
  
  return(results)
}

#' Process all years in parallel
#' 
#' @param ipums_files Named vector of file paths
#' @param output_dir Directory to save results
#' @param n_cores Number of cores (default: auto-detect)
#' @param ... Other parameters passed to process_single_year
#' @return List of summary info for each year
#' 
#' What it does:
#' 1. Sets up parallel processing workers
#' 2. Processes each year independently in parallel
#' 3. Saves results to compressed CSV files
#' 4. Returns summary statistics
process_all_years_parallel <- function(ipums_files, output_dir, 
                                       n_cores = NULL,
                                       cohort_range, income_definitions, 
                                       interval = 100,
                                       age_filter_lower = 17, age_filter_upper = 24) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 1
  }
  
  cat("========================================\n")
  cat("PARALLEL PROCESSING\n")
  cat("========================================\n")
  cat("Years:", length(ipums_files), "\n")
  cat("Cores:", n_cores, "\n")
  cat("Output:", output_dir, "\n")
  cat("========================================\n\n")
  
  # Set up parallel processing
  plan(multisession, workers = n_cores)
  
  overall_start <- Sys.time()
  
  # Process years in parallel
  results_list <- future_map(names(ipums_files), function(year_val) {
    filepath <- ipums_files[[year_val]]
    year_start <- Sys.time()
    
    # Process year
    results <- process_single_year(
      ipums_filepath = filepath,
      year_val = as.numeric(year_val),
      cohort_range = cohort_range,
      income_definitions = income_definitions,
      interval = interval,
      age_filter_lower = age_filter_lower,
      age_filter_upper = age_filter_upper,
      sample_n = NULL
    )
    
    year_end <- Sys.time()
    time_taken <- as.numeric(difftime(year_end, year_start, units = "mins"))
    
    # Save results
    output_file <- file.path(output_dir, paste0("income_ranks_", year_val, ".csv.gz"))
    fwrite(results, output_file)
    
    list(
      year = year_val,
      output_file = output_file,
      n_rows = nrow(results),
      time_minutes = time_taken
    )
  }, .options = furrr_options(seed = TRUE), .progress = TRUE)
  
  overall_end <- Sys.time()
  total_time <- as.numeric(difftime(overall_end, overall_start, units = "mins"))
  
  cat("\n========================================\n")
  cat("PARALLEL PROCESSING COMPLETE!\n")
  cat("Total time:", round(total_time, 1), "minutes\n")
  cat("========================================\n")
  
  # Close parallel workers
  plan(sequential)
  
  # Save summary
  summary_df <- data.frame(
    year = sapply(results_list, function(x) x$year),
    n_rows = sapply(results_list, function(x) x$n_rows),
    time_minutes = sapply(results_list, function(x) x$time_minutes),
    output_file = sapply(results_list, function(x) x$output_file)
  )
  
  summary_file <- file.path(output_dir, "processing_summary.csv")
  fwrite(summary_df, summary_file)
  cat("Summary saved to:", summary_file, "\n")
  
  return(results_list)
}

#' Create aggregations only
#' 
#' @param data original data containing columns for serial, pernum, family pointers, income, year etc.
#' @param income_definitions Table with income definitions
#' @param cohort_range Birth years to analyze
#' @param age_filter_lower Lower age threshold
#' @param age_filter_upper Upper age threshold
#' @return data.table with appended columns for all aggregation-definition pairs
#' 
#' What it does:
#' 1. Preps data and verifies presences of necessary columns
#' 2. Applies aggregations and multiplies by income definitions
#' 3. Returns original data with new appended columns for each aggregation-definition, linked by target ego
append_income_aggregations <- function(data, income_definitions, 
                                       cohort_range = 1983:2000,
                                       age_filter_lower = 17, age_filter_upper = 24){
  setDT(data)
  
  required_cols <- c("INCWAGE", "INCBUS00", "INCTOT", "INCSS", "INCRETIR", "INCINVST",
                     "CBSERIAL", "CBPERNUM", "BIRTHYR", "AGE", "YEAR", "MARST", "RELATED",
                     "FAMUNIT", "SPLOC", "MOMLOC", "POPLOC", "MOMLOC2", "POPLOC2", "PERWT")
  
  if (!all(required_cols %in% names(data))) {
    missing <- setdiff(required_cols, names(data))
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }
  
  prepped_data <- prep_for_aggregation(data, cohort_range, income_definitions,
                                       age_filter_lower, age_filter_upper, sample_n=NULL)
  
  agg_result <- apply_all_aggregations_optimized(prepped_data, income_definitions$name)
  
  egos <- agg_result$egos %>% select(starts_with("CB") | starts_with("x"))
  
  guardians <- agg_result$guardians %>% select(CBSERIAL, CBPERNUM, ego_PERNUM, starts_with("x"))
  
  # reshape
  guardians[, pernum_seq := seq_len(.N), by = .(CBSERIAL, ego_PERNUM)]
  
  # Reshape wide
  guardians_w <- dcast(guardians,
                       CBSERIAL + ego_PERNUM ~ pernum_seq,
                       value.var = c("CBPERNUM", "xearn_GUARDIAN", "xemp_GUARDIAN", "xinc_GUARDIAN", "xcominc_GUARDIAN"))
  
  # rename values
  guardians_w %>% 
    rename(CBPERNUM = ego_PERNUM,
           GUARDIAN1 = CBPERNUM_1,
           GUARDIAN2 = CBPERNUM_2) -> guardians_w
  
  married_guardians <- agg_result$married_guardians %>% select(CBSERIAL, CBPERNUM, ego_PERNUM, starts_with("x"))
  
  married_guardians[, pernum_seq := seq_len(.N), by = .(CBSERIAL, ego_PERNUM)]
  
  mguardians_w <- dcast(married_guardians,
                        CBSERIAL + ego_PERNUM ~ pernum_seq,
                        value.var = c("CBPERNUM", "xearn_MARRIED_GUARDIAN", "xemp_MARRIED_GUARDIAN", "xinc_MARRIED_GUARDIAN", "xcominc_MARRIED_GUARDIAN"))
  
  # rename values
  mguardians_w %>% 
    rename(CBPERNUM = ego_PERNUM,
           MARRIED_GUARDIAN1 = CBPERNUM_1,
           MARRIED_GUARDIAN2 = CBPERNUM_2) -> mguardians_w
  
  # join back to original data
  result <- egos[data,
                 on = .(CBSERIAL, CBPERNUM)]
  
  result <- guardians_w[result,
                        on = .(CBSERIAL, CBPERNUM)]
  
  result <- mguardians_w[result,
                         on = .(CBSERIAL, CBPERNUM)]
  
  cat(paste0(ncol(result)-ncol(data), " income columns appended:"), 
      paste(setdiff(names(result), names(data)), 
            collapse = ", "), "\n")
  
  return(result)
}
