# Income-Rankings-IPUMS

## Description

This repository contains R scripts for calculating cohort-specific percentile rankings of income using the 2000 Decennial census and 2005-2023 American Community Survey data. It also contains the output of the program.

## Files

### Data Files
- `output.zip` - 20 .csv files for each year (2000, 2005-2023) containing income-rank copula for that year.
- `data.zip` - 20 .csv files for each year (2000, 2005-2023) containing public IPUMS data.

### R Scripts
- `define_rank_income.R` - Script containing all helper functions for program.
- `income_rank_main.R` - Main script to run production.

## Usage

1. Download `data.zip` and both R scripts.
2. Change appropriate directories in `income_rank_main.R` including to source `define_rank_income.R`
3. Run function to process all years.

## Contact
Renee Louis - rlouis@stanford.edu
