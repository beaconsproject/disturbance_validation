# Script to validate disturbance attributes
# Updated: 2024-04-04

setwd('validation')

# Load function from github
source('functions/test_types.R')
source('functions/validate_features.R')

# Function arguments
## data_pkg = name of data package
## lyr_line = name of linear disturbance layer
## lyr_poly = name of areal disturbance layer
## output_dir = location of output directory (will be created if it doesn't exist)

# Test data
dta <- 'input/fda10ab_disturb_errors.gpkg'

# test_types function
test_types(data_pkg=dta, lyr_line='sd_line', lyr_poly='sd_poly')

# validate_features function
validate_features(data_pkg=dta, lyr_line='sd_line', lyr_poly='sd_poly', output_dir='output/')
