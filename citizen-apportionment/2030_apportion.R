# ==============================================================================
# 2030 CONGRESSIONAL APPORTIONMENT PROJECTION ANALYSIS
# ==============================================================================
# 
# This script projects 2030 state populations using multiple statistical methods
# and compares apportionment results based on total vs. citizen-only populations.
#
# Key Features:
# - Multiple projection methods (linear, polynomial, weighted, moving average)
# - Model validation and accuracy assessment
# - Uncertainty quantification across methods
# - Visualization of trends and projections
#
# Data Sources:
# - American Community Survey (ACS) B05001 table (Citizenship Status)
# - Years: 2018, 2019, 2021, 2022, 2023 (2020 unavailable)
# - 2020 Decennial Census for baseline apportionment
#
# Author: [Your Name]
# Last Updated: [Date]
# ==============================================================================

# Clear environment and suppress scientific notation
rm(list = ls(all = TRUE))
options(scipen = 999)

# ==============================================================================
# LOAD LIBRARIES AND DATA SOURCES
# ==============================================================================

source('https://raw.githubusercontent.com/jcervas/R-Functions/refs/heads/main/get_acs/get_acs.R')
fips <- read.csv("https://raw.githubusercontent.com/jcervas/Data/refs/heads/master/fips.csv")

# ==============================================================================
# CORE FUNCTIONS
# ==============================================================================

# ------------------------------------------------------------------------------
# Apportionment Function
# ------------------------------------------------------------------------------
# Distributes congressional seats among states using various methods

apportion_values <- function(values, target_sum, method = c("Hill-Huntington", "Jefferson", "Adams", "Webster"), initial_seats = 0) {
  method <- match.arg(method)
  n <- length(values)

  if (target_sum < n * initial_seats) {
    stop("Target sum must be at least the total number of initial seats.")
  }

  # Assign initial seats
  allocations <- rep(initial_seats, n)
  remaining <- target_sum - sum(allocations)

  # Define priority function based on method
  priority_function <- switch(method,
    "Jefferson" = function(pop, seats) pop / (seats + 1),
    "Adams" = function(pop, seats) pop / seats,
    "Webster" = function(pop, seats) pop / (seats + 0.5),
    "Hill-Huntington" = function(pop, seats) pop / sqrt(seats * (seats + 1))
  )

  for (i in seq_len(remaining)) {
    priorities <- priority_function(values, allocations)
    winner <- which.max(priorities)
    allocations[winner] <- allocations[winner] + 1
  }

  return(allocations)
}

# ------------------------------------------------------------------------------
# Individual Apportionment Methods
# ------------------------------------------------------------------------------

#' Hill-Huntington apportionment method
hill_huntington <- function(populations, total_seats) {
  return(apportion_values(populations, total_seats, method = "Hill-Huntington", initial_seats = 1))
}

#' Jefferson apportionment method
jefferson <- function(populations, total_seats) {
  return(apportion_values(populations, total_seats, method = "Jefferson", initial_seats = 1))
}

#' Adams apportionment method
adams <- function(populations, total_seats) {
  return(apportion_values(populations, total_seats, method = "Adams", initial_seats = 1))
}

#' Webster apportionment method
webster <- function(populations, total_seats) {
  return(apportion_values(populations, total_seats, method = "Webster", initial_seats = 1))
}

# ------------------------------------------------------------------------------
# ACS Data Collection Function
# ------------------------------------------------------------------------------
# Standardized function to collect and clean ACS citizenship data

get_acs_citizenship_data <- function(year, geography = "state") {
  cat("Fetching ACS", year, "data for", geography, "...\n")
  
  # Get ACS data
  acs_data <- get_acs(
    table = "B05001", 
    year = year, 
    geography = geography, 
    var_types = "E",
    acs_year = 1
  )
  
  # Remove DC and Puerto Rico for state-level data
  if (geography == "state") {
    acs_data <- subset(acs_data, !(acs_data$GEOID %in% c("11", "72")))
  }
  
  # Calculate citizenship variables
  result <- data.frame(
    NAME = acs_data$NAME,
    GEOID = acs_data$GEOID,
    stringsAsFactors = FALSE
  )
  
  # Add year-specific columns
  result[[paste0("CITIZEN", substr(year, 3, 4))]] <- acs_data$B05001_001E - acs_data$B05001_006E
  result[[paste0("NOT_CIT", substr(year, 3, 4))]] <- acs_data$B05001_006E
  result[[paste0("TOTAL", substr(year, 3, 4))]] <- acs_data$B05001_001E
  
  return(result)
}

# ==============================================================================
# DATA COLLECTION
# ==============================================================================

# ------------------------------------------------------------------------------
# Congressional District Data (2023 only)
# ------------------------------------------------------------------------------

cat("=== COLLECTING CONGRESSIONAL DISTRICT DATA ===\n")

acs_2023_cd <- get_acs(
  table = "B05001", 
  year = 2023, 
  geography = "cd", 
  var_types = "E",
  acs_year = 1
)

b05001_cd <- data.frame(
  NAME = acs_2023_cd$NAME,
  GEOID = acs_2023_cd$GEOID,
  STATE = sub(".*,\\s*", "", acs_2023_cd$NAME), # Extract state from NAME
  # Extract district number or "At Large" from NAME
  District = ifelse(
    grepl("At Large", acs_2023_cd$NAME, ignore.case = TRUE), 1,
    ifelse(grepl("District", acs_2023_cd$NAME, ignore.case = TRUE),
           as.numeric(sub(".*District\\s(\\d+).*", "\\1", acs_2023_cd$NAME)), NA)
  ),
  CITIZEN23 = acs_2023_cd$B05001_001E - acs_2023_cd$B05001_006E,
  NOT_CIT23 = acs_2023_cd$B05001_006E,
  TOTAL23 = acs_2023_cd$B05001_001E,
  stringsAsFactors = FALSE
)

# Remove non-states
b05001_cd <- subset(b05001_cd, !(STATE %in% c("Puerto Rico", "District of Columbia")))

# ------------------------------------------------------------------------------
# State-Level Data Collection (Multiple Years)
# ------------------------------------------------------------------------------

cat("=== COLLECTING STATE-LEVEL DATA FOR MULTIPLE YEARS ===\n")

# Define years for analysis (2020 not available in ACS)
analysis_years <- c(2018, 2019, 2021, 2022, 2023)

# Collect data for each year
state_data_list <- list()
for (year in analysis_years) {
  state_data_list[[as.character(year)]] <- get_acs_citizenship_data(year, "state")
}

# Merge all years together
cat("Merging state data across years...\n")
state_acs <- state_data_list[["2018"]]
for (year in analysis_years[-1]) {
  state_acs <- merge(state_acs, state_data_list[[as.character(year)]], 
                     by = c("NAME", "GEOID"), all = TRUE)
}

cat("Data collection complete. States:", nrow(state_acs), "\n\n")
# ==============================================================================
# PROJECTION FUNCTIONS
# ==============================================================================

# ------------------------------------------------------------------------------
# Two-Point Projection Methods
# ------------------------------------------------------------------------------

#' Linear growth projection using two data points
project_linear <- function(start, end, start_year, end_year, target_year) {
  annual_growth <- (end - start) / (end_year - start_year)
  years_to_target <- target_year - end_year
  as.integer(end + annual_growth * years_to_target)
}

#' Exponential growth projection using two data points
project_exponential <- function(start, end, start_year, end_year, target_year) {
  growth_rate <- (end / start)^(1 / (end_year - start_year)) - 1
  years_to_target <- target_year - end_year
  as.integer(end * (1 + growth_rate)^years_to_target)
}

#' Average annual percent change projection using two data points
project_aapc <- function(start, end, start_year, end_year, target_year) {
  aapc <- (end / start)^(1 / (end_year - start_year)) - 1
  years_to_target <- target_year - end_year
  as.integer(end * (1 + aapc)^years_to_target)
}

# ------------------------------------------------------------------------------
# Multi-Point Projection Methods
# ------------------------------------------------------------------------------

#' Linear regression projection using multiple data points
project_linear_regression <- function(values, years, target_year) {
  if (length(values) != length(years) || length(values) < 2) {
    stop("Values and years must have the same length and at least 2 points")
  }
  
  lm_model <- lm(values ~ years)
  predicted <- predict(lm_model, newdata = data.frame(years = target_year))
  as.integer(max(0, predicted))  # Ensure non-negative
}

#' Polynomial regression projection using multiple data points
project_polynomial <- function(values, years, target_year, degree = 2) {
  if (length(values) != length(years) || length(values) < (degree + 1)) {
    stop(paste("Need at least", degree + 1, "data points for degree", degree, "polynomial"))
  }
  
  poly_model <- lm(values ~ poly(years, degree, raw = TRUE))
  predicted <- predict(poly_model, newdata = data.frame(years = target_year))
  as.integer(max(0, predicted))  # Ensure non-negative
}

#' Weighted trend projection (gives more weight to recent data)
project_weighted_trend <- function(values, years, target_year, weight_factor = 2) {
  if (length(values) != length(years) || length(values) < 2) {
    stop("Values and years must have the same length and at least 2 points")
  }
  
  # Create weights that increase exponentially for more recent years
  weights <- weight_factor^(0:(length(years)-1))
  
  # Weighted linear regression
  lm_model <- lm(values ~ years, weights = weights)
  predicted <- predict(lm_model, newdata = data.frame(years = target_year))
  as.integer(max(0, predicted))  # Ensure non-negative
}

#' Moving average of growth rates projection
project_moving_average <- function(values, years, target_year, window = 2) {
  if (length(values) != length(years) || length(values) < 2) {
    stop("Values and years must have the same length and at least 2 points")
  }
  
  # Calculate year-over-year growth rates
  growth_rates <- numeric(length(values) - 1)
  for (i in 2:length(values)) {
    growth_rates[i-1] <- (values[i] / values[i-1]) - 1
  }
  
  # Use moving average of recent growth rates
  recent_rates <- tail(growth_rates, window)
  avg_growth_rate <- mean(recent_rates)
  
  # Project from the last known value
  years_to_project <- target_year - max(years)
  last_value <- values[length(values)]
  
  projected <- last_value * (1 + avg_growth_rate)^years_to_project
  as.integer(max(0, projected))
}

# ------------------------------------------------------------------------------
# Projection Application Function
# ------------------------------------------------------------------------------

#' Apply multiple projection methods to a single state's data
apply_projections_to_state <- function(total_values, citizen_values, years_observed, 
                                       target_year, state_name = "") {
  
  results <- list()
  
  # 2-point methods (using most recent two years)
  recent_years <- tail(years_observed, 2)
  recent_total <- tail(total_values, 2)
  recent_citizen <- tail(citizen_values, 2)
  
  results$total_linear_2pt <- project_linear(recent_total[1], recent_total[2], 
                                             recent_years[1], recent_years[2], target_year)
  results$citizen_linear_2pt <- project_linear(recent_citizen[1], recent_citizen[2], 
                                               recent_years[1], recent_years[2], target_year)
  
  results$total_exp_2pt <- project_exponential(recent_total[1], recent_total[2], 
                                               recent_years[1], recent_years[2], target_year)
  results$citizen_exp_2pt <- project_exponential(recent_citizen[1], recent_citizen[2], 
                                                 recent_years[1], recent_years[2], target_year)
  
  results$total_aapc_2pt <- project_aapc(recent_total[1], recent_total[2], 
                                         recent_years[1], recent_years[2], target_year)
  results$citizen_aapc_2pt <- project_aapc(recent_citizen[1], recent_citizen[2], 
                                           recent_years[1], recent_years[2], target_year)
  
  # Multi-point methods
  tryCatch({
    results$total_linear_reg <- project_linear_regression(total_values, years_observed, target_year)
    results$citizen_linear_reg <- project_linear_regression(citizen_values, years_observed, target_year)
  }, error = function(e) {
    results$total_linear_reg <<- NA
    results$citizen_linear_reg <<- NA
  })
  
  tryCatch({
    results$total_polynomial <- project_polynomial(total_values, years_observed, target_year, degree = 3)
    results$citizen_polynomial <- project_polynomial(citizen_values, years_observed, target_year, degree = 3)
  }, error = function(e) {
    tryCatch({
      results$total_polynomial <<- project_polynomial(total_values, years_observed, target_year, degree = 2)
      results$citizen_polynomial <<- project_polynomial(citizen_values, years_observed, target_year, degree = 2)
    }, error = function(e2) {
      results$total_polynomial <<- NA
      results$citizen_polynomial <<- NA
    })
  })
  
  tryCatch({
    results$total_weighted <- project_weighted_trend(total_values, years_observed, target_year)
    results$citizen_weighted <- project_weighted_trend(citizen_values, years_observed, target_year)
  }, error = function(e) {
    results$total_weighted <<- NA
    results$citizen_weighted <<- NA
  })
  
  tryCatch({
    results$total_moving_avg <- project_moving_average(total_values, years_observed, target_year, window = 3)
    results$citizen_moving_avg <- project_moving_average(citizen_values, years_observed, target_year, window = 3)
  }, error = function(e) {
    results$total_moving_avg <<- NA
    results$citizen_moving_avg <<- NA
  })
  
  return(results)
}

# ==============================================================================
# PROJECTION ANALYSIS
# ==============================================================================

cat("=== APPLYING PROJECTION METHODS ===\n")

# Configuration
years_observed <- analysis_years  # c(2018, 2019, 2021, 2022, 2023)
target_year <- 2030
projection_methods <- c("linear_2pt", "exp_2pt", "aapc_2pt", "linear_reg", 
                        "polynomial", "weighted", "moving_avg")

# Initialize projection columns
for (method in projection_methods) {
  state_acs[[paste0("total2030_", method)]] <- NA
  state_acs[[paste0("citizen2030_", method)]] <- NA
}

# Apply projections to each state
cat("Calculating projections for", nrow(state_acs), "states...\n")

for (i in 1:nrow(state_acs)) {
  if (i %% 10 == 0) cat("Processing state", i, "of", nrow(state_acs), "\n")
  
  # Extract data for current state
  total_values <- c(state_acs$TOTAL18[i], state_acs$TOTAL19[i], state_acs$TOTAL21[i], 
                    state_acs$TOTAL22[i], state_acs$TOTAL23[i])
  citizen_values <- c(state_acs$CITIZEN18[i], state_acs$CITIZEN19[i], state_acs$CITIZEN21[i], 
                      state_acs$CITIZEN22[i], state_acs$CITIZEN23[i])
  
  # Apply all projection methods
  projections <- apply_projections_to_state(total_values, citizen_values, 
                                            years_observed, target_year, 
                                            state_acs$NAME[i])
  
  # Store results
  state_acs$total2030_linear_2pt[i] <- projections$total_linear_2pt
  state_acs$citizen2030_linear_2pt[i] <- projections$citizen_linear_2pt
  state_acs$total2030_exp_2pt[i] <- projections$total_exp_2pt
  state_acs$citizen2030_exp_2pt[i] <- projections$citizen_exp_2pt
  state_acs$total2030_aapc_2pt[i] <- projections$total_aapc_2pt
  state_acs$citizen2030_aapc_2pt[i] <- projections$citizen_aapc_2pt
  state_acs$total2030_linear_reg[i] <- projections$total_linear_reg
  state_acs$citizen2030_linear_reg[i] <- projections$citizen_linear_reg
  state_acs$total2030_polynomial[i] <- projections$total_polynomial
  state_acs$citizen2030_polynomial[i] <- projections$citizen_polynomial
  state_acs$total2030_weighted[i] <- projections$total_weighted
  state_acs$citizen2030_weighted[i] <- projections$citizen_weighted
  state_acs$total2030_moving_avg[i] <- projections$total_moving_avg
  state_acs$citizen2030_moving_avg[i] <- projections$citizen_moving_avg
}

# Calculate non-citizen projections for each method
cat("Calculating non-citizen projections...\n")
for (method in projection_methods) {
  total_col <- paste0("total2030_", method)
  citizen_col <- paste0("citizen2030_", method)
  noncitizen_col <- paste0("noncitizen2030_", method)
  
  state_acs[[noncitizen_col]] <- state_acs[[total_col]] - state_acs[[citizen_col]]
}

# Set default projections (using weighted method as recommended)
state_acs$total2030_proj <- state_acs$total2030_weighted
state_acs$citizen2030_proj <- state_acs$citizen2030_weighted
state_acs$noncitizen2030_proj <- state_acs$noncitizen2030_weighted

cat("Projection calculations complete.\n\n")

# ==============================================================================
# RESULTS ANALYSIS AND COMPARISON
# ==============================================================================

cat("=== ANALYZING PROJECTION RESULTS ===\n")

# ------------------------------------------------------------------------------
# Method Comparison and Uncertainty Assessment
# ------------------------------------------------------------------------------

# Create projection comparison matrix
projection_comparison <- data.frame(
  state = state_acs$NAME,
  total_linear_2pt = state_acs$total2030_linear_2pt,
  total_linear_reg = state_acs$total2030_linear_reg,
  total_polynomial = state_acs$total2030_polynomial,
  total_weighted = state_acs$total2030_weighted,
  total_moving_avg = state_acs$total2030_moving_avg,
  citizen_linear_2pt = state_acs$citizen2030_linear_2pt,
  citizen_linear_reg = state_acs$citizen2030_linear_reg,
  citizen_polynomial = state_acs$citizen2030_polynomial,
  citizen_weighted = state_acs$citizen2030_weighted,
  citizen_moving_avg = state_acs$citizen2030_moving_avg,
  stringsAsFactors = FALSE
)

# Calculate method differences from linear regression baseline
state_acs$total_diff_poly_vs_linear <- state_acs$total2030_polynomial - state_acs$total2030_linear_reg
state_acs$total_diff_weighted_vs_linear <- state_acs$total2030_weighted - state_acs$total2030_linear_reg
state_acs$citizen_diff_poly_vs_linear <- state_acs$citizen2030_polynomial - state_acs$citizen2030_linear_reg
state_acs$citizen_diff_weighted_vs_linear <- state_acs$citizen2030_weighted - state_acs$citizen2030_linear_reg

# ------------------------------------------------------------------------------
# Apportionment Calculations
# ------------------------------------------------------------------------------

cat("Calculating congressional apportionments...\n")

# Total population-based apportionments
apportion_2030_linear_2pt <- apportion_values(state_acs$total2030_linear_2pt, target_sum = 435, initial_seats = 1)
apportion_2030_linear_reg <- apportion_values(state_acs$total2030_linear_reg, target_sum = 435, initial_seats = 1)
apportion_2030_polynomial <- apportion_values(state_acs$total2030_polynomial, target_sum = 435, initial_seats = 1)
apportion_2030_weighted <- apportion_values(state_acs$total2030_weighted, target_sum = 435, initial_seats = 1)
apportion_2030_moving_avg <- apportion_values(state_acs$total2030_moving_avg, target_sum = 435, initial_seats = 1)

# Citizen-only apportionments
apportion_2030_citizen_linear_2pt <- apportion_values(state_acs$citizen2030_linear_2pt, target_sum = 435, initial_seats = 1)
apportion_2030_citizen_linear_reg <- apportion_values(state_acs$citizen2030_linear_reg, target_sum = 435, initial_seats = 1)
apportion_2030_citizen_polynomial <- apportion_values(state_acs$citizen2030_polynomial, target_sum = 435, initial_seats = 1)
apportion_2030_citizen_weighted <- apportion_values(state_acs$citizen2030_weighted, target_sum = 435, initial_seats = 1)
apportion_2030_citizen_moving_avg <- apportion_values(state_acs$citizen2030_moving_avg, target_sum = 435, initial_seats = 1)

# Comprehensive apportionment results
apportion_2030_projection <- data.frame(
  state = state_acs$NAME,
  total_linear_2pt = state_acs$total2030_linear_2pt,
  total_linear_reg = state_acs$total2030_linear_reg,
  total_polynomial = state_acs$total2030_polynomial,
  total_weighted = state_acs$total2030_weighted,
  total_moving_avg = state_acs$total2030_moving_avg,
  apportion_linear_2pt = apportion_2030_linear_2pt,
  apportion_linear_reg = apportion_2030_linear_reg,
  apportion_polynomial = apportion_2030_polynomial,
  apportion_weighted = apportion_2030_weighted,
  apportion_moving_avg = apportion_2030_moving_avg,
  apportion_citizen_linear_2pt = apportion_2030_citizen_linear_2pt,
  apportion_citizen_linear_reg = apportion_2030_citizen_linear_reg,
  apportion_citizen_polynomial = apportion_2030_citizen_polynomial,
  apportion_citizen_weighted = apportion_2030_citizen_weighted,
  apportion_citizen_moving_avg = apportion_2030_citizen_moving_avg,
  stringsAsFactors = FALSE
)

# Calculate uncertainty metrics
apportion_2030_projection$method_variance_total <- apply(
  apportion_2030_projection[,c("apportion_linear_reg", "apportion_polynomial", 
                               "apportion_weighted", "apportion_moving_avg")], 
  1, var, na.rm = TRUE
)

apportion_2030_projection$method_variance_citizen <- apply(
  apportion_2030_projection[,c("apportion_citizen_linear_reg", "apportion_citizen_polynomial", 
                               "apportion_citizen_weighted", "apportion_citizen_moving_avg")], 
  1, var, na.rm = TRUE
)

# Display uncertainty analysis
cat("\n=== PROJECTION UNCERTAINTY ANALYSIS ===\n")
cat("States with highest projection uncertainty (total population):\n")
top_uncertain_total <- apportion_2030_projection[
  order(apportion_2030_projection$method_variance_total, decreasing = TRUE)[1:10], 
  c("state", "method_variance_total")
]
print(top_uncertain_total)

cat("\nStates with highest projection uncertainty (citizen population):\n")
top_uncertain_citizen <- apportion_2030_projection[
  order(apportion_2030_projection$method_variance_citizen, decreasing = TRUE)[1:10], 
  c("state", "method_variance_citizen")
]
print(top_uncertain_citizen)

# ==============================================================================
# DATA SUMMARY AND EXPORT
# ==============================================================================

cat("\n=== CREATING SUMMARY DATASETS ===\n")

# Create clean summary dataset
state_acs_summary <- data.frame(
  state_name = state_acs$NAME,
  state_geoid = state_acs$GEOID,
  # Historical data
  total_2018 = state_acs$TOTAL18,
  citizen_2018 = state_acs$CITIZEN18,
  not_citizen_2018 = state_acs$NOT_CIT18,
  total_2019 = state_acs$TOTAL19,
  citizen_2019 = state_acs$CITIZEN19,
  not_citizen_2019 = state_acs$NOT_CIT19,
  total_2021 = state_acs$TOTAL21,
  citizen_2021 = state_acs$CITIZEN21,
  not_citizen_2021 = state_acs$NOT_CIT21,
  total_2022 = state_acs$TOTAL22,
  citizen_2022 = state_acs$CITIZEN22,
  not_citizen_2022 = state_acs$NOT_CIT22,
  total_2023 = state_acs$TOTAL23,
  citizen_2023 = state_acs$CITIZEN23,
  not_citizen_2023 = state_acs$NOT_CIT23,
  # Projections (default: weighted method)
  total_2030_proj = state_acs$total2030_proj,
  citizen_2030_proj = state_acs$citizen2030_proj,
  noncitizen_2030_proj = state_acs$noncitizen2030_proj,
  stringsAsFactors = FALSE
)

# Create summary of all projection methods
projection_totals <- projection_comparison

cat("Summary datasets created successfully.\n")

# ==============================================================================
# MODEL VALIDATION AND VISUALIZATION
# ==============================================================================

cat("\n=== MODEL VALIDATION ===\n")

# ------------------------------------------------------------------------------
# Cross-Validation Function
# ------------------------------------------------------------------------------

#' Evaluate model accuracy using cross-validation
evaluate_model_accuracy <- function(values, years, test_year_index, method = "linear_reg") {
  if (length(values) < 3) return(NA)
  
  # Use all data except test year to predict test year
  train_values <- values[-test_year_index]
  train_years <- years[-test_year_index]
  test_value <- values[test_year_index]
  test_year <- years[test_year_index]
  
  predicted <- switch(method,
    "linear_reg" = project_linear_regression(train_values, train_years, test_year),
    "polynomial" = project_polynomial(train_values, train_years, test_year, 
                                     degree = min(2, length(train_values)-1)),
    "weighted" = project_weighted_trend(train_values, train_years, test_year),
    "moving_avg" = project_moving_average(train_values, train_years, test_year)
  )
  
  # Return percentage error
  if (is.na(predicted) || test_value == 0) return(NA)
  abs((predicted - test_value) / test_value) * 100
}

# ------------------------------------------------------------------------------
# Model Accuracy Assessment
# ------------------------------------------------------------------------------

cat("Calculating model accuracy using cross-validation...\n")

accuracy_results <- data.frame(
  state = state_acs$NAME,
  total_linear_reg_error = NA,
  total_polynomial_error = NA,
  total_weighted_error = NA,
  citizen_linear_reg_error = NA,
  citizen_polynomial_error = NA,
  citizen_weighted_error = NA,
  stringsAsFactors = FALSE
)

for (i in 1:nrow(state_acs)) {
  total_vals <- c(state_acs$TOTAL18[i], state_acs$TOTAL19[i], state_acs$TOTAL21[i], 
                  state_acs$TOTAL22[i], state_acs$TOTAL23[i])
  citizen_vals <- c(state_acs$CITIZEN18[i], state_acs$CITIZEN19[i], state_acs$CITIZEN21[i], 
                    state_acs$CITIZEN22[i], state_acs$CITIZEN23[i])
  
  # Test accuracy by predicting 2023 using 2018-2022 data
  accuracy_results$total_linear_reg_error[i] <- evaluate_model_accuracy(total_vals, years_observed, 5, "linear_reg")
  accuracy_results$total_polynomial_error[i] <- evaluate_model_accuracy(total_vals, years_observed, 5, "polynomial")
  accuracy_results$total_weighted_error[i] <- evaluate_model_accuracy(total_vals, years_observed, 5, "weighted")
  
  accuracy_results$citizen_linear_reg_error[i] <- evaluate_model_accuracy(citizen_vals, years_observed, 5, "linear_reg")
  accuracy_results$citizen_polynomial_error[i] <- evaluate_model_accuracy(citizen_vals, years_observed, 5, "polynomial")
  accuracy_results$citizen_weighted_error[i] <- evaluate_model_accuracy(citizen_vals, years_observed, 5, "weighted")
}

# Calculate and display average errors
avg_errors <- colMeans(accuracy_results[,-1], na.rm = TRUE)
cat("\n=== AVERAGE PREDICTION ERRORS (% error) ACROSS ALL STATES ===\n")
print(round(avg_errors, 2))

# ------------------------------------------------------------------------------
# Visualization: Florida Case Study
# ------------------------------------------------------------------------------

cat("\n=== CREATING VISUALIZATION ===\n")

create_state_projection_plot <- function(state_name = "Florida") {
  state_data <- state_acs[state_acs$NAME == state_name, ]
  if (nrow(state_data) == 0) {
    cat("State", state_name, "not found.\n")
    return()
  }
  
  years_full <- 2018:2030
  years_known <- analysis_years
  
  # Known data
  total_known <- c(state_data$TOTAL18, state_data$TOTAL19, state_data$TOTAL21, 
                   state_data$TOTAL22, state_data$TOTAL23)
  citizen_known <- c(state_data$CITIZEN18, state_data$CITIZEN19, state_data$CITIZEN21, 
                     state_data$CITIZEN22, state_data$CITIZEN23)
  
  # Create interpolated projections for smooth curves
  total_interp_linear <- approx(c(years_known, 2030), 
                               c(total_known, state_data$total2030_linear_reg), 
                               xout = years_full)$y
  total_interp_weighted <- approx(c(years_known, 2030), 
                                 c(total_known, state_data$total2030_weighted), 
                                 xout = years_full)$y
  total_interp_poly <- approx(c(years_known, 2030), 
                             c(total_known, state_data$total2030_polynomial), 
                             xout = years_full)$y
  
  citizen_interp_linear <- approx(c(years_known, 2030), 
                                 c(citizen_known, state_data$citizen2030_linear_reg), 
                                 xout = years_full)$y
  citizen_interp_weighted <- approx(c(years_known, 2030), 
                                   c(citizen_known, state_data$citizen2030_weighted), 
                                   xout = years_full)$y
  citizen_interp_poly <- approx(c(years_known, 2030), 
                               c(citizen_known, state_data$citizen2030_polynomial), 
                               xout = years_full)$y
  
  # Create plot
  plot(years_full, total_interp_linear, type = "n", 
       ylim = range(c(total_interp_linear, total_interp_weighted, total_interp_poly,
                      citizen_interp_linear, citizen_interp_weighted, citizen_interp_poly), na.rm = TRUE),
       ylab = "Population", xlab = "Year", 
       main = paste(state_name, "Population Projections: Multiple Methods (2018-2030)"))
  
  # Known data points
  points(years_known, total_known, col = "blue", pch = 16, cex = 1.2)
  points(years_known, citizen_known, col = "red", pch = 16, cex = 1.2)
  
  # Connect known data points
  lines(years_known, total_known, col = "blue", lwd = 2, lty = 1)
  lines(years_known, citizen_known, col = "red", lwd = 2, lty = 1)
  
  # Projections from 2023 to 2030
  proj_years <- 2023:2030
  proj_indices <- which(years_full %in% proj_years)
  
  # Total population projections
  lines(proj_years, total_interp_linear[proj_indices], col = "blue", lwd = 2, lty = 2)
  lines(proj_years, total_interp_weighted[proj_indices], col = "darkblue", lwd = 2, lty = 3)
  lines(proj_years, total_interp_poly[proj_indices], col = "lightblue", lwd = 2, lty = 4)
  
  # Citizen population projections  
  lines(proj_years, citizen_interp_linear[proj_indices], col = "red", lwd = 2, lty = 2)
  lines(proj_years, citizen_interp_weighted[proj_indices], col = "darkred", lwd = 2, lty = 3)
  lines(proj_years, citizen_interp_poly[proj_indices], col = "pink", lwd = 2, lty = 4)
  
  # Add vertical line at 2023 to separate known from projected
  abline(v = 2023, col = "gray", lty = 2)
  
  # Add note about missing 2020 data
  text(2020, max(total_known) * 0.95, "2020\n(missing)", col = "gray", cex = 0.8)
  
  legend("topleft",
         legend = c("Total (Known)", "Total (Linear)", "Total (Weighted)", "Total (Polynomial)",
                    "Citizen (Known)", "Citizen (Linear)", "Citizen (Weighted)", "Citizen (Polynomial)"),
         col = c("blue", "blue", "darkblue", "lightblue", "red", "red", "darkred", "pink"),
         lwd = 2, lty = c(1, 2, 3, 4, 1, 2, 3, 4), cex = 0.7)
}

# Create the visualization
create_state_projection_plot("Florida")


# ==============================================================================
# APPORTIONMENT CALCULATIONS
# ==============================================================================

cat("\n=== CALCULATING APPORTIONMENT ===\n")

#' Apply apportionment methods to projected populations
calculate_state_apportionment <- function(state_acs, projection_column) {
  # Use only 50 states (exclude DC, PR, territories)
  state_50 <- state_acs[!(state_acs$NAME %in% c("District of Columbia", "Puerto Rico")), ]
  
  if (!projection_column %in% names(state_50)) {
    cat("Warning: Column", projection_column, "not found in data.\n")
    return(NULL)
  }
  
  populations <- state_50[[projection_column]]
  names(populations) <- state_50$NAME
  
  # Remove any NA or invalid values
  valid_indices <- !is.na(populations) & populations > 0
  if (sum(valid_indices) == 0) {
    cat("Warning: No valid population data for", projection_column, "\n")
    return(NULL)
  }
  
  populations <- populations[valid_indices]
  
  cat("Calculating apportionment with", length(populations), "states for", projection_column, "...\n")
  
  # Use the apportion_values function directly with Hill-Huntington method
  results <- list()
  
  tryCatch({
    results$hill_huntington <- apportion_values(populations, 435, method = "Hill-Huntington", initial_seats = 1)
    names(results$hill_huntington) <- names(populations)
  }, error = function(e) {
    cat("Error in Hill-Huntington apportionment:", e$message, "\n")
    results$hill_huntington <<- NULL
  })
  
  tryCatch({
    results$jefferson <- apportion_values(populations, 435, method = "Jefferson", initial_seats = 1)
    names(results$jefferson) <- names(populations)
  }, error = function(e) {
    cat("Error in Jefferson apportionment:", e$message, "\n")
    results$jefferson <<- NULL
  })
  
  tryCatch({
    results$adams <- apportion_values(populations, 435, method = "Adams", initial_seats = 1)
    names(results$adams) <- names(populations)
  }, error = function(e) {
    cat("Error in Adams apportionment:", e$message, "\n")
    results$adams <<- NULL
  })
  
  tryCatch({
    results$webster <- apportion_values(populations, 435, method = "Webster", initial_seats = 1)
    names(results$webster) <- names(populations)
  }, error = function(e) {
    cat("Error in Webster apportionment:", e$message, "\n")
    results$webster <<- NULL
  })
  
  return(results)
}

# ------------------------------------------------------------------------------
# Current 2020 Apportionment (for comparison)
# ------------------------------------------------------------------------------

cat("Reading current 2020 apportionment for comparison...\n")

# Create 2020 apportionment mapping (from official Census data)
current_2020_reps <- c(
  "Alabama" = 7, "Alaska" = 1, "Arizona" = 9, "Arkansas" = 4, "California" = 52,
  "Colorado" = 8, "Connecticut" = 5, "Delaware" = 1, "Florida" = 28, "Georgia" = 14,
  "Hawaii" = 2, "Idaho" = 2, "Illinois" = 17, "Indiana" = 9, "Iowa" = 4,
  "Kansas" = 4, "Kentucky" = 6, "Louisiana" = 6, "Maine" = 2, "Maryland" = 8,
  "Massachusetts" = 9, "Michigan" = 13, "Minnesota" = 8, "Mississippi" = 4, "Missouri" = 8,
  "Montana" = 2, "Nebraska" = 3, "Nevada" = 4, "New Hampshire" = 2, "New Jersey" = 12,
  "New Mexico" = 3, "New York" = 26, "North Carolina" = 14, "North Dakota" = 1, "Ohio" = 15,
  "Oklahoma" = 5, "Oregon" = 6, "Pennsylvania" = 17, "Rhode Island" = 2, "South Carolina" = 7,
  "South Dakota" = 1, "Tennessee" = 9, "Texas" = 38, "Utah" = 4, "Vermont" = 1,
  "Virginia" = 11, "Washington" = 10, "West Virginia" = 2, "Wisconsin" = 8, "Wyoming" = 1
)

current_mapping <- current_2020_reps

# ------------------------------------------------------------------------------
# Calculate Apportionments for All Projection Methods
# ------------------------------------------------------------------------------

# For each projection method, calculate all apportionment methods
projection_methods <- c("total2030_linear_reg", "total2030_polynomial", 
                       "total2030_weighted", "citizen2030_linear_reg", 
                       "citizen2030_polynomial", "citizen2030_weighted")

apportionment_results <- list()

for (proj_method in projection_methods) {
  cat("Calculating apportionment for projection method:", proj_method, "\n")
  
  apport_result <- calculate_state_apportionment(state_acs, proj_method)
  if (!is.null(apport_result)) {
    apportionment_results[[proj_method]] <- apport_result
  }
}

# ------------------------------------------------------------------------------
# Summary Results Table
# ------------------------------------------------------------------------------

create_apportionment_summary <- function(apportionment_results, current_mapping) {
  if (length(apportionment_results) == 0) {
    cat("No apportionment results to summarize.\n")
    return(NULL)
  }
  
  # Get all unique states from results
  all_states <- unique(unlist(lapply(apportionment_results, function(proj) {
    if (is.null(proj)) return(NULL)
    unique(unlist(lapply(proj, function(method_result) {
      if (is.null(method_result)) return(NULL)
      names(method_result)
    })))
  })))
  
  if (length(all_states) == 0) {
    cat("No valid state results found in apportionment data.\n")
    return(NULL)
  }
  
  # Create summary data frame
  summary_df <- data.frame(
    State = all_states,
    Current_2020 = NA,
    stringsAsFactors = FALSE
  )
  
  # Add current 2020 values
  for (i in 1:nrow(summary_df)) {
    state_name <- summary_df$State[i]
    if (state_name %in% names(current_mapping)) {
      summary_df$Current_2020[i] <- current_mapping[[state_name]]
    }
  }
  
  # Add projected apportionments
  for (proj_name in names(apportionment_results)) {
    proj_data <- apportionment_results[[proj_name]]
    if (is.null(proj_data)) next
    
    for (method_name in names(proj_data)) {
      method_results <- proj_data[[method_name]]
      if (is.null(method_results)) next
      
      col_name <- paste0(proj_name, "_", method_name)
      summary_df[[col_name]] <- NA
      
      for (i in 1:nrow(summary_df)) {
        state_name <- summary_df$State[i]
        if (state_name %in% names(method_results)) {
          summary_df[[col_name]][i] <- method_results[[state_name]]
        }
      }
    }
  }
  
  return(summary_df)
}

cat("Creating comprehensive apportionment summary...\n")
apportionment_summary <- create_apportionment_summary(apportionment_results, current_mapping)

if (!is.null(apportionment_summary)) {
  cat("\n=== APPORTIONMENT SUMMARY (First 10 states) ===\n")
  print(head(apportionment_summary, 10))
  
  # Show states with biggest changes
  if ("Current_2020" %in% names(apportionment_summary)) {
    # Calculate changes for Hill-Huntington method with total population linear regression
    total_hh_col <- "total2030_linear_reg_hill_huntington"
    if (total_hh_col %in% names(apportionment_summary)) {
      apportionment_summary$change_hh_total_linear <- 
        apportionment_summary[[total_hh_col]] - apportionment_summary$Current_2020
      
      biggest_gains <- apportionment_summary[order(-apportionment_summary$change_hh_total_linear), ]
      biggest_losses <- apportionment_summary[order(apportionment_summary$change_hh_total_linear), ]
      
      cat("\n=== BIGGEST PROJECTED GAINS (Hill-Huntington, Total Pop, Linear Regression) ===\n")
      print(head(biggest_gains[, c("State", "Current_2020", total_hh_col, "change_hh_total_linear")], 5))
      
      cat("\n=== BIGGEST PROJECTED LOSSES (Hill-Huntington, Total Pop, Linear Regression) ===\n")
      print(head(biggest_losses[, c("State", "Current_2020", total_hh_col, "change_hh_total_linear")], 5))
    }
  }
}

# ==============================================================================
# ADDITIONAL ANALYSIS AND OUTPUT
# ==============================================================================

cat("\n=== ADDITIONAL ANALYSIS ===\n")

# Save results to CSV
if (!is.null(apportionment_summary)) {
  output_file <- "2030_apportionment_projections.csv"
  write.csv(apportionment_summary, output_file, row.names = FALSE)
  cat("Apportionment results saved to:", output_file, "\n")
}

# Summary statistics
cat("\n=== PROJECTION SUMMARY STATISTICS ===\n")
summary_stats <- data.frame(
  Population = c("Total", "Citizen"),
  Method = c("Linear Regression", "Linear Regression"),
  Min_2030 = c(min(state_acs$total2030_linear_reg, na.rm = TRUE),
               min(state_acs$citizen2030_linear_reg, na.rm = TRUE)),
  Max_2030 = c(max(state_acs$total2030_linear_reg, na.rm = TRUE),
               max(state_acs$citizen2030_linear_reg, na.rm = TRUE)),
  Mean_2030 = c(mean(state_acs$total2030_linear_reg, na.rm = TRUE),
                mean(state_acs$citizen2030_linear_reg, na.rm = TRUE)),
  stringsAsFactors = FALSE
)
print(summary_stats)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Data years used:", paste(analysis_years, collapse = ", "), "\n")
cat("Projection methods applied: Linear Regression, Polynomial, Weighted Trend\n")
cat("Apportionment methods applied: Hill-Huntington, Jefferson, Adams, Webster\n")
cat("Results include", nrow(state_acs), "states and territories\n")

