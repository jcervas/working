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

# ==============================================================================
# CONFIGURATION: Set the years to include in analysis
# ==============================================================================
# Define years for analysis (2020 not available in ACS due to COVID impacts)
# Modify this range to include/exclude years as needed
analysis_years <- seq(2006, 2023, by = 1)
analysis_years <- analysis_years[!analysis_years %in% c(2020)]  # Exclude 2020 due to COVID impacts

# ==============================================================================
# DATA COLLECTION
# ==============================================================================

# Collect data for each year
state_data_list <- list()
for (year in analysis_years) {
  state_data_list[[as.character(year)]] <- get_acs_citizenship_data(year, "state")
}

# Merge all years together
cat("Merging state data across years...\n")
# Use the first year in analysis_years as the base for merging
first_year <- as.character(min(analysis_years))
state_acs <- state_data_list[[first_year]]
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
  
  # Apply demographic realism constraints
  last_value <- values[length(values)]
  years_to_project <- target_year - max(years)
  
  # Calculate implied annual growth rate
  if (last_value > 0) {
    implied_annual_growth <- (predicted / last_value)^(1/years_to_project) - 1
    
    # Cap annual growth rates at realistic demographic limits
    # Most US states don't sustain >2% annual population growth long-term
    max_annual_growth <- 0.02  # 2% per year
    min_annual_growth <- -0.015  # -1.5% per year (decline states)
    
    if (implied_annual_growth > max_annual_growth) {
      # Use capped growth rate
      predicted <- last_value * (1 + max_annual_growth)^years_to_project
      cat("Capping linear regression growth for demographic realism\n")
    } else if (implied_annual_growth < min_annual_growth) {
      # Use capped decline rate
      predicted <- last_value * (1 + min_annual_growth)^years_to_project
      cat("Capping linear regression decline for demographic realism\n")
    }
  }
  
  as.integer(max(0, predicted))  # Ensure non-negative
}

#' Polynomial regression projection using multiple data points
project_polynomial <- function(values, years, target_year, degree = 2) {
  if (length(values) != length(years) || length(values) < (degree + 1)) {
    # Fall back to linear regression if not enough points
    return(project_linear_regression(values, years, target_year))
  }
  
  # Limit degree to prevent overfitting and extreme extrapolation
  max_degree <- min(degree, 2, length(values) - 1)
  
  tryCatch({
    poly_model <- lm(values ~ poly(years, max_degree, raw = TRUE))
    predicted <- predict(poly_model, newdata = data.frame(years = target_year))
    
    # Check for reasonable bounds - if projection is more than 50% different from
    # linear trend, use a constrained approach
    linear_pred <- project_linear_regression(values, years, target_year)
    if (abs(predicted - linear_pred) / linear_pred > 0.5) {
      # Use weighted combination of polynomial and linear
      predicted <- 0.7 * linear_pred + 0.3 * predicted
    }
    
    as.integer(max(0, predicted))  # Ensure non-negative
  }, error = function(e) {
    # Fall back to linear regression on error
    return(project_linear_regression(values, years, target_year))
  })
}

#' Weighted trend projection (gives more weight to recent data)
project_weighted_trend <- function(values, years, target_year, weight_factor = 3) {
  if (length(values) != length(years) || length(values) < 2) {
    stop("Values and years must have the same length and at least 2 points")
  }
  
  # Create weights that increase exponentially for more recent years
  # Use higher weight factor for stronger recency bias
  weights <- weight_factor^(0:(length(years)-1))
  
  # Also try a more recent-focused approach: use only last 3 points if we have 5
  if (length(values) >= 4) {
    # Focus on most recent 3-4 years with very high weights
    recent_values <- tail(values, 3)
    recent_years <- tail(years, 3)
    recent_weights <- c(1, 3, 9)  # Much higher weight on most recent
    
    # Weighted linear regression on recent data
    recent_model <- lm(recent_values ~ recent_years, weights = recent_weights)
    predicted <- predict(recent_model, newdata = data.frame(recent_years = target_year))
  } else {
    # Use all data with exponential weighting
    lm_model <- lm(values ~ years, weights = weights)
    predicted <- predict(lm_model, newdata = data.frame(years = target_year))
  }
  
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

#' Recent trend projection (emphasizes last 2-3 data points)
project_recent_trend <- function(values, years, target_year) {
  if (length(values) != length(years) || length(values) < 3) {
    # Fall back to simple linear projection with last two points
    if (length(values) >= 2) {
      return(project_linear(values[length(values)-1], values[length(values)], 
                           years[length(years)-1], years[length(years)], target_year))
    } else {
      return(NA)
    }
  }
  
  # Use only the most recent 3 points for projection
  recent_n <- min(3, length(values))
  recent_values <- tail(values, recent_n)
  recent_years <- tail(years, recent_n)
  
  # Simple linear regression on recent data
  recent_model <- lm(recent_values ~ recent_years)
  predicted <- predict(recent_model, newdata = data.frame(recent_years = target_year))
  
  as.integer(max(0, predicted))
}

#' Robust linear regression using median-based trend
project_robust_linear <- function(values, years, target_year) {
  if (length(values) != length(years) || length(values) < 3) {
    return(project_linear_regression(values, years, target_year))
  }
  
  # Calculate year-over-year changes
  changes <- diff(values)
  time_gaps <- diff(years)
  annual_changes <- changes / time_gaps
  
  # Use median annual change (more robust to outliers)
  median_annual_change <- median(annual_changes)
  
  # Project from last known value
  last_value <- values[length(values)]
  years_to_project <- target_year - max(years)
  
  projected <- last_value + median_annual_change * years_to_project
  
  as.integer(max(0, projected))
}

#' Growth rate ensemble projection with confidence intervals
#' Similar to hurricane prediction - uses multiple growth rate scenarios
project_growth_ensemble <- function(values, years, target_year, confidence_levels = c(0.1, 0.5, 0.9)) {
  if (length(values) != length(years) || length(values) < 3) {
    return(list(
      best_estimate = project_linear_regression(values, years, target_year),
      confidence_intervals = NULL,
      growth_rates = NULL
    ))
  }
  
  # Calculate all possible growth rate dyads
  growth_rates <- c()
  time_spans <- c()
  
  for (i in 1:(length(values)-1)) {
    for (j in (i+1):length(values)) {
      start_val <- values[i]
      end_val <- values[j]
      start_year <- years[i]
      end_year <- years[j]
      
      if (start_val > 0 && end_val > 0) {
        # Calculate annualized growth rate
        time_span <- end_year - start_year
        annual_rate <- (end_val / start_val)^(1/time_span) - 1
        
        growth_rates <- c(growth_rates, annual_rate)
        time_spans <- c(time_spans, time_span)
      }
    }
  }
  
  if (length(growth_rates) == 0) {
    return(list(
      best_estimate = project_linear_regression(values, years, target_year),
      confidence_intervals = NULL,
      growth_rates = NULL
    ))
  }
  
  # Weight recent periods more heavily
  recency_weights <- exp(seq(0, 2, length.out = length(growth_rates)))
  
  # Calculate weighted statistics
  weighted_mean <- weighted.mean(growth_rates, recency_weights)
  weighted_sd <- sqrt(weighted.mean((growth_rates - weighted_mean)^2, recency_weights))
  
  # Use last known value as starting point
  last_value <- values[length(values)]
  years_to_project <- target_year - max(years)
  
  # Generate ensemble projections
  projections <- c()
  
  # Best estimate (weighted mean)
  best_estimate <- last_value * (1 + weighted_mean)^years_to_project
  
  # Confidence intervals using normal distribution of growth rates
  confidence_intervals <- list()
  for (conf_level in confidence_levels) {
    # Calculate quantiles of the growth rate distribution
    z_score <- qnorm(conf_level)
    rate_quantile <- weighted_mean + z_score * weighted_sd
    
    # Apply demographic realism constraints
    rate_quantile <- pmax(pmin(rate_quantile, 0.03), -0.02)  # Cap at ±2-3% annually
    
    projection <- last_value * (1 + rate_quantile)^years_to_project
    confidence_intervals[[paste0("p", round(conf_level * 100))]] <- as.integer(max(0, projection))
  }
  
  return(list(
    best_estimate = as.integer(max(0, best_estimate)),
    confidence_intervals = confidence_intervals,
    growth_rates = growth_rates,
    weighted_mean_rate = weighted_mean,
    weighted_sd_rate = weighted_sd,
    time_spans = time_spans
  ))
}

#' Monte Carlo simulation for population projection uncertainty
project_monte_carlo <- function(values, years, target_year, n_simulations = 1000) {
  if (length(values) != length(years) || length(values) < 3) {
    return(list(
      best_estimate = project_linear_regression(values, years, target_year),
      confidence_intervals = NULL
    ))
  }
  
  # Calculate historical growth rates
  growth_rates <- c()
  for (i in 1:(length(values)-1)) {
    if (values[i] > 0 && values[i+1] > 0) {
      time_span <- years[i+1] - years[i]
      annual_rate <- (values[i+1] / values[i])^(1/time_span) - 1
      growth_rates <- c(growth_rates, annual_rate)
    }
  }
  
  if (length(growth_rates) == 0) {
    return(list(
      best_estimate = project_linear_regression(values, years, target_year),
      confidence_intervals = NULL
    ))
  }
  
  # Estimate parameters of growth rate distribution
  mean_rate <- mean(growth_rates)
  sd_rate <- sd(growth_rates)
  
  # Monte Carlo simulation
  last_value <- values[length(values)]
  years_to_project <- target_year - max(years)
  
  simulated_projections <- c()
  
  for (sim in 1:n_simulations) {
    # Sample growth rate from normal distribution
    sampled_rate <- rnorm(1, mean_rate, sd_rate)
    
    # Apply demographic constraints
    sampled_rate <- pmax(pmin(sampled_rate, 0.03), -0.02)
    
    # Project forward
    projection <- last_value * (1 + sampled_rate)^years_to_project
    simulated_projections <- c(simulated_projections, max(0, projection))
  }
  
  # Calculate confidence intervals
  confidence_intervals <- list(
    p10 = as.integer(quantile(simulated_projections, 0.1)),
    p25 = as.integer(quantile(simulated_projections, 0.25)),
    p50 = as.integer(quantile(simulated_projections, 0.5)),
    p75 = as.integer(quantile(simulated_projections, 0.75)),
    p90 = as.integer(quantile(simulated_projections, 0.9))
  )
  
  return(list(
    best_estimate = as.integer(median(simulated_projections)),
    confidence_intervals = confidence_intervals,
    simulations = simulated_projections,
    mean_rate = mean_rate,
    sd_rate = sd_rate
  ))
}

#' Sequential Ensemble Projection (year-by-year forecasting)
project_growth_ensemble_sequential <- function(values, years, target_year, confidence_levels = c(0.1, 0.5, 0.9)) {
  if (length(values) != length(years) || length(values) < 3) {
    return(list(
      best_estimate = project_linear_regression(values, years, target_year),
      confidence_intervals = NULL,
      yearly_projections = NULL
    ))
  }
  
  # Calculate all possible growth rate dyads (same as original)
  growth_rates <- c()
  time_spans <- c()
  
  for (i in 1:(length(values)-1)) {
    for (j in (i+1):length(values)) {
      start_val <- values[i]
      end_val <- values[j]
      start_year <- years[i]
      end_year <- years[j]
      
      if (start_val > 0 && end_val > 0) {
        time_span <- end_year - start_year
        annual_rate <- (end_val / start_val)^(1/time_span) - 1
        growth_rates <- c(growth_rates, annual_rate)
        time_spans <- c(time_spans, time_span)
      }
    }
  }
  
  if (length(growth_rates) == 0) {
    return(list(
      best_estimate = project_linear_regression(values, years, target_year),
      confidence_intervals = NULL,
      yearly_projections = NULL
    ))
  }
  
  # Weight recent periods more heavily
  recency_weights <- exp(seq(0, 2, length.out = length(growth_rates)))
  weighted_mean <- weighted.mean(growth_rates, recency_weights)
  weighted_sd <- sqrt(weighted.mean((growth_rates - weighted_mean)^2, recency_weights))
  
  # Sequential projection year by year
  last_value <- values[length(values)]
  last_year <- max(years)
  current_value <- last_value
  yearly_projections <- list()
  
  # Project each year sequentially
  for (year in (last_year + 1):target_year) {
    # Add slight drift/uncertainty each year (growth rate can change)
    year_offset <- year - last_year
    drift_factor <- 1 + (year_offset - 1) * 0.1  # Increasing uncertainty over time
    
    # Calculate projections for this year
    year_projections <- list()
    
    # Best estimate
    projected_value <- current_value * (1 + weighted_mean)
    year_projections$best <- as.integer(max(0, projected_value))
    
    # Confidence intervals with increasing uncertainty
    adjusted_sd <- weighted_sd * drift_factor
    for (conf_level in confidence_levels) {
      z_score <- qnorm(conf_level)
      rate_quantile <- weighted_mean + z_score * adjusted_sd
      rate_quantile <- pmax(pmin(rate_quantile, 0.03), -0.02)  # Demographic constraints
      
      projection <- current_value * (1 + rate_quantile)
      year_projections[[paste0("p", round(conf_level * 100))]] <- as.integer(max(0, projection))
    }
    
    yearly_projections[[as.character(year)]] <- year_projections
    current_value <- year_projections$best  # Use best estimate for next year
  }
  
  # Extract final year results
  final_projections <- yearly_projections[[as.character(target_year)]]
  confidence_intervals <- final_projections[paste0("p", round(confidence_levels * 100))]
  
  return(list(
    best_estimate = final_projections$best,
    confidence_intervals = confidence_intervals,
    yearly_projections = yearly_projections,
    weighted_mean_rate = weighted_mean,
    weighted_sd_rate = weighted_sd
  ))
}

#' Sequential Monte Carlo simulation (year-by-year with evolving uncertainty)
project_monte_carlo_sequential <- function(values, years, target_year, n_simulations = 1000) {
  if (length(values) != length(years) || length(values) < 3) {
    return(list(
      best_estimate = project_linear_regression(values, years, target_year),
      confidence_intervals = NULL,
      yearly_projections = NULL
    ))
  }
  
  # Calculate historical growth rates
  growth_rates <- c()
  for (i in 1:(length(values)-1)) {
    if (values[i] > 0 && values[i+1] > 0) {
      time_span <- years[i+1] - years[i]
      annual_rate <- (values[i+1] / values[i])^(1/time_span) - 1
      growth_rates <- c(growth_rates, annual_rate)
    }
  }
  
  if (length(growth_rates) == 0) {
    return(list(
      best_estimate = project_linear_regression(values, years, target_year),
      confidence_intervals = NULL,
      yearly_projections = NULL
    ))
  }
  
  mean_rate <- mean(growth_rates)
  sd_rate <- sd(growth_rates)
  
  # Sequential Monte Carlo simulation
  last_value <- values[length(values)]
  last_year <- max(years)
  yearly_projections <- list()
  
  # Run simulations for each year sequentially
  for (year in (last_year + 1):target_year) {
    year_simulations <- c()
    
    for (sim in 1:n_simulations) {
      # Start from last known value or previous year's simulation result
      if (year == last_year + 1) {
        current_value <- last_value
      } else {
        # Use previous year's simulation result for this simulation path
        prev_year_sims <- yearly_projections[[as.character(year - 1)]]$simulations
        current_value <- prev_year_sims[sim]
      }
      
      # Add uncertainty that increases with time
      year_offset <- year - last_year
      uncertainty_multiplier <- 1 + (year_offset - 1) * 0.2  # Increasing uncertainty
      adjusted_sd <- sd_rate * uncertainty_multiplier
      
      # Sample growth rate with evolving uncertainty
      sampled_rate <- rnorm(1, mean_rate, adjusted_sd)
      sampled_rate <- pmax(pmin(sampled_rate, 0.03), -0.02)  # Demographic constraints
      
      # Project this year
      projected_value <- current_value * (1 + sampled_rate)
      year_simulations <- c(year_simulations, max(0, projected_value))
    }
    
    # Calculate statistics for this year
    confidence_intervals <- list(
      p10 = as.integer(quantile(year_simulations, 0.1)),
      p25 = as.integer(quantile(year_simulations, 0.25)),
      p50 = as.integer(quantile(year_simulations, 0.5)),
      p75 = as.integer(quantile(year_simulations, 0.75)),
      p90 = as.integer(quantile(year_simulations, 0.9))
    )
    
    yearly_projections[[as.character(year)]] <- list(
      best_estimate = as.integer(median(year_simulations)),
      confidence_intervals = confidence_intervals,
      simulations = year_simulations
    )
  }
  
  # Extract final year results
  final_results <- yearly_projections[[as.character(target_year)]]
  
  return(list(
    best_estimate = final_results$best_estimate,
    confidence_intervals = final_results$confidence_intervals,
    yearly_projections = yearly_projections,
    mean_rate = mean_rate,
    sd_rate = sd_rate
  ))
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
    results$total_polynomial <- project_polynomial(total_values, years_observed, target_year, degree = 2)
    results$citizen_polynomial <- project_polynomial(citizen_values, years_observed, target_year, degree = 2)
  }, error = function(e) {
    # Fall back to linear regression if polynomial fails
    results$total_polynomial <<- project_linear_regression(total_values, years_observed, target_year)
    results$citizen_polynomial <<- project_linear_regression(citizen_values, years_observed, target_year)
  })
  
  tryCatch({
    results$total_weighted <- project_weighted_trend(total_values, years_observed, target_year)
    results$citizen_weighted <- project_weighted_trend(citizen_values, years_observed, target_year)
  }, error = function(e) {
    results$total_weighted <<- NA
    results$citizen_weighted <<- NA
  })
  
  tryCatch({
    results$total_moving_avg <- project_moving_average(total_values, years_observed, target_year, window = 2)
    results$citizen_moving_avg <- project_moving_average(citizen_values, years_observed, target_year, window = 2)
  }, error = function(e) {
    results$total_moving_avg <<- NA
    results$citizen_moving_avg <<- NA
  })
  
  # Add robust linear method
  tryCatch({
    results$total_robust_linear <- project_robust_linear(total_values, years_observed, target_year)
    results$citizen_robust_linear <- project_robust_linear(citizen_values, years_observed, target_year)
  }, error = function(e) {
    results$total_robust_linear <<- NA
    results$citizen_robust_linear <<- NA
  })
  
  # Add recent trend method (last 3 years)
  tryCatch({
    results$total_recent_trend <- project_recent_trend(total_values, years_observed, target_year)
    results$citizen_recent_trend <- project_recent_trend(citizen_values, years_observed, target_year)
  }, error = function(e) {
    results$total_recent_trend <<- NA
    results$citizen_recent_trend <<- NA
  })
  
  # Add sequential growth ensemble method (year-by-year forecasting)
  tryCatch({
    total_ensemble <- project_growth_ensemble_sequential(total_values, years_observed, target_year)
    citizen_ensemble <- project_growth_ensemble_sequential(citizen_values, years_observed, target_year)
    
    results$total_ensemble_best <- total_ensemble$best_estimate
    results$citizen_ensemble_best <- citizen_ensemble$best_estimate
    
    # Store confidence intervals
    if (!is.null(total_ensemble$confidence_intervals)) {
      results$total_ensemble_p10 <- total_ensemble$confidence_intervals$p10
      results$total_ensemble_p50 <- total_ensemble$confidence_intervals$p50
      results$total_ensemble_p90 <- total_ensemble$confidence_intervals$p90
    }
    if (!is.null(citizen_ensemble$confidence_intervals)) {
      results$citizen_ensemble_p10 <- citizen_ensemble$confidence_intervals$p10
      results$citizen_ensemble_p50 <- citizen_ensemble$confidence_intervals$p50
      results$citizen_ensemble_p90 <- citizen_ensemble$confidence_intervals$p90
    }
    
    # Store growth rate statistics
    results$total_mean_growth_rate <- total_ensemble$weighted_mean_rate
    results$citizen_mean_growth_rate <- citizen_ensemble$weighted_mean_rate
    
  }, error = function(e) {
    results$total_ensemble_best <<- NA
    results$citizen_ensemble_best <<- NA
  })
  
  # Add sequential Monte Carlo method (year-by-year simulation)
  tryCatch({
    total_mc <- project_monte_carlo_sequential(total_values, years_observed, target_year, n_simulations = 500)
    citizen_mc <- project_monte_carlo_sequential(citizen_values, years_observed, target_year, n_simulations = 500)
    
    results$total_monte_carlo <- total_mc$best_estimate
    results$citizen_monte_carlo <- citizen_mc$best_estimate
    
    # Store Monte Carlo confidence intervals
    if (!is.null(total_mc$confidence_intervals)) {
      results$total_mc_p10 <- total_mc$confidence_intervals$p10
      results$total_mc_p25 <- total_mc$confidence_intervals$p25
      results$total_mc_p50 <- total_mc$confidence_intervals$p50
      results$total_mc_p75 <- total_mc$confidence_intervals$p75
      results$total_mc_p90 <- total_mc$confidence_intervals$p90
    }
    if (!is.null(citizen_mc$confidence_intervals)) {
      results$citizen_mc_p10 <- citizen_mc$confidence_intervals$p10
      results$citizen_mc_p25 <- citizen_mc$confidence_intervals$p25
      results$citizen_mc_p50 <- citizen_mc$confidence_intervals$p50
      results$citizen_mc_p75 <- citizen_mc$confidence_intervals$p75
      results$citizen_mc_p90 <- citizen_mc$confidence_intervals$p90
    }
    
  }, error = function(e) {
    results$total_monte_carlo <<- NA
    results$citizen_monte_carlo <<- NA
  })
  
  return(results)
}

# ==============================================================================
# PROJECTION ANALYSIS
# ==============================================================================

cat("=== APPLYING PROJECTION METHODS ===\n")

# Configuration
years_observed <- analysis_years
target_year <- 2030
projection_methods <- c("linear_2pt", "exp_2pt", "aapc_2pt", "linear_reg", 
                        "polynomial", "weighted", "moving_avg", "recent_trend", 
                        "robust_linear", "ensemble_best", "monte_carlo")

# Initialize projection columns
for (method in projection_methods) {
  state_acs[[paste0("total2030_", method)]] <- NA
  state_acs[[paste0("citizen2030_", method)]] <- NA
}

# Initialize ensemble confidence interval columns
ensemble_columns <- c("ensemble_p10", "ensemble_p50", "ensemble_p90",
                     "mc_p10", "mc_p25", "mc_p50", "mc_p75", "mc_p90")
for (col in ensemble_columns) {
  state_acs[[paste0("total2030_", col)]] <- NA
  state_acs[[paste0("citizen2030_", col)]] <- NA
}

# Initialize growth rate columns
state_acs$total_mean_growth_rate <- NA
state_acs$citizen_mean_growth_rate <- NA

# Apply projections to each state
cat("Calculating projections for", nrow(state_acs), "states...\n")

for (i in 1:nrow(state_acs)) {
  if (i %% 10 == 0) cat("Processing state", i, "of", nrow(state_acs), "\n")
  
  # Extract data for current state (only for years with valid data)
  total_values <- c()
  citizen_values <- c()
  years_with_data <- c()
  
  for (year in analysis_years) {
    year_suffix <- substr(year, 3, 4)
    total_col <- paste0("TOTAL", year_suffix)
    citizen_col <- paste0("CITIZEN", year_suffix)
    
    # Only include if both columns exist and have valid data
    if (total_col %in% names(state_acs) && citizen_col %in% names(state_acs)) {
      total_val <- state_acs[[total_col]][i]
      citizen_val <- state_acs[[citizen_col]][i]
      
      if (!is.na(total_val) && !is.na(citizen_val)) {
        total_values <- c(total_values, total_val)
        citizen_values <- c(citizen_values, citizen_val)
        years_with_data <- c(years_with_data, year)
      }
    }
  }
  
  # Skip if we don't have enough data points
  if (length(total_values) < 2) {
    cat("Warning: Insufficient data for", state_acs$NAME[i], "- skipping\n")
    next
  }
  
  # Apply all projection methods
  projections <- apply_projections_to_state(total_values, citizen_values, 
                                            years_with_data, target_year, 
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
  state_acs$total2030_recent_trend[i] <- projections$total_recent_trend
  state_acs$citizen2030_recent_trend[i] <- projections$citizen_recent_trend
  state_acs$total2030_robust_linear[i] <- projections$total_robust_linear
  state_acs$citizen2030_robust_linear[i] <- projections$citizen_robust_linear
  
  # Store ensemble results
  state_acs$total2030_ensemble_best[i] <- projections$total_ensemble_best
  state_acs$citizen2030_ensemble_best[i] <- projections$citizen_ensemble_best
  state_acs$total2030_monte_carlo[i] <- projections$total_monte_carlo
  state_acs$citizen2030_monte_carlo[i] <- projections$citizen_monte_carlo
  
  # Store confidence intervals
  state_acs$total2030_ensemble_p10[i] <- projections$total_ensemble_p10
  state_acs$total2030_ensemble_p50[i] <- projections$total_ensemble_p50
  state_acs$total2030_ensemble_p90[i] <- projections$total_ensemble_p90
  state_acs$citizen2030_ensemble_p10[i] <- projections$citizen_ensemble_p10
  state_acs$citizen2030_ensemble_p50[i] <- projections$citizen_ensemble_p50
  state_acs$citizen2030_ensemble_p90[i] <- projections$citizen_ensemble_p90
  
  state_acs$total2030_mc_p10[i] <- projections$total_mc_p10
  state_acs$total2030_mc_p25[i] <- projections$total_mc_p25
  state_acs$total2030_mc_p50[i] <- projections$total_mc_p50
  state_acs$total2030_mc_p75[i] <- projections$total_mc_p75
  state_acs$total2030_mc_p90[i] <- projections$total_mc_p90
  state_acs$citizen2030_mc_p10[i] <- projections$citizen_mc_p10
  state_acs$citizen2030_mc_p25[i] <- projections$citizen_mc_p25
  state_acs$citizen2030_mc_p50[i] <- projections$citizen_mc_p50
  state_acs$citizen2030_mc_p75[i] <- projections$citizen_mc_p75
  state_acs$citizen2030_mc_p90[i] <- projections$citizen_mc_p90
  
  # Store growth rate statistics
  state_acs$total_mean_growth_rate[i] <- projections$total_mean_growth_rate
  state_acs$citizen_mean_growth_rate[i] <- projections$citizen_mean_growth_rate
}

# Calculate non-citizen projections for each method
cat("Calculating non-citizen projections...\n")
for (method in projection_methods) {
  total_col <- paste0("total2030_", method)
  citizen_col <- paste0("citizen2030_", method)
  noncitizen_col <- paste0("noncitizen2030_", method)
  
  state_acs[[noncitizen_col]] <- state_acs[[total_col]] - state_acs[[citizen_col]]
}

# Set default projections (using ensemble method as most robust)
state_acs$total2030_proj <- state_acs$total2030_ensemble_best
state_acs$citizen2030_proj <- state_acs$citizen2030_ensemble_best
state_acs$noncitizen2030_proj <- state_acs$noncitizen2030_ensemble_best

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
  # Extract data dynamically for all analysis years
  total_vals <- c()
  citizen_vals <- c()
  
  for (year in analysis_years) {
    year_suffix <- substr(year, 3, 4)
    total_col <- paste0("TOTAL", year_suffix)
    citizen_col <- paste0("CITIZEN", year_suffix)
    
    total_vals <- c(total_vals, state_acs[[total_col]][i])
    citizen_vals <- c(citizen_vals, state_acs[[citizen_col]][i])
  }
  
  # Test accuracy by predicting last year using all previous years data
  last_year_index <- length(analysis_years)
  accuracy_results$total_linear_reg_error[i] <- evaluate_model_accuracy(total_vals, years_observed, last_year_index, "linear_reg")
  accuracy_results$total_polynomial_error[i] <- evaluate_model_accuracy(total_vals, years_observed, last_year_index, "polynomial")
  accuracy_results$total_weighted_error[i] <- evaluate_model_accuracy(total_vals, years_observed, last_year_index, "weighted")
  
  accuracy_results$citizen_linear_reg_error[i] <- evaluate_model_accuracy(citizen_vals, years_observed, last_year_index, "linear_reg")
  accuracy_results$citizen_polynomial_error[i] <- evaluate_model_accuracy(citizen_vals, years_observed, last_year_index, "polynomial")
  accuracy_results$citizen_weighted_error[i] <- evaluate_model_accuracy(citizen_vals, years_observed, last_year_index, "weighted")
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

  # Use the configured analysis years
  years_known <- analysis_years
  years_full <- min(years_known):2030
  
  cat("Creating plot for", state_name, "with years:", paste(range(years_known), collapse="-"), "\n")
  
  # Extract known data dynamically
  total_known <- c()
  citizen_known <- c()
  
  for (year in analysis_years) {
    year_suffix <- substr(year, 3, 4)
    total_col <- paste0("TOTAL", year_suffix)
    citizen_col <- paste0("CITIZEN", year_suffix)
    
    total_known <- c(total_known, state_data[[total_col]])
    citizen_known <- c(citizen_known, state_data[[citizen_col]])
  }
  
  # Create interpolated projections for smooth curves
  total_interp_linear <- approx(c(years_known, 2030), 
                               c(total_known, state_data$total2030_linear_reg), 
                               xout = years_full)$y
  total_interp_weighted <- approx(c(years_known, 2030), 
                                 c(total_known, state_data$total2030_weighted), 
                                 xout = years_full)$y
  total_interp_recent <- approx(c(years_known, 2030), 
                               c(total_known, state_data$total2030_recent_trend), 
                               xout = years_full)$y
  
  citizen_interp_linear <- approx(c(years_known, 2030), 
                                 c(citizen_known, state_data$citizen2030_linear_reg), 
                                 xout = years_full)$y
  citizen_interp_weighted <- approx(c(years_known, 2030), 
                                   c(citizen_known, state_data$citizen2030_weighted), 
                                   xout = years_full)$y
  citizen_interp_recent <- approx(c(years_known, 2030), 
                                 c(citizen_known, state_data$citizen2030_recent_trend), 
                                 xout = years_full)$y
  
  # Create plot
  plot(years_full, total_interp_linear, type = "n", 
       ylim = range(c(total_interp_linear, total_interp_weighted, total_interp_recent,
                      citizen_interp_linear, citizen_interp_weighted, citizen_interp_recent), na.rm = TRUE),
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
  lines(proj_years, total_interp_recent[proj_indices], col = "navy", lwd = 3, lty = 1)
  
  # Citizen population projections  
  lines(proj_years, citizen_interp_linear[proj_indices], col = "red", lwd = 2, lty = 2)
  lines(proj_years, citizen_interp_weighted[proj_indices], col = "darkred", lwd = 2, lty = 3)
  lines(proj_years, citizen_interp_recent[proj_indices], col = "maroon", lwd = 3, lty = 1)
  
  # Add vertical line at 2023 to separate known from projected
  abline(v = 2023, col = "gray", lty = 2)
  
  # Add note about missing 2020 data
  text(2020, max(total_known) * 0.95, "2020\n(missing)", col = "gray", cex = 0.8)
  
  legend("topleft",
         legend = c("Total (Known)", "Total (Linear Reg)", "Total (Weighted)", "Total (Recent Trend)",
                    "Citizen (Known)", "Citizen (Linear Reg)", "Citizen (Weighted)", "Citizen (Recent Trend)"),
         col = c("blue", "blue", "darkblue", "navy", "red", "red", "darkred", "maroon"),
         lwd = c(2, 2, 2, 3, 2, 2, 2, 3), lty = c(1, 2, 3, 1, 1, 2, 3, 1), cex = 0.7)
}

# Create the visualization
create_state_projection_plot("Florida")

# ------------------------------------------------------------------------------
# Hurricane-Style Ensemble Projection Visualization
# ------------------------------------------------------------------------------

create_ensemble_projection_plot <- function(state_name = "Pennsylvania", method = c("ensemble", "monte_carlo", "both")) {
  method <- match.arg(method)
  
  state_data <- state_acs[state_acs$NAME == state_name, ]
  if (nrow(state_data) == 0) {
    cat("State", state_name, "not found.\n")
    return()
  }
  
  # Use the configured analysis years
  years_known <- analysis_years
  
  # Extract known data dynamically from available columns
  total_known <- c()
  citizen_known <- c()
  
  for (year in years_known) {
    year_suffix <- substr(year, 3, 4)
    total_col <- paste0("TOTAL", year_suffix)
    citizen_col <- paste0("CITIZEN", year_suffix)
    
    # Only include if column exists and has data
    if (total_col %in% names(state_data) && !is.na(state_data[[total_col]])) {
      total_known <- c(total_known, state_data[[total_col]])
      citizen_known <- c(citizen_known, state_data[[citizen_col]])
    }
  }
  
  # Set up plot with ensemble projections
  par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))
  
  # === TOTAL POPULATION PLOT ===
  
  # Define projection year and last data year
  proj_year <- 2030
  last_year <- max(years_known)
  last_year_suffix <- substr(last_year, 3, 4)
  
  # Calculate range for y-axis based on selected method
  if (method == "ensemble") {
    total_min <- min(c(total_known, state_data$total2030_ensemble_p10), na.rm = TRUE)
    total_max <- max(c(total_known, state_data$total2030_ensemble_p90), na.rm = TRUE)
    method_title <- "Ensemble Forecast"
  } else if (method == "monte_carlo") {
    total_min <- min(c(total_known, state_data$total2030_mc_p10), na.rm = TRUE)
    total_max <- max(c(total_known, state_data$total2030_mc_p90), na.rm = TRUE)
    method_title <- "Monte Carlo Forecast"
  } else {
    total_min <- min(c(total_known, state_data$total2030_ensemble_p10, state_data$total2030_mc_p10), na.rm = TRUE)
    total_max <- max(c(total_known, state_data$total2030_ensemble_p90, state_data$total2030_mc_p90), na.rm = TRUE)
    method_title <- "Ensemble + Monte Carlo Forecast"
  }
  
  plot(years_known, total_known, type = "n", 
      xlim = range(c(years_known, proj_year), na.rm = TRUE), 
      ylim = c(total_min * 0.98, total_max * 1.02),
       ylab = "Total Population", xlab = "Year", 
       main = paste(state_name, "Total Population:", method_title))
  
  # Add uncertainty bands based on selected method
  last_total <- state_data[[paste0("TOTAL", last_year_suffix)]]
  
  # Ensemble confidence bands
  if ((method == "ensemble" || method == "both") && 
      !is.na(state_data$total2030_ensemble_p10) && !is.na(state_data$total2030_ensemble_p90)) {
    
    polygon(c(last_year, proj_year, proj_year, last_year), 
             c(last_total, state_data$total2030_ensemble_p10, 
               state_data$total2030_ensemble_p90, last_total),
             col = rgb(0, 0, 1, 0.2), border = NA)
    
    lines(c(last_year, proj_year), c(last_total, state_data$total2030_ensemble_p10), 
          col = "blue", lty = 2, lwd = 1)
    lines(c(last_year, proj_year), c(last_total, state_data$total2030_ensemble_p90), 
          col = "blue", lty = 2, lwd = 1)
  }
  
  # Monte Carlo confidence bands
  if ((method == "monte_carlo" || method == "both") &&
      !is.na(state_data$total2030_mc_p25) && !is.na(state_data$total2030_mc_p75)) {
    polygon(c(last_year, proj_year, proj_year, last_year), 
             c(last_total, state_data$total2030_mc_p25, 
               state_data$total2030_mc_p75, last_total),
             col = rgb(0, 1, 0, 0.3), border = NA)
  }
  
  # Historical data
  points(years_known, total_known, col = "black", pch = 16, cex = 1.2)
  lines(years_known, total_known, col = "black", lwd = 2)
  
  # Best estimates - conditional based on method
  if ((method == "ensemble" || method == "both") && !is.na(state_data$total2030_ensemble_best)) {
    points(proj_year, state_data$total2030_ensemble_best, col = "blue", pch = 16, cex = 1.5)
    lines(c(last_year, proj_year), c(last_total, state_data$total2030_ensemble_best), 
          col = "blue", lwd = 3)
  }
  
  if ((method == "monte_carlo" || method == "both") && !is.na(state_data$total2030_monte_carlo)) {
    points(proj_year, state_data$total2030_monte_carlo, col = "green", pch = 17, cex = 1.5)
    lines(c(last_year, proj_year), c(last_total, state_data$total2030_monte_carlo), 
          col = "green", lwd = 2, lty = 3)
  }
  
  # Vertical line at last year
  abline(v = last_year, col = "gray", lty = 2)
  
  # Add text annotations
  if (!is.na(state_data$total_mean_growth_rate)) {
    text(2025, total_max * 0.95, 
         paste0("Avg Growth Rate: ", round(state_data$total_mean_growth_rate * 100, 2), "%/year"),
         cex = 0.8, col = "darkblue")
  }
  
  # Dynamic legend based on method
  if (method == "ensemble") {
    legend("topleft",
           legend = c("Historical", "Ensemble Best", "80% CI (Ensemble)"),
           col = c("black", "blue", rgb(0,0,1,0.2)),
           lwd = c(2, 3, 8), lty = c(1, 1, 1), cex = 0.7)
  } else if (method == "monte_carlo") {
    legend("topleft",
           legend = c("Historical", "Monte Carlo", "50% CI (MC)"),
           col = c("black", "green", rgb(0,1,0,0.3)),
           lwd = c(2, 2, 8), lty = c(1, 3, 1), cex = 0.7)
  } else {
    legend("topleft",
           legend = c("Historical", "Ensemble Best", "Monte Carlo", "80% CI (Ensemble)", "50% CI (MC)"),
           col = c("black", "blue", "green", rgb(0,0,1,0.2), rgb(0,1,0,0.3)),
           lwd = c(2, 3, 2, 8, 8), lty = c(1, 1, 3, 1, 1), cex = 0.7)
  }
  
  # === CITIZEN POPULATION PLOT ===
  
  # Calculate range for y-axis based on selected method
  if (method == "ensemble") {
    citizen_min <- min(c(citizen_known, state_data$citizen2030_ensemble_p10), na.rm = TRUE)
    citizen_max <- max(c(citizen_known, state_data$citizen2030_ensemble_p90), na.rm = TRUE)
  } else if (method == "monte_carlo") {
    citizen_min <- min(c(citizen_known, state_data$citizen2030_mc_p10), na.rm = TRUE)
    citizen_max <- max(c(citizen_known, state_data$citizen2030_mc_p90), na.rm = TRUE)
  } else {
    citizen_min <- min(c(citizen_known, state_data$citizen2030_ensemble_p10, state_data$citizen2030_mc_p10), na.rm = TRUE)
    citizen_max <- max(c(citizen_known, state_data$citizen2030_ensemble_p90, state_data$citizen2030_mc_p90), na.rm = TRUE)
  }
  
  plot(years_known, citizen_known, type = "n", 
       xlim = range(c(years_known, proj_year), na.rm = TRUE), ylim = c(citizen_min * 0.98, citizen_max * 1.02),
       ylab = "Citizen Population", xlab = "Year", 
       main = paste(state_name, "Citizen Population:", method_title))
  
  # Add uncertainty bands based on selected method
  last_citizen <- state_data[[paste0("CITIZEN", last_year_suffix)]]
  if ((method == "ensemble" || method == "both") &&
      !is.na(state_data$citizen2030_ensemble_p10) && !is.na(state_data$citizen2030_ensemble_p90)) {
    polygon(c(last_year, proj_year, proj_year, last_year), 
             c(last_citizen, state_data$citizen2030_ensemble_p10, 
               state_data$citizen2030_ensemble_p90, last_citizen),
             col = rgb(1, 0, 0, 0.2), border = NA)
    
    lines(c(last_year, proj_year), c(last_citizen, state_data$citizen2030_ensemble_p10), 
          col = "red", lty = 2, lwd = 1)
    lines(c(last_year, proj_year), c(last_citizen, state_data$citizen2030_ensemble_p90), 
          col = "red", lty = 2, lwd = 1)
  }
  
  # Monte Carlo confidence bands
  if ((method == "monte_carlo" || method == "both") &&
      !is.na(state_data$citizen2030_mc_p25) && !is.na(state_data$citizen2030_mc_p75)) {
    polygon(c(last_year, proj_year, proj_year, last_year), 
             c(last_citizen, state_data$citizen2030_mc_p25, 
               state_data$citizen2030_mc_p75, last_citizen),
             col = rgb(1, 0.5, 0, 0.3), border = NA)
  }
  
  # Historical data
  points(years_known, citizen_known, col = "black", pch = 16, cex = 1.2)
  lines(years_known, citizen_known, col = "black", lwd = 2)
  
  # Best estimates - conditional based on method
  if ((method == "ensemble" || method == "both") && !is.na(state_data$citizen2030_ensemble_best)) {
    points(proj_year, state_data$citizen2030_ensemble_best, col = "red", pch = 16, cex = 1.5)
    lines(c(last_year, proj_year), c(last_citizen, state_data$citizen2030_ensemble_best), 
          col = "red", lwd = 3)
  }
  
  if ((method == "monte_carlo" || method == "both") && !is.na(state_data$citizen2030_monte_carlo)) {
    points(proj_year, state_data$citizen2030_monte_carlo, col = "orange", pch = 17, cex = 1.5)
    lines(c(last_year, proj_year), c(last_citizen, state_data$citizen2030_monte_carlo), 
          col = "orange", lwd = 2, lty = 3)
  }
  
  # Vertical line at last year
  abline(v = last_year, col = "gray", lty = 2)
  
  # Add text annotations
  if (!is.na(state_data$citizen_mean_growth_rate)) {
    text(2025, citizen_max * 0.95, 
         paste0("Avg Growth Rate: ", round(state_data$citizen_mean_growth_rate * 100, 2), "%/year"),
         cex = 0.8, col = "darkred")
  }
  
  # Dynamic legend based on method
  if (method == "ensemble") {
    legend("topleft",
           legend = c("Historical", "Ensemble Best", "80% CI (Ensemble)"),
           col = c("black", "red", rgb(1,0,0,0.2)),
           lwd = c(2, 3, 8), lty = c(1, 1, 1), cex = 0.7)
  } else if (method == "monte_carlo") {
    legend("topleft",
           legend = c("Historical", "Monte Carlo", "50% CI (MC)"),
           col = c("black", "orange", rgb(1,0.5,0,0.3)),
           lwd = c(2, 2, 8), lty = c(1, 3, 1), cex = 0.7)
  } else {
    legend("topleft",
           legend = c("Historical", "Ensemble Best", "Monte Carlo", "80% CI (Ensemble)", "50% CI (MC)"),
           col = c("black", "red", "orange", rgb(1,0,0,0.2), rgb(1,0.5,0,0.3)),
           lwd = c(2, 3, 2, 8, 8), lty = c(1, 1, 3, 1, 1), cex = 0.7)
  }
  
  par(mfrow = c(1, 1))  # Reset plot layout
}

# ------------------------------------------------------------------------------
# Create projection visualizations with different methods
# ------------------------------------------------------------------------------

# Ensemble method only
cat("Creating Ensemble-only forecast for New York...\n")
create_ensemble_projection_plot("New York", method = "ensemble")

# Monte Carlo method only  
cat("Creating Monte Carlo-only forecast for Florida...\n")
create_ensemble_projection_plot("Florida", method = "monte_carlo")

# Both methods (default behavior)
cat("Creating combined forecast for Texas...\n")
create_ensemble_projection_plot("Texas", method = "both")



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

