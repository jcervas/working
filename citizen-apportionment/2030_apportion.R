# Clear environment and suppress scientific notation
rm(list = ls(all = TRUE))
options(scipen = 999)

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

source('https://raw.githubusercontent.com/jcervas/R-Functions/refs/heads/main/get_acs/get_acs.R')
fips <- read.csv("https://raw.githubusercontent.com/jcervas/Data/refs/heads/master/fips.csv")

# Get 2023 ACS data for congressional districts
acs_2023_cd <- get_acs(
  table = "B05001", 
  year = 2023, 
  geography = "cd", 
  var_types = "E",
  acs_year = 1)

b05001_cd <- cbind.data.frame(
  NAME = acs_2023_cd$NAME,
  GEOID = acs_2023_cd$GEOID,
  STATE = sub(".*,\\s*", "", acs_2023_cd$NAME), # Extract state from NAME,
  # Extract district number or "At Large" from NAME
  District = ifelse(
    grepl("At Large", acs_2023_cd$NAME, ignore.case = TRUE),
    1,
    ifelse(
      grepl("District", acs_2023_cd$NAME, ignore.case = TRUE),
      as.numeric(sub(".*District\\s(\\d+).*", "\\1", acs_2023_cd$NAME)),
      NA
    )
  ),
  CITIZEN23 = acs_2023_cd$B05001_001E - acs_2023_cd$B05001_006E,
  NOT_CIT23 = acs_2023_cd$B05001_006E, # Not a citizen
  TOTAL23 = acs_2023_cd$B05001_001E # Total population
)

b05001_cd <- subset(b05001_cd, !(STATE %in% c("Puerto Rico", "District of Columbia")))

# Get 2021 ACS data for states
acs_2021_st <- get_acs(
  table = "B05001", 
  year = 2021, 
  geography = "state", 
  var_types = "E",
  acs_year = 1)

acs_2021_st <- subset(acs_2021_st, !(acs_2021_st$GEOID %in% c("11", "72")))

acs_2021_st <- cbind.data.frame(
  NAME = acs_2021_st$NAME,
  GEOID = acs_2021_st$GEOID,
  CITIZEN21 = acs_2021_st$B05001_001E - acs_2021_st$B05001_006E,
  NOT_CIT21 = acs_2021_st$B05001_006E, # Not a citizen      
  TOTAL21 = acs_2021_st$B05001_001E # Total population
) 

# Get 2023 ACS data for states
acs_2023_st <- get_acs(
  table = "B05001", 
  year = 2023, 
  geography = "state", 
  var_types = "E",
  acs_year = 1)

acs_2023_st <- subset(acs_2023_st, !(acs_2023_st$GEOID %in% c("11", "72")))

acs_2023_st <- cbind.data.frame(
  NAME = acs_2023_st$NAME,
  GEOID = acs_2023_st$GEOID,
  CITIZEN23 = acs_2023_st$B05001_001E - acs_2023_st$B05001_006E,
  NOT_CIT23 = acs_2023_st$B05001_006E, # Not a citizen        
  TOTAL23 = acs_2023_st$B05001_001E # Total population
)

# Combine state and congressional district data
a <- merge(acs_2021_st, acs_2023_st, by = c("NAME","GEOID"), suffixes = c("21", "23"))

# Project 2030 total population using compound annual growth rate from 2021 to 2023
years_ahead <- 2030 - 2023
growth_rate <- (a$TOTAL23 / a$TOTAL21)^(1 / (2023 - 2021)) - 1
cit_growth_rate <- (a$CITIZEN23 / a$CITIZEN21)^(1 / (2023 - 2021)) - 1
a$total30_est <- as.integer(a$TOTAL23 * (1 + growth_rate)^years_ahead)
a$citizen30_est <- as.integer(a$CITIZEN23 * (1 + cit_growth_rate)^years_ahead)
a$noncitizen30_est <- a$total30_est - a$citizen30_est
a <- a[, c("NAME", "GEOID", "TOTAL21", "CITIZEN21", "NOT_CIT21", "TOTAL23", "CITIZEN23", "NOT_CIT23", "total30_est", "citizen30_est", "noncitizen30_est")]
# Rename columns for clarity
names(a) <- c("name", "geoid", "total21", "citizen21", "not_citizen21", "total23", "citizen23", "not_citizen23", "total30_est", "citizen30_est", "noncitizen30_est")

# Use the 2020 Census data for apportionment
all_states <- decennialAPI(state = "all", geo = "state", table = "P1", year = "2020", variables = c("P1_001N")

     "P1_011N", "P1_012N", "P1_013N", "P1_014N", "P1_015N", "P1_016N", "P1_017N", "P1_018N", "P1_019N",
                "P1_020N")
)

# Apportionment for 2030 based on estimated total population and citizen population
apportion_values(a[,"total20"], target_sum = 435, initial_seats = 1)

###

apportion_2030 <- data.frame(
     `name` = a[,"name"],
     `total` = a[,"total20"],
     `est.2030.total` = a[,"est.2030.total"],
     `est.2023.citizen` = a[,"est.2023.citizen"],
     `Census2020` = apportion_values(a[,"total20"], target_sum = 435, initial_seats = 1), 
     `Estimate2030TotalPopulation` = apportion_values(a[,"est.2030.total"], target_sum = 435, initial_seats = 1),
     `Estimate2030CitizenOnly` = apportion_values(a[,"est.2023.citizen"], target_sum = 435, initial_seats = 1),
     check.names = FALSE)

apportion_2030 <- merge(fips, apportion_2030, by.x="state", by.y="name")

apportion_2030$noncitDiff20 <- apportion_2030$Estimate2030CitizenOnly - apportion_2030$Census2020
apportion_2030$noncitDiff <- apportion_2030$Estimate2030CitizenOnly - apportion_2030$Estimate2030TotalPopulation
head(apportion_2030)

write.csv(apportion_2030, '/Users/cervas/Library/Mobile Documents/com~apple~CloudDocs/Downloads/est2030apportion.csv', row.names=F)

# CD Malapportionment

cd_citizen <- read.csv('/Users/cervas/Library/Mobile Documents/com~apple~CloudDocs/Downloads/The New Political Consequences to Remo/CD-Citizenship-ACSDT1Y2023.B05001-Data.csv')

# Step 1: Make sure 'State' column is present in both datasets
cd_citizen$State <- sub(".*,\\s*", "", cd_citizen$Geographic.Area.Name)
head(cd_citizen)

# Step 2: Calculate averages by state
avg_by_state <- aggregate(cbind(total, citizen, noncitizen) ~ State, data = cd_citizen, FUN = mean)

# Optional: Rename columns to avoid confusion
names(avg_by_state)[2:4] <- c("avg_total", "avg_citizen", "avg_noncitizen")

# Step 3: Merge averages back to original dataset
cd_citizen_with_averages <- merge(cd_citizen, avg_by_state, by = "State", all.x = TRUE)

cd_citizen_with_averages$dev_total <- (cd_citizen_with_averages$total - cd_citizen_with_averages$avg_total) / cd_citizen_with_averages$total
cd_citizen_with_averages$dev_citizen <- (cd_citizen_with_averages$citizen - cd_citizen_with_averages$avg_citizen) / cd_citizen_with_averages$avg_citizen


head(cd_citizen_with_averages[order(cd_citizen_with_averages$dev_citizen),], 10)
tail(cd_citizen_with_averages[order(cd_citizen_with_averages$dev_citizen),], 10)

