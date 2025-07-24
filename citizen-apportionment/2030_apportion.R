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

fips <- read.csv("https://raw.githubusercontent.com/jcervas/Data/refs/heads/master/fips.csv")
a <- read.csv('/Users/cervas/Library/Mobile Documents/com~apple~CloudDocs/Downloads/The New Political Consequences to Remo/est-2030-pop-ACS21-ACS23-1-year.csv')

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

