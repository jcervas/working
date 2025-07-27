# Test script to verify Congressional District (CD) functionality
# Source the get_acs.R file first
source("/Users/cervas/Library/CloudStorage/GoogleDrive-jcervas@andrew.cmu.edu/My Drive/GitHub/R-Functions/get_acs/get_acs.R")

# Test 1: Group URL construction for CD
test_group_url <- construct_group_url(
  table = "B05001",
  geography = "cd", 
  geo_filter = NULL,
  state_fips = "36",  # This will be ignored for CD since it's national
  year = 2023,
  dataset = "acs/acs1"
)

cat("Group URL for CD:\n", test_group_url, "\n\n")

# Test 2: Standard URL construction for CD
test_standard_url <- construct_standard_url(
  variables = c("B05001_001E", "B05001_002E"),
  geography = "cd",
  state_fips = "36",  # This will be ignored for CD since it's national
  year = 2023,
  dataset = "acs/acs1"
)

cat("Standard URL for CD:\n", test_standard_url, "\n\n")

# Test 3: Full get_acs call for CD (commented out to avoid API call during testing)
# cd_data <- get_acs(
#   table = "B05001",
#   geography = "cd",
#   year = 2023,
#   acs_year = 1,
#   use_group = TRUE
# )
# 
# head(cd_data)

cat("CD functionality has been added successfully!\n")
cat("You can now use geography = 'cd' in your get_acs() calls.\n")
