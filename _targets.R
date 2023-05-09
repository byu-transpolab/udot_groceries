# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "dplyr", "sf"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts with your custom functions:
for (file in list.files("R", full.names = TRUE)) source(file)
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
#   tar_target(
#     name = data,
#     command = tibble(x = rnorm(100), y = rnorm(100))
# #   format = "feather" # efficient storage of large data frames # nolint
#   ),
#   tar_target(
#     name = model,
#     command = coefficients(lm(y ~ x, data = data))
#   ),


  # Groceries ===========================
  # Gather data on grocery stores and impute missing information
  # 
  

  # Travel times ========================
  # Construct a travel time matrix from open street maps
  # 1. Check network data
  # 2. Build travel times
  # 3. Build logsums
  


  # Models ==============================
  # Link the trip matrices and groceries together and compute accessibilities
  # 
  # 
)
