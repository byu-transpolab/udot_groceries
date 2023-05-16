# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint
options(tigris_use_cache = TRUE)
options(java.parameters = '-Xmx10G')

# Set target options:
tar_option_set(
  packages = c("tibble", "dplyr", "sf", "jsonlite", "nngeo", "r5r",
               "processx", "readr", "stringr", "tidycensus"), # packages that your targets need to run
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

this_crs = 3675


# Replace the target list below with your own:
list(

  # 1. Locations ===========================
  # Gather data on grocery stores and block groups and impute missing information
  # 1.1 Block group centroids
  tar_target(bgcentroids, get_bgcentroids()),
  # 1.2 Block group ACS data
  tar_target(bg_acs, get_acsdata(bgcentroids)),
  # 1.3 Grocery stores (with NEMS data)
  tar_target(nems_saltlake, "data/nems_saltlake.csv", format = "file"),
  tar_target(nems_sanjuan,  "data/nems_sanjuan.csv",  format = "file"),
  tar_target(nems_utah,     "data/nems_utah.csv",     format = "file"),
  tar_target(nems_groceries, get_nems_groceries(nems_saltlake, nems_sanjuan, nems_utah, this_crs)),
  
  # 1.4 Other grocery stores
  tar_target(grocery_sourcedata, "data/utah_allgroceries.geojson", format = "file"),
  tar_target(all_groceries, get_all_groceries(grocery_sourcedata, nems_groceries, bg_acs, this_crs)),
 
  # 1.5 Imputation
  tar_target(imputed_groceries, impute_store_data(all_groceries, bgcentroids, bg_acs)),

  # 2. Travel times ========================
  # Construct a travel time matrix from open street maps
  # 2.1. Gather network data
  tar_target(osmium_script, "sh/get_osm.sh", format = "file"),
  tar_target(merged_osm_file, run_shell_script(osmium_script, "r5/merged.osm.pbf"), format = "file"),
  tar_target(gtfs, get_gtfs("r5/gtfs.zip"), format = "file"),
  
  # 2.2 Build travel times
  tar_target(times, calculate_times(all_groceries, bgcentroids, 
                                    merged_osm_file, gtfs, 
                                    landuselimit = NULL, bglimit = NULL,
                                    max_trip_duration = 120)),
  
  # 2.2 Build logsums
  tar_target(util_file, "data/mode_utilities.json", format = "file"),
  tar_target(utilities, read_utilities(util_file)),
  tar_target(modechoice_logsums, calculate_logsums(times, utilities)),


  # 3. Accessibilities ==============================
  # Link the trip matrices and groceries together and compute accessibilities
  # 
  # 
  
  # Dummy targets so we don't end a list with a comma-------
  tar_target(
    name = data,
    command = tibble(x = rnorm(100), y = rnorm(100))
  ),
  tar_target(
    name = model,
    command = coefficients(lm(y ~ x, data = data))
  )

)
