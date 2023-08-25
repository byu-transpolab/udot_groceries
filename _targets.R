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
               "mlogit", "nemsr",
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
ut_counties = tigris::counties("Utah") |> dplyr::select(NAME, GEOID)

# Replace the target list below with your own:
list(

  # 1. Locations ===========================
  # Gather data on grocery stores and block groups and impute missing information
  # 1.1 Block group centroids
  tar_target(bgcentroids, get_bgcentroids()),
  tar_target(bg, tigris::block_groups("UT", year = 2019)),
  # 1.2 Block group ACS data
  tar_target(bg_acs, get_acsdata(bgcentroids)),
  # 1.3 Grocery stores (with NEMS data)
  tar_target(n1, "data/nems.sav",  format = "file"),
  tar_target(n2, "data/nems2.sav", format = "file"),
  tar_target(brands, "data/brands.csv", format = "file"),
  tar_target(nems_groceries, get_nems_groceries(list(n1, n2), brands, this_crs)),
  tar_target(neighbor_acs, get_neighbor_acs(bg_acs, bgcentroids, nems_groceries)),
  
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
                                    max_trip_duration = 180)),
  
  # 2.2 Build logsums
  tar_target(util_file, "data/mode_utilities.json", format = "file"),
  tar_target(utilities, read_utilities(util_file)),
  tar_target(mcls, calculate_logsums(times, utilities)),
  tar_target(nocarmcls, calculate_logsums(times, utilities, nocar = TRUE)),


  # 3. Accessibilities ==============================
  # Link the trip matrices and groceries together and compute accessibilities
  # 
  # 3.0 Flows
  tar_target(streetlight_sl, "data/streetlight/streetlight_groceries_saltlake.csv", format = "file"),
  tar_target(streetlight_ut, "data/streetlight/streetlight_groceries_utah.csv",     format = "file"),
  tar_target(streetlight_sj, "data/streetlight/streetlight_groceries_sanjuan.csv", format = "file"),
  tar_target(flows_sl, read_sl_data(streetlight_sl, mcls)),
  tar_target(flows_ut, read_sl_data(streetlight_ut, mcls, "UT-")),
  tar_target(flows_sj, read_sl_data(streetlight_sj, mcls)),
   
  # 3.1 estimate models
  tar_target(estdata_sl, make_estdata(flows_sl, mcls, nems_groceries |> filter(county == "Salt Lake"), bg_acs, n_obs = 10000, n_alts = 11)),
  tar_target(estdata_ut, make_estdata(flows_ut, mcls, nems_groceries |> filter(county == "Utah"), bg_acs, n_obs = 10000, n_alts = 11)),
  tar_target(estdata_sj, make_estdata(flows_sj, mcls, nems_groceries |> filter(county == "San Juan"), bg_acs, n_obs = 10000, n_alts = 11)),
  
  tar_target(sl_models, estimate_model(estdata_sl)),
  tar_target(ut_models, estimate_model(estdata_ut)),
  tar_target(sj_models, estimate_model(estdata_sj)),
  
  tar_target(sl_dc, sl_models[["All"]]),
  tar_target(ut_dc, ut_models[["All"]]),
  tar_target(sj_dc, sj_models[["All"]]),
  
  # 3.2 allocate counties to models
  # Salt Lake County
  tar_target(sl_orig, make_access_data(
    bg_acs, imputed_groceries, mcls,  
    geoids = ut_counties |> filter(NAME == "Salt Lake") |> pull(GEOID),
    completed_id = 1)),
  # Salt Lake no car
  tar_target(sl_orig_nocar, make_access_data(
    bg_acs, imputed_groceries, nocarmcls,  
    geoids = ut_counties |> filter(NAME == "Salt Lake") |> pull(GEOID),
    completed_id = 1)),
  # Other Wasatch Front Counties
  tar_target(wf_orig, make_access_data(
    bg_acs, imputed_groceries, mcls,  
    geoids = ut_counties |> filter(NAME %in% c("Utah", "Weber", "Davis")) |> 
      pull(GEOID), 
    completed_id = 1)),
  # Rural Utah
  tar_target(ru_orig, make_access_data(
    bg_acs, imputed_groceries, mcls,  
    geoids = ut_counties |> filter(!NAME %in% c("Salt Lake", "Utah", "Weber", "Davis")) |> 
      pull(GEOID), max_car = 180, completed_id = 1)),
  
  
  # 3.3 compute accessibility logsums
  tar_target(sl_access, compute_dclogsum(sl_orig, sl_dc)),
  tar_target(slnocar_access, compute_dclogsum(sl_orig_nocar, sl_dc)),
  tar_target(wf_access, compute_dclogsum(wf_orig, ut_dc)),
  tar_target(ru_access, compute_dclogsum(ru_orig, sj_dc)),
  tar_target(access, dplyr::bind_rows(sl_access, wf_access, ru_access)),
  
  
  # 3.4 Maps
  tar_target(utbgaccess, make_utbgaccess(access, bg, bg_acs)),
  tar_target(nocaraccess, make_nocaraccess(access, slnocar_access, bg, bg_acs))

)
