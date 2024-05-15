#' Update times for the Salt Lake improved transport scenario
#' 
#' @param times
#' 
make_newtimes <- function(times, dists, bgs){
  
  # times we are leaving alone
  unadjusted <- times |> 
    filter(!(blockgroup %in% bgs & grepl("SL-", resource)))
  
  adjusted <- times |> 
    filter(blockgroup %in% bgs & grepl("SL-", resource)) |> 
    left_join(dists, by = c("blockgroup" = "bg", "resource")) |> 
    mutate(
      # make walk times based on distance and walking speed only
      distance_meters = ifelse(
        as.numeric(distance_meters) > 10000,
        NA,
        as.numeric(distance_meters)
      ),
      
      # eliminate waiting for transit 
      waittime = case_when(
        mode == "TRANSIT" ~ pmin(10, waittime),
        TRUE ~ waittime
      ),
      walktime = case_when(
        mode == "TRANSIT" ~ pmin(10, walktime),
        TRUE ~ walktime
      ),
      
      
      duration = case_when(
        mode == "WALK" ~ sqrt(2) * distance_meters / 1.07 / 60, # meters / (3.5 fps = 1.07 meters/second) / (60 seconds / minute)
        mode == "TRANSIT" ~ walktime + waittime + transittime,
        TRUE ~ duration 
      ),
    ) 
  
  bind_rows(unadjusted, adjusted)
}

#' Update times so that there are no travel costs to reach stores with deliveries.
#' 
#' @param times
#' @param dists
#' @param stores
make_delivery_times <- function(times, dists, stores){
  times <- mutate(times, id = row_number())
  
  threshold <- 3000
  units(threshold) <- "m"
  unadjusted <- times |> 
    left_join(dists, by = c("blockgroup" = "bg", "resource")) |> 
    filter((! resource %in% stores$id) |  distance_meters > threshold)
  
  adjusted <- times |> 
    filter(!id %in% unadjusted$id) |> 
    mutate(duration = 0,
           transfers = 0,
           walktime = 0,
           waittime = 0,
           transittime = 0)
  
  bind_rows(unadjusted, adjusted) |> select(-id)
}


#' Calculate euclidean distances between blockgroups and stores
#' 
#' @param bgcentroids
#' @param all_groceries
#' 
make_dists <- function(bgcentroids, all_groceries){
  
  d <- st_distance(bgcentroids, all_groceries)
  rownames(d) <- bgcentroids$id
  colnames(d) <- all_groceries$id
  
  d |> 
    as_tibble(rownames = "bg") |>  
    pivot_longer(-bg, names_to = "resource", values_to = "distance_meters")
}

#' Calculate multimodal travel times between bgcentroids and destinations
#' 
#' @param landuse Destination features
#' @param bgcentroids Population-weighted blockgroup centroid
#' @param merged_osm_file path to osm pbf file
#' @param gtfs path to gtfs zip file
#' @param landuselimit The maximum number of resources to sample. Default NULL means all included
#' @param bglimit The maximum number of block groups included in sample. deafult NULL means all included
#' 
#' @return A tibble with times between Block groups and resources by multiple modes
#' 
#' @details Parallelized, will use parallel::detectCores() - 1
#' @examples
#' # settin' up for debuggin'
#' options(java.parameters = '-Xmx10G')
#' library(targets)
#' library(r5r)
#' library(tidyverse)
#' tar_load(merged_osm_file)
#' tar_load(gtfs)
#' landuse <- tar_read(all_groceries) |> filter(county == "Salt Lake")
#' bgcentroids <- tar_read(bgcentroids) |> filter(substr(id, 3,5) == "035")
#' source("R/travel_times.R")
#' landuselimit = 10; bglimit = 10
#' max_trip_duration = 120
#' 
calculate_times <- function(landuse, bgcentroids, merged_osm_file, gtfs = NULL,  
                            landuselimit = NULL, bglimit = NULL,
                            max_trip_duration = 120){
  
  # start connection to r5
  if(!file.exists(merged_osm_file)) stop("OSM file not present.")
  if(!file.exists(gtfs)) stop("GTFS file not present.")
  r5r_core <- r5r::setup_r5(dirname(merged_osm_file), verbose = FALSE)
  on.exit(r5r::stop_r5())
  
  
  # get lat / long for the landuse and the centroids
  ll <- get_latlong(landuse)
  bg <- get_latlong(bgcentroids)
  
  # limit the number of cells for the time calculations (for debugging)
  if(!is.null(landuselimit)) ll <- ll |>  dplyr::sample_n(landuselimit)
  if(!is.null(bglimit)) bg <- bg |>  dplyr::sample_n(bglimit)
  
  # routing inputs
  departure_datetime <- as.POSIXct("05-09-2023 08:00:00",
                                   format = "%d-%m-%Y %H:%M:%S")
  time_window <- 60L # how many minutes are scanned
  percentiles <- 25L # assumes riders have some knowledge of transit schedules
  
  
  # get the car travel times
  message("Getting transit times")
  car_tt <- r5r::travel_time_matrix(
    r5r_core,
    bg,
    ll,
    mode = "CAR",
    departure_datetime = departure_datetime,
    time_window = time_window,
    percentiles = percentiles,
    max_trip_duration = max_trip_duration,
    verbose = FALSE,
    progress = TRUE
  ) |> 
    dplyr::mutate(mode = "CAR") |> 
    rename(total_time = travel_time_p25)
  
  # get the walk times
  message("Getting transit times")
  walk_tt <- r5r::travel_time_matrix(
    r5r_core,
    bg,
    ll,
    mode = "WALK",
    departure_datetime = departure_datetime,
    time_window = time_window,
    percentiles = percentiles,
    max_trip_duration = max_trip_duration,
    walk_speed = 3.6, # km/hr
    verbose = FALSE,
    progress = TRUE
  ) |> 
    mutate(mode = "WALK") |> 
    rename(total_time = travel_time_p25)
  
  
  # get the transit times
  message("Getting transit times")
  transit_tt <- r5r::expanded_travel_time_matrix(
    r5r_core,
    # only do this for block groups on the wasatch front
    bg |> filter(substr(id, 3, 5) %in% c(
      "011", # davis
      "035", # salt lake
      "057", # weber
      "049"  #utah
      )),
    ll,
    mode = "TRANSIT",
    mode_egress = "WALK",
    departure_datetime = departure_datetime,
    time_window = time_window,
    breakdown = TRUE,
    max_trip_duration = 120,
    walk_speed = 3.6, # km/hr
    verbose = FALSE,
    progress = TRUE
  ) |> 
    as_tibble() |> 
    filter(n_rides > 0) |> 
    mutate(mode = "TRANSIT")  |> 
    group_by(from_id, to_id) |> 
    arrange(total_time) |> 
    slice(1)
  
  
  alltimes <- bind_rows(
    transit_tt, 
    car_tt, 
    walk_tt,
  ) |> 
    ungroup() |> 
    transmute(
      blockgroup = from_id,
      resource = to_id,
      mode = mode,
      duration = total_time,
      transfers = n_rides - 1,
      walktime = access_time + egress_time,
      waittime = wait_time,
      transittime = ride_time
    ) |> 
    as_tibble()
  
  alltimes
}


#' Function to get lat / long from sf data as matrix
#' 
#' @param sfc A simple features collection
#' @return A data frame with three columns, id, LATITUDE and LONGITUDE
#' 
#' @details If sfc is a polygon, will first calculate the centroid.
#' 
get_latlong <- function(sfc){
  
  suppressWarnings(
    tib <- sfc |>
      sf::st_centroid() |> # will always warn for constant geometry
      sf::st_transform(4326) |>
      dplyr::transmute(
        id = as.character(id),
        lat = sf::st_coordinates(geometry)[, 2],
        lon = sf::st_coordinates(geometry)[, 1],
      ) |>
      sf::st_set_geometry(NULL)
  )
  
  tib
}



#' Calculate mode choice logsums
#' 
#' @param times A tibble returned from calculate_times
#' @param utilities A list of mode choice utilities
#' @param walkspeed Assumed walking speed in miles per hour
#' @param nocar 
#' 
#' @return A tibble with the mode choice logsum for each resource / blockgroup
#'   pair
calculate_logsums <- function(times, utilities, walkspeed = 2.8, nocar = FALSE) {
  
  w_times <- times |>
    tidyr::pivot_wider(id_cols = c("resource", "blockgroup"), names_from = mode,
                values_from = c(duration, transfers, walktime, waittime, transittime)) |>
    dplyr::filter(!is.na(duration_CAR))
  
  if(is.null(w_times$transittime_TRANSIT)) w_times$transittime_TRANSIT <- NA
  if(is.null(w_times$waittime_TRANSIT)) w_times$waittime_TRANSIT <- NA
  if(is.null(w_times$walktime_TRANSIT)) w_times$walktime_TRANSIT <- NA
  
  lsum <- w_times %>%
    dplyr::mutate(
      utility_CAR = as.numeric(
        utilities$CAR$constant + duration_CAR * utilities$CAR$ivtt
      ),
      utility_TRANSIT = as.numeric(
        utilities$TRANSIT$constant + 
          transittime_TRANSIT * utilities$TRANSIT$ivtt + 
          waittime_TRANSIT * utilities$TRANSIT$wait + 
          walktime_TRANSIT * utilities$TRANSIT$access
      ), 
      utility_WALK = as.numeric(
        utilities$WALK$constant + 
          duration_WALK * utilities$WALK$ivtt + 
          ifelse(walktime_WALK > utilities$WALK$distance_threshold,
                 # minutes * hr / min * mi/hr *  util / mi
                 walktime_WALK / 60 * walkspeed * utilities$WALK$long_distance,
                 walktime_WALK / 60 * walkspeed * utilities$WALK$short_distance
          )
      ), 
      
    ) %>%
    dplyr::select(blockgroup, resource, contains("utility")) %>%
    tidyr::pivot_longer(cols = contains("utility"), 
                 names_to = "mode", names_prefix = "utility_", values_to = "utility") %>%
    dplyr::group_by(resource, blockgroup) 
 
  if(nocar){
    lsum <- lsum |> 
      dplyr::filter(mode != "CAR") |> 
      dplyr::summarise(mclogsum = logsum(utility))
  } else {
    lsum <- lsum |> 
      dplyr::summarise(mclogsum = logsum(utility))
  }
  
  
  dplyr::left_join(w_times, lsum, by = c("resource", "blockgroup")) |> 
    dplyr::filter(!is.infinite(mclogsum))
  
}

logsum <- function(utility){
  log(sum(exp(utility), na.rm = TRUE))
}

prob_u <- function(utility){
  exp(utility) / sum(exp(utility), na.rm = TRUE)
}

read_utilities <- function(file){
  
  read_json(file, simplifyVector = TRUE)
  
}
