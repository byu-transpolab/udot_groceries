
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
#' 
calculate_times <- function(landuse, bgcentroids, merged_osm_file, gtfs = NULL,  
                            landuselimit = NULL, bglimit = NULL,
                            max_trip_duration = 120){
  
  # start connection to r5
  if(!file.exists(merged_osm_file)) stop("OSM file not present.")
  if(!file.exists(gtfs)) stop("GTFS file not present.")
  r5r_core <- r5r::setup_r5(dirname(merged_osm_file), verbose = FALSE)
  
  
  # get lat / long for the landuse and the centroids
  ll <- get_latlong(landuse)
  bg <- get_latlong(bgcentroids)
  
  # limit the number of cells for the time calculations (for debugging)
  if(!is.null(landuselimit)) ll <- ll |>  dplyr::sample_n(landuselimit)
  if(!is.null(bglimit)) bg <- bg |>  dplyr::sample_n(bglimit)
  
  # routing inputs
  departure_datetime <- as.POSIXct("10-05-2023 08:00:00",
                                   format = "%d-%m-%Y %H:%M:%S")
  time_window <- 60L # how many minutes are scanned
  percentiles <- 25L # assumes riders have some knowledge of transit schedules
  
  
  # get the car travel times
  car_tt <- r5r::travel_time_matrix(
    r5r_core,
    bg,
    ll,
    mode = "CAR",
    departure_datetime = departure_datetime,
    time_window = time_window,
    percentiles = percentiles,
    breakdown = FALSE, # don't need detail for car trips
    breakdown_stat = "min",
    max_trip_duration = max_trip_duration,
    verbose = FALSE,
    progress = TRUE
  ) |> 
    dplyr::mutate(mode = "CAR")
  
  # get the walk times
  walk_tt <- r5r::travel_time_matrix(
    r5r_core,
    bg,
    ll,
    mode = "WALK",
    departure_datetime = departure_datetime,
    time_window = time_window,
    percentiles = percentiles,
    breakdown = FALSE,
    breakdown_stat = "min",
    max_walk_dist = 10000, # in meters
    max_trip_duration = max_trip_duration,
    walk_speed = 3.6, # meters per second
    verbose = FALSE,
    progress = TRUE
  ) |> 
    mutate(mode = "WALK")
  
  
  # get the transit times
  transit_tt <- r5r::travel_time_matrix(
    r5r_core,
    bg,
    ll,
    mode = "TRANSIT",
    mode_egress = "WALK",
    departure_datetime = departure_datetime,
    time_window = time_window,
    percentiles = percentiles,
    breakdown = TRUE,
    breakdown_stat = "mean",
    max_walk_dist = 1000, # in meters
    max_trip_duration = max_trip_duration,
    walk_speed = 3.6, # meters per second
    verbose = FALSE,
    progress = TRUE
  ) |> 
    mutate(mode = "TRANSIT")  %>%
    filter(n_rides > 0)
  
  
  alltimes <- bind_rows(
    transit_tt, 
    car_tt, 
    walk_tt,
  ) |> 
    transmute(
      blockgroup = fromId,
      resource = toId,
      mode = mode,
      duration = travel_time,
      transfers = n_rides,
      walktime = access_time + egress_time,
      waittime = wait_time,
      transittime = ride_time
    ) |> 
    # keep only the shortest itinerary by origin / destination / mode
    # this is necessary because the parks have multiple points.
    group_by(resource, blockgroup, mode) |> 
    arrange(duration, .by_group = TRUE) |> 
    slice(1) |> 
    as_tibble()
  
  stop_r5()
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
#' 
#' @return A tibble with the mode choice logsum for each resource / blockgroup
#'   pair
calculate_logsums <- function(times, utilities, walkspeed = 2.8) {
  
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
    dplyr::group_by(resource, blockgroup) %>%
    dplyr::summarise(mclogsum = logsum(utility))
  
  dplyr::left_join(w_times, lsum, by = c("resource", "blockgroup"))
  
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
