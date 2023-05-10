
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
    pivot_wider(id_cols = c("resource", "blockgroup"), names_from = mode,
                values_from = c(duration, transfers, walktime, waittime, transittime)) |>
    filter(!is.na(duration_CAR))
  
  if(is.null(w_times$transittime_TRANSIT)) w_times$transittime_TRANSIT <- NA
  if(is.null(w_times$waittime_TRANSIT)) w_times$waittime_TRANSIT <- NA
  if(is.null(w_times$walktime_TRANSIT)) w_times$walktime_TRANSIT <- NA
  
  lsum <- w_times %>%
    mutate(
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
    select(blockgroup, resource, contains("utility")) %>%
    pivot_longer(cols = contains("utility"), 
                 names_to = "mode", names_prefix = "utility_", values_to = "utility") %>%
    group_by(resource, blockgroup) %>%
    summarise(mclogsum = logsum(utility))
  
  left_join(w_times, lsum, by = c("resource", "blockgroup"))
  
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
