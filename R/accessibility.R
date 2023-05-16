#' Compute the destination choice logsum
#' 
#' @param origins
#' @param destinations
#' @param mcls
#' @param model
#' 
compute_dclogsum <- function(origins, destinations, mcls, model){
  
}


#' Read a destination choice model
#' 
#' @param file File with rds object
#' 
read_dc_fit <- function(file) {
  models <- readr::read_rds(file)
  models$`All-Logsum`
}