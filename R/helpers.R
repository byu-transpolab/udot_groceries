#' Run a shell script
#' 
#' @param script Path to script that this function will run
#' 
#' @return 
#' 
run_shell_script <- function(script, outfile) {
  ret <- processx::run(script, spinner = TRUE, echo = FALSE,
                       env = c(PATH = "/opt/homebrew/bin:/bin"))
  
  if (ret$status == 0) {
    if (file.exists(outfile)) {
      return(outfile)
    } else {
      stop("Could not identify output file")
    }
  } else {
    message(ret$stdout)
    message(ret$stderr)
    stop("======= Error in shell script ==========")
  }
}


#' Get GTFS File for OpenTripPlanner
#' 
#' @param file to data file
#' @return Stores a GTFS file in the appropriate location
#' 
get_gtfs <- function(path){
  if(!file.exists(path)){
    # originally from UTA: May 4 2023
    download.file("https://gtfsfeed.rideuta.com/gtfs.zip",
                  destfile = path)
  } else {
    message(path, " already available")
  }
  return(path) # to use file target, need to return path to data. 
}
