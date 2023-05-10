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