#' runPinPath
#' @return This function will run the PinPath shiny app
#' @examples runPinPath()
#' @export
runPinPath <- function(force.browser = FALSE){
  appDir <- system.file("myapp", package = "PinPath")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `PinPath`.", call. = FALSE)
  }
  
  if (force.browser == FALSE){
    shiny::runApp(appDir, display.mode = "normal")
  } else{
    shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
  }
}