# ------------------------------------------------------------------------------
#' @title Run the PinPath shiny app.
#'
#' @description This function runs the PinPath shiny app.
#' @param force.browser (optional) Logical (TRUE or FALSE). 
#' Should the app be opened the the browser?.
#' @return This function will run the PinPath shiny app
#' @examples 
#' \dontrun{
#' runPinPath()
#' }
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