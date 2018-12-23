#' Command to run epivis shiny application
#'
#' @details run a graphical user interface with epivis
#'
#' @import shiny
#' @export
run_epidrive <- function() {
  #note to self, ideally would like to launch in browser
  shiny::runApp("inst/epivis_shiny/", display.mode = "normal")
}