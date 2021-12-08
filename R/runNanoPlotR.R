#' Launch Shiny App For Package NanoPlotR
#'
#' A function that launches the shiny app for this package.
#' The shiny app allows user to input a RNA modification result file,
#' then app will plot the visualization plots.
#' The code has been placed in \code{./inst/shiny-scripts}.
#'
#' @return No return value but opens up a shiny page.
#'
#' @examples
#' \dontrun{
#' NanoPlotR::runNanoPlotR()
#' }
#'
#' @author Dae-won Gong \email{daewon.gong@mail.utoronto.ca}
#'
#' @references
#' Grolemund, G. (2015). Learn Shiny - Video Tutorials. \href{https://shiny.rstudio.com/tutorial/}{Link}
#'
#' @export
#' @importFrom shiny runApp
#'
runNanoPlotR <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "NanoPlotR")
  shiny::runApp(appDir, display.mode = "normal")
  return()
}

# [END]
