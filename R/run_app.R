#' @title run_app
#'
#' @description
#' Execution of the shiny application. The application includes the performance of row-wise normalization,
#' as well as the total sum normalization, VST, VSN, quantile normalization, ComBat, and M-ComBat.
#' In addition, the application includes multiple pre-processing steps.
#' Moreover, it contains download options for the normalized values, as well as the resulting plots.
#' The plots and the normalized values can also be viewed directly together with the PCA plot score.
#'
#' @param local Boolean that states whether the app is called locally. If set, further options for manual downloads are included in the app.
#'
#' @return Executes the shiny application.
#'
#' @export
#'
#'

run_app <- function(local = FALSE) {
  # path to app.R file inside inst/shiny_app -> files inside inst are also downloaded by installation
  package_dir <- system.file(package = "rowwisenorm")  # path where package is installed
  app_r_path <- file.path(package_dir, "shiny_app", "app.R")  # path to app.R located in inst/shiny_app

  # appDir <- system.file("shiny_app", package = "rowwisenorm")

  # Set the "local" option
  options(local = local)

  shiny::shinyAppFile(
    file.path(app_r_path),
  )

  # alternatively: use app.R inside R/shiny_app, then working dir needs to be rowwisenorm
  # BUT: not sure where this is when user installs, the files in R folder are not installed -> will not be working
  # shiny::shinyAppDir(
  #   file.path("./R/shiny_app/"),  # assumes rowwisenorm as getwd() - R scripts not in user's installation!
  # )

}
