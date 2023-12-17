#' @title run_app
#'
#' @description
#' Execution of the shiny application. The application includes the performance of row-wise normalization,
#' as well as the total sum normalization, VST, VSN, quantile normalization, ComBat, and M-ComBat.
#'
#' @param local Boolean that states whether the app is called locally. If set, further options for manual downloads are included in the app.
#'
#' @return executes the shiny application
#'
#' @export
#'
#'

# run_app <- function(local=F){
#   #runApp("./R/shiny_app/app.R")  # assumes current working directory to be rowwisenorm
#
#   #args <- if (local) "local" else ""  # *1 both ways is_local FALSE
#   args <- ifelse(local, "TRUE", "FALSE")  # *2 both ways is_local TRUE
#
#   #args <- as.character(local)
#   print(args)
#   print(class(args))
#
#   # options(shiny.args = args)
#   # runApp("./R/shiny_app/app.R")
#
#   shiny::shinyAppDir(
#     system.file("./R/shiny_app", package = "rowwisenorm"),
#     options = list(shiny.args = args)
#   )
# }
#
# run_app <- function(local = FALSE) {
#  origin_wd <- getwd()
#  setwd()
#
# }
#
# run_app <- function(local = FALSE) {
#   args <- ifelse(local, "TRUE", "FALSE")
#   print(args)
#
#   app_dir <- system.file("./R/shiny_app", package = "rowwisenorm")
#   runApp(appDir = app_dir, options = list(shiny.args = args))
# }

run_app <- function(local = FALSE) {
  args <- ifelse(local, "TRUE", "FALSE")

  app_dir <- system.file("shiny_app", package = "rowwisenorm")

  shiny::shinyAppDir(
    app_dir,
    options = list(shiny.args = args)
  )
}
