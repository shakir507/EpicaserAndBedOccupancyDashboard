run_app <- function(...) {

  app_file <- system.file(
    "shiny",
    "app_GalwayTwenteJoint.R",
    package = "EpicaserAndBedOccupancyDashboard"
  )

  if (!nzchar(app_file)) {
    stop("Dashboard file was not found in the installed package.")
  }

  shiny::runApp(appDir = app_file, ...)
}
