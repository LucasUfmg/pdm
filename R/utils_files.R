#' Run prioritization model
#'
#' Executes model fitting and prioritization for each month.
#'
#' @param name file name
#' @param package package necessary
#' @return shapefile
#' @export
# Internal helper to load shapefiles from inst/extdata
load_ext_shp <- function(name, package = "pdm") {

  shp_path <- system.file("extdata", paste0(name), package = package)

  print(paste("DEBUG path:", shp_path))

  if (shp_path == "") {
    stop("Shapefile not found in inst/extdata/: ", name, call. = FALSE)
  }

  sf::st_read(shp_path, quiet = TRUE)
}
