#' Read attributes from source as a data frame
#'
#' See here for SQL details http://www.gdal.org/ogr_sql.html
#' @param dsource data source string
#' @param layer layer index (0)
#' @param sql optional sql query executed against the data source
#'
#' @return
#' @export
#' @importFrom vapour vapour_read_attributes
#' @examples
#' f <- system.file("extdata", "point.shp", package = "vapour")
#' read_gdal_table(f, sql = "SELECT * FROM point WHERE FID > 4 AND FID < 8")
#' file <- "list_locality_postcode_meander_valley.tab"
#' mvfile <- system.file(file.path("extdata/tab", file), package="vapour")
#' read_gdal_table(mvfile, sql = "SELECT NAME, PLAN_REF, SHAPE_AREA * 2 AS area FROM list_locality_postcode_meander_valley WHERE POSTCODE < 7304")
#' read_gdal_table(mvfile)
read_gdal_table <- function(dsource, layer = 0, sql = NULL) {
  assertthat::is.scalar(layer)
  assertthat::is.number(layer)
  assertthat::is.string(dsource)
  if (is.null(sql)) sql <- ""
  assertthat::is.string(sql)
  tibble::as_tibble(vapour_read_attributes(dsource, layer, sql))
}


#' Read geometry from source as a blob vector.
#'
#' @param dsource data source string
#' @param layer layer index (0)
#'
#' @return
#' @export
#' @importFrom blob new_blob
#' @importFrom vapour vapour_read_geometry
#' @examples
#' file <- "list_locality_postcode_meander_valley.tab"
#' mvfile <- system.file(file.path("extdata/tab", file), package="vapour")
#' tibble::tibble(wkb = read_gdal_geometry(mvfile))
read_gdal_geometry <- function(dsource, layer = 0, sql = NULL) {
  assertthat::is.scalar(layer)
  assertthat::is.number(layer)
  assertthat::is.string(dsource)
  if (is.null(sql)) sql <- ""
  assertthat::is.string(sql)
  blob::new_blob(vapour_read_geometry(dsource, layer, sql) )
}

