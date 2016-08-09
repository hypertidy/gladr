#' Geometry
#'
#' Just the geometry, for your interpretation.
#'
#' This function returns the raw output of the internal source function
#' 'R_OGR_CAPI_features' in 'rgdal'.
#' It is possible to have the same layer name in a folder of files, i.e
#' for MapInfo or shapefile, so don't do that.
#' @param x data source name
#' @param y layer name, or number, defaults to first layer '1'
#' @param verbose report on action if \code{TRUE} and \code{y} for layer not specified, the available layers will be reported
#'
#' @return raw list as per 'R_OGR_CAPI_FEATURES'
#' @export
#' @importFrom rgdal ogrListLayers
#' @examples
#'
#' #dsn <- system.file("extdata", "inlandwaters.gpkg", package = "ggpolypath")
#'
#' mt <- otg:::mtg(otg::otg(system.file("extdata", "inlandwaters.gpkg", package = "ggpolypath")))
#' plot(spbabel::sp(mt))
#'  plot(spbabel::sp(mt)[3, ], asp = "", col = "grey", rule = 'evenodd')
otg <-  function(x, y = NULL, verbose = TRUE) {
  layers <- rgdal::ogrListLayers(x)
  telldefault <- FALSE
  if (is.null(y)) {
    y <- 1L
    telldefault <- TRUE
  }
  if (is.numeric(y)) {
    if (y > length(layers)) stop(sprintf("data source has only %i layers", length(layers)))
    y <- layers[y]
  }
  if (!any(grepl(y, layers))) stop(sprintf("layer not found: %s", y))
  if (verbose & telldefault) {
    message(sprintf("found %i layers in %s:", length(layers), x))
    message(paste(layers, collapse = ", "))
    message("\n")
    message(sprintf("reading first layer:  %s", y))
  }
  .Call("R_OGR_CAPI_features", as.character(x),
                  as.character(y), comments=TRUE,
                  PACKAGE="rgdal")
}

atomics <- function(x) unlist(lapply(x, is.atomic))
## there's a general name function in gris
setIfNotNamed <- function(x) {
  abc <- letters[c(24:26, 1:23)]
  nms <- names(x)
  if (is.null(nms)) return(setNames(x, abc[seq_along(x)]))
  setNames(x, abc[set_along(x)])
}
tibulate <-  function(x) {
  if (all(atomics(x))) { # we're at the deepest level
    tibble::as_tibble(setIfNotNamed(x))
  } else {
    lapply(x, tibulate)
  }
}

#otg("inst/extdata/datasource.gpkg", "polygon")
mtg <- function(x) {
  dplyr::bind_rows(lapply(tibulate(x$crdlist),
         function(y) dplyr::bind_rows(lapply(y, bind_rows, .id = "island_"),
                                      .id = "branch_")), .id = "object_") %>%
    mutate(x_ = x, y_ = y, island_ = island_ == 1, order_ = row_number())
}


