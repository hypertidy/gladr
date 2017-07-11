#' Metadata
#'
#' Just the metadata, for your interpretatation. 
#' 
#' "Metadata" here means "feature level" data, the rows in the spatial table. 
#' @param dsn data source name, i.e. a file path
#' @param layer named layer, numbered layer, or left otu
#' @param stringsAsFactors never
#' @param integer64 policy on these
#' @param verbose verbosity TRUE or FALSE
#' @return a data frame, no factors is the default
#' @export
#'
#' @importFrom rgdal ogrFIDs ogrInfo
#' @examples
#' inlandwaters <- otd(system.file("extdata", "datasource.gpkg", package = "gladr"))
#' ##mixed_poly_lines <- otd(system.file("extdata", "Drawing.mif", package = "gladr"))
otd <- function(dsn, layer = NULL, stringsAsFactors = FALSE, integer64 = "no.loss", verbose = TRUE) {
  layer <- choose_a_layer(dsn, layer, verbose)
  fids <- rgdal::ogrFIDs(dsn = dsn, layer = layer)
  if (attr(fids, "i") != attr(fids, "nf")) {
    retain <- 1:attr(fids, "i")
    afids <- 0:(attr(fids, "nf") - 1)
    deleted <- afids[!(afids %in% fids[retain])]
    warning(paste("Deleted feature IDs: ", paste(deleted, 
                                                 collapse = ", ")))
    fids <- fids[retain]
  }
  ## we can't have mixed topologies, this will error
  ogr_info <- rgdal::ogrInfo(dsn = dsn, layer = layer, 
                      encoding = NULL, use_iconv = FALSE, swapAxisOrder = FALSE, 
                      require_geomType = NULL)
  if (is.null(ogr_info$nListFields))  {
    nListFields <- 0
  } else {
    nListFields <- ogr_info$nListFields
  }
  
  int64 <- switch(integer64, allow.loss = 1L, warn.loss = 2L, 
                  no.loss = 3L)
  
  iflds <- as.integer((1:ogr_info$nitems) - 1)
  
  attr(iflds, "nListFields") <- as.integer(nListFields)
  nflds <- length(iflds)
  
  attr(iflds, "nflds") <- as.integer(nflds)
  attr(iflds, "int64") <- as.integer(int64)
  
  
  dlist <- .Call("ogrDataFrame", as.character(dsn), as.character(layer), 
                 as.integer(fids), iflds, PACKAGE = "rgdal")
  
  dlist
  #as.data.frame(dlist, stringsAsFactors = stringsAsFactors)
  
}