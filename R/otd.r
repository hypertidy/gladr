otd <- dont_use_this_egregious_hack_to_read_a_dataframe_with_GDAL <- function(dsn, layer, stringsAsFactors = FALSE) {
  fids <- ogrFIDs(dsn = dsn, layer = layer)
  if (attr(fids, "i") != attr(fids, "nf")) {
    retain <- 1:attr(fids, "i")
    afids <- 0:(attr(fids, "nf") - 1)
    deleted <- afids[!(afids %in% fids[retain])]
    warning(paste("Deleted feature IDs: ", paste(deleted, 
                                                 collapse = ", ")))
    fids <- fids[retain]
  }
  ogr_info <- ogrInfo(dsn = dsn, layer = layer, 
                      encoding = NULL, use_iconv = FALSE, swapAxisOrder = FALSE, 
                      require_geomType = NULL)
  if (is.null(ogr_info$nListFields))  {
    nListFields <- 0
  } else {
    nListFields <- ogr_info$nListFields
  }
  
  integer64 <- "no.loss"
  int64 <- switch(integer64, allow.loss = 1L, warn.loss = 2L, 
                  no.loss = 3L)
  
  iflds <- as.integer((1:ogr_info$nitems) - 1)
  
  attr(iflds, "nListFields") <- as.integer(nListFields)
  nflds <- length(iflds)
  
  attr(iflds, "nflds") <- as.integer(nflds)
  attr(iflds, "int64") <- as.integer(int64)
  
  
  dlist <- .Call("ogrDataFrame", as.character(dsn), as.character(layer), 
                 as.integer(fids), iflds, PACKAGE = "rgdal")
  
  as.data.frame(dlist, stringsAsFactors = stringsAsFactors)
  
}