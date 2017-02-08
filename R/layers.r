choose_a_layer <- function(dsn, layer = NULL,  verbose = TRUE) {
  layers <- rgdal::ogrListLayers(dsn)
  telldefault <- FALSE
  if (is.null(layer)) {
    layer <- 1L
    telldefault <- TRUE
  }
  if (is.numeric(layer)) {
    if (layer > length(layers)) stop(sprintf("data source has only %i layers", length(layers)))
    layer <- layers[layer]
  }
  if (!any(grepl(layers, layers))) stop(sprintf("layer not found: %s \n available layers are %s", layer, paste(layers, collapse = ", ")))
  if (verbose & telldefault) {
    message(sprintf("found %i layers in %s:", length(layers), dsn))
    message(paste(layers, collapse = ", "))
    message("\n")
    message(sprintf("reading first layer:  %s", layer))
  }
  layer
}