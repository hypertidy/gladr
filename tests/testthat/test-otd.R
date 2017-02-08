context("otd")

test_that("read metadata works", {
  inlandwaters <- otd(system.file("extdata", "datasource.gpkg", package = "gladr"))
  mixed_poly_lines <- otg(system.file("extdata", "Drawing.mif", package = "gladr"))
})
