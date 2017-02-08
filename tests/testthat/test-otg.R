context("otg")

test_that("read geometry works", {
  inlandwaters <- otg(system.file("extdata", "datasource.gpkg", package = "gladr"))
  mixed_poly_lines <- otg(system.file("extdata", "Drawing.mif", package = "gladr"))
})
