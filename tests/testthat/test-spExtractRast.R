

test_that("Extract Raster Values works", {
  
  skip_on_cran()

  # Set Up Data
  WYspplt <- spMakeSpatialPoints(xyplt = WYplt,
                                 xy.uniqueid = "CN",
                                 xvar = "LON_PUBLIC",
                                 yvar = "LAT_PUBLIC",
                                 xy.crs = 4269)

  fornffn <- system.file("extdata",
                         "sp_data/WYbighorn_forest_nonforest_250m.tif",
                         package = "FIESTA")

  demfn <- system.file("extdata",
                       "sp_data/WYbighorn_dem_250m.img",
                       package = "FIESTA")

  extrastlst <- suppressWarnings(spExtractRast(WYspplt,
                                               rastlst = c(fornffn, demfn),
                                               xy.uniqueid = "CN",
                                               keepNA = FALSE))

  # Test if plots have expected number of columns after extract (19->21)
  ext_plts <- extrastlst$sppltext
  exp_names <- 21
  extracted_num <- length(names(ext_plts))
  expect_equal(extracted_num, exp_names)
  expect_snapshot(ext_plts)

})
