

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

  # regular single value extraction
  extract1 <- suppressWarnings(spExtractRast(WYspplt,
                                             rastlst = c(fornffn, demfn),
                                             xy.uniqueid = "CN",
                                             keepNA = FALSE))

  # Test if plots have expected number of columns after extract 
  ext_plts <- extract1$sppltext
  exp_names <- 21
  extracted_num <- length(names(ext_plts))
  expect_equal(extracted_num, exp_names)
  expect_snapshot(ext_plts)
  
  
  # window extraction
  # windowstat = "mean"
  extract2 <- suppressWarnings(spExtractRast(WYspplt,
                                             rastlst = c(demfn),
                                             xy.uniqueid = "CN",
                                             windowsize = 3,
                                             windowstat = "mean",
                                             keepNA = FALSE))
  
  expect_equal(dim(extract2$sppltext), c(121, 20))
  
  # windowstat = "value"
  extract3 <- suppressWarnings(spExtractRast(WYspplt,
                                             rastlst = c(demfn),
                                             xy.uniqueid = "CN",
                                             windowsize = 3,
                                             windowstat = "value",
                                             keepNA = FALSE))
  
  expect_equal(dim(extract3$sppltext), c(121, 28))
  

})
