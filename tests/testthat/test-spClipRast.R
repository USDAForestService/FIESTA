
test_that("Clip Raster Works", {

  skip_on_cran()

  # Set Up

  fornffn <- system.file("extdata",
                         "sp_data/WYbighorn_forest_nonforest_250m.tif",
                         package = "FIESTA")

  WYbhdistfn <- system.file("extdata",
                            "sp_data/WYbighorn_districtbnd.shp",
                            package = "FIESTA")

  WYbhdist <- spImportSpatial(WYbhdistfn)

  WYbhMW <- WYbhdist[WYbhdist$DISTRICTNA == "Medicine Wheel Ranger District",]

  # Test spClipRaster
  clipped_rast_loc <- spClipRast(fornffn, clippolyv = WYbhMW, outfolder = tempdir())
  clipped_rast <- FIESTAutils::rasterInfo(clipped_rast_loc)

  exp_xsize <- 241
  exp_ysize <- 345
  clipped_xsize <- clipped_rast$xsize
  clipped_ysize <- clipped_rast$ysize

  expect_equal(exp_xsize, clipped_xsize)
  expect_equal(exp_ysize, clipped_ysize)
})