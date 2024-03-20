
test_that("Clip Point returns expected number of points", {
  
  skip_on_cran()

  # Set up
  WYbhfn <- system.file("extdata",
                        "sp_data/WYbighorn_adminbnd.shp",
                        package = "FIESTA")

  WYbh <- spImportSpatial(WYbhfn)

  # Test
  clipped_feature <- spClipPoint(xyplt = WYplt,
                                 uniqueid = "CN",
                                 clippolyv = WYbh,
                                 spMakeSpatial_opts = list(xvar = "LON_PUBLIC",
                                                           yvar = "LAT_PUBLIC",
                                                           xy.crs = 4269))

  expected_num <- 56
  clipped_num <- length(clipped_feature$clip_xyplt$CN) # Num of Features

  expect_equal(clipped_num, expected_num)
})