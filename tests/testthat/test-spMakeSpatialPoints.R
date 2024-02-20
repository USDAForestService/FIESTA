

test_that("Make Spatial Points works", {

  skip_on_cran()

  # Run Make Spatial Points with no Error
  expect_no_error(spMakeSpatialPoints(xyplt = WYplt,
                                      xy.uniqueid = "CN",
                                      xvar = "LON_PUBLIC",
                                      yvar = "LAT_PUBLIC",
                                      xy.crs = 4269))
})

test_that("Make Spatial Points works with Shapefile Export", {

  skip_on_cran()

  outfolder <- tempdir()

  # Run Make Spatial Points with Export with no Error
  expect_output(expect_no_error(spMakeSpatialPoints(xyplt = WYplt,
                                                    xy.uniqueid = "CN", 
                                                    xvar = "LON_PUBLIC", 
                                                    yvar = "LAT_PUBLIC",
                                                    xy.crs = 4269,
                                                    exportsp = TRUE,
                                                    savedata_opts = list(out_dsn = "spplt",
                                                                         out_fmt = "shp",
                                                                         outfolder = outfolder,
                                                                         out_layer = "WYplots",
                                                                         overwrite_layer = TRUE))))
})