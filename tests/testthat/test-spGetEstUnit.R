test_that("Get Estimation Unit returns expected result", {
  
  skip_on_cran()

  # Set Up Data
  WYbhfn <- system.file("extdata",
                        "sp_data/WYbighorn_adminbnd.shp",
                        package = "FIESTA")

  WYbhdistfn <- system.file("extdata",
                            "sp_data/WYbighorn_districtbnd.shp",
                            package = "FIESTA")

  # Get Est Unit acres for Bighorn National Forest
  unitdat.bh <- suppressWarnings(spGetEstUnit(xyplt = WYplt,
                                              uniqueid = "CN",
                                              unit_layer = WYbhfn,
                                              spMakeSpatial_opts=list(xvar = "LON_PUBLIC",
                                                                      yvar = "LAT_PUBLIC",
                                                                      xy.crs = 4269)))


  result <- head(unitdat.bh)
  expect_snapshot(result)

  # Get Est Unit acres and strata information for Bighorn National Forest
  unitdat.bhdist <- suppressWarnings(spGetEstUnit(xyplt = WYplt,
                                                  uniqueid = "CN",
                                                  unit_layer = WYbhdistfn,
                                                  unitvar = "DISTRICTNA",
                                                  spMakeSpatial_opts=list(xvar = "LON_PUBLIC",
                                                                          yvar = "LAT_PUBLIC",
                                                                          xy.crs = 4269)))

  result2 <- head(unitdat.bhdist)
  expect_snapshot(result2)
})
