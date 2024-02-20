
test_that("Extract Polygon Attributes works", {
  
  skip_on_cran()

  # Set Up Data
  WYspplt <- spMakeSpatialPoints(xyplt = WYplt,
                                 xy.uniqueid = "CN",
                                 xvar = "LON_PUBLIC",
                                 yvar = "LAT_PUBLIC",
                                 xy.crs = 4269)

  WYbhdistfn <- system.file("extdata",
                            "sp_data/WYbighorn_districtbnd.shp",
                            package = "FIESTA")

  WYbhdist <- spImportSpatial(WYbhdistfn)

  # Test Extract Provides warning if Null Values
  expect_warning(spExtractPoly(WYspplt,
                               xy.uniqueid = "CN",
                               polyvlst = WYbhdist))

  # Test if plots have expected number of columns after extract (19->23)
  extpolylst <- suppressWarnings(spExtractPoly(WYspplt,
                                               xy.uniqueid = "CN",
                                               polyvlst = WYbhdist))

  ext_plts <- extpolylst$spxyext
  exp_names <- 23
  extracted_num <- length(names(ext_plts))
  expect_equal(extracted_num, exp_names)
  expect_snapshot(ext_plts)

  # Test if plots have expected number of columns after extract
  # where keepNA = false, and polyvarlst applied (19->21)
  extpolylst2 <- suppressWarnings(spExtractPoly(WYspplt,
                                                xy.uniqueid = "CN",
                                                polyvlst = WYbhdist,
                                                polyvarlst = c("FORESTNUMB",
                                                               "FORESTNAME"),
                                                keepNA = FALSE))

  ext_plts2 <- extpolylst2$spxyext
  exp_names2 <- 20
  extracted_num2 <- length(names(ext_plts2))
  expect_equal(extracted_num2, exp_names2)
  expect_snapshot(ext_plts2)

})