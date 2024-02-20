test_that("spGetStrata returns expected result", {
  
  skip_on_cran()

  skip_if(gdalraster::proj_version()$major < 9, "gdalraster proj version out of date")

  # Set Up Data
  WYbhfn <- system.file("extdata",
                        "sp_data/WYbighorn_adminbnd.shp",
                        package = "FIESTA")
  fornffn <- system.file("extdata",
                         "sp_data/WYbighorn_forest_nonforest_250m.tif",
                         package = "FIESTA")


  WYspplt <- spMakeSpatialPoints(xyplt = WYplt, 
                                 xy.uniqueid = "CN", 
                                 xvar = "LON_PUBLIC", 
                                 yvar = "LAT_PUBLIC", 
                                 xy.crs = 4269)
  # Test spGetStrata
  stratlst <- suppressWarnings(spGetStrata(WYspplt, 
                                           uniqueid = "CN", 
                                           unit_layer = WYbhfn, 
                                           strattype = "RASTER", 
                                           strat_layer = fornffn))

  expect_snapshot(stratlst$stratalut)
})
