test_that("spZonalRast works and has expected output", {
  
  skip_on_cran()

  skip_if(gdalraster::proj_version()$major < 9, "gdalraster proj version out of date")

  # Set up data
  WYbhdistfn <- system.file("extdata",
                            "sp_data/WYbighorn_districtbnd.shp",
                            package = "FIESTA")

  demfn <- system.file("extdata",
                       "sp_data/WYbighorn_dem_250m.img",
                       package = "FIESTA")

  WYbhdist <- spImportSpatial(WYbhdistfn)

  # Test Zonal Raster
  zonallst <- spZonalRast(polyv = WYbhdist, 
                          polyv.att = "DISTRICTNA", 
                          rastfn = demfn, 
                          zonalstat = c("mean", "sum", "npixels", "majority"))

  zonal_ext <- zonallst$zonalext
  expect_snapshot(zonal_ext)
})
