

test_that("Export Shapefile Works", {

  skip_on_cran()

  # Set Output
  outfolder <- tempdir()
  
  # Import Spatial File
  WYbhfn <- system.file("extdata",
                        "sp_data/WYbighorn_adminbnd.shp",
                        package = "FIESTA")

  WYbh <- spImportSpatial(WYbhfn)

  expect_no_error(spExportSpatial(WYbh,
                                  savedata_opts = list(out_dsn = "WYbh.shp", 
                                                       outfolder = outfolder, 
                                                       overwrite_dsn = TRUE)))


  expect_output(spExportSpatial(WYbh,
                                savedata_opts = list(out_dsn = "WYbh.shp", 
                                                     outfolder = outfolder, 
                                                     overwrite_dsn = TRUE)))

})