

test_that("Reproject Raster file works", {

  skip_on_cran()

  # Set Up Data
  fornffn <- system.file("extdata",
                         "sp_data/WYbighorn_forest_nonforest_250m.tif",
                         package = "FIESTA")

  # Reproject Raster
  new_prj <- "+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

  reprj_outfile <- spReprojectRaster(fornffn, crs.new = new_prj, outfolder = tempdir())
  reprj_rast <- FIESTAutils::rasterInfo(reprj_outfile)
  reprj_crs <- reprj_rast$crs

  # Check that raster Info CRS contains projected UTM Zone
  crs_contains_utmzone <- grepl("UTM zone 12N", reprj_crs)
  expect_true(crs_contains_utmzone)

  # Check that raster Info CRS contains projected Datum
  crs_contains_datum <- grepl("NAD83", reprj_crs)
  expect_true(crs_contains_datum)
})
