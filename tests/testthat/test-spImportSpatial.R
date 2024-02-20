

# Test 1
test_that("Spatial Import Works and returns Vector", {
  
  skip_on_cran()

  WYbhfn <- system.file("extdata",
                        "sp_data/WYbighorn_adminbnd.shp",
                        package = "FIESTA")

  expect_vector(spImportSpatial(WYbhfn))
})

# Test 2
test_that("Spatial Import throws appropriate error if invalid file", {
  
  skip_on_cran()

  nonExistent <- system.file("extdata",
                             "sp_data/not_real.shp",
                             package = "FIESTA")

  expect_message(spImportSpatial(nonExistent), "invalid spatial layer\n")
})