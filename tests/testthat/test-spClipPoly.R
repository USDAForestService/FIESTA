
test_that("Clip polygon works as expected", {
  
  skip_on_cran()

  # Setup Admin Boundary to Clip
  WYbhfn <- system.file("extdata",
                        "sp_data/WYbighorn_adminbnd.shp",
                        package = "FIESTA")

  WYbh <- spImportSpatial(WYbhfn)

  # Setup District Boundary and subset to clip by
  WYbhdistfn <- system.file("extdata",
                            "sp_data/WYbighorn_districtbnd.shp",
                            package = "FIESTA")

  WYbhdist <- spImportSpatial(WYbhdistfn)

  subset_district <- WYbhdist[WYbhdist$DISTRICTNU == "01",]

  # Run Clipped Function
  clipped <- spClipPoly(polyv = WYbh, clippolyv = subset_district, areacalc = TRUE)

  # Check Original Acres
  # (If this changes than the expected clip may not be accurate)
  exp_original_acres <- 1112790
  original_acres <- round(WYbh$GIS_ACRES)
  expect_equal(original_acres, exp_original_acres)

  # Check Calculated Area
  exp_acrs <- 334252
  clipped_acres <- round(clipped$ACRES_GIS)
  expect_equal(clipped_acres, exp_acrs)
})