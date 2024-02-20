test_that("Union Poly Works and creates expected SF object", {
  
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
  subset_district <- WYbhdist[WYbhdist$DISTRICTNU == "03",]

  polyUnion <- suppressWarnings(spUnionPoly(polyv1 = subset_district,
                                polyv2 = WYbh,
                                areacalc = TRUE))

  expect_snapshot(polyUnion)
})
