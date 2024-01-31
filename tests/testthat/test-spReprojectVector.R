
test_that("Reproject of SF Vector Object works", {

  skip_on_cran()

  WYspplt <- spMakeSpatialPoints(xyplt = WYplt,
                                 xy.uniqueid = "CN",
                                 xvar = "LON_PUBLIC",
                                 yvar = "LAT_PUBLIC",
                                 xy.crs = 4269)

  new_prj <- "+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

  # Reproject with new projection
  WYspplt.utm12 <- spReprojectVector(layer = WYspplt, crs.new = new_prj)

  # Retrieve new projection from SF object
  retrieved_prj <- sf::st_crs(WYspplt.utm12)

  # Check that retrieved projection contains input projection
  expect_contains(retrieved_prj, new_prj)
})
