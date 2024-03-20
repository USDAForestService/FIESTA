test_that("Get XY creates consistent result", {
  
  skip_on_cran()

  # Set up Data
  WYbhfn <- system.file("extdata",
                        "sp_data/WYbighorn_adminbnd.shp",
                        package = "FIESTA")

  # Run spGetXY and expect snapshots
  WYbhxy <- spGetXY(bnd = WYbhfn,
                    xy_datsource = "datamart",
                    eval = "FIA",
                    eval_opts = eval_options(Cur = TRUE),
                    returnxy = TRUE)

  pltids <- WYbhxy$pltids
  spxy <- WYbhxy$spxy

  expect_snapshot(pltids)
  expect_snapshot(spxy)


  # Run spGetXY returning only nonspatial pltids and expect snapshots
  WYbhxyids <- spGetXY(bnd = WYbhfn,
                       xy_datsource = "datamart",
                       eval = "FIA",
                       eval_opts = eval_options(Cur = TRUE),
                       returnxy = FALSE)

  nonsp_plotids <- WYbhxyids$pltids

  expect_snapshot(nonsp_plotids)
})
