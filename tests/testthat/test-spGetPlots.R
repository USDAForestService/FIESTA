

test_that("multiplication works", {

  skip_on_cran()

  # Set Up Data
  WYbhfn <- system.file("extdata",
                        "sp_data/WYbighorn_adminbnd.shp",
                        package = "FIESTA")

  # Run spGetPlots and expect snapshots
  WYbhdat <- spGetPlots(bnd = WYbhfn,
                        states = "Wyoming",
                        datsource = "datamart",
                        eval = "FIA",
                        eval_opts = eval_options(Cur = TRUE),
                        istree = FALSE)

  pltids <- WYbhdat$pltids
  spxy <- WYbhdat$spxy

  expect_snapshot(pltids)
  expect_snapshot(spxy)

})
