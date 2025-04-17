
# For `FIESTA`'s GB Module, the `modGBpop` function calculates and outputs: number of plots, adjustment factors, and an expansion factor by strata.
test_that("modGBpop Testing", {

  skip_on_cran()

  # Running GBpop
  GBpop <- modGBpop(popTabs = list(cond=WYcond, tree=WYtree),
                    pltassgn = WYpltassgn,
                    pltassgnid = "CN",
                    unitarea = WYunitarea,
                    unitvar = "ESTN_UNIT",
                    stratalut = WYstratalut,
                    strvar = "STRATUMCD",
                    strata = TRUE,
                    strata_opts = strata_options(getwt = TRUE))

  expect_equal(dim(GBpop$treex), c(18574, 19)) ##Checking various dimensions of GBpop outputs
  expect_equal(dim(GBpop$pltcondx), c(3224, 29))
  expect_equal(dim(GBpop$unitarea), c(23, 2))
})
