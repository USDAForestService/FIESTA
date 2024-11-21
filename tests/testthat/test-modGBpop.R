
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


  # List of Names for output types
  GBpop_names <- list(c('module', 'popType', 'pltidsadj', 'pltcondx',
                        'pltcondflds', 'pjoinid', 'cuniqueid', 'condid', 'ACI',
                        'areawt', 'areawt2', 'adjcase', 'dbqueries',
                        'dbqueriesWITH', 'pltassgnx', 'pltassgnid', 'unitarea',
                        'areavar', 'areaunits', 'unitvar', 'unitvars', 'unitltmin',
                        'strata', 'stratalut', 'strvar', 'strwtvar', 'plotsampcnt',
                        'condsampcnt', 'states', 'invyrs', 'adj', 'P2POINTCNT',
                        'plotunitcnt', 'treex', 'tuniqueid', 'adjfactors',
                        'adjvarlst', 'popdatindb'))


  GBpop_names_actual <- list(names(GBpop))

  expect_equal(GBpop_names_actual, GBpop_names) ##Checking if pop names are changing -  may not be the best test, can be subject to change
  expect_equal(dim(GBpop$treex), c(18574, 19)) ##Checking various dimensions of GBpop outputs
  expect_equal(dim(GBpop$pltcondx), c(3224, 29)) 
  expect_equal(dim(GBpop$unitarea), c(23, 2)) 
})
