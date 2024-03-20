
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
  GBpop$estvar.area

  # List of Names for output types
  GBpop_names <- list(c("module","popType", "condx", "pltcondx", "cuniqueid", "condid", "ACI",
                        "ACI.filter", "pltassgnx", "pltassgnid", "unitarea", "areavar", "areaunits", "unitvar", "unitvars",
                        "strata", "stratalut",   "strvar", "strwtvar", "expcondtab", "plotsampcnt",
                        "condsampcnt", "states", "invyrs", "estvar.area", "adj", "P2POINTCNT",
                        "treex", "tuniqueid", "adjtree", "stratwarnlut"))


  GBpop_names_actual <- list(names(GBpop))

  expect_equal(GBpop_names_actual, GBpop_names) ##Checking if pop names are changing -  may not be the best test, can be subject to change
  expect_equal(dim(GBpop$treex), c(18380, 21)) ##Checking various dimensions of GBpop outputs
  expect_equal(dim(GBpop$condx), c(3210, 13)) 
  expect_equal(dim(GBpop$unitarea), c(23, 2)) 
})
