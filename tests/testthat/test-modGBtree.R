
test_that("modGBtree Testing", {
  
  skip_on_cran()

  # Running GBpop
  GBpop <- modGBpop(popTabs = list(cond = WYcond, tree = WYtree),
                    pltassgn = WYpltassgn,
                    pltassgnid = "CN",
                    unitarea = WYunitarea,
                    unitvar = "ESTN_UNIT",
                    stratalut = WYstratalut,
                    strvar = "STRATUMCD",
                    strata = TRUE,
                    strata_opts = strata_options(getwt = TRUE))

  # Running GBtree - TPA  
  GBtree_TPA <- modGBtree(GBpop,
                          estvar = "TPA_UNADJ",
                          estvar.filter = "STATUSCD == 1",
                          rowvar = "FORTYPCD",
                          sumunits = TRUE,
                          table_opts = list(row.FIAname = TRUE, col.FIAname=TRUE))

  # Runing GBtree - VOL  
  GBtree_VOL <- modGBtree(GBpop,
                          estvar = "VOLCFNET",
                          estvar.filter = "STATUSCD == 1",
                          rowvar = "FORTYPCD",
                          sumunits = TRUE,
                          table_opts = list(row.FIAname = TRUE, col.FIAname=TRUE))
  

  # GBtree outputs assigned to $est
  modGBtree_TPA_est <- GBtree_TPA$est
  modGBtree_VOL_est <- GBtree_VOL$est



  # List of names for each tree type
  modGBtree_TPA_est_names <- list(c("Rocky Mountain juniper",
                                    "Juniper woodland",
                                    "Pinyon / juniper woodland",
                                    "Douglas-fir",
                                    "Ponderosa pine",
                                    "Engelmann spruce",
                                    "Engelmann spruce / subalpine fir",
                                    "Subalpine fir",
                                    "Blue spruce",
                                    "Lodgepole pine",
                                    "Limber pine",
                                    "Whitebark pine",
                                    "Bur oak", "Elm / ash / black locust",
                                    "Cottonwood",
                                    "Sugarberry / hackberry / elm / green ash",
                                    "Aspen",
                                    "Nonstocked",
                                    "Total"))

  expect_equal(list(modGBtree_TPA_est$`Forest type`), modGBtree_TPA_est_names) ##ensuring tree names are consistant across output
  expect_equal(dim(modGBtree_TPA_est) , c(19, 3)) ##ensuring dimension of modGB_TPA is correct
  expect_equal(dim(modGBtree_VOL_est) , c(19, 3)) ##ensuring dimension of modGB VOL is correct
  expect_snapshot(modGBtree_TPA_est) ##Outputting snapshot
  expect_snapshot(modGBtree_VOL_est) ##Outputting snapshot

})
