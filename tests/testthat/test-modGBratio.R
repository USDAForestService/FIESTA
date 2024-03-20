
test_that("modGBratio Testing", {
  
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

  # Running GBratio - VOL
  modGBratio_VOL <- modGBratio(GBpop, 
                      landarea = "FOREST",
                      estvarn = "VOLCFNET",
                      estvarn.filter = "STATUSCD == 1",
                      rowvar = "FORTYPCD",
                      table_opts = list(row.FIAname = TRUE))

  # Running GBratio - TPA  
  modGBratio_TPA <- modGBratio(GBpop, 
                      landarea = "FOREST",
                      estvarn = "TPA_UNADJ",
                      estvarn.filter = "STATUSCD == 1",
                      rowvar = "FORTYPCD",
                      table_opts = list(row.FIAname = TRUE))

  # Running GBratio - BA  
  modGBratio_BA <- modGBratio(GBpop, 
                      landarea = "FOREST",
                      estvarn = "BA",
                      estvarn.filter = "STATUSCD == 1",
                      rowvar = "FORTYPCD",
                      table_opts = list(row.FIAname = TRUE))


  modGBratio_VOL_est <- modGBratio_VOL$est
  modGBratio_TPA_est <- modGBratio_TPA$est
  modGBratio_BA_est <- modGBratio_BA$est
  
  modGBratio_est_names <- list(c("Rocky Mountain juniper",
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
                                 "Bur oak",
                                 "Elm / ash / black locust",
                                 "Cottonwood",
                                 "Sugarberry / hackberry / elm / green ash",
                                 "Aspen",
                                 "Nonstocked",
                                 "Total"))

  expect_equal(list(modGBratio_VOL_est$`Forest type`), modGBratio_est_names) ##Ensuring names are consistant
  expect_equal(dim(modGBratio_VOL$est), c(19,3)) ##Ensuring consistant dimensionality among outputs
  expect_equal(dim(modGBratio_TPA$est), c(19,3))
  expect_equal(dim(modGBratio_BA$est), c(19,3))
  expect_snapshot(modGBratio_VOL_est) ##Outputting snapshot for each output
  expect_snapshot(modGBratio_TPA_est)
  expect_snapshot(modGBratio_BA_est)
})
