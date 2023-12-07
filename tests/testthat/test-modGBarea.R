
test_that("modGBarea Testing", {
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

  # Running GBarea  
  GBarea <- modGBarea(GBpop, 
                      landarea = "FOREST",
                      rowvar = "FORTYPCD",
                      table_opts = list(row.FIAname = TRUE))

  # GBarea outputed assigned to $est
  modGBarea_est <- GBarea$est

  area.raw <- round(GBarea$raw$rowest[1,3], digits = 1)
  area.raw_char <- as.character(area.raw)

  GBarea$raw$rowest

  modGBarea_est_names <- list(c("Rocky Mountain juniper",
                                "Juniper woodland",
                                "Pinyon / juniper woodland",
                                "Douglas-fir",
                                "Ponderosa pine",
                                "Engelmann spruce",
                                "Engelmann spruce / subalpine fir",
                                "Subalpine fir",
                                "Blue spruce", "Lodgepole pine",
                                "Limber pine", "Whitebark pine",
                                "Bur oak", "Elm / ash / black locust",
                                "Cottonwood", "Sugarberry / hackberry / elm / green ash", 
                                "Aspen", "Nonstocked", "Total"))

  expect_equal(GBarea$est[1, 2], area.raw_char) ##Converted numeric value to character and tested for correct value 
  expect_equal(list(modGBarea_est$`Forest type`), modGBarea_est_names) ##ensuring tree list is correct
  expect_equal(dim(modGBarea_est), c(19 , 3)) ##Ensuring dimensionality is consistant
  expect_snapshot(modGBarea_est) ##outputting snapshot
})
