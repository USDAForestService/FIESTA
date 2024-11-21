test_that("modMAratio testing", {
  
  skip()
  
  MApopdat <- FIESTA::modMApop(popTabs = list(tree = FIESTA::WYtree,
                                              cond = FIESTA::WYcond),
                               pltassgn = FIESTA::WYpltassgn,
                               pltassgnid = "CN",
                               unitarea = FIESTA::WYunitarea,
                               unitvar = "ESTN_UNIT",
                               unitzonal = FIESTA::WYunitzonal,
                               prednames = c("dem", "tcc", "tpi", "tnt"),
                               predfac = "tnt")
  mod1 <- modMAratio(MApopdat,
                     estvarn = "VOLCFNET",
                     estvarn.filter = "STATUSCD == 1",
                     rowvar = "STDSZCD",
                     prednames = c("dem", "tcc", "tpi"))
  
  expect_snapshot(mod1$est)
  
})