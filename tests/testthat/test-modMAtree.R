test_that("modMAtree testing", {
  
  skip_on_cran()
  
  MApopdat <- modMApop(popTabs = list(tree = FIESTA::WYtree,
                                      cond = FIESTA::WYcond),
                       pltassgn = FIESTA::WYpltassgn,
                       pltassgnid = "CN",
                       unitarea = FIESTA::WYunitarea,
                       unitvar = "ESTN_UNIT",
                       unitzonal = FIESTA::WYunitzonal,
                       prednames = c("dem", "tcc", "tpi", "tnt"),
                       predfac = "tnt")
  
  
  est1 <- modMAtree(MApopdat = MApopdat,
                    MAmethod = "greg",
                    estvar = "VOLCFNET",
                    estvar.filter = "STATUSCD == 1")
  
  expect_equal(nrow(est1$est), nrow(MApopdat$unitlut))
  


})