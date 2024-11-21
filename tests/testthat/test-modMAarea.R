test_that("modMAarea testing", {
  
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
  
  mod1 <- modMAarea(MApopdat = MApopdat,
            MAmethod = "greg",
            landarea = "FOREST")
  
  expect_equal(nrow(mod1$est), nrow(MApopdat$unitlut))

  
})