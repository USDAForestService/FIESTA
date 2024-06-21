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
  expect_snapshot(est1$est)
  
  # shouldn't sum to totals when modelselect is done on a per-estn_unit basis
  expect_warning(
    modMAtree(MApopdat = MApopdat,
              MAmethod = "greg",
              estvar = "BA",
              modelselect = TRUE,
              estvar.filter = "STATUSCD == 1",
              rowvar = "FORTYPCD")
  )
  
  # check estimates w rowvar & colvar set
  est2 <- modMAtree(MApopdat = MApopdat,
                    MAmethod = "greg",
                    estvar = "BA",
                    sumunits = TRUE,
                    table_opts = table_options(allin1 = TRUE),
                    estvar.filter = "STATUSCD == 1",
                    rowvar = "FORTYPCD",
                    colvar = "STDSZCD")
  
  expect_snapshot(est2$est)
  
  # check estimates for gregEN + FIA adjustment option with set prednames
  est3 <- modMAtree(MApopdat = MApopdat,
                    MAmethod = "gregEN",
                    estvar = "TPA_UNADJ",
                    estvar.filter = "STATUSCD == 1",
                    prednames = c("tcc", "dem"),
                    FIA = FALSE)
  
  expect_equal(dim(est3$est), c(23, 3))
  pred_used <- names(est3$raw$predselectlst$totest)[-c(1,2)]
  expect_equal(pred_used, c("tcc", "dem"))
  

})