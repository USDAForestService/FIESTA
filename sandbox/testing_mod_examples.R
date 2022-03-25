MApopdat <- modMApop(popTabs = list(tree = WYtree, cond = WYcond),
                     pltassgn = WYpltassgn,
                     auxdat = modeldat)

MApopdat2 <- modMApop(popTabs = list(tree = FIESTA::WYtree,
                                     cond = FIESTA::WYcond),
                      pltassgn = FIESTA::WYpltassgn,
                      pltassgnid = "CN",
                      unitarea = FIESTA::WYunitarea,
                      unitvar = "ESTN_UNIT",
                      unitzonal = FIESTA::WYunitzonal,
                      prednames = c("dem", "tcc", "tpi"),
                      predfac = "tnt")


test <- modMAarea(MApopdat = MApopdat2,
          MAmethod = "greg")
