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
                      prednames = c("dem", "tcc", "tpi", "tnt"),
                      predfac = "tnt")


test2 <- modMAarea(MApopdat = MApopdat2,
          MAmethod = "greg")

SApopdat <- modSApop(popTabs = list(tree = FIESTA::WYtree,
                                    cond = FIESTA::WYcond),
                     pltassgn = FIESTA::WYpltassgn,
                     pltassgnid = "CN",
                     dunitarea = FIESTA::WYunitarea,
                     dunitvar = "ESTN_UNIT",
                     dunitzonal = FIESTA::WYunitzonal,
                     prednames = c("dem", "tcc", "tpi", "tnt"),
                     predfac = "tnt")


test <- modSAarea(SApopdatlst = SApopdat,
          prednames = c("dem", "tcc", "tpi", "tnt"),
          SApackage = "sae",
          SAmethod = "area",
          multest = FALSE,
          totals = TRUE)

SApopdat <- modSApop(pltdat = SApltdat, 
                     auxdat = auxdat,
                     smallbnd = WYbhdistfn,
                     smallbnd.domain = smallbnd.domain)