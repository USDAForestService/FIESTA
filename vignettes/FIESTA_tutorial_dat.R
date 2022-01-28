## ----setup, include = F-------------------------------------------------------
library(knitr)
knitr::opts_chunk$set(message = F, warning = F)

## ---- include=FALSE-----------------------------------------------------------
# Sets up output folding
hooks = knitr::knit_hooks$get()
hook_foldable = function(type) {
  force(type)
  function(x, options) {
    res = hooks[[type]](x, options)
    
    if (isFALSE(options[[paste0("fold.", type)]])) return(res)
    
    paste0(
      "<details><summary>", type, "</summary>\n\n",
      res,
      "\n\n</details>"
    )
  }
}
knitr::knit_hooks$set(
  output = hook_foldable("output"),
  plot = hook_foldable("plot")
)

## ---- message = F, warning = F------------------------------------------------
# Load library
library(FIESTA)

## -----------------------------------------------------------------------------

WYconddat.nfs <- datFilter(
        x = WYcond, 
        xfilter = "ADFORCD > 0"
        )

names(WYconddat.nfs)
WYcond.nfs <- WYconddat.nfs$xf
dim(WYcond.nfs)
head(WYcond.nfs)

## -----------------------------------------------------------------------------
WYtreedat.dead <- datFilter(
        x = WYtree, 
        xfilter = "STATUSCD == 2 & STANDING_DEAD_CD == 1 & SPCD == 746"
        )

names(WYtreedat.dead)
WYtree.deadasp <- WYtreedat.dead$xf
head(WYtree.deadasp)
dim(WYtree.deadasp)

dim(WYtree)

## -----------------------------------------------------------------------------
WYtreedat.dead2 <- datFilter(
        x = WYtree, 
        xfilter = "STATUSCD == 2 & STANDING_DEAD_CD == 1 & SPCD == 746",
        othertabnms = c("WYplt", "WYcond")
        )

names(WYtreedat.dead2)
WYtree.deadasp2 <- WYtreedat.dead2$xf
head(WYtree.deadasp2)
dim(WYtree.deadasp2)

WYtree.deadasptabs <- WYtreedat.dead2$cliptabs
names(WYtree.deadasptabs)
WYplt.deadasp <- WYtree.deadasptabs$clip_WYplt
WYcond.deadasp <- WYtree.deadasptabs$clip_WYcond

dim(WYplt.deadasp)
dim(WYcond.deadasp)

## -----------------------------------------------------------------------------
## Get number of plots by county
datFreq(
      x = WYplt, 
      xvar="COUNTYCD"
      )

## Get number of plots by county and plot status
datFreq(
      x = WYplt, 
      xvar=c("COUNTYCD", "PLOT_STATUS_CD")
      )

## Get number of plots by county and plot status with subtotals
datFreq(
      x = WYplt, 
      xvar = c("COUNTYCD", "PLOT_STATUS_CD"), 
      subtotal = TRUE
      )

## Get number of plots by county and plot status with subtotals
datFreq(
      x = WYplt, 
      xvar = c("COUNTYCD", "PLOT_STATUS_CD"), 
      subtotal = TRUE,
      subtotalcol = "COUNTYCD"
      )


## -----------------------------------------------------------------------------
## Get summed condition proportions by forest type class and stand size class
datPivot(
      x = WYcond, 
      pvar = "CONDPROP_UNADJ", 
      xvar = "FORTYPCD", 
      yvar = "STDSZCD"
      )

## Get average height by species group code and status code
datPivot(
      x = WYtree, 
      pvar = "HT", 
      xvar = "SPGRPCD", 
      yvar = "TREECLCD", 
      pfun = mean
      )

## -----------------------------------------------------------------------------
ref_diacl2in

WYtreelut <- datLUTclass(
                x = WYtree, 
                xvar = "DIA",
                LUT = ref_diacl2in, 
                LUTclassnm = "DIACL2IN"
                )

names(WYtreelut)
WYtree2 <- WYtreelut$xLUT
head(WYtree2)
dim(WYtree)
dim(WYtree2)

## -----------------------------------------------------------------------------
diacl25 <- data.frame(
              MIN = c(5,25), 
              MAX = c(25, 100), 
              DIACL25 = c("5.0-24.9", "25.0+" )
              )
diacl25


WYtreelut2 <- datLUTclass(
                  x = WYtree, 
                  xvar = "DIA", 
                  LUT = diacl25, 
                  LUTclassnm = "DIACL25"
                  )

names(WYtreelut2)
WYtree2 <- WYtreelut2$xLUT
head(WYtree2)
dim(WYtree)
dim(WYtree2)

## -----------------------------------------------------------------------------
cutbreaks <- c(0,25,50,100)
WYcondlut <- datLUTclass(
                x = WYcond, 
                xvar = "LIVE_CANOPY_CVR_PCT", 
                cutbreaks = cutbreaks
                )

names(WYcondlut)

head(WYcondlut$xLUT)
WYcondlut$LUT

## -----------------------------------------------------------------------------
head(ref_codes)
unique(ref_codes$VARIABLE)

## -----------------------------------------------------------------------------
ref_dstrbcd <- ref_codes[ref_codes$VARIABLE == "DSTRBCD",]
head(ref_dstrbcd)

## -----------------------------------------------------------------------------
WYcondlut <- datLUTnm(
              x = WYcond, 
              xvar = "DSTRBCD1", 
              LUT = ref_dstrbcd, 
              LUTvar = "VALUE",
              LUTnewvar = "MEANING", 
              LUTnewvarnm = "DSTRB1NM"
              )

names(WYcondlut)
WYcond2 <- WYcondlut$xLUT
head(WYcond2[WYcond2$DSTRBCD1 > 0, ])

## -----------------------------------------------------------------------------
WYcondlut2 <- datLUTnm(
                x = WYcond, 
                xvar = "DSTRBCD1", 
                FIAname = TRUE
                )

names(WYcondlut2)
WYcond3 <- WYcondlut2$xLUT
head(WYcond3[WYcond3$DSTRBCD1 > 0, ])

## -----------------------------------------------------------------------------
condsumdat <- datSumCond(
                cond = WYcond, 
                csumvar = "LIVE_CANOPY_CVR_PCT"
                )

names(condsumdat)

condsum <- condsumdat$condsum
head(condsum)

## -----------------------------------------------------------------------------
condsum[condsum$PLT_CN == 40405596010690,]
WYcond[WYcond$PLT_CN == 40405596010690,]

## -----------------------------------------------------------------------------
condsum <- datSumCond(
              cond = WYcond, 
              plt = WYplt, 
              csumvar = "LIVE_CANOPY_CVR_PCT"
              )$condsum
head(condsum)

## -----------------------------------------------------------------------------
condsum <- datSumCond(
              cond = WYcond, 
              plt = WYplt, 
              csumvar = "LIVE_CANOPY_CVR_PCT",
              cfilter = "STDSZCD == 1" 
              )$condsum
head(condsum)

## Check results
condsum[condsum$CN == 40405596010690,]
WYcond[WYcond$PLT_CN == 40405596010690,]

## -----------------------------------------------------------------------------
condnf <- datSumCond(
              cond = WYcond, 
              plt = WYplt, 
              csumvar = "CONDPROP_UNADJ", 
              csumvarnm = "cond_nf", 
              cfilter = "COND_STATUS_CD %in% c(2,3)"
              )$condsum

condnf[condnf$CN == 40404737010690,]
WYcond[WYcond$PLT_CN == 40404737010690,]

## -----------------------------------------------------------------------------
condres <- datSumCond(
              cond = WYcond, 
              plt = WYplt, 
              csumvar = "CONDPROP_UNADJ",
              csumvarnm = "cond_reserved", 
              cfilter = "RESERVCD == 1"
              )$condsum

WYcond[WYcond$PLT_CN == 46792188020004,]

## -----------------------------------------------------------------------------
treesumdat1 <- datSumTree(
                tree = WYtree, 
                plt = WYplt, 
                tsumvarlst = c("BA", "VOLCFNET"),
                TPA = FALSE, 
                tfilter = "STATUSCD == 1", 
                bycond = FALSE, 
                tround = 2
                )

names(treesumdat1)
treesum1 <- treesumdat1$treedat
head(treesum1)
treesumdat1$sumvars

## -----------------------------------------------------------------------------
treesumdat2 <- datSumTree(
                  tree = WYtree, 
                  plt = WYplt, 
                  tsumvarlst = c("BA", "VOLCFNET"),
                  TPA = TRUE, 
                  tfilter = "STATUSCD == 1", 
                  bycond = FALSE, 
                  tround = 2
                  )

names(treesumdat2)
treesum2 <- treesumdat2$treedat
head(treesum2)

## -----------------------------------------------------------------------------
checkvars <- c("PLT_CN", "CONDID", "SUBP", "TREE", "STATUSCD", "SPCD", "DIA", "HT", 
    "BA", "VOLCFNET", "TPA_UNADJ")
testplt <- WYtree[WYtree$PLT_CN == 40404758010690, checkvars]
testplt
sum(testplt[testplt$STATUSCD == 1, "BA"], na.rm=TRUE)
sum(testplt[testplt$STATUSCD == 1, "BA"] * testplt[testplt$STATUSCD == 1, "TPA_UNADJ"], na.rm=TRUE)

treesum1[treesum1$CN == 40404758010690,]
treesum2[treesum2$CN == 40404758010690,]

## -----------------------------------------------------------------------------
treesumdat3 <- datSumTree(
                  tree = WYtree, 
                  plt = WYplt, 
                  tsumvarlst = c("DIA", "HT"),
                  TPA = FALSE, 
                  tfun = mean, 
                  tfilter = "STATUSCD == 1", 
                  bycond = FALSE, 
                  tround = 2
                  )

names(treesumdat3)
treesum3 <- treesumdat3$treedat
head(treesum3)

## Test DIA and HT results for 1 plot
testplt
mean(testplt[testplt$STATUSCD == 1, "DIA"], na.rm=TRUE)
mean(testplt[testplt$STATUSCD == 1, "HT"], na.rm=TRUE)

treesum3[treesum3$CN == 40404758010690,]

## -----------------------------------------------------------------------------
treesumdat4a <- datSumTree(
                  tree = WYtree, 
                  plt = WYplt, 
                  tsumvarlst = "TPA_UNADJ",
                  TPA = TRUE, 
                  tfilter = "STATUSCD == 1", 
                  bycond = FALSE, 
                  tround = 2
                  )

names(treesumdat4a)
treesum4a <- treesumdat4a$treedat
head(treesum4a)

## -----------------------------------------------------------------------------
treesumdat4b <- datSumTree(
                  tree = WYtree, 
                  plt = WYplt, 
                  cond = WYcond, 
                  tsumvarlst = "VOLCFNET",
                  tfilter = "STATUSCD == 1", 
                  bycond = FALSE, 
                  getadjplot = TRUE
                  )

names(treesumdat4b)
treesum4b <- treesumdat4b$treedat
head(treesum4b)

## -----------------------------------------------------------------------------
treesumdat4c <- datSumTree(
                  tree = WYtree, 
                  plt = WYplt, 
                  cond = WYcond, 
                  tsumvarlst = "VOLCFNET",
                  tfilter = "STATUSCD == 1", 
                  bycond = FALSE, 
                  getadjplot = FALSE
                  )

treesum4c <- treesumdat4c$treedat

cn <- 40407815010690
WYcond[WYcond$PLT_CN == cn, ]
treesum4b[treesum4b$CN == cn, ]
treesum4c[treesum4c$CN == cn, ]

## -----------------------------------------------------------------------------
treesumdat5a <- datSumTree(
                  tree = WYtree, 
                  plt = WYplt, 
                  seed = WYseed, 
                  tsumvarlst = "TPA_UNADJ",
                  TPA = TRUE, 
                  addseed = TRUE, 
                  tfilter = "STATUSCD == 1", 
                  bycond = FALSE, 
                  tround = 2
                  )

names(treesumdat5a)
treesum5a <- treesumdat5a$treedat
head(treesum5a)

treesum5a[treesum5a$CN %in% cn,]
WYseed[WYseed$PLT_CN == cn,]

## -----------------------------------------------------------------------------
treesumdat5b <- datSumTree(
                  tree = WYtree, 
                  plt = WYplt, 
                  cond = WYcond, 
                  seed = WYseed, 
                  tsumvarlst = "TPA_UNADJ",
                  TPA = TRUE, 
                  addseed = TRUE, 
                  tfilter = "STATUSCD == 1", 
                  bycond = FALSE, 
                  tround = 2,
                  getadjplot  =TRUE
                  )

names(treesumdat5b)
treesum5b <- treesumdat5b$treedat
head(treesum5b)

treesum5a[treesum5a$CN %in% cn,]
treesum5b[treesum5b$CN %in% cn,]
WYcond[WYcond$PLT_CN %in% cn,]

## -----------------------------------------------------------------------------
treedomBA <- datSumTreeDom(
                tree = WYtree, 
                cond = WYcond, 
                plt = WYplt, 
                puniqueid = "CN", 
                bycond = FALSE, 
                tsumvar = "BA", 
                TPA = TRUE, 
                tdomtot = TRUE, 
                tdomtotnm = "BA_LIVE", 
                tdomprefix = "BA_LIVE", 
                tround = 2, 
                tfilter = "STATUSCD==1"
                )

names(treedomBA)
tdomdat <- treedomBA$tdomdat
tdomvarlut <- treedomBA$tdomvarlut
tdomlst <- treedomBA$tdomlst
tdomtotnm <- treedomBA$tdomtotnm

head(tdomdat)
tdomvarlut
tdomlst
tdomtotnm

dim(WYplt)
dim(tdomdat)

## -----------------------------------------------------------------------------
treedomCNT <- datSumTreeDom(
                tree = WYtree, 
                cond = WYcond, 
                plt = WYplt, 
                puniqueid = "CN", 
                bycond = FALSE, 
                tsumvar = "PLT_CN", 
                TPA = TRUE, 
                tdomtot = TRUE, 
                tdomprefix = "CNT", 
                tround = 0, 
                tfilter = "STATUSCD==1"
                )

names(treedomCNT)
tdomdat.tree <- treedomCNT$tdomdat
tdomvarlut <- treedomCNT$tdomvarlut
tdomlst <- treedomCNT$tdomlst
tdomtotnm <- treedomCNT$tdomtotnm

head(tdomdat.tree)

## -----------------------------------------------------------------------------
treedomCNTs <- datSumTreeDom(
                cond = WYcond, 
                plt = WYplt, 
                seed = WYseed, 
                puniqueid = "CN", 
                bycond = FALSE, 
                tsumvar = "PLT_CN", 
                TPA = TRUE, 
                tdomtot = TRUE, 
                tdomprefix = "CNT", 
                tround = 0
                )

names(treedomCNTs)
tdomdat.seed <- treedomCNTs$tdomdat
tdomvarlut <- treedomCNTs$tdomvarlut
tdomlst <- treedomCNTs$tdomlst
tdomtotnm <- treedomCNTs$tdomtotnm

head(tdomdat.seed)

## -----------------------------------------------------------------------------
treedomCNTs <- datSumTreeDom(
                tree = WYtree, 
                cond = WYcond, 
                plt = WYplt, 
                seed = WYseed, 
                puniqueid = "CN", 
                bycond  =FALSE, 
                tsumvar = "PLT_CN", 
                TPA = TRUE, 
                tdomtot = TRUE, 
                tdomprefix = "CNT", 
                tround = 0, 
                tfilter = "STATUSCD==1", 
                addseed = TRUE
                )

names(treedomCNTs)
tdomdat.treeseed <- treedomCNTs$tdomdat
tdomvarlut <- treedomCNTs$tdomvarlut
tdomlst <- treedomCNTs$tdomlst
tdomtotnm <- treedomCNTs$tdomtotnm

head(tdomdat.treeseed)

cn <- 40404730010690
tdomdat.tree[tdomdat.tree$CN == cn,]
tdomdat.seed[tdomdat.seed$CN == cn,]
tdomdat.treeseed[tdomdat.treeseed$CN == cn,]

## -----------------------------------------------------------------------------
treedomCNTs <- datSumTreeDom(
                  tree = WYtree, 
                  cond = WYcond, 
                  plt = WYplt, 
                  seed = WYseed, 
                  puniqueid ="CN", 
                  bycond = FALSE, 
                  tsumvar = "PLT_CN", 
                  TPA = TRUE, 
                  tdomtot = TRUE, 
                  tdomprefix = "CNT", 
                  savedata = FALSE, 
                  tfilter = "STATUSCD==1", 
                  addseed = TRUE, 
                  presence = TRUE
                  )

names(treedomCNTs)
tdomdat.pres <- treedomCNTs$tdomdat.pres

head(tdomdat.pres)

## -----------------------------------------------------------------------------
treedomCNTs <- datSumTreeDom(tree=WYtree, cond=WYcond, plt=WYplt, seed=WYseed, 
	    puniqueid="CN", bycond=FALSE, tsumvar="PLT_CN", TPA=TRUE, tdomtot=TRUE,
	    tdomprefix="CNT", savedata=FALSE, tround=0, tfilter="STATUSCD==1",
		  addseed=TRUE, presence=TRUE, proportion=TRUE)
names(treedomCNTs)
tdomdat.pres <- treedomCNTs$tdomdat.pres
tdomdat.prop <- treedomCNTs$tdomdat.prop

head(tdomdat.pres)

## -----------------------------------------------------------------------------
treedomCNTs <- datSumTreeDom(
                tree = WYtree, 
                cond = WYcond, 
                plt = WYplt, 
                seed = WYseed, 
                puniqueid = "CN", 
                bycond = FALSE, 
                tsumvar = "PLT_CN", 
                TPA = TRUE, 
                tdomtot = TRUE, 
                tdomprefix = "CNT", 
                tround = 0, 
                tfilter = "STATUSCD==1",
                addseed = TRUE, 
                presence = TRUE, 
                proportion = TRUE, 
                cover = TRUE, 
                tdombarplot = TRUE
                )

names(treedomCNTs)
tdomdat.pres <- treedomCNTs$tdomdat.pres
tdomdat.prop <- treedomCNTs$tdomdat.prop
tdomdat.cov <- treedomCNTs$tdomdat.cov

cn=40404742010690
tdomdat.tree[tdomdat.tree$CN == cn,]
tdomdat.seed[tdomdat.seed$CN == cn,]
tdomdat.treeseed[tdomdat.treeseed$CN == cn,]
tdomdat.pres[tdomdat.pres$CN == cn,]
tdomdat.prop[tdomdat.prop$CN == cn,]
tdomdat.cov[tdomdat.cov$CN == cn,]

## -----------------------------------------------------------------------------
treedomCNTs <- datSumTreeDom(
                tree = WYtree, 
                cond = WYcond, 
                plt = WYplt, 
                seed = WYseed, 
                puniqueid = "CN", 
                bycond = FALSE, 
                tsumvar = "PLT_CN", 
                TPA = TRUE, 
                tdomtot = TRUE, 
                tdomprefix = "CNT", 
                tround = 0, 
                tfilter = "STATUSCD==1", 
                addseed = TRUE, 
                presence = TRUE, 
                proportion = TRUE, 
                tdombarplot = TRUE
                )

names(treedomCNTs)
tdomdat.pres <- treedomCNTs$tdomdat.pres
tdomdat.prop <- treedomCNTs$tdomdat.prop

head(tdomdat.pres)

## -----------------------------------------------------------------------------
treedomBA <- datSumTreeDom(
                tree = WYtree, 
                cond = WYcond, 
                plt = WYplt, 
                puniqueid = "CN", 
                bycond = FALSE, 
                tsumvar = "BA", 
                TPA = TRUE, 
                tdomprefix = "BA", 
                tdomvarlst = 113, 
                tround = 2, 
                tfilter = "STATUSCD==1"
                )

names(treedomBA)
ba.limber <- treedomBA$tdomdat

head(ba.limber)

## -----------------------------------------------------------------------------
## Total basal area per acre by species and diameter class
DIALUT <- ref_diacl2in[ref_diacl2in$MIN <= 37, ]
names(DIALUT)[names(DIALUT) == "MEANING"] <- "DIACL2IN"

## Append diameter classes to tree table
datlut <- datLUTclass(
                    x = WYtree, 
                    xvar = "DIA", 
                    LUT = DIALUT, 
                    LUTclassnm = "DIACL2IN"
                    )
WYtree2 <- datlut$xLUT

## Species and diameter class
treedomDIACNTs <- datSumTreeDom(
                    tree = WYtree2, 
                    cond = WYcond, 
                    plt = WYplt, 
                    puniqueid = "CN", 
                    bycond = FALSE, 
                    tsumvar = "PLT_CN", 
                    TPA = TRUE, 
                    tdomtot = TRUE, 
                    tdomprefix = "CNT", 
                    tround = 2, 
                    tfilter = "STATUSCD==1 & DIA > 30", 
                    presence = TRUE, 
                    proportion = TRUE, 
                    tdomvar2 = "DIACL2IN", 
                    tdombarplot = TRUE
                    )

names(treedomDIACNTs)
tdomdat.pres <- treedomDIACNTs$tdomdat.pres
tdomdat.prop <- treedomDIACNTs$tdomdat.prop

head(tdomdat.pres)

