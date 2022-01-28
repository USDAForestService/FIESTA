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

## ---- warning = F, message = F------------------------------------------------
library(FIESTA)

## -----------------------------------------------------------------------------
## Set options
options(scipen = 6)
options(stringsAsFactors = FALSE)

## Set outfolder
outfolder <- tempdir()

## -----------------------------------------------------------------------------

# File names for spatial layers, stored as external data objects in FIESTA. 
WYbhfn <- system.file("extdata", "sp_data/WYbighorn_adminbnd.shp", package="FIESTA")
fornffn <- system.file("extdata", "sp_data/WYbighorn_forest_nonforest_250m.tif", package="FIESTA")


# Get estimation unit and strata information for Bighorn National Forest.
stratdat.bh <- spGetStrata(
      xyplt = WYplt,
      uniqueid = "CN", 
      unit_layer = WYbhfn, 
      strat_layer = fornffn,
      spMakeSpatial_opts = list(xvar = "LON_PUBLIC", 
                                yvar = "LAT_PUBLIC", 
                                xy.crs = 4269)
      )

## Get names of output list components
names(stratdat.bh)


## Plot assignment of strata and estimation unit (ONEUNIT, STRATUMCD)
head(stratdat.bh$pltassgn)

## Area by estimation unit
stratdat.bh$unitarea

## Pixel counts and strata weights (strwt) by strata and estimation unit
stratdat.bh$stratalut

## Variable names
stratdat.bh$unitvar        # Estimation unit variable
stratdat.bh$strvar         # Strata variable
stratdat.bh$areavar        # Area variable



## -----------------------------------------------------------------------------
# File names for external spatial data 
WYbhdistfn <- system.file("extdata", "sp_data/WYbighorn_districtbnd.shp", package="FIESTA")
WYbhdist.att <- "DISTRICTNA"
fornffn <- system.file("extdata", "sp_data/WYbighorn_forest_nonforest_250m.tif", package="FIESTA")

# Get estimation unit and strata information for Bighorn National Forest Districts
stratdat.bhdist <- spGetStrata(
      xyplt = WYplt,
      uniqueid = "CN", 
      unit_layer=WYbhdistfn, 
      unitvar=WYbhdist.att,
      strat_layer=fornffn,
      spMakeSpatial_opts = list(xvar = "LON_PUBLIC", 
                                yvar = "LAT_PUBLIC", 
                                xy.crs = 4269)
      )

## Get names of output list components
names(stratdat.bhdist)


## Plot assignment of strata and estimation unit (DISTRICTNA, STRATUMCD)
head(stratdat.bhdist$pltassgn)

## Area by estimation units (Districts)
stratdat.bhdist$unitarea

## Pixel counts and strata weights (strwt) by strata and estimation unit
stratdat.bhdist$stratalut


## Variable names
stratdat.bhdist$unitvar        # Estimation unit variable
stratdat.bhdist$strvar         # Strata variable
stratdat.bhdist$areavar        # Area variable


## ---- results = FALSE, message = F--------------------------------------------

## ---- results = FALSE, message = F--------------------------------------------
GBpopdat <- modGBpop(
  popTabs = list(cond = FIESTA::WYcond,          # FIA plot/condition data
                 tree = FIESTA::WYtree,          # FIA tree data
                 seed = FIESTA::WYseed),         # FIA seedling data
  popTabIDs = list(cond = "PLT_CN"),             # unique ID of plot in cond
  pltassgn = FIESTA::WYpltassgn,  # plot assignments
  pltassgnid = "CN",              # unique ID of plot in pltassgn
  pjoinid = "PLT_CN",             # plot id to join to pltassgn
  unitarea = WYunitarea,          # area by estimation units
  unitvar = "ESTN_UNIT",          # name of estimation unit
  strata = TRUE,                  # if using post-stratification
  stratalut = WYstratalut,        # strata classes and pixels counts
  strata_opts = strata_options(getwt = TRUE)              # strata options
  )



## ---- results = T-------------------------------------------------------------
names(GBpopdat)

## -----------------------------------------------------------------------------
## Look at output from GBpopdat
GBpopdat$plotsampcnt	# Number of plots by plot status
GBpopdat$condsampcnt	# Number of conditions by condition status
# Strata-level population data, including number of plots and adjustment factors
GBpopdat$stratalut  
## Adjustment factors added to condition-level data
GBpopdat$condx
## Adjustment factors added to tree data
GBpopdat$treex
## Adjustment factors added to seedling data
GBpopdat$seedx

## ---- results = TRUE, eval = FALSE--------------------------------------------
#  qry <- "select estn_unit, stratumcd, p1pointcnt, p2pointcnt, expns,
#            adj_factor_macr, adj_factor_subp, adj_factor_micr from pop_stratum
#          where evalid = 561301 order by estn_unit, stratumcd"
#  pop_stratum <- DBqryCSV(
#                    qry,
#                    states="Wyoming",
#                    sqltables="pop_stratum"
#                    )
#  head(pop_stratum)
#  head(GBpopdat$stratalut)

## ---- results = FALSE, message = F--------------------------------------------

## -----------------------------------------------------------------------------

## Bighorn National Forest

## Using output list from spGetStrata()
GBpopdat.bh <- modGBpop(
      popTabs=list(plt=WYplt, cond=WYcond, tree=WYtree, seed=WYseed),
      stratdat=stratdat.bh)

## Get names of output list components
names(GBpopdat.bh)


## Using output as individual parameter inputs
GBpopdat.bh <- modGBpop(
      popTabs=list(plt=WYplt, cond=WYcond, tree=WYtree, seed=WYseed),
      popTabIDs=list(plt="CN"),
      pltassgn=stratdat.bh$pltassgn, 
      pltassgnid="CN", 
      unitvar=stratdat.bh$unitvar, 
      unitarea=stratdat.bh$unitarea, 
      areavar=stratdat.bh$areavar, 
      strata=TRUE,
      stratalut=stratdat.bh$stratalut, 
      strvar=stratdat.bh$strvar
      )

## Get names of output list components
names(GBpopdat.bh)

## Condition information with adjusted condition proportions for area
head(GBpopdat.bh$condx)

## Tree information with tree-level adjustment factors
head(GBpopdat.bh$treex)

## Seedling information with adjustment factors
head(GBpopdat.bh$seedx)

## Strata-level information, including number of plots by strata and strata-level adjustment factors
GBpopdat.bh$stratalut



## ---- results = FALSE, message = F--------------------------------------------

## -----------------------------------------------------------------------------

## Bighorn National Forest District


## Using output list from spGetStrata()
GBpopdat.bhdist <- modGBpop(
      popTabs=list(plt=WYplt, cond=WYcond, tree=WYtree, seed=WYseed), 
      stratdat=stratdat.bhdist)

## Get names of output list components
names(GBpopdat.bhdist)

GBpopdat.bhdist <- modGBpop(
      popTabs=list(plt=WYplt, cond=WYcond, tree=WYtree, seed=WYseed), 
      pltassgn=stratdat.bhdist$pltassgn, 
      pltassgnid="CN", 
      unitvar=stratdat.bhdist$unitvar, 
      unitarea=stratdat.bhdist$unitarea, 
      areavar=stratdat.bhdist$areavar,
      strata=TRUE,
      stratalut=stratdat.bhdist$stratalut, 
      strvar=stratdat.bhdist$strvar
      )


## Get names of output list components
names(GBpopdat.bhdist)
 
## Condition information with adjusted condition proportions for area
head(GBpopdat.bhdist$condx)

## Tree information with tree-level adjustment factors
head(GBpopdat.bhdist$treex)

## Seedling information with adjustment factors
head(GBpopdat.bhdist$seedx)

## Strata-level information, including number of plots by strata and strata-level adjustment factors
GBpopdat.bhdist$stratalut


## -----------------------------------------------------------------------------
area1.1 <- modGBarea(
    GBpopdat = GBpopdat,      # pop - population calculations for WY, post-stratification
    landarea = "FOREST",      # est - forest land filter
    sumunits = TRUE,          # est - sum estimation units to population
    )


## ---- results = T-------------------------------------------------------------
names(area1.1)

## ---- results = T-------------------------------------------------------------
area1.1$est

## ---- results = TRUE----------------------------------------------------------
## Raw data (list object) for estimate
raw1.1 <- area1.1$raw        # extract raw data list object from output
names(raw1.1)
head(raw1.1$unit_totest)    # estimates by estimation unit (i.e., ESTN_UNIT)
raw1.1$totest               # estimates for population (i.e., WY)


## -----------------------------------------------------------------------------
## Area of forest land by forest type, Wyoming, 2011-2013
area1.2 <- modGBarea(
    GBpopdat = GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea = "FOREST",         # est - forest land filter
    rowvar = "FORTYPCD",         # est - row domain
    sumunits = TRUE,             # est - sum estimation units to population
    returntitle = TRUE           # out - return title information
    )


## -----------------------------------------------------------------------------
names(area1.2)

## -----------------------------------------------------------------------------
## Estimate and percent sampling error of estimate
area1.2$est

## -----------------------------------------------------------------------------
## Raw data (list object) for estimate
raw1.2 <- area1.2$raw      # extract raw data list object from output
names(raw1.2)
head(raw1.2$unit_totest) # estimates by estimation unit (i.e., ESTN_UNIT)
raw1.2$totest            # estimates for population (i.e., WY)
head(raw1.2$unit_rowest) # estimates by row, by estimation unit (i.e., ESTN_UNIT)
head(raw1.2$rowest)      # estimates by row for population (i.e., WY)


## Titles (list object) for estimate
titlelst1.2 <- area1.2$titlelst
names(titlelst1.2)
titlelst1.2


## -----------------------------------------------------------------------------
## Area of forest land by forest type and stand-size class, Wyoming, 2011-2013
area1.3 <- modGBarea(
    GBpopdat = GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea = "FOREST",         # est - forest land filter
    rowvar = "FORTYPCD",         # est - row domain
    colvar = "STDSZCD",          # est - column domain
    sumunits = TRUE,             # est - sum estimation units to population
    savedata = TRUE,             # out - save data to outfolder
    returntitle = TRUE,          # out - return title information
    table_opts = list(
      row.FIAname = TRUE,          # table - row domain names
      col.FIAname = TRUE,          # table - column domain names
      allin1 = TRUE                # table - return output with est(pse)
      ),
    savedata_opts = list(
      outfolder = outfolder,       # save - outfolder for saving data
      outfn.pre = "WY"             # save - prefix for output files
      )
    )

## -----------------------------------------------------------------------------
## Look at output list
names(area1.3)

## Estimate and percent sampling error of estimate
head(area1.3$est)


## Raw data (list object) for estimate
raw1.3 <- area1.3$raw      # extract raw data list object from output
names(raw1.3)
head(raw1.3$unit_totest) # estimates by estimation unit (i.e., ESTN_UNIT)
head(raw1.3$totest)      # estimates for population (i.e., WY)
head(raw1.3$unit_rowest) # estimates by row, by estimation unit (i.e., ESTN_UNIT)
head(raw1.3$rowest)      # estimates by row for population (i.e., WY)
head(raw1.3$unit_colest) # estimates by column, by estimation unit (i.e., ESTN_UNIT)
head(raw1.3$colest)      # estimates by column for population (i.e., WY)
head(raw1.3$unit_grpest) # estimates by row and column, by estimation unit (i.e., ESTN_UNIT)
head(raw1.3$grpest)      # estimates by row and column for population (i.e., WY)


## Titles (list object) for estimate
titlelst1.3 <- area1.3$titlelst
names(titlelst1.3)
titlelst1.3


## List output files in outfolder
list.files(outfolder, pattern = "WY_area")
list.files(paste0(outfolder, "/rawdata"), pattern = "WY_area")

## -----------------------------------------------------------------------------
area2.1 <- modGBarea(
    GBpopdat = GBpopdat.bh,       # pop - population calculations for Bighorn NF, post-stratification
    landarea = "FOREST",          # est - forest land filter
    sumunits = FALSE,             # est - sum estimation units to population
    rowvar = "FORTYPCD",          # est - row domain
    colvar = "STDSZCD",           # est - column domain
    returntitle = TRUE,           # out - return title information
    title_opts = list(
      title.ref = "Bighorn National Forest, 2011-2013"  # title - customize title reference
      ),
    table_opts = list(   
      row.FIAname = TRUE,         # table - return FIA row names
      col.FIAname = TRUE          # table - return FIA column names
      )
    )

## ---- results = T-------------------------------------------------------------
names(area2.1)

## ---- results = T-------------------------------------------------------------
area2.1$est

## ---- results = TRUE----------------------------------------------------------
## Raw data (list object) for estimate
raw2.1 <- area2.1$raw      # extract raw data list object from output
names(raw2.1)
head(raw2.1$unit_grpest)  # estimates by row and group domains


## Titles (list object) for estimate
titlelst2.1 <- area2.1$titlelst
names(titlelst2.1)
titlelst2.1


## -----------------------------------------------------------------------------

area2.2 <- modGBarea(
    GBpopdat = GBpopdat.bh,        # pop - population calculations for Bighorn NF, post-stratification
    landarea = "FOREST",           # est - forest land filter
    sumunits = TRUE,               # est - sum estimation units to population
    rowvar = "FORTYPGRPCD",        # est - row domain
    colvar = "DSTRBCD1",           # est - column domain
    returntitle = TRUE,            # out - return title information
    title_opts = list(
      title.ref = "Bighorn National Forest, 2011-2013"  # title - customize title reference
      ),
    table_opts = list(   
      row.FIAname = TRUE,          # table - return FIA row names
      col.FIAname = TRUE,          # table - return FIA column names
      estnull = 0,
      rowlut = c(180, 200, 220, 260, 280, 900, 999),
      raw.keep0 = TRUE
      )
    )


## ---- results = T-------------------------------------------------------------
names(area2.2)

## ---- results = T-------------------------------------------------------------
area2.2$est

## ---- results = TRUE----------------------------------------------------------
## Raw data (list object) for estimate
raw2.2 <- area2.2$raw      # extract raw data list object from output
names(raw2.2)
head(raw2.2$unit_grpest)  # estimates by row and group domains


## Titles (list object) for estimate
titlelst2.2 <- area2.2$titlelst
names(titlelst2.2)
titlelst2.2


## -----------------------------------------------------------------------------
area3.1 <- modGBarea(
    GBpopdat = GBpopdat.bhdist,    # pop - population calculations for Bighorn NF, post-stratification
    landarea = "FOREST",           # est - forest land filter
    sumunits = TRUE,               # est - sum estimation units to population
    pcfilter = "DSTRBCD1 > 0",     # est - condition filter for table output
    rowvar = "FORTYPGRPCD",        # est - row domain
    colvar = "DSTRBCD1",           # est - column domain
    returntitle = TRUE,            # out - return title information
    title_opts = list(
      title.ref = "Bighorn National Forest, 2011-2013"  # title - customize title reference
      ),
    table_opts = list(   
      row.FIAname = TRUE,          # table - return FIA row names
      col.FIAname = TRUE           # table - return FIA column names
      )
    )


## ---- results = T-------------------------------------------------------------
names(area3.1)

## ---- results = T-------------------------------------------------------------
area3.1$est

## ---- results = TRUE----------------------------------------------------------
## Raw data (list object) for estimate
raw3.1 <- area3.1$raw       # extract raw data list object from output
names(raw3.1)
head(raw3.1$unit_rowest)   # estimates by estimation unit for row domains
raw3.1$rowest              # estimates for population for row domains

head(raw3.1$unit_colest)   # estimates by estimation unit for column domains
raw3.1$colest              # estimates for population for column domains


## Titles (list object) for estimate
titlelst3.1 <- area3.1$titlelst
names(titlelst3.1)
titlelst3.1


## -----------------------------------------------------------------------------
ref_estvar[, c("ESTTITLE", "ESTVAR", "ESTFILTER", "ESTUNITS")]

## -----------------------------------------------------------------------------
## Return raw data and titles
## Total net cubic-foot volume of live trees (at least 5 inches diameter), Wyoming, 2011-2013 
tree1.1 <- modGBtree(
    GBpopdat = GBpopdat,         # pop - population calculations
    landarea = "FOREST",         # est - forest land filter
    sumunits = TRUE,             # est - sum estimation units to population
    estvar = "VOLCFNET",               # est - net cubic-foot volume
    estvar.filter = "STATUSCD == 1",   # est - live trees only
    returntitle = TRUE           # out - return title information
    )


## -----------------------------------------------------------------------------
## Look at output list
names(tree1.1)

## Estimate and percent sampling error of estimate
tree1.1$est


## Raw data (list object) for estimate
raw1.1 <- tree1.1$raw      # extract raw data list object from output
names(raw1.1)
head(raw1.1$unit_totest)   # estimates by estimation unit (i.e., ESTN_UNIT)
head(raw1.1$totest)        # estimates for population (i.e., WY)


## Titles (list object) for estimate
titlelst1.1 <- tree1.1$titlelst
names(titlelst1.1)
titlelst1.1

## -----------------------------------------------------------------------------
tree1.2 <- modGBtree(
    GBpopdat = GBpopdat,         # pop - population calculations
    landarea = "FOREST",         # est - forest land filter
    sumunits = TRUE,             # est - sum estimation units to population
    estvar = "VOLCFNET",               # est - net cubic-foot volume
    estvar.filter = "STATUSCD == 1",   # est - live trees only
    rowvar = "FORTYPCD",         # est - row domain 
    returntitle = TRUE           # out - return title information
    )


## -----------------------------------------------------------------------------
## Look at output list
names(tree1.2)

## Estimate and percent sampling error of estimate
tree1.2$est

## Raw data (list object) for estimate
raw1.2 <- tree1.2$raw      # extract raw data list object from output
names(raw1.2)
head(raw1.2$unit_totest)   # estimates by estimation unit (i.e., ESTN_UNIT)
head(raw1.2$totest)        # estimates for population (i.e., WY)
head(raw1.2$unit_rowest)   # estimates by row, by estimation unit (i.e., ESTN_UNIT)
head(raw1.2$rowest)        # estimates by row for population (i.e., WY)


## Titles (list object) for estimate
titlelst1.2 <- tree1.2$titlelst
names(titlelst1.2)
titlelst1.2


## -----------------------------------------------------------------------------
## Create barplot
datBarplot(
      raw1.2$unit_rowest, 
      xvar = titlelst1.2$title.rowvar, 
      yvar = "est"
      )


## -----------------------------------------------------------------------------

## Create fancier barplot
datBarplot(
      raw1.2$unit_rowest, 
      xvar = titlelst1.2$title.rowvar, 
      yvar = "est",
      errbars = TRUE, 
      sevar = "est.se", 
      main = wraptitle(titlelst1.2$title.row, 75),
      ylabel = titlelst1.2$title.yvar, 
      divideby = "million"
      )


## -----------------------------------------------------------------------------
tree1.3 <- modGBtree(
    GBpopdat = GBpopdat,         # pop - population calculations
    landarea = "FOREST",         # est - forest land filter
    sumunits = TRUE,             # est - sum estimation units to population
    estvar = "VOLCFNET",               # est - net cubic-foot volume
    estvar.filter = "STATUSCD  == 1",   # est - live trees only
    rowvar = "FORTYPCD",         # est - row domain
    colvar = "STDSZCD",          # est - column domain
    returntitle = TRUE,          # out - return title information
    savedata = TRUE,             # out - save data to outfolder
    table_opts = table_options(
      row.FIAname = TRUE,          # est - row domain names
      col.FIAname = TRUE,          # est - column domain names
      allin1 = TRUE                # out - return output with est(pse)
    ),
    savedata_opts = savedata_options(
      outfolder = outfolder,       # out - outfolder for saving data
      outfn.pre = "WY"             # out - prefix for output files
      )
    )


## -----------------------------------------------------------------------------
## Look at output list from modGBarea()
names(tree1.3)

## Estimate and percent sampling error of estimate
tree1.3$est


## Raw data (list object) for estimate
raw1.3 <- tree1.3$raw      # extract raw data list object from output
names(raw1.3)
head(raw1.3$unit_totest)   # estimates by estimation unit (i.e., ESTN_UNIT)
head(raw1.3$totest)        # estimates for population (i.e., WY)
head(raw1.3$unit_rowest)   # estimates by row, by estimation unit (i.e., ESTN_UNIT)
head(raw1.3$rowest)        # estimates by row for population (i.e., WY)
head(raw1.3$unit_colest)   # estimates by column, by estimation unit (i.e., ESTN_UNIT)
head(raw1.3$colest)        # estimates by column for population (i.e., WY)
head(raw1.3$unit_grpest)   # estimates by row and column, by estimation unit (i.e., ESTN_UNIT)
head(raw1.3$grpest)        # estimates by row and column for population (i.e., WY)


## Titles (list object) for estimate
titlelst1.3 <- tree1.3$titlelst
names(titlelst1.3)
titlelst1.3


## List output files in outfolder
list.files(outfolder, pattern = "WY_tree")
list.files(paste0(outfolder, "/rawdata"), pattern = "WY_tree")


## -----------------------------------------------------------------------------
## Number of live trees (at least 1 inch diameter) by species
tree1.4 <- modGBtree(
    GBpopdat = GBpopdat,         # pop - population calculations
    landarea = "FOREST",         # est - forest land filter
    sumunits = TRUE,             # est - sum estimation units to population
    estvar = "TPA_UNADJ",               # est - number of trees per acre 
    estvar.filter = "STATUSCD == 1",    # est - live trees only
    rowvar = "SPCD",             # est - row domain
    returntitle = TRUE,          # out - return title information
    table_opts = table_options(    
      row.FIAname = TRUE,          # est - row domain names
      allin1 = FALSE               # out - return output with est and pse
      )
    )


## -----------------------------------------------------------------------------
## Look at output list
names(tree1.4)

## Estimate and percent sampling error of estimate
tree1.4$est

## -----------------------------------------------------------------------------
## Number of live trees by species, including seedlings
tree1.5 <- modGBtree(
    GBpopdat = GBpopdat,         # pop - population calculations
    estseed = "add",             # est - add seedling data
    landarea = "FOREST",         # est - forest land filter
    sumunits = TRUE,             # est - sum estimation units to population
    estvar = "TPA_UNADJ",               # est - number of trees per acre 
    estvar.filter = "STATUSCD == 1",    # est - live trees only
    rowvar = "SPCD",             # est - row domain
    returntitle = TRUE,          # out - return title information
    table_opts = table_options(
      row.FIAname = TRUE,          # est - row domain names
      allin1 = FALSE,              # out - return output with est and pse
      )
    )


## -----------------------------------------------------------------------------
## Look at output list
names(tree1.5)

## Estimate and percent sampling error of estimate
tree1.5$est


## Compare estimates with and without seedlings
head(tree1.4$est)
head(tree1.5$est)

## -----------------------------------------------------------------------------
## Number of live trees seedlings by species
tree1.6 <- modGBtree(
    GBpopdat = GBpopdat,         # pop - population calculations
    estseed = "only",            # est - add seedling data
    landarea = "FOREST",         # est - forest land filter
    sumunits = TRUE,             # est - sum estimation units to population
    estvar = "TPA_UNADJ",        # est - number of trees per acre 
    rowvar = "SPCD",             # est - row domain
    returntitle = TRUE,          # out - return title information
    table_opts = table_options(
      row.FIAname = TRUE,          # est - row domain names
      allin1 = FALSE               # out - return output with est and pse
      )
    )


