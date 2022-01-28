## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)

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
# File names for external spatial data
WYbhfn <- system.file("extdata", "sp_data/WYbighorn_adminbnd.shp",
                      package = "FIESTA")
WYbhdistfn <- system.file("extdata", "sp_data/WYbighorn_districtbnd.shp",
                          package = "FIESTA")

## predictor variables
fornffn <- system.file("extdata", "sp_data/WYbighorn_forest_nonforest_250m.tif",
                       package = "FIESTA")
demfn <- system.file("extdata", "sp_data/WYbighorn_dem_250m.img",
                     package = "FIESTA")

# Derive new predictor layers from dem
library(raster)
dem <- raster(demfn)
slp <- terrain(dem,
               opt = "slope",
               unit = "degrees",
               filename = paste0(outfolder, "/WYbh_slp.img"), 
               overwrite = TRUE)
asp <- terrain(dem,
               opt = "aspect",
               unit = "degrees", 
               filename = paste0(outfolder, "/WYbh_asp.img"),
               overwrite = TRUE)

## -----------------------------------------------------------------------------
WYspplt <- spMakeSpatialPoints(
  xyplt = WYplt,
  xy.uniqueid = "CN",
  xvar = "LON_PUBLIC",
  yvar = "LAT_PUBLIC",
  prj = "longlat",
  datum = "NAD83"
)

rastlst.cont <- c(demfn, slp, asp)
rastlst.cont.name <- c("dem", "slp", "asp")
rastlst.cat <- fornffn
rastlst.cat.name="fornf"

## ---- results='hide'----------------------------------------------------------
modeldat <- spGetAuxiliary(
  xyplt = WYspplt,
  uniqueid = "CN",
  unit_layer = WYbhfn,
  unitvar = NULL,
  rastlst.cont = rastlst.cont,
  rastlst.cont.name = rastlst.cont.name,
  rastlst.cat = rastlst.cat,
  rastlst.cat.name = rastlst.cat.name,
  rastlst.cont.stat = "mean",
  asptransform = TRUE,
  rast.asp = asp,
  keepNA = FALSE,
  showext = FALSE,
  savedata = FALSE)

## -----------------------------------------------------------------------------
str(modeldat, max.level = 1)

## -----------------------------------------------------------------------------
MApopdat <- modMApop(popTabs = list(tree = WYtree, cond = WYcond),
                     pltassgn = WYpltassgn,
                     auxdat = modeldat)

## -----------------------------------------------------------------------------
str(MApopdat, max.level = 1)

## -----------------------------------------------------------------------------
area1 <- modMAarea(
  MApopdat = MApopdat, # pop - population calculations for WY, post-stratification
  MAmethod = "greg", # est - model-assisted method
  landarea = "FOREST", # est - forest land filter
  )


## -----------------------------------------------------------------------------
str(area1, max.level = 2)

area1$est

## -----------------------------------------------------------------------------
area2 <- modMAarea(
  MApopdat = MApopdat, # pop - population calculations for WY, post-stratification
  MAmethod = "gregEN", # est - model-assisted method
  landarea = "FOREST", # est - forest land filter
  )


## -----------------------------------------------------------------------------
str(area2, max.level = 2)

## -----------------------------------------------------------------------------
area2$raw$predselectlst$totest

## -----------------------------------------------------------------------------
area3 <- modMAarea(
    MApopdat = MApopdat,         # pop - population calculations for WY, post-stratification
    MAmethod = "greg",           # est - model-assisted method
    landarea = "FOREST",         # est - forest land filter
    rowvar = "FORTYPCD",         # est - row domain
    returntitle = TRUE           # out - return title information
    )

## -----------------------------------------------------------------------------
str(area3, max.level = 1)

## -----------------------------------------------------------------------------
## Estimate and percent sampling error of estimate
area3$est

## -----------------------------------------------------------------------------
## Raw data (list object) for estimate
raw3 <- area3$raw      # extract raw data list object from output
names(raw3)
head(raw3$unit_totest) # estimates by estimation unit (i.e., ESTN_UNIT)
raw3$totest            # estimates for population (i.e., WY)
head(raw3$unit_rowest) # estimates by row, by estimation unit (i.e., ESTN_UNIT)
head(raw3$rowest)      # estimates by row for population (i.e., WY)


## Titles (list object) for estimate
titlelst3 <- area3$titlelst
names(titlelst3)
titlelst3

## -----------------------------------------------------------------------------
## Area of forest land by forest type and stand-size class, Wyoming, 2011-2013
area4 <- modMAarea(
    MApopdat = MApopdat,         # pop - population calculations for WY, post-stratification
    MAmethod = "greg",           # est - model-assisted method
    landarea = "FOREST",         # est - forest land filter
    rowvar = "FORTYPCD",         # est - row domain
    colvar = "STDSZCD",          # est - column domain
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

area4$est

## -----------------------------------------------------------------------------
## Look at output list
names(area4)

## Estimate and percent sampling error of estimate
head(area4$est)


## Raw data (list object) for estimate
raw4 <- area4$raw      # extract raw data list object from output
names(raw4)
head(raw4$unit_totest) # estimates by estimation unit (i.e., ESTN_UNIT)
head(raw4$totest)      # estimates for population (i.e., WY)
head(raw4$unit_rowest) # estimates by row, by estimation unit (i.e., ESTN_UNIT)
head(raw4$rowest)      # estimates by row for population (i.e., WY)
head(raw4$unit_colest) # estimates by column, by estimation unit (i.e., ESTN_UNIT)
head(raw4$colest)      # estimates by column for population (i.e., WY)
head(raw4$unit_grpest) # estimates by row and column, by estimation unit (i.e., ESTN_UNIT)
head(raw4$grpest)      # estimates by row and column for population (i.e., WY)


## Titles (list object) for estimate
titlelst4 <- area4$titlelst
names(titlelst4)
titlelst4


## List output files in outfolder
list.files(outfolder, pattern = "WY_area")
list.files(paste0(outfolder, "/rawdata"), pattern = "WY_area")

## -----------------------------------------------------------------------------
estvar <- "VOLCFNET"
live_trees <- "STATUSCD == 1"

## -----------------------------------------------------------------------------
## Return raw data and titles
## Total net cubic-foot volume of live trees (at least 5 inches diameter), Wyoming, 2011-2013 
tree1 <- modMAtree(
    MApopdat = MApopdat,         # pop - population calculations
    MAmethod = "greg",           # est - model-assisted method
    landarea = "FOREST",         # est - forest land filter
    estvar = estvar,             # est - net cubic-foot volume
    estvar.filter = live_trees,  # est - live trees only
    returntitle = TRUE           # out - return title information
    )

names(tree1)
tree1$raw$unit_totest

## -----------------------------------------------------------------------------
## Return raw data and titles
## Total net cubic-foot volume of live trees (at least 5 inches diameter), Wyoming, 2011-2013 
tree2 <- modMAtree(
    MApopdat = MApopdat,         # pop - population calculations
    MAmethod = "gregEN",         # est - model-assisted method
    landarea = "FOREST",         # est - forest land filter
    estvar = estvar,             # est - net cubic-foot volume
    estvar.filter = live_trees,  # est - live trees only
    returntitle = TRUE           # out - return title information
    )

## -----------------------------------------------------------------------------
str(tree2, max.level = 2)

## -----------------------------------------------------------------------------
tree2$raw$predselectlst

## -----------------------------------------------------------------------------
tree3 <- modMAtree(
    MApopdat = MApopdat,         # pop - population calculations
    MAmethod = "greg",           # est - model-assisted method
    landarea = "FOREST",         # est - forest land filter
    estvar = "VOLCFNET",               # est - net cubic-foot volume
    estvar.filter = "STATUSCD == 1",   # est - live trees only
    rowvar = "FORTYPCD",         # est - row domain 
    returntitle = TRUE           # out - return title information
    )


## -----------------------------------------------------------------------------
## Look at output list
names(tree3)

## Estimate and percent sampling error of estimate
tree3$est

## Raw data (list object) for estimate
raw3 <- tree3$raw      # extract raw data list object from output
names(raw3)
head(raw3$unit_totest)   # estimates by estimation unit (i.e., ESTN_UNIT)
head(raw3$totest)        # estimates for population (i.e., WY)
head(raw3$unit_rowest)   # estimates by row, by estimation unit (i.e., ESTN_UNIT)
head(raw3$rowest)        # estimates by row for population (i.e., WY)


## Titles (list object) for estimate
titlelst3 <- tree3$titlelst
names(titlelst3)
titlelst3

## -----------------------------------------------------------------------------
## Create barplot
datBarplot(
      raw3$unit_rowest, 
      xvar = titlelst3$title.rowvar, 
      yvar = "est"
      )

## -----------------------------------------------------------------------------
## Create fancier barplot
datBarplot(
      raw3$unit_rowest, 
      xvar = titlelst3$title.rowvar, 
      yvar = "est",
      errbars = TRUE, 
      sevar = "est.se", 
      main = wraptitle(titlelst3$title.row, 75),
      ylabel = titlelst3$title.yvar, 
      divideby = "million"
      )

## -----------------------------------------------------------------------------
tree4 <- modMAtree(
    MApopdat = MApopdat,         # pop - population calculations
    MAmethod = "greg",           # est - model-assisted method
    landarea = "FOREST",         # est - forest land filter
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
names(tree4)

## Estimate and percent sampling error of estimate
tree4$est


## Raw data (list object) for estimate
raw4 <- tree4$raw      # extract raw data list object from output
names(raw4)
head(raw4$unit_totest)   # estimates by estimation unit (i.e., ESTN_UNIT)
head(raw4$totest)        # estimates for population (i.e., WY)
head(raw4$unit_rowest)   # estimates by row, by estimation unit (i.e., ESTN_UNIT)
head(raw4$rowest)        # estimates by row for population (i.e., WY)
head(raw4$unit_colest)   # estimates by column, by estimation unit (i.e., ESTN_UNIT)
head(raw4$colest)        # estimates by column for population (i.e., WY)
head(raw4$unit_grpest)   # estimates by row and column, by estimation unit (i.e., ESTN_UNIT)
head(raw4$grpest)        # estimates by row and column for population (i.e., WY)


## Titles (list object) for estimate
titlelst4 <- tree4$titlelst
names(titlelst4)
titlelst4


## List output files in outfolder
list.files(outfolder, pattern = "WY_tree")
list.files(paste0(outfolder, "/rawdata"), pattern = "WY_tree")


## -----------------------------------------------------------------------------
## Number of live trees (at least 1 inch diameter) by species
tree5 <- modMAtree(
    MApopdat = MApopdat,         # pop - population calculations
    MAmethod = "greg",           # est - model-assisted method
    landarea = "FOREST",         # est - forest land filter
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
names(tree5)

## Estimate and percent sampling error of estimate
tree5$est

## -----------------------------------------------------------------------------
MApopdat_seed <- modMApop(popTabs = list(tree = WYtree,
                                         cond = WYcond,
                                         seed = WYseed),
                     pltassgn = WYpltassgn,
                     auxdat = modeldat)

## -----------------------------------------------------------------------------
## Number of live trees by species, including seedlings
tree6 <- modMAtree(
    MApopdat = MApopdat_seed,         # pop - population calculations
    MAmethod = "greg",           # est - model-assisted method
    estseed = "add",             # est - add seedling data
    landarea = "FOREST",         # est - forest land filter
    estvar = "TPA_UNADJ",               # est - number of trees per acre 
    estvar.filter = "STATUSCD == 1",    # est - live trees only
    rowvar = "SPCD",             # est - row domain
    returntitle = TRUE,          # out - return title information
    table_opts = table_options(
      row.FIAname = TRUE,          # est - row domain names
      allin1 = FALSE)              # out - return output with est and pse
    )

## -----------------------------------------------------------------------------
## Look at output list
names(tree6)

## Estimate and percent sampling error of estimate
tree6$est


## Compare estimates with and without seedlings
head(tree5$est)
head(tree6$est)

