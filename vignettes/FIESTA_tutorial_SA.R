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
FIAplotsfn <- system.file("extdata", "FIApublic_M331B.db", package="FIESTA")
FIAplotsfn <- "C:/_tsf/_GitHub/FIESTA/inst/extdata/FIA_data/FIApublic_M331B.db"

WYbhfn <- system.file("extdata", "sp_data/WYbighorn_adminbnd.shp", package="FIESTA")
WYbhdistfn <- system.file("extdata", "sp_data/WYbighorn_districtbnd.shp", package="FIESTA")
WYbhdist.att <- "DISTRICTNA"

fornffn <- system.file("extdata", "sp_data/WYbighorn_forest_nonforest_250m.tif", package="FIESTA")
demfn <- system.file("extdata", "sp_data/WYbighorn_dem_250m.img", package="FIESTA")

# # Derive new layers from dem
# aspfn <- file.path(outfolder, "WYbh_asp.img")
# gdalUtils::gdaldem("aspect", input_dem=demfn, output=aspfn, of="GTiff")
# 
# slpfn <- file.path(outfolder, "WYbh_slp.img")
# gdalUtils::gdaldem("slope", input_dem=demfn, output=slpfn, of="GTiff")


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
smallbnd <- WYbhdistfn
smallbnd.domain <- "DISTRICTNA"

## -----------------------------------------------------------------------------
SApltdat <- spGetPlots(bnd = WYbhdistfn,
                     xy_datsource = "obj",
                     xy = WYplt,
                     xy.uniqueid = "CN",
                     xvar = "LON_PUBLIC",
                     yvar = "LAT_PUBLIC",
                     xy.crs = 4269, 
                     datsource = "obj",
                     istree = TRUE,
                     isseed = TRUE,
                     plot_layer = WYplt,
                     cond_layer = WYcond,
                     tree_layer = WYtree,
                     seed_layer = WYseed,
                     invyrs = 2011:2013,
                     showsteps = TRUE,
                     returnxy=TRUE,
                     savedata_opts = savedata_options(outfolder = outfolder))

## -----------------------------------------------------------------------------
str(SApltdat, max.level = 1)

## ---- results='hide'----------------------------------------------------------
rastlst.cont <- c(dem, slp, asp)
rastlst.cont.name <- c("dem", "slp", "asp")
rastlst.cat <- fornffn
rastlst.cat.name <- "fornf"

unit_layer <- WYbhdistfn
unitvar <- "DISTRICTNA"

auxdat <- spGetAuxiliary(
  xyplt = SApltdat$spxy,
  uniqueid = "CN",
  unit_layer = unit_layer,
  unitvar = "DISTRICTNA",
  rastlst.cont = rastlst.cont,
  rastlst.cont.name = rastlst.cont.name,
  rastlst.cont.stat = "mean",
  rastlst.cont.NODATA = 0,
  rastlst.cat = rastlst.cat,
  rastlst.cat.name = rastlst.cat.name,
  asptransform = TRUE,
  rast.asp = asp,
  keepNA = FALSE,
  showext = FALSE,
  savedata = FALSE
)
names(auxdat)

## -----------------------------------------------------------------------------
str(auxdat, max.level = 1)

## -----------------------------------------------------------------------------
SApopdat <- modSApop(pltdat = SApltdat, 
                     auxdat = auxdat,
                     smallbnd = WYbhdistfn,
		                 smallbnd.domain = smallbnd.domain)

## -----------------------------------------------------------------------------
str(SApopdat, max.level = 1)

## -----------------------------------------------------------------------------
all_preds <- c("slp", "dem", "asp_cos", "asp_sin", "fornf2")

## -----------------------------------------------------------------------------
area1 <- modSAarea(
  SApopdatlst = SApopdat,        # pop - population calculations for WY, post-stratification
  prednames = all_preds,         # est - charater vector of predictors to be used in the model
  SApackage = "JoSAE",           # est - character string of the R package to do the estimation
  SAmethod = "unit"              # est - method of small area estimation. Either "unit" or "area"
  )

## -----------------------------------------------------------------------------
str(area1, max.level = 1)
area1$est
area1$raw$SAmethod

## -----------------------------------------------------------------------------
str(area1$raw, max.level = 1)

## -----------------------------------------------------------------------------
area2 <- modSAarea(
  SApopdatlst = SApopdat,         # pop - population calculations for WY, post-stratification
  prednames = "slp",          # est - charater vector of predictors to be used in the model
  SApackage = "JoSAE",            # est - character string of the R package to do the estimation
  SAmethod = "area"               # est - method of small area estimation. Either "unit" or "area"
  )

## -----------------------------------------------------------------------------
area2$est

## -----------------------------------------------------------------------------
area2$multest

## -----------------------------------------------------------------------------
area3 <- modSAarea(
  SApopdatlst = SApopdat,           # pop - population calculations for WY, post-stratification
  prednames = all_preds,            # est - character vector of predictors to be used in the model
  SApackage = "hbsae",              # est - character string of the R package to do the estimation
  SAmethod = "unit"                 # est - method of small area estimation. Either "unit" or "area"
  )

## -----------------------------------------------------------------------------
area3$est
area3$raw$SAmethod
area3$raw$SApackage

## -----------------------------------------------------------------------------
area4 <- modSAarea(
  SApopdatlst = SApopdat,           # pop - population calculations for WY, post-stratification
  prednames = all_preds,            # est - charater vector of predictors to be used in the model
  SApackage = "hbsae",              # est - character string of the R package to do the estimation
  SAmethod = "unit",                # est - method of small area estimation. Either "unit" or "area"
  prior = function(x) 1             # est - prior on ratio of between and within area variation
  )

## -----------------------------------------------------------------------------
area3$est
area4$est

## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
area5 <- modSAarea(
  SApopdatlst = SApopdat,      # pop - population calculations for WY, post-stratification
  prednames = all_preds,       # est - charater vector of predictors to be used in the model
  SApackage = "JoSAE",         # est - character string of the R package to do the estimation
  SAmethod = "unit",           # est - method of small area estimation. Either "unit" or "area"
  modelselect = TRUE           # est - elastic net variable selection
  )

## -----------------------------------------------------------------------------
area5$est
area5$raw$predselect.unit

## -----------------------------------------------------------------------------
estvar <- "VOLCFNET"
live_trees <- "STATUSCD == 1"

## -----------------------------------------------------------------------------
tree1 <- modSAtree(
    SApopdatlst = SApopdat,      # pop - population calculations for WY, post-stratification
    prednames = all_preds,       # est - character vector of predictors to be used in the model
    SApackage = "JoSAE",         # est - character string of the R package to do the estimation
    SAmethod = "unit",           # est - method of small area estimation. Either "unit" or "area"  
    landarea = "FOREST",         # est - forest land filter
    estvar = estvar,             # est - net cubic-foot volume
    estvar.filter = live_trees   # est - live trees only
    )

## -----------------------------------------------------------------------------
tree1$est
tree1$multest

## -----------------------------------------------------------------------------
tree2 <- modSAtree(
    SApopdatlst = SApopdat,      # pop - population calculations for WY, post-stratification
    prednames = all_preds,       # est - charater vector of predictors to be used in the model
    SApackage = "JoSAE",         # est - character string of the R package to do the estimation
    SAmethod = "unit",           # est - method of small area estimation. Either "unit" or "area"  
    landarea = "FOREST",         # est - forest land filter
    estvar = estvar,             # est - net cubic-foot volume
    estvar.filter = live_trees,   # est - live trees only
    modelselect = TRUE
    )

## -----------------------------------------------------------------------------
tree2$raw$predselect.unit
tree2$est

## -----------------------------------------------------------------------------
tree3 <- modSAtree(
    SApopdatlst = SApopdat,      # pop - population calculations for WY, post-stratification
    prednames = all_preds,       # est - charater vector of predictors to be used in the model
    SApackage = "JoSAE",         # est - character string of the R package to do the estimation
    SAmethod = "unit",           # est - method of small area estimation. Either "unit" or "area"  
    landarea = "FOREST",         # est - forest land filter
    estvar = "BA",               # est - net cubic-foot volume
    estvar.filter = live_trees,  # est - live trees only
    returntitle = TRUE
    )

## -----------------------------------------------------------------------------
tree3$est

## -----------------------------------------------------------------------------
tree3$titlelst

## -----------------------------------------------------------------------------
tree4 <- modSAtree(
    SApopdatlst = SApopdat,      # pop - population calculations for WY, post-stratification
    prednames = "dem",       # est - charater vector of predictors to be used in the model
    SApackage = "sae",         # est - character string of the R package to do the estimation
    SAmethod = "area",           # est - method of small area estimation. Either "unit" or "area"  
    landarea = "FOREST",         # est - forest land filter
    estvar = "BA",               # est - net cubic-foot volume
    estvar.filter = live_trees,  # est - live trees only
    returntitle = TRUE
    )

## -----------------------------------------------------------------------------
tree4$est

## -----------------------------------------------------------------------------
tree5 <- modSAtree(
    SApopdatlst = SApopdat,      # pop - population calculations for WY, post-stratification
    prednames = all_preds,       # est - charater vector of predictors to be used in the model
    SApackage = "JoSAE",         # est - character string of the R package to do the estimation
    SAmethod = "unit",           # est - method of small area estimation. Either "unit" or "area"  
    landarea = "FOREST",         # est - forest land filter
    estvar = "BA",               # est - net cubic-foot volume
    estvar.filter = live_trees,  # est - live trees only
    savedata = TRUE,
    savedata_opts = savedata_options(
      outfolder = outfolder
    )
    )

## -----------------------------------------------------------------------------
tree6 <- modSAtree(
    SApopdatlst = SApopdat,      # pop - population calculations for WY, post-stratification
    prednames = all_preds,       # est - charater vector of predictors to be used in the model
    SApackage = "hbsae",         # est - character string of the R package to do the estimation
    SAmethod = "unit",           # est - method of small area estimation. Either "unit" or "area"  
    landarea = "FOREST",         # est - forest land filter
    estvar = "BA",               # est - net cubic-foot volume
    estvar.filter = live_trees,  # est - live trees only
    savedata = TRUE,
    savedata_opts = savedata_options(
      outfolder = outfolder,
      outfn.pre = "HB_unit"
    )
    )

## -----------------------------------------------------------------------------
list.files(outfolder, pattern = "HB")

## -----------------------------------------------------------------------------
tree6$est

