## ----setup, include = F-------------------------------------------------------
library(knitr)
knitr::opts_chunk$set(message = F, warning = F, eval = F)

## ---- include=FALSE-----------------------------------------------------------
#  # Sets up output folding
#  hooks = knitr::knit_hooks$get()
#  hook_foldable = function(type) {
#    force(type)
#    function(x, options) {
#      res = hooks[[type]](x, options)
#  
#      if (isFALSE(options[[paste0("fold.", type)]])) return(res)
#  
#      paste0(
#        "<details><summary>", type, "</summary>\n\n",
#        res,
#        "\n\n</details>"
#      )
#    }
#  }
#  knitr::knit_hooks$set(
#    output = hook_foldable("output"),
#    plot = hook_foldable("plot")
#  )

## ---- warning = F, message = F------------------------------------------------
#  library(FIESTA)

## -----------------------------------------------------------------------------
#  # File names for external spatial data
#  WYbhfn <- system.file("extdata",
#                        "sp_data/WYbighorn_adminbnd.shp",
#                        package = "FIESTA")
#  WYbhdistfn <- system.file("extdata",
#                            "sp_data/WYbighorn_districtbnd.shp",
#                            package = "FIESTA")
#  WYbhdist.att <- "DISTRICTNA"
#  
#  fornffn <- system.file("extdata",
#                         "sp_data/WYbighorn_forest_nonforest_250m.tif",
#                         package = "FIESTA")
#  demfn <- system.file("extdata",
#                       "sp_data/WYbighorn_dem_250m.img",
#                       package = "FIESTA")
#  
#  
#  # Other spatial layers used for examples, extracted using the raster package, getData function.
#  # County-level boundaries for USA and subset for Wyoming (Note: must have internet connection)
#  USAco <- raster::getData("GADM", country = "USA", level = 2)
#  WYco <- USAco[USAco$NAME_1 == "Wyoming",]
#  
#  head(USAco@data)

## ---- include = F-------------------------------------------------------------
#  ## Set options
#  options(scipen=6)
#  options(stringsAsFactors=FALSE)
#  outfolder <- tempdir()

## -----------------------------------------------------------------------------
#  ## Import external data shapefiles
#  WYbh <- spImportSpatial(WYbhfn)
#  WYbhdist <- spImportSpatial(WYbhdistfn)
#  
#  ## Display boundary
#  plot(sf::st_geometry(WYbhdist), border="blue")
#  plot(sf::st_geometry(WYbh), add=TRUE)
#  

## -----------------------------------------------------------------------------
#  ## Export Spatial Polygons layer to a shapefile
#  spExportSpatial(WYbh,
#                  savedata_opts = list(out_dsn = "WYbh.shp",
#                                       outfolder = outfolder,
#                                       overwrite_dsn = TRUE)
#                  )
#  

## -----------------------------------------------------------------------------
#  head(WYplt)
#  
#  WYspplt <- spMakeSpatialPoints(xyplt = WYplt,
#                                 xy.uniqueid = "CN",
#                                 xvar = "LON_PUBLIC",
#                                 yvar = "LAT_PUBLIC",
#                                 prj = "longlat",
#                                 datum = "NAD83")
#  head(WYspplt)
#  

## -----------------------------------------------------------------------------
#  WYspplt <- spMakeSpatialPoints(xyplt = WYplt,
#                                 xy.uniqueid = "CN",
#                                 xvar = "LON_PUBLIC",
#                                 yvar = "LAT_PUBLIC",
#                                 xy.crs = 4269
#                                 )
#  WYspplt
#  

## -----------------------------------------------------------------------------
#  ## Display output
#  plot(sf::st_geometry(WYbhdist))
#  plot(sf::st_geometry(WYspplt), add=TRUE)
#  
#  ## NOTE: To display multiple layers, all layers must be in the same coordinate system.
#  lapply(list(WYbh, WYbhdist, WYspplt), sf::st_crs)

## -----------------------------------------------------------------------------
#  WYspplt <- spMakeSpatialPoints(xyplt = WYplt,
#                                 xy.uniqueid = "CN",
#                                 xvar = "LON_PUBLIC",
#                                 yvar = "LAT_PUBLIC",
#                                 xy.crs = 4269,
#                                 exportsp = TRUE,
#                                 savedata_opts = list(
#                                      out_dsn = "spplt",
#                                      out_fmt = "shp",
#                                      outfolder = outfolder,
#                                      out_layer = "WYplots",
#                                      overwrite_layer = TRUE)
#                                 )
#  

## -----------------------------------------------------------------------------
#  sf::st_crs(WYspplt)
#  prj <- "+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#  WYspplt.utm12 <- spReprojectVector(layer = WYspplt,
#                                     crs.new = prj)
#  
#  ## Check results
#  sf::st_crs(WYspplt.utm12)

## -----------------------------------------------------------------------------
#  ## Get points within Bighorn National Forest boundary (project on the fly)
#  
#  WYbhptslst <- spClipPoint(xyplt = WYplt,
#                            uniqueid = "CN",
#                            clippolyv = WYbh,
#                            spMakeSpatial_opts=list(xvar = "LON_PUBLIC",
#                                                    yvar = "LAT_PUBLIC",
#                                                    prj = "longlat",
#                                                    datum = "NAD83")
#                            )
#  
#  WYbhptslst <- spClipPoint(xyplt = WYplt,
#                            uniqueid = "CN",
#                            clippolyv = WYbh,
#                            savedata = TRUE,
#                            exportsp = TRUE,
#                            spMakeSpatial_opts=list(xvar = "LON_PUBLIC",
#                                                    yvar = "LAT_PUBLIC",
#                                                    prj = "longlat",
#                                                    datum = "NAD83"),
#                            savedata_opts = list(outfolder=outfolder,
#                                                 out_layer = "WYbh")
#                            )
#  
#  names(WYbhptslst)
#  WYbhspplt <- WYbhptslst$clip_xyplt
#  WYbhprj <- WYbhptslst$clip_polyv
#  

