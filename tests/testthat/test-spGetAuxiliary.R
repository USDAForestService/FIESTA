test_that("Get Auxiliary creates consistent output", {

  skip_on_cran()

  skip_if(gdalraster::proj_version()$major < 9, "gdalraster proj version out of date")

  # Set Up Data for spGetAuxiliary
  outfolder <- tempdir()

  WYbhfn <- system.file("extdata",
                      "sp_data/WYbighorn_adminbnd.shp",
                      package = "FIESTA")
  fornffn <- system.file("extdata",
                       "sp_data/WYbighorn_forest_nonforest_250m.tif",
                       package = "FIESTA")
  demfn <- system.file("extdata",
                     "sp_data/WYbighorn_dem_250m.img",
                     package = "FIESTA")

  library(terra)
  dem <- rast(demfn)

  slpfn <- paste0(outfolder, "/WYbh_slp.img")
  slp <- terra::terrain(dem,
                        v = "slope",
                        unit = "degrees",
                        filename = slpfn, 
                        overwrite = TRUE,
                        NAflag = -99999.0)
  aspfn <- paste0(outfolder, "/WYbh_asp.img")
  asp <- terra::terrain(dem,
                        v = "aspect",
                        unit = "degrees", 
                        filename = aspfn,
                        overwrite = TRUE,
                        NAflag = -99999.0)


  # Get Wyoming Plots
  WYspplt <- spMakeSpatialPoints(xyplt = WYplt,
                                 xy.uniqueid = "CN",
                                 xvar = "LON_PUBLIC",
                                 yvar = "LAT_PUBLIC",
                                 xy.crs = 4269)

  # Run sp Get Auxiliary
  rastlst.cont <- c(demfn, slp, asp)
  rastlst.cont.name <- c("dem", "slp", "asp")
  rastlst.cat <- fornffn
  rastlst.cat.name <- "fornf"

  modeldat <- suppressWarnings(spGetAuxiliary(xyplt = WYspplt,
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
                                              savedata = FALSE))

  pltassgn <- modeldat$pltassgn
  unitzonal <- modeldat$unitzonal
  unitarea <- modeldat$unitarea

  expect_snapshot(unitzonal)
  expect_snapshot(unitarea)
  expect_snapshot(pltassgn)
})