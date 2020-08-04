anSAdata <- function(SAdoms, smallbnd=NULL, RS=NULL, 
	clipxy=TRUE, datsource="sqlite", data_dsn=NULL, istree=TRUE, 
	plot_layer="plot", cond_layer="cond", tree_layer="tree", 
	puniqueid="CN", measCur=TRUE, measEndyr=NULL, measEndyr.filter=NULL,
	intensity1=FALSE, rastlst.cont=NULL, rastlst.cont.name=NULL, 
	rastlst.cat=NULL, rastlst.cat.name=NULL, rastlst.cat.NODATA=NULL, 
	vars2keep="AOI", showsteps=FALSE, savedata=FALSE, savexy=FALSE, 
	savesteps=FALSE, saveobj=FALSE, outfolder=NULL, out_fmt = "csv", 
	out_dsn = NULL, outfn.pre=NULL, outfn.date=FALSE, overwrite=TRUE, 
	SApltdat=NULL, ...) {


  ## Set global variables
  gui <- FALSE
  ref_titles <- FIESTA::ref_titles
  plt=AOI <- NULL



  ####################################################################
  ## Get FIA plot data from SQLite within boundary
  ####################################################################
  if (is.null(SApltdat)) {
    if (!is.null(measEndyr.filter)) {
      SApltdat <- spGetPlots(bnd=SAdoms, bnd.filter=measEndyr.filter, 
		RS=RS, statebnd.att="COUNTYFIPS", clipxy=clipxy, datsource=datsource, 
		data_dsn=data_dsn, istree=istree, plot_layer=plot_layer, 
		cond_layer=cond_layer, tree_layer=tree_layer, measCur=TRUE,
		measEndyr=measEndyr, intensity1=intensity1, savedata=FALSE, ...)
      xyplt <- SApltdat$clip_xyplt
      xy.uniqueid <- SApltdat$xy.uniqueid
      bnd <- SApltdat$clip_poly
      puniqueid <- SApltdat$puniqueid
      pjoinid <- SApltdat$pjoinid
      plt <- SApltdat$clip_tabs$clip_plt
      cond <- SApltdat$clip_tabs$clip_cond
      tree <- SApltdat$clip_tabs$clip_tree

      SApltdat <- spGetPlots(bnd=SAdoms, bnd.filter=paste0("!", measEndyr.filter), 
		RS=RS, clipxy=clipxy, datsource=datsource, 
		data_dsn=data_dsn, istree=istree, plot_layer=plot_layer, 
		cond_layer=cond_layer, tree_layer=tree_layer, measCur=TRUE,
		intensity1=intensity1, savedata=FALSE, ...)
      xyplt2 <- SApltdat$clip_xyplt
      plt2 <- SApltdat$clip_tabs$clip_plt
      cond2 <- SApltdat$clip_tabs$clip_cond
      tree2 <- SApltdat$clip_tabs$clip_tree

      xyplt <- rbind(xyplt, xyplt2)
      plt <- rbindlist(list(plt, plt2))
      cond <- rbindlist(list(cond, cond2))
      tree <- rbindlist(list(tree, tree2))

      rm(xyplt2)
      rm(plt2)
      rm(tree2)
      gc()

    } else {

      SApltdat <- spGetPlots(bnd=SAdoms, RS=RS, clipxy=clipxy, datsource=datsource, 
		data_dsn=data_dsn, istree=istree, plot_layer=plot_layer, 
		cond_layer=cond_layer, tree_layer=tree_layer, measCur=measCur,
		measEndyr=measEndyr, intensity1=intensity1, savedata=FALSE, ...)
      xyplt <- SApltdat$clip_xyplt
      xy.uniqueid <- SApltdat$xy.uniqueid
      puniqueid <- SApltdat$puniqueid
      pjoinid <- SApltdat$pjoinid
      plt <- SApltdat$clip_tabs$clip_plt
      cond <- SApltdat$clip_tabs$clip_cond
      tree <- SApltdat$clip_tabs$clip_tree
    }
    if (is.null(SApltdat)) return(NULL)

    if (saveobj) {
      message("saving SApltdat object to: ", file.path(outfolder, "SApltdat.rda"), "...")
      save(SApltdat, file=file.path(outfolder, "SApltdat.rda"))
    }
  } else {

    xyplt <- SApltdat$clip_xyplt
    xy.uniqueid <- SApltdat$xy.uniqueid
    puniqueid <- SApltdat$puniqueid
    pjoinid <- SApltdat$pjoinid
    plt <- SApltdat$clip_tabs$clip_plt
    cond <- SApltdat$clip_tabs$clip_cond
    tree <- SApltdat$clip_tabs$clip_tree
  }


  if (showsteps) {
    ## Set plotting margins
    mar <-  par("mar")
    par(mar=c(1,1,1,1))

    plot(sf::st_geometry(SAdoms), border="grey")
    #plot(sf::st_geometry(bnd), add=TRUE)
    plot(sf::st_geometry(xyplt), add=TRUE, col="blue", cex=.25)
    if (!is.null(smallbnd)) 
      plot(sf::st_geometry(smallbnd), add=TRUE, border="red")

    par(mar=mar)
  }

  if (savesteps) {
    ## Set plotting margins
    mar <-  par("mar")
    par(mar=c(1,1,1,1))

    out_layer <- paste0("SAdoms_plots")
    jpgfn <- paste0(outfolder, "/", out_layer, ".jpg")
    jpeg(jpgfn, res=400, units="in", width=8, height=10)
    plot(sf::st_geometry(SAdoms), border="grey")
    #plot(sf::st_geometry(bnd), add=TRUE)
    plot(sf::st_geometry(xyplt), add=TRUE, col="blue", cex=.25)
    if (!is.null(smallbnd)) 
      plot(sf::st_geometry(smallbnd), add=TRUE, border="red")
    dev.off()
    message("Writing jpg to ", jpgfn, "\n")
    par(mar=mar)
  }

  ## Check number of plots (Note: must be greater than 2 plots)
  extpoly <- spExtractPoly(xyplt=xyplt, polyvlst=SAdoms, 
		uniqueid=xy.uniqueid, polyvarlst=unique(c("DOMAIN", "AOI")), 
		keepNA=FALSE)
  test <- data.table(st_drop_geometry((extpoly$sppltext)))
  test <- test[AOI == 1, .N, by="DOMAIN"]

  message("checking number of plots...")
  print(test)

  if (all(test$N <= 2)) {
    message("ALL AOIs have 2 plots or less... no estimates generated")
    return(NULL)
  }

  ####################################################################
  ## Get model data
  ####################################################################
  message("summarizing auxiliary model data...")
  SAmodeldat <- spGetModeldat(xyplt=xyplt, uniqueid=pjoinid, 
		module="SA", dom_layer=SAdoms, 
	  	rastlst.cont=rastlst.cont, rastlst.cont.name=rastlst.cont.name, 
		rastlst.cat=rastlst.cat, rastlst.cat.name=rastlst.cat.name, 
		rastlst.cat.NODATA=NULL, vars2keep="AOI", savedata=FALSE)
  pltassgn <- SAmodeldat$pltassgn
  dunitlut <- SAmodeldat$domzonal
  dunitvar <- SAmodeldat$domvar
  prednames <- SAmodeldat$prednames
  zonalnames <- SAmodeldat$zonalnames
  predfac <- SAmodeldat$predfac
  dunitarea <- SAmodeldat$domarea
  areavar <- SAmodeldat$areavar
  pltassgnid <- SAmodeldat$pltassgnid


  ##########################################
  ## Create output list
  ##########################################
  SAdata <- list(SAdoms=SAdoms, plt=plt, pltassgn=pltassgn, cond=cond, tree=tree, 
			dunitarea=dunitarea, dunitvar=dunitvar, areavar=areavar, 
			dunitlut=dunitlut, prednames=prednames, predfac=predfac,
			zonalnames=zonalnames, puniqueid=puniqueid, pjoinid=pjoinid, 
			pltassgnid=pltassgnid)
  if (savexy) {
    SAdata$xyplt <- xyplt
    SAdata$xy.uniqueid <- xy.uniqueid
  }

  if (saveobj) {
    objfn <- getoutfn(outfn="SApopdat.rda", outfolder=outfolder, 
		overwrite=overwrite, outfn.date=TRUE)
    save(SAdata, file=objfn)
    message("saving object to: ", objfn)
  } 

  if (savedata) {
   if (!is.null(RS))
     datExportData(sf::st_drop_geometry(SAdoms), outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="SAdoms", 
		outfn.date=outfn.date, overwrite_layer=overwrite)

    if (savexy)
      datExportData(sf::st_drop_geometry(xyplt), outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="xyplt", 
		outfn.date=outfn.date, overwrite_layer=overwrite)

    datExportData(pltassgn, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltassgn", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    datExportData(plt, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="plt", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    datExportData(cond, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="cond", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    datExportData(tree, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="tree", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    datExportData(dunitarea, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="dunitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    datExportData(dunitlut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="dunitlut", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
  }
   	
  return(SAdata)
}
