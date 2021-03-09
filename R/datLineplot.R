datLineplot <- function(x, xvar, yvar, plotCI=FALSE, sevar=NULL, 
	CIlst=c(68,95), CIcolorlst=c("dark grey", "black"), addshade=FALSE, 
	device.type="dev.new", jpeg.res=300, device.height=5, device.width=8, 
	ylim=NULL, divideby=NULL, ylabel=NULL, xlabel=NULL, mar=NULL, addlegend=FALSE, 
	main=NULL, cex.main=1, cex.label=1, cex.names=0.8, las.xnames=0, las.ynames=1, 
	savedata=FALSE, outfolder=NULL, outfn=NULL, outfn.pre=NULL, outfn.date=TRUE, 
	overwrite=FALSE, ...){ 
  ####################################################################################
  ## DESCRIPTION: Function to generate a barplot of frequencies ordered from most
  ##        to least.
  #####################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) x=xvar=yvar=x.order=savedata <- NULL

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows")
    Filters=rbind(Filters,csv=c("Comma-delimited files (*.csv)", "*.csv"))


  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## CHECK INPUTS
  ##############################################
  ## Check data table
  datx <- FIESTA::pcheck.table(x, caption="Table?", stopifnull=TRUE)
  datvarlst <- names(datx)

  ## Check xvar
  xvar <- FIESTA::pcheck.varchar(var2check=xvar, varnm="xvar", checklst=datvarlst, 
	stopifnull=TRUE, caption="X variable", warn="xvar not in data table") 
 
  ## Check yvar
  datvarlst <- datvarlst[datvarlst != xvar] 
  yvar <- FIESTA::pcheck.varchar(var2check=yvar, varnm="yvar", checklst=datvarlst, 
	caption="Y variable", warn="yvar not in data table", 
	stopifnull=TRUE, multiple=FALSE) 


  ## Check divideby
  ########################################################
  dividebylst <- c("hundred", "thousand", "million")
  divideby <- FIESTA::pcheck.varchar(var2check=divideby, varnm="divideby", 
		gui=gui, checklst=dividebylst, caption="Divide estimates?")


  if (!is.null(divideby)) {
    dividebynum <- ifelse(divideby == "hundred", 100, 
				ifelse(divideby == "thousand", 1000, 
					ifelse(divideby == "million", 1000000, 1)))
    datx[[yvar]] <- datx[[yvar]] / dividebynum
  }
  
  ## Check plotCI 
  plotCI <- FIESTA::pcheck.logical(plotCI, "Confidence Intervals?", "YES")

  if (plotCI) {
    ## Check CIlst
    if (!is.vector(CIlst) || !is.numeric(CIlst) || !all(CIlst %in% 1:99)) {
      stop("CIlst must be a numeric vector between 0 and 100")
    }
    CIindx <- match(CIlst, sort(CIlst))
    CIlst <- sort(CIlst)
    
    ## Check CIcolorlst
    CIcolorlst <- pcheck.colors(CIcolorlst, length(CIlst))
    CIcolorlst <- CIcolorlst[CIindx]

    ## Check addshade
    addshade <- FIESTA::pcheck.logical(addshade, "Add shading?", "YES")

    ## Check sevar
    datvarlst <- datvarlst[datvarlst != yvar] 
    sevar <- FIESTA::pcheck.varchar(var2check=sevar, varnm="sevar", 
		checklst=datvarlst, caption="SE variable", 
		warn="sevar not in data table", stopifnull=TRUE, multiple=FALSE) 
 
    if (!is.null(divideby)) {
      datx[[sevar]] <- datx[[sevar]] / dividebynum
    }

    ## Add left and right CI values for all in CIlst
    datx <- addCI(datx, estnm="EST", senm="SE", conf.level=CIlst) 

    ## Set ylim to max CIlst
    maxCI <- max(CIlst)
    maxCIleft <- paste0("CI", maxCI, "left")
    maxCIright <- paste0("CI", maxCI, "right")
    ylim <- c(min(datx[[maxCIleft]]) - min(datx[[maxCIleft]]) * .05,
 		max(datx[[maxCIright]]) + max(datx[[maxCIright]]) * .05)   
  } else {
    ylim <- c(min(datx[[yvar]]) - min(datx[[yvar]]) * .05, 
		max(datx[[yvar]]) + max(datx[[yvar]]) * .05)
  }

  ## GET addlegend 
  addlegend <- FIESTA::pcheck.logical(addlegend, "Add legend?", "NO")

  ## Check device.type
  device.typelst <- c("jpg", "pdf", "postscript", "win.metafile")

  if (length(device.type) == 0 || is.null(device.type)) 
    device.type <- "dev.new"

  device.type[device.type == "windows"] <- "dev.new"
  if (any(!device.type %in% c("dev.new", device.typelst))) {
    stop("illegal 'device.type' device types must be one or more: ",
		"of 'dev.new' 'jpg' 'pdf' or 'ps'")
  }
  
  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, "Save bar plot?", "NO")

  if (savedata) {
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite files?", first="NO", gui=gui)  
    outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="YES", gui=gui)  
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)

    if (is.null(outfn)) outfn <- paste0("BARPLOT_", yvar)

    if (!any(device.type %in% device.typelst)) {  
      message("no export device specified..  adding jpg format")
      device.type <- c(device.type, "jpg")
    }
  }

  ################################################################################  
  ### DO WORK
  ################################################################################ 
  ## Change NULL values to 0
  datx <- DT_NAto0(datx, yvar)
 

  ## SET UP MAR and TEXT PLACEMENT AND ADD TEXT
  ######################################################
  maxattnum <- 15
  xmaxnum <- max(nchar(x[[xvar]]))
  ymaxnum <- max(sapply(round(na.omit(datx[[yvar]])), nchar)) 
 
  ## las.xnames
  ######################
  if (is.null(las.xnames)) {
    if (length(datx[[xvar]]) > maxattnum || xmaxnum > 10) {
      las.xnames <- 3
    } else {
      las.xnames <- 1
    }
  }
  srt <- ifelse(las.xnames == 1, 0, ifelse(las.xnames == 3, 90, 60))

  ## ylabel
  ######################
  if (is.null(ylabel) & !is.null(divideby)) {
    ylabel <- yvar
  }
  ylabel <- paste0(ylabel, " (", divideby, "s)")
 
  if (!is.null(ylabel)) { 
    yside <- 2
    ylasnum <- 0
    ylinenum <- ymaxnum * cex.names/2 + 2
  } else {
    ylinenum <- ymaxnum * cex.names/2 + 1
  }

  ## xlabel
  ######################
  if (!is.null(xlabel)) { 
    xside <- 1			## axis position (1:xaxis; 2:yaxis)
    xlasnum <- 1			## orientation (0:horizontal; 1:vertical)
    linenum <- 2
    if (las.xnames %in% c(0,2)) {
      xlinenum <- .36 * xmaxnum + linenum 
    } else {
      xlinenum <- 4
    }
  } else {
    xlabel <- xvar
    xlinenum <- ifelse(las.xnames==0, xmaxnum/3, xmaxnum/4)
    #xlinenum = 8
  }

  ## Set mar (number of lines for margins - bottom, left, top, right)
  if (is.null(mar)) {
    mar <-  par("mar")
    mar[3] <- ifelse(!is.null(main), 3, 2)		## top mar
    mar[1] <- xlinenum * cex.names + 3.5		## bottom mar
    mar[2] <- ylinenum + 1.6				## left mar
    mar[4] <- 0.5						## right mar
  }

  ## Generate line plots
  #################################################
  for (i in 1:length(device.type)) {
    ### Output filenames ###
    #################################################
    if (device.type[i] == "dev.new") {
#      dev.new(width=device.width, height=device.height)
#      dev.new()

    } else {
      if (savedata) {
        ext <- device.type[i]
 
        OUTPUTfn <- getoutfn(outfn, outfolder=outfolder, outfn.pre=outfn.pre, 
		outfn.date=outfn.date, overwrite=overwrite, ext=ext)
 
        switch(device.type[i], 
          jpg = {jpeg(filename=OUTPUTfn, width=device.width, height=device.height, 
			res=jpeg.res, units="in")},
          ps = {postscript(file=OUTPUTfn, width=device.width, height=device.height)},
          pdf = {pdf(file=OUTPUTfn, width=device.width, height=device.height)},
          stop("invalid device.type") 
        )
      } else {
        device.type[-i] <- device.type[-i]
      }
    }

    ## GENERATE BARPLOT
    ###########################################
    op <- par(xpd=NA, cex=par("cex"), mar=mar, las=las.xnames, mgp=c(3,0.5,0))

    plot(datx[[xvar]], y=datx[[yvar]], type="b", ylim=ylim, xlab=xlabel, 
		ylab=ylabel, cex.axis=cex.names, las=las.ynames)

    if (plotCI && addshade) {
      graphics::polygon(c(datx[[xvar]], rev(datx[[xvar]])), 
			c(datx[[maxCIleft]], rev(datx[[maxCIright]])),
          col="light gray", border = NA)
      graphics::lines(x=datx[[xvar]], y=datx[[yvar]], lwd=2)
      graphics::points(x=datx[[xvar]], y=datx[[yvar]])

      graphics::lines(x=datx[[xvar]], y=datx[[maxCIleft]], lwd=2, lty="dashed",
 		col=CIcolorlst[length(CIcolorlst)])
      graphics::lines(x=datx[[xvar]], y=datx[[maxCIright]], lwd=2, lty="dashed", 
		col=CIcolorlst[length(CIcolorlst)])

      if (length(CIlst) > 1) {
        for (i in 1:length(CIlst[-length(CIlst)])) {
          CI <- CIlst[-length(CIlst)][i]
          CIcolor <- CIcolorlst[-length(CIcolorlst)][i]
          CIlwr <- paste0("CI", CI, "left")
          CIupr <- paste0("CI", CI, "right")
        
          graphics::lines(x=datx[[xvar]], y=datx[[CIlwr]], lwd=2, lty="dashed", col=CIcolor)
          graphics::lines(x=datx[[xvar]], y=datx[[CIupr]], lwd=2, lty="dashed", col=CIcolor)
        }
      }
    }
    if (addlegend) {
      legend("bottomleft", lty="dashed", col=CIcolorlst, legend=CIlst, cex=.75, bty="n")
    }
 
    if (savedata && !device.type[i] %in% c("default", "dev.new")) {

      message("###################################\n", 
			"Plot written to: ", OUTPUTfn, 
		"\n###################################")

      dev.off()
    }
  }
}