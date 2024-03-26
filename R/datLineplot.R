#' Data - Generates line graph.
#' 
#' Generate a line plot of multiple estimates.
#' 
#' If parameters = NULL, then it will prompt user for input.
#' 
#' @param x Data frame or comma-delimited file (*.csv) - a frequency table.
#' @param xvar String. Name of X variable.
#' @param yvar String. Name of the y variable (e.g., FREQ).
#' @param plotCI Logical. If TRUE, adds confidence intervals to plot as dotted
#' lines.
#' @param sevar String. Name of the variable with standard error values.
#' @param CIlst String. Numeric vector. If plotCI=TRUE, identifies percent
#' confidence interval to add to plot.
#' @param CIcolorlst String. Character vector. If plotCI=TRUE, identifies
#' colors to plot confidence interval lines. Must be same length as CIlst and
#' from colors() list.
#' @param addshade Logical. If TRUE, adds a light grey shading between the
#' large confidence interval lines.
#' @param device.type String. Type(s) of device for plotting ("dev.new", "jpg",
#' "pdf").
#' @param jpeg.res Integer. Resolution for jpeg image.
#' @param device.height Integer. Height (in inches) of barplot, if writing to
#' file.
#' @param device.width Integer. Width (in inches) of barplot, if writing to
#' file.
#' @param ylim Number. A vector of min and max values, c(min,max) for the y
#' axis (or x axis if horiz=TRUE). If NULL, defaults to maximum y value. If
#' errbars=TRUE, the ylim defaults to the maximum y value plus the standard
#' error.
#' @param divideby String. Conversion number for output ('hundred', 'thousand',
#' 'million').
#' @param ylabel String. Label for the y axis (same as ylab).
#' @param xlabel String. Label for the x axis (same as xlab).
#' @param xticks Numeric vector. Vector of tick marks for x axis.
#' @param mar See par.. A numerical vector representing number of lines for
#' margins (c(bottom, left, top, right).
#' @param addlegend Logical. If TRUE, adds legend to bar plot (only applicable
#' if grouping).
#' @param main String. Title for plot.
#' @param cex.main Number. Expansion factor for title.
#' @param cex.label Number. A number representing cex in barplot (size
#' expansion of x and/or ylabels.
#' @param cex.names Number. Expansion factor for axis names (bar labels) (e.g.,
#' 0.5 represents half the size.
#' @param las.xnames Number. The direction of x variable names (0,1,3).
#' 0:diagonal (Default), 1:horizontal; 3:vertical.
#' @param las.ynames Number. The direction of y variable names (0,1,3).
#' 0:diagonal (Default), 1:horizontal; 3:vertical.
#' @param savedata Logical. If TRUE, writes output data to outfolder (jpg and
#' pdf).
#' @param outfolder String. The name of the output folder. If savedata=TRUE,
#' all output saved to the outfolder. If savedata=FALSE, only a text file of
#' input parameters is saved.
#' @param outfn String. The name of the output file if savedata=TRUE (*.csv).
#' Do not include extension. If NULL, the file will be named
#' BARPLOT_'yvar_date'.csv
#' @param outfn.pre String. Add a prefix to output name (e.g., "01").
#' @param outfn.date Logical. If TRUE, add date to end of outfile (e.g.,
#' outfn_'date'.csv).
#' @param overwrite Logical. If TRUE and exportshp=TRUE, overwrite files in
#' outfolder.
#' @param ...  additional arguments to pass to barplot(), including a list of
#' arguments for legend() arguments (e.g., args.legend=list(x="topleft",
#' "bty="n"), for moving legend to topleft and removing box around legend).
#'
#' @return Outputs barplot to display window.
#' @note If savedata = TRUE, writes a jpg and pdf of barplot to outfolder.
#' 
#' To add legend parameters, add a parameter named args.legend, defined as a
#' list of specific legend parameters (see ?legend)...  ex. ..,
#' args.legend=list(x="topright"). If specifying x and y, x defines the lower
#' right corner of legend box and y defines the upper right corner of box.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' # Lineplot of cubic foot volume by above-ground biomass, Wyoming tree data
#' # datLineplot(x = WYtree, xvar = "VOLCFNET", yvar = "DRYBIO_AG") # needs work
#' @export datLineplot
datLineplot <- function(x,
                        xvar,
                        yvar, 
                        plotCI=FALSE,
                        sevar=NULL, 
                        CIlst=c(68,95),
                        CIcolorlst=c("dark grey", "black"), 
                        addshade=FALSE, 
                        device.type="dev.new",
                        jpeg.res=300, 
                        device.height=5,
                        device.width=8, 
                        ylim=NULL,
                        divideby=NULL, 
                        ylabel=NULL,
                        xlabel=NULL,
                        xticks=NULL, 
                        mar=NULL,
                        addlegend=FALSE,
                        main=NULL,
                        cex.main=1,
                        cex.label=1, 
                        cex.names=0.9,
                        las.xnames=0,
                        las.ynames=1,
                        savedata=FALSE, 
                        outfolder=NULL,
                        outfn=NULL,
                        outfn.pre=NULL,
                        outfn.date=TRUE, 
                        overwrite=FALSE,
                        ...){ 
  ####################################################################################
  ## DESCRIPTION: Function to generate a barplot of frequencies ordered from most
  ##        to least.
  #####################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) x <- xvar <- yvar <- x.order <- savedata <- NULL

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") {
    Filters <- rbind(Filters,csv=c("Comma-delimited files (*.csv)", "*.csv"))
  }

  ## Set global variables
  xlim <- NULL

  ## Set par 
  tempmar <-  graphics::par("mar")
  xpd <-  graphics::par("xpd")
  on.exit(graphics::par(mar=tempmar, xpd=xpd))
  
  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## CHECK INPUTS
  ##############################################
  ## Check data table
  datx <- pcheck.table(x, caption="Table?", stopifnull=TRUE)
  datvarlst <- names(datx)

  ## Check xvar
  xvar <- pcheck.varchar(var2check=xvar, varnm="xvar", checklst=datvarlst, 
                         stopifnull=TRUE, caption="X variable", warn="xvar not in data table") 
 
  ## Check yvar
  datvarlst <- datvarlst[datvarlst != xvar] 
  yvar <- pcheck.varchar(var2check=yvar, varnm="yvar", checklst=datvarlst, 
                         caption="Y variable", warn="yvar not in data table", 
                         stopifnull=TRUE, multiple=FALSE) 


  ## Check divideby
  ########################################################
  dividebylst <- c("hundred", "thousand", "million")
  divideby <- pcheck.varchar(var2check=divideby, varnm="divideby", gui=gui,
                             checklst=dividebylst, caption="Divide estimates?")


  if (!is.null(divideby)) {
    dividebynum <- switch(divideby,
                          "hundred" = 100,
                          "thousand" = 1000,
                          "million" = 1000000,
                          1)
    datx[[yvar]] <- datx[[yvar]] / dividebynum
  }
  
  ## Check plotCI 
  plotCI <- pcheck.logical(plotCI, "Confidence Intervals?", "YES")

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
    addshade <- pcheck.logical(addshade, "Add shading?", "YES")

    ## Check sevar
    datvarlst <- datvarlst[datvarlst != yvar] 
    sevar <- pcheck.varchar(var2check=sevar, varnm="sevar", checklst=datvarlst,
                            caption="SE variable", warn="sevar not in data table",
                            stopifnull=TRUE, multiple=FALSE) 
 
    if (!is.null(divideby)) {
      datx[[sevar]] <- datx[[sevar]] / dividebynum
    }

    ## Add left and right CI values for all in CIlst
    datx <- addCI(datx, estnm="EST", senm="SE", conf.level=CIlst) 

    if (is.null(ylim)) {
      ## Set ylim to max CIlst
      maxCI <- max(CIlst)
      maxCIleft <- paste0("CI", maxCI, "left")
      maxCIright <- paste0("CI", maxCI, "right")

      add.ylim <- mean(datx[[yvar]]) * .15
      ylim.min <- min(datx[,maxCIleft, with=FALSE]) - add.ylim
      ylim.max <- max(datx[,maxCIright, with=FALSE]) + add.ylim 
      ylim <- c(ylim.min, ylim.max)
    }
   
  } else {
    if (is.null(ylim)) {
      add.ylim <- mean(datx[[yvar]]) * .15
      ylim.min <- min(datx[,yvar, with=FALSE]) - add.ylim
      ylim.max <- max(datx[,yvar, with=FALSE]) + add.ylim 
      ylim <- c(ylim.min, ylim.max)
    }
  }

  ## Set tick marks
  ylim.min <- ylim[1]		## bug fixed by Liz
  ylim.max <- ylim[2]		## bug fixed by Liz

  if (is.null(xticks)) {
    xticks <- datx[[xvar]]
  }
  xlim <- c(min(xticks), max(xticks))

  ## GET addlegend 
  addlegend <- pcheck.logical(addlegend, "Add legend?", "NO")

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
  savedata <- pcheck.logical(savedata, "Save bar plot?", "NO")

  if (savedata) {
    overwrite <- pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite files?", first="NO", gui=gui)  
    outfn.date <- pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="YES", gui=gui)  
    outfolder <- pcheck.outfolder(outfolder, gui)

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
    ylabel <- paste0(divideby, "s")
  } else if (!is.null(ylabel) && !is.null(divideby)) {
    ylabel <- paste0(ylabel, " (", divideby, "s)")
  }
 
  if (!is.null(ylabel)) { 
    yside <- 2
    ylasnum <- 0
  } 
  ylinenum <- ymaxnum * cex.names/2 + 1

  ## xlabel
  ######################
  if (!is.null(xlabel)) { 
    xside <- 1			## axis position (1:xaxis; 2:yaxis)
    xlasnum <- 1			## orientation (0:horizontal; 1:vertical)
    linenum <- 1.5
    if (las.xnames %in% c(0,2)) {
      xlinenum <- .36 * xmaxnum + linenum 
    } else {
      xlinenum <- 2.5
    }
  } else {
    xlabel <- xvar
    xlinenum <- ifelse(las.xnames==0, xmaxnum/3, xmaxnum/4)
  }
  ## Set mar (number of lines for margins - bottom, left, top, right)
  if (is.null(mar)) {
    mar <-  par("mar")
    mar[3] <- ifelse(!is.null(main), 3, 2)		## top mar
    mar[1] <- xlinenum * cex.names + 3			## bottom mar
    mar[2] <- ylinenum + 2				## left mar
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
               jpg = {jpeg(filename=OUTPUTfn, width=device.width, height=device.height, res=jpeg.res, units="in")},
               ps = {postscript(file=OUTPUTfn, width=device.width, height=device.height)},
               pdf = {pdf(file=OUTPUTfn, width=device.width, height=device.height)},
              stop("invalid device.type"))
        
      } else {
        device.type[-i] <- device.type[-i]
      }
    }
 
    ## GENERATE BARPLOT
    ###########################################
    op <- par(xpd=NA, cex=par("cex"), mar=mar, las=las.xnames, mgp=c(3,0.5,0))

    plot(datx[[xvar]], y=datx[[yvar]], type="b", ylim=ylim, ylab='', 
         xlim=xlim, xlab='', cex.axis=cex.names, las=las.ynames, xaxt="n")
    axis(side=1, at=xticks, labels=FALSE)
    #text(x=xticks, par("usr")[3], labels=datx[[xvar]], adj = c(1, 2),
	#		cex=cex.names, srt=srt, xpd=TRUE)
    #text(x=xticks, par("usr")[3], labels=datx[[xvar]], adj = c(1.4, 1),
	#		cex=cex.names, srt=srt, xpd=TRUE)
 
    offset <- ifelse(srt==60, 1.5, 1)
    text(x=xticks, par("usr")[3], labels=datx[[xvar]], pos=1, cex=cex.names,
         srt=srt, xpd=TRUE, offset=offset)

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

    ## SET UP TEXT PLACEMENT AND ADD TEXT
    ######################################################
    if (!is.null(ylabel)) {
      mtext(ylabel, side=yside, line=cex.names*ylinenum, cex.lab=cex.label, las=ylasnum) 
    }
    if (!is.null(xlabel)) {
      mtext(xlabel, side=xside, line=cex.names*xlinenum, cex.lab=cex.label, las=xlasnum) 
    }
    if (!is.null(main)) {
      title(main=main, cex.main=cex.main)
    }
      
    if (savedata && !device.type[i] %in% c("default", "dev.new")) {

      message("###################################\n", 
			        "Plot written to: ", OUTPUTfn, 
		          "\n###################################")

      dev.off()
    }
  }
}

