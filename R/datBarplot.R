#' Data - Generates frequency barplot.
#' 
#' Generate a barplot of from a frequency data frame.
#' 
#' If parameters = NULL, then it will prompt user for input.
#' 
#' @param x Data frame or comma-delimited file (*.csv) - a frequency table.
#' @param xvar String. Name of X variable.
#' @param yvar String. Name of the y variable (e.g., FREQ).
#' @param grpvar String. Name of the variable for grouping.
#' @param errbars Logical. If TRUE, error bars are added to bar plot (sevar or
#' psevar must also be populated).
#' @param x.order String or Vector. Define order of xvar based on y values:
#' descending ("DESC") or ascending ("ASC") or vector of row numbers.  If NULL,
#' the order of the input table is used.
#' @param sevar String. Name of the variable with standard error values.
#' @param psevar String. Name of the variable with percent standard error.
#' @param device.type String. Type(s) of device for plotting ("dev.new", "jpg",
#' "pdf").
#' @param jpeg.res Integer. Resolution for jpeg image.
#' @param device.height Integer. Height (in inches) of barplot, if writing to
#' file.
#' @param device.width Integer. Width (in inches) of barplot, if writing to
#' file.
#' @param horiz Logical. If TRUE, bars are drawn horizontally with first bar at
#' the bottom.  If FALSE, bars are drawn vertically with first bar to the left
#' (barplot parameter).
#' @param toplabelvar String. Name of variable in x for adding labels to place
#' above each bar (e.g., NBRPLOTS.gt0).
#' @param ylim Number. A vector of min and max values, c(min,max) for the y
#' axis (or x axis if horiz=TRUE). If NULL, defaults to maximum y value. If
#' errbars=TRUE, the ylim defaults to the maximum y value plus the standard
#' error.
#' @param divideby String. Conversion number for output ('hundred', 'thousand',
#' 'million').
#' @param ylabel String. Label for the y axis (same as ylab).
#' @param xlabel String. Label for the x axis (same as xlab).
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
#' bty="n"), for moving legend to topleft and removing box around legend).
#' @return Outputs barplot to display window.
#' @note If savedata = TRUE, writes a jpg and pdf of barplot to outfolder.
#' 
#' To add legend parameters, add a parameter named args.legend, defined as a
#' list of specific legend parameters (see ?legend)...  e.g., 
#' args.legend=list(x="topright"). If specifying x and y, x defines the lower
#' right corner of legend box and y defines the upper right corner of box.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' # Set up data frame for example
#' ftyptab <- data.frame(cbind(FORTYPCD = c(182, 184, 201, 221, 265),
#'                             FREQ = c(110, 7, 900, 410, 155),
#'                             SE = c(10, 11, 18, 14, 22)))
#'                             
#' # Create basic barplot                           
#' datBarplot(x = ftyptab, xvar = "FORTYPCD")
#' 	
#' # Add standard errors to basic barplot
#' datBarplot(x = ftyptab, xvar = "FORTYPCD", errbars = TRUE, sevar = "SE")
#' @export datBarplot
datBarplot <- function(x, 
                       xvar = NULL, 
                       yvar = "FREQ", 
                       grpvar = NULL, 
                       errbars = FALSE, 
                       x.order = NULL, 
                       sevar = NULL, 
                       psevar = NULL, 
                       device.type = "dev.new", 
                       jpeg.res = 300, 
                       device.height = 5, 
                       device.width = 8, 
                       horiz = FALSE, 
                       toplabelvar = NULL, 
                       ylim = NULL, 
                       divideby = NULL, 
                       ylabel = NULL, 
                       xlabel = NULL, 
                       mar = NULL, 
                       addlegend = FALSE, 
                       main = NULL, 
                       cex.main = 1, 
                       cex.label = 1, 
                       cex.names = 0.8, 
                       las.xnames = 0, 
                       las.ynames = 1, 
                       savedata = FALSE, 
                       outfolder = NULL, 
                       outfn = NULL, 
                       outfn.pre = NULL, 
                       outfn.date = TRUE, 
                       overwrite = FALSE, 
                       ...){ 
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

  ## Set par 
  mar <-  graphics::par("mar")
  xpd <-  graphics::par("xpd")
  on.exit(graphics::par(mar=mar, xpd=xpd))
  

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## CHECK INPUTS
  ##############################################
  ## Check data table
  datx <- pcheck.table(x, caption="Frequency table?", stopifnull=TRUE)
  datnmlst <- names(datx)

  ## Automatically set xvar to variable not called FREQ if all conditions are met
  if (is.null(xvar) && ncol(datx) == 2 && length(yvar) == 1 && yvar == "FREQ") 
    xvar <- datnmlst[which(datnmlst != "FREQ")]

  ## Generate list of possible variables to check or select from
  if (is.null(yvar)) {
    xvarlst <- datnmlst
  } else {
    xvarlst <- datnmlst[which(!datnmlst %in% yvar)] 
  }

  ## Check xvar
  xvar <- pcheck.varchar(var2check=xvar, varnm="xvar", checklst=xvarlst, 
	caption="X variable", warn="xvar not in data table") 

  ## Modify list of possible variables to check or select from
  if (is.null(xvar)) {
    yvarlst <- xvarlst
  } else {
    yvarlst <- datnmlst[which(datnmlst != xvar)] 
  }

  ## Make sure xvar is a character
  if (!is.character(x[[xvar]])) x[[xvar]] <- as.character(x[[xvar]])

  ## Check xvar
  yvar <- pcheck.varchar(var2check=yvar, varnm="yvar", checklst=yvarlst, 
	caption="Y variable", warn="xvar not in data table", multiple=TRUE) 

  ## Check divideby
  ########################################################
  dividebylst <- c("hundred", "thousand", "million")
  divideby <- pcheck.varchar(var2check=divideby, varnm="divideby", 
		gui=gui, checklst=dividebylst, caption="Divide estimates?")

  if (!is.null(divideby)) {
    dividebynum <- ifelse(divideby == "hundred", 100, 
				ifelse(divideby == "thousand", 1000, 
					ifelse(divideby == "million", 1000000, 1)))
  }
  
  ## Check errbars 
  errbars <- pcheck.logical(errbars, "Error bars?", "NO")
    
  ## Modify list of possible variables to check or select from
  newvarlst <- yvarlst[which(!yvarlst %in% yvar)]

  ## Check psevar or sevar if errbars = TRUE
  if (errbars) {
    ## Check sevar
    sevar <- pcheck.varchar(var2check=sevar, varnm="sevar", checklst=newvarlst, 
		caption="se variable", warn="sevar not in data table", multiple=TRUE)

    if (is.null(psevar)) { 
      psevar <- pcheck.varchar(var2check=psevar, varnm="psevar", checklst=newvarlst, 
		caption="pse variable", warn="psevar not in data table", multiple=TRUE)
    }
    if (is.null(psevar) && is.null(sevar))
      stop("must include sevar(s) or psevar(s) for adding error bars")
  }

  ## GET addlegend 
  addlegend <- pcheck.logical(addlegend, "Add legend?", "NO")

  ## Top labels
  topvarlst <- datnmlst[which(!datnmlst %in% c(xvar,yvar))] 
  toplabelvar <- pcheck.varchar(var2check=toplabelvar, varnm="toplabelvar", 
		checklst=topvarlst, caption="Top label variable", 
		warn="toplabelvar not in data table", multiple=FALSE) 

  ## Check grpvar
  grpvarlst <- datnmlst[which(!datnmlst %in% c(xvar,yvar))]
  grpvar <- pcheck.varchar(var2check=grpvar, varnm="grpvar", checklst=grpvarlst, 
	caption="Group variable", warn="grpvar not in data table", multiple=FALSE) 

  ## Make sure grpvar is a character
  if (!is.null(grpvar)) 
    datx[[grpvar]] <- as.character(datx[[grpvar]])

  ## Check device.type
  device.typelst <- c("jpg", "pdf", "postscript", "win.metafile")

  if (length(device.type) == 0 || is.null(device.type)) 
    device.type <- "dev.new"

  device.type[device.type == "windows"] <- "dev.new"
  if (any(!device.type %in% c("dev.new", device.typelst)))
    stop("illegal 'device.type' device types must be one or more: ",
		"of 'dev.new' 'jpg' 'pdf' or 'ps'")
  
  ## Check savedata 
  savedata <- pcheck.logical(savedata, "Save bar plot?", "NO")

  if (savedata) {
    overwrite <- pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite files?", first="NO", gui=gui)  
    outfn.date <- pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="YES", gui=gui)  
    outfolder <- pcheck.outfolder(outfolder, gui)

    if (is.null(outfn)) outfn <- paste0("BARPLOT_", paste(yvar, collapse="_"))

    if (!any(device.type %in% device.typelst)) {  
      message("no export device specified..  adding jpg format")
      device.type <- c(device.type, "jpg")
    }
  }

  ################################################################################  
  ### DO WORK
  ################################################################################  
  ## Check if yvar is numeric
  datx[, (yvar) := lapply(.SD, as.numeric), .SDcols=yvar]

  ## Change NULL values to 0
  datx <- DT_NAto0(datx, yvar)
 
  ## Divide by 
  if (!is.null(divideby)) {
    datx[, (yvar) := lapply(.SD, function(x) x / dividebynum), .SDcols=yvar]
  }

  ## sevar/psevar
  ######################
  if (errbars) {
    if (is.null(sevar)) {
      if (!is.null(psevar)) {
        sevar <- {}
        for (j in 1:length(psevar)) {
          jvar <- paste0("sevar", j) 
          datx[[psevar[j]]] <- suppressWarnings(as.numeric(datx[[psevar[j]]]))
          datx[[jvar]] <- datx[[psevar[j]]] * datx[[yvar[j]]] / 100
          sevar <- c(sevar, jvar)
        }
      }
    }
    if (!is.null(divideby)) {
      datx[, (sevar) := lapply(.SD, function(x) x / dividebynum), .SDcols=sevar]
    }
  }

  ## Aggregate table with selected variables
  datxbp <- datx[, lapply(.SD, sum, na.rm=TRUE), 
			by=c(xvar, grpvar, sevar, toplabelvar), .SDcols=yvar]
 
  ## Ordering
  if (!is.null(x.order)) {
    if (length(x.order) == 1) {
      if (x.order == "DESC") {
        if (is.null(grpvar)) {
          orderval <- ifelse(horiz, 1, -1)
          data.table::setorderv(datxbp, yvar, order=rep(orderval, length(yvar)))
        } else {
          data.table::setorderv(datxbp, c(grpvar, yvar), order=c(1,rep(-1, length(yvar)))) 
        }
      } else if (x.order == "ASC") {
        if (is.null(grpvar)) {
          orderval <- ifelse(horiz, -1, 1)
          data.table::setorderv(datxbp, yvar, order=rep(orderval, length(yvar)))
        } else {
          data.table::setorderv(datxbp, c(grpvar, yvar), order=c(1,rep(1, length(yvar)))) 
        }
      }
    } else if (!is.vector(x.order)) {
      stop("invalid x.order..  must be 'DESC', 'ASC' or a vector of xvar values")
    } else if (is.numeric(x.order)) {
      if (all(x.order %in% datxbp[[xvar]])) {
        datxbp <- datxbp[order(datxbp[[xvar]]), ]
      } else if (all(x.order %in% seq(1:nrow(datxbp)))) {
        datxbp <- datxbp[x.order, ]
      } else {
        notinOrder <- datx[-x.order, xvar, with=FALSE] 
        message("missing values in x.order list: ", toString(notinOrder)) 
        datxbp <- datxbp[x.order, ]
      }
    } else if (all(datxbp[[xvar]] %in% as.character(x.order))) {
      datxbp <- datxbp[match(as.character(x.order), datxbp[[xvar]]),]
    } else if (!all(datxbp[[xvar]] %in% as.character(x.order))) {
      notinOrder <- datxbp[[xvar]][which(!datxbp[[xvar]] %in% as.character(x.order))] 
      message("missing values in x.order list: ", toString(notinOrder)) 
      datxbp <- datxbp[match(as.character(x.order), datxbp[[xvar]]),]    
    }
  }

  ## ylim
  ######################
  if (is.null(ylim)) {
    ## Set minimum ylim
    if (all(is.na(datxbp[,yvar, with = FALSE]))) {
      ylim.min <- min(datxbp[,xvar, with=FALSE], na.rm=TRUE)
	} else {  
      ylim.min <- ifelse(min(datxbp[,yvar, with=FALSE], na.rm=TRUE) < 0, 
							min(datxbp[,yvar, with=FALSE], na.rm=TRUE), 0)
    }

    ## Set maximum ylim  
    if (errbars) {
      ylim.max <- max(1.04 * 
			datxbp[,yvar, with=FALSE] + datxbp[,sevar, with=FALSE], na.rm=TRUE)
    } else {
	  if (all(is.na(datxbp[,yvar, with=FALSE]))) {
	    ylim.max <- 0
	  } else {
        ylim.max <- max(datxbp[,yvar, with=FALSE], na.rm=TRUE)
	  }
    }
	## Not sure how to handle this (when est.se = NaN) ... set to 0 for now
	if (ylim.min == "-Inf") ylim.min <- 0
	if (ylim.max == "-Inf") ylim.max <- 0
    ylim <- c(ylim.min, ylim.max)
	
  } else {
    if (length(ylim) != 2) {
      stop("ylim must be format c(min,max)")
    } else {
      ylim.min <- ylim[1]		## bug fixed by Liz
      ylim.max <- ylim[2]		## bug fixed by Liz
    }
  }

  ## If negative min number is greater than positive max number, match positive number
  ##	to negative number
  if (ylim.min < 0) {
    if (abs(ylim.min) > ylim.max) {
      ylim.max <- abs(ylim.min)
      ylim <- c(ylim.min, ylim.max)
    }
    if (abs(ylim.min) < ylim.max) {
      ylim.min <- -ylim.max
      ylim <- c(ylim.min, ylim.max)
    }
  }

  if (trunc(ylim.max) > 100) {
    ticks <- pretty(c(ylim.min, ylim.max), n=5)

    ## Check how much extend out
    interval <- ticks[length(ticks)] - ticks[(length(ticks)-1)]
    if ((ylim.max - ticks[(length(ticks)-1)]) > .18*interval) {
      ylim.max <- max(ticks)
    } else {
      ylim.max <- ticks[(length(ticks)-1)]
    }
    ylim <- c(ylim.min, ylim.max)

  } else {
    if (trunc(ylim.max) == 100) {
      ylim.max <- 100
      ticks <- (0:5)*20
    } else {
      if (ylim.max >= 80) {
        ylim.max <- ceiling(ylim.max/20)*20
        ticks <- (0:(ylim.max/20))*20
      } else {
        if (ylim.max >= 30) {
          ylim.max <- ceiling(ylim.max/10)*10
          ticks <- (0:(ylim.max/10))*10
        } else {
          ylim.max <- ceiling(ylim.max/5)*5
          ticks <- (0:(ylim.max/5))*5}
      }
    }
  }

  ## If negative min number is greater than positive max number, match positive number
  ##	to negative number
  if (ylim.min < 0) {
    if (abs(ylim.min) > ylim.max) {
      ylim.max <- abs(ylim.min)
      ylim <- c(ylim.min, ylim.max)
    }
    if (abs(ylim.min) < ylim.max) {
      ylim.min <- -ylim.max
      ylim <- c(ylim.min, ylim.max)
    }
  }

  if (horiz) {
    xlim <- ylim
    ylim <- NULL
  } else {
    xlim <- NULL
  }

  if (any(is.na(x[[xvar]]))) {
    message("xvar has NA values... removing")
    x <- x[!is.na(x[[xvar]]),]
  }
 
  ## SET UP MAR and TEXT PLACEMENT AND ADD TEXT
  ######################################################
  maxattnum <- 15
  xmaxnum <- max(nchar(x[[xvar]]))
  ymaxnum <- max(sapply(round(na.omit(datxbp[, yvar, with=FALSE])), nchar)) 
 
  ## las.xnames
  ######################
  if (is.null(las.xnames)) {
    if (horiz) {
      las.xnames <- 1
    } else {
      if (length(datxbp[[xvar]]) > maxattnum || xmaxnum > 10) {
        las.xnames <- 3
      } else {
        las.xnames <- 1
      }
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
    if (horiz) {
      yside <- 1
      ylasnum <- 1
      ylinenum <- 2
    } else {
      yside <- 2
      ylasnum <- 0
      ylinenum <- ymaxnum * cex.names/2 + 2
    }    
  } else {
    ylinenum <- ymaxnum * cex.names/2 + 1
  }

  ## xlabel
  ######################
  if (!is.null(xlabel)) { 
    xside <- ifelse(horiz, 2, 1)		## axis position (1:xaxis; 2:yaxis)
    xlasnum <- ifelse(horiz, 0, 1)	## orientation (0:horizontal; 1:vertical)
    linenum <- 2
    if (horiz) {
      #xlinenum <- .36 * (xmaxnum * cex.names) + linenum 
      xlinenum <- .36 * xmaxnum + linenum 
    } else {
      if (las.xnames %in% c(0,2)) {
        xlinenum <- .36 * xmaxnum + linenum 
      } else {
        xlinenum <- 4
      }
    }
  } else {
    if (horiz) {
     xlinenum <- (xmaxnum * cex.names)/2 - 1
    } else {
      xlinenum <- ifelse(las.xnames==0, xmaxnum/3, xmaxnum/4)
      #xlinenum = 8
    }
  }

  ## Set mar (number of lines for margins - bottom, left, top, right)
  #if (is.null(mar)) {
    mar <-  par("mar")
    mar[3] <- ifelse(!is.null(main), 3, 2)		## top mar
    if (horiz) {
      mar[1] <- ylinenum + 2.0				## bottom mar
      mar[2] <- xlinenum + (10/xlinenum)		## left mar
      mar[4] <- 2.5						## right mar
    } else {
      mar[1] <- xlinenum * cex.names + 2.8		## bottom mar
      mar[2] <- ylinenum + 1.6				## left mar
      mar[4] <- 0.5						## right mar
    }
  #}

  ## GENERATE BARPLOTS
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

    if (is.null(grpvar)) {
      if (length(yvar) == 1) {
        bp <- barplot(as.vector(datxbp[[yvar]]), xlim=xlim, ylim=ylim, horiz=horiz, 
			las=las.ynames, ...)
        if (horiz) {
          text(-.5, bp, adj = c(1, .5), xpd=TRUE, labels=datxbp[[xvar]], 
			cex=cex.names, srt=0)
        } else {
          text(bp, par("usr")[3], adj = c(1, 1), xpd=TRUE, labels=datxbp[[xvar]], 
			cex=cex.names, srt=srt)
        }
      } else {

        xmat <- as.matrix(t(datxbp[, yvar, with=FALSE]))
        if (addlegend) {
          bp <- barplot(xmat, beside=TRUE, xlim=xlim,
 			ylim=ylim, cex.names=cex.names, axisnames=FALSE, horiz=horiz,
 			legend=rownames(xmat), las=las.ynames, ...)
        } else {
           bp <- barplot(xmat, beside=TRUE, xlim=xlim, ylim=ylim, cex.names=cex.names, 
            	axisnames=FALSE, horiz=horiz, las=las.ynames, ...)
        }
        if (horiz) {
          text(-.5, apply(bp, 2, mean), adj = c(1, .5), xpd=TRUE,
 		  labels=datxbp[[xvar]], cex=cex.names, srt=0)
        } else {
          text(apply(bp, 2, mean), par("usr")[3], adj = c(1, 1), xpd=TRUE,
 		  labels=datxbp[[xvar]], cex=cex.names, srt=srt)
        }
      }     
    } else {
      xmat <- tapply(datxbp[,yvar, with=FALSE][[1]], 
			list(datxbp[[xvar]], datxbp[[grpvar]]), I)
      xmat[is.na(xmat)] <- 0

      if (addlegend) {
         bp <- barplot(xmat, beside=TRUE, xlim=xlim, ylim=ylim, cex.names=cex.names,
 		horiz=horiz, legend=rownames(xmat), cex.axis=cex.names, las=las.xnames, ...)
      } else {
         bp <- barplot(xmat, beside=TRUE, xlim=xlim, ylim=ylim, cex.names=cex.names,
 		horiz=horiz, cex.axis=cex.names, las=las.xnames, ...)
      }
    }

    ## SET UP TEXT PLACEMENT AND ADD TEXT
    ######################################################
    if (!is.null(ylabel))  
      mtext(ylabel, side=yside, line=cex.names*ylinenum, cex.lab=cex.label, las=ylasnum) 
    if (!is.null(xlabel)) 
      mtext(xlabel, side=xside, line=cex.names*xlinenum, cex.lab=cex.label, las=xlasnum) 
    if (!is.null(main))
      title(main=main, cex.main=cex.main)

    ## ADD TOP LABELS (NOTE: only works with 1 yvar... need to look into)
    ####################################
    if (!is.null(toplabelvar) && length(yvar) == 1) {
      toplabels <- round(datxbp[[toplabelvar]])
      ## Labels on top
      #up <- max(datxbp[[yvar]]) * 0.05
      #ypos <- datxbp[[yvar]] + up
      ypos <- datxbp[[yvar]]
      #ypos <- datxbp[, yvar, with=FALSE]
      xpos <- bp + .25
      text(xpos, ypos, toplabels, cex=.55, pos=3)
    }

    ## ADD ERROR BARS
    ######################################################
    if (errbars) {
      if (length(yvar) == 1) {
        if (!is.null(grpvar)) {
          y <- xmat
          semat <- tapply(datxbp[,sevar, with=FALSE][[1]], 
					list(as.character(datxbp[[xvar]]), 															as.character(datxbp[[grpvar]])), I)
          lower <- upper <- semat
        } else {
          y <- datxbp[[yvar]]
          lower <- upper <- datxbp[[sevar]]
        }
      } else {
        y <- xmat
        semat <- as.matrix(t(datxbp[,sevar, with=FALSE]))
        lower <- upper <- semat
      }
      if (horiz) {
        segments(y - lower, bp, y + upper, bp, lwd=2)
      } else {
        arrows(bp, y-lower, bp, y+upper, angle=90, code=3, length=0, lwd=2)
      }
    }

    if (savedata && !device.type[i] %in% c("default", "dev.new")) {

      message("###################################\n", 
			"Barplot written to: ", OUTPUTfn, 
		"\n###################################")

      dev.off()
    }
  }
}
