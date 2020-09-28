datBarplot <- function(x, xvar=NULL, yvar="FREQ", grpvar=NULL, errbars=FALSE, 
	x.order=NULL, sevar=NULL, psevar=NULL, device.type="dev.new", jpeg.res=300, 
	device.height=5, device.width=8, horiz=FALSE, toplabelvar=NULL, 
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

  options(scipen=6)

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## CHECK INPUTS
  ##############################################
  ## Check data table
  datx <- setDF(FIESTA::pcheck.table(x, caption="Frequency table?", stopifnull=TRUE))
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
  xvar <- FIESTA::pcheck.varchar(var2check=xvar, varnm="xvar", checklst=xvarlst, 
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
  yvar <- FIESTA::pcheck.varchar(var2check=yvar, varnm="yvar", checklst=yvarlst, 
	caption="Y variable", warn="xvar not in data table", multiple=TRUE) 


  ## Change any NULL values in Estimate variable(s) to 0
  if (sum(is.na(datx[[yvar]])) >= 1) {
    message("na values are invalid.. changing to 0")
    datx[is.na(datx)] <- 0
  }  

  ## Check divideby
  ########################################################
  dividebylst <- c("hundred", "thousand", "million")
  divideby <- FIESTA::pcheck.varchar(var2check=divideby, varnm="divideby", 
		gui=gui, checklst=dividebylst, caption="Divide estimates?")

  if (!is.null(divideby))
    dividebynum <- ifelse(divideby == "hundred", 100, 
				ifelse(divideby == "thousand", 1000, 
					ifelse(divideby == "million", 1000000, 1)))
  

  ## Check errbars 
  errbars <- FIESTA::pcheck.logical(errbars, "Error bars?", "NO")
    
  
  ## Modify list of possible variables to check or select from
  newvarlst <- yvarlst[which(!yvarlst %in% yvar)]

  ## Check psevar or sevar if errbars = TRUE
  if (errbars) {
    ## Check sevar
    sevar <- FIESTA::pcheck.varchar(var2check=sevar, varnm="sevar", checklst=newvarlst, 
		caption="se variable", warn="sevar not in data table", multiple=TRUE)

    if (is.null(psevar)) { 
      psevar <- FIESTA::pcheck.varchar(var2check=psevar, varnm="psevar", checklst=newvarlst, 
		caption="pse variable", warn="psevar not in data table", multiple=TRUE)
    }
    if (is.null(psevar) && is.null(sevar))
      stop("must include sevar(s) or psevar(s) for adding error bars")
  }

  ## GET addlegend 
  addlegend <- FIESTA::pcheck.logical(addlegend, "Add legend?", "NO")


  ## Top labels
  topvarlst <- datnmlst[which(!datnmlst %in% c(xvar,yvar))] 
  toplabelvar <- FIESTA::pcheck.varchar(var2check=toplabelvar, varnm="toplabelvar", 
		checklst=topvarlst, caption="Top label variable", 
		warn="toplabelvar not in data table", multiple=FALSE) 

  ## Check grpvar
  grpvarlst <- datnmlst[which(!datnmlst %in% c(xvar,yvar))]
  grpvar <- FIESTA::pcheck.varchar(var2check=grpvar, varnm="grpvar", checklst=grpvarlst, 
	caption="Group variable", warn="grpvar not in data table", multiple=FALSE) 

  ## Make sure grpvar is a character
  if (!is.null(grpvar)) 
    datx[[grpvar]] <- as.character(datx[[grpvar]])

  ## Decending order
  order.d <- NULL
  if (!is.null(x.order)) {
    if (length(x.order) == 1) {
      if (x.order == "DESC") {
        order.d <- TRUE
      }else if (x.order == "ASC") {
        order.d <- FALSE
      }
    } else if (!is.vector(x.order)) {
      stop("invalid x.order..  must be 'DESC', 'ASC' or a vector of xvar values")
    } else if (!all(x[[xvar]] %in% x.order)) {
      notinOrder <- x[[xvar]][which(!x[[xvar]] %in% x.order)]  
      if (length(notinOrder) > 0) {
        stop("invalid x.order values.. missing: ", paste(notinOrder, collapse=","))
      } else{
        stop("invalid x.order values")
      }
    }
  }

  ## Check device.type
  device.typelst <- c("jpg", "pdf", "postscript", "win.metafile")

  if (length(device.type) == 0 || is.null(device.type)) 
    device.type <- "dev.new"

  device.type[device.type == "windows"] <- "dev.new"
  if (any(!device.type %in% c("dev.new", device.typelst)))
    stop("illegal 'device.type' device types must be one or more: ",
		"of 'dev.new' 'jpg' 'pdf' or 'ps'")
  
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
  ## Check if yvar is numeric
  for (y in yvar) {
    if (!is.numeric(datx[[y]])) datx[[y]] <- as.numeric(datx[[y]])
    
    if (!is.null(divideby))
      datx[[y]] <- datx[[y]] / dividebynum
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
    } else {
      if (!is.null(divideby)) {
        datx[[sevar]] <- datx[[sevar]] / dividebynum
      }	
    }
  }

  ## Aggregate table with selected variables
  datxbp <- aggregate(datx[,yvar, drop=FALSE], 
		datx[, c(xvar, grpvar, sevar, toplabelvar), drop=FALSE], sum, 
		stringsAsFactors=FALSE)

  if (!is.null(order.d)) {
    if (is.null(grpvar)) {
      decreasing <- ifelse(horiz, FALSE, TRUE)
      datxbp <- datxbp[order(datxbp[[yvar]], decreasing=decreasing),]
    } else {
      datxbp <- datxbp[order(datxbp[[grpvar]], rev(datxbp[[xvar]])),]
    }
  } else if(!is.null(x.order)) {
    if (horiz) x.order <- rev(x.order)
    if (is.null(grpvar)) {
      datxbp <- datxbp[order(match(datxbp[,xvar], x.order)),]
    } else {
      datxbp <- datxbp[order(datxbp[,grpvar], match(datxbp[,xvar], x.order)),]
    }
  } else {
    datxbp <- datxbp
  }
 
  ## ylim
  ######################
  if (is.null(ylim)) {
    ## Set minimum ylim
    ylim.min <- ifelse(min(datxbp[[yvar]]) < 0, min(datxbp[[yvar]]), 0)

    ## Set maximum ylim  
    if (errbars) {
      #ylim.max <- max(1.04 * (datxbp[[yvar]] + 2 * datxbp[[sevar]]), na.rm=TRUE)
      ylim.max <- max(1.04 * (datxbp[[yvar]] + datxbp[[sevar]]), na.rm=TRUE)
    } else {
      ylim.max <- max(datxbp[[yvar]])
    }
    ylim <- c(ylim.min, ylim.max)

  } else {
    if (length(ylim) != 2) {
      stop("ylim must be format c(min,max)")
    } else {
      ylim <- ylim
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
    print(x)
    message("xvar has NA values... removing")
    x <- x[!is.na(x[[xvar]]),]
  }
 
  ## SET UP MAR and TEXT PLACEMENT AND ADD TEXT
  ######################################################
  maxattnum <- 15
  xmaxnum <- max(nchar(x[[xvar]]))

  ymaxnum <- max(sapply(round(na.omit(datxbp[[yvar]])), nchar)) 
 
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
  if (is.null(mar)) {
    mar <-  par("mar")
    mar[3] <- ifelse(!is.null(main), 3, 2)		## top mar
    if (horiz) {
      mar[1] <- ylinenum + 2.0				## bottom mar
      mar[2] <- xlinenum + (10/xlinenum)		## left mar
      mar[4] <- 2.5						## right mar
    } else {
      mar[1] <- xlinenum * cex.names + 3.5		## bottom mar
      mar[2] <- ylinenum + 1.6				## left mar
      mar[4] <- 0.5						## right mar
    }
  }

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
#         bp <- barplot(datxbp[[yvar]], names.arg=datxbp[[xvar]], xlim=xlim, ylim=ylim, 
#          cex.names=cex.names, horiz=horiz, las=las.ynames, ...)
      } else {
        xmat <- as.matrix(t(datxbp[[yvar]]))
        if (addlegend) {
           bp <- barplot(xmat, beside=TRUE, names.arg=datxbp[[xvar]], xlim=xlim, ylim=ylim, 
            cex.names=cex.names, cex.axis=cex.names, horiz=horiz, legend=rownames(xmat), 
            las=las.ynames, ...)
        } else {
           bp <- barplot(xmat, beside=TRUE, names.arg=datxbp[[xvar]], xlim=xlim, ylim=ylim, 
            cex.names=cex.names, cex.axis=cex.names, horiz=horiz, las=las.ynames, ...)
        }
      }
      #text(bp, par("usr")[3]-0.25, adj=1, xpd=TRUE, labels=datxbp[[xvar]], cex=cex.names, srt=srt)
      text(bp, par("usr")[3], adj = c(1, 1), xpd=TRUE, labels=datxbp[[xvar]], 
			cex=cex.names, srt=srt)

     
    } else {
      xmat <- tapply(datxbp[[yvar]], list(datxbp[[xvar]], datxbp[[grpvar]]), I)
      xmat[is.na(xmat)] <- 0
      if (addlegend) {
         bp <- barplot(xmat, beside=TRUE, xlim=xlim, ylim=ylim, cex.names=cex.names, horiz=horiz, 
          legend=rownames(xmat), cex.axis=cex.names, las=las.xnames, ...)
      } else {
         bp <- barplot(xmat, beside=TRUE, xlim=xlim, ylim=ylim, cex.names=cex.names, horiz=horiz, 
          cex.axis=cex.names, las=las.xnames, ...)
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

    ## ADD TOP LABELS
    ####################################
    if (!is.null(toplabelvar)) {
      toplabels <- round(datxbp[[toplabelvar]])
      ## Labels on top
      #up <- max(datxbp[[yvar]]) * 0.05
      #ypos <- datxbp[[yvar]] + up
      ypos <- datxbp[[yvar]]
      xpos <- bp + .15
      text(xpos, ypos, toplabels, cex=.75, pos=3)
    }
 
    ## ADD ERROR BARS
    ######################################################
    if (errbars) {
      if(length(yvar) == 1){
        y <- datxbp[[yvar]]
        lower <- upper <- datxbp[[sevar]]
      } else {
        y <- xmat
        semat <- as.matrix(t(datxbp[[sevar]]))
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
