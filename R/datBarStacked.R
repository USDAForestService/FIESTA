#' Data - Generates frequency barplot.
#' 
#' Generate a barplot of frequencies in order from most to least.
#' 
#' # Note: This function uses a customized lengthwise axis (y axis if horiz=F,
#' x axis if horiz=T) # Therefore to modify this axis by par commands (other
#' than the # ones specifically included above) may require changing the
#' function. For example par(yaxp) # may not work.  # # # The arguments bar.lab
#' and bar.lim are equivalent to xlab/ylab, xlim/ylim, controlling # whichever
#' is the lengthwise axis in the bar charts (as detrmined by horiz).  # # #
#' bar.ratio is only used when bar.lim is specified, it controls what
#' percentage of the # plot area will be occupied by the specified range of the
#' chart itself. For example, setting # bar.lim=c(0,80) and bar.ratio=0.75 will
#' result in the 80% tick mark occuring 75% of the way # across the plot
#' region, leaving 25% of the plot region for the legend. (Note, if any of #
#' the bars are taller than 80% they will still overlap the legend.)  # # If
#' bar.lim is not specified, the function will automatically scale the
#' lengthwise axis so # that all the bars are as tall as possible, but none of
#' the bars over lap the legend.  # # The main purpose of specifying bar.lim is
#' if you want multiple forest types to be shown at # the same scale,
#' sacrificing individual forest type details to make the graphs comparable #
#' across forest types. Then you may need to fiddle with bar.ratio till the
#' plots all look good.
#' 
#' @param x Data frame or comma-delimited file (*.csv) - table of values to be
#' plotted.
#' @param main.attribute String. The column to be used for each bar.
#' @param sub.attribute String. The column to be used to subdivide each bar.
#' @param response String. The column of values to be plotted. Currently
#' defaults to "phat".
#' @param percent Logical. If TRUE, values cover values in a stack are
#' converted to percent of stack.
#' @param LUT.color Data frame or comma-delimited file (*.csv) - look up table
#' for colors.  Must contain column with same name as sub.attribute.
#' @param color String. Automated color selection ("rainbow", "topo", "heat",
#' "terrain", "cm").
#' @param device.type String. The type(s) of device for ploting ("default",
#' "jpg", "pdf", or "ps").
#' @param jpeg.res Integer. The resolution for jpeg image.
#' @param device.width Integer. The width of output device (in inches)'
#' @param device.height Integer. The height of output device (in inches)'
#' @param mar See par.. A numerical vector representing number of lines for
#' margins (c(bottom, left, top, right).
#' @param horiz Logical. See barplot. If FALSE, the bars are drawn vertically
#' with the first bar to the left. If TRUE, the bars are drawn horizontally
#' with the first bar at the bottom.
#' @param bar.lim Number vector. Equivalent to xlim or ylim, for whichever is
#' the lengthwise axis in barcharts (ex. c(0,10)). Warning: for lower limits
#' other than zero (ex. c(20,100), will behave strangely because par(xpd) is
#' set to NA.
#' @param bar.ratio Proportion of figure area taken up by barplot vs taken by
#' legend.
#' @param ylabel String. A label for the y axis (same as ylab).
#' @param xlabel String. A label for the x axis (same as xlab).
#' @param las.xnames Number. The direction of x variable names (0,1,2,3).
#' 0:Default, parallel; 1:horizontal; 2:perpendicular; 3:vertical.
#' @param main.order String vector. A vector of main.attribute names in desired
#' order for bars. May also be 'DESC' or 'ASC'.
#' @param sub.order String vector. Avector of sub.attribute names in desired
#' order for stack, with the first name used as the column in each stack. If
#' NULL, the order is based on the overall cover of each sub.attribute. May
#' also be 'DESC' or 'ASC'.
#' @param legend.fit Logical. Should bar.lim be changed to fit the legend of
#' the plot.  Will only be used if the legend is on the right side of a
#' horizontal plot or the top of a vertical plot. (i.e. horiz=FALSE).
#' @param legend.cex Number. Expansion factor for legend text.
#' @param legend.x See legend. The x coordinate to be used to position the
#' legend. If horiz=TRUE, suggested options include "topright" or
#' "bottomright". If horiz=FALSE, suggested options include "topleft" or
#' "topright".
#' @param legend.y See legend. The y coordinate to be used to position the
#' legend.
#' @param legend.title See legend. A title for the legend.
#' @param legend.bty See legend. the type of box to be drawn around the legend.
#' @param legend.bg See legend. The background color for the legend box.
#' @param legend.inset See legend. The distance from the margins as a fraction
#' of the plot region.
#' @param legend.xpd See legend.
#' @param main String. Title for plot.
#' @param cex.main Number. Expansion factor for title.
#' @param cex.label Number. A number representing cex in barplot (size
#' expansion of x and/or ylabels.
#' @param cex.names Number. Expansion factor for axis names (bar labels). Ex.
#' 0.5 represents half the size.
#' @param sub.add0 Logical. If TRUE, adds categories with 0 values to
#' sub.attribute legend.
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
#' @param ...  list of additional arguments to pass to barplot(); names of the
#' list are used as argument names.
#' @return Outputs stacked barplot to display window.
#' @note If savedata = TRUE, writes a jpg and pdf of barplot to outfolder.
#' @author Elizabeth Freeman, Tracey S. Frescino
#' @keywords data
#' @export datBarStacked
datBarStacked <- function(x, 
                          main.attribute, 
                          sub.attribute, 
                          response = "phat", 
                          percent = FALSE, 
                          LUT.color = NULL, 
                          color = "rainbow", 
                          device.type = "default", 
                          jpeg.res = 300,
                          device.width = 9, 
                          device.height = 6, 
                          mar = NULL, 
                          horiz = TRUE, 
                          bar.lim = NULL, 
                          bar.ratio = 1, 
                          ylabel = NULL, 
                          xlabel = NULL, 
                          las.xnames = NULL, 
                          main.order = NULL, 
                          sub.order = NULL, 
                          legend.fit = NULL, 
                          legend.cex = 0.8, 
                          legend.x = NULL, 
                          legend.y = NULL, 
                          legend.title = NULL, 
                          legend.bty = "o", 
                          legend.bg = par("bg"), 
                          legend.inset= 0, 
                          legend.xpd = par("xpd"), 
                          main = NULL, 
                          cex.main = 1, 
                          cex.label = 1, 
                          cex.names = 0.8, 
                          sub.add0 = FALSE, 
                          savedata = FALSE, 
                          outfolder = NULL, 
                          outfn = NULL, 
                          outfn.pre = NULL, 
                          outfn.date = TRUE, 
                          overwrite = FALSE, 
                          ...){

### Arguments ###

  # x  table of values to be plotted.
  #
  # main.attribute  column to be used for each bar. currently defaults to "VEGGRP1" 
  # sub.attribute  column to be used to subdivide each bar. currently defaults to "SPP"
  # response        column of values to be plotted. currently defaults to "phat"
  #
  #
  # LUT.color  look up table for colors (for example, LUT_SPP_VEGGRP).
  #    must contain collumns named:  same name as sub.attribute
  #              COLOR (for plot.type="fancy bar" or "fancy pie"
  #
  # cex      usual meaning
  # legend.cex  usual meaning  
  # horiz    usual meaning (barcharts only)  
  # ylabel    equivalent to ylab, for whichever is the lengthwise axis in barcharts
  # xlabel    equivalent to xlab, for whichever is the lengthwise axis in barcharts
  # bar.lim    equivalent to xlim or ylim, for whichever is the lengthwise axis in barcharts
  #      warning: for lower limits other than zero, will behave 
  #       strangely because par(xpd) is set to NA. I suggest just changing upper limits.
  #      For example, bar.lim=c(0,100) or bar.lim=c(0,80), not bar.lim=c(20,100).
  # bar.ratio    proportion of figure area taken up by barplot vs taken by legend.
  # main.order  A vector of main.attribute names in desired order for bars.
  # sub.order  A vector of sub.attribute names in desired order for stack, with the first 
  #        name the first in each stack. If NULL, the order is based on the overall
  #        cover of each sub.attribute.
  #
  # legend.fit        TRUE or FALSE, should bar.lim be changed to fit the legend into the plot.
  #      will only be used on if the legend is on the rightside of a horizontal plot
  #      or the top of a vertical plot (i.e. horiz=F)
  # legend.           other arguments to pass to legend() function
  # color    c("rainbow", "topo", "heat", "terrain", "cm")

### Details ###

  # Note: This function uses a customized lengthwise axis (y axis if horiz=F, x axis if horiz=T) 
  # Therefore to modify this axis by par commands (other than the 
  # ones specifically included above) may require changing the function. For example par(yaxp) 
  # may not work.
  #
  #
  # The argument bar.lim is equivalent to xlim/ylim, controlling whichever is the lengthwise axis
  # in the bar charts (as detrmined by horiz).
  #
  #
  # bar.ratio is only used when bar.lim is specified, it controls what percentage of the 
  # plot area will be occupied by the specified range of the chart itself. For example, setting 
  # bar.lim=c(0,80) and bar.ratio=0.75 will result in the 80% tick mark occuring 75% of the way 
  # across the plot region, leaving 25% of the plot region for the legend. (Note, if any of 
  # the bars are taller than 80% they will still overlap the legend.)
  #
  # If bar.lim is not specified, the function will automatically scale the lengthwise axis so 
  # that all the bars are as tall as possible, but none of the bars over lap the legend.
  #
  # The main purpose of specifying bar.lim is if you want multiple forest types to be shown at
  # the same scale, sacrificing individual forest type details to make the graphs comparable
  # across forest types. Then you may need to fiddle with bar.ratio till the plots all look good.


  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if(gui){
    x=response=color=savedata <- NULL
  }

  ## Set par 
  mar <-  graphics::par("mar")
  xpd <-  graphics::par("xpd")
  on.exit(graphics::par(mar=mar, xpd=xpd))
  
  ###################################################################################
  ########################## Check Device Type ######################################
  ###################################################################################
  device.typelst <- c("jpg", "pdf", "postscript", "win.metafile")

  if(length(device.type)==0 || is.null(device.type)){
    device.type <- "default"
  }

  device.type[device.type=="windows"]<-"default"
  if(any(!device.type %in% c("default", device.typelst))){
    stop("illegal 'device.type' device types must be one or more of 'default' 'jpg' 'pdf' or 'ps'")
  }

  device.type<-sort(device.type)
  if("default"%in%device.type){
    device.type <- c(device.type[device.type!="default"],"default")
  }


  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## Estimate table
  ################################
  datx <- pcheck.table(x, caption="Estimate table?")
  if(is.null(datx) || nrow(datx) == 0){
    stop("check x")
  }

  ### GET main.attribute and sub.attribute
  varlst <- names(datx)

  main.attribute <- pcheck.varchar(var2check=main.attribute, varnm="main.attribute", 
	checklst=varlst, caption="main attribute", warn="main.attribute not in data table") 
  if (!is.character(datx[[main.attribute]])) {
    datx[[main.attribute]] <- as.character(datx[[main.attribute]]) }
  sub.attribute <- pcheck.varchar(var2check=sub.attribute, varnm="sub.attribute", 
	checklst=varlst, caption="sub attribute", warn="sub.attribute not in data table")
  if (!is.character(datx[[sub.attribute]])) { 
    datx[[sub.attribute]] <- as.character(datx[[sub.attribute]]) }
  response <- pcheck.varchar(var2check=response, varnm="response", checklst=varlst, 
      caption="response", warn="response not in data table") 
  if (!is.numeric(datx[[response]])) { 
    stop("the response attribute must be numeric") }

  ## Check horiz
  horiz <- pcheck.logical(horiz, varnm="horiz", 
		title="Horizontal bars?", first="NO", gui=gui)  


  ###################################################################################
  ########################## Process pbar.table ####################################
  ###################################################################################

  ## Divide by
  divide <- FALSE
  if (max(abs(datx[[response]])) > 10000000) {
    datx[[response]] <- datx[,response] / 1000000
    divide <- TRUE
    dlabel <- "(millions)"
  } else if (max(abs(datx[[response]])) > 10000) {
    datx[[response]] <- datx[[response]] / 1000
    divide <- TRUE
    dlabel <- "(thousands)"
  }

  p.sub <- data.frame(  main=datx[[main.attribute]],
          sub=datx[[sub.attribute]],
          response=datx[[response]])

  ################## NEW CODE
  main.names <- unique(p.sub$main)
  sub.names <- unique(p.sub$sub)

  ## Order main.attribute values
  if (!is.null(main.order)) {
    if (length(main.order) == 1 && main.order %in% c("ASC", "DESC")) {
      tmp <- aggregate(p.sub$response, by=list(p.sub$main), sum)
      names(tmp) <- c("main", "response")
      decreasing <- ifelse(main.order == "DESC", TRUE, FALSE)
      if (!horiz) decreasing <- !decreasing
      tmp <- tmp[order(tmp$response, decreasing=decreasing), ]
      main.order <- tmp$main
    }

    main.order <- as.character(main.order)
    if (!all(main.names %in% main.order)) {
      not <- main.names[which(!main.names %in% main.order)]
      stop("check main.order..  incomplete number of names: ", toString(not))
    } else if (!all(main.order %in% main.names)) {
      main.order <- main.order[which(main.order %in% main.names)]
    }
  } else {
    main.order <- main.names
  }
  main.order <- rev(main.order)

  ## Order sub.attribute values
  if (!is.null(sub.order)) {
    if (length(sub.order) == 1 && sub.order %in% c("ASC", "DESC")) {
      tmp <- aggregate(p.sub$response, by=list(p.sub$sub), sum)
      names(tmp) <- c("sub", "response")
      decreasing <- ifelse(sub.order == "DESC", FALSE, TRUE)
      tmp <- tmp[order(tmp$response, decreasing=decreasing), ]
      sub.order <- tmp$sub
    }

    sub.order <- as.character(sub.order)
    if (!all(sub.names %in% sub.order)) {
      not <- sub.names[which(!sub.names %in% sub.order)]
      stop("check sub.order..  incomplete number of names: ", addcommas(not))
    } else if (!all(sub.order %in% sub.names)){
      sub.order <- sub.order[which(sub.order %in% sub.names)]
    }
  }
 
  ## LUT.color
  ################################
  LUT.color <- pcheck.table(LUT.color, caption="Color lookup table?", gui=gui)
  
  if (!is.null(LUT.color)) {

    ## Check if correct variables in table
    if(!sub.attribute%in%names(LUT.color)){
      stop("'LUT.color' must contain column with same name as sub.attribute")}

    if(!"COLOR"%in%names(LUT.color)){
      stop("'LUT.color' must contain column named 'COLOR'")}

    if(any(!p.sub$sub%in%LUT.color[,sub.attribute])){
      stop("'estimate table' contains values of 'sub.attribute' not included in 'LUT.color'")}


    if(is.factor(LUT.color$COLOR)){
      LUT.color$COLOR<-as.character(LUT.color$COLOR)}

    if(is.factor(LUT.color[,sub.attribute])){
      LUT.color[,sub.attribute]<-as.character(LUT.color[,sub.attribute])}

    if(any(!LUT.color$COLOR%in%colors())){
      stop("'LUT.color' contains values of 'COLOR' not included in 'colors()'")}

    if(anyDuplicated(LUT.color[,sub.attribute])){
      stop("'LUT.color' contains duplicate 'sub.attributes'")}

  } else {

    ## GET TABTYPE IF NULL
    ###########################################
    colorlst <- c("rainbow", "topo", "heat", "terrain", "cm")
    color <- pcheck.varchar(var2check=color, varnm="color", checklst=colorlst, 
      caption="Color table (rainbow, topo, heat, terrain, cm)",
      warn="check color.. not valid")   

    if (color != "rainbow")
      color <- paste(color, "colors", sep=".")
    
    subclasses <- unique(datx[[sub.attribute]])
    nbrclasses <- length(subclasses)
  
    LUT.color <- data.frame(cbind(subclasses, get(color)(nbrclasses)),
 		stringsAsFactors=FALSE)
    names(LUT.color) <- c(sub.attribute, "COLOR")

  }

  if(!is.null(bar.lim)){
    warning("if legend overlaps bars, decrease bar.ratio")}    


  ### GET savedata 
  savedata <- pcheck.logical(savedata, "Save bar plot?", "NO")

  if (savedata) {
    overwrite <- pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite files?", first="NO", gui=gui)  
    outfn.date <- pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="YES", gui=gui)  
    outfolder <- pcheck.outfolder(outfolder, gui)
  
    ## outfn
    if(is.null(outfn))
      outfn <- paste0("BARSTACKED_", main.attribute, "_", sub.attribute)
    
    if(!any(device.type %in% device.typelst)){  
      warning("no export device specified..  using jpg format")
      device.type <- c(device.type, "jpg")
    }
  }


  ################################################################################  
  ### DO WORK
  ################################################################################  

  ## SET UP MAR and TEXT PLACEMENT AND ADD TEXT
  ######################################################
  maxattnum <- 15
  xmaxnum <- max(nchar(p.sub$main))
    
  ## las.xnames
  ######################
  if (is.null(las.xnames)) {
    if (horiz) {
      las.xnames <- 1
    } else {
      if (length(unique(p.sub$main)) > maxattnum || xmaxnum > 10) {
        las.xnames <- 3
      } else {
        las.xnames <- 1
      }
    }
  }
  srt <- ifelse(las.xnames == 1, 0, ifelse(las.xnames == 3, 60, 90))

  ## ylabel
  ######################
  ymaxnum <- max(sapply(round(p.sub$response), nchar)) 
  if (!is.null(ylabel)) {
    if (horiz) {
      yside <- 1
      ylasnum <- 1
      ylinenum <- 2
    } else {
      yside <- 2
      ylasnum <- 0
      ylinenum <- ymaxnum * cex.names/2 + 1
    }
    if (divide) {
      ylabel <- paste(ylabel, dlabel)
    }
  } else {
    ylinenum <- ymaxnum * cex.names/2 + 2
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

    mar[3] <- ifelse(!is.null(main), 3.5, 2)	## top mar
    mar[4] <- 2.5						## right mar

    if (horiz) {
      mar[1] <- ylinenum + 2.0				## bottom mar
      mar[2] <- xlinenum + 2.0				## left mar
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
    if (device.type[i] != "default") { 
      ext <- device.type[i]
      OUTPUTfn <- getoutfn(outfn, outfolder=outfolder, outfn.pre=outfn.pre, 
		outfn.date=outfn.date, overwrite=overwrite, ext=ext)
    }

    if (device.type[i] == "default") {
      dev.new(width=device.width, height=device.height, record=TRUE)
    } else {
      if (savedata) {
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
      
    # create matrix with: 
    #  one row for each possible value of sub.attribute
    #  one column for each possible group


    mat.type <- xtabs(response ~ sub + main, p.sub)

    ################## NEW CODE
    ## Order main.attribute values
    if (!is.null(main.order))
      mat.type <- mat.type[, main.order] 

    ## Order sub.attribute values
    if (!is.null(sub.order))
      mat.type <- mat.type[match(sub.order, row.names(mat.type)),]

    
    ### if percent
    if (percent)
      mat.type <- t(t(mat.type)/colSums(mat.type) * 100)
    
    ################## END NEW CODE

    #find ordered unique names of main.attribute and sub.attribute
    main.names <- colnames(mat.type)
    sub.names <- rownames(mat.type)

    #find maximum height of each bar (must be done before linking colors to 'mat.type'
    all.max <- apply(mat.type[,], 2, sum, na.rm = TRUE)

    ## Remove rows with 0 values
    if (!sub.add0) {
      sub.names <- sub.names[rowSums(mat.type) > 0]
      mat.type <- mat.type[rowSums(mat.type) > 0,]
    }

    #find colors
    sub.colors <- vector("character", length(sub.names))
    names(sub.colors) <- sub.names
    sub.colors[] <- LUT.color$COLOR[match(names(sub.colors), LUT.color[[sub.attribute]])]

    #legend.inset <- legend.inset+.04

    # make bar plot
  
    if (horiz == FALSE) {
      op <- par(xpd=NA, cex=par("cex"), mar=mar)

      # check legend.x and legend.y
      # check legend.x and legend.y
      if(is.null(legend.x) && is.null(legend.y)){
        legend.x <- "topright"}
      if(is.null(legend.fit)){
        legend.fit <- TRUE}
      if(!legend.x %in% c("topright","topleft")){
        legend.fit <- FALSE}

      # make sure there is space for legend
      plot(c(0,1),c(0,1),type="n",bty="n",xaxt="n",yaxt="n",xlab="",ylab="",xaxs="i",yaxs="i")
      LEGEND <- legend(  x=legend.x,
              y=legend.y,
              title=legend.title,
              bty=legend.bty,
              bg=legend.bg,
              inset=legend.inset,
              xpd=legend.xpd,
              legend=names(sub.colors),
              fill=sub.colors,
              cex=legend.cex,
              ncol=2,
              plot=FALSE)
      BP <- barplot(mat.type, col=sub.colors, yaxt="n", horiz=horiz, cex.names=cex.names, 
            plot=FALSE,...)

      if (legend.fit) {
        # check left clearance
        if (legend.x == "topright") {
          LEG.left <- (LEGEND$rect$left)*(max(BP)+min(BP))
          intheway <- (LEG.left < BP+0.5*(BP[2]-BP[1]))}
        if (legend.x == "topleft") {
          LEG.right <- (LEGEND$rect$left+LEGEND$rect$w)*(max(BP)+min(BP))
          intheway <- (LEG.right > BP-0.5*(BP[2]-BP[1]))}
        # check top/bottom clearance
        if (legend.x == "bottomright") {
          LEG.top <- (LEGEND$rect$top)*(max(BP)+min(BP))
          intheway <- (LEG.top > BP-0.5*(BP[2]-BP[1])) }


        legendside.max <- max(all.max[intheway])
      
        # check bottom clearance
        LEG.bottom <- LEGEND$rect$top-LEGEND$rect$h-0.02
        legendside.max<-legendside.max/LEG.bottom
      }
      
      # calculate lengthwise axis

      if (is.null(bar.lim)) {
        bar.max <- max(all.max,na.rm = TRUE)
      } else {
        bar.max <- max(bar.lim)}

      if (percent) {
        bar.max <- 100
        ticks <- (0:5)*20
      } else {
        if (trunc(bar.max) > 100) {
          ticks <- pretty(c(0,bar.max), n=5)
          bar.max <- max(ticks)
        } else {
          if (trunc(bar.max) == 100) {
            bar.max <- 100
            ticks <- (0:5)*20
          } else {
            if (bar.max >= 80) {
              bar.max <- ceiling(bar.max/20)*20
              ticks <- (0:(bar.max/20))*20
            } else {
              if (bar.max >= 30) {
                bar.max <- ceiling(bar.max/10)*10
                ticks <- (0:(bar.max/10))*10
              } else {
                bar.max <- ceiling(bar.max/5)*5
                ticks <- (0:(bar.max/5))*5}}}}
      }

      if (is.null(bar.lim)) {
        if (legend.fit) {
          bar.lim <- c(0, max(bar.max,legendside.max)/bar.ratio)
        } else {
          bar.lim <- c(0, bar.max/bar.ratio)}
      } else {
        bar.lim <- bar.lim/bar.ratio
      }

      par(new=TRUE)
      bp <- barplot(mat.type, col=sub.colors, ylim=bar.lim, yaxt="n", horiz=horiz, 
          cex.names=cex.names, axisnames=FALSE, ...)
      text(bp, par("usr")[3], adj = c(1, 1), xpd=TRUE, labels=main.order, 
			cex=cex.names, srt=srt)

      bp.max <- max(bp)+min(bp)

      lines(c(0,0),c(0,bar.max))
      for (j in ticks) {
        lines(c(-bp.max/80,0), c(j,j))}
 
      text(x=rep(0, length(ticks)), y=ticks, labels=ticks, cex=cex.names, pos=2, 
		offset=.5)
      mtext(ylabel, line=2, side=2, at=max(ticks)/2, cex=cex.label, adj=.5)

      ## if length of sub is greater than 4, create 2 columns in legend
      ncol <- ifelse (length(sub.colors) > 4, 2, 1)

      # make legend
      legend(  x=legend.x,
          y=legend.y,
          title=legend.title,
          bty=legend.bty, 
          bg=legend.bg,
          inset=legend.inset,
          xpd=legend.xpd,
          legend=names(sub.colors),
          fill=sub.colors,
          cex=legend.cex,
          ncol=ncol)
      if (!is.null(main))
        title(main=main, cex.main=cex.main)

    }

    if (horiz==TRUE) {
      op <- par(xpd=NA, cex=par("cex"), las=1, mar=mar, mgp=c(3,0.5,0))

      # check legend.x and legend.y
      if (is.null(legend.x) && is.null(legend.y))
        legend.x <- "bottomright"

      if (is.null(legend.fit))
        legend.fit <- TRUE
      if (!legend.x %in% c("topright","bottomright"))
        legend.fit <- FALSE

      if (legend.fit)
        legend.inset <- legend.inset + .04

      # make sure there is space for legend
      plot(c(0,1),c(0,1),type="n",bty="n",xaxt="n",yaxt="n",xlab="",ylab="",xaxs="i",yaxs="i")
      LEGEND <- legend(  x=legend.x,
              y=legend.y,
              title=legend.title,
              bty=legend.bty, 
              bg=legend.bg,
              inset=legend.inset,
              xpd=legend.xpd,
              legend=names(sub.colors),
              fill=sub.colors,
              cex=legend.cex,
              ncol=1,
              plot=FALSE)

      BP <- barplot(mat.type, col=sub.colors, xaxt="n", horiz=horiz, cex.names=cex.names, 
          plot=FALSE,...)

      if (legend.fit) {
        # check top/bottom clearance
        if(legend.x == "bottomright"){
          LEG.top <- (LEGEND$rect$top) * (max(BP)+min(BP))
          intheway <- (LEG.top>BP-0.5 * (BP[2]-BP[1])) }
        if(legend.x == "topright"){
          LEG.bottom <- (LEGEND$rect$top-LEGEND$rect$h) * (max(BP)+min(BP))
          intheway <- (LEG.bottom<BP+0.5 * (BP[2]-BP[1]))}

        # reverse order of all.max
        #rev.all.max < -all.max[length(all.max):1]
        #legendside.max <- max(rev.all.max[intheway],na.rm=TRUE)

        legendside.max <- max(all.max[intheway],na.rm=TRUE)


        # check left clearance
        LEG.left <- LEGEND$rect$left - 0.06
        legendside.max <- legendside.max/LEG.left
      }

      # calculate lengthwise axis

      if (is.null(bar.lim)) {
        bar.max <- max(all.max, na.rm = TRUE)
      } else {
        bar.max <- max(bar.lim)
      }

      if (percent) {
        bar.max <- 100
        ticks <- (0:5)*20
      } else {
        if (trunc(bar.max) > 100) {
          ticks <- pretty(c(0,bar.max), n=5)
          bar.max <- max(ticks)
        } else {
          if (trunc(bar.max) == 100) {
            bar.max <- 100
            ticks <- (0:5)*20
          } else {
            if (bar.max >= 80) {
              bar.max <- ceiling(bar.max/20)*20
              ticks <- (0:(bar.max/20))*20
            } else {
              if (bar.max >= 30) {
                bar.max <- ceiling(bar.max/10)*10
                ticks <- (0:(bar.max/10))*10
              } else {
                bar.max <- ceiling(bar.max/5)*5
                ticks <- (0:(bar.max/5))*5}}}}
      }

      if (is.null(bar.lim)) {
        if (legend.fit) {
          bar.lim <- c(0, max(bar.max,legendside.max)/bar.ratio)
        } else {
          bar.lim <- c(0, bar.max/bar.ratio)}
      } else {
        bar.lim <- bar.lim/bar.ratio}

      par(new=TRUE)
      bp <- barplot(mat.type, col=sub.colors, xlim=bar.lim, xaxt="n", cex.names=cex.names, 
          horiz=horiz, ...)
      bp.max <- max(bp) + min(bp)

      ## y axis
      lines(c(0,bar.max),c(0,0))
      for (j in ticks) {
        lines(c(j,j), c(-bp.max/80,0))}
      text(y=rep(0,length(ticks)), x=ticks, labels=ticks, cex=cex.names, pos=1, offset=.5)
      mtext(ylabel, line=1.25, side=1, at=max(ticks)/2, cex=cex.label, adj=.5)

      # make legend
      legend(  x=legend.x,
          y=legend.y,
          title=legend.title,
          bty=legend.bty, 
          bg=legend.bg,
          inset=legend.inset,
          xpd=legend.xpd,
          legend=names(sub.colors),
          fill=sub.colors,
          #legend=rev(names(sub.colors)),
          #fill=rev(sub.colors),
          cex=legend.cex,
          ncol=1)
      }
      if (!is.null(main))
        title(main=main, cex.main=cex.main)

    ## x label
    if (!is.null(xlabel))  
      mtext(xlabel, side=xside, line=xlinenum, cex=cex.label, las=xlasnum) 

    par(op)

    if(savedata & device.type[i]!="default"){

      message("###################################\n", 
			"Stacked barplot written to: ", OUTPUTfn, 
		"\n###################################")
    dev.off()}
  }
}



