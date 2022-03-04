#' Data - Generates barplot of photo-based change estimates.
#' 
#' Generate a bar plot of net change for photo-based estimates of land use /
#' land cover change.
#' 
#' 
#' @param gainloss Data frame or comma-delimited file (*.csv) - table with gain
#' loss estimates.
#' @param CI Number. Confidence Interval to include on plot.
#' @param figTitle String. Title of figure.
#' @return Outputs barplot to display window.
#' @note If savedata = TRUE, writes a jpg and pdf of barplot to outfolder.
#' @author Tracey S. Frescino
#' @keywords data
#' @export datPBplotchg
datPBplotchg <- function(gainloss, CI=95, figTitle="") {
  ## DESCRIPTION: Generates a plot with gain and loss estimates and confidence intervals.
  ## --> function to plot net changes in classes for LU and LC
  ##     Input(s): type of data (LU = Land Use, LC = Land Class), 
  ##               full path to and filename for input csv, figure title
  ##     Output(s): bar chart, with net change (gain, loss, no change/0%) for each 
  ##                LU or LC class, with uncertainty info (diff.CI95left,right)
  ## Code originated from Stacie Bender - Geospatial Specialist at Forest Service, 
  ##		Geospatial Technology and Applications Center (GTAC), Salt Lake City, Utah

  ## Define functions
  ############################################################################
  # --> to specify the number of decimal places:
  #     * Input(s): number to round, number of decimal places you want in the num
  #     * Output(s): the number, w/ specificed n decimal places
  setDecPlaces <- function(x, k) format(round(x, k), nsmall=k) # x = number, k = num decimal places

  # --> to get num "pretty" decimal places that would make a "pretty"-formatted number
  #     (basically trailing zeroes trimmed)
  #     * Input(s): a number!
  #     * Output(s): number of decimal places that would make the number "pretty"

  mar <-  graphics::par("mar")
  xpd <-  graphics::par("xpd")
  on.exit(graphics::par(mar=mar, xpd=xpd))

  getNPrettyDecPlaces <- function(x) {
    nPrettyDecPlaces <- 0 # default = whole numbers
    x <- as.character(abs(as.numeric(x)))
    fields <- unlist(strsplit(x,".",fixed=TRUE)) # trims trailing zeroes by default?
  
    if (length(fields) > 1) # decimal point was specified; get n decimal places to make num pretty
      nPrettyDecPlaces <- nchar(fields[2]) # wholeNum check 
  
    return(nPrettyDecPlaces)    
  } # end function getNPrettyDecPlaces

  tabInfo <- pcheck.table(gainloss, returnDT=FALSE)

  Classes <- row.names(tabInfo)
  nClasses <- length(Classes)

  # file structure change on 4/9/2018 in estimate files - code around these
  category <- unlist(strsplit(tabInfo$gain.val,split=" to "))
  category <- category[seq(2,length(category),by=2)] # get only even elements on the right hand side of the "to" equation

  tabInfo <- cbind(category, tabInfo)

  if (!CI %in% c(67, 95, 99)) stop("CI must be either 67, 95, or 99")
  
  # set up variables that will be used to make the plot
  barData <- as.numeric(nClasses)
  barUncData <- as.numeric(nClasses)
  barColors <- as.character(nClasses)
  barLabels <- as.character(nClasses)
  classLabCol <- rep("black",nClasses)
  
  # populate barData, barColors, barLabels
  for (Idx in 1:nClasses) {
    
    if (nrow(tabInfo[row.names(tabInfo) == Classes[Idx],]) == 0) {
      barData[Idx] <- 0 # assume zero if not present in TF table
      barUncData[Idx] <- 0
      barLabels[Idx] <- ""
      classLabCol[Idx] <- "darkgray" # combo doesn't exist in this state
    } else {
      barData[Idx] <- tabInfo[row.names(tabInfo) == Classes[Idx], "diff.est"]
      barUncData[Idx] <- tabInfo[row.names(tabInfo) == Classes[Idx], paste0("diff.CI",CI,"left")]
      if (barData[Idx] > 0) 
        barUncData[Idx] <- tabInfo[row.names(tabInfo) == Classes[Idx], paste0("diff.CI",CI,"right")]
                       
      if (barData[Idx] == 0) {
        barLabels[Idx] <- paste0(setDecPlaces(barData[Idx],0),"%")  
      } else {
        barLabels[Idx] <- paste0(setDecPlaces(barData[Idx],3),"%")  
      }
    } # end if/check for non-existent data
          
    barColors[Idx] <- "red"
    if (barData[Idx] > 0) barColors[Idx] <- "blue"
     
  } # end for loop over ALL Classes

  # set up y-axis increments and ticks 
  ymin <- min(c(barData, barUncData), na.rm=TRUE)
  ymax <- max(c(barData, barUncData), na.rm=TRUE)
  magnitude <- max(c(abs(ymin), abs(ymax)))*1.20 # make room for bar labels

  yticks <- pretty(c((-1)*magnitude, magnitude))
  ymin <- yticks[1]
  ymax <- tail(yticks,n=1)

  # set up plot layout - in this case, 1 panel with room for title at top
  subpanel_coords <- matrix(NA, nrow=1, ncol=4) # nrow = nSubPanels
                                             # ncol = L,R,B,T
  minHorizSideSpace <- 0.05
  addlHorizSideSpace <- (14-nClasses) * minHorizSideSpace/2

  subpanel_coords[1,1] <- minHorizSideSpace + addlHorizSideSpace
  subpanel_coords[1,2] <- 1.0 - minHorizSideSpace - addlHorizSideSpace
  subpanel_coords[1,3] <- 0.03
  subpanel_coords[1,4] <- 0.80

  invisible(split.screen(subpanel_coords)) # invisible suppresses the "##[1] 1 2" output

  screen(1)
  par(mar=c(3,2,0,0)) # leave space for class names along xaxis and room for % labels 
                      # along yaxis

  # plot the gains and losses for each ICE LU or LC class in a bar graph
  barp <- barplot(barData, col=barColors, ylim=c(ymin,ymax), 
                 beside=TRUE, horiz=FALSE, space=0.25, axes=FALSE,
                 xlab="", ylab="", font=1,las=1, ann=FALSE, names.arg=NA)

  # add uncertainty data (95 CI)
  xcoords <- barp[,1]
  for (i in 1:length(xcoords)) {
    yLo <- min(c(barData[i], barUncData[i]))
    yHi <- max(c(barData[i], barUncData[i]))
    lines(x=rep(xcoords[i],2), y=c(yLo,yHi), col='darkgrey', lwd=4)
  }
  
  # add data values as labels at the end of bars.
  labPosn <- 1 # below bar by default
  for (i in 1:length(barData)) {
    if (barData[i] < 0) {
      labPosn <- 1 # label losses with % loss below the bar
    } else {
      labPosn <- 3 # label gains with %gain above the bar
    } # end check for zero/nonzero data value

    text(x=barp[i,1], y=barData[i], labels=barLabels[i], xpd=NA, cex=0.65, pos=labPosn)
  }

  # custom labels and positioning for x-axis
  xcoords <- barp[,1]

  par(xpd=NA) # allow plotting/annotation outside figure region
  prettyClassLabs <- Classes

#  y <- ymin-(ymax-ymin)*0.23
  y <- ymin - 2.5
  text(x=(xcoords-.5),y=y,cex=0.75,srt=45,labels=prettyClassLabs,col=classLabCol)

  # custom y-axis labels, ticks and overall axis title
  # --> determine max num of decimal places to display
  maxNDecPlaces <- 0
  for (ytickIdx in 1:length(yticks)) {
    currN <- getNPrettyDecPlaces(yticks[ytickIdx])
    if (currN > maxNDecPlaces) maxNDecPlaces <- currN
  } # end for loop over yticks

  par(xpd=NA) # allow plotting/annotation outside figure region
  axis(2,at=yticks,labels=paste0(setDecPlaces(yticks,maxNDecPlaces),"%"),
       las=2,cex.axis=0.9,tck=-0.050,mgp=c(3,0.5,0))

  # add reference lines to plot
  par(xpd=FALSE) # restrict plotting to figure region; otherwise ref. lines will go outside fig region.
  abline(h=yticks,lty="dotted",col="gray")
  abline(h=0,lty="solid",col="black")

  # add barplot a 2nd time, so as to have bars covering up reference lines (cleaner look)
  barp = barplot(barData,col = barColors,ylim=c(ymin,ymax), 
                 beside=TRUE,horiz=FALSE,space=0.25,axes=FALSE,
                 xlab="",ylab="",font=1,las=1,ann=FALSE,names.arg=NA,add=TRUE)

  # add legend to explain that gray line = 95% confidence interval
  par(xpd=NA) # allow drawing outside of box plot area
#  legend("topright",
#                    legend=c("95% Confidence Interval"),
#                    col=c("darkgray"),
#                    lty=c("solid"),lwd=3,
#                    bty='n',text.col="black",cex=0.8,horiz=TRUE,
#                    inset=c(-0.00,-0.15))

#  legend("topleft",
#                    legend=c("Gain","Loss"),
#                    col=c("blue","red"),
#                    pch=c(15,15),
#                    pt.cex=1.5,x.intersp=1.0,
#                    bty='n',text.col="black",cex=0.8,horiz=TRUE,
#                    inset=c(0,-0.15))

#  legend("topright",
#                    legend=c("Gain","Loss"),
#                    col=c("blue", "red"),
#                    pch=c(15,15),
#                    pt.cex=1.5,x.intersp=1.0,
#                    bty='n',text.col="black",cex=0.8,horiz=FALSE,
#                    inset=c(0,-0.18))

  legend("bottomright",
                    legend=c("Gain","Loss"),
                    col=c("blue", "red"),
                    pch=c(15,15),
                    pt.cex=1.5,x.intersp=1.0,
                    bty='n',text.col="black",cex=0.8,horiz=FALSE,
                    inset=c(0,0.0))

  legend("topright",
                    legend=c("95% Confidence Interval"),
                    col=c("darkgray"),
                    lty=c("solid"),lwd=3,
                    bty='n',text.col="black",cex=0.8,horiz=TRUE,
                    inset=c(-0.00,0.00))


        
  box()

  # flush out graphics
  close.screen(all.screens=TRUE)

  # add overall figure title
  title(main=figTitle,line=3,cex.main=1.2)
} # end function plotNetChangesByClass
