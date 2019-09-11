\name{datBarplot}
\alias{datBarplot}
\title{
	Data - Generates frequency barplot.
}
\description{
	Generate a barplot of from a frequency data frame.
}
\usage{
datBarplot(x, xvar = NULL, yvar = "FREQ", grpvar = NULL, errbars = FALSE, 
	x.order = NULL, sevar = NULL, psevar = NULL, device.type = "dev.new", 
	jpeg.res = 300, device.height = 5, device.width = 8, horiz = FALSE, 
	toplabels = NULL, ylim = NULL, yscale = NULL, ylabel = NULL, xlabel = NULL, 
	mar = NULL, addlegend = FALSE, main = NULL, cex.main = 1, cex.label = 1, 
	cex.names = 0.8, las.xnames = 0, savedata = FALSE, outfolder = NULL, 
	outfn = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Data frame or comma-delimited file (*.csv) - a frequency table. }
  \item{xvar}{ String. Name of X variable. }
  \item{yvar}{ String. Name of the y variable (e.g., FREQ). }
  \item{grpvar}{ String. Name of the variable for grouping. }
  \item{errbars}{ Logical. If TRUE, error bars are added to bar plot (sevar or psevar must 
	also be populated). }
  \item{x.order}{ String or Vector. Define order of xvar: descending ("DESC") or 
	ascending ("ASC"). Use vector for more than one xvar. }
  \item{sevar}{ String. Name of the variable with standard error values. }
  \item{psevar}{ String. Name of the variable with percent standard error. }
  \item{device.type}{ String. Type(s) of device for plotting ("dev.new", "jpg", "pdf", or "ps"). }
  \item{jpeg.res}{ Integer. Resolution for jpeg image. }
  \item{device.height}{ Integer. Height (in inches) of barplot, if writing to file. }
  \item{device.width}{ Integer. Width (in inches) of barplot, if writing to file. }
  \item{horiz}{ Logical. If TRUE, bars are drawn horizontally with first bar at the bottom. 
	If FALSE, bars are drawn vertically with first bar to the left (barplot parameter).  }
  \item{toplabels}{ String vector. A vector of labels to place above each bar. }
  \item{ylim}{ Number. A vector of min and max values, c(min,max) for the y axis (or x axis 
	if horiz=TRUE). If NULL, defaults to maximum y value. If errbars=TRUE, the ylim defaults 
	to the maximum y value plus the standard error. }
  \item{yscale}{ String. The amount to rescale data values ('hundred', 'thousand', 'million'). 
	The respective label will be added to the ylabel. }
  \item{ylabel}{ String. Label for the y axis (same as ylab). }
  \item{xlabel}{ String. Label for the x axis (same as xlab). }
  \item{mar}{ See par.. A numerical vector representing number of lines for margins 
	(c(bottom, left, top, right). }
  \item{addlegend}{ Logical. If TRUE, adds legend to bar plot (only applicable if grouping). }
  \item{main}{ String. Title for plot. }
  \item{cex.main}{ Number. Expansion factor for title. } 
  \item{cex.label}{ Number. A number representing cex in barplot (size expansion of x and/or ylabels. }
  \item{cex.names}{ Number. Expansion factor for axis names (bar labels). Ex. 0.5 represents 
	half the size. } 
  \item{las.xnames}{ Number. The direction of x variable names (0,1,3). 0:diagonal (Default), 
	1:horizontal; 3:vertical. } 
  \item{savedata}{ Logical. If TRUE, writes output data to outfolder (jpg and pdf). }
  \item{outfolder}{ String. The name of the output folder. If savedata=TRUE, all output 
	saved to the outfolder. If savedata=FALSE, only a text file of input parameters is saved. }
  \item{outfn}{ String. The name of the output file if savedata=TRUE (*.csv). Do not 
	include extension. If NULL, the file will be named BARPLOT_'yvar_date'.csv }
  \item{...}{ list of additional arguments to pass to barplot(); names of the list are used 
	as argument names. }

}
\details{
	If parameters = NULL, then it will prompt user for input.
}
\value{
	Outputs barplot to display window.
}
\author{
	Tracey S. Frescino
}
\note{
 If savedata = TRUE, writes a jpg and pdf of barplot to outfolder.

 To add legend parameters, add a parameter named args.legend, defined as a list of specific 
	legend parameters (see ?legend)...  ex. .., args.legend=list(x="topright"). If specifying 
	x and y, x defines the lower right corner of legend box and y defines the upper right 
	corner of box. 
}
\examples{
	#ftyptab <- data.frame(cbind(FORTYPCD=c(182,184,201,221,265), FREQ=c(110,7,900,410,155)))
	#datBarplot(x=ftyptab, xvar="FORTYPCD")
}

\keyword{ data }
