\name{anMOD_barplot}
\alias{anMOD_barplot}
\title{
	Analysis - Generate estimates for custom-defined boundary.
}
\description{
	Wrapper to get plot data, stratification data and estimates within
	a custom-defined boundary.	
}
\usage{
anMOD_barplot(MODest, barplot.row=TRUE, barplot.ord="DESC", barplot.color=NULL, 
	barplot.ylim = NULL, barplot.nplt = FALSE, savedata = FALSE, 
	outfolder = NULL, outfn.pre = NULL, outfn.date = FALSE, overwrite = FALSE, 
	title.ref = NULL, title.main = NULL, divideby = NULL) 
}
\arguments{
  \item{MODest}{ R list object. FIA estimation data from mod*est() functions. }
  \item{barplot.row}{ Logical. If TRUE, generate barplot based on table rows. If FALSE,
	columns are used. }
  \item{barplot.ord}{ String. The order of x based on y values ('DESC', "ASC') for 
	barplot. If NULL, the default order of the table is used. }
  \item{barplot.color}{ String. The base color palette for barplot 
	('rainbow', 'heat', 'terrain', 'topo', 'cm', 'hcl1', 'hcl2').  }
  \item{barplot.ylim}{ Number. A vector of min and max values, c(min,max) for the 
	barplot y axis. If NULL, defaults to maximum y value plus the standard error. }
  \item{barplot.nplt}{ Logical. If TRUE, Adds number of plots on top of bar. }
  \item{savedata}{ Logical. If TRUE, saves data objects to outfolder. }
  \item{outfolder}{ String. If savedata=TRUE or exportsp=TRUE, name of output folder. 
	If NULL, the working directory is used. }
  \item{outfn.pre}{ String. Add a prefix to output name (e.g., "01"). }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
  \item{title.ref}{ String. Table title for reference. }
  \item{title.main}{ String. Main title for table and barplot. }
  \item{divideby}{ String. Conversion number for output ('hundred', 'thousand', 'million'). }
}
 
\value{
  Estimate.
}
\author{
	Tracey S. Frescino
}
\keyword{ data }

