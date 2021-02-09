\name{anGBest_custom}
\alias{anGBest_custom}
\title{
	Analysis - Generate estimates for custom-defined boundary.
}
\description{
	Wrapper to get plot data, stratification data and estimates within
	a custom-defined boundary.	
}
\usage{
anGBest_custom(GBpopdat, esttype = "TREE", estseed = "none", landarea = "FOREST", 
	pfilter = NULL, cfilter = NULL, estvar = NULL, estvar.filter = NULL, 
	rowvar = NULL, colvar = NULL, treedia.brks = c(0,5,10,20,50,100), sumunits=TRUE,
	divideby = NULL, title.ref = NULL, title.main = NULL, getbarplot = FALSE, 
	barplot.row=TRUE, barplot.ord=NULL, barplot.color=NULL, barplot.ylim = NULL, 
	barplot.nplt = FALSE, savedata = FALSE, outfolder = NULL, outfn.pre = NULL, 
	outfn.date = FALSE, overwrite = TRUE, ...)
}
\arguments{
  \item{GBpopdat}{ R list object. FIA population data and calculations for estimation. }
  \item{esttype}{ String. Type of estimation ("AREA", "TREE", "RATIO"). }
  \item{estseed}{ String. What to do with seedling information ("none", "only", "add"). }
  \item{landarea}{ String. The sample area filter for estimates ('ALL', 'FOREST', 'TIMBERLAND'). 
	If landarea=FOREST, filtered to COND_STATUS_CD = 1; If landarea=TIMBERLAND, filtered 
	to SITECLCD in(1:6) and RESERVCD = 0. }
  \item{pfilter}{ String A filter for plot-level data (e.g., "INVYR == 2012"). }
  \item{cfilter}{ String. A filter for condition-level data (e.g., "FORTYPCD == 180"). 
	Must be R syntax. }
  \item{estvar}{ String. Name of the tree estimate variable. }
  \item{estvar.filter}{ String. A tree filter for the estimate variable. Must be R syntax 
	(e.g,. "STATUSCD == 1"). }
  \item{rowvar}{ String. Name of row domain variable in cond or tree. If rowvar=NULL,
	only total estimates are output. If rowvar="DIACL", treedia.brks are used
	for class breaks. }
  \item{colvar}{ String. Name of column domain variable in cond or tree. If colvar="DIACL",
 	treedia.brks are used for class breaks. }
  \item{treedia.brks}{ Integer vector. Vector of breaks to define diameter classes 
	(e.g., c(0,5,10,20,50,100). Define rowvar or colvar = "DIACL". }
  \item{sumunits}{ Logical. If TRUE, estimation units are summed and returned in
	one table. }
  \item{divideby}{ String. Conversion number for output ('hundred', 'thousand', 'million'). }
  \item{title.ref}{ String. Table title for reference. }
  \item{title.main}{ String. Main title for table and barplot. }
  \item{getbarplot}{ Logical. If TRUE, generates a barplot of table rows or columns. }
  \item{barplot.row}{ Logical. If TRUE, generate barplot based on table rows. If FALSE,
	columns are used. }
  \item{barplot.ord}{ String. The order of x based on y values ('DESC', "ASC') for 
	barplot. If NULL, the default order of the table is used. }
  \item{barplot.color}{ String. The base color palette for barplot 
	('rainbow', 'heat', 'terrain', 'topo', 'cm', 'hcl1', 'hcl2', 'BrewerDark2',
	'BrewerPaired', 'BrewerBlues'). Note: All colors beginning with 'Brewer' 
	are color palettes from the RColorBrewer package. }
  \item{barplot.ylim}{ Number. A vector of min and max values, c(min,max) for the 
	barplot y axis. If NULL, defaults to maximum y value plus the standard error. }
  \item{barplot.nplt}{ Logical. If TRUE, Adds number of plots on top of bar. }
  \item{savedata}{ Logical. If TRUE, saves data objects to outfolder. }
  \item{outfolder}{ String. If savedata=TRUE or exportsp=TRUE, name of output folder. 
	If NULL, the working directory is used. }
  \item{outfn.pre}{ String. Add a prefix to output name (e.g., "01"). }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
  \item{...}{ Parameters for modGB*() functions. }
}
 
\value{
  Estimate.
}
\author{
	Tracey S. Frescino
}
\keyword{ data }

