\name{anGBest_batch}
\alias{anGBest_batch}
\title{
	Analysis - Get estimates for Green-Book module.
}
\description{
	Wrapper to generate estimates for each evalid in list for one or 
	more estimation variables and one or more estimation variable filters.
}
\usage{
anGBest_batch(GBpopdatlst = NULL, esttype = "AREA", estseed = "none", 
	landarea = "FOREST", pcfilter = NULL, estvarlst = NULL, 
	estvar.filterlst = NULL, rowvar = NULL, colvar = NULL, sumunits = TRUE,
	savedata = FALSE, raw_dsn="estdat", raw_fmt="sqlite", outfolder = NULL,
	outfn.pre=NULL, outfn.date=FALSE, overwrite_dsn = FALSE, 
	overwrite_layer = TRUE, append_layer = FALSE, title.filterlst = NULL, ...)
}
\arguments{
  \item{GBpopdatlst}{ List of list. One or more population data objects returned 
	from FIESTA::modGBpop(), anGBpop_eval() or anGBpop_evalcustom(). }
  \item{esttype}{ String. Estimation type ('AREA', 'TREE', 'RATIO'). Only
	one estimation type is allowed at a time. }
  \item{estseed}{ String. Use seedling data only or add to tree data. Seedling 
	estimates are only for counts (estvar='TPA_UNADJ')-('none', 'only', 'add'). }
  \item{landarea}{ String. The condition-level filter for defining land area 
	('ALL', 'FOREST', 'TIMBERLAND'). If landarea='FOREST', COND_STATUS_CD = 1; 
	if landarea='TIMBERLAND', SITECLCD in(1:6) & RESERVCD = 0. }
  \item{pcfilter}{ String. A filter for plot or cond attributes (including pltassgn). 
	Must be R logical syntax. }
  \item{estvarlst}{ String vector. One or more tree-level estimate variable 
	(e.g., 'VOLCFNET'). }
  \item{estvar.filterlst}{ String vector. One or more tree-level filter for estvar. 
	Must be R syntax (e.g., 'STATUSCD == 1'). }
  \item{rowvar}{ String. Optional. Name of domain variable to group estvar by 
	for rows in table output. Rowvar must be included in an input data frame 
	(i.e., plt, cond, tree). If no rowvar is included, an estimate is returned
	for the total estimation unit. Include colvar for grouping by 2 variables. }
  \item{colvar}{ String. Optional. If rowvar != NULL, name of domain variable to 
	group estvar by for columns in table output. Colvar must be included in an 
	input data frame (i.e., plt, cond, tree). }
  \item{sumunits}{ Logical. If TRUE, sums estimation units to population. }
  \item{savedata}{ Logical. If TRUE, saves data to outfolder. }
  \item{raw_dsn}{ String. Data source name for rawdata output. If extension is not
	included, out_fmt is used. Use full path if outfolder=NULL. }
  \item{raw_fmt}{ String. File format for rawdata output 
	('csv', 'sqlite', 'db', 'sqlite3', 'db3', 'gpkg', 'gdb'). If out_fmt='gdb', 
	must have ArcGIS license and install arcgisbinding package. }
  \item{outfolder}{ String. The output folder path. If NULL and savedata=TRUE, 
	outfolder is the working directory. }
  \item{outfn.pre}{ String. The name used for prefix of outfiles (e.g., outfn.pre'_plt*'). }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile (e.g., outfn_'date'.csv). }
  \item{overwrite_dsn}{ Logical. If TRUE, overwrites out_dsn. }
  \item{overwrite_layer}{ Logical. If TRUE, overwrites layers in out_dsn
	or files if out_fmt = 'csv'. }
  \item{append_layer}{ Logical. If TRUE, appends layers to existing out_dsn
	or files if out_fmt = 'csv'. Note: currently cannot append layers if
	out_fmt = "gdb". }
  \item{title.filterlst}{ String vector. A title corresponding to estvar.filterlst. }
  \item{...}{ If GBpopdatlst=NULL, parameters for FIESTA::anGBest_eval(). }
}
 
\value{
  The output area organized by estimation type and estimation variable.\cr
 
  If savedata=TRUE, data are export to specified format (out_fmt).
}
\examples{

  ## Get population data for 
  GBpoplst <- anGBpop_eval(evalEndyrlst=c(2015:2019), states=c("AZ","NM"), 
	evalType="VOL")

  ## Get area estimates by forest type and stand-size class
  GBestarea <- anGBest_batch(GBpopdatlst=GBpoplst, esttype="AREA", 
	rowvar="FORTYPCD", colvar="STDSZCD", savedata=FALSE)
  names(GBestarea)
  names(GBestarea$AREA)

  ## Look at area estimates for totals, row totals, and groups 
  GBestarea$AREA$totest
  head(GBestarea$AREA$rowest)
  head(GBestarea$AREA$grpest)


  ## Get tree estimates for volume, basal area, number of trees, biomass and carbon
  ##    for live trees and standing dead trees by forest type
  GBesttree <- anGBest_batch(GBpopdatlst=GBpoplst, esttype="TREE", 
	estvarlst=c("VOLCFNET", "BA", "TPA_UNADJ", "DRYBIO_AG", "CARBON_AG"),
	estvar.filterlst=c("STATUSCD==1", "STATUSCD == 2 & STANDING_DEAD_CD == 1 & DIA >= 5.0"),
	rowvar="FORTYPCD",
	savedata=FALSE, title.filterlst=c("live", "standing_dead"))
  names(GBest)
  names(GBest$VOLCFNET_live_FOREST)

  ## Look at tree estimates for totals and row totals for live cuft volume on forest land
  GBest$VOLCFNET_live_FOREST$totest
  GBest$VOLCFNET_live_FOREST$rowest
}
\author{
	Tracey S. Frescino
}
\keyword{ data }

