\name{anGBestSppByYear}
\alias{anGBestSppByYear}
\title{
	ANALYSIS - Green-book estimates of species by inventory year.
}
\description{
	Generates table and barplot of species by inventory year.
}
\usage{
anGBestSppByYear(tree = NULL, cond = NULL, plt = NULL, tuniqueid = "PLT_CN", 
	puniqueid = "CN", getStrata = FALSE, unitarea = NULL, stratalut = NULL, 
	strvar = "STRATUMCD", areavar = "ACRES", state = NULL, invyrs = NULL, 
	sppvar = "SPCD", sppcd = NULL, pltcond.filter = NULL, estvar = NULL, 
	estvar.TPA = TRUE, estvar.filter = NULL, bplot = TRUE, savedata = FALSE, 
	outfolder = NULL, outfn = NULL, overwrite = FALSE, title.main = NULL, 
	title.estvar = NULL, title.spp = NULL)
}
\arguments{
  \item{tree}{ Data frame. The tree table with tree data to aggregate to condition-level. }
  \item{cond}{ Data frame or comma-delimited file(*.csv). The condition level table. }
  \item{plt}{ Data frame, comma-delimited file(*.csv). The plot level table. }
  \item{tuniqueid}{ String. Unique identifier for the tree table. }
  \item{puniqueid}{ String. Unique identifier for the plt table. }
  \item{getStrata}{ Logical. If TRUE, gets strata information from online zipfiles. }
  \item{unitarea}{ Numeric or DF. Total area by estimation unit. If only 1 estimation 
	unit, include number of total acreage for the area of interest or a data frame
	with areavar. If more than one estimation unit, provide a data frame of total 
	area by estimation unit, including unitvar and areavar. }
  \item{stratalut}{ DF/DT. If strata=TRUE, look-up table with strata 
	proportions ('strwt') by strata (and estimation unit). To calculate 
	'strwt', set getwt=TRUE and getwtvar= name of variable with information 
	to calculate weights from (e.g., pixel counts) }.
  \item{strvar}{ String. Name of the strata variable in strlut and plt (or cond). }
  \item{areavar}{ String. Name of the area variable in strlut. }
  \item{state}{ String vector. Name of state (Ex. c("Arizona","Colorado","Idaho",...). }
  \item{invyrs}{ Integer vector. Inventory year(s) (ex. c(2000, 2001, 2002)). }
  \item{sppvar}{ String. Name of the species variable ("SPCD", "SPGRPCD"). }
  \item{sppcd}{ String. Integer. The number code for desired species. }
  \item{pltcond.filter}{ String. A filter for plt or cond table. Must be R syntax. }
  \item{estvar}{ String. Name of the tree estimate variable. }
  \item{estvar.TPA}{ Logical. If TRUE, the estimate variable will be multiplied by trees 
  	per acre (TPA_UNADJ). }
  \item{estvar.filter}{ String. A filter for the estimate variable. Must be R syntax 
	(Ex. "STATUSCD == 1"). }
  \item{bplot}{ Logical. If TRUE, outputs a barplot of species estvar by inventory year. }
  \item{savedata}{ Logical. If TRUE, writes output data to outfolder (jpg and pdf). }
  \item{outfolder}{ String. Name of the output folder. If savedata=TRUE, all output 
	saved to the outfolder. If savedata=FALSE, only a text file of input parameters is saved. }
  \item{outfn}{ String. Name of the output file if savedata=TRUE (*.csv). Do not 
	include extension. If NULL, the file will be named BARPLOT_'yvar_date'.csv. }
  \item{overwrite}{ Logical. If TRUE, overwrite files in outfolder. }
  \item{title.main}{ String. TITLE: Title for table or bar plot. }
  \item{title.estvar}{ String. TITLE: The pretty name for the estimation variable used in title. 
  	If NULL, = estvar. }
  \item{title.spp}{ String. TITLE: The pretty name for spcd used in title. If NULL, = "Species". }
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
}

\keyword{ data }
