\name{anPBpopICE}
\alias{anPBpopICE}
\title{
	ANALYSIS - Generate population data for ICE.
}
\description{
	Generates a set of population data for area of interest.
}
\usage{
anPBpopICE(ice.pntfn=NULL, ice.pltfn = NULL, T1, T2, plotid = "plot_id", 
	pntid = "dot_cnt", pltassgn = NULL, pltassgnid = NULL, 
	unitvar = NULL, unitvar2 = NULL, unitarea = NULL, areavar = "ACRES", 
	unitcombine = FALSE, strata = FALSE, stratalut = NULL, strvar = NULL, 
	getwt = TRUE, getwtvar = "P1POINTCNT", stratcombine = TRUE, 
	sumunits = FALSE, saveobj = FALSE, savedata = FALSE, 
	outfolder = NULL, overwrite=FALSE, PBdataICE=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ice.pntfn}{ Comma-delimited file (*.csv). The full path file name of 
	ICE point-level data. }
  \item{ice.pltfn}{ Comma-delimited file (*.csv). The full path file name of 
	ICE plot-level data (optional). }
  \item{T1}{ Integer. Year of Time 1 imagery (YYYY). }
  \item{T2}{ Interger. Year of Time 2 imagery (YYYY). }
  \item{plotid}{ String. Name of unique identifier for ice45 plots. }
  \item{pntid}{ String. Name of unique identifier for the points. }
  \item{pltassgn}{ DF/DT or comma-delimited file (*.csv). Plot-level table with
	plot assignments for strata (optional). }
  \item{pltassgnid}{ String. Name of variable for unique identifier of pltassgn. }
  \item{unitvar}{ String. Name of the estimation unit variable in cond or pltassgn 
	with estimation unit assignment for each plot (e.g., 'ESTN_UNIT'). 
	If one estimation unit, set unitvar=NULL. }
  \item{unitvar2}{ String. Name of a second estimation unit variable in cond or 
	pltassgn with assignment for each plot (e.g., 'STATECD'). }
  \item{unitarea}{ Numeric or DF. Total area by estimation unit. If only 1 estimation 
	unit, include number of total acreage for the area of interest or a data frame
	with areavar. If more than one estimation unit, provide a data frame of total 
	area by estimation unit, including unitvar and areavar. }
  \item{areavar}{ String. Name of area variable in unitarea. Default="ACRES". }
  \item{unitcombine}{ Logical. If TRUE, automatically combines estimation units if less 
	than 2 plots in any one estimation unit. See notes for more info. }
  \item{strata}{ Logical. If TRUE, applies post-stratification to estimation. }
  \item{stratalut}{ DF/DT. Look-up table with strata weights (proportions) or area by 
	strata (estimation unit). If no strata weights, set getwt=TRUE to calculate.
	If strata=FALSE, include total area by estimation unit in unitarea -
	a stratalut will be generated with one strata (ONESTRAT) and strwt = 1. }
  \item{strvar}{ String. Name of the strata variable in stratalut and pnt or plt. 
	The default="STRATA". }
  \item{getwt}{ Logical. If TRUE, calculates strata weights from stratatlut areavar. }
  \item{getwtvar}{ String. Name of variable in stratalut to calculate weights (strwt).
 	Default="P1POINTCNT". }
  \item{stratcombine}{ Logical. If TRUE, automatically combines estimation units if less 
	than 2 plots in any one estimation unit. See notes for more info. }
  \item{sumunits}{ Logical. If TRUE, sums Estimation unit results. (Note: Need to 
	include area by estimation unit). }
  \item{saveobj}{ Logical. If TRUE, saves object to outfolder. }
  \item{savedata}{ Logical. If TRUE, saves data tables to outfolder. }
  \item{outfolder}{ String. The outfolder to write files to. }
  \item{overwrite}{ Logical. If TRUE, overwrites files. }
  \item{PBdataICE}{ R Object. Output data object from FIESTA::anPBpopICE_data() function. }
  \item{...}{ Other parameters to FIESTA::datExportData() function. }
}
\details{
	If variables are NULL, then it will prompt user to input variables.
		
}
\value{
  List object of population data for ICE estimation.

  \item{PBx}{ Photo-based interpretation data for estimation. }
  \item{pltassgnx}{ Data frame. Plot-level data with plot assignments of 
		Estimation unit as stratum value. }
  \item{plotid}{ String. Unique identifier of plot. }
  \item{pntid}{ String. Unique identifier of points. }
  \item{pltassgnid}{ String. Unique identifier of plot in pltassgn. }
  \item{tabtype}{ String. Table type ('PCT', 'AREA').  }
  \item{sumunits}{ Logical. If TRUE, sums estimation unit results. }
  \item{unitvar}{ String. Name of variable defining estimation units. }
  \item{strlut}{ Data frame. Strata-level information (e.g., pixel counts) 
		by estimation unit. }
  \item{strvar}{ String. Name of variable defining strata. }
  \item{plotsampcnt}{ NULL. }
  \item{getprop}{ Logical. If TRUE, gets proportions from PBx data. }
  \item{reflst}{ List object of reference tables for ICE (changelut, coverlut,
		uselut, agentlut, use_FORlut). }
  \item{domlut}{ Domain reference for table output. }
}
\author{
	Tracey S. Frescino
}
\references{
Frescino, Tracey S.; Moisen, Gretchen G.; Megown, Kevin A.; Nelson, Val J.; Freeman, 
Elizabeth A.; Patterson, Paul L.; Finco, Mark; Brewer, Ken; Menlove, James 2009. 
Nevada Photo-Based Inventory Pilot (NPIP) photo sampling procedures. Gen. Tech. Rep. 
RMRS-GTR-222. Fort Collins, CO: U.S. Department of Agriculture, Forest Service, 
Rocky Mountain Research Station. 30 p.

Patterson, Paul L. 2012. Photo-based estimators for the Nevada photo-based inventory. 
Res. Pap. RMRS-RP-92. Fort Collins, CO: U.S. Department of Agriculture, Forest Service, 
Rocky Mountain Research Station. 14 p.

}


\keyword{ data }
