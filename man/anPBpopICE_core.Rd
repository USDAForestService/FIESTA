\name{anPBpopICE_core}
\alias{anPBpopICE_core}
\title{
	ANALYSIS - Generate core tables for ICE.
}
\description{
	Generates a set of core tables based on area of interest.
}
\usage{
anPBpopICE_core(ice.pntfn, ice.plotfn = NULL, AOInm, T1, T2, 
	plotid = "plot_id", pntid = "dot_cnt", pltassgn = NULL, pltassgnid = NULL, 
	unitvar = NULL, unitvar2 = NULL, unitarea = NULL, areavar = "ACRES", 
	unit.action = "keep", strata = FALSE, stratalut = NULL, strvar = NULL, 
	getwt = TRUE, getwtvar = "P1POINTCNT", stratcombine = TRUE, 
	tabtype = "PCT", sumunits = FALSE, rawdata = TRUE, returntitle = TRUE, 
	outfolder = NULL, outfn.pre = NULL, outfn.date = TRUE, overwrite = FALSE, 
	pnt1 = FALSE, domlut = NULL, PBpopdatICE = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ice.pntfn}{ Comma-delimited file (*.csv). The full path file name of 
	ICE point-level data. }
  \item{ice.plotfn}{ Comma-delimited file (*.csv). The full path file name of 
	ICE plot-level data (optional). }
  \item{AOInm}{ String. The name of the area of interest (i.e., population)
	for titles and output names. }
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
  \item{unit.action}{ String. What to do if number of plots in an estimation unit 
	is less than minplotnum.unit ('keep', 'remove' 'combine'). If unit.action='combine',
	combines estimation unit to the following estimation unit in unitlut. }
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
  \item{tabtype}{ String. Type of units for the table ("AREA", "PCT"). }
  \item{sumunits}{ Logical. If TRUE, estimation units are summed and returned in
	one table. }
  \item{rawdata}{ Logical. If TRUE, returns a list of raw data tables that are used
	for estimation (See Value). If savedata = TRUE, this table is also written to file. }
  \item{returntitle}{ Logical. If TRUE, returns title information for output tables. }
  \item{outfolder}{ String.  The outfolder to write files to. }
  \item{outfn.pre}{ String. A short name used for output file names. If NULL, 
	the AOInm is used. }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile (e.g., outfn_'date'.csv). }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
  \item{pnt1}{ Logical. If TRUE, use only center point when no change observed. }
  \item{domlut}{ Data frame. Lookup table of domain categories and titles for estimation.
	If NULL, default from FIESTA::PBpopdatICE(). }
  \item{PBpopdatICE}{ List object. Population data output from FIESTA::PBpopdatICE(). }
  \item{...}{ Other parameters to FIESTA::anPBestICE_data() function. }
}
\details{
	If variables are NULL, then it will prompt user to input variables.
		
}
\value{
  The following tables are saved to a folder named 'estimates' in the outfolder.
  If rawdata=TRUE, the rawdata is saved to a folder named 'rawdata' in the 
	estimates folder.

  \item{table01}{ Estimates of change_pnt on all lands. }
  \item{table02}{ Estimates of chg_ag_2_GRP on all lands. }
  \item{table03}{ Estimates of land use at Time 1 on all lands. }
  \item{table04}{ Estimates of land use at Time 2 on all lands. }
  \item{table05}{ Estimates of land cover at Time 1 all lands. }
  \item{table06}{ Estimates of land cover at Time 2 on all lands. }
  \item{table07}{ Percent of forest land use change. }
  \item{table08}{ Percent of land cover change where land use was forest at Time 1. }
  \item{table09}{ Percent of land use change from Time 1 to Time 2. }
  \item{table10}{ Percent of land cover change from Time 1 to Time 2. }
  \item{table11}{ Percent of land cover by land use at Time 1. }
  \item{table12}{ Percent of land cover by land use at Time 2. }
  \item{table13}{ Percent change of land use at Time 1 within land use at Time 2. }
  \item{table13a}{ Percent change of land use at Time 1 within land cover at Time 2. }
  \item{table14}{ Percent change of land cover at Time 1 within land use at Time 2. }
  \item{table15}{ Percent change of land use at Time 1 within within change agent group. }
  \item{table16}{ Percent change of land cover at Time 1 within change agent group. }
  \item{table17}{ Percent change of Land Use from Time 1 to Time 2. }
  \item{table18}{ Percent change of Land Cover from Time 1 to Time 2. }
  \item{table19}{ Percent change of Land Use (Forest/Nonforest) from Time 1 to Time 2. }
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
