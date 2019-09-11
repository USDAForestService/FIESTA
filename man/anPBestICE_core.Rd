\name{anPBestICE_core}
\alias{anPBestICE_core}
\title{
	ANALYSIS - Generate core tables for ICE.
}
\description{
	Generates a set of core tables based on state of interest.
}
\usage{
anPBestICE_core(icedat, state, T1, T2, pltstrat = NULL, puniqueid = "CN", 
	plotid = "plot_id", pntid = "dot_id", tabtype = "PCT", sumunits = FALSE, 
	strata = FALSE, stratalut = NULL, unitvar = NULL, unitvar2 = NULL, 
	unitarea = NULL, strvar=NULL, getwt = TRUE, getwtvar = "P1POINTCNT", 
	autocombine=TRUE, lutfolder = "_luts", savedata = TRUE, returntitle=TRUE, 
	outfolder = NULL, outfn.date = TRUE, overwrite = FALSE, rawdata = TRUE, 
	pnt1 = FALSE, estname = NULL, stabbr = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{icedat}{ DF/DT or comma-delimited file (*.csv). The point-level 
	table (may contain condition-level info). }
  \item{state}{ String. The name of state for area of interest (e.g., "Vermont", "Utah"). }
  \item{T1}{ Integer. Year of Time 1 imagery. }
  \item{T2}{ Interger. Year of Time 2 imagery. }
  \item{pltstrat}{ DF/DT or comma-delimited file (*.csv). Plot-level table. }
  \item{puniqueid}{ String. Name of variable for unique identifier of pltstrat. }
  \item{plotid}{ String. Name of unique identifier for ice45 plots. }
  \item{pntid}{ String. Name of unique identifier for the points. }
  \item{tabtype}{ String. Type of units for the table ("AREA", "PCT"). }
  \item{sumunits}{ Logical. If TRUE, estimation units are summed and returned in
	one table. }
  \item{strata}{ Logical. If TRUE, applies post-stratification to estimation. }
  \item{stratalut}{ DF/DT. Look-up table with strata weights (proportions) or area by 
	strata (estimation unit). If no strata weights, set getwt=TRUE to calculate.
	If strata=FALSE, include total area by estimation unit in unitarea -
	a stratalut will be generated with one strata (ONESTRAT) and strwt = 1. }
  \item{unitvar}{ String. Name of the estimation unit variable in cond or pltstrat 
	with estimation unit assignment for each plot (e.g., 'ESTN_UNIT'). 
	If one estimation unit, set unitvar=NULL. }
  \item{unitvar2}{ String. Name of a second estimation unit variable in cond or 
	pltstrat with assignment for each plot (e.g., 'STATECD'). }
  \item{unitarea}{ Numeric or DF. Total area by estimation unit. If only 1 estimation 
	unit, include number of total acreage for the area of interest or a data frame
	with areavar. If more than one estimation unit, provide a data frame of total 
	area by estimation unit, including unitvar and areavar. }
  \item{strvar}{ String. Name of the strata variable in stratalut and pnt or plt. 
	The default="STRATA". }
  \item{getwt}{ Logical. If TRUE, calculates strata weights from stratatlut areavar. }
  \item{getwtvar}{ String. Name of variable in stratalut to calculate weights (strwt).
 	Default="P1POINTCNT". }
  \item{autocombine}{ Logical. If TRUE, automatically combines estimation units if less 
	than 2 plots in any one estimation unit. See notes for more info. }
  \item{lutfolder}{ String. Folder name where LUT files are located. }
  \item{savedata}{ Logical. If TRUE, saves tables to outfolder. }
  \item{returntitle}{ Logical. If TRUE, returns title(s). }
  \item{outfolder}{ String.  The outfolder to write files to. }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile (e.g., outfn_'date'.csv). }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
  \item{rawdata}{ Logical. If TRUE, returns a list of raw data tables that are used
	for estimation (See Value). If savedata = TRUE, this table is also written to file. }
  \item{pnt1}{ Logical. If TRUE, use only center point when no change observed. }
  \item{estname}{ String. Tag name of type of estimation to append to file names and titles. }
  \item{stabbr}{ String. If state not in ref_statecd, add an abbreviation for file names. }
}
\details{
	If variables are NULL, then it will prompt user to input variables.
		
}
\value{
  If FIA=TRUE or unitvar=NULL and/or colvar=NULL, 1 table will be output including 
	estimations and percent sampling errors. Otherwise the following 2 tables will be 
	output as a list.

  \item{est}{ DF. Acre estimates by domain (and estimation unit).  }
  \item{pse}{ DF. Percent sampling error for estimates by unit and domain. }
  \item{est.raw}{ List. If rawdata=TRUE, a list of raw data tables is returned. }
  
  If savedata=TRUE, area table(s) are written to outfolder.
}
\author{
	Tracey S. Frescino, Paul L. Patterson, Elizabeth A. Freeman
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
