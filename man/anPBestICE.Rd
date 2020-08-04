\name{anPBestICE}
\alias{anPBestICE}
\title{
	ANALYSIS - Photo-based estimates for Image Change Estimation (ICE).
}
\description{
	Generates percent or acre estimates by domain (and estimation unit).
}
\usage{
anPBestICE(icedat, pltassgn = NULL, state, T1, T2, puniqueid = "CN", 
	plotid = "plot_id", pntid = "dot_id", tabtype = "PCT", sumunits = FALSE, 
	strata = FALSE, ratio = FALSE, landarea = "ALL", pnt.filter = NULL, 
	plt.filter = NULL, unitvar = NULL, unitvar2 = NULL, unitarea = NULL, 
	areavar = "ACRES", unitcombine = FALSE, stratalut = NULL, strvar = "STRATA", 
	getwt = TRUE, getwtvar = "P1POINTCNT", stratcombine = FALSE, rowvar = NULL, 
	colvar = NULL, row.add0 = FALSE, col.add0 = FALSE, domlut = NULL, 
	ratioden = "ROWVAR", lutfolder = NULL, allin1 = FALSE, savedata = FALSE, 
	outfolder = NULL, outfn = NULL, outfn.pre = NULL, outfn.date = TRUE, 
	overwrite = FALSE, addtitle = TRUE, title.main = NULL, title.ref = NULL, 
	title.rowvar = NULL, title.colvar = NULL, title.unitvar = NULL, 
	title.filter = NULL, returntitle = FALSE, estround = NULL, pseround = NULL, 
	estnull = 0, psenull = "--", rowlut = NULL, collut = NULL, rawdata = FALSE, 
	gainloss = FALSE, gainloss.vals = NULL, stabbr = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{icedat}{ DF/DT or comma-delimited file (*.csv). The point-level 
	table (may contain condition-level info). }
  \item{pltassgn}{ DF/DT, comma-delimited file(*.csv), SpatialDataFrame, or 
	shapefile(*.shp) with strata/estimation unit variables. Optional. }
  \item{state}{ String. The name of state for area of interest (e.g., "Vermont", "Utah"). }
  \item{T1}{ Integer. Year of Time 1 imagery. }
  \item{T2}{ Interger. Year of Time 2 imagery. }
  \item{puniqueid}{ String. Name of unique identifier for pltassgn. }
  \item{plotid}{ String. Name of unique identifier for ice45 plots. }
  \item{pntid}{ String. Name of unique identifier for the points. }
  \item{tabtype}{ String. Type of units for the table ("AREA", "PCT"). }
  \item{sumunits}{ Logical. If TRUE, estimation units are summed and returned in
	one table. }
  \item{strata}{ Logical. If TRUE, add strata information for post-stratication. }
  \item{ratio}{ Logical. If TRUE, ratio estimates are generated. }
  \item{landarea}{ String. Sample area filter for estimates ("ALL", "CHANGE"). 
	Change includes plots with observed change. }
  \item{pnt.filter}{ String. A global filter for the pnt file. Must be R syntax. }
  \item{plt.filter}{ String. A plot-level filter. Must be R syntax. }
  \item{unitvar}{ String. Name of the estimation unit variable in stratalut and cond  
	or pltassgn tables. }
  \item{unitvar2}{ String. Name of a second estimation unit variable in cond or 
	pltassgn with assignment for each plot (e.g., 'STATECD'). }
  \item{unitarea}{ Numeric or DF. Total acres by estimation unit. If only 1 estimation 
	unit, include number of total acreage for the area of interest or a data frame
	with areavar. If more than one estimation unit, provide a data frame of total 
	acres by estimation unit, including unitvar and areavar. }
  \item{areavar}{ String. Name of the acre variable in stratalut. Default="ACRES". }
  \item{unitcombine}{ Logical. If TRUE, automatically combines estimation units if less 
	than 2 plots in any one estimation unit. See notes for more info. }
  \item{stratalut}{ DF/DT. If strata=TRUE, look-up table with strata 
	proportions ('strwt') by strata (and estimation unit). To calculate 
	'strwt', set getwt=TRUE and getwtvar= name of variable with information 
	to calculate weights from (e.g., pixel counts) }.
  \item{strvar}{ String. Name of the strata variable in stratalut and pnt or plt. 
	The default="STRATA". }
  \item{getwt}{ Logical. If TRUE, calculates strata weights from stratatlut areavar. }
  \item{getwtvar}{ String. Name of variable in stratalut to calculate weights (strwt).
 	Default="P1POINTCNT". }
  \item{stratcombine}{ Logical. If TRUE, automatically combines estimation units if less 
	than 2 plots in any one estimation unit. See notes for more info. }
  \item{rowvar}{ String. Name of domain variable in pnt used for output estimation table 
	rows. If only 1 domain, must be rowvar. }
  \item{colvar}{ String. Name of domain variable in pnt used for output estimation table 
	columns. If only 1 domain, colvar=NULL. }
  \item{row.add0}{ Logical. If TRUE, add the rows that have 0 values. }
  \item{col.add0}{ Logical. If TRUE, add the columns that have 0 values. }  
  \item{domlut}{ DF/DT or comma-delimited (*.csv). Look-up table to define the variables 
	in the pnt table with category codes (DOMCODE) and code names (DOMNAME), and to 
	set a pretty name for the variable to use in output table (DOMTITLE). This table 
	is also used to define domain variables and populate row.orderby, col.orderby,
 	title.rowvar, and title.colvar parameters. Optional. }
  \item{ratioden}{ String. ("ROWVAR", "COLVAR"). If ratio=TRUE, is rowvar or colvar 
	the denominator. Default="ROWVAR". }    
  \item{lutfolder}{ String. Folder name where LUT files are located. }
  \item{allin1}{ Logical. If TRUE, one table is returned with both estimates and percent 
	standard error: estimates (percent standard error). }
  \item{savedata}{ Logical. If TRUE, saves tables to outfolder. }
  \item{outfolder}{ String.  The outfolder to write files to. }
  \item{outfn}{ String. Name of the output file(s) if savedata=TRUE (*.xlsx). Do not 
	include extensions or dates. The date will be added to end of file name. If NULL, 
	generic file names will be given (ex. area_'rowvar'_'date'.csv) }
  \item{outfn.pre}{ String. A prefix to give output name (ex. "01"). }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile (e.g., outfn_'date'.csv). }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
  \item{addtitle}{ Logical. If TRUE, adds title to the outfile. }
  \item{title.main}{ String. TITLE, if savedata=TRUE: The complete title used for table. If 
	title.main=NULL and addtitle=TRUE, the title.* parameters are used to generate title string. 
	Note: a title.ref may be added with title.main. }
  \item{title.ref}{ String. TITLE, if savedata=TRUE: The ending text of the table title 
	(i.e. Nevada, 2004-2005). If NULL, = "". }
  \item{title.rowvar}{ String. TITLE, if savedata=TRUE: pretty name for the row domain 
	variable. If NULL, = rowvar. }
  \item{title.colvar}{ String. TITLE, if savedata=TRUE: pretty name for the column domain 
	variable. If NULL, = colvar. }
  \item{title.unitvar}{ String. TITLE, if savedata=TRUE: pretty name for the estimation 
	unit variable. If NULL, = unitvar. }
  \item{title.filter}{ String. TITLE, if savedata=TRUE: pretty name for the filters. 
	If NULL, = "". }
  \item{returntitle}{ Logical. If TRUE, returns a character string of the title of the 
	output data frame. }
  \item{estround}{ Integer. Number of decimal places for estimates. }
  \item{pseround}{ Integer. Number of decimal places for percent standard error. }
  \item{estnull}{ Integer or String. Replace NULL values for estimates. }
  \item{psenull}{ Integer or String. Replace NULL values for percent standard error. }
  \item{add0}{ Logical. If TRUE, add the row and columns that have 0 values. }
  \item{rowlut}{ DF/DT. If add0=TRUE, a lookup table of unique row codes and names to
	use for row values in table. }
  \item{collut}{ DF/DT. If add0=TRUE, a lookup table of unique column codes and names to
	use for column values in table. }
  \item{rawdata}{ Logical. If TRUE, returns a list of raw data tables that are used
	for estimation (See Value). If savedata = TRUE, this table is also written to file. }
  \item{gainloss}{ Logical. If TRUE, a table with the difference of gain and loss 
	along with the variance and standard error, in percent, is generated. }
  \item{gainloss.vals}{ String vector. A vector of names for values in gainloss table. }
  \item{stabbr}{ String. State abbreviation or other name for population. }
}
\details{
	If variables are NULL, then it will prompt user to input variables.
		
}
\value{
  If FIA=TRUE or unitvar=NULL and/or colvar=NULL, 1 table will be output including 
	estimations and percent sampling errors. Otherwise the following 2 tables will be 
	output as a list.

  \item{est}{ DF. Acre estimates by domain (and estimation unit).  }
  \item{est.pse}{ DF. Percent sampling error for estimates by unit and domain. }
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
