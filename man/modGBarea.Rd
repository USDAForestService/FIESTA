\name{modGBarea}
\alias{modGBarea}
\title{
	Green-Book module - Generate area estimates.
}
\description{
	Generates area estimates by domain (and estimation unit). Calculations are based 
	on Scott et al. 2005 ('the green-book') for mapped forest inventory plots. The 
	non-ratio estimator for estimating area by stratum and domain is used. Plots that 
	are totally nonsampled are excluded from estimation dataset. Next, an adjustment 
	factor is calculated by strata to adjust for nonsampled (nonresponse) conditions 
	that have proportion less than 1. The attribute is the proportion of the plot which 
	is divided by the adjustment factor, and averaged by stratum. Strata means are 
	combined using the strata weights and then expanded to area using the total land 
	area in the population. 
	  
}
\usage{
modGBarea(cond, pltstrat = NULL, cuniqueid = "PLT_CN", puniqueid = "CN", 
	sumunits = FALSE, adjsamp = TRUE, strata = TRUE, landarea = "ALL", 
	ACI = FALSE, nonsamp.filter = NULL, plt.filter = NULL, cond.filter = NULL, 
	unitvar = "ESTN_UNIT", unitvar2 = NULL, autocombine = TRUE, unitarea = NULL, 
	areavar = "ACRES", stratalut = NULL, strvar = "STRATUMCD", getwt = TRUE, 
	getwtvar = "P1POINTCNT", rowvar = NULL, rowvar.filter = NULL, colvar = NULL, 
	colvar.filter = NULL, row.FIAname = FALSE, col.FIAname = FALSE, 
	row.orderby = NULL, col.orderby = NULL, row.add0 = FALSE, col.add0 = FALSE, 
	rowlut = NULL, collut = NULL, rowgrp = FALSE, rowgrpnm=NULL, rowgrpord=NULL, 
	allin1 = FALSE, estround = 1, pseround = 2, estnull = 0, psenull = "--", 
	divideby = NULL, savedata = FALSE, rawdata = FALSE, outfolder = NULL, 
	outfn = NULL, outfn.pre=NULL, outfn.date = TRUE, overwrite = FALSE, 
	addtitle = TRUE, returntitle = FALSE, title.main = NULL, title.ref = NULL, 
	title.rowvar = NULL, title.colvar = NULL, title.unitvar = NULL, 
	title.filter = NULL, gui = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cond}{ DF/DT or comma-separated values (CSV) file (*.csv). The condition-level 
	table with one record per condtions and including nonsampled conditions. 
	Plot variables and strata/estimation unit variable(s) may be included for 
	post-stratification. See details for necessary variables to include. }
  \item{pltstrat}{ DF/DT, comma-separated values (CSV) file(*.csv), SpatialDataFrame, or 
	shapefile(*.shp) with one record per plot and strata/estimation unit variable(s)
	if post-stratification. Include nonsampled plots only if PLOT_STATUS_CD variable 
	is in table. Optional. }
  \item{cuniqueid}{ String. Unique identifier in cond. }
  \item{puniqueid}{ String. Unique identifier in pltstrat. }
  \item{sumunits}{ Logical. If TRUE, estimation units are summed and returned in
	one table. }
  \item{adjsamp}{ Logical. If TRUE, adjustment factors are calculated for nonsampled 
	(nonresponse) conditions based on summed proportions for sampled conditions
	divided by the total plots in the strata/estimation unit. Adjustments are only 
	calculated for annual inventory plots (designcd=1). }
  \item{strata}{ Logical. If TRUE, add data information for stratification. }
  \item{landarea}{ String. The sample area filter for estimates ("ALL", "FOREST", "TIMBERLAND"). 
	If landarea=FOREST, filtered to COND_STATUS_CD = 1; If landarea=TIMBERLAND, 
	filtered to SITECLCD in(1:6) and RESERVCD = 0. }
  \item{ACI}{ Logical. If TRUE, includes All Condition Inventory (ACI) plots. Removes 
	nonsampled nonforest lands (NF_COND_STATUS_CD = 5). }
  \item{nonsamp.filter}{ String. A logical expression for filtering nonsampled conditions. 
	Must be R syntax. If nonsamp.filter=NULL, a default is set to: 'COND_STATUS_CD != 5'; 
	if ACI: 'is.na(NF_COND_STATUS_CD) | NF_COND_STATUS_CD != 5'. 
	Set nonsamp.filter="NONE" to keep nonsampled plots and conditions in analysis. }
  \item{plt.filter}{ String. A filter for the pltstrat. Must be R logical syntax. }
  \item{cond.filter}{ String. A filter for the cond. Must be R logical syntax. }
  \item{unitvar}{ String. Name of the estimation unit variable in cond or pltstrat 
	with estimation unit assignment for each plot (e.g., 'ESTN_UNIT'). 
	If one estimation unit, set unitvar=NULL. }
  \item{unitvar2}{ String. Name of a second estimation unit variable in cond or 
	pltstrat with assignment for each plot (e.g., 'STATECD'). }
  \item{autocombine}{ Logical. If TRUE, automatically combines estimation units if less 
	than 2 plots in any one estimation unit. See notes for more info. }
  \item{unitarea}{ Numeric or DF. Total area by estimation unit. If only 1 estimation 
	unit, include a number of total acreage for the area of interest or a data frame
	with areavar. If more than one estimation unit, provide a data frame of total 
	area by estimation unit, including unitvar and areavar. }
  \item{areavar}{ String. Name of acre variable in unitarea. Default="ACRES". }
  \item{stratalut}{ DF/DT. If strata=TRUE, look-up table with strata 
	proportions ('strwt') by strata (and estimation unit). To calculate 
	'strwt', set getwt=TRUE and getwtvar= name of variable with information 
	to calculate weights from (e.g., pixel counts) }.
  \item{strvar}{ String. Name of strata variable in stratalut and table with strata
	assignment for each plot. Default="STRATUMCD". }
  \item{getwt}{ Logical. If TRUE, calculates strata weights from getwtvar in stratatlut. 
	If FALSE, 'strwt' variable must be in stratalut. }
  \item{getwtvar}{ String. Name of variable in stratalut to calculate weights (strwt).
 	Default="P1POINTCNT". }
  \item{rowvar}{ String. Name of row domain variable in cond. If only one domain, 
	rowvar = domain variable. If more than one domain, include colvar. If no domain,
	rowvar = NULL. }
  \item{rowvar.filter}{ String. Filter for row variable. Must be R syntax. }
  \item{colvar}{ String. Name of column domain variable in cond. }
  \item{colvar.filter}{ String. Filter for column variable. Must be R syntax. }
  \item{row.FIAname}{ Logical. If TRUE, gets FIA reference names for row variable based on 
	ref_codes. Only available for certain variables. }
  \item{col.FIAname}{ Logical. If TRUE, gets FIA reference names for column variable based
	on ref_codes. Only available for certain variables. }
  \item{row.orderby}{ String. Name of variable to sort table rows. If row.FIAname=TRUE 
	and a ref_* exists for rowvar, the rowvar code is used to sort. If NULL, the 
	table is sorted by rowvar. }    
  \item{col.orderby}{ String. Name of variable to sort table columns. If col.FIAname=TRUE 
	and a ref_* exists for colvar, the colvar code is used to sort. If NULL, the 
	table is sorted by colvar. }  
  \item{row.add0}{ Logical. If TRUE, add the rows that have 0 values. }
  \item{col.add0}{ Logical. If TRUE, add the columns that have 0 values. }  
  \item{rowlut}{ Data frame. A lookup table with variable codes and descriptions to
	include in rows of output table (See notes for more information and format). }
  \item{collut}{ Data frame. A lookup table with variable codes and descriptions to
	include in columns of output table (See notes for more information and format). }
  \item{rowgrp}{ Logical. If TRUE, appends row groups to first column of table. 
	Only available if group category exists in ref_codes table (e.g., FORTYPGRPCD, OWNGRPCD). }
  \item{rowgrpnm}{ String. Name of row group variable. }  
  \item{rowgrpord}{ String. Name of row group variable to sort table rows. }  
  \item{allin1}{ Logical. If TRUE, both estimates and percent sample error are output
	in one table as: estimates (percent sample error). }
  \item{estround}{ Integer. Number of decimal places for estimates. }
  \item{pseround}{ Integer. Number of decimal places for percent sampling error. }
  \item{estnull}{ Number or character. The number or symbol to use to indicate 'not sampled'
		for estimate. }
  \item{psenull}{ Number or character. The number or symbol to use to indicate 'not sampled'
		for percent standard errror. }
  \item{divideby}{ String. Conversion number for output ('hundred', 'thousand', 'million'). }
  \item{savedata}{ Logical. If TRUE, saves table(s) to outfolder. }
  \item{rawdata}{ Logical. If TRUE, returns a list of raw data tables that are used
	for estimation (See Value). If savedata = TRUE, also written to outfolder. }
  \item{outfolder}{ String. The outfolder to write files to. If NULL, files are 
	written to working directory, or if gui, a window to browse. }
  \item{outfn}{ String. Name of the output file(s) if savedata=TRUE. Do not include
	extension. Default file name includes parameter inputs (e.g., area_'rowvar'_'date'). }
  \item{outfn.pre}{ String. Add a prefix to output name (e.g., "01"). }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile (e.g., outfn_'date'.csv). }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
  \item{addtitle}{ Logical. If TRUE and savedata=TRUE, adds title to outfile. }
  \item{returntitle}{ Logical. If TRUE, returns title(s) of the estimation table(s). }
  \item{title.main}{ String. TITLE, if savedata=TRUE and/or returntitle=TRUE: the complete 
	title used for table. If title.main=NULL, the title.* parameters are used to 
	generate title string. Note: if title.ref is not NULL, it is added to title.main. }
  \item{title.ref}{ String. TITLE, if savedata=TRUE and/or returntitle=TRUE: the ending 
	text of the table title (e.g., Nevada, 2004-2005). If NULL, = "". }
  \item{title.rowvar}{ String. TITLE, if savedata=TRUE and/or returntitle=TRUE: pretty 
	name for the row domain variable. If NULL, = rowvar. }
  \item{title.colvar}{ String. TITLE, if savedata=TRUE and/or returntitle=TRUE: pretty 
	name for the column domain variable. If NULL, = colvar. }
  \item{title.unitvar}{ String. TITLE, if savedata=TRUE and/or returntitle=TRUE: pretty  
	name for the estimation unit variable. If NULL, = unitvar. }
  \item{title.filter}{ String. TITLE, if savedata=TRUE and/or returntitle=TRUE: pretty 
	name for filter(s). If NULL, = "". }
  \item{gui}{ Logical. If TRUE, user is prompted for parameters. }
}
\details{
	If variables are NULL, then it will prompt user to input variables.

     	Necessary variables:\cr
	\tabular{llll}{
		\tab \bold{Data} \tab \bold{Variable}	\tab \bold{Description}\cr
	 	\tab cond \tab cuniqueid	\tab Unique identifier for each plot, to link to pltstrat 
								(ex. PLT_CN).\cr
		\tab  \tab CONDID		\tab Unique identfier of each condition on plot. 
								Set CONDID=1, if only 1 condition per plot.\cr
		\tab  \tab CONDPROP_UNADJ	\tab Unadjusted proportion of condition on each plot. 
								Set CONDPROP_UNADJ=1, if only 1 condition per plot.\cr
		\tab  \tab COND_STATUS_CD	\tab Status of each forested condition on plot 
							(i.e. accessible forest, nonforest, water, etc.)\cr
		\tab  \tab NF_COND_STATUS_CD	\tab If ACI=TRUE. Status of each nonforest condition on plot 
							(i.e. accessible nonforest, nonsampled nonforest)\cr
  		\tab  \tab SITECLCD		\tab If landarea=TIMBERLAND. Measure of site productivity.\cr
  		\tab  \tab RESERVCD		\tab If landarea=TIMBERLAND. Reserved status.\cr

	 	\tab pltstrat \tab puniqueid	\tab Unique identifier for each plot, to link to cond 
								(ex. CN).\cr
	 	\tab  \tab STATECD		\tab Identifies state each plot is located in.\cr
	 	\tab  \tab INVYR		\tab Identifies inventory year of each plot.\cr
	 	\tab  \tab PLOT_STATUS_CD	\tab Status of each plot (i.e. sampled, nonsampled). 
						If not included, all plots are assumed as sampled.\cr							
	}

	For available reference tables: sort(unique(FIESTA::ref_codes$VARIABLE)) \cr
		
}
\value{
  A list with estimates with percent sampling error for rowvar (and colvar). 
	If sumunits=TRUE or unitvar=NULL and colvar=NULL, one data frame is returned.
	Otherwise, a list object is returned with the following information. 
	If savedata=TRUE, all data frames are written to outfolder.

  \item{est}{ Data frame. Area estimates, in area units (e.g., acres), by rowvar, colvar 
	(and estimation unit). If sumunits=TRUE or one estimation unit and colvar=NULL, 
	or allin1=TRUE, estimates and percent sampling error are in one data frame. }
  \item{pse}{ Data frame. Percent sampling errors (Confidence level 68%) for estimates by 
	rowvar and colvar (and estimation unit). }
  \item{titlelst}{ List. If returntitle=TRUE a list with
	table title(s). The list contains one title if est and pse are in the same table 
	and two titles if est and pse are in separate tables. Row and column tables are 
	also included in list. }
  \item{raw}{ List. If rawdata=TRUE, a list including the processing data used for  
	estimation including: number of plots and conditions; stratification 
	information; and 1 to 8 tables with calculated values for table cells and totals 
	(See processing data below). }

  Raw data

  \item{plotsampcnt}{ Table. Number of plots by plot status (e.g., sampled forest 
	on plot, sampled nonforest, nonsampled). }
  \item{condsampcnt}{ DF. Number of conditions by condition status (forest land,
	nonforest land, noncensus water, census water, nonsampled). }
  \item{unitarea}{ DF. Area by estimation unit. }
  \item{expcondtab}{ DF. Condition-level area expansion factors. }
  \item{domdat}{ DF. Final data table used for estimation. }

  \item{stratdat}{ Data frame. Strata information by estimation unit. }
	\tabular{lll}{
		\tab \bold{Variable}	\tab \bold{Description} \cr
	 	\tab unitvar		\tab estimation unit \cr
	 	\tab strvar			\tab stratum value \cr
	 	\tab strwtvar		\tab number of pixels by strata and estimation unit \cr
	 	\tab n.strata		\tab number of plots in strata (after totally nonsampled plots removed) \cr
	 	\tab n.total		\tab number of plots for estimation unit \cr
	 	\tab strwt			\tab proportion of area (or plots) by strata and estimation unit (strata weight) \cr
	 	\tab CONDPROP_UNADJ_SUM	\tab summed condition proportion by strata and estimation unit \cr
	 	\tab CONDPROP_ADJFAC	\tab adjusted condition proportion by strata after nonresponse plots removed \cr
	 	\tab AREA			\tab total area for estimation unit \cr
	 	\tab CONDPROP_ADJFAC	\tab average area \cr
	}

  \item{processing data}{ Data frames. Separate data frames containing calculated variables 
	used in estimation process. The number of processing tables depends on the input
	parameters. The tables include: total by estimation unit (unit.totest); rowvar totals
 	(unit.rowest), colvar totals, if not NULL (unit.colvar); and a combination of rowvar 
	and colvar, if colvar is not NULL (unit.grpvar). If sumunits=TRUE, the raw data for the 
	summed estimation units are also included (totest, rowest, colest, grpest, respectively). 
	These tables do not included estimate proportions (nhat and nhat.var). 

	The data frames include the following information:
	\tabular{lll}{
		\tab \bold{Variable}	\tab \bold{Description}\cr
	 	\tab nhat		\tab estimate proportion of land \cr
	 	\tab nhat.var	\tab variance estimate of estimated proportion of land \cr
	 	\tab AREA		\tab total area for estimation unit \cr
	 	\tab est		\tab estimated area of land { nhat*areavar } \cr
	 	\tab est.var	\tab variance estimate of estimate acres of land { nhat.var*areavar^2 } \cr
	 	\tab est.se		\tab standard error of estimated area of land { sqrt(est.var) } \cr
	 	\tab est.cv		\tab coefficient of variation of estimated area of land { est.se/est } \cr
	 	\tab pse		\tab percent sampling error of estimate { est.cv*100 } \cr
	 	\tab CI99left	\tab left tail of 99 percent confidence interval for estimated area \cr
	 	\tab CI99right	\tab right tail of 99 percent confidence interval for estimated area \cr
	 	\tab CI95left	\tab left tail of 95 percent confidence interval for estimated area \cr
	 	\tab CI95right	\tab right tail of 95 percent confidence interval for estimated area \cr
	 	\tab CI67left	\tab left tail of 67 percent confidence interval for estimated area \cr
	 	\tab CI67right	\tab right tail of 67 percent confidence interval for estimated area \cr
	}
  }
 
}
\author{
	Tracey S. Frescino, Paul L. Patterson, Elizabeth A. Freeman
}
\references{
	Scott, Charles T.; Bechtold, William A.; Reams, Gregory A.; Smith, William D.; Westfall, 
	James A.; Hansen, Mark H.; Moisen, Gretchen G. 2005. Sample-based estimators used by the 
	Forest Inventory and Analysis national information management system. Gen. Tech. Rep. SRS-80. 
	Asheville, NC: U.S. Department of Agriculture, Forest Service, Southern Research Station, 
	p.53-77.
}

\note{

	ADJUSTMENT FACTOR:\cr
	The adjustment factor is necessary to account for nonsampled conditions. It is calculated 
		for each estimation unit by strata by summing the unadjusted condition proportions   
		(CONDPROP_UNADJ) and dividing by the number of plots in the strata/estimation unit.

	If ACI=FALSE, only nonsampled forest conditions are accounted for in the adjustment factor. \cr
	If ACI=TRUE, the nonsampled nonforest conditions are removed as well and accounted for 
	in adjustment factor. This is if you are interested in estimates for all lands or nonforest 
	lands in the All-Condition-Inventory.


	STRATA:\cr
	Stratification is used to reduce variance in population estimates by partitioning the 
	population into homogenous classes (strata), such as forest and nonforest. For 
	stratified sampling methods, the strata sizes (weights) must be either known or 
	estimated. Remotely-sensed data is often used to generate strata weights with 
	proporation of pixels by strata. If stratification is desired (strata=TRUE), the 
	required data include: stratum assignment for the center location of each plot, 
	stored in either pltstrat or cond; and a look-up table with the area or proportion 
	of the total area of each strata value by estimation unit, making sure the name of 
	the strata (and estimation unit) variable and values match the plot assignment name(s) 
	and value(s).


	sumunits:\cr
	An estimation unit is a population, or area of interest, with known area and number
	of plots. Individual counties or combined Super-counties are common estimation units 
	for FIA. An estimation unit may also be a subpopulation of a larger population 
	(e.g., Counties within a State). Subpopulations are mutually exclusive and independent 
	within a population, therefore estimated totals and variances are additive. For example, 
	State-level estimates are generated by summing estimates from all subpopulations 
	within the State (Bechtold and Patterson. 2005. Chapter 2). Each plot must be assigned 
	to only one estimation unit.

	If sumunits=TRUE, estimates are generated by estimation unit, summed together, and 
	returned as one estimate. If rawdata=TRUE, estimates by individual estimation unit
	are also returned.

	If sumunits=FALSE, estimates are generated and returned by estimation unit as one
	data frame. If savedata=TRUE, a separate file is written for each estimation unit.


	autocombine:\cr
	If TRUE and less than 2 plots in any one estimation unit, all estimation units with 
	10 or less plots are combined. The current method for combining is to group the
	estimation unit with less than 10 plots with the estimation unit following in 
	consecutive order (numeric or alphabetical), restrained by survey unit (UNITCD) if 
	included in dataset, and continuing until the number of plots equals 10. If there are 
	no estimation units following in order, it is combined with the estimation unit previous 
	in order. 

	rowlut/collut:\cr
	There are several objectives for including rowlut/collut look-up tables: 1) to include 
	descriptive names that match row/column codes in the input table; 2) to use number 
	codes that match row/column names in the input table for ordering rows; 3) to add 
	rows and/or columns with 0 values for consistency. No duplicate names are allowed.

	Include 2 columns in the table:\cr
	1-the merging variable with same name as the variable in the input merge table;\cr 
	2-the ordering or descriptive variable.\cr
 	If the ordering variable is the rowvar/colvar in the input table and the descriptive 
	variable is in rowlut/collut, set row.orderby/col.orderby equal to rowvar/colvar. If 
	the descriptive variable is the rowvar/colvar in the input table, and the ordering 
	code variable is in rowlut/collut, set row.orderby/col.orderby equal to the variable 
	name of the code variable in rowlut/collut.

}

\examples{

  \dontrun{

  ## Rows only; combine estimation units (sumunits=TRUE)
  modGBarea(cond=WYcond, pltstrat=WYpltstrat, sumunits=TRUE, landarea="FOREST",
	unitarea=WYunitarea, unitvar="ESTN_UNIT", stratalut=WYstrlut, rowvar="FORTYPCD", 
 	row.FIAname=TRUE)
  

  ## Rows only; combine estimation units (sumunits=TRUE; allin1=TRUE)
  modGBarea(cond=WYcond, pltstrat=WYpltstrat, sumunits=TRUE, landarea="FOREST",
	unitarea=WYunitarea, unitvar="ESTN_UNIT", stratalut=WYstrlut, rowvar="FORTYPCD", 
 	row.FIAname=TRUE, allin1=TRUE)
  }
}


\keyword{ data }
