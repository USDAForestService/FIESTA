\name{modPB}
\alias{modPB}
\title{
	Photo-Based module - Generate photo-based estimates.
}
\description{
	Generates percent, area or ratio-of-means estimates, with associated 
sampling error by domain (and estimation unit). Calculations are based on 
Patterson (2012) photo-based estimators for the Nevada photo-based inventory.
}
\usage{
modPB(pnt = NULL, pltpct = NULL, plotid = "plot_id", pntid = NULL, 
	pltpctvars = NULL, plt = NULL, pltassgn = NULL, puniqueid = "CN", 
	pltassgnid = "CN", plt.nonsamp.filter = NULL, tabtype = "PCT", 
	strata = FALSE, strtype = "POST", sumunits = FALSE, unitvar = NULL, 
	unitvar2 = NULL, unitarea = NULL, areavar = "ACRES", unitcombine = FALSE, 
	stratalut = NULL, strvar = "STRATUMCD", getwt = TRUE, getwtvar = "P1POINTCNT", 
	stratcombine = TRUE, ratio = FALSE, landarea = "ALL", landarea.filter = NULL, 
	pnt.nonsamp.filter = NULL, pnt.filter = NULL, plt.filter = NULL, 
	rowvar = NULL, colvar = NULL, row.orderby = NULL, col.orderby = NULL, 
	row.add0 = FALSE, col.add0 = FALSE, rowlut = NULL, collut = NULL, 
	domlut = NULL, domvarlst = NULL, ratioden = "ROWVAR", allin1 = FALSE, 
	estround = 3, pseround = 3, estnull = 0, psenull = "--", divideby = NULL, 
	savedata = FALSE, rawdata = FALSE, outfolder = NULL, outfn = NULL, 
	outfn.pre = NULL, outfn.date = TRUE, overwrite = FALSE, addtitle = TRUE, 
	returntitle = FALSE, title.main = NULL, title.ref = NULL, 
	title.rowvar = NULL, title.colvar = NULL, title.unitvar = NULL, 
	title.filter = NULL, title.units = "acres", gainloss = FALSE, 
	gainloss.vals = NULL, PBpopdat = NULL, gui = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pnt}{ DF/DT or comma-delimited file (*.csv). Point-level table with
	one record per point. If NULL, aggregated point counts must be in pntcnt. }
  \item{pltpct}{ DF/DT or comma-delimited file (*.csv). Plot-domain-level table 
	with percent observed by domain per plot. }
  \item{plotid}{ String. Unique identifier of plot in pnt. All values
	must match puniqueid values, if pltassgn is not NULL. }
  \item{pntid}{ String. Unique identifier of points in pnt. }
  \item{pltpctvars}{ String vector. Variables in pltpct for estimation. If NULL,
	all variables are used except plotid in pltpct. }
  \item{plt}{ DF/DT, comma-separated values (CSV) file(*.csv), or layer in dsn,
	Can also be a shapefile(*.shp) with one record per plot, a spatial layer in dsn,
	or a sf R object. Plot-level variables. If nonsampled plots are included, 
	PLOT_STATUS_CD variable must be in table. Optional. }
  \item{pltassgn}{ DF/DT, comma-delimited file(*.csv), SpatialDataFrame, or 
	shapefile(*.shp). The plot-level data with one record per plot, including 
	estimation unit and/or strata information. Optional. }
  \item{puniqueid}{ String. Unique identifier of plot. }
  \item{pltassgnid}{ String. Name of unique identifier of plot in pltassgn with 
	All values must match plotid values if pnt is not NULL. }
  \item{plt.nonsamp.filter}{ String. An expression for filtering nonsampled plots. 
	Must be R syntax. }
  \item{tabtype}{ String. Type of units for the table ("PCT", "AREA"). }
  \item{strata}{ Logical. If TRUE, add data information for stratification. }
  \item{strtype}{ String. If strata=TRUE, the type of strata ('POST', 'PRE'). 
	Note: the variance equations are slighlty different. }
  \item{sumunits}{ Logical. If TRUE, estimation units are combined to one table for
	output. Note: only available if tabtype="AREA". Acres}
  \item{unitvar}{ String. Name of the estimation unit variable in cond or pltassgn 
	with estimation unit assignment for each plot (e.g., 'ESTN_UNIT'). 
	If one estimation unit, set unitvar=NULL. }
  \item{unitvar2}{ String. Name of a second estimation unit variable in cond or 
	pltassgn with assignment for each plot (e.g., 'STATECD'). }
  \item{unitarea}{ Numeric or DF. Total area by estimation unit. If only 1 estimation 
	unit, include number of total acreage for the area of interest or a data frame
	with areavar. If more than one estimation unit, provide a data frame of total 
	area by estimation unit, including unitvar and areavar. }
  \item{areavar}{ String. Name of acre variable in unitarea. Default="ACRES". }
  \item{unitcombine}{ Logical. If TRUE, automatically combines estimation units if less 
	than 2 plots in any one estimation unit. See notes for more info. }
  \item{stratalut}{ DF/DT. If strata=TRUE, look-up table with strata 
	proportions ('strwt') by strata (and estimation unit). To calculate 
	'strwt', set getwt=TRUE and getwtvar= name of variable with information 
	to calculate weights from (e.g., pixel counts) }.
  \item{strvar}{ String. Name of strata variable in stratalut and table with strata
	assignment for each plot. Default="STRATUMCD". }
  \item{getwt}{ Logical. If TRUE, calculates strata weights from stratatlut getwtvar. 
	If FALSE, 'strwt' variable must be in stratalut. }
  \item{getwtvar}{ String. Name of variable in stratalut to calculate weights (strwt).
 	Default="P1POINTCNT". }
  \item{stratcombine}{ Logical. If TRUE, automatically combines estimation units if less 
	than 2 plots in any one estimation unit. See notes for more info. }
  \item{ratio}{ Logical. If TRUE, ratio estimates are generated. }  
  \item{landarea}{ String. Sample area for estimates ("ALL", "CHANGE"). Used to
	describe landarea.filter. }
  \item{landarea.filter}{ String. filter for land area. Must be R syntax. }
  \item{pnt.nonsamp.filter}{ String. An expression for filtering nonsampled points
	(e.g., cloud coverage). Must be R syntax. }
  \item{pnt.filter}{ String. A global filter for the pnt file. Must be R syntax. }
  \item{plt.filter}{ String. A global filter for the plt file. Must be R syntax. }
  \item{rowvar}{ String. Name of domain variable in pnt used for output estimation 
	table rows. If only 1 domain, must be rowvar. If no domain, rowvar=NULL. }
  \item{colvar}{ String. Name of domain variable in pnt used for output estimation 
	table columns. If only 1 domain, colvar=NULL. }
  \item{row.orderby}{ String. Name of the variable to use to sort the table rows with. 
	If NULL, rowvar is used. }    
  \item{col.orderby}{ String. Name of the variable to use to sort the table columns with. 
	If NULL, colvar is used. Only use if colvar is not NULL. }
  \item{ratioden}{ String. ("ROWVAR" or "COLVAR"). If ratio, defines whether the rowvar
	or colvar in estimation output table is the denominator.  }
  \item{row.add0}{ Logical. If TRUE, add the rows that have 0 values. }
  \item{col.add0}{ Logical. If TRUE, add the columns that have 0 values. }  
  \item{rowlut}{ Data frame. A lookup table with variable codes and descriptions to
	include in rows of output table (See notes for more information and format). }
  \item{collut}{ Data frame. A lookup table with variable codes and descriptions to
	include in columns of output table (See notes for more information and format). }
  \item{domlut}{ DF/DT or comma-delimited (*.csv). Look-up table to define the variables 
	in the pnt table with category codes (DOMCODE) and code names (DOMNAME), and to 
	set a pretty name for the variable to use in output table (DOMTITLE). This table 
	is also used to populate rowvar/colvar, row.orderby/col.orderby, and
 	title.rowvar/title.colvar parameters. Optional. }
  \item{domvarlst}{ String vector. A vector of variable names that can be row or column 
	domains (codes and names). Optional. }
  \item{allin1}{ Logical. If TRUE, one table with both estimates and percent standard error 
	will be output: estimates (percent standard error). }
  \item{estround}{ Integer. Number of decimal places for estimates. }
  \item{pseround}{ Integer. Number of decimal places for percent standard error. }
  \item{estnull}{ Integer or String. Value to replace NULL values for estimates. }
  \item{psenull}{ Integer or String. Value to replace NULL values for percent standard error. }
  \item{divideby}{ String. Conversion number for output ('hundred', 'thousand', 'million'). }
  \item{savedata}{ Logical. If TRUE, saves tables to outfolder. }
  \item{rawdata}{ Logical. If TRUE, returns a list of raw data tables that are used
	for estimation (See Value). If savedata = TRUE, this table is also written to file. }
  \item{outfolder}{ String.  The outfolder to write files to. }
  \item{outfn}{ String. Name of the output file(s) if savedata=TRUE (*.xlsx). Do not 
	include extensions or dates. The date will be added to end of file name. If NULL, 
	generic file names will be given (ex. area_'rowvar'_'date'.csv) }
  \item{outfn.pre}{ String. A prefix to output name (e.g., "01"). }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile (e.g., outfn_'date'.csv). }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
  \item{addtitle}{ Logical. If TRUE, adds title to the outfile. }
  \item{returntitle}{ Logical. If TRUE, returns a character string of the title of the 
	output data frame. }
  \item{title.main}{ String. Complete title used for table. If title=NULL and addtitle=TRUE, 
	the title.* parameters are used to generate title string. }
  \item{title.ref}{ String. TITLE: The ending text of the table title (i.e. Nevada, 2004-2005). 
	If NULL, = "". }
  \item{title.rowvar}{ String. TITLE: pretty name for the row domain variable. 
	If NULL, = rowvar. }
  \item{title.colvar}{ String. TITLE: pretty name for the column domain variable. 
	If NULL, = colvar. }
  \item{title.unitvar}{ String. TITLE: pretty name for the estimation unit variable. 
	If NULL, = unitvar. }
  \item{title.filter}{ String. TITLE, if savedata=TRUE: pretty name for the filters. 
	If NULL, = "". }
  \item{title.units}{ String. TITLE, if savedata=TRUE: title name for area units. 
	If NULL, title.units = "acres". }
  \item{gainloss}{ Logical. If TRUE, a table with the difference of gain and loss 
	along with the variance and standard error, in percent, is generated. }
  \item{gainloss.vals}{ String vector. A vector of names for values in gainloss table. }
  \item{PBpopdat}{ List. Population data objects returned from modPBpop(). }
  \item{gui}{ Logical. If gui, user is prompted for parameters. }
}
\details{
	If variables are NULL, then it will prompt user to input variables.
		
}
\value{
  A list with estimates with percent sampling error for rowvar (and colvar). 
	If sumunits=TRUE or unitvar=NULL and colvar=NULL, one data frame is returned.
	Otherwise, a list object is returned with the following information. 
	If savedata=TRUE, all data frames are written to outfolder.

  \item{est}{ DF. Estimated percent cover or area by rowvar, colvar, (and estimation unit).  }
  \item{pse}{ DF. Percent sampling error of estimates by rowvar, colvar (and estimation unit). }
  \item{titlelst}{ List with 1 or 2 string vectors. If returntitle=TRUE a list with
	table title(s). The list contains one title if est and pse are in the same table 
	and two titles if est and pse are in separate tables. Row and column tables are 
	also included in list. }
  \item{raw}{ List of data frames. If rawdata=TRUE, a list including the processing
	data used for estimation including: number of plots and conditions; stratification 
	information; and 1 to 8 tables with calculated values for table cells and totals 
	(See processing data below). }
  

  Raw data

  \item{pntsampcnt}{ Table. Number of points by rowvar/colvar (sampled and nonsampled). }

  \item{stratdat}{ Data frame. Strata information by estimation unit. }
	\tabular{lll}{
		\tab \bold{Variable}	\tab \bold{Description}\cr
	 	\tab unitvar		\tab estimation unit\cr
	 	\tab strvar			\tab strata \cr
	 	\tab areavar		\tab If tabtype='AREA', area by strata for estimation unit\cr
	 	\tab n.strata		\tab number of plots in strata (after totally nonsampled plots removed) \cr
	 	\tab n.total		\tab number of plots for estimation unit \cr
	 	\tab TOTAREA		\tab If tabtype='AREA', total area for estimation unit \cr
	 	\tab strwt			\tab proportion of area (or number of plots) by strata (strata weight) \cr
	}

  \item{processing data}{ Data frames. Separate data frames of variables used in estimation 
	process for the rowvar, colvar and combination of rowvar and colvar (if colvar is not NULL), 
	and grand total by estimation unit (unit.rowest, unit.colest, unit.grpest, respectively) 
	and summed estimation units, if FIA=TRUE (rowest, colest, grpest, respectively). 

	The data frames include the following information:
	\tabular{lll}{
		\tab \bold{Variable}	\tab \bold{Description}\cr
	 	\tab phat		\tab estimated proportion of covered land \cr
	 	\tab phat.var	\tab variance of estimated proportion of covered land \cr
	 	\tab areavar	\tab If tabtype='AREA', total area for estimation unit\cr
	 	\tab est		\tab If tabtype='AREA', estimated area of land { phat*areavar }
						If tabtype='PCT', estimated percent cover of land { phat*100 } \cr
	 	\tab est.var	\tab variance of estimated area of land { phat.var*areavar } \cr
						If tabtype='PCT', estimated percent cover of land { phat.var*100 } \cr
	 	\tab est.se	\tab standard error of estimated area or percent cover { sqrt(est.var) } \cr
	 	\tab est.cv	\tab coefficient of variance of estimated area or percent cover { est.se/est } \cr
	 	\tab est.pse	\tab percent sampling error of estimated area of percent cover { est.cv*100 } \cr
	 	\tab CI99left	\tab left tail of 99 percent confidence interval for estimate \cr
	 	\tab CI99right	\tab right tail of 99 percent confidence interval for estimate \cr
	 	\tab CI95left	\tab left tail of 95 percent confidence interval for estimate \cr
	 	\tab CI95right	\tab right tail of 95 percent confidence interval for estimate \cr
	 	\tab CI67left	\tab left tail of 67 percent confidence interval for estimate \cr
	 	\tab CI67right	\tab right tail of 67 percent confidence interval for estimate \cr
	}

     if ratio=TRUE:
	\tabular{lll}{
		\tab \bold{Variable}	\tab \bold{Description}\cr
	 	\tab phat.n	\tab estimated proportion of covered land, for numerator (colvar) \cr
	 	\tab phat.var.n	\tab variance of estimated proportion of covered land, for numerator (colvar) \cr
	 	\tab phat.d	\tab estimated proportion of covered land, for denominator (rowvar) \cr
	 	\tab phat.var.d	\tab variance of estimated proportion of covered land, for denominator (rowvar) \cr
 		\tab covar		\tab covariance of estimated proportion of numerator (rowvar) and denominator (colvar) \cr
	 	\tab rhat		\tab ratio of estimated proportions (numerator-colvar / denominator-rowvar) \cr
	 	\tab rhat.var	\tab variance of ratio of estimated proportions \cr
	 	\tab rhat.se	\tab standard error of ratio of estimated proportions { sqrt(rhat.var) } \cr
	 	\tab rhat.cv	\tab coefficient of variation of ratio of estimated proportions { rhat.se/rhat } \cr
	 	\tab areavar	\tab If tabtype='AREA', total area for estimation unit\cr
	 	\tab est		\tab If tabtype='AREA', estimated area of land { rhat*areavar }
						If tabtype='PCT', estimated percent cover of land { rhat*100 } \cr
	 	\tab est.var	\tab variance of estimated area of land { rhat.var*areavar } \cr
						If tabtype='PCT', estimated percent cover of land { rhat.var*100 } \cr
	 	\tab est.se	\tab standard error of estimated area or percent cover { sqrt(est.var) } \cr
	 	\tab est.cv	\tab coefficient of variance of estimated area or percent cover { est.se/est } \cr
	 	\tab est.pse	\tab percent sampling error of estimated area of percent cover { est.cv*100 } \cr
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
Frescino, Tracey S.; Moisen, Gretchen G.; Megown, Kevin A.; Nelson, Val J.; Freeman, 
Elizabeth A.; Patterson, Paul L.; Finco, Mark; Brewer, Ken; Menlove, James 2009. 
Nevada Photo-Based Inventory Pilot (NPIP) photo sampling procedures. Gen. Tech. Rep. 
RMRS-GTR-222. Fort Collins, CO: U.S. Department of Agriculture, Forest Service, 
Rocky Mountain Research Station. 30 p.

Patterson, Paul L. 2012. Photo-based estimators for the Nevada photo-based inventory. 
Res. Pap. RMRS-RP-92. Fort Collins, CO: U.S. Department of Agriculture, Forest Service, 
Rocky Mountain Research Station. 14 p.

}

\note{

	STRATA:\cr
	Stratification is used to reduce variance in population estimates by partitioning the 
	population into homogenous classes (strata), such as forest and nonforest. For 
	stratified sampling methods, the strata sizes (weights) must be either known or 
	estimated. Remotely-sensed data is often used to generate strata weights with 
	proporation of pixels by strata. If stratification is desired (strata=TRUE), the 
	required data include: stratum assignment for the center location of each plot, 
	stored in either pltassgn or cond; and a look-up table with the area or proportion 
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


	stratcombine:\cr
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



\keyword{ data }
