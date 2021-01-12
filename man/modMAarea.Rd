\name{modMAarea}
\alias{modMAarea}
\title{
	Model-Assisted module - Generate model-assisted area estimates.
}
\description{
	Generates area estimates by estimation unit. Estimates are calculated from 
	McConville et al. (2018)'s mase R package.	 
}
\usage{    
modMAarea(cond = NULL, plt = NULL, pltassgn = NULL, dsn = NULL, cuniqueid = "PLT_CN", 
	condid="CONDID", puniqueid = "CN", pltassgnid = "CN", pjoinid="CN", evalid=NULL,
 	invyrs=NULL,ACI = FALSE, adj = "samp", MAmethod = "greg", plt.nonsamp.filter = NULL, 
	cond.nonsamp.filter = NULL, unitvar = NULL, unitvar2 = NULL, unitarea = NULL, 
	areavar = "ACRES", unitcombine = FALSE, minplotnum.unit = 10, unitlut = NULL, 
	npixelvar = "npixels", prednames = NULL, predfac = NULL, PSstrvar = NULL, 
	stratcombine = TRUE, landarea = "ALL", plt.filter = NULL, 
	cond.filter = NULL, sumunits = FALSE, rowvar=NULL, colvar=NULL, row.FIAname=FALSE, 
	col.FIAname=FALSE, row.orderby=NULL, col.orderby=NULL, row.add0=FALSE, 
	col.add0=FALSE, rowlut=NULL, collut=NULL, rowgrp=FALSE, rowgrpnm=NULL, 
	rowgrpord=NULL, allin1 = FALSE, estround = 0, pseround = 3, estnull = "--", 
	psenull = "--", divideby = NULL, savedata = FALSE, rawdata = FALSE, outfolder = NULL, 
	outfn = NULL, outfn.pre = NULL, outfn.date = TRUE, overwrite = FALSE, addtitle = TRUE, 
	returntitle = FALSE, title.main = NULL, title.ref = NULL, title.rowvar = NULL, 
	title.colvar = NULL, title.unitvar = NULL, title.filter = NULL, 
	MApopdat = NULL, MAdata = NULL, gui = FALSE)
}
 
\arguments{
  \item{cond}{ DF/DT, comma-separated values (CSV) file (*.csv), or layer in dsn. 
	The condition-level variables with one record per condition, including or 
	excluding nonsampled conditions. Plot variables and strata variable(s)
	may be included for post-stratification if plt and pltassgn=NULL. See details 
	for necessary variables to include. }
  \item{plt}{ DF/DT, comma-separated values (CSV) file(*.csv), or layer in dsn,
	Can also be a shapefile(*.shp) with one record per plot, a spatial layer in dsn,
	or a sf R object. Plot-level variables. If nonsampled plots are included, 
	PLOT_STATUS_CD variable must be in table. Optional. }
  \item{pltassgn}{ DF/DT, comma-delimited file(*.csv), SpatialDataFrame, or 
	shapefile(*.shp) with one record per plot, estimation unit variable(s) and 
	mean (continuous rasters) or proportions (categorical rasters) by 
	estimation unit. Optional. }
  \item{dsn}{ String. Name of database where cond and pltassgn tables reside. 
	The dsn varies by driver. See gdal OGR vector formats 	
	(https://www.gdal.org/ogr_formats.html). }
  \item{cuniqueid}{ String. Unique identifier of plot in cond. }
  \item{condid}{ String. Unique identifier of plot conditions. If no condid in 
	cond, assumed to have 1 condition per plot. A CONDID=1 is automatically added. }
  \item{puniqueid}{ String. Unique identifier of plot. }
  \item{pltassgnid}{ String. Unique identifier of plot. }
  \item{pjoinid}{ String. Name of variable in plot to join to pltassgn. This
	variable does not need to be unique (e.g., PLOT_ID). If NULL, pjoinid=puniqueid. }
  \item{evalid}{ Numeric. FIA Evaluation identifier for subsetting plots for population. }
  \item{invyrs}{ Integer vector. Inventory year(s) (e.g., c(2000, 2001, 2002)). }
  \item{ACI}{ Logical. If TRUE, including All Condition Inventory (ACI) plots. Removes 
	nonsampled nonforest lands (NF_COND_STATUS_CD = 5). Tree data must be included. }
  \item{adj}{ String. How to calculate adjustment factors for nonsampled 
	(nonresponse) conditions based on summed proportions for by plot ('samp', 'plot').
	'samp' - adjustments are calculated at strata/estimation unit level; 
	'plot' - adjustments are calculated at plot-level. Adjustments 
	are only calculated for annual inventory plots (designcd=1). } 
  \item{MAmethod}{ String. mase (i.e., model-assisted) method to use 
	('HT', 'PS', 'greg', 'gregEN') }
  \item{plt.nonsamp.filter}{ String. A logical expression for filtering nonsampled plots. 
	Must be R syntax. If plt.nonsamp.filter=NULL, a default is set to: 'PLOT_STATUS_CD != 3'. } 
  \item{cond.nonsamp.filter}{ String. A logical expression for filtering nonsampled conditions. 
	Must be R syntax. If cond.nonsamp.filter=NULL, a default is set to: 'COND_STATUS_CD != 5'; 
	if ACI: 'is.na(NF_COND_STATUS_CD) | NF_COND_STATUS_CD != 5'. }
  \item{unitvar}{ String. Name of the estimation unit variable in cond or pltstrat 
	with estimation unit assignment for each plot. If FIA, default="ESTUNIT". }
  \item{unitvar2}{ String. Name of a second estimation unit variable in cond or 
	pltstrat with assignment for each plot (e.g., 'STATECD'). }
  \item{unitarea}{ Numeric or DF. Total area by estimation unit. If only 1 estimation 
	unit, include number of total acreage for the area of interest or a data frame
	with areavar. If more than one estimation unit, provide a data frame of total 
	area by estimation unit, including unitvar and areavar. }
  \item{areavar}{ String. Name of acre variable in unitarea. Default="ACRES". }
  \item{unitcombine}{ Logical. If TRUE, automatically combines estimation units if less 
	than 2 plots in any one estimation unit. See notes for more info. }
  \item{minplotnum.unit}{ Integer. Minimum number of plots for estimation unit. }
  \item{unitlut}{ DF/DT. Table with ancillary information by estimation 
	unit. For continuous data, means by estimation unit; for categorical data, 
	proportion of class by estimation unit. }
  \item{npixelvar}{ String. Name of variable in unitlut with number of pixels by 
	estimation unit. }
  \item{prednames}{ String vector. Name(s) of predictor variables to use in model. }
  \item{predfac}{ String vector. Name(s) of factor predictor variables to use in model. }
  \item{PSstrvar}{ String. Name of strata variable in stratalut and table with strata
	assignment for each plot. Default="STRATUMCD". }
  \item{stratcombine}{ Logical. If TRUE, automatically combines estimation units if less 
	than 2 plots in any one estimation unit. See notes for more info. }
  \item{landarea}{ String. The sample area filter for estimates ('ALL', 'FOREST', 'TIMBERLAND'). 
	If landarea=FOREST, filtered to COND_STATUS_CD = 1; If landarea=TIMBERLAND, filtered 
	to SITECLCD in(1:6) and RESERVCD = 0. }
  \item{plt.filter}{ String. A filter for the pltstrat. Must be R syntax. }
  \item{cond.filter}{ String. A filter for the cond. Must be R syntax. }
  \item{sumunits}{ Logical. If TRUE, estimation units are summed and returned in
	one table. }
  \item{rowvar}{ String. Name of the row domain variable in cond or tree. If only one 
	domain, rowvar = domain variable. If more than one domain, include colvar. If no 
	domain, rowvar = NULL. }
  \item{colvar}{ String. Name of the column domain variable in cond or tree. }
  \item{row.FIAname}{ Logical. If TRUE, gets FIA reference names for row variable based on 
	ref_codes. Only available for certain variables. }
  \item{col.FIAname}{ Logical. If TRUE, gets FIA reference names for column variable based
	on ref_codes. Only available for certain variables. }
  \item{row.orderby}{ String. Name of variable to sort table rows. If row.FIAname=TRUE 
	and a ref_* exists for rowvar, the rowvar code is used to sort. If NULL, the 
	table is sorted by rowvar.}    
  \item{col.orderby}{ String. Name of variable to sort table columns. If col.FIAname=TRUE 
	and a ref_* exists for colvar, the colvar code is used to sort. If NULL, the 
	table is sorted by colvar.}    
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
	extension. Default file name includes parameter inputs 
	(e.g., tree_'estvar.name'_'rowvar'_'date') }
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
	name for filter(s). If title.filter=NULL, a default is generated from cond.filter. 
	If title.filter="", no title.filter is used. }
  \item{MApopdat}{ List. Population data objects returned from modMApop(). }
  \item{MAdata}{ List. Modeldat output from FIESTA::anMAdata(). }
  \item{gui}{ Logical. If gui, user is prompted for parameters. }
}
\details{
	If variables are NULL, then it will prompt user to input variables.

     	Necessary variables:\cr
	\tabular{llll}{
		\tab \bold{Data} \tab \bold{Variable}	\tab \bold{Description}\cr
	 	\tab tree \tab tuniqueid	\tab Unique identifier for each plot, to link to pltstrat 
								(ex. PLT_CN).\cr
		\tab  \tab CONDID		\tab Unique identifier of each condition on plot, to link to cond. 
								Set CONDID=1, if only 1 condition per plot.\cr
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

	Reference names are available for the following variables: \cr
		ADFORCD, AGENTCD, CCLCD, DECAYCD, DSTRBCD, KINDCD, OWNCD, OWNGRPCD, FORTYPCD, 
		FLDTYPCD, FORTYPCDCALC, TYPGRPCD, FORINDCD, RESERVCD, LANDCLCD, STDSZCD, FLDSZCD, 
		PHYSCLCD, MIST_CL_CD, PLOT_STATUS_CD, STATECD, TREECLCD, TRTCD, SPCD, SPGRPCD 

}
\value{
  If FIA=TRUE or unitvar=NULL and colvar=NULL, one data frame is returned with tree estimates 
	and percent sample errors. Otherwise, a list is returned with tree estimates in one 
	data frame (est) and percent sample errors in another data frame (est.pse). If
	rawdata=TRUE, another list is returned including raw data used in the estimation process.
     If addtitle=TRUE and returntitle=TRUE, the title for est/pse is returned. If savedata=TRUE, 
	all data frames are written to outfolder.

  \item{est}{ Data frame. Tree estimates by rowvar, colvar (and estimation unit). If 
	FIA=TRUE or one estimation unit and colvar=NULL, estimates and percent sampling 
	error are in one data frame. }
  \item{pse}{ Data frame. Percent sampling errors for estimates by rowvar and colvar 
	(and estimation unit). }
  \item{titlelst}{ List with 1 or 2 string vectors. If returntitle=TRUE a list with
	table title(s). The list contains one title if est and pse are in the same table 
	and two titles if est and pse are in separate tables. }
  \item{raw}{ List of data frames. If rawdata=TRUE, a list including: number of plots by
	plot status, if in dataset (plotsampcnt); number of conditions by condition status
	(condsampcnt); data used for post-stratification (stratdat); and 1-8 tables 
	with calculated variables used for processing estimates and percent sampling error
	for table cell values and totals (See processing data below). }


  Raw data

  \item{plotsampcnt}{ Table. Number of plots by plot status (ex. sampled forest on plot, 
	sampled nonforest, nonsampled). }
  \item{condsampcnt}{ DF. Number of conditions by condition status (forest land,
	nonforest land, noncensus water, census water, nonsampled). }

  \item{stratdat}{ Data frame. Strata information by estimation unit. }
	\tabular{lll}{
		\tab \bold{Variable}	\tab \bold{Description}\cr
	 	\tab ESTUNIT		\tab estimation unit\cr
	 	\tab STRATA			\tab strata \cr
	 	\tab ACRES			\tab area by strata for estimation unit\cr
	 	\tab n.strata		\tab number of plots in strata (and estimation unit) \cr
	 	\tab n.total		\tab number of plots for estimation unit \cr
	 	\tab TOTACRES		\tab total area for estimation unit \cr
	 	\tab strwt			\tab proportion of area (or number of plots) by strata (strata weight) \cr
	 	\tab expfac.strata	\tab expansion factor (in area unit (e.g., acres) by strata (areavar/n.strata) \cr
	}

  \item{processing data}{ Data frames. Separate data frames containing calculated variables 
	used in estimation process. The number of processing tables depends on the input
	parameters. The tables include: total by estimation unit (unit.totest); rowvar totals
 	(unit.rowest), and if colvar is not NULL, colvar totals, (unit.colvar); and a combination 
	of rowvar and colvar (unit.grpvar). If FIA=TRUE, the raw data for the summed estimation 
	units are also included (totest, rowest, colest, grpest, respectively). These tables do 
	not included estimate proportions (nhat and nhat.var). 

	The data frames include the following information:
	\tabular{lll}{
		\tab \bold{Variable}	\tab \bold{Description}\cr
	 	\tab nhat		\tab estimated proportion of trees \cr
	 	\tab nhat.var	\tab estimated variance of estimated proportion of trees \cr
	 	\tab ACRES		\tab total area for estimation unit \cr
	 	\tab est		\tab estimated area of trees { nhat*ACRES } \cr
	 	\tab est.var	\tab estimated variance of estimated area of trees { nhat.var*areavar^2 } \cr
	 	\tab est.se	\tab standard error of estimated area of trees { sqrt(est.var) } \cr
	 	\tab est.cv	\tab coefficient of variation of estimated area of trees { est.se/est } \cr
	 	\tab pse		\tab percent sampling error of estimate { est.cv*100 } \cr
	 	\tab CI99left	\tab left tail of 99 percent confidence interval for estimated area \cr
	 	\tab CI99right	\tab right tail of 99 percent confidence interval for estimated area \cr
	 	\tab CI95left	\tab left tail of 95 percent confidence interval for estimated area \cr
	 	\tab CI95right	\tab right tail of 95 percent confidence interval for estimated area \cr
	 	\tab CI67left	\tab left tail of 67 percent confidence interval for estimated area \cr
	 	\tab CI67right	\tab right tail of 67 percent confidence interval for estimated area \cr
	}
  }
  
  Table(s) are also written to outfolder.
}
\author{
	Tracey S. Frescino
}
\references{
	Kelly McConville, Becky Tang, George Zhu, Shirley Cheung, and Sida Li (2018). mase: Model-Assisted 
	Survey Estimation. R package version 0.1.2 https://cran.r-project.org/package=mase
}

\note{
 
	ADJUSTMENT FACTOR:\cr
	The adjustment factor is necessary to account for nonsampled conditions. It is calculated 
		for each estimation unit by strata. by summing the unadjusted proportions of the 
		subplot, microplot, and macroplot (i.e. *PROP_UNADJ) and dividing by the number 
		of plots in the strata/estimation unit).

	An adjustment factor is determined for each tree based on the size of the plot it was 
	measured on. This is identified using TPA_UNADJ as follows: 

	\tabular{llr}{
	\tab \bold{PLOT SIZE}	\tab \bold{TPA_UNADJ} \cr
	\tab SUBPLOT	\tab 6.018046 \cr
	\tab MICROPLOT	\tab 74.965282 \cr
	\tab MACROPLOT	\tab 0.999188 \cr }

	If ACI=FALSE, only nonsampled forest conditions are accounted for in the adjustment factor. \cr
	If ACI=TRUE, the nonsampled nonforest conditions are removed as well and accounted for in adjustment factor.
	This is if you are interested in estimates for all lands or nonforest lands in the All-Condition-Inventory.


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
	If MAmethod='PS', and stratcombine=TRUE, and less than 2 plots in any one estimation unit, 
	all estimation units with 10 or less plots are combined. The current method for combining 
	is to group the estimation unit with less than 10 plots with the estimation unit following 
	in consecutive order (numeric or alphabetical), restrained by survey unit (UNITCD) if 
	included in dataset, and continuing until the number of plots equals 10. If there are 
	no estimation units following in order, it is combined with the estimation unit previous 
	in order. 

	autoxreduce:\cr
	If MAmethod='GREG', and autoxreduce=TRUE, and there is an error because of multicolinearity,
	a variable reduction method is applied to remove correlated variables. The method used
	is based on the variance-inflation factor (vif) from a linear model. The vif estimates
	how much the variance of each x variable is inflated due to mulitcolinearity in the model.

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


     UNITS:\cr
     The following variables are converted from pounds (from FIA database) to short tons by 
	multiplying the variable by 0.0005.
     DRYBIO_AG, DRYBIO_BG, DRYBIO_WDLD_SPP, DRYBIO_SAPLING, DRYBIO_STUMP, DRYBIO_TOP, DRYBIO_BOLE, 
	DRYBIOT, DRYBIOM, DRYBIOTB, JBIOTOT, CARBON_BG, CARBON_AG 


	MORTALITY:\cr
     For Interior-West FIA, mortality estimates are mainly based on whether a tree has died 
		within the last 5 years of when the plot was measured. If a plot was remeasured, 
		mortality includes trees that were alive the previous visit but were dead in the 
		next visit. If a tree was standing the previous visit, but was not standing in the 
		next visit, no diameter was collected (DIA = NA) but the tree is defined as mortality. 


	Common tree filters: \cr

	\tabular{llr}{
	\tab \bold{FILTER}		\tab \bold{DESCRIPTION} \cr
 	\tab "STATUSCD == 1"		\tab Live trees \cr	
	\tab "STATUSCD == 2"		\tab Dead trees \cr	
	\tab "TPAMORT_UNADJ > 0"	\tab Mortality trees \cr
	\tab "STATUSCD == 2 & DIA >= 5.0"	\tab Dead trees >= 5.0 inches diameter \cr
	\tab "STATUSCD == 2 & AGENTCD == 30"	\tab Dead trees from fire \cr }
}

\keyword{ data }
