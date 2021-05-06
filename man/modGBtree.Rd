\name{modGBtree}
\alias{modGBtree}
\title{
	Green-Book module - Generate tree estimates.
}
\description{
	Generates tree and or seedling estimates by domain and/or tree domain (and estimation unit). 
	Calculations are based on Scott et al. 2005 ('the green-book') for mapped forest 
	inventory plots. The non-ratio estimator for estimating tree attributes
	by stratum and domain is used. Plots that are totally nonsampled are excluded from 
	estimation dataset. Next, an adjustment factor is calculated by strata to adjust for 
	nonsampled (nonresponse) conditions that have proportion less than 1. Attributes 
	adjusted to a per-acre value are summed by plot, divided by the adjustment factor, 
	and averaged by stratum. Strata means are combined using the strata weights and then 
	expanded to using the total land area in the population. 
}
\usage{    
modGBtree(GBpopdat = NULL, estseed = "none", landarea = "FOREST", 
	pcfilter = NULL, estvar = NULL, estvar.filter = NULL, 
	rowvar = NULL, colvar = NULL, row.FIAname = FALSE, col.FIAname = FALSE, 
	row.orderby = NULL, col.orderby = NULL, row.add0 = FALSE, col.add0 = FALSE, 
	rowlut = NULL, collut = NULL, rowgrp = FALSE, rowgrpnm = NULL, rowgrpord = NULL, 
	sumunits = TRUE, allin1 = FALSE, estround = 1, pseround = 2, 
	estnull = "--", psenull = "--", divideby = NULL, savedata = FALSE, 
	outfolder = NULL, outfn.pre = NULL, outfn.date = TRUE, addtitle = TRUE, 
	rawdata = FALSE, rawonly = FALSE, raw_fmt = "csv", raw_dsn = NULL, 
	overwrite_dsn = FALSE, overwrite_layer = TRUE, append_layer = FALSE, 
	returntitle = FALSE, title.main = NULL, title.ref = NULL, title.rowvar = NULL, 
	title.colvar = NULL, title.unitvar = NULL, title.estvar = NULL, title.filter = NULL, 
	gui = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{GBpopdat}{ List. Population data objects returned from FIESTA::modGBpop(). }
  \item{estseed}{ String. Use seedling data only or add to tree data. Seedling 
	estimates are only for counts (estvar='TPA_UNADJ')-('none', 'only', 'add'). }
  \item{landarea}{ String. The condition-level filter for defining land area 
	('ALL', 'FOREST', 'TIMBERLAND'). If landarea='FOREST', COND_STATUS_CD = 1; 
	if landarea='TIMBERLAND', SITECLCD in(1:6) & RESERVCD = 0. }
  \item{pcfilter}{ String. A filter for plot or cond attributes (including pltassgn). 
	Must be R logical syntax. }
  \item{estvar}{ String. Name of the tree-level estimate variable (e.g., 'VOLCFNET'). }
  \item{estvar.filter}{ String. A tree-level filter for estvar. Must be R syntax 
	(e.g., 'STATUSCD == 1'). }
  \item{rowvar}{ String. Optional. Name of domain variable to group estvar by 
	for rows in table output. Rowvar must be included in an input data frame 
	(i.e., plt, cond, tree). If no rowvar is included, an estimate is returned
	for the total estimation unit. Include colvar for grouping by 2 variables. }
  \item{colvar}{ String. Optional. If rowvar != NULL, name of domain variable to 
	group estvar by for columns in table output. Colvar must be included in an 
	input data frame (i.e., plt, cond, tree). }
  \item{row.FIAname}{ Logical. If TRUE, retrieves default FIA reference names for 
	rowvar located in FIESTA::ref_codes data frame. Names are only available for 
	certain variables (Check sort(unique(FIESTA::ref_codes$VARIABLE)) for available names.
  	If row.FIAname = TRUE and rowvar is in FIESTA::ref_codes, the rowvar name
	is used for the output table, and the rowvar code is used to sort. }
  \item{col.FIAname}{ Logical. If TRUE, retrieves default FIA reference names for 
	colvar located in FIESTA::ref_codes data frame. Names are only available for 
	certain variables. Check: sort(unique(FIESTA::ref_codes$VARIABLE)) for available names. 
	If col.FIAname = TRUE and rowvar is in FIESTA::ref_codes, the colvar name
	is used for the output table, and the colvar code is used to sort. }
  \item{row.orderby}{ String. Optional. Name of variable to sort table rows. Both
	the rowvar and row.orderby variables must be included in the same input data.frame. 
	if NULL, and row.FIAname=FALSE or rowvar is not in FIESTA::ref_codes, the rows
	are ordered by rowvar. }
  \item{col.orderby}{ String. Optional. Name of variable to sort table columns. Both
	the colvar and col.orderby variables must be included in the same input data.frame. 
	if NULL, and col.FIAname=FALSE or colvar is not in FIESTA::ref_codes, the columns
	are ordered by colvar. }
  \item{row.add0}{ Logical. If TRUE, include rows with 0 values to the output table. }
  \item{col.add0}{ Logical. If TRUE, include columns with 0 values to the output table. }  
  \item{rowlut}{ Data frame. A lookup table with variable codes and code names to
	include as rows of output table (See notes for more information and format). }
  \item{collut}{ Data frame. A lookup table with variable codes and code names to
	include as columns of output table (See notes for more information and format). }
  \item{rowgrp}{ Logical. If TRUE, appends row groups to first column of table. 
	Only available if group category exists in ref_codes table or defined in rowgrpnm 
	(e.g., FORTYPGRPCD, OWNGRPCD). }   
  \item{rowgrpnm}{ String. Name of variable for grouping rowvar. Variable must be
	included in same input table as rowvar. }  
  \item{rowgrpord}{ String. Name of variable to sort row group variable. Variable 
	must be included in same input table as rowgrpnm. }  
  \item{sumunits}{ Logical. If TRUE, estimation units are summed and returned in
	one table. }
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
  \item{outfolder}{ String. The outfolder to write files to. If NULL, files are 
	written to working directory, or if gui, a window to browse. }
  \item{outfn.pre}{ String. If savedata=TRUE, prefix for output files. If rawdata=TRUE, 
	prefix for rawdata files (if raw_fmt = 'csv') or raw_dsn (if raw_fmt != 'csv'). }
  \item{outfn.date}{ Logical. If TRUE, add current date to out_dsn. }
  \item{addtitle}{ Logical. If TRUE and savedata=TRUE, adds title to outfile. }
  \item{rawdata}{ Logical. If TRUE, returns a list of raw data tables that are used
	for estimation (See Value). If savedata = TRUE, tables are written to outfolder 
	(if raw_fmt='csv') or raw_dsn (if raw_fmt != 'csv'). }
  \item{rawonly}{ Logical. If TRUE, only rawdata are output. If dataset includes
	many estimation units, and only raw data tables are desired, it is more 
	efficient to output raw data only. }
  \item{raw_fmt}{ String. Format for output rawdata tables 
	('sqlite', 'sqlite3', 'db', 'db3', 'gpkg', 'csv', 'gdb', 'shp'). }
  \item{raw_dsn}{ String. Data source name for rawdata output. If extension is not
	included, out_fmt is used. Use full path if outfolder=NULL. }
  \item{overwrite_dsn}{ Logical. If TRUE, overwrites raw_dsn, if exists. }
  \item{overwrite_layer}{ Logical. If TRUE, overwrites the output. If rawdata=TRUE, 
	overwrites out_layer in rawdata folder (if raw_fmt = 'csv') or out_layers in 
	raw_dsn (if raw_fmt != 'csv'). }
  \item{append_layer}{ Logical. If TRUE, and rawdata=TRUE, appends raw data 
	to existing *.csv files (if raw_fmt = 'csv') or raw_dsn layers (if raw_fmt != 'csv". }  
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
  \item{title.estvar}{ String. TITLE: if savedata=TRUE and/or returntitle=TRUE: pretty 
	name for the estimate variable. If NULL, title.estvar = estvar.name. }
  \item{title.filter}{ String. TITLE, if savedata=TRUE and/or returntitle=TRUE: pretty 
	name for filter(s). If title.filter=NULL, a default is generated from cfilter. 
	If title.filter="", no title.filter is used. }
  \item{gui}{ Logical. If gui, user is prompted for parameters. }
  \item{...}{ Parameters for modGBpop() if GBpopdat is NULL. }
}
\details{
	If variables are NULL, then it will prompt user to input variables.

     	Necessary variables:\cr
	\tabular{llll}{
		\tab \bold{Data} \tab \bold{Variable}	\tab \bold{Description}\cr
	 	\tab tree \tab tuniqueid	\tab Unique identifier for each plot, to link to pltassgn 
								(ex. PLT_CN).\cr
		\tab  \tab CONDID		\tab Unique identifier of each condition on plot, to link to cond. 
								Set CONDID=1, if only 1 condition per plot.\cr
		\tab  \tab TPA_UNADJ		\tab Number of trees per acre each sample tree represents
								(ex. DESIGNCD=1: TPA_UNADJ=6.018046 for trees on subplot;
								74.965282 for trees on microplot).\cr
	 	\tab cond \tab cuniqueid	\tab Unique identifier for each plot, to link to pltassgn 
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
		\tab  \tab SUBPROP_UNADJ	\tab Unadjusted proportion of subplot conditions on each plot. 
								Set SUBPROP_UNADJ=1, if only 1 condition per subplot.\cr
		\tab  \tab MICRPROP_UNADJ	\tab If microplot tree attributes. Unadjusted proportion 
							of microplot conditions on each plot. Set MICRPROP_UNADJ=1, 
							if only 1 condition per microplot.\cr
		\tab  \tab MACRPROP_UNADJ	\tab If macroplot tree attributes. Unadjusted proportion 
							of macroplot conditions on each plot. Set MACRPROP_UNADJ=1, 
							if only 1 condition per macroplot.\cr
	 	\tab pltassgn \tab puniqueid	\tab Unique identifier for each plot, to link to cond 
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

  \item{est}{ Data frame. Tree estimates by rowvar, colvar (and estimation unit). If 
	sumunits=TRUE or one estimation unit and colvar=NULL, estimates and percent sampling 
	error are in one data frame. }
  \item{pse}{ Data frame. Percent sampling errors (Confidence level 68%) for estimates by 
	rowvar and colvar (and estimation unit). Note: for 95% confidence level, multiply
	percent sampling error by 1.96. }
  \item{titlelst}{ List with 1 or 2 string vectors. If returntitle=TRUE a list with
	table title(s). The list contains one title if est and pse are in the same table 
	and two titles if est and pse are in separate tables. }
  \item{raw}{ List of data frames. If rawdata=TRUE, a list including the processing
	data used for estimation including: number of plots and conditions; stratification 
	information; and 1 to 8 tables with calculated values for table cells and totals 
	(See processing data below). }


  Raw data

  \item{plotsampcnt}{ Table. Number of plots by plot status (ex. sampled forest on plot, 
	sampled nonforest, nonsampled). }
  \item{condsampcnt}{ DF. Number of conditions by condition status (forest land,
	nonforest land, noncensus water, census water, nonsampled). }
  \item{unitarea}{ DF. Area by estimation unit. }
  \item{expcondtab}{ DF. Condition-level area expansion factors. }
  \item{tdomdat}{ DF. Final data table used for estimation. }

  \item{stratdat}{ Data frame. Strata information by estimation unit. }
	\tabular{lll}{
		\tab \bold{Variable}	\tab \bold{Description}\cr
	 	\tab unitvar		\tab estimation unit \cr
	 	\tab strvar		\tab stratum value \cr
	 	\tab strwtvar	\tab number of pixels by strata and estimation unit \cr
	 	\tab n.strata	\tab number of plots in strata (after totally nonsampled plots removed) \cr
	 	\tab n.total	\tab number of plots for estimation unit \cr
	 	\tab strwt		\tab proportion of area (or plots) by strata and estimation unit (i.e., strata weight) \cr
	 	\tab CONDPROP_UNADJ_SUM	\tab summed condition proportion by strata and estimation unit \cr
	 	\tab CONDPROP_ADJFAC	\tab adjusted condition proportion by strata after nonsampled plots removed \cr
	}

  \item{processing data}{ Data frames. Separate data frames containing calculated variables 
	used in estimation process. The number of processing tables depends on the input
	parameters. The tables include: total by estimation unit (unit.totest); rowvar totals
 	(unit.rowest), and if colvar is not NULL, colvar totals, (unit.colvar); and a combination 
	of rowvar and colvar (unit.grpvar). If sumunits=TRUE, the raw data for the summed estimation 
	units are also included (totest, rowest, colest, grpest, respectively). These tables do 
	not included estimate proportions (nhat and nhat.var). 

	The data frames include the following information:
	\tabular{lll}{
		\tab \bold{Variable}	\tab \bold{Description}\cr
	 	\tab nhat		\tab estimated proportion of trees \cr
	 	\tab nhat.var	\tab variance estimate of estimated proportion of trees \cr
	 	\tab NBRPLT.gt0	\tab Number of non-zero plots used in estimates \cr
	 	\tab ACRES		\tab total area for estimation unit \cr
	 	\tab est		\tab estimated area of trees { nhat*ACRES } \cr
	 	\tab est.var	\tab variance estimate of estimated area of trees { nhat.var*areavar^2 } \cr
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
	Tracey S. Frescino, Paul L. Patterson, Elizabeth A. Freeman
}
\references{
	Scott, Charles T.; Bechtold, William A.; Reams, Gregory A.; Smith, William D.; Westfall, James A.; 
	Hansen, Mark H.; Moisen, Gretchen G. 2005. Sample-based estimators used by the Forest Inventory and
	Analysis national information management system. Gen. Tech. Rep. SRS-80. Asheville, NC: U.S. Department
	of Agriculture, Forest Service, Southern Research Station, p.53-77.
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
	If ACI=TRUE, the nonsampled nonforest conditions are removed as well and accounted 
		for in adjustment factor.
	This is if you are interested in estimates for all lands or nonforest lands in the 
		All-Condition-Inventory.

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

\examples{

  ## Rows only; combine estimation units (sumunits=TRUE)
  MODest <- modGBtree(tree=WYtree, cond=WYcond, pltassgn=WYpltassgn, pltassgnid="CN", 
	unitarea=WYunitarea, unitvar="ESTN_UNIT", stratalut=WYstrlut, sumunits=TRUE, 
	landarea="FOREST", estvar="VOLCFNET", estvar.filter="STATUSCD==1", 
	rowvar="FORTYPCD", row.FIAname=TRUE)
  names(MODest)
  MODest$est

  ## Rows only; by estimation units (allin1=TRUE)
  MODest <- modGBtree(tree=WYtree, cond=WYcond, pltassgn=WYpltassgn, pltassgnid="CN", 
	unitarea=WYunitarea, unitvar="ESTN_UNIT", stratalut=WYstrlut, sumunits=TRUE, 
	landarea="FOREST", estvar="VOLCFNET", estvar.filter="STATUSCD==1", 
	rowvar="FORTYPCD", row.FIAname=TRUE, allin1=TRUE)
  names(MODest)
  MODest$est

}

\keyword{ data }
