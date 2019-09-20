\name{anGBest_core_data}
\alias{anGBest_core_data}
\title{
	ANALYSIS - Extract data for core tables from FIA database.
}
\description{
	Extract data for core tables from FIA database.
}
\usage{
anGBest_core_data(state, datsource = "ORACLE", FS_FIADB = TRUE, evalCur = TRUE,
	evalEndyr = NULL, evalType = c("ALL", "AREAVOL", "GRM"), outfolder=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state}{ String. The name of state for tables (e.g., 'Vermont', 'Utah') }
  \item{datsource}{ String. Source of FIA data ('ORACLE', "CSV'). 
	If datsource="ORACLE", select permission of FIA Oracle tables must be granted to 
	user and an ODBC connection set up on computer. Use DBtestORACLE function to 
	test connection to ORACLE. If datsource="CSV", *.csv or *.zip (if ZIP=TRUE) files 
	are downloaded and extracted from FIA DataMart 
	(http://apps.fs.usda.gov/fia/datamart/datamart.html). If datsource is equal to 
	CSV, the actual coordinates are not available. }
  \item{FS_FIADB}{ Logical. If datsource='ORACLE'. If TRUE, data are extracted from FIA's 
	national public database (Oracle - FS_FIADB schema). If FALSE, data are extracted 
	from the regional FIADB (Oracle FS_NIMS_FIADB_'NIMS_UNIT' schema). }
  \item{evalCur}{ Logical. If TRUE, the most current FIA Evalidation for state(s). }
  \item{evalEndyr}{ Integer. The last year of the evaluation period of interest. Selects
	only sampled plots and conditions for the evalidation period ending in the specified 
	year. If more than one state, create a named list object with years labeled
	by state (ex. list(Utah=2014, Colorado=2013). }
  \item{evalType}{ String vector. The type(s) of evaluation of interest 
	('ALL', 'AREAVOL', 'GRM', 'P2VEG', 'DWM', 'INV', 'REGEN', 'CRWN'). 
	The evalType 'ALL' includes nonsampled plots; 'AREAVOL' includes plots used
	for area or tree estimates (eval_typ %in% c(CURR, VOL)); The evalType 'GRM'
	includes plots used for growth, removals, mortality, and change estimates
	(eval_typ %in% c(GROW, MORT, REMV, CHNG)). Multiple types are accepted. 
	See FIA database manual for regional availability and/or differences. 
	See notes for more information on evalType. }
  \item{outfolder}{ String. The path of folder to output tables. }
}
\details{
	If variables are NULL, then it will prompt user to input variables.
		
}
\author{
	Tracey S. Frescino
}

\keyword{ data }
