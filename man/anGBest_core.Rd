\name{anGBest_core}
\alias{anGBest_core}
\title{
	ANALYSIS - Generate core tables for Green-Book.
}
\description{
	Generates a set of core tables based on state of interest.
}
\usage{
anGBest_core(state, datsource = "ORACLE", FS_FIADB = FALSE, evalCur = TRUE,
	evalEndyr = NULL, evalType=c("all", "areavol", "grm"), fill = TRUE, 
	addSEcol = FALSE, allin1 = FALSE, outfolder=NULL, datPlots = NULL, 
	datStrata = NULL, ref_countycd = NULL, ref_unitcd = NULL, 
	returndata = FALSE, savedata = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state}{ String. The name of state for tables (ex. "Vermont", "Utah", etc.) }
  \item{datsource}{ String. Source of FIA data ("ORACLE", "CSV"). 
	If datsource="ORACLE", select permission of FIA Oracle tables must be granted to 
	user and an ODBC connection set up on computer. Use DBtestORACLE function to 
	test connection to ORACLE. If datsource="CSV", *.csv or *.zip (if ZIP=TRUE) files 
	are downloaded and extracted from FIA DataMart 
	(http://apps.fs.usda.gov/fia/datamart/datamart.html). If datsource is equal to 
	CSV, the actual coordinates are not available. }
  \item{FS_FIADB}{ Logical. If datsource="ORACLE". If TRUE, data are extracted from FIA's 
	national public database (Oracle - FS_FIADB schema). If FALSE, data are extracted 
	from the regional FIADB (Oracle FS_NIMS_FIADB_'NIMS_UNIT' schema). }
  \item{evalCur}{ Logical. If TRUE, the most current FIA Evalidation for state(s). }
  \item{evalEndyr}{ Integer. The last year of the evaluation period of interest. Selects
	only sampled plots and conditions for the evalidation period ending in the specified 
	year. If more than one state, create a named list object with years labeled
	by state (ex. list(Utah=2014, Colorado=2013). }
  \item{evalType}{ String. The type of evaluation of interest ("all", "area/vol", 
	"grow/mort", "dwm"). 'all' includes nonsampled plots (*00); 'area/vol' includes 
	plots used for area and volume estimates (*01); 'grow/mort' includes plots used 
	for growth/mortality/removal estimates or change in all FIA research stations 
	except RMRS (*03); 'dwm' includes plots used for down woody material estimates (*07). 
	Multiple types accepted. See notes for more information on evalType. }
  \item{fill}{ Logical. If TRUE, color fill is applied to all tables. }
  \item{addSEcol}{ Logical. If TRUE, percent standard error values are added to
		columns side-by-side. }
  \item{allin1}{ Logical. If TRUE, percent standard error values are added to
		same cell as estimates. }
  \item{outfolder}{ String. The path of folder to output tables. }
  \item{datPlots}{ R object. Plot data. }
  \item{datStrata}{ R object. Strata data. }
  \item{ref_countycd}{ R object. Reference names for counties. }
  \item{ref_unitcd}{ R object. Reference names for survey units. }
  \item{returndata}{ Logical. If TRUE, returns data objects. }
  \item{savedata}{ Logical. If TRUE, saves data objects to outfolder (*.rda) }
  \item{...}{ Other parameters to DBgetPlots. }
}
\details{
	If variables are NULL, then it will prompt user to input variables.
		
}
\author{
	Tracey S. Frescino
}

\keyword{ data }
