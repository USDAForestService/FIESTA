\name{DBgetEvalid}
\alias{DBgetEvalid}
\title{
	Database - Gets or checks evalid and gets inventory years from FIA database.
}
\description{
	Extracts an evalid for identifying an estimation group of plots. If evalCur=TRUE, 
	extracts current evalid and inventory years for state(s), or for a specified 
	evalEndyr. Otherwise checks an evalid and returns states and inventory years. 
}
\usage{
DBgetEvalid(states = NULL, rs = NULL, datsource = "CSV", ZIP = TRUE, FS_FIADB = TRUE, 
	invyrtab = NULL, invtype = "ANNUAL", evalCur = TRUE, evalEndyr = NULL, 
	evalid = NULL, evalType = "all", dbconn = NULL, dbconnopen = FALSE, 
	isdwm = FALSE, gui = FALSE)
}
\arguments{
  \item{states}{ String or numeric vector. Name(s) (e.g., "Arizona","New Mexico") or 
	code(s) (e.g., 4, 35) of states for evalid. }
  \item{rs}{ String vector. Name of research station(s) ("RMRS","SRS","NCRS","NERS","PNWRS"). }
  \item{datsource}{ String. Source of FIA data ("ORACLE", "CSV"). 
	If datsource="ORACLE", select permission of FIA Oracle tables must be granted to 
	user and an ODBC connection set up on computer. Use DBtestORACLE function to 
	test connection to ORACLE. If datsource="CSV", *.csv or *.zip (if ZIP=TRUE) files 
	are downloaded and extracted from FIA DataMart 
	(http://apps.fs.usda.gov/fia/datamart/datamart.html). If datsource is equal to 
	CSV, the actual coordinates are not available. }
  \item{ZIP}{ Logical. If TRUE, Zip files (*.zip) are downloaded from FIA DataMart, 
	saved to a temporary space, extracted, and deleted from temporary space. }
  \item{FS_FIADB}{ Logical. If TRUE, data are extracted from FIA's national public 
	database (Oracle - FS_FIADB schema). If FALSE, data are extracted from the regional 
	FIADB (Oracle FS_NIMS_FIADB_'NIMS_UNIT' schema). }
  \item{invyrtab}{ Data frame. A data frame including inventory years by state. If NULL,
	it is generated from SURVEY table from FIA database based on states and invtype. }
  \item{invtype}{ String. The type of FIA data to extract ("PERIODIC", "ANNUAL").  
	See further details below. }
  \item{evalCur}{ Logical. If TRUE, the most current evalidation is extracted for state(s). }
  \item{evalEndyr}{ Number. The last year of the evaluation period of interest. Selects
	only sampled plots and conditions for the evalidation period ending in the specified 
	year. If more than one state, create a named list object with evalEndyr
	labeled for each state (ex. list(Utah=2014, Colorado=2013). }
  \item{evalid}{ Integer. To check an already existing evalid. }
  \item{evalType}{ String vector. The type(s) of evaluation of interest ("all", "areavol", 
	"grm", "dwm"). 'all' includes nonsampled plots (*00); 'areavol' includes 
	plots used for area and volume estimates (*01); 'growmort' includes plots used 
	for growth/mortality/removal estimates or change in all FIA research stations 
	except RMRS (*03); 'dwm' includes plots used for down woody material estimates (*07). 
	Multiple types accepted. }
  \item{dbconn}{ Database connection. The connection information to the FIA Oracle database. }
  \item{dbconnopen}{ Logical. If TRUE, the dbconn connection is not closed. }
  \item{isdwm}{ Logical. If TRUE, summarized down woody debris data are extracted from 
	FIA database (COND_DWM_CALC). }
  \item{gui}{ Logical. If TRUE, gui windows pop up for parameter selection. }
}

\value{
  returnlst - a list of the following objects:
  \item{states}{ String vector. State names. }
  \item{rslst}{ String vector. FIA research station names included in output. }
  \item{evalidlist}{ Named list. evalid by state. }
  \item{invtype}{ String. Inventory type for states(s) (ANNUAL/PERIODIC). }
  \item{invyrtab}{ Data frame. Inventory years by state for evalidlist. }
  \item{evalType}{ String vector. Evaluation type(s) included in query. }
  \item{invyrs}{ Named list. Inventory years by state for evalidlist. }
  \item{datsource}{ String. The datsource used for query (ORACLE or CSV). }
  \item{FS_FIADB}{ Logical. TRUE, if datsource=ORACLE and FS_FIADB schema is used. }
}

\note{
	FIA database tables used:
	1. SURVEY	- To get latest inventory year, invyrtab = NULL
	2. POP_EVAL - To get EVALID and EVALID years
}

\author{
	Tracey S. Frescino
}

\examples{

  \dontrun{

    ## Get evalid and inventory years for Wyoming
    WYeval <- DBgetEvalid(states="Wyoming")
    names(WYeval)

    WYeval$evalidlist
    WYeval$invtype
    WYeval$invyrtab
    WYeval$evalType
    WYeval$invyrs


    ## Get evalid for Utah and Wyoming (from CSV files)
    DBgetEvalid(states=c("Wyoming", "Utah"), datsource="CSV")
  }
}

\keyword{ data }
