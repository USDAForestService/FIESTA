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
DBgetEvalid(states = NULL, RS = NULL, invyrtab = NULL, invtype = "ANNUAL", 
	evalCur = TRUE, evalEndyr = NULL, evalid = NULL, evalAll = FALSE, 
	evalType = "AREAVOL", isveg = FALSE, isdwm = FALSE, gui = FALSE, returnPOP = TRUE)
}
\arguments{
  \item{states}{ String or numeric vector. Name(s) (e.g., 'Arizona','New Mexico') or 
	code(s) (e.g., 4, 35) of states for evalid. }
  \item{RS}{ String vector. Name of research station(s) ('RMRS','SRS','NCRS','NERS','PNWRS'). }
  \item{invyrtab}{ Data frame. A data frame including inventory years by state. If NULL,
	it is generated from SURVEY table from FIA database based on states and invtype. }
  \item{invtype}{ String. The type of FIA data to extract ('PERIODIC', 'ANNUAL').  
	See further details below. }
  \item{evalCur}{ Logical. If TRUE, the most current evalidation is extracted for state(s). }
  \item{evalEndyr}{ Number. The last year of the evaluation period of interest. Selects
	only sampled plots and conditions for the evalidation period ending in the specified 
	year. If more than one state, create a named list object with evalEndyr
	labeled for each state (ex. list(Utah=2014, Colorado=2013). }
  \item{evalid}{ Integer. To check an already existing evalid. }
  \item{evalAll}{ Logical. If TRUE, gets all EVALIDs for invtype. }
  \item{evalType}{ String vector. The type(s) of evaluation of interest 
	('ALL', 'AREAVOL', 'GRM', 'P2VEG', 'DWM", 'INV', 'REGEN', 'CRWN'). 
	The evalType 'ALL' includes nonsampled plots; 'AREAVOL' includes plots used
	for area or tree estimates (eval_typ %in% c(CURR, VOL)); The evalType 'GRM'
	includes plots used for growth, removals, mortality, and change estimates
	(eval_typ %in% c(GROW, MORT, REMV, CHNG)). Multiple types are accepted. 
	See FIA database manual for regional availability and/or differences. }
  \item{isveg}{ Logical. If TRUE, the Evaluation for understory vegetation is added. }
  \item{isdwm}{ Logical. If TRUE, the Evaluation for DWM is added. }
  \item{gui}{ Logical. If TRUE, gui windows pop up for parameter selection. }
  \item{returnPOP}{ Logical. If TRUE, returns population table (SURVEY, POP_PLOT_STRATUM_ASSGN). }
}

\value{
  returnlst - a list of the following objects:
  \item{states}{ String vector. State names. }
  \item{rslst}{ String vector. FIA research station names included in output. }
  \item{evalidlist}{ Named list. evalid by state. }
  \item{invtype}{ String. Inventory type for states(s) (ANNUAL/PERIODIC). }
  \item{invyrtab}{ Data frame. Inventory years by state for evalidlist. }
  \item{evalTypelist}{ Named list. Evaluation type(s) by state. }
  \item{invyrs}{ Named list. Inventory years by state for evalidlist. }
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
