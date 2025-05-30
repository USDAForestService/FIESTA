% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DBgetEvalid.R
\name{DBgetEvalid}
\alias{DBgetEvalid}
\title{Database - Gets or checks FIA EVALIDs and/or gets inventory years from FIA's
online publicly-available DataMart
(https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html).}
\usage{
DBgetEvalid(
  states = NULL,
  RS = NULL,
  datsource = "datamart",
  data_dsn = NULL,
  invtype = "ANNUAL",
  evalCur = TRUE,
  evalEndyr = NULL,
  evalid = NULL,
  evalAll = FALSE,
  evalType = "VOL",
  invyrtab = NULL,
  dbTabs = dbTables(),
  dbconn = NULL,
  schema = NULL,
  dbconnopen = FALSE,
  returnPOP = FALSE,
  gui = FALSE
)
}
\arguments{
\item{states}{String or numeric vector. Name (e.g., 'Arizona','New Mexico')
or code (e.g., 4, 35) of state(s) for evalid. If all states in one or more
FIA Research Station is desired, set states=NULL and use RS argument to
define RS.}

\item{RS}{String vector. Name of research station(s)
('RMRS','SRS','NCRS','NERS','PNWRS').  Do not use if states is populated.}

\item{datsource}{Source of data ('datamart', 'sqlite').}

\item{data_dsn}{If datsource='sqlite', the file name (data source name) of
the sqlite database (*.sqlite).}

\item{invtype}{String. The type of FIA data to extract ('PERIODIC',
'ANNUAL').  Only 1 allowed at a time. See further details below.}

\item{evalCur}{Logical. If TRUE, the most current FIA Evaluation is extracted
for state(s).}

\item{evalEndyr}{Number. The end year of the FIA Evaluation period of interest.
Selects only sampled plots and conditions for the evaluation period. If
more than one state, create a named list object with evalEndyr labeled for
each state (e.g., list(Utah=2014, Colorado=2013).}

\item{evalid}{Integer. One or more EVALID to check if exists.}

\item{evalAll}{Logical. If TRUE, gets all EVALIDs for invtype.}

\item{evalType}{String vector. The type(s) of evaluation of interest ('ALL',
'CURR', 'VOL', 'GRM', 'P2VEG', 'DWM", 'INV', 'REGEN', 'CRWN').  The evalType
'ALL' includes nonsampled plots; 'CURR' includes plots used for area
estimates; 'VOL' includes plots used for area and/or tree estimates; The
evalType 'GRM' includes plots used for growth, removals, mortality, and
change estimates (eval_typ %in% c(GROW, MORT, REMV, CHNG)).  Multiple types
are accepted. See details below and FIA database manual for regional
availability and/or differences.}

\item{invyrtab}{Data frame. A data frame including inventory years by state.
If NULL, it is generated from SURVEY table from FIA database based on states
and invtype.}

\item{dbTabs}{List of database tables the user would like returned.
See help(dbTables) for a list of options.}

\item{dbconn}{Open database connection.}

\item{schema}{String. Schema in database where tables are.}

\item{dbconnopen}{Logical. If TRUE, the dbconn connection is not closed.}

\item{returnPOP}{Logical. If TRUE, returns pop tables (SURVEY, 
POP_PLOT_STRATUM_ASSGN) as R objects instead of table names, if in db.}

\item{gui}{Logical. If TRUE, prompts user (deprecated).}
}
\value{
A list of the following objects: \item{states}{ String vector. State
names. } \item{rslst}{ String vector. FIA research station names included in
output. } \item{evalidlist}{ Named list. evalid by state. } \item{invtype}{
String. Inventory type for states(s) (ANNUAL/PERIODIC). } \item{invyrtab}{
Data frame. Inventory years by state for evalidlist. } \item{evalTypelist}{
Named list. Evaluation type(s) by state. } \item{invyrs}{ Named list.
Inventory years by state for evalidlist. } \item{SURVEY}{ Data frame. If
returnPOP=TRUE, the SURVEY table from FIADB. }
}
\description{
Extracts FIA EVALIDs for identifying an estimation group of plots. EVALIDs
may be extracted by most current evaluation (evalCur=TRUE) or by the end
year of an evaluation (evalEndyr) or all evaluations in the database for one
or more states. See details for more information.
}
\details{
FIA Evaluation\cr An Evaluation defines a group of plots in the FIA Database
used for state-level estimates, representing different spans of data and
different stratification and area adjustments. An Evaluation Type (evalType)
is used to identify a specific set of plots for a particular response to be
able to ensure a sample-based estimate for a population. See FIA's Database
documentation for current available Evaluation Types and descriptions
(https://www.fia.fs.fed.us/library/database-documentation/index.php).

EVALID\cr An EVALID is a unique code defining an Evaluation, generally in
the format of a 2-digit State code, a 2-digit year code, and a 2-digit
Evaluation Type code.

EVAL_TYP\cr \tabular{llll}{ \tab \bold{EVALIDCD} \tab \bold{EVAL_TYP} \tab
\bold{Description}\cr \tab 00 \tab EXPALL \tab All area\cr \tab 01 \tab
EXPVOL/EXPCURR \tab Area/Volume\cr \tab 03 \tab
EXPCHNG/EXPGROW/EXPMORT/EXPREMV \tab Area Change/GRM\cr \tab 07 \tab EXPDWM
\tab DWM\cr \tab 08 \tab EXPREGEN \tab Regeneration\cr \tab 09 \tab EXPINV
\tab Invasive\cr \tab 10 \tab EXPP2VEG \tab Veg profile\cr \tab 12 \tab
EXPCRWN \tab Crown\cr }
}
\note{
FIA database tables used:\cr 1. SURVEY - To get latest inventory year,
invyrtab = NULL\cr 2. POP_EVAL - To get EVALID and EVALID years
}
\examples{
\dontrun{
# Get evalid and inventory years for Wyoming
WYeval <- DBgetEvalid(states="Wyoming")
names(WYeval)

WYeval$evalidlist
WYeval$invtype
WYeval$invyrtab
WYeval$evalType
WYeval$invyrs


# Get evalid for Utah and Wyoming
DBgetEvalid(states=c("Wyoming", "Utah"))

# Get evalid for an FIA Research Station
RSevalid <- DBgetEvalid(RS="NERS")
names(RSevalid)
RSevalid$evalidlist
}
}
\author{
Tracey S. Frescino
}
\keyword{data}
