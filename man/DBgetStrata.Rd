\name{DBgetStrata}
\alias{DBgetStrata}
\title{
	Database - Gets stratification information from FIA database. 
}
\description{
	Gets strata information from FIA's Oracle database or FIA DataMart, including:
	(1) strata and estimation unit assignment per plot; (2) total area by estimation unit;
	(3) pixel counts and number plots by strata/estimation unit. Include a data frame
	of plots, states, or evaluation information.}

\usage{
DBgetStrata(dat = NULL, uniqueid = "CN", states = NULL, evalid = NULL, 
	evalCur = TRUE, evalEndyr = NULL, evalAll = FALSE, evalType = "AREAVOL", 
	savedata = FALSE, outfolder = NULL, outfn.pre = NULL, outfn.date = FALSE, 
	overwrite = FALSE, getassgn = TRUE, POP_PLOT_STRATUM_ASSGN = NULL)
}
\arguments{
  \item{dat}{ Data frame, comma-delimited file (*.csv), or shapefile (*.shp). The strata 
	value is merged to this table and returned as a data frame. See details for necessary
	variables. }
  \item{uniqueid}{ String. The unique plot identifier of dat (e.g., 'CN'). }
  \item{states}{ String or numeric vector. Name(s) (e.g., 'Arizona','New Mexico') or 
	code(s) (e.g., 4, 35) of states for strata if dat=NULL. }
  \item{evalid}{ Integer. The evaluation if dat=NULL. }
  \item{evalCur}{ Logical. If TRUE, the most current evalidation for state(s). }
  \item{evalEndyr}{ Number. The end year of evalidation time period for state(s). }
  \item{evalAll}{ Logical. If TRUE, gets all EVALIDs for invtype. }
  \item{evalType}{ String vector. The type(s) of evaluation of interest 
	('ALL', 'AREAVOL', 'GRM', 'P2VEG', 'DWM', 'INV', 'REGEN', 'CRWN'). 
	The evalType 'ALL' includes nonsampled plots; 'AREAVOL' includes plots used
	for area or tree estimates (eval_typ %in% c(CURR, VOL)); The evalType 'GRM'
	includes plots used for growth, removals, mortality, and change estimates
	(eval_typ %in% c(GROW, MORT, REMV, CHNG)). Multiple types are accepted. 
	See FIA database manual for regional availability and/or differences. 
	See notes for more information on evalType. }
  \item{savedata}{ Logical. If TRUE, writes output to outfolder. }
  \item{outfolder}{ String.* If savedata=TRUE, name of the output folder. If NULL, 
	data are saved to the working directory. }
  \item{outfn.pre}{ String. The name used for prefix of outfiles (e.g., outfn.pre'_plt*'.) }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile (e.g., outfn_'date'.csv). }
  \item{overwrite}{ Logical. If TRUE, if savedata=TRUE, overwrites files. }
  \item{getassgn}{ Logical. If TRUE, extracts plot assignments from POP_PLOT_STRATUM_ASSGN 
	table in database. }
  \item{POP_PLOT_STRATUM_ASSGN}{ Data frame. The POP_PLOT_STRATUM_ASSGN for state(s). }

}
\details{
	Dependent packages: DBI, rgdal

	The following variables must be present in dat: 
		STATECD, UNITCD, INVYR, a uniqueid (ex. "PLT_CN"), and PLOT_STATUS_CD (if nonsampled 
		plots in dataset).

	FIADB TABLES USED:
	\tabular{lll}{
	 	\tab FS_FIADB.SURVEY	\tab To get latest inventory year.\cr
		\tab FS_FIADB.POP_EVAL	\tab To get EVALID and EVALID years.\cr
		\tab FS_FIADB.POP_ESTN_UNIT 	\tab To get total area by estimation unit 
		(AREATOT_EU-includes water).\cr
		\tab FS_FIADB.POP_STRATUM	\tab To get pixel counts by stratum and estimation unit.\cr
		\tab FS_FIADB.POP_PLOT_STRATUM_ASSGN	\tab To get estimation unit & stratum 
		assignment for each plot.\cr
	}

	Area by estimation unit includes total area for all plots (evaltype="all").
}
\value{
  FIAstrata - a list of the following objects:
  \item{pltassgn}{ Data frame. Plot-level strata/estimation unit assignment. 
		If dat is not NULL, strata/estimation unit variables are appended to dat. }
  \item{pltassgnid}{ String. Name of unique identifier of plot in pltassgn. }
  \item{unitarea}{ Data frame. Total acres by estimation unit. }
  \item{unitvar}{ String. Name of the estimation unit variable (ESTN_UNIT). }
  \item{areavar}{ String. Name of the acre variable (ACRES). }
  \item{strlut}{ Data frame. Strata look-up table with summarized pixel counts
		(P1POINTCNT) by strata/estimation unit. }
  \item{strvar}{ String. Name of the strata variable (STRATA). }
  \item{strwtvar}{ String. Name of the strata weight variable (P1POINTCNT). }
  \item{evalid}{ List. evalid by state. }

  Outputs to outfolder (if savedata=TRUE):
	\tabular{ll}{
	\tab	- CSV file of pltassgn (*'date'.csv).\cr
	\tab	- CSV file of unitarea (*'date'.csv).\cr
	\tab	- CSV file of stratalut (*'date'.csv).\cr
	\tab	- If collapsed, a CSV file of original classes and new collapsed classes.\cr
	}
}
\author{
	Tracey S. Frescino
}
\note{
     Steps used in data extraction: 
     
	\enumerate{
    	  \item Get EVALID and EVALID years by state - DBgetEvalid().
	  \item unitarea: get total area by estimation unit for EVALID (POP_ESTN_UNIT).
	  \item stratalut: get pixel counts by estimation unit and stratum for EVALID (POP_STRATUM).
	  \item pltassgn: get estimation unit and stratum assignment for each plot for EVALID. 									(POP_PLOT_STRATUM_ASSGN).
	  \item If dat is not NULL, merge pltassgn assignment to dat.
	  \item Merge number of plots to stratalut
 	  \item Check for only 1 MEASYEAR or 1 INVYR and number of plots by strata/estimation unit.
		If less than minimumnum plots per strata/estimation unit collapse using the following 
		algorithm. 
	}

	Strata collapsing: \cr
	If there are less than minplotnum (10) plots in the smallest strata of the estimation unit, 
	these plots are grouped with the larger strata in the same estimation unit and defined as 
	the highest strata value. If, after grouping, there are still less than minplotnum, all 
	of these plots are combined with the corresponding strata of the estimation unit above. 
	If there are no records above, then they are combined with the estimation unit below.  
	The process repeats, grouping the strata to the highest strata value if necessary. 
	All grouping is restrained within survey units (UNITCD). 


	More than one evaluation: \cr
	If attributing a table of plots and there are plots that have been visited more than
	once, all plots are assigned an estimation unit and strata value, but the area and 
	strata proportions are from the most current evaluation for the dataset. The plots
	outside the most current evaluation are attributes with values from the next most
	current evaluation occurring in the database.

}

\examples{

  ## Get strata for the most current evaluation of a state (ex. Wyoming)
#  WYstrat1 <- DBgetStrata(states="Wyoming", evalCur=TRUE)
#  names(WYstrat1)

#  head(WYstrat1$pltassgn)
#  WYstrat1$unitarea
#  WYstrat1$unitvar
#  WYstrat1$areavar
#  WYstrat1$strlut
#  WYstrat1$strvar
#  WYstrat1$strwtvar
#  WYstrat1$evalid


#  ## Get strata information for a specific set of plots
#  WYstrat4 <- DBgetStrata(dat=WYplt, datsource="CSV")
#  names(WYstrat4)

#  head(WYstrat4$pltassgn)
#  WYstrat4$unitarea
#  WYstrat4$strlut
#  WYstrat4$evalid
}

\keyword{ data }
