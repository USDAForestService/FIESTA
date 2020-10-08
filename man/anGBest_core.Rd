\name{anGBest_core}
\alias{anGBest_core}
\title{
	ANALYSIS - Generate core tables for Green-Book.
}
\description{
	Generates a set of core tables based on state of interest.
}
\usage{
anGBest_core(state, evalCur = TRUE, evalEndyr = NULL, 
	evalType = c("ALL", "AREAVOL", "GRM"), fill = TRUE, addSEcol = FALSE, 
	allin1 = FALSE, savedata = FALSE, outfolder = NULL, outfn.date = TRUE, 
	divideby.vol = "million", datCore = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state}{ String. The name of state for tables (e.g., 'Vermont', 'Utah') }
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
  \item{fill}{ Logical. If TRUE, color fill is applied to all tables. }
  \item{addSEcol}{ Logical. If TRUE, percent standard error values are added to
		columns side-by-side. }
  \item{allin1}{ Logical. If TRUE, percent standard error values are added to
		same cell as estimates. }
  \item{savedata}{ Logical. If TRUE, saves data objects to outfolder (*.rda) }
  \item{outfolder}{ String. The path of folder to output tables. }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{divideby.vol}{ String. divideby for volume tables 
		('hundred', 'thousand', 'million'). }
  \item{datCore}{ R object. FIA data from database, returned from anGBest_core_data.
	Includes datPlots, datStrata, ref_countycd, and ref_unitcd. }
}
\details{
	If variables are NULL, then it will prompt user to input variables.
		
}
\author{
	Tracey S. Frescino
}

\keyword{ data }
