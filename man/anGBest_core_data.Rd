\name{anGBest_core_data}
\alias{anGBest_core_data}
\title{
	ANALYSIS - Extract data for core tables from FIA database.
}
\description{
	Extract data for core tables from FIA database.
}
\usage{
anGBest_core_data(state, evalCur = TRUE, evalEndyr = NULL, 
	evalType = c("ALL", "AREAVOL", "GRM"), savedata = TRUE, 
	outfolder = NULL, outfn.date = TRUE)
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
  \item{savedata}{ Logical. If TRUE, saves data objects to outfolder (*.rda) }
  \item{outfolder}{ String. The path of folder to output tables. }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
}
\details{
	If variables are NULL, then it will prompt user to input variables.
		
}
\author{
	Tracey S. Frescino
}

\keyword{ data }
