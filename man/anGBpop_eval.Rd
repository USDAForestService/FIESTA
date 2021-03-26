\name{anGBpop_eval}
\alias{anGBpop_eval}
\title{
	Analysis - Get estimates for Green-Book module.
}
\description{
	Wrapper to generate estimates for each evalid in list.	
}
\usage{
anGBpop_eval(evalidlst = NULL, evalCur = FALSE, evalEndyrlst = NULL, 
	states = NULL, RS = NULL, evalType = "VOL", datsource = "datamart", 
	data_dsn = NULL, isseed = FALSE, ppsanm = "pop_plot_stratum_assgn", 
	byEndyr = FALSE, savedata = FALSE, outfolder = NULL, ...)
}
\arguments{
  \item{evalidlst}{ Integer vector. Vector of one or more evalid (e.g, 491801). }
  \item{evalCur}{ Logical. If TRUE, gets the most current evaluation for
	each state. }
  \item{evalEndyrlst}{ Integer vector. Vector of one or more Evaluation 
	end years to generate estimates (YYYY). }
  \item{states}{ String vector. Name(s) of states in evaluation. }
  \item{RS}{ String vector. Name of research station(s) ('RMRS','SRS','NCRS','NERS','PNWRS').
	Do not use if states is populated. }
  \item{evalType}{ String vector. The type(s) of evaluation of interest 
	('ALL', 'CURR', VOL', 'GRM', 'P2VEG', 'DWM", 'INV', 'REGEN', 'CRWN'). 
	The evalType 'ALL' includes nonsampled plots; 'CURR' includes plots 
	used for area estimates; 'VOL' includes plots used for area and/or tree
	estimates; The evalType 'GRM' includes plots used for growth, removals,
	mortality, and change estimates (eval_typ %in% c(GROW, MORT, REMV, CHNG)). 
	Multiple types are accepted. See details below and FIA database manual 
	for regional availability and/or differences. }
  \item{datsource}{ String. Source of data ('datamart', 'sqlite'). }
  \item{data_dsn}{ If datsource='sqlite', the file name (data source name) of the 
	sqlite database (*.sqlite). }
  \item{isseed}{ Logical. If TRUE, add seedling data from Evaluation(s). } 
  \item{ppsanm}{ String. Name of pop_plot_stratum_assgn table in database. } 
  \item{byEndyr}{ Logical. If TRUE, organizes evalidlst by evalEndyr. 
	Set TRUE if estimates include more than one state (e.g., if area of 
	interest crosses more than one state - Manti-LaSal). }
  \item{savedata}{ Logical. If TRUE, saves data to outfolder. }
  \item{outfolder}{ String. The output folder path. If NULL and savedata=TRUE, 
	outfolder is the working directory. }
  \item{...}{ Parameters to FIESTA::modGBpop(). }
}
 
\value{
  Set of population data from FIESTA::modGBpop() for each Evaluation specified.
}
\examples{

  ## Calculate population data for Utah, area evaluations 2015-2019
  GBpoplst <- anGBpop_eval(evalEndyrlst=c(2015:2019), states="Utah", 
	evalType="CURR")

  ## Get acres by forest type from each Evaluation in GBpoplst
  lapply(GBpoplst, function(x) modGBarea(GBpopdat=x, rowvar="FORTYPGRPCD", 
	landarea="FOREST", row.FIAname=TRUE))

}
\author{
	Tracey S. Frescino
}
\keyword{ data }

