\name{anGBpop_eval}
\alias{anGBpop_eval}
\title{
	Analysis - Get estimates for Green-Book module.
}
\description{
	Wrapper to generate estimates for each evalid in list.	
}
\usage{
anGBpop_eval(evalidlst=NULL, evalEndyrlst=NULL, states, evalType = "VOL", 
	data_dsn = NULL, isseed = FALSE, ppsanm = "pop_plot_stratum_assgn", 
	savedata = FALSE, out_dsn=NULL, out_fmt="sqlite", outfolder=NULL, 
	outfn.pre=NULL, outfn.date=FALSE, overwrite_dsn = FALSE, 
	overwrite_layer = TRUE)
}
\arguments{
  \item{evalidlst}{ Integer vector. Vector of one or more evalid (e.g, 491801). }
  \item{evalEndyrlst}{ Integer vector. Vector of one or more Evaluation 
	end years to generate estimates (YYYY). }
  \item{states}{ String vector. Name(s) of states in evaluation. }
  \item{evalType}{ String vector. The type(s) of evaluation of interest 
	('ALL', 'CURR', VOL', 'GRM', 'P2VEG', 'DWM", 'INV', 'REGEN', 'CRWN'). 
	The evalType 'ALL' includes nonsampled plots; 'CURR' includes plots 
	used for area estimates; 'VOL' includes plots used for area and/or tree
	estimates; The evalType 'GRM' includes plots used for growth, removals,
	mortality, and change estimates (eval_typ %in% c(GROW, MORT, REMV, CHNG)). 
	Multiple types are accepted. See details below and FIA database manual 
	for regional availability and/or differences. }
  \item{data_dsn}{ String. Name of database with FIA plot data. The database
	must include the following tables: plot, cond, tree (if esttype='TREE'), 
	and ppsa (POP_PLOT_STRATUM_ASSGN). If NULL, data are extracted from 
	FIA datamart. }
  \item{isseed}{ Logical. If TRUE, add seedling data from Evaluation(s). } 
  \item{ppsanm}{ String. Name of pop_plot_stratum_assgn table in database. } 
  \item{savedata}{ Logical. If TRUE, saves data to outfolder. }
  \item{out_dsn}{ String. Data source name for output. If extension is not
	included, out_fmt is used. Use full path if outfolder=NULL. }
  \item{out_fmt}{ String. File format for output ('csv', 'sqlite','gpkg', 'gdb').
	If out_fmt %in% c('sqlite','gpkg'), RSQLite package must be installed. If
	out_fmt='gdb', arcgisbinding package and R-Bridge must be installed. }
  \item{outfolder}{ String. The output folder path. If NULL and savedata=TRUE, 
	outfolder is the working directory. }
  \item{outfn.pre}{ String. The name used for prefix of outfiles (e.g., outfn.pre'_plt*'). }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile (e.g., outfn_'date'.csv). }
  \item{overwrite_dsn}{ Logical. If TRUE, overwrites the out_dsn, if exists. }
  \item{overwrite_layer}{ Logical. If TRUE, overwrites the out_layer, if exists. }
}
 
\value{
  Data.
}
\examples{
  GBpoplst <- anGBpop_eval(evalEndyrlst=c(2015:2019), states="Utah", 
	evalType="CURR")

  lapply(GBpoplst, function(x) modGBarea(GBpopdat=x, rowvar="FORTYPGRPCD", 
	landarea="FOREST", row.FIAname=TRUE))

}
\author{
	Tracey S. Frescino
}
\keyword{ data }

