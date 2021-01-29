\name{anGBpop_evalidlst}
\alias{anGBpop_evalidlst}
\title{
	Analysis - Get estimates for Green-Book module.
}
\description{
	Wrapper to generate estimates for each evalid in list.	
}
\usage{
anGBpop_evalidlst(evalidlst, data_dsn = NULL, istree = FALSE, isseed = FALSE,
	ppsanm = "pop_plot_stratum_assgn", pjoinid = "CN", savedata = FALSE, 
	outfolder = NULL, out_fmt = "sqlite", out_dsn = NULL, overwrite = FALSE)
}
\arguments{
  \item{evalidlst}{ Integer vector. List of evalids to generate estimates. }
  \item{data_dsn}{ String. Name of database with FIA plot data. The database
	must include the following tables: plot, cond, tree (if esttype='TREE'), 
	and ppsa (POP_PLOT_STRATUM_ASSGN). If NULL, data are extracted from 
	FIA datamart. } 
  \item{istree}{ Logical. If TRUE, add tree data from Evaluation. } 
  \item{isseed}{ Logical. If TRUE, add seedling data from Evaluation. } 
  \item{ppsanm}{ String. Name of pop_plot_stratum_assgn table in database. } 
  \item{pjoinid}{ String. Name of variable in ppsanm to join with plot table. } 
  \item{savedata}{ Logical. If TRUE, saves data to outfolder. }
  \item{outfolder}{ String. Name of output folder for saved data. If NULL,  
	the working directory is used. }
  \item{out_fmt}{ String. Format for output tables ('csv', 'sqlite', 'gpkg').}
  \item{out_dsn}{ String. Name of database if out_fmt = c('sqlite', 'gpkg'). }
  \item{overwrite}{ Logical. If TRUE, overwrites data in outfolder. }
}
 
\value{
  Data.
}
\examples{
evalidlst <- c(491501, 491601, 491701, 491801, 491901)
GBpoplst <- anGBpop_evalidlst(evalidlst=evalidlst)

lapply(GBpoplst, function(x) modGBarea(GBpopdat=x, rowvar="FORTYPGRPCD", 
	landarea="FOREST", row.FIAname=TRUE))

}
\author{
	Tracey S. Frescino
}
\keyword{ data }

