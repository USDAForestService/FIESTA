\name{anGBest_evalidlst}
\alias{anGBest_evalidlst}
\title{
	Analysis - Get estimates for Green-Book module.
}
\description{
	Wrapper to generate estimates for each evalid in list.	
}
\usage{
anGBest_evalidlst(evalidlst, data_dsn = NULL, esttype = "AREA", 
	landarea = "FOREST", rowvar = NULL, colvar = NULL, estvar = NULL, 
	estvar.filter = NULL, savedata = FALSE, outfolder = NULL)
}
\arguments{
  \item{evalidlst}{ Integer vector. List of evalids to generate estimates. }
  \item{data_dsn}{ String. Name of database with FIA plot data. The database
	must include the following tables: plot, cond, tree (if esttype='TREE'), 
	and ppsa (POP_PLOT_STRATUM_ASSGN). If NULL, data are extracted from 
	FIA datamart. } 
  \item{esttype}{ String. Type of estimate ('AREA', 'TREE', 'RATIO'). } 
  \item{landarea}{ String. The condition-level filter for defining land area 
	('ALL', 'FOREST', 'TIMBERLAND'). If landarea='FOREST', COND_STATUS_CD = 1; 
	if landarea='TIMBERLAND', SITECLCD in(1:6) & RESERVCD = 0. }
  \item{rowvar}{ String. Optional. Name of domain variable to group estvar by 
	for rows in table output. Rowvar must be included in an input data frame 
	(i.e., plt, cond, tree). If no rowvar is included, an estimate is returned
	for the total estimation unit. Include colvar for grouping by 2 variables. }
  \item{colvar}{ String. Optional. If rowvar != NULL, name of domain variable to 
	group estvar by for columns in table output. Colvar must be included in an 
	input data frame (i.e., plt, cond, tree). }
  \item{estvar}{ String. Name of the tree-level estimate variable (e.g., 'VOLCFNET'). }
  \item{estvar.filter}{ String. A tree-level filter for estvar. Must be R syntax 
	(e.g., 'STATUSCD == 1'). }
  \item{savedata}{ Logical. If TRUE, saves data to outfolder. }
  \item{outfolder}{ String. Name of output folder for saved data. If NULL,  
	the working directory is used. }
}
 
\value{
  Data.
}
\examples{
#evalidlst <- c(491501, 491601, 491701, 491801, 491901)
#est <- anGBest_evalidlst(evalidlst=evalidlst, esttype="AREA", landarea="FOREST",
#	rowvar="FORTYPCD")

}
\author{
	Tracey S. Frescino
}
\keyword{ data }

