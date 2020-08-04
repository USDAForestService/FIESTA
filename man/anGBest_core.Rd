\name{anGBest_core}
\alias{anGBest_core}
\title{
	ANALYSIS - Generate core tables for Green-Book.
}
\description{
	Generates a set of core tables based on state of interest.
}
\usage{
anGBest_core(datCore, fill = TRUE, addSEcol = FALSE, allin1 = FALSE, 
	savedata = FALSE, outfolder = NULL, divideby.vol = "million")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datCore}{ R object. FIA data from database, returned from anGBest_core_data.
	Includes datPlots, datStrata, ref_countycd, and ref_unitcd. }
  \item{fill}{ Logical. If TRUE, color fill is applied to all tables. }
  \item{addSEcol}{ Logical. If TRUE, percent standard error values are added to
		columns side-by-side. }
  \item{allin1}{ Logical. If TRUE, percent standard error values are added to
		same cell as estimates. }
  \item{savedata}{ Logical. If TRUE, saves data objects to outfolder (*.rda) }
  \item{outfolder}{ String. The path of folder to output tables. }
  \item{divideby.vol}{ String. divideby for volume tables 
		('hundred', 'thousand', 'million'). }
}
\details{
	If variables are NULL, then it will prompt user to input variables.
		
}
\author{
	Tracey S. Frescino
}

\keyword{ data }
