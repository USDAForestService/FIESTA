\name{anGBpop_core}
\alias{anGBpop_core}
\title{
	ANALYSIS - Generate core tables for Green-Book custom population.
}
\description{
	Generates a set of core tables based on area of interest.
}
\usage{
anGBpop_core(GBpopdat, title.ref, xlsx = TRUE, outfolder = NULL, 
	outfn.date = TRUE, divideby.vol = "million")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{GBpopdat}{ R object. Population data from database, returned from 
	modGBpop() function for a specified area of interest. }
  \item{title.ref}{ Reference information for title and output file names. }
  \item{xlsx}{ Logical. If TRUE, adds all tables to Excel spreadsheet. }
  \item{evalCur}{ Logical. If TRUE, the most current FIA Evalidation for state(s). }
  \item{outfolder}{ String. The path of folder to output tables. }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
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
