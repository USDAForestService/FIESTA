\name{anGBpop_report}
\alias{anGBpop_report}
\title{
	ANALYSIS - Generate a report from core tables for Green-Book custom population.
}
\description{
	Generates a report from a set of core tables based on area of interest.
}
\usage{
anGBpop_report(GBpopdat, AOInm, photofn = NULL, photo_author = NULL, 
	EVALIDator_match = FALSE, popfilter = NULL, title.ref = NULL, outfolder = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{GBpopdat}{ List object. Population data from FIESTA::modGBpop(). }
  \item{AOInm}{ String. Name of area of interest. }
  \item{photofn}{ String. Path name to photo to add to report. }
  \item{photo_author}{ String. Name of photo author (Optional). }
  \item{EVALIDator_match}{ Logical. If TRUE, estimates match EVALIDator. }
  \item{popfilter}{ String. Name of variable or string that filters population. Optional. }
  \item{title.ref}{ String. Reference information for title and output file names. }
  \item{outfolder}{ String. The path of folder to output tables. }
}
\details{
	If variables are NULL, then it will prompt user to input variables.
		
}
\author{
	Tracey S. Frescino
}

\keyword{ data }
