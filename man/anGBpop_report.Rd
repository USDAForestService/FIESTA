\name{anGBpop_report}
\alias{anGBpop_report}
\title{
	ANALYSIS - Generate a report from core tables for Green-Book custom population.
}
\description{
	Generates a report from a set of core tables based on area of interest.
}
\usage{
anGBpop_report(rawfolder, AOInm, photofn = NULL, title.ref = NULL, outfolder = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawfolder}{ String. Full path name of rawdata folder with estimates. }
  \item{AOInm}{ String. Name of area of interest. }
  \item{photofn}{ String. Path name to photo to add to report. }
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
