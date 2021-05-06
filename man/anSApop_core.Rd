\name{anSApop_core}
\alias{anSApop_core}
\title{
	ANALYSIS - Generate core tables for Small Area module.
}
\description{
	Generates a set of core tables based on a small area of interest.
}
\usage{
anSApop_core(SApopdat, title.ref, xlsx = FALSE, outfolder = NULL, 
	outfn.pre = NULL, outfn.date = FALSE, overwrite = FALSE, 
	fortypgrpcd = 220, divideby.vol = "million")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SApopdat}{ R object. Population data from database, returned from 
	modSApop() function for a specified area of interest. }
  \item{title.ref}{ String. Reference information for title and output file names. }
  \item{xlsx}{ Logical. If TRUE, adds all tables to Excel spreadsheet. }
  \item{outfolder}{ String. The path of folder to output tables. }
  \item{outfn.pre}{ String. Add a prefix to output name (e.g., "01"). }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite}{ Logical. If TRUE, overwrites file if it exists. }
  \item{fortypgrpcd}{ Integer vector. Forest type group code for focus
	for estimation. }
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
