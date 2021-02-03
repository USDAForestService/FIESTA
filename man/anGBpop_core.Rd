\name{anGBpop_core}
\alias{anGBpop_core}
\title{
	ANALYSIS - Generate core tables for Green-Book custom population.
}
\description{
	Generates a set of core tables based on area of interest.
}
\usage{
anGBpop_core(GBpopdat, title.ref, xlsx = FALSE, outfolder = NULL, 
	outfn.pre = NULL, outfn.date = FALSE, overwrite = FALSE, 
	treedia.brks = c(0,5,10,15,20,25,50,100), divideby.vol = "million")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{GBpopdat}{ R object. Population data from database, returned from 
	modGBpop() function for a specified area of interest. }
  \item{title.ref}{ String. Reference information for title and output file names. }
  \item{xlsx}{ Logical. If TRUE, adds all tables to Excel spreadsheet. }
  \item{outfolder}{ String. The path of folder to output tables. }
  \item{outfn.pre}{ String. Add a prefix to output name (e.g., "01"). }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite}{ Logical. If TRUE, overwrites file if it exists. }
  \item{treedia.brks}{ Integer vector. Numeric breaks for tree diameters. }
  \item{divideby.vol}{ String. divideby for volume tables 
		('hundred', 'thousand', 'million'). }
}
\details{
  If variables are NULL, then it will prompt user to input variables.
		
}
\author{
	Tracey S. Frescino
}
\examples{

  ## Get population data using strata and area estimates for evaluations ending 2015 to 2019
  WYbhfn <- system.file("extdata", "sp_data/WYbighorn_adminbnd.shp", package="FIESTA")
  GBpop <- anGBpop(WYbhfn, datsource="datamart")
  names(GBpop)

  names(GBpop$GBdata)
  names(GBpop$GBpopdat)

  ## Run core tables
  anGBpop_core(GBpopdat=GBpop$GBpopdat, title.ref="Black Hills", xlsx=TRUE)
}
\keyword{ data }
