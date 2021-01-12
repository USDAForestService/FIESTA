\name{anGBpop_ADFOR}
\alias{anGBpop_ADFOR}
\title{
	Analysis - Generate population data for National Forest boundary.
}
\description{
	Wrapper to get population data for a National Forest boundary delineated
	by the Automated Land Program, Ownership spatial layer (in FIESTAdata package).	
}
\usage{
anGBpop_ADFOR(ADFOR.name = NULL, RS = NULL, strat_layer = NULL, outfolder = NULL, ...)
}
\arguments{
  \item{ADFOR.name}{ String. Name of National Forest. }
  \item{RS}{ String. Name of FIA research station to restrict states to. } 
  \item{strat_layer}{ String. Name of path to raster strata layer. } 
  \item{outfolder}{ String. If savedata=TRUE or exportsp=TRUE, name of output folder. 
	If NULL, the working directory is used. }
  \item{...}{ String. Parameters for anGBpop(). }
}
 
\value{
  Estimate.
}
\author{
	Tracey S. Frescino
}
\keyword{ data }

