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
anGBpop_ADFOR(ADFOR.name = NULL, RS = NULL, xy = NULL, xy_dsn = NULL, 
	strat_layer = NULL, outfolder = NULL, ...)
}
\arguments{
  \item{ADFOR.name}{ String. Name of National Forest. }
  \item{RS}{ String. Name of FIA research station to restrict states to. } 
  \item{xy}{ sf R object or String. Table with xy coordinates. Can be a spatial 
	polygon object, data frame, full pathname to a shapefile, or name of a 
	layer within a database. }
  \item{xy_dsn}{ String. Data source name (dsn; i.e., pathname or database name)
	of xy. The dsn varies by driver. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html). Optional if bnd_layer is an R object. } 
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

