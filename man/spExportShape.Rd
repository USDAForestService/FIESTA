\name{spExportShape}
\alias{spExportShape}
\title{
	Spatial - Exports an S4 Spatial object to an Esri shapefile (*.shp).
}
\description{
	Exports an S4 Spatial object to an Esri shapefile (*.shp).
}
\usage{
spExportShape(spobj, outshpnm = NULL, outfolder = NULL, outfn.date = TRUE,
	overwrite = FALSE, uniqueid = NULL)
}
\arguments{
  \item{spobj}{ S4 object. A Spatial object. }
  \item{outshpnm}{ String. Name of output shapefile. If outshpnm = NULL, 
		the name is outshp_'date' (extension is optional). If path is
		included in name, the path is used as outfolder. }
  \item{outfolder}{ String. Optional. Name of output folder. If NULL, the 
		shapefile is saved to working directory. }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite}{ Logical. If TRUE, overwrites file if exists. }
  \item{uniqueid}{ String. Name of the unique identifier. Optional. 
		For checking duplicate records. }
}
\details{
	Uses rgdal writeOGR function.
}
\value{
  	A shapefile is written to the specified outfolder or working directory. 
}
\author{
	Tracey S. Frescino
}
\note{
  If exportshp=TRUE:\cr
 	The ArcGIS driver truncates variable names to 10 characters or less.
	Variable names are changed before export using an internal function
	(trunc10shp). Name changes are output to the outfolder, 'outshpnm'_newnames.csv.

  If Spatial object has more than 1 record, it cannot be exported. 

}
\keyword{ data }
