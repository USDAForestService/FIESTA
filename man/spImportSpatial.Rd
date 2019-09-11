\name{spImportSpatial}
\alias{spImportSpatial}
\title{
	Spatial - Imports a spatial vector layer to an S4 Spatial object.
}
\description{
	Imports a spatial vector layer to an S4 Spatial object.
}
\usage{
spImportSpatial(layer, dsn = NULL)
}
\arguments{
  \item{layer}{ S4 SpatialPoints object or shapefile filename (*.shp), including 
	full path and extension. The spatial layer must have a defined projection.
	If NULL, include xyplt. }
  \item{dsn}{ String. The data source name (dsn; i.e., folder or database name)
	of layer. The dsn varies by driver. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html). Optional. } 
}
\details{
	The layer can be a point, line, or polygon. 
}
\value{
  	A spatial object
}
\author{
	Tracey S. Frescino
}
\note{
  	Wrapper for rgdal package... readOGR function. GUI option available - spImportSpatial() 
}
\keyword{ data }
