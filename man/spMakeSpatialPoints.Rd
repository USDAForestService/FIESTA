\name{spMakeSpatialPoints}
\alias{spMakeSpatialPoints}
\title{
	Spatial - Generates an S4 SpatialPoints object from X/Y coordinates.
}
\description{
	Generates an S4 SpatialPoints object with defined projection from a data 
	table or matrix including X and Y coordinates, with option to export as an 
	ArcGIS shapefile (*.shp).
}
\usage{
spMakeSpatialPoints(xyplt = NULL, uniqueid = NULL, x = NULL, y = NULL, EPSGCD = NULL,
	prj4str = NULL, prj = NULL, datum = NULL, zone = NULL, zoneS = FALSE, 
	aea.param = "USGS", exportshp = FALSE, outfolder = NULL, outshpnm = NULL,
	outfn.date = TRUE, overwrite = FALSE)
}
\arguments{
  \item{xyplt}{ Data frame, data.table, matrix, or comma-delimited file (*.csv). 
	Includes unique identifier of rows (uniqueid) and x and y coordinates. }
  \item{uniqueid}{ String.* Unique identifier of xyplt rows. }
  \item{x}{ String.* Name of variable in xyplt defining x coordinate. }
  \item{y}{ String.* Name of variable in xyplt defining y coordinate. }
  \item{EPSGCD}{ Integer. International Association of Oil and Gas Producers
	(IOGP), EPSG Geodetic Parameter Dataset definition of a coordinate system. }  
  \item{prj4str}{ String. PROJ.4 string identifying spatial reference system 
	(e.g., "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"). }
  \item{prj}{ String.* Projection, or coordinate system of the X/Y coordinates 
	("longlat", "utm", "aea"). If other, include PROJ.4 string in prj4str. }
  \item{datum}{ String.* Datum of projection ("WGS84", "NAD83", "NAD27"). }
  \item{zone}{ Integer. If prj="utm", the UTM zone. }
  \item{zoneS}{ Logical. If prj="utm", if the UTM zone is in the Southern hemisphere. }
  \item{aea.param}{ String. If prj="aea", the associated lat/lon parameters 
	(USGS: " +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0").
	If other, include PROJ.4 string in prj4str. }
  \item{exportshp}{ Logical. If TRUE, exports Spatial object to outfolder as an 
	ArcGIS shapefile (*.shp). }
  \item{outfolder}{ String. Name of output folder (if exportshp=TRUE). If NULL, 
	the working directory is used. }
  \item{outshpnm}{ String. Name of output shapefile (if exportshp=TRUE), the
	extension is optional. If outshpnm=NULL, default is outshp_'date'.shp. }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite}{ Logical. If TRUE, overwrites file if exists. }
}
\details{
	If exportshp=TRUE, uses rgdal writeOGR function.
}
\value{
  \item{shp}{ S4 SpatialPoints or SpatialPointsDataFrame object. }

  If exportshp = TRUE, a shapefile is written to the specified outfolder or working directory. 
}
\author{
	Tracey S. Frescino
}
\note{
  If exportshp=TRUE:\cr
 	The ArcGIS driver truncates variable names to 10 characters or less.
	Variable names are changed before export using an internal function
	(trunc10shp). Name changes are output to the outfolder, 'outshpnm'_newnames.csv.
	The returned Spatial object will have original names, before truncating.

  If Spatial object has more than 1 record, it cannot be exported. 

   Common Datums and associated spheroid (ellipsoid):\cr
	NAD27 - North American Datum of 1927 - Clarke 1866 spheroid\cr
	NAD83 - North American Datum of 1983 - GRS 1980 spheroid\cr
	WGS84 - World Geodetic System of 1984 - WGS 1984 spheroid\cr
}
\keyword{ data }
