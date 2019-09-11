\name{spReprojectShape}
\alias{spReprojectShape}
\title{
	Spatial - Reprojects an Esri shapefile (*shp) or S4 Spatial object.
}
\description{
	Reprojects an Esri shapefile (*.shp) or S4 Spatial object to a new geographic
	or projected coordinate system, with option to save new object.
}
\usage{
spReprojectShape(splayer, splayer_dsn=NULL, EPSGCD.new=NULL, prj4str.new = NULL, 
	prj.new = NULL, datum.new = NULL, zone.new = NULL, zoneS.new = FALSE, 
	aea.param.new = "USGS", exportshp = FALSE, ...)
}
\arguments{
  \item{splayer}{ S4 SpatialPoints object or shapefile filename (*.shp), including 
	full path and extension. The spatial layer must have a defined projection.
	If NULL, include xyplt. }
  \item{splayer_dsn}{ String. The data source name (dsn; i.e., folder or database name)
	of splayer. The dsn varies by driver. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html). Optional. } 
  \item{EPSGCD.new}{ Integer. New International Association of Oil and Gas Producers
	(IOGP), EPSG Geodetic Parameter Dataset definition of a coordinate system. }  
  \item{prj4str.new}{ String. New PROJ.4 string identifying spatial reference system 
	(e.g., "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"). }
  \item{prj.new}{ String.* New projection, or coordinate system of the X/Y coordinates 
	("longlat", "utm", "aea"). If other, include PROJ.4 string in prj4str. }
  \item{datum.new}{ String.* New datum of projection ("WGS84", "NAD83", "NAD27"). }
  \item{zone.new}{ Integer. If prj="utm", the new UTM zone. }
  \item{zoneS.new}{ Logical. If prj="utm", if the new UTM zone is in the Southern hemisphere. }
  \item{aea.param.new}{ String. If prj="aea", the associated lat/lon parameters 
	(USGS: " +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0").
	If other, include PROJ.4 string in prj4str. }
  \item{exportshp}{ Logical. If TRUE, exports new Spatial object to an ArcGIS shapefile 
	(*.shp) to outfolder. }
  \item{...}{ Other parameters for spExportShape. }
}
\details{
  If exportshp=TRUE:\cr
 	The rgdal, writeOGR function is called. The ArcGIS driver truncates variable 
	names to 10 characters or less. Variable names are changed before export using 
	an internal function (trunc10shp). If savedata=TRUE, name changes are output to 
	the outfolder, 'outshpnm'_newnames.csv. The returned Spatial object will have 
	original names. If Spatial object has more than 1 record, it will be returned 
	but not exported. 

}

\value{
  \item{shp}{ S4 SpatialPoints or SpatialPointsDataFrame object. }

  If exportshp = TRUE, a shapefile is written to the specified outfolder or working directory. 
}
\author{
	Tracey S. Frescino
}

\note{

  The shp must have a defined coordinate reference system. Test using sp::proj4string(shp).

  Coordinate Reference Systems (CRS)\cr
  An ellipse is an estimated model describing the basic shape of the Earth and is the basis
	for all coordinate systems. There are many ellipsoids designed for local (e.g., NAD27) 
	or global (e.g., WGS84, GRS80) use. The datum defines the reference position of the 
	coordinate axes associated with a specific ellipsoid. Specifying the datum also defines 
	the ellipsoid, whereas specifying the ellipsoid does not provide information of the datum.

  WGS84 vs NAD83
  WGS84 and NAD83 datums are often used interchangeably, and use very similar ellipsoids
	(WGS84 and GRS80, respectively), but have different reference points. Slight tectonic
	shifts through time have caused increased divergence between the two, with NAD83 
	datum intended to track movements more consistently.  

  Common Datums and associated spheroid (ellipsoid):\cr
	NAD27 - North American Datum of 1927 (Clarke 1866 spheroid)\cr
	NAD83 - North American Datum of 1983 (GRS 1980 spheroid)\cr
	WGS84 - World Geodetic System of 1984 (WGS 1984 spheroid)\cr

  From R, use projInfo for list of different projections and datums.\cr
  > projInfo(type="proj")\cr
  > projInfo(type="datum")\cr

  Common EPSG codes in U.S.\cr
	EPSG:4326 - Longitude/Latitude (WGS84) - Common for global displays (used by Google Earth)\cr
	EPSG:4269 - Longitude/Latitude (NAD83) - Most common by U.S. Federal Agencies\cr


  The spTransform (rgdal) method is used for map projection conversion and datum transformation
	using PROJ.4 arguments. Datum transformation only occurs if the +datum tag is present in the 
	both the from and to PROJ.4 strings. The +towgs84 tag is used when no datum transformation 
	is needed. PROJ.4 transformations assume NAD83 and WGS84 are identical unless other
	transformation parameters are specified. Be aware, providing inaccurate or incomplete CRS 	
	information may lead to erroneous data shifts when reprojecting. See spTransform help 
	documentation for more details.
	

}
\keyword{ data }
