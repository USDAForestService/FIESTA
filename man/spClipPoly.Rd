\name{spClipPoly}
\alias{spClipPoly}
\title{
	Spatial - Subsets an S4 SpatialPolygons layer with another SpatialPolygons layer.
}
\description{
	Subsets an S4 SpatialPolygons object(s) or shapefile(s) with another 
	SpatialPolygons object or polygon shapefile (*.shp), with option to export 
	the clipped SpatialPolygons layer to a shapefile (*.shp).
}				

\usage{
spClipPoly(polyv_layer, polyv_dsn = NULL, clippolyv_layer, clippolyv_dsn = NULL, 
	showext = FALSE, areacalc = FALSE, exportshp = FALSE, outfolder = NULL, 
	outshpnm = NULL, outfn.date = TRUE, overwrite = FALSE)
}

\arguments{
  \item{polyv_layer}{ String or S4 object. The name of the polygon layer 
	to be clipped. The layer name may be a full pathname to a file, the basename 
	to a file, a spatial layer name from a database, or a SpatialPolygons object 
	with a defined projection. }
  \item{polyv_dsn}{ String. The data source name (dsn; i.e., folder or database name)
	of poly_layer. The dsn varies by driver. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html). Optional. } 
  \item{clippolyv_layer}{ String or S4 object. The name of the polygon layer 
	to use for clipping. The layer name may be a full pathname to a file, the 
	basename to a file, a spatial layer name from a database, or a SpatialPolygons 
	object with a defined projection. }
  \item{clippolyv_dsn}{ String. The data source name (dsn; i.e., folder or database name)
	of clippolyv_layer. The dsn varies by driver. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html). Optional. } 
  \item{showext}{ Logical. If TRUE, layer extents are displayed in plot window. }
  \item{areacalc}{ Logical. If TRUE, calculate area of unioned polygons and append
	to attribute table (See details). }
  \item{exportshp}{ Logical. If TRUE, exports Spatial object to outfolder as an 
	ArcGIS shapefile (*.shp). Variable names are truncated to 10 characters or less
	(See note below). }
  \item{outfolder}{ String. If exportshp=TRUE, name of output folder. If NULL,
	the working directory is used. }
  \item{outshpnm}{ String. Name of output clipped polygon. If NULL, default is 
	'polyclip.shp'. }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite}{ Logical. If TRUE, overwrite files in outfolder. }
}
\details{
  Uses raster::crop to merge two polygons.
  Generates a new ID for each polygon and appends attributes from both polygons.

  areacalc\cr
  If areacalc = TRUE and the unioned SpatialPolygon object is not in a projected 
	coordinate system (i.e., longlat), the object will be reprojected to the 
	Albers Equal Area projection before area is calculated.  

}
\value{
  SpatialPolygonsDataframe object of clipped polygon. If polyv and clippolyv have 
	different projections, the projection of returned object will have the same projection
	as polyv (See note about on-the-fly projection conversion). 

  If exportshp=TRUE, the SpatialPolygons object is exported to outfolder
	(Default name: datext_'date'.shp). Variable names are truncated to 10 characters or 
	less (See note below). Name changes are output to 'outfn'_newnames_'data'.csv in
	outfolder.
}
\author{
	Tracey S. Frescino
}
\note{
  
  On-the-fly projection conversion\cr 
  The spTransform (rgdal) method is used for on-the-fly map projection conversion and 
	datum transformation using PROJ.4 arguments. Datum transformation only occurs if 
	the +datum tag is present in the both the from and to PROJ.4 strings. The +towgs84 
	tag is used when no datum transformation is needed. PROJ.4 transformations assume 
	NAD83 and WGS84 are identical unless other transformation parameters are specified. 
	Be aware, providing inaccurate or incomplete CRS 	information may lead to erroneous 
	data shifts when reprojecting. See spTransform help documentation for more details.

  If exportshp=TRUE:\cr
 	The writeOGR (rgdal) function is called. The ArcGIS driver truncates variable 
	names to 10 characters or less. Variable names are changed before export using 
	an internal function (trunc10shp). If Spatial object has more than 1 record, 
	it will be returned but not exported. 
}

\keyword{ data }
