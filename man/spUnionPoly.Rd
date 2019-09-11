\name{spUnionPoly}
\alias{spUnionPoly}
\title{
	Spatial - Generates one S4 SpatialPolygons object from two SpatialPolygons
	layers, including features and attributes from both polygons. 
}
\description{
	Generates one S4 SpatialPolygons object from two SpatialPolygons
	layers, including features and attributes from both polygons. 
}

\usage{
spUnionPoly(poly1_dsn, poly1_layer, poly2_dsn, poly2_layer, clip = FALSE, 
	showext = FALSE, areacalc = FALSE, exportshp = FALSE, outfolder = NULL, 
	outshpnm = NULL, overwrite = FALSE)
}

\arguments{
  \item{poly1_dsn}{ String or S4object. The data source name for the 
	spatial polygons layer1. The data source name varies by driver and may be 
	a file name, but may also be a folder or a connection string to a database. 
	See gdal OGR vector formats (https://www.gdal.org/ogr_formats.html)) for
	more details. A SpatialPolgonsDataFrame R object or full pathname of a 
	ArcGIS Shapefile (*.shp) are also valid options. The SpatialPolygons layer 
	must have a defined projection. }
  \item{poly1_layer}{ String. The layer name for the estimation unit spatial polygons 
	layer1. The layer name varies by driver and may be the layer name of a dsn 
	filename or a spatial layer name from a database. For ArcGIS Shapefile, include 
	full path in dsn and layer=name without extension or layer=NULL. If layer=NULL, 
	the assumption is dsn is a full pathname to an ArcGIS Shapefile or a 
	SpatialPolygonsDataFrame. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html)) for more details. The SpatialPolygons 
	layer must have a defined projection. }
  \item{poly2_dsn}{ String or S4object. The data source name for the 
	spatial polygons layer2. The data source name varies by driver and may be 
	a file name, but may also be a folder or a connection string to a database. 
	See gdal OGR vector formats (https://www.gdal.org/ogr_formats.html)) for
	more details. A SpatialPolgonsDataFrame R object or full pathname of a 
	ArcGIS Shapefile (*.shp) are also valid options. The SpatialPolygons layer 
	must have a defined projection. }
  \item{poly2_layer}{ String. The layer name for the estimation unit spatial polygons 
	layer2. The layer name varies by driver and may be the layer name of a dsn 
	filename or a spatial layer name from a database. For ArcGIS Shapefile, include 
	full path in dsn and layer=name without extension or layer=NULL. If layer=NULL, 
	the assumption is dsn is a full pathname to an ArcGIS Shapefile or a 
	SpatialPolygonsDataFrame. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html)) for more details. The SpatialPolygons 
	layer must have a defined projection. }
  \item{clip}{ Logical. If TRUE, returns intersection of polygons. }
  \item{showext}{ Logical. If TRUE, layer extents are displayed in plot window. }
  \item{areacalc}{ Logical. If TRUE, calculate area of unioned polygons and append
	to attribute table (See details). }
  \item{exportshp}{ Logical. If TRUE, exports Spatial object to outfolder as an 
	ArcGIS shapefile (*.shp). Variable names are truncated to 10 characters or less
	(See note below). }
  \item{outfolder}{ String. If exportshp=TRUE, name of output folder. If NULL,
	the working directory is used. }
  \item{outshpnm}{ String. Name of output union polygon. If NULL, default is 
	'polyunion.shp'. }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
}
\details{
 *If variable = NULL, then it will prompt user for input.

  Uses raster function union to merge two polygons and crop, if clip=TRUE.
  Generates a new ID for each polygon and appends attributes from both polygons.

  areacalc\cr
  If areacalc = TRUE and the unioned SpatialPolygon object is not in a projected 
	coordinate system (i.e., longlat), the object will be reprojected to the 
	Albers Equal Area projection before area is calculated.  

}
\value{
  SpatialPolygonsDataFrame object of the Unioned polygon. If clip=TRUE, it is 
	the interection of polyv1 and polyv2. If polyv1 and polyv2 have different 
	projections, the projection of returned object will have the same projection
	as polyv1 (See note about on-the-fly projection conversion).

  If exportshp=TRUE, the SpatialPolygonsDataFrame object is exported to outfolder
	(Default name: polyunion_'date'.shp). Variable names are truncated to 10 characters or 
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
