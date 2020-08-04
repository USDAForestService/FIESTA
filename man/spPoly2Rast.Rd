\name{spPoly2Rast}
\alias{spPoly2Rast}
\title{
	Spatial - Converts SpatialPolygons layer to raster. 
}
\description{
	Converts SpatialPolygons layer to raster. 
}

\usage{
spPoly2Rast(polyv, polyv_dsn=NULL, polyv.att, polyv.lut = NULL, rastfn.template, 
	outfolder=NULL, outfn="polyrast", outext="img", outfn.pre=NULL, outfn.date=TRUE, 
	overwrite=FALSE)
}

\arguments{
  \item{polyv}{ sf R object or String. Polygon data to convert to raster. 
	Can be a spatial polygon object, full pathname to a shapefile, or name 
	of a layer within a database. }
  \item{polyv_dsn}{ String. Data source name (dsn; e.g., sqlite or shapefile 
	pathname) of layer to convert. The dsn varies by driver. See gdal OGR vector
	formats (https://www.gdal.org/ogr_formats.html). Optional if polyv is sf object. }
  \item{polyv.att}{ String. Name of attribute in polyv to rasterize. }
  \item{polyv.lut}{ Data frame. Look up table of codes, if polyv.att is character or
	want to group codes. }
  \item{rastfn.template}{ String. Full path name of raster to use as template for 
	new raster. }
  \item{outfolder}{ String. If exportshp=TRUE, name of output folder. If NULL,
	the working directory is used. }
  \item{outfn}{ String. Name of output raster. If NULL, default is 'polyrast'. }
  \item{outfn.pre}{ String. Add a prefix to output name (e.g., "01"). }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile (e.g., outfn_'date'.csv). }
  \item{outext}{ String. Name of raster extension (fmt). }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
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
