\name{spGetSAdoms}
\alias{spGetSAdoms}
\title{
	Spatial - Generate spatial domains for small area estimation.
}
\description{
	Wrapper to get spatial domains for small area estimation. Small area polygons
	area unioned with helper polygons and subset to the extent of the large
	area. The smallbnd and largebnd are optional. If NULL, the helper polygons
	are used as small area domains. A variable named 'AOI' is appended to 
	attribute table to indicate the area of interest, or small area polygons.	
}
\usage{
spGetSAdoms(smallbnd_layer, smallbnd_dsn = NULL, smallbnd.att, 
	smallbnd.name = NULL, smallbnd.filter = NULL, helperbnd_layer = NULL, 
	helperbnd_dsn = NULL, helperbnd.att = NULL, helperbnd.name = NULL, 
	helperbnd.filter = NULL, largebnd_layer = NULL, largebnd_dsn = NULL, 
	largebnd.att = NULL, largebnd.name = NULL, largebnd.filter = NULL, 
	exportshp = FALSE, outfolder = NULL, outfn = NULL, outfn.date = TRUE, 
	overwrite = FALSE)
}
\arguments{
  \item{smallbnd_layer}{ String or S4 object. The name of the smallbnd 
	spatial polygons layer. The layer name may be a full pathname to a file, 
	the basename to a file, a spatial layer name from a database, or a 
	SpatialPolygons object with a defined projection. }
  \item{smallbnd_dsn}{ String. The data source name (dsn; i.e., folder or 
	database name) of smallbnd_layer. The dsn varies by driver. See gdal OGR 
	vector formats (https://www.gdal.org/ogr_formats.html). Optional. } 
  \item{smallbnd.att}{ String. The atribute in smallbnd polygon layer that defines
	small area(s). } 
  \item{smallbnd.name}{ String. The output name for the atribute in smallbnd polygon 
	layer that defines small area(s). If NULL, smallbnd.att is default. } 
  \item{smallbnd.filter}{ String. A filter for smallbnd. Must be R syntax. } 
  \item{helperbnd_layer}{ String or S4 object. The name of the helperbnd 
	spatial polygons layer. The layer name may be a full pathname to a file, 
	the basename to a file, a spatial layer name from a database, or a 
	SpatialPolygons object with a defined projection. }
  \item{helperbnd_dsn}{ String. The data source name (dsn; i.e., folder or 
	database name) of helperbnd_layer. The dsn varies by driver. See gdal OGR 
	vector formats (https://www.gdal.org/ogr_formats.html). Optional. } 
  \item{helperbnd.att}{ String. The atribute in helperbnd polygon layer that defines
	helper areas. } 
  \item{helperbnd.name}{ String. The output name for the atribute in helperbnd polygon 
	layer that defines helper area(s). If NULL, helperbnd.att is default. } 
  \item{helperbnd.filter}{ String. A filter for helperbnd. Must be R syntax. } 
  \item{largebnd_layer}{ String or S4 object. The name of the largebnd 
	spatial polygons layer. The layer name may be a full pathname to a file, 
	the basename to a file, a spatial layer name from a database, or a 
	SpatialPolygons object with a defined projection. }
  \item{largebnd_dsn}{ String. The data source name (dsn; i.e., folder or 
	database name) of largebnd_layer. The dsn varies by driver. See gdal OGR 
	vector formats (https://www.gdal.org/ogr_formats.html). Optional. } 
  \item{largebnd.att}{ String. The atribute in largebnd polygon layer that defines
	large area(s). } 
  \item{largebnd.name}{ String. The output name for the atribute in largebnd polygon 
	layer that defines large area(s). If NULL, largebnd.att is default. } 
  \item{largebnd.filter}{ String. A filter for largebnd. Must be R syntax. } 
  \item{exportshp}{ Logical. If TRUE, exports Spatial object to outfolder as an 
	ArcGIS shapefile (*.shp). Variable names are truncated to 10 characters or less
	(See note below). }
  \item{outfolder}{ String. If savedata=TRUE or exportshp=TRUE, name of output folder. 
	If NULL, the working directory is used. }
  \item{outfn}{ String. Name of the output shapefile/.csv file (Do not include 
	extension). If NULL, default name is datext_'date'. }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
}
 
\value{
  A SpatialPolygonsDataFrame object with smallbnd and helperbnd unioned and clipped to 
	extent of largebnd. Attributes with DOMAIN and SMALLAREA included.

  If exportshp=TRUE, the SpatialPointsDataFrame object is exported to outfolder
	(Default name: datext_'date'.shp). Variable names are truncated to 10 characters or 
	less (See note below). Name changes are output to 'outfn'_newnames_'data'.csv in
	outfolder.
}
\author{
	Tracey S. Frescino
}
\note{

  If exportshp=TRUE:\cr
 	The writeOGR (rgdal) function is called. The ArcGIS driver truncates variable 
	names to 10 characters or less. Variable names are changed before export using 
	an internal function (trunc10shp). If Spatial object has more than 1 record, 
	it will be returned but not exported. 
  
  The spTransform (rgdal) method is used for on-the-fly map projection conversion and 
	datum transformation using PROJ.4 arguments. Datum transformation only occurs if 
	the +datum tag is present in the both the from and to PROJ.4 strings. The +towgs84 
	tag is used when no datum transformation is needed. PROJ.4 transformations assume 
	NAD83 and WGS84 are identical unless other transformation parameters are specified. 
	Be aware, providing inaccurate or incomplete CRS 	information may lead to erroneous 
	data shifts when reprojecting. See spTransform help documentation for more details.
}
\keyword{ data }

