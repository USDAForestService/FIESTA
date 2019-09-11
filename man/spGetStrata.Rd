\name{spGetStrata}
\alias{spGetStrata}
\title{
	Spatial - Extracts point attribute values and pixel counts for strata and
		estimation unit spatial layers.
}
\description{
	Extracts values from a polygon layer of estimation units and a raster
	layer of strata pixel categories. Calculates area by estimation unit. 
	Points are reprojected on-the-fly to projection of rasters using
	PROJ.4 transformation parameters and rgdal spTransform function. }

\usage{
spGetStrata(spplt = NULL, spplt_dsn = NULL, xyplt = NULL, uniqueid = "PLT_CN", 
	unittype = "POLY", unit_layer = NULL, unit_dsn = NULL, unitvar = NULL, 
	strattype = "RASTER", strat_layer = NULL, strat_dsn = NULL, strvar = NULL, 
	areaunits = "ACRES", rast.NODATA = NULL, keepnull = FALSE, showext = FALSE, 
	savedata = FALSE, exportshp = FALSE, outfolder = NULL, outfn = NULL, 
	overwrite = FALSE, ...)
}

\arguments{
  \item{spplt}{ S4 SpatialPoints object or shapefile filename (*.shp), including 
	full path and extension. The spatial layer must have a defined projection.
	If NULL, include xyplt. }
  \item{spplt_dsn}{ String. The data source name (dsn; i.e., folder or database name)
	of spplt. The dsn varies by driver. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html). Optional. } 
  \item{xyplt}{ Data frame, data.table, matrix, or comma-delimited file (*.csv). 
	Includes unique identifier of rows (uniqueid) and x and y coordinates. 	
	If spplt=NULL, x/y coordinates are used for data extraction. Must include 
	projection information in respective parameters. }
  \item{uniqueid}{ String.* Unique identifier of xyplt rows. }
  \item{unittype}{ String. Spatial layer type of unitlayer ("POLY", "RASTER"). }
  \item{unit_layer}{ String or S4 object. The name of the estimation unit 
	spatial polygons layer. The layer name may be a full pathname to a file, 
	the basename to a file, a spatial layer name from a database, or a 
	SpatialPolygons object with a defined projection. }
  \item{unit_dsn}{ String. The data source name (dsn; i.e., folder or database name)
	of unit_layer. The dsn varies by driver. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html). Optional. } 
  \item{unitvar}{ String. Name of estimation unit variable in unitlayer. }
  \item{strattype}{ String. Spatial layer type of stratlayer ("POLY", "RASTER"). 
	Note: raster objects must be written to file. }
  \item{strat_layer}{ String or raster object. Name of the strata spatial raster 
	layer, including fullpath and extension (e.g., *.img). If raster object,
	must be written to file. }
  \item{strat_dsn}{ String. The data source name (dsn; i.e., folder or database name, 
	if polygon layer) of strat_layer. The dsn varies by driver. See gdal OGR vector 
	formats (https://www.gdal.org/ogr_formats.html). Optional. } 
  \item{strvar}{ String. Name of strata variable in stratlayer. }
  \item{areaunits}{ String. Output area units ("ACRES", "HECTARES", "SQMETERS"). }
  \item{rast.NODATA}{ Numeric. NODATA value if stratlayer is raster. These
	values will be removed from output strata table, assuming outside boundary. }
  \item{keepnull}{ Logical. If TRUE, returns data frame of NULL values. }
  \item{showext}{ Logical. If TRUE, layer extents are displayed in plot window. }
  \item{savedata}{ Logical. If TRUE, the input data with extracted values are
	saved to outfolder. }
  \item{exportshp}{ Logical. If TRUE, exports Spatial object to outfolder as an 
	ArcGIS shapefile (*.shp). Variable names are truncated to 10 characters or less
	(See note below). }
  \item{outfolder}{ String. If savedata=TRUE or exportshp=TRUE, name of output folder. 
	If NULL, the working directory is used. }
  \item{outfn}{ String. Name of the output shapefile/.csv file (Do not include 
	extension). If NULL, default name is datext_'date'. }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
  \item{...}{ Other parameters for spMakeSpatialPoints. }
}
\details{
 *If variable = NULL, then it will prompt user for input.

  If there is a raster and SpatialPolygon layer, and the projection of the 
	SpatialPolygons is different than the projection of the raster, the 
	SpatialPolygons object is reprojected to the projection of raster 
	(See note about on-the-fly projection conversion).

}
\value{
  \item{pltstrat}{ Data frame. Input point data with extracted estimation unit and
	strata values appended. }
  \item{unitarea}{ Data frame. Area by estimation unit. }
  \item{unitvar}{ Data frame. Variable name for estimation unit in unitarea. }
  \item{acrevar}{ Data frame. Variable name for area in unitarea. }
  \item{stratalut}{ Data frame. Strata proportions (weights) by estimation unit and strata. }
  \item{strvar}{ Data frame. Variable name for strata values in stratalut. }

  If savedata=TRUE, pltstrat and unitarea are saved to outfolder (Default name: *_'date'.csv).
  If exportshp=TRUE, the SpatialPointsDataFrame object is exported to outfolder
	(Default name: datext_'date'.shp). Variable names are truncated to 10 characters or 
	less (See note below). Name changes are output to 'outfn'_newnames_'data'.csv in
	outfolder.
   
}
\author{
	Tracey S. Frescino, Chris Toney
}
\note{

  If exportshp=TRUE:\cr
 	The writeOGR (rgdal) function is called. The ArcGIS driver truncates variable 
	names to 10 characters or less. Variable names are changed before export using 
	an internal function (trunc10shp). If Spatial object has more than 1 record, 
	it will be returned but not exported. 

  
  On-the-fly projection conversion\cr 
  The spTransform (rgdal) method is used for on-the-fly map projection conversion and 
	datum transformation using PROJ.4 arguments. Datum transformation only occurs if 
	the +datum tag is present in the both the from and to PROJ.4 strings. The +towgs84 
	tag is used when no datum transformation is needed. PROJ.4 transformations assume 
	NAD83 and WGS84 are identical unless other transformation parameters are specified. 
	Be aware, providing inaccurate or incomplete CRS 	information may lead to erroneous 
	data shifts when reprojecting. See spTransform help documentation for more details.

  unitarea\cr
  Area by estimation unit is calculated and returned as object named unitarea. 
	Area is based on the projection of unit_layer. If no unit_layer input, than area 
	is calculated from pixel counts.

}

\keyword{ data }
