\name{spGetEstUnit}
\alias{spGetEstUnit}
\title{
	Spatial - Extracts point attribute values and area from a
		SpatialPolygons or raster estimation unit layer.
}

\description{
	Wrapper to get point attribute values and area from a SpatialPolygons 
	or raster layer of estimation units and calculates area. Points are 
	reprojected on-the-fly to projection of unitlayer using PROJ.4 
	transformation parameters and rgdal spTransform function.
	- Point attribute extraction from SpatialPolygons (spExtractPoly)
		or from raster (spExtractRast)
	- Calulate area by estimation unit(s) (areacalc.poly/areacalc.pixel) 
}

\usage{
spGetEstUnit(spplt = NULL, spplt_dsn = NULL, xyplt = NULL, uniqueid = "PLT_CN", 
	unittype = "POLY", unit_layer = NULL, unit_dsn = NULL, unitvar = NULL, 
	areavar = NULL, areaunits = "ACRES", rast.NODATA = NULL, keepnull = FALSE, 
	showext = FALSE, savedata = FALSE, exportshp = FALSE, outfolder = NULL, 
	outfn = NULL, overwrite = FALSE, ...)
}

\arguments{
  \item{spplt}{ S4 SpatialPoints object or shapefile filename (*.shp), including 
	full path and extension. The spatial layer must have a defined projection.
	If NULL, include xyplt. }
  \item{spplt_dsn}{ String. Name of the folder or database with spplt. }
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
  \item{areavar}{ String. Name of area variable unit variable in unitlayer. If 
	NULL, calculates area by unitvar. }
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
  \item{pltunit}{ Data frame. Input point data with extracted estimation unit and
	strata values appended. }
  \item{sppltunit}{ SpatialPointsDataframe. Spatial point data with extracted 
	estimation unit values appended. }
  \item{unitarea}{ Data frame. Area by estimation unit. }
  \item{unitvar}{ Data frame. Variable name for estimation unit in unitarea. }
  \item{acrevar}{ Data frame. Variable name for area in unitarea. }

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
}

\keyword{ data }
