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
spGetEstUnit(xyplt, xyplt_dsn = NULL, uniqueid = "PLT_CN", unittype = "POLY", 
	unit_layer = NULL, unit_dsn = NULL, unitvar = NULL, unit.filter = NULL, 
	areavar = NULL, areaunits = "ACRES", rast.NODATA = NULL, keepNA = FALSE, 
	showext = FALSE, savedata = FALSE, exportsp = FALSE, exportNA = FALSE, 
	outfolder = NULL, out_fmt = "shp", out_dsn = NULL, out_layer = "unit_assgn", 
	outfn.date = TRUE, outfn.pre = NULL, overwrite = FALSE, ...)
}

\arguments{
  \item{xyplt}{ Data frame object or String. Name of layer with xy coordinates
	and unique identifier. Can be layer with xy_dsn, full pathname, including 
	extension, or file name (with extension) in xy_dsn folder. }
  \item{xyplt_dsn}{ String. Name of database where xyplt is. The dsn varies by
	driver. See gdal OGR vector formats (https://www.gdal.org/ogr_formats.html).}
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
  \item{unit.filter}{ String. Filter to subset unit_layer spatial layer. }
  \item{areavar}{ String. Name of area variable unit variable in unitlayer. If 
	NULL, calculates area by unitvar. }
  \item{areaunits}{ String. Output area units ("ACRES", "HECTARES", "SQMETERS"). }
  \item{rast.NODATA}{ Numeric. NODATA value if stratlayer is raster. These
	values will be removed from output strata table, assuming outside boundary. }
  \item{keepNA}{ Logical. If TRUE, returns data frame of NA values. }
  \item{showext}{ Logical. If TRUE, layer extents are displayed in plot window. }
  \item{savedata}{ Logical. If TRUE, the input data with extracted values are
	saved to outfolder. }
  \item{exportsp}{ Logical. If TRUE, the extracted strata point data are 
	exported to outfolder. }
  \item{exportNA}{ Logical. If TRUE, NA values are exported to outfolder. }
  \item{outfolder}{ String. If savedata=TRUE or exportshp=TRUE, name of output folder. 
	If NULL, the working directory is used. }
  \item{out_fmt}{ String. Format for output tables ('csv', 'sqlite', 'gpkg').}
  \item{out_dsn}{ String. Name of database if out_fmt = c('sqlite', 'gpkg'). }
  \item{out_layer}{ String. Name of layer in out_dsn if database.}
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile 
	(e.g., outfn_'date'.csv). }
  \item{outfn.pre}{ String. Add a prefix to output name (e.g., "01"). }
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
  \item{pltassgnid}{ String. Unique identifier of plot. }

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

  If exportsp=TRUE:\cr
 	If out_fmt="shp", the writeOGR (rgdal) function is called. The ArcGIS driver 
	truncates variable names to 10 characters or less. Variable names are changed 
	before export using an internal function (trunc10shp). If Spatial object has 
	more than 1 record, it will be returned but not exported. 

  
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
