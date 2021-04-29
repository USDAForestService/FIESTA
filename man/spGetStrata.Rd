\name{spGetStrata}
\alias{spGetStrata}
\title{
	Spatial wrapper - Extracts point attribute values and pixel counts for 
		strata and estimation unit spatial layers.
}
\description{
	Wrapper to extract attribute and area from a polygon or raster estimation 
	unit layer and a polygon or raster layer with strata pixel categories. }

\usage{
spGetStrata(xyplt, xyplt_dsn = NULL, uniqueid = "PLT_CN", unittype = "POLY", 
	unit_layer = NULL, unit_dsn = NULL, unitvar = NULL, unit.filter = NULL,
	strattype = "RASTER", strat_layer = NULL, strat_dsn = NULL, strvar = NULL, 
	strat_lut = NULL, areaunits = "ACRES", rast.NODATA = NULL, keepNA = FALSE, 
	keepxy = TRUE, showext = FALSE, savedata = FALSE, exportsp = FALSE, 
	exportNA = FALSE, outfolder = NULL, out_fmt = "shp", out_dsn = NULL, 
	out_layer = "strat_assgn", outfn.date = TRUE, outfn.pre = NULL, 
	overwrite_dsn = FALSE, overwrite_layer = TRUE, ...)
}

\arguments{
  \item{xyplt}{ Data frame, sf object, full pathname to *.csv or *shp, or 
	layer name in a geodatabase. Includes XY coordinates and unique identifier.
	If non-spatial, include parameters for FIESTA::spMakeSpatialPoints(). }
  \item{xyplt_dsn}{ String. Name of database where xyplt is. The dsn varies by
	driver. See gdal OGR vector formats (https://www.gdal.org/ogr_formats.html).}
  \item{uniqueid}{ String.* Unique identifier of xyplt records. }
  \item{unittype}{ String. Spatial layer type of unitlayer ("POLY", "RASTER"). 
	Note: raster unit layers are converted to polygon. }
  \item{unit_layer}{ sf R object or String. Name of estimation unit spatial
	layer. Can be a spatial polygon object, full pathname to a shapefile,
	name of a polygon layer within a database, or a full pathname to raster 
	file. }
  \item{unit_dsn}{ String. Data source name (dsn; e.g., sqlite or shapefile 
	pathname) of unit_layer. The dsn varies by driver. See gdal OGR vector
	formats (https://www.gdal.org/ogr_formats.html). Optional if unit_layer 
	is sf object. }
  \item{unitvar}{ String. If unittype="POLY", name of attribute in unit_layer
	defining estimation units. If NULL, the unit_layer represents one 
	estimation unit. }
  \item{unit.filter}{ String. Filter to subset unit_layer spatial layer. }
  \item{strattype}{ String. Spatial layer type of stratlayer ("POLY", "RASTER"). 
	Note: polygon strata layers are converted to raster. }
  \item{strat_layer}{ sf R object or full pathname of spatial stratification layer.
	Can be a spatial polygon object, full pathname to a shapefile, name of a 
	polygon layer within a database, or a full pathname to raster file. }
  \item{strat_dsn}{ String. Data source name (dsn; e.g., sqlite or shapefile 
	pathname) of strat_layer. The dsn varies by driver. See gdal OGR vector
	formats (https://www.gdal.org/ogr_formats.html). Optional if unit_layer 
	is sf object. }
  \item{strvar}{ String. If strattype="POLY", name of strata attribute in 
	strat_layer. }
  \item{strat_lut}{ Data frame. A look-up table of codes to aggregate. The
	format of table includes 2 columns, one column same name as strvar. 
	If strattype="RASTER", strvar="value". }
  \item{areaunits}{ String. Output area units ("ACRES", "HECTARES", "SQMETERS"). }
  \item{rast.NODATA}{ Numeric. NODATA value if stratlayer is raster 
	(See notes). This values will be converted to NA and removed from output 
	if keepNA=FALSE. }
  \item{keepNA}{ Logical. If TRUE, returns data frame of NA values. }
  \item{keepxy}{ Logical. If FALSE, removes XY coordinates from pltassgn output. }
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
  \item{out_layer}{ String. Name of layer in out_dsn if database. }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile 
	(e.g., outfn_'date'.csv). }
  \item{outfn.pre}{ String. Add a prefix to output name (e.g., "01"). }
  \item{overwrite_dsn}{ Logical. If TRUE, overwrite dsn. }
  \item{overwrite_layer}{ Logical. If TRUE, overwrite layer(s) in dsn. }
  \item{...}{ Other parameters for spMakeSpatialPoints. }
}
\details{
 *If variable = NULL, then it will prompt user for input.

  If spatial layers have different projections, the polygon spatial layer is 
	reprojected to the projection of raster (See note about on-the-fly projection 
	conversion). If both layers are long/lat coordinate system, they are
	reprojected to default coordinate system (Conus Albers, NAD83).

}
\value{
  \item{pltassgn}{ Data frame. Input xyplt data with extracted estimation unit and
	strata values appended. }
  \item{unitarea}{ Data frame. Area by estimation unit. }
  \item{unitvar}{ Data frame. Variable name for estimation unit in unitarea. }
  \item{acrevar}{ Data frame. Variable name for area in unitarea. }
  \item{stratalut}{ Data frame. Strata proportions (weights) by estimation unit and strata. }
  \item{strvar}{ Data frame. Variable name for strata values in stratalut. }
  \item{NAlst}{ sf List. If NA values exist after data extraction, the spatial NA
	points are returned. }
  \item{pltassgnid}{ String. Unique identifier of plot. }

  If savedata=TRUE, pltassgn and unitarea are saved to outfolder.\cr
  If exportsp=TRUE, the spatial sf points object is exported to outfolder.\cr.
  If exportNA=TRUE and NA values exist after data extraction, the spatial NA points
	are exported to outfolder.
   
}
\author{
	Tracey S. Frescino
}
\note{

  rast.NODATA\cr
  NODATA values are raster pixel values that have no data of interest, including pixels 
	within the extent of the layer, but outside the area of interest. Sometimes these 
	pixels have been defined previously. The defined NODATA pixels are imported to R 
	as NULL values. When not previously defined, the pixels outside the area of interest 
	will be the minimum or maximum value depending on the data type 
	(e.g., 16-bit signed: min=-32,768; max=32,768) or byte size (1 byte: min=0; max=255).
      These NODATA values will be added to the zonal statistic calculations if not
	specified in rast.NODATA. 


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


  polygon to raster\cr
  If strattype="POLY", a raster template is created based on the masked extent of
	strat_layer, with strat_layer projected coordinate system and 30 meter pixel size.


}

\keyword{ data }
