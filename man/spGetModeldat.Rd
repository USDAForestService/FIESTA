\name{spGetModeldat}
\alias{spGetModeldat}
\title{
	Spatial wrapper - Extracts and compiles auxiliary data within a specified boundary.
}
\description{
	Wrapper to extract and compile auxiliary data by domain (i.e, estimation 
		unit or small area domain). The following information is compiled:\cr 
	- Attribute defining domain (i.e., estimation unit) from domain layer\cr
	- Area by domain (i.e., estimation unit)\cr 
	- Zonal statistics by domain (i.e., estimation unit) - spZonalRast()\cr
}

\usage{
spGetModeldat(xyplt, xyplt_dsn = NULL, uniqueid = "PLT_CN", 
	module = "SA", domtype = "POLY", dom_layer = NULL, dom_dsn = NULL, 
	domvar = "DOMAIN", rastlst.cont = NULL, rastlst.cont.name = NULL, 
	rastlst.cont.stat = "mean", rastlst.cont.NODATA = NULL, rastlst.cat = NULL, 	
	rastlst.cat.name = NULL, rastlst.cat.NODATA = NULL, rastfolder = NULL, 
	asptransform = FALSE, rast.asp = NULL, rast.lut = NULL, rastlut = NULL, 
	areacalc = TRUE, areaunits = "ACRES", keepNA = TRUE, NAto0 = TRUE, 
	showext = FALSE, savedata = FALSE, exportsp = FALSE, exportNA = FALSE, 
	outfolder = NULL, out_fmt = "csv", out_dsn = NULL, outfn.pre = NULL, 
	outfn.date = FALSE, overwrite = TRUE, vars2keep = NULL, ...)
}

\arguments{
  \item{xyplt}{ Data frame object or String. Name of layer with xy coordinates
	and unique identifier. Can be layer with xy_dsn, full pathname, including 
	extension, or file name (with extension) in xy_dsn folder. }
  \item{xyplt_dsn}{ String. Name of database where xyplt is. The dsn varies by
	driver. See gdal OGR vector formats (https://www.gdal.org/ogr_formats.html).}
  \item{uniqueid}{ String.* Unique identifier of xyplt rows. }
  \item{module}{ String. The module using ('SA', 'MA'). }
  \item{domtype}{ String. Spatial layer type of domlayer ("POLY", "RASTER"). }
  \item{dom_layer}{ String or S4 object. The name of the domain
	spatial polygons layer. The layer name may be a full pathname to a file, 
	the basename to a file, a spatial layer name from a database, or a 
	SpatialPolygons object with a defined projection. }
  \item{dom_dsn}{ String. The data source name (dsn; i.e., folder or database name)
	of dom_layer. The dsn varies by driver. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html). Optional. } 
  \item{domvar}{ String. Name of domain variable in domlayer. }
  \item{rastlst.cont}{ String vector or list. Continuous rasters (file names 
	or raster objects). }
  \item{rastlst.cont.name}{ String vector. Output names for continuous rasters. 
	Optional. If NULL, name of raster is default or name+'_'+layer number 
	for multi-band layers. }
  \item{rastlst.cont.NODATA}{ Numeric vector. NODATA value for continuous rasters 
	(See notes). These values will be converted to NA and removed from output 
	if keepNA=FALSE. If 1 number, the same value will be used for all continuous 
	rasters. If more than 1 number, the number of values must be equal to the 
	number of rasters in rastlst.cont. }
  \item{rastlst.cont.stat}{ String. Zonal statistic for continuous rasters. }
  \item{rastlst.cat}{ String vector or list. Categorical rasters (file names 
	or raster objects). }
  \item{rastlst.cat.name}{ String vector. Output names for categorical rasters. }
  \item{rastlst.cat.NODATA}{ Numeric vector. NODATA value for categorical rasters 
	(See notes). These values will be converted to NA and removed from output 
	if keepNA=FALSE. If 1 number, the same value will be used for all categorical 
	rasters. If more than 1 number, the number of values must be equal to the 
	number of rasters in rastlst.cat. }
  \item{rastfolder}{ String. Name of the folder with raster layers. Optional. 
	Useful if all raster layers are in same folder. }
  \item{asptransform}{ Logical. If TRUE, transforms aspect to Northness and Eastness
	indices using sin and cosine functions. }
  \item{rast.asp}{ String or raster object. The raster in rastlst.cont that is the
	aspect raster (Note: aspect must have units in degrees). }
  \item{rast.lut}{ String or raster object. The raster in rastlst.cat that is the
 	raster for collapsing values using rastlut. }
  \item{rastlut}{ String or raster object. The raster look up table used for 
	collapsing rast.lut values. }
  \item{areacalc}{ Logical. If TRUE, returns acres by domvar. }
  \item{areaunits}{ String. Output area units ("ACRES", "HECTARES", "SQMETERS"). }
  \item{keepNA}{ Logical. If TRUE, returns data frame of NA values. }
  \item{NAto0}{ Logical. If TRUE, converts extracted NA values to 0. }
  \item{showext}{ Logical. If TRUE, layer extents are displayed in plot window. }
  \item{savedata}{ Logical. If TRUE, the input data with extracted values are
	saved to outfolder. }
  \item{exportsp}{ Logical. If TRUE, the extracted raster point data are 
	exported to outfolder. }
  \item{exportNA}{ Logical. If TRUE, NA values are exported to outfolder. }
  \item{outfolder}{ String. If savedata=TRUE or exportsp=TRUE, name of output folder. 
	If NULL, the working directory is used. }
  \item{out_fmt}{ String. Format for output tables ('csv', 'sqlite', 'gpkg').}
  \item{out_dsn}{ String. Name of database if out_fmt = c('sqlite', 'gpkg'). }
  \item{outfn.pre}{ String. Add a prefix to output name (e.g., "01"). }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite}{ Logical. If TRUE and exportsp=TRUE, overwrite files in outfolder. }
  \item{vars2keep}{ String vector. Attributes in SAdoms, other than domvar to
	include in domzonal output and extract to pltassgn points. }
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
  \item{pltassgn}{ sf object. xyplt data with extracted values from rastlst*. }
  \item{domzonal}{ Data frame. Number of pixels and zonal statistics from 
	continuous rasters or zonal proportions from categorical raster for each 
	domain (i.e., estimation unit). }
  \item{domvar}{ Data frame. Domain (i.e., estimation unit) name. }
  \item{inputdf}{ Data frame. Raster information input to zonal summaries. }
  \item{prednames}{ String vector. Name(s) of predictor variable(s). }
  \item{zonalnames}{ String vector. Name(s) of zonal variable(s). }
  \item{predfac}{ String vector. Name(s) of categorical (i.e. factor) variable(s). }
  \item{npixelvar}{ String. Name of variable describing number of pixels. }
  \item{domarea}{ Data frame. Area by domain (i.e., estimation unit). }
  \item{areavar}{ String. Name of variable describing acres in domarea. }
  \item{pltassgnid}{ String. Unique identifier of plot. }

  If savedata=TRUE, datstrat and unitarea are saved to outfolder.
  If exportsp=TRUE, the sf object is exported to outfolder.
   
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
}

\keyword{ data }
