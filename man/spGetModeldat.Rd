\name{spGetModeldat}
\alias{spGetModeldat}
\title{
	Spatial - Extracts point attribute values and area from SpatialPolygons 
		estimation unit or small area domain layer and zonal statistics
		(i.e., mean, probablity) from auxiliary raster layers.
}
\description{
	Wrapper to extract attribute and area from SpatialPolygons estimation
	unit or small area domain layer and zonal statistics (i.e., mean, probability) 
	from a list of raster auxiliary layers. Points are reprojected on-the-fly 
	to projection of SpatialPolygon or rasters using PROJ.4 transformation 
	parameters and rgdal spTransform function. Includes options to use bilinear 
	interpolation or summarize over a window of n pixels using a specified 
	statistic. 
	- Point attribute extraction from SpatialPolygons (spExtractPoly)
		or from raster (spExtractRast)
	- Calculate area by estimation unit(s) (areacalc.poly/areacalc.pixel) 
	- Zonal statistics (spZonalRast)
}

\usage{
spGetModeldat(spplt = NULL, spplt_dsn = NULL, xyplt = NULL, uniqueid = "PLT_CN", 
	module = "SA", domtype = "POLY", dom_layer = NULL, dom_dsn = NULL, 
	domvar = "DOMAIN", rastlst.cont = NULL, rastlst.cont.name = NULL, 
	rastlst.cont.stat = "mean", rastlst.cat = NULL, rastlst.cat.name = NULL, 
	rastfolder = NULL, rast.NODATA = NULL, asptransform = FALSE, rast.asp = NULL, 
	rast.lut = NULL, rastlut = NULL, areacalc = TRUE, areaunits = "ACRES", 
	keepnull = FALSE, showext = FALSE, savedata = FALSE, exportshp = FALSE, 
	outfolder = NULL, outfn = NULL, outfn.date = TRUE, overwrite = FALSE, 
	vars2keep = NULL, ...)
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
  \item{rastlst.cont.stat}{ String. Zonal statistic for continuous rasters. }
  \item{rastlst.cat}{ String vector or list. Categorical rasters (file names 
	or raster objects). }
  \item{rastlst.cat.name}{ String vector. Output names for categorical rasters. }
  \item{rastfolder}{ String. Name of the folder with raster layers. Optional. 
	Useful if all raster layers are in same folder. }
  \item{rast.NODATA}{ Numeric. NODATA value if stratlayer is raster. These
	values will be removed from output strata table, assuming outside boundary. }
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
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
  \item{vars2keep}{ String vector. Attributes in SAdoms, other than domvar to
	include in domzonal output and extract to pltmodel points. }
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
  \item{sppltmodel}{ SpatialPointsDataframe. Spatial point data with extracted 
	estimation unit and small area domain and raster values appended. }
  \item{pltmodel}{ Data frame. Point data with extracted estimation unit and
	raster values appended. }
  \item{domzonal}{ Data frame. Zonal statistics (continuous rasters) or zonal
	proportions (categorical proportions) by domain, or estimation unit. }
  \item{domvar}{ Data frame. Domain, or estimation unit name. }
  \item{inputdf}{ Data frame. Raster information input to zonal summaries. }
  \item{prednames}{ String vector. Name(s) of predictor variable(s). }
  \item{zonalnames}{ String vector. Name(s) of zonal variable(s). }
  \item{predfac}{ String vector. Name(s) of categorical (i.e. factor) variable(s). }
  \item{domacres}{ Data frame. Acres by domain, or estimation unit. }
  \item{areavar}{ Data frame. Variable name for acres in unitarea. }

  If savedata=TRUE, datstrat and unitarea are saved to outfolder (Default name: *_'date'.csv).
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
