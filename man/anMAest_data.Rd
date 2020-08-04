\name{anMAest_data}
\alias{anMAest_data}
\title{
	Analysis - Generate estimates for state boundaries.
}
\description{
	Wrapper to get estimates using Model-assisted methods - generalized
		regression estimator (GREG).	
}
\usage{
anMAest_data(bnd, bnd_dsn = NULL, bnd.att, bnd.filter = NULL, 
	states = NULL, xy = NULL, xy_dsn = NULL, xy.uniqueid = "PLT_CN", 
	xvar = "LON_PUBLIC", yvar = "LAT_PUBLIC", xy.crs = 4269, clipxy = TRUE, 
	datsource = "sqlite", SQLitefn = NULL, istree = TRUE, plot_layer = "plot", 
	cond_layer = "cond", tree_layer = "tree", puniqueid = "CN", 
	intensity1 = FALSE, rastlst.cont = NULL, rastlst.cont.name = NULL, 
	rastlst.cat = NULL, rastlst.cat.name = NULL, rastlst.cat.NODATA = NULL, 
	showsteps = FALSE, savedata = FALSE, savexy = FALSE, savesteps = FALSE, 
	outfolder = NULL, out_fmt = "csv", out_dsn = NULL, outfn.pre = NULL, 
	outfn.date = FALSE, overwrite = TRUE)
}

\arguments{
  \item{bnd}{ sf R object or String. Area of interest boundary. Can be a spatial 
	polygon object, full pathname to a shapefile, or name of a layer within 
	a database. }
  \item{bnd_dsn}{ String. Data source name (dsn; e.g., sqlite or shapefile 
	pathname) of bnd. The dsn varies by driver. See gdal OGR 
	vector formats (https://www.gdal.org/ogr_formats.html). Optional if bnd
	is an R object. } 
  \item{bnd.att}{ String. Name of attribute in bnd to identify polygons to 
	summarize statistics for. }
  \item{bnd.filter}{ String. Filter to subset bnd spatial layer. }
  \item{states}{ States of intesecting boundary. Optional. }
  \item{xy}{ sf R object or String. Table with xy coordinates. Can be a spatial 
	polygon object, data frame, full pathname to a shapefile, or name of a 
	layer within a database. }
  \item{xy_dsn}{ String. Data source name (dsn; i.e., pathname or database name)
	of xy. The dsn varies by driver. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html). Optional if bnd_layer is an R object. } 
  \item{xy.uniqueid}{ String. Unique identifier of xy. }
  \item{xvar}{ String. Name of variable in xyplt defining x coordinate. }
  \item{yvar}{ String. Name of variable in xyplt defining y coordinate. }
  \item{xy.crs}{ PROJ.4 String or CRS object or Integer EPSG code defining Coordinate 
	Reference System. }  
  \item{datsource}{ String. Source of FIA data ("datamart", "sqlite", "csv").  
	If datsource="sqlite", specify database name in SQLitefn and layers in 
	*_layer arguments. 
	If datsource="datamart", files are downloaded and extracted 
	from FIA DataMart (http://apps.fs.usda.gov/fia/datamart/datamart.html). See details
	for more information about plot coordinates.
	If datsource="csv", specify *.csv file names in *_layer arguments. }
  \item{clipxy}{ Logical. If TRUE, clips xy data to bnd. }
  \item{SQLitefn}{ String. Name of SQLite database where *_layers reside. }
  \item{istree}{ Logical. If TRUE, extract tree data from FIA database. }
  \item{plot_layer}{ String. Name of layer in database of file name of FIA plot table. }
  \item{cond_layer}{ String. Name of layer in database of file name of FIA cond table. }
  \item{tree_layer}{ String. Name of layer in database of file name of FIA tree table 
	if istree=TRUE. }
  \item{puniqueid}{ String. Name of unique identifier of plt. }
  \item{intensity1}{ Logical. If TRUE, include only plots with INTENSITY=1. }
  \item{rastlst.cont}{ String vector or list. Continuous rasters (file names 
	or raster objects). }
  \item{rastlst.cont.name}{ String vector. Output names for continuous rasters. 
	Optional. If NULL, name of raster is default or name+'_'+layer number 
	for multi-band layers. }
  \item{rastlst.cat}{ String vector or list. Categorical rasters (file names 
	or raster objects). }
  \item{rastlst.cat.name}{ String vector. Output names for categorical rasters. }
  \item{rastlst.cat.NODATA}{ Numeric. NODATA value for continuous rasters. These
	values will be removed from output strata table. If 1 number, the same
	value will be used for all categorical rasters. If more than 1 number, 
	the number of values must be equal to the number of rasters in rastlst.cat. }
  \item{showsteps}{ Logical. If TRUE, show intermediate steps. }
  \item{savedata}{ Logical. If TRUE, saves data to outfolder. }
  \item{savexy}{ Logical. If TRUE, saves xy coordinates to outfolder. }
  \item{savesteps}{ Logical. If TRUE, saves intermediate data steps. }
  \item{outfolder}{ String. Name of output folder for saved data. If NULL,  
	the working directory is used. }
  \item{out_fmt}{ String. Format for output tables ('csv', 'sqlite', 'gpkg').}
  \item{out_dsn}{ String. Name of database if out_fmt = c('sqlite', 'gpkg'). }
  \item{outfn.pre}{ String. Add a prefix to output name (e.g., "01"). }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile 
	(e.g., outfn_'date'.csv). }
  \item{overwrite}{ Logical. If TRUE, overwrite existing files. }
}
 
\value{
  Data.
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

