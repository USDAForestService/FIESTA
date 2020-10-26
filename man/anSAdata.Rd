\name{anSAdata}
\alias{anSAdata}
\title{
	Data - Get data for small area estimation.
}
\description{
	Wrapper to get data for small area estimation.	
}
\usage{
anSAdata(SAdoms, smallbnd = NULL, RS = NULL, clipxy = TRUE, 
	datsource = "sqlite", data_dsn = NULL, istree = TRUE, plot_layer = "plot", 
	cond_layer = "cond", tree_layer = "tree", puniqueid = "CN",
 	xy.joinid = "PLT_CN", measCur = TRUE, measEndyr = NULL, measEndyr.filter = NULL, 
	intensity1 = FALSE, rastlst.cont = NULL, rastlst.cont.name = NULL, 
	rastlst.cat = NULL, rastlst.cat.name = NULL, rastlst.cat.NODATA = NULL, 
	vars2keep = "AOI", showsteps = FALSE, savedata = FALSE, 
	savexy = FALSE, savesteps = FALSE, saveobj = FALSE, outfolder = NULL, 
	out_fmt = "csv", out_dsn = NULL, outfn.pre = NULL, outfn.date = FALSE, 
	overwrite = TRUE, SApltdat = NULL, ...)
}

\arguments{
  \item{SAdoms}{ sf R object or String. Model domains. 
	Can be a spatial sf object, full pathname to a shapefile, or name of 
	a layer within a database. }
  \item{smallbnd}{ sf R object or String. Area of Interest (AOI) boundary. 
	Can be a spatial polygon object, full pathname to a shapefile, or name of 
	a layer within a database. }
  \item{RS}{ String. Name of FIA research station to restrict states to
	('RMRS','SRS','NCRS','NERS','PNWRS'). If NULL, all research stations are included. } 
  \item{clipxy}{ Logical. If TRUE, clip xy data. } 
  \item{datsource}{ String. Data source of FIA data ('sqlite', 'csv', 'datamart'). } 
  \item{data_dsn}{ String. Data sournce name. Name of database where *_layers reside. }
  \item{istree}{ Logical. If TRUE, extract tree data from FIA database. }
  \item{plot_layer}{ String. Name of layer in database of file name of FIA plot table. }
  \item{cond_layer}{ String. Name of layer in database of file name of FIA cond table. }
  \item{tree_layer}{ String. Name of layer in database of file name of FIA tree table 
	if istree=TRUE. }
  \item{puniqueid}{ String. Name of unique identifier of plt. }
  \item{xy.joinid}{ String. Variable to join XY data to plot data. Not necessary to
	be unique. If using most current XY coordinates, use identifier for a plot 
	(e.g., PLOT_ID). }
  \item{measCur}{ Logical. If TRUE, extract plots with most current measurement for 
	state(s). }
  \item{measEndyr}{ Integer year (YYYY). If measCur=TRUE, extract plots with most 
	current measurement for state(s) for years measured before measEndyr. }
  \item{measEndyr.filter}{ Filter. If measCur=TRUE and measEndyr != NULL, a filter 
	for bnd to identify and area to use measEndyr, such as disturbed areas
	where you want to exclude plots measured after disturbance. }
  \item{intensity1}{ Logical. If TRUE, include only single intensity plots 
	(i.e., INTENSITY = 1). }
  \item{rastlst.cont}{ String vector or list. A list of raster(s) with continuous
	data values (e.g., DEM). The list may include file name of raster(s) or 
	raster objects that are not InMemory. }
  \item{rastlst.cont.name}{ String vector. Output names for continuous rasters. 
	Optional. If NULL, name of raster is used as default or name+'_'+layer number 
	for multi-band layers. }
  \item{rastlst.cat}{ String vector or list. A list of raster(s) with thematic 
	(i.e., categorical) data values. The list may include file name of raster(s) or 
	raster objects that are not InMemory. }
  \item{rastlst.cat.name}{ String vector. Output names for categorical rasters. 
	If NULL, name of raster is used as default or name+'_'+layer number 
	for multi-band layers. }
  \item{rastlst.cat.NODATA}{ Numeric vector. NODATA value for categorical rasters 
	(See notes). These values will be converted to NA and removed from output 
	if keepNA=FALSE. If 1 number, the same value will be used for all categorical 
	rasters. If more than 1 number, the number of values must be equal to the 
	number of rasters in rastlst.cat. }
  \item{vars2keep}{ String. Name of variable(s) in smallbnd to keep for output. }
  \item{showsteps}{ Logical. If TRUE, clipped points are displayed.
	If savedata=TRUE, the display is saved as *.jpg to outfolder. }
  \item{savedata}{ Logical. If TRUE, saves data to outfolder. }
  \item{savexy}{ Logical. If TRUE, save xy coordinates to rawfolder. }
  \item{savesteps}{ Logical. If TRUE, save steps spatial intermediate layers. }
  \item{saveobj}{ Logical. If TRUE, save SAest object to outfolder. }
  \item{outfolder}{ String. Name of output folder for saved data. If NULL,  
	the working directory is used. }
  \item{out_fmt}{ String. Format for output tables ('csv', 'sqlite', 'gpkg').}
  \item{out_dsn}{ String. Name of database if out_fmt = c('sqlite', 'gpkg'). }
  \item{outfn.pre}{ String. If out_fmt = "shp", a new folder is created within outfolder
	named outfn.pre. If out_fmt != "shp", a prefix is concatenated to out_dsn, using
	an underscore as separator. }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile 
	(e.g., outfn_'date'.csv). }
  \item{overwrite}{ Logical. If TRUE, overwrite existing files. }
  \item{SApltdat}{ R object. FIA plot data, output from FIESTA::spGetPlots(). }
  \item{...}{ Other parameters for DBgetPlots. }
}
 
\value{
  Data.
}
\author{
	Tracey S. Frescino
}
\note{

  If exportsp=TRUE:\cr
 	If out_fmt="shp", the writeOGR (rgdal) function is called. The ArcGIS 
	driver truncates variable names to 10 characters or less. Variable names are 
	changed before export using an internal function (trunc10shp). If Spatial o
	bject has more than 1 record, it will be returned but not exported. 
  
  The spTransform (rgdal) method is used for on-the-fly map projection conversion and 
	datum transformation using PROJ.4 arguments. Datum transformation only occurs if 
	the +datum tag is present in the both the from and to PROJ.4 strings. The +towgs84 
	tag is used when no datum transformation is needed. PROJ.4 transformations assume 
	NAD83 and WGS84 are identical unless other transformation parameters are specified. 
	Be aware, providing inaccurate or incomplete CRS information may lead to erroneous 
	data shifts when reprojecting. See spTransform help documentation for more details.
}
\keyword{ data }

