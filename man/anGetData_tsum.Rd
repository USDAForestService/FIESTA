\name{anGetData_tsum}
\alias{anGetData_tsum}
\title{
	Data - Get plot data and auxiliary data for estimation.
}
\description{
	Wrapper to get plot data and auxiliary data for estimation.	
}
\usage{
anGetData_tsum(bnd_layer, bnd_dsn=NULL, bnd.att=NULL, bnd.filter=NULL, RS = NULL, 
	xy_datsource = NULL, xy = NULL, xy_dsn = NULL, xyjoinid = "PLOT_ID", 
	clipxy = TRUE, datsource = "sqlite", data_dsn = NULL, 
	isseed = FALSE, plot_layer = "plot", cond_layer = "cond", tree_layer = "tree", 
	seed_layer = "seed", puniqueid = "CN", intensity1 = TRUE,
	estvarlst = NULL, estvar.filter="STATUSCD == 1", TPA = TRUE, getadjplot = TRUE, 
	rastfolder = NULL, rastlst.cont = NULL, rastlst.cont.name = NULL, 
	rastlst.cont.NODATA = NULL, rastlst.cat = NULL, rastlst.cat.name = NULL, 
	rastlst.cat.NODATA = NULL, strata = FALSE, strvar = NULL, vars2keep = NULL, 
	minplots = 0, showsteps = FALSE, savedata = FALSE, savexy = FALSE, savesteps = FALSE, 
	saveobj = FALSE, objnm = "tsumdat", outfolder = NULL, out_fmt = "csv", out_dsn = NULL, 
	outfn.pre = NULL, outfn.date = FALSE, overwrite_dsn = FALSE, overwrite_layer = TRUE, 
	append_layer = FALSE, pltdat = NULL, ...)
}

\arguments{
  \item{bnd_layer}{ sf R object or String. Boundary layer of estimation 
	domain units. Can be a spatial sf object, full pathname to a shapefile, 
	or name of a layer within a database. }
  \item{bnd_dsn}{ String. Name of data source name with bnd_layer, if in a database. }
  \item{bnd.att}{ String. Name of attribute in boundary file to define estimation
	domain units. }
  \item{bnd.filter}{ String. Optional filter of bnd_layer. }
  \item{RS}{ String. Name of FIA research station to restrict states to
	('RMRS','SRS','NCRS','NERS','PNWRS'). If NULL, all research stations are included. }
  \item{xy_datsource}{ String. Data source of XY data ('sqlite', 'gdb', 'csv', 'datamart') } 
  \item{xy}{ sf R object or String. Table with xy coordinates. Can be a spatial 
	polygon object, data frame, full pathname to a shapefile, or name of a 
	layer within a database. }
  \item{xy_dsn}{ String. Data source name (dsn; i.e., pathname or database name)
	of xy. The dsn varies by driver. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html). Optional if bnd_layer is an R object. } 
  \item{xyjoinid}{ String. Variable to join XY data to plot data. Not necessary to
	be unique. If using most current XY coordinates, use identifier for a plot 
	(e.g., PLOT_ID). }
  \item{clipxy}{ Logical. If TRUE, clip xy data. } 
  \item{datsource}{ String. Data source of FIA data ('sqlite', 'gdb', 'csv', 'datamart'). } 
  \item{data_dsn}{ String. Data sournce name. Name of database where *_layers reside. }
  \item{istree}{ Logical. If TRUE, extract tree data from FIA database. }
  \item{isseed}{ Logical. If TRUE, extract seedling data from FIA database. }
  \item{plot_layer}{ String. Name of layer in database of file name of FIA plot table. }
  \item{cond_layer}{ String. Name of layer in database of file name of FIA cond table. }
  \item{tree_layer}{ String. Name of layer in database of file name of FIA tree table 
	if istree=TRUE. }
  \item{seed_layer}{ String. Name of layer in database of file name of FIA seedling 
	table if isseed=TRUE. }
  \item{puniqueid}{ String. Name of unique identifier of plt. }
  \item{intensity1}{ Logical. If TRUE, include only single intensity plots 
	(i.e., INTENSITY = 1). }
  \item{estvarlst}{ String (vector). Tree-level variable(s) estimation variables
	(e.g., "TPA_UNADJ", "BA"). }
  \item{estvar.filter}{ String. A filter for all variables in estvarlst. }
  \item{TPA}{ Logical. If TRUE, tsumvarlst variable(s) are multiplied by the respective 
	trees-per-acre variable to get per-acre measurements. }
  \item{getadjplot}{ Logical. If TRUE, adjustments are calculated for nonsampled 
	conditions on plot. }
  \item{rastfolder}{ String. Name of the folder with raster layers. Optional. 
	Useful if all raster layers are in same folder. }
  \item{rastlst.cont}{ String vector or list. A list of raster(s) with continuous
	data values (e.g., DEM). The list may include file name of raster(s) or 
	raster objects that are not InMemory. }
  \item{rastlst.cont.name}{ String vector. Output names for continuous rasters. 
	Optional. If NULL, name of raster is used as default or name+'_'+layer number 
	for multi-band layers. }
  \item{rastlst.cont.NODATA}{ Numeric vector. NODATA value for categorical rasters 
	(See notes). These values will be converted to NA and removed from output 
	if keepNA=FALSE. If 1 number, the same value will be used for all categorical 
	rasters. If more than 1 number, the number of values must be equal to the 
	number of rasters in rastlst.cont. }
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
  \item{strata}{ Logical. If TRUE, include stratalut in output dataset. }
  \item{strvar}{ String. Name of strata variable to include in stratalut. 
	Name must be in rastlst.cat.name or rastlst.cat (if rastlst.cat.name= NULL). }
  \item{vars2keep}{ String. Name of variable(s) in smallbnd to keep for output. }
  \item{minplots}{ Integer. Minimum number of plots in any on domain unit. } 
  \item{showsteps}{ Logical. If TRUE, clipped points are displayed.
	If savedata=TRUE, the display is saved as *.jpg to outfolder. }
  \item{savedata}{ Logical. If TRUE, saves data to outfolder. }
  \item{savexy}{ Logical. If TRUE, save xy coordinates to rawfolder. }
  \item{savesteps}{ Logical. If TRUE, save steps spatial intermediate layers. }
  \item{saveobj}{ Logical. If TRUE, save SAest object to outfolder. }
  \item{objnm}{ String. Name of saved object name. }
  \item{outfolder}{ String. Name of output folder for saved data. If NULL,  
	the working directory is used. }
  \item{out_fmt}{ String. Format for output ('csv', 'sqlite', 'db', 'sqlite3', 
	'db3', 'gpkg', 'gdb'). If out_fmt='gdb', must have ArcGIS license and 
	install arcgisbinding package. }
  \item{out_dsn}{ String. Name of database if out_fmt != 'csv'. }
  \item{outfn.pre}{ String. If out_fmt = "shp", a new folder is created within outfolder
	named outfn.pre. If out_fmt != "shp", a prefix is concatenated to out_dsn, using
	an underscore as separator. }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile 
	(e.g., outfn_'date'.csv). }
  \item{overwrite_dsn}{ Logical. If TRUE, overwrites out_dsn. }
  \item{overwrite_layer}{ Logical. If TRUE, overwrites layers in out_dsn
	or files if out_fmt = 'csv'. }
  \item{append_layer}{ Logical. If TRUE, appends layers to existing out_dsn
	or files if out_fmt = 'csv'. Note: currently cannot append layers if
	out_fmt = "gdb". }
  \item{pltdat}{ R object. FIA plot data, output from FIESTA::spGetPlots(). }
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

