\name{anMAdata}
\alias{anMAdata}
\title{
	Analysis - Generate estimates for state boundaries.
}
\description{
	Wrapper to get estimates using Model-assisted methods - generalized
		regression estimator (GREG).	
}
\usage{
anMAdata(bnd_layer, bnd_dsn=NULL, bnd.att=NULL, bnd.filter=NULL, 
	RS = NULL, clipxy = TRUE, datsource = "sqlite", data_dsn = NULL, 
	istree = TRUE, isseed = FALSE, plot_layer = "plot", cond_layer = "cond", 
	tree_layer = "tree", seed_layer = "seed", puniqueid = "CN", intensity1 = TRUE, 
	rastfolder = NULL, rastlst.cont = NULL, rastlst.cont.name = NULL, 
	rastlst.cat = NULL, rastlst.cat.name = NULL, rastlst.cat.NODATA = NULL, 
	showsteps = FALSE, cex.plots=0.5, savedata = FALSE, savexy = FALSE, 
	savesteps = FALSE, saveobj=FALSE, outfolder = NULL, out_fmt = "csv", 
	out_dsn = NULL, outfn.pre = NULL, outfn.date = FALSE, overwrite_dsn = FALSE, 
	overwrite_layer = TRUE, MApltdat=NULL, ...)
}

\arguments{
  \item{bnd_layer}{ sf R object or String. Area of interest (population boundary 
	or estimation unit). Can be a spatial polygon object, full pathname to a 
	shapefile, or name of a layer within a database. }
  \item{bnd_dsn}{ String. Data source name (dsn; e.g., sqlite or shapefile 
	pathname) of boundary. The dsn varies by driver. See gdal OGR vector formats
	(https://www.gdal.org/ogr_formats.html). Optional if bnd is sf object. } 
  \item{bnd.att}{ String. The attribute in bnd_layer that defines the
	estimation unit(s). If NULL, bnd is default as one estimation unit. } 
  \item{bnd.filter}{ String. Filter to subset bnd SpatialPolygons layer. }
  \item{RS}{ String. Name of FIA research station to restrict states to. } 
  \item{clipxy}{ Logical. If TRUE, clip xy data. } 
  \item{datsource}{ String. Data source of FIA data ('sqlite', 'csv', 'datamart'). } 
  \item{data_dsn}{ String. Name of database where *_layers reside. }
  \item{istree}{ Logical. If TRUE, extract tree data from FIA database. }
  \item{isseed}{ Logical. If TRUE, extract seedling data from FIA database. }
  \item{plot_layer}{ String. Name of layer in database of file name of FIA plot table. }
  \item{cond_layer}{ String. Name of layer in database of file name of FIA cond table. }
  \item{tree_layer}{ String. Name of layer in database of file name of FIA tree table 
	if istree=TRUE. }
  \item{seed_layer}{ String. Name of layer in database of file name of FIA seedling 
	table if isseed=TRUE. }
  \item{puniqueid}{ String. Name of unique identifier of plt. }
  \item{intensity1}{ Logical. If TRUE, include only plots with INTENSITY=1. }
  \item{rastfolder}{ String. Name of the folder with raster layers. Optional. 
	Useful if all raster layers are in same folder. }
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
  \item{showsteps}{ Logical. If TRUE, clipped points are displayed.
	If savedata=TRUE, the display is saved as *.jpg to outfolder. }
  \item{cex.plots}{ Number. Size of dots representing plots in display. }
  \item{savedata}{ Logical. If TRUE, saves data to outfolder. }
  \item{savexy}{ Logical. If TRUE, saves xy coordinates to outfolder. }
  \item{savesteps}{ Logical. If TRUE, saves plotting steps as jpg images to outfolder. }
  \item{saveobj}{ Logical. If TRUE, save SAest object to outfolder. }
  \item{outfolder}{ String. Name of output folder for saved data. If NULL,  
	the working directory is used. }
  \item{out_fmt}{ String. Format for output tables ('csv', 'sqlite', 'gpkg').}
  \item{out_dsn}{ String. Name of database if out_fmt = c('sqlite', 'gpkg'). }
  \item{outfn.pre}{ String. Add a prefix to output name (e.g., "01"). }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile 
	(e.g., outfn_'date'.csv). }
  \item{overwrite_dsn}{ Logical. If TRUE, overwrites the out_dsn, if exists. }
  \item{overwrite_layer}{ Logical. If TRUE, overwrites the out_layer, if exists. }
  \item{MApltdat}{ R object. FIA plot data, output from FIESTA::spGetPlots(). }
  \item{...}{ Parameters for spGetPlots(). }
}
 
\value{
  Data.
}
\author{
	Tracey S. Frescino
}
\note{

  If exportsp=TRUE:\cr
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

