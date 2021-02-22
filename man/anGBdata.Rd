\name{anGBdata}
\alias{anGBdata}
\title{
	Analysis - Get data for Green-Book module.
}
\description{
	Wrapper to get plot data and stratification data within a boundary.	
}
\usage{
anGBdata(bnd_layer, bnd_dsn = NULL, bnd.att = NULL, bnd.filter=NULL,
	RS = NULL, clipxy = TRUE, datsource = "sqlite", data_dsn = NULL, 
	istree = TRUE, isseed = FALSE, plot_layer = "plot", cond_layer = "cond", 
	tree_layer = "tree", seed_layer = "seed", puniqueid = "CN", 
	intensity1 = TRUE, strata = TRUE, strattype = "RASTER", 
	strat_layer = NULL, strat_dsn = NULL, strvar = NULL, showsteps = FALSE, 
	cex.plots = 0.5, savedata = FALSE, savexy = TRUE, savesteps = FALSE, 
	saveobj = FALSE, outfolder = NULL, out_fmt = "csv", out_dsn = NULL, 
	outfn.pre = NULL, outfn.date = FALSE, overwrite_dsn = FALSE, 
	overwrite_layer = TRUE, GBpltdat = NULL, ...)
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
  \item{strata}{ Logical. If TRUE, get strata weights from strat_layer. }
  \item{strattype}{ String. Spatial layer type of stratlayer ("POLY", "RASTER"). 
	Note: raster objects must be written to file. }
  \item{strat_layer}{ String or raster object. Name of the strata spatial raster 
	layer, including fullpath and extension (e.g., *.img). If raster object,
	must be written to file. }
  \item{strat_dsn}{ String. The data source name (dsn; i.e., folder or database name, 
	if polygon layer) of strat_layer. The dsn varies by driver. See gdal OGR vector 
	formats (https://www.gdal.org/ogr_formats.html). Optional. } 
  \item{strvar}{ String. If strattype="POLY", name of strata attribute in 
	strat_layer. }
  \item{showsteps}{ Logical. If TRUE, show intermediate steps. }
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
  \item{GBpltdat}{ R object. FIA plot data, output from FIESTA::spGetPlots(). }
  \item{...}{ Parameters for spGetPlots(). }
}
 
\value{
  Data.
}
\author{
	Tracey S. Frescino
}
\keyword{ data }

