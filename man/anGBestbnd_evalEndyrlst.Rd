\name{anGBestbnd_evalEndyrlst}
\alias{anGBestbnd_evalEndyrlst}
\title{
	Analysis - Get estimates for Green-Book module.
}
\description{
	Wrapper to generate estimates for each evalid in list.	
}
\usage{
anGBestbnd_evalEndyrlst(evalEndyrlst, data_dsn = NULL, 
	bnd_layer, bnd_dsn = NULL, bnd.att = NULL, bnd.filter=NULL, 
	xy_dsn = NULL, xy_layer = "xyCur_ACTUAL", xy.uniqueid = "PLOT_ID",
	xvar = "LON_ACTUAL", yvar = "LAT_ACTUAL", xy.crs = 4269, 
	strat_layer = NULL, strat_lut = NULL, esttype = "AREA", landarea = "FOREST", 
	rowvar = NULL, colvar = NULL, estvar = NULL, estvar.filter = NULL, 
	savedata = FALSE, outfolder = NULL)
}
\arguments{
  \item{evalEndyrlst}{ Integer vector. Vector of one or more Evaluation 
	end years to generate estimates (YYYY). }
  \item{data_dsn}{ String. Name of database with FIA plot data. The database
	must include the following tables: plot, cond, tree (if esttype='TREE'), 
	and ppsa (POP_PLOT_STRATUM_ASSGN). If NULL, data are extracted from 
	FIA datamart. } 
  \item{bnd_layer}{ sf R object or String. Area of interest (population boundary 
	or estimation unit). Can be a spatial polygon object, full pathname to a 
	shapefile, or name of a layer within a database. }
  \item{bnd_dsn}{ String. Data source name (dsn; e.g., sqlite or shapefile 
	pathname) of boundary. The dsn varies by driver. See gdal OGR vector formats
	(https://www.gdal.org/ogr_formats.html). Optional if bnd is sf object. } 
  \item{bnd.att}{ String. The attribute in bnd_layer that defines the
	estimation unit(s). If NULL, bnd is default as one estimation unit. } 
  \item{bnd.filter}{ String. Filter to subset bnd SpatialPolygons layer. } 
  \item{xy_dsn}{ String. Name of database with XY data. } 
  \item{xy_layer}{ String. Layer name of XY data. } 
  \item{xy.uniqueid}{ String. Unique identifier of x rows. }
  \item{xvar}{ String. Name of variable in xyplt defining x coordinate. }
  \item{yvar}{ String. Name of variable in xyplt defining y coordinate. }
  \item{xy.crs}{ PROJ.4 String or CRS object or Integer EPSG code defining Coordinate 
	Reference System. (e.g., EPSG:4269-Geodetic coordinate system for 
	North America, NAD83). } 
  \item{strat_layer}{ String. Name of raster stratification layer. } 
  \item{strat_lut}{ Data frame. A look-up table of codes to aggregate. The
	format of table includes 2 columns, one column same name as strvar. 
	If strattype="RASTER", strvar="value". }
  \item{esttype}{ String. Type of estimate ('AREA', 'TREE', 'RATIO'). } 
  \item{landarea}{ String. The condition-level filter for defining land area 
	('ALL', 'FOREST', 'TIMBERLAND'). If landarea='FOREST', COND_STATUS_CD = 1; 
	if landarea='TIMBERLAND', SITECLCD in(1:6) & RESERVCD = 0. }
  \item{rowvar}{ String. Optional. Name of domain variable to group estvar by 
	for rows in table output. Rowvar must be included in an input data frame 
	(i.e., plt, cond, tree). If no rowvar is included, an estimate is returned
	for the total estimation unit. Include colvar for grouping by 2 variables. }
  \item{colvar}{ String. Optional. If rowvar != NULL, name of domain variable to 
	group estvar by for columns in table output. Colvar must be included in an 
	input data frame (i.e., plt, cond, tree). }
  \item{estvar}{ String. Name of the tree-level estimate variable (e.g., 'VOLCFNET'). }
  \item{estvar.filter}{ String. A tree-level filter for estvar. Must be R syntax 
	(e.g., 'STATUSCD == 1'). }
  \item{savedata}{ Logical. If TRUE, saves data to outfolder. }
  \item{outfolder}{ String. Name of output folder for saved data. If NULL,  
	the working directory is used. }
}
 
\value{
  Data.
}
\examples{
#evalidlst <- c(491501, 491601, 491701, 491801, 491901)
#est <- anGBest_evalidlst(evalidlst=evalidlst, esttype="AREA", landarea="FOREST",
#	rowvar="FORTYPCD")

}
\author{
	Tracey S. Frescino
}
\keyword{ data }

