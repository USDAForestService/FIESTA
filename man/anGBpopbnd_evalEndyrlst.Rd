\name{anGBpopbnd_evalEndyrlst}
\alias{anGBpopbnd_evalEndyrlst}
\title{
	Analysis - Get estimates for Green-Book module by FIA Evaluation End year.
}
\description{
	Wrapper to generate estimates for each evaluation ending in the year in list.
	If boundary crosses more than one Evaluation state, Evaluations for each 
	state ending in the same year are used. Data are subset to boundary. 	
}
\usage{
anGBpopbnd_evalEndyrlst(evalEndyrlst, data_dsn = NULL, istree = TRUE, 
  	isseed = FALSE, ppsnm = "pop_plot_stratum_assgn", pjoinid = "CN", 
	bnd_layer, bnd_dsn = NULL, bnd.att = NULL, bnd.filter=NULL, 
	xy_dsn = NULL, xy_layer = "xyCur_ACTUAL", xy.uniqueid = "PLOT_ID",
	xvar = "LON_ACTUAL", yvar = "LAT_ACTUAL", xy.crs = 4269, 
	strat_layer = NULL, strat_lut = NULL, 
	savedata = FALSE, outfolder = NULL, overwrite = FALSE)
}
\arguments{
  \item{evalEndyrlst}{ Integer vector. Vector of one or more Evaluation 
	end years to generate estimates (YYYY). }
  \item{data_dsn}{ String. Name of database with FIA plot data. The database
	must include the following tables: plot, cond, tree (if esttype='TREE'), 
	and ppsa (POP_PLOT_STRATUM_ASSGN). If NULL, data are extracted from 
	FIA datamart. }
  \item{istree}{ Logical. If TRUE, add tree data from Evaluation(s). } 
  \item{isseed}{ Logical. If TRUE, add seedling data from Evaluation(s). } 
  \item{ppsnm}{ String. Name of pop_plot_stratum_assgn table in database. } 
  \item{pjoinid}{ String. Name of variable in ppsanm to join with plot table. } 
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
  \item{savedata}{ Logical. If TRUE, saves data to outfolder. }
  \item{outfolder}{ String. Name of output folder for saved data. If NULL,  
	the working directory is used. }
  \item{overwrite}{ Logical. If TRUE, overwrites data in outfolder. }
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

