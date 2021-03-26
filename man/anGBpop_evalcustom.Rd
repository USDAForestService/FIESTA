\name{anGBpop_evalcustom}
\alias{anGBpop_evalcustom}
\title{
	Analysis - Get estimates for Green-Book module by FIA Evaluation End year.
}
\description{
	Wrapper to generate estimates for each evaluation ending in the year in list.
	If boundary crosses more than one Evaluation state, Evaluations for each 
	state ending in the same year are used. Data are subset to boundary. 	
}
\usage{
anGBpop_evalcustom(evalidlst=NULL, evalCur=FALSE, evalEndyrlst=NULL, 
	bnd_layer, bnd_dsn = NULL, bnd.att = NULL, bnd.filter=NULL, evalType = "VOL", 
	datsource = "datamart", data_dsn=NULL, isseed = FALSE, intensity1 = FALSE, 
	xymeasCur = TRUE, pjoinid = "PLOT_ID", xy_dsn = NULL, xy_layer = "xyCur_ACTUAL", 
	xy.uniqueid = "PLOT_ID", xvar = "LON_ACTUAL", yvar = "LAT_ACTUAL", 
	xy.crs = 4269, strat_layer = NULL, strat_lut = NULL, byEndyr = FALSE, 
	savedata = FALSE, outfolder=NULL, ...)
}
\arguments{
  \item{evalidlst}{ Integer vector. Vector of one or more evalid (e.g, 491801). }
  \item{evalCur}{ Logical. If TRUE, gets the most current evaluation for
	each state. }
  \item{evalEndyrlst}{ Integer vector. Vector of one or more Evaluation 
	end years to generate estimates (YYYY). }
  \item{bnd_layer}{ sf R object or String. Area of interest (population boundary 
	or estimation unit). Can be a spatial polygon object, full pathname to a 
	shapefile, or name of a layer within a database. }
  \item{bnd_dsn}{ String. Data source name (dsn; e.g., sqlite or shapefile 
	pathname) of boundary. The dsn varies by driver. See gdal OGR vector formats
	(https://www.gdal.org/ogr_formats.html). Optional if bnd is sf object. } 
  \item{bnd.att}{ String. The attribute in bnd_layer that defines the
	estimation unit(s). If NULL, bnd is default as one estimation unit. } 
  \item{bnd.filter}{ String. Filter to subset bnd SpatialPolygons layer. } 
  \item{evalType}{ String vector. The type(s) of evaluation of interest 
	('ALL', 'CURR', VOL', 'GRM', 'P2VEG', 'DWM", 'INV', 'REGEN', 'CRWN'). 
	The evalType 'ALL' includes nonsampled plots; 'CURR' includes plots 
	used for area estimates; 'VOL' includes plots used for area and/or tree
	estimates; The evalType 'GRM' includes plots used for growth, removals,
	mortality, and change estimates (eval_typ %in% c(GROW, MORT, REMV, CHNG)). 
	Multiple types are accepted. See details below and FIA database manual 
	for regional availability and/or differences. }
  \item{datsource}{ String. Source of data ('datamart', 'sqlite'). }
  \item{data_dsn}{ If datsource='sqlite', the file name (data source name) of the 
	sqlite database (*.sqlite). }
  \item{isseed}{ Logical. If TRUE, add seedling data from Evaluation(s). } 
  \item{intensity1}{ Logical. If TRUE, extracts single intensity plots only from
	database. Assures equal probability sample within boundary. }
  \item{xymeasCur}{ Logical. If TRUE, and more than one plot measured at same location,
	only the most current coordinate is returned for the plot. }
  \item{pjoinid}{ String. Name of variable in ppsanm to join with plot table. } 
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
  \item{byEndyr}{ Logical. If TRUE, organizes evalidlst by evalEndyr. 
	Set TRUE if estimates include more than one state (e.g., if area of 
	interest crosses more than one state - Manti-LaSal). }
  \item{savedata}{ Logical. If TRUE, saves data to outfolder. }
  \item{outfolder}{ String. The output folder path. If NULL and savedata=TRUE, 
	outfolder is the working directory. }
  \item{...}{ Parameters to FIESTA::modGBpop(). }
}
 
\value{
  Set of population data (from modGBpop()) for bnd.
}
\examples{
  ## Get population data and area estimates for WY Bighorn National Forest 
  ##		for evaluations ending 2015 to 2019
  WYbhfn <- system.file("extdata", "sp_data/WYbighorn_adminbnd.shp", package="FIESTA")
  fornffn <- system.file("extdata", "sp_data/WYbighorn_forest_nonforest_250m.tif", package="FIESTA")

  GBpoplst <- anGBpop_evalcustom(evalEndyrlst=c(2015:2019), bnd_layer=WYbhfn, 
	xymeasCur=TRUE, xy.uniqueid="PLOT_ID", evalType="CURR", strat_layer=fornffn)

  estlst <- lapply(GBpoplst, function(x) modGBarea(GBpopdat=x, rowvar="FORTYPGRPCD", 
	landarea="FOREST", row.FIAname=TRUE))
  names(estlst)
  names(estlst$eval561501)
}
\author{
	Tracey S. Frescino
}
\keyword{ data }

