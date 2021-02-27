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
anGBpop_evalcustom(evalidlst=NULL, evalEndyrlst=NULL, bnd_layer, 
	bnd_dsn = NULL, bnd.att = NULL, bnd.filter=NULL, evalType = "VOL", 
	data_dsn = NULL, isseed = FALSE, xymeasCur = TRUE, pjoinid = "PLOT_ID", 
	xy_dsn = NULL, xy_layer = "xyCur_ACTUAL", xy.uniqueid = "PLOT_ID", 
	xvar = "LON_ACTUAL", yvar = "LAT_ACTUAL", xy.crs = 4269, 
	strat_layer = NULL, strat_lut = NULL, savedata = FALSE, 
	out_dsn = NULL, out_fmt = "sqlite", outfolder=NULL, outfn.pre = NULL, 
	outfn.date = FALSE, overwrite_dsn = FALSE, overwrite_layer = TRUE)
}
\arguments{
  \item{evalidlst}{ Integer vector. Vector of one or more evalid (e.g, 491801). }
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
  \item{data_dsn}{ String. Name of database with FIA plot data. The database
	must include the following tables: plot, cond, tree (if esttype='TREE'), 
	and ppsa (POP_PLOT_STRATUM_ASSGN). If NULL, data are extracted from 
	FIA datamart. }
  \item{isseed}{ Logical. If TRUE, add seedling data from Evaluation(s). } 
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
  \item{savedata}{ Logical. If TRUE, saves data to outfolder. }
  \item{out_dsn}{ String. Data source name for output. If extension is not
	included, out_fmt is used. Use full path if outfolder=NULL. }
  \item{out_fmt}{ String. File format for output ('csv', 'sqlite','gpkg', 'gdb').
	If out_fmt %in% c('sqlite','gpkg'), RSQLite package must be installed. If
	out_fmt='gdb', arcgisbinding package and R-Bridge must be installed. }
  \item{outfolder}{ String. The output folder path. If NULL and savedata=TRUE, 
	outfolder is the working directory. }
  \item{outfn.pre}{ String. The name used for prefix of outfiles (e.g., outfn.pre'_plt*'). }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile (e.g., outfn_'date'.csv). }
  \item{overwrite_dsn}{ Logical. If TRUE, overwrites the out_dsn, if exists. }
  \item{overwrite_layer}{ Logical. If TRUE, overwrites the out_layer, if exists. }
}
 
\value{
  Data.
}
\examples{
  ## Get population data and area estimates for evaluations ending 2015 to 2019
  WYbhfn <- system.file("extdata", "sp_data/WYbighorn_adminbnd.shp", package="FIESTA")
  GBpoplst <- anGBpop_evalcustom(evalEndyrlst=c(2015:2019), bnd_layer=WYbhfn, 
	xymeasCur=TRUE, xy.uniqueid="PLOT_ID", evalType="CURR")

  est <- lapply(GBpoplst, function(x) modGBarea(GBpopdat=x, rowvar="FORTYPGRPCD", 
	landarea="FOREST", row.FIAname=TRUE))
  names(est)

  ## Get population data using strata and area estimates for evaluations ending 2015 to 2019
  fornffn <- system.file("extdata", "sp_data/WYbighorn_forest_nonforest_250m.tif", package="FIESTA")
  GBpoplst2 <- anGBpop_evalcustom(evalEndyrlst=c(2015:2019), bnd_layer=WYbhfn, 
	xymeasCur=TRUE, xy.uniqueid="PLOT_ID", evalType="CURR", strat_layer=fornffn)

  est.strat <- lapply(GBpoplst2, function(x) modGBarea(GBpopdat=x, rowvar="FORTYPGRPCD", 
	landarea="FOREST", row.FIAname=TRUE))

  est$eval2019
  est.strat$eval2019
}
\author{
	Tracey S. Frescino
}
\keyword{ data }

