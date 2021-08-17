\name{spGetXY}
\alias{spGetXY}
\title{
	Spatial - Extracts data within a given boundary.
}
\description{
	Wrapper to get FIA plots within the boundary population (area of interest)
  	- Intersect with state boundary 
  	- Get FIA plots for intersected states, including tree, and spatial
  	- Clip spatial coordinates and other tables to boundary (spClipPoint)
}
\usage{
spGetXY(bnd, bnd_dsn = NULL, bnd.filter = NULL, states = NULL, RS = NULL, 
	xy = NULL, xy_dsn = NULL, xy.uniqueid = "PLT_CN", 
	xvar = NULL, yvar = NULL, xy.crs = 4269, xyjoinid = NULL, pjoinid = "CN", 
	xy_datsource = NULL, clipxy = TRUE, plot_layer = "plot", evalid = NULL, 
	evalCur = FALSE, evalEndyr = NULL, measCur = FALSE, measEndyr = NULL, 
	measEndyr.filter = NULL, invyrs = NULL, allyrs = FALSE, intensity1 = FALSE, 
	showsteps = FALSE, savedata = FALSE, exportsp = FALSE, returnxy = TRUE, 
	outfolder = NULL, out_fmt = "csv", out_dsn = NULL, out_layer = "xyplt", 
	outfn.pre = NULL, outfn.date = FALSE, overwrite_dsn = FALSE, 
	overwrite_layer = FALSE, append_layer = FALSE)
}
\arguments{
  \item{bnd}{ sf R object, Area of Interest (AOI) boundary. Can be a spatial 
	sf object, full pathname to a shapefile, or name of a layer within a database. }
  \item{bnd_dsn}{ String. Data source name (dsn; e.g., sqlite or shapefile 
	pathname) of bnd. The dsn varies by driver. See gdal OGR 
	vector formats (https://www.gdal.org/ogr_formats.html). Optional if bnd
	is an R object. } 
  \item{bnd.filter}{ String. Filter to subset bnd spatial layer. }
  \item{states}{ String. The name of state(s) for tables (e.g., "Vermont", "Utah"). }
  \item{RS}{ String. Name of FIA research station to restrict states to
	('RMRS','SRS','NCRS','NERS','PNWRS'). If NULL, all research stations are included. } 
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
  \item{xyjoinid}{ String. Variable in xy to join to plot data. If NULL, 
	xyjoinid=xy.uniqueid. }
  \item{pjoinid}{ String. Variable in plt to join to XY data. Not necessary to
	be unique. If using most current XY coordinates, use identifier for a plot 
	(e.g., PLOT_ID). }
  \item{xy_datsource}{ String. Source of XY data ("obj", "csv", "datamart", "sqlite").  
	If datsource=NULL, checks extension of xy_dsn or xy to identify datsource. }
  \item{clipxy}{ Logical. If TRUE, clips xy data to bnd. }
  \item{plot_layer}{ String. Name of plot_layer in database. }
  \item{evalid}{ Integer. To extract data for a specific evaluation period. See notes 
	for more information about FIA Evaluations. }
  \item{evalCur}{ Logical. If TRUE, extract plots with most current FIA Evalidation 
	for state(s). }
  \item{evalEndyr}{ Integer. Defining end year of Evaluation (yyyy). }
  \item{measCur}{ Logical. If TRUE, extract plots with most current measurement for 
	state(s). }
  \item{measEndyr}{ Integer year (YYYY). If measCur=TRUE, extract plots with most 
	current measurement for state(s) for years measured before measEndyr. }
  \item{measEndyr.filter}{ Filter. If measCur=TRUE and measEndyr != NULL, a filter 
	for bnd to identify and area to use measEndyr, such as disturbed areas
	where you want to exclude plots measured after disturbance. }
  \item{invyrs}{ Integer vector. Defining specific years of data (e.g., 2010:2015). }
  \item{allyrs}{ Logical. If TRUE, selects all years (annual inventory) in database. }
  \item{intensity1}{ Logical. If TRUE, include only single intensity plots 
	(i.e., INTENSITY = 1). }
  \item{showsteps}{ Logical. If TRUE, display data in device window. }
  \item{savedata}{ Logical. If TRUE, saves data to outfolder. }
  \item{exportsp}{ Logical. If TRUE, exports spatial data to outfolder. }
  \item{returnxy}{ Logical. If TRUE, returns xy coordinates. }
  \item{outfolder}{ String. If savedata=TRUE or savexy=TRUE, savebnd=TRUE, 
	name of output folder. If NULL, the working directory is used. }
  \item{out_fmt}{ String. Format for output ('csv', 'sqlite', 'db', 'sqlite3', 
	'db3', 'gpkg', 'gdb'). If out_fmt='gdb', must have ArcGIS license and 
	install arcgisbinding package. }
  \item{out_dsn}{ String. Name of database if out_fmt != 'csv'. }
  \item{out_layer}{ String. Name of layer in database if out_fmt != 'csv'. }
  \item{outfn.pre}{ String. Add a prefix to output name (e.g., "01"). }
  \item{outfn.date}{ Logical. If TRUE, adds current date to out_dsn name 
	or file name if out_fmt = 'csv'. }
  \item{overwrite_dsn}{ Logical. If TRUE, overwrites out_dsn. Note: cannot
	overwrite out_fmt="gdb". }
  \item{overwrite_layer}{ Logical. If TRUE, overwrites layers in out_dsn
	or files if out_fmt = 'csv'. }
  \item{append_layer}{ Logical. If TRUE, appends layers to existing out_dsn
	or files if out_fmt = 'csv'. Note: currently cannot append layers if
	out_fmt = "gdb". }
}
\value{
  \item{xypltx}{ sf object. Input xy data clipped to boundary. }
  \item{bndx}{ sf object. Input bnd. }
  \item{tabs}{ list object. List of input layers clipped to boundary (pltx,condx,etc.). }
  \item{xy.uniqueid}{ String. Name of unique identifier of xy. }
  \item{puniqueid}{ String. Name of unique identifier of plot in plt. }
  \item{pjoinid}{ String. Name of unique identifier of plot in plt. }

  If savedata=TRUE, outdat data frame is saved to outfolder.    
}
\details{

	\bold{datsource}

	Plots are extracted from 3 different data sources:\cr
	1) CSV - data have previously been extracted from the FIA database and stored 
		as CSV files.\cr
	2) datamart - data are extracted from FIA's publically-available datamart.\cr
	3) sqlite - data have previously been extracted from the FIA database and stored 
		within a SQLite database.\cr 


	\bold{Selection parameters}

	FIA plots are selected based on the following parameters:\cr
	\tabular{ll}{
	\tab evalid - the FIA evaluation identifier\cr 
	\tab evalCur - the most current FIA evaluation in database\cr 
	\tab evalEndyr - the FIA evaluation ending in evalEndyr\cr 
	\tab evalType - the FIA evaluation type ('ALL', 'AREAVOL', 'GRM', 'P2VEG', 'DWM', 'INV', 'REGEN', 'CRWN')\cr 
	\tab measCur - the most current measurement of each plot in database\cr 
	\tab measEndyr - the most current measuremtn of each plot in database in or prior to measEndyr\cr 
	\tab Endyr.filter - a filter for bnd that specifies the boundary where measEndyr should be applied\cr 
 	}
}


\note{

  If savebnd=TRUE:\cr
 	If out_fmt=c('csv','shp'), the writeOGR (rgdal) function is called. The ArcGIS driver 
	truncates variable names to 10 characters or less. Variable names are changed 
	before export using an internal function (trunc10shp). If Spatial object has 
	more than 1 record, it will be returned but not exported. 

  If datsource="datmart", (default), data are imported from FIA DataMart. 
	The plot coordinates have been altered for privacy 
	(See https://www.fia.fs.fed.us/tools-data/spatial/Policy/index.php for details). 
	The zip files are extracted on-the-fly from the online website. Web server connections 
	will affect download speeds.
		
}
\author{
	Tracey S. Frescino
}

\keyword{ data }
