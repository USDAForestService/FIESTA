\name{anGBpop}
\alias{anGBpop}
\title{
	Analysis - Generate estimates for custom-defined boundary.
}
\description{
	Wrapper to get plot data, stratification data and estimates within
	a custom-defined boundary. Includes FIESTA functions:
	anGBdata() (spGetPlots(), spGetStrata()), and modGBpop().	
}
\usage{
anGBpop(bnd, bnd_dsn = NULL, bnd.att = NULL, bnd.filter = NULL, popType = "VOL", 
	isseed = FALSE, datsource = "sqlite", SQLitefn = NULL, RS = NULL, 
	strat_layer = NULL, showsteps = FALSE, savedata = FALSE, savexy = TRUE, 
	outfolder = NULL, out_fmt = "csv", out_dsn = NULL, outfn.pre = NULL, 
	outfn.date = FALSE, overwrite_dsn = FALSE, overwrite_layer = TRUE,
	GBdata = NULL, ...)
}
\arguments{
  \item{bnd}{ sf R object or String. Area of interest (population boundary 
	or estimation unit). Can be a spatial polygon object, full pathname to a 
	shapefile, or name of a layer within a database. }
  \item{bnd_dsn}{ String. Data source name (dsn; e.g., sqlite or shapefile 
	pathname) of boundary. The dsn varies by driver. See gdal OGR vector formats
	(https://www.gdal.org/ogr_formats.html). Optional if bnd is sf object. } 
  \item{bnd.att}{ String. The attribute in bnd_layer that defines the
	estimation unit(s). If NULL, bnd is default as one estimation unit. } 
  \item{bnd.filter}{ String. Filter to subset bnd SpatialPolygons layer. }
  \item{popType}{ String. Type of evaluation(s) to include in population data 
	("VOL"). Note: currently only 'VOL' is available. }
  \item{isseed}{ Logical. If TRUE, extract seedling data from FIA database. }
  \item{datsource}{ String. Data source of FIA data ('sqlite', 'csv', 'datamart'). } 
  \item{SQLitefn}{ String. Full pathname to SQLite database with plot data. }
  \item{RS}{ String. Name of FIA research station to restrict states to. } 
  \item{strat_layer}{ String or raster object. Name of the strata spatial raster 
	layer, including fullpath and extension (e.g., *.img). If raster object,
	must be written to file. }
  \item{showsteps}{ Logical. If TRUE, show intermediate steps. }
  \item{savedata}{ Logical. If TRUE, saves data objects to outfolder. }
  \item{savexy}{ Logical. If TRUE, saves xy coordinates to outfolder. }
  \item{outfolder}{ String. If savedata=TRUE or exportsp=TRUE, name of output folder. 
	If NULL, the working directory is used. }
  \item{out_fmt}{ String. Format for output tables ('csv', 'sqlite', 'gpkg').}
  \item{out_dsn}{ String. Name of database if out_fmt = c('sqlite', 'gpkg'). }
  \item{outfn.pre}{ String. Add a prefix to output name (e.g., "01"). }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite_dsn}{ Logical. If TRUE, overwrites the out_dsn, if exists. }
  \item{overwrite_layer}{ Logical. If TRUE, overwrites the out_layer, if exists. }
  \item{GBdata}{ R list object. FIA data for estimation. }
  \item{...}{ Parameters for GBdata. }
}
 
\value{
  Estimate.
}
\author{
	Tracey S. Frescino
}
\examples{

  ## Get population data using strata and area estimates for evaluations ending 2015 to 2019
  WYbhfn <- system.file("extdata", "sp_data/WYbighorn_adminbnd.shp", package="FIESTA")
  GBpop <- anGBpop(WYbhfn, datsource="datamart")
  names(GBpop)

  names(GBpop$GBdata)
  names(GBpop$GBpopdat)

}
\keyword{ data }

