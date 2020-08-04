\name{anSApop_ecomap}
\alias{anSApop_ecomap}
\title{
	Analysis - Generate population data for Small-Area estimation.
}
\description{
	Wrapper to compile population data for generating estimates using 
	Small-Area methods. Includes creating SAdoms, extracting FIA data,
	summarizing spatial auxiliary data, and calculating population 
	adjustments.	
}
\usage{
anSApop_ecomap(smallbnd, smallbnd_dsn = NULL, smallbnd.unique, 
	smallbnd.domain = NULL, smallbnd.filter = NULL, smallbnd.stfilter = NULL, 
	smallbnd.ecofilter = NULL, maxbnd.threshold = 10, largebnd.threshold = 10,
	nbrdom.min = 10, datsource = "sqlite", SQLitefn, RS = NULL, measEndyr = NULL, 
	measEndyr.filter = NULL, rastlst.cont = NULL, rastlst.cont.name = NULL, 
	rastlst.cat = NULL, rastlst.cat.name = NULL, showsteps = FALSE, 
	savedata = FALSE, savexy = FALSE, saveobj = TRUE, outfolder = NULL, 
	outfn.pre = NULL, out_fmt = "sqlite", out_dsn = NULL, outfn.date = FALSE, 
	overwrite = TRUE, SAdomdat = NULL, SAdata = NULL, ...)
}
\arguments{
  \item{smallbnd}{ sf R object or String. Small area of interest boundary. 
	Can be a spatial polygon object, full pathname to a shapefile, or name 
	of a layer within a database. }
  \item{smallbnd_dsn}{ String. Data source name (dsn; e.g., sqlite or shapefile 
	pathname) of smallbnd. The dsn varies by driver. See gdal OGR 
	vector formats (https://www.gdal.org/ogr_formats.html). Optional if smallbnd
	is an R object. } 
  \item{smallbnd.unique}{ String. The attribute in smallbnd that defines unique 
	identifier in smallbnd that defines the unique small area(s). If NULL,
	an attribute is appended to smallbnd attribute table and used as smallbnd.unique, 
	defining one polygon (SMALLAREA="SMALLAREA"). } 
  \item{smallbnd.domain}{ String. A different attribute to use as for modeling domains.
	If NULL, smallbnd.att=smallbnd.unique. }
  \item{smallbnd.filter}{ String. A filter for smallbnd. Must be R syntax. } 
  \item{smallbnd.stfilter}{ String. A spatial filter for smallbnd to include
	only smallbnd polygons that intersect (or overlap >= 30%) the filter 
	boundary. The filter is based on the stunitco internal R object, with 
	attributes: STATECD, STATENM, UNITCD, UNITNM, COUNTYCD, COUNTYNM. The 
	filter should include one of these attributes and must be R syntax. } 
  \item{smallbnd.ecofilter}{ String. A spatial filter for smallbnd to include
	only smallbnd polygons that intersect (or overlap >= 30%) the filter 
	boundary. The filter is based on the ecomap internal R object, with 
	attributes: PROVINCE, SECTION, SUBSECTION. The filter should include 
	one of these attributes and must be R syntax. } 
  \item{maxbnd.threshold}{ Integer. Percent for including additional maxbnds for 
	selecting helperbnds. If multiSAdoms=FALSE, the maxbnd with greatest 
	percentage over the maxbnd.threshold is selected. }
  \item{largebnd.threshold}{ Integer. Percent for including additional largebnds
	for selecting helperbnds. }
  \item{nbrdom.min}{ Integer. Set number for minimum domains for modeling. If NULL,
	all domains within largebnd are selected. }
  \item{datsource}{ String. Name of datasource ('sqlite', 'datamart'). }
  \item{SQLitefn}{ String. Full pathname to SQLite database with plot data. }
  \item{RS}{ String. Name of FIA research station to restrict states to. } 
  \item{measEndyr}{ Logical. Extract plots with most current measurement for 
	state(s) for years measured before measEndyr. }
  \item{measEndyr.filter}{ Filter. If measEndyr=NULL, a filter for plots. }
  \item{rastlst.cont}{ String vector or list. Continuous rasters (file names 
	or raster objects). }
  \item{rastlst.cont.name}{ String vector. Output names for continuous rasters. 
	Optional. If NULL, name of raster is default or name+'_'+layer number 
	for multi-band layers. }
  \item{rastlst.cat}{ String vector or list. Categorical rasters (file names 
	or raster objects). }
  \item{rastlst.cat.name}{ String vector. Output names for categorical rasters. }
  \item{showsteps}{ Logical. If TRUE, shows steps for creating SAdoms. }
  \item{savedata}{ Logical. If TRUE, saves data objects to outfolder. }
  \item{savexy}{ Logical. If TRUE, saves xy coordinates to outfolder. }
  \item{saveobj}{ Logical. If TRUE, saves data list objects to outfolder. }
  \item{outfolder}{ String. If savedata=TRUE or exportsp=TRUE, name of output folder. 
	If NULL, the working directory is used. }
  \item{outfn.pre}{ String. Add a prefix to output name (e.g., "01"). }
  \item{out_fmt}{ String. Format for output tables ('csv', 'sqlite', 'gpkg').}
  \item{out_dsn}{ String. Name of database if out_fmt = c('sqlite', 'gpkg'). }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
  \item{SAdomdat}{ R list object. SA domain data. }
  \item{SAdata}{ R list object. FIA data for estimation. }
  \item{...}{ Parameters for SAdata. }
}
 
\value{
  Estimate.
}
\author{
	Tracey S. Frescino
}
\keyword{ data }

