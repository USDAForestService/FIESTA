\name{anSApop_ecomap}
\alias{anSApop_ecomap}
\title{
	Analysis - Generate population data for Small-Area estimation.
}
\description{
	Wrapper to compile population data for generating estimates using 
	Small Area Estimation (SAE) methods (modSA*). Includes: creating model 
	domain units (spGetSAdoms); extracting FIA data and summarizing spatial 
	auxiliary data (anSAdata); and calculating population adjustments (modSApop).

	The selection of model domain units are set to the hierarchical-based, 
	2007 Cleland Ecomap boundaries (Cleland, and others 2007), where the 
	Subsection boundaries are used for delineating helper polygons; the 
	Section boundaries are used for defining the large area; and the Province 
	boundaries are used for restraining the automated selection process.	
}
\usage{
anSApop_ecomap(smallbnd, smallbnd_dsn = NULL, smallbnd.unique, 
	smallbnd.domain = NULL, smallbnd.filter = NULL, smallbnd.stfilter = NULL, 
	smallbnd.ecofilter = NULL, maxbnd.threshold = 10, largebnd.threshold = 10,
	nbrdom.min = 10, datsource = "sqlite", SQLitefn, RS = NULL, measEndyr = NULL, 
	measEndyr.filter = NULL, rastlst.cont = NULL, rastlst.cont.name = NULL, 
	rastlst.cat = NULL, rastlst.cat.name = NULL, showsteps = FALSE, 
	savedata = FALSE, savexy = FALSE, saveobj = TRUE, outfolder = NULL, 
	out_fmt = "sqlite", out_dsn = NULL, outfn.date = FALSE, overwrite_dsn = FALSE, 
	overwrite_layer = TRUE, SAdomdat = NULL, SAdata = NULL, ...)
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
  \item{out_fmt}{ String. Format for output tables ('csv', 'sqlite', 'gpkg').}
  \item{out_dsn}{ String. Name of database if out_fmt = c('sqlite', 'gpkg'). }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite_dsn}{ Logical. If TRUE, overwrites the out_dsn, if exists. }
  \item{overwrite_layer}{ Logical. If TRUE, overwrites the out_layer, if exists. }
  \item{SAdomdat}{ R list object. SA domain data. }
  \item{SAdata}{ R list object. FIA data for estimation. }
  \item{...}{ Parameters for SAdata. }
}
 
\value{
  A list with population data for Small-Area estimates. 

  \item{SAdomsdf}{ Data frame. Attribute table from SAdoms spatial layer. }
  \item{condx}{ Data frame. Condition-level data with adjusted condition proportions
		and predictor values. }
  \item{pltcondx}{ Data frame. Plot/Condition data used for estimation. }
  \item{cuniqueid}{ String. Unique identifier of plot in condx and pltcondx. }
  \item{condid}{ String. Unique identifier of condition in condx and pltcondx. }
  \item{treex}{ Data frame. If esttype='TREE', tree-level data, including sample 
	adjustment factor. }
  \item{tuniqueid}{ String. If esttype='TREE', unique identifier of plot in treex. }
  \item{ACI.filter}{ String. If ACI=FALSE, ACI.filter="COND_STATUS_CD == 1" . }
  \item{dunitarea}{ Data frame. Area by model domain unit. }
  \item{areavar}{ String. Name of area variable in dunitarea. }
  \item{dunitvar}{ String. Name of variable defining model domain units in dunitarea. }
  \item{dunitlut}{ Data frame. Table of model domain units with zonal statistics 
	of predictor values, number of plots by domain unit. }
  \item{prednames}{ String vector. Name of variables in dunitlut and condx 
	defining small area model predictors. }
  \item{plotsampcnt}{ Data frame. Number of plots by PLOT_STATUS_CD. }
  \item{condsampcnt}{ Data frame. Number of conditions by COND_STATUS_CD. }
  \item{states}{ String. State names in dataset. }
  \item{invyrs}{ String. Range of inventory years in dataset. }
  \item{adjtree}{ Logical. If TRUE, treex includes adjustment factors. }
}

\references{
	Cleland, D.T.; Freeouf, J.A.; Keys, J.E., Jr.; Nowacki, G.J.; Carpenter, C; 
	McNab, W.H. 2007. Ecological Subregions: Sections and Subsections of the 
	Conterminous United States [1:3,500,000] [CD-ROM]. Sloan, A.M., cartog. 
	Gen. Tech. Report WO-76. Washington, DC: U.S. Department of Agriculture, 
	Forest Service.
}

\author{
	Tracey S. Frescino
}
\keyword{ data }

