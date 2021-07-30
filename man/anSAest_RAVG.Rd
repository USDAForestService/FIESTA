\name{anSAest_RAVG}
\alias{anSAest_RAVG}
\title{
	Analysis - Generate unit-level and area-level estimates for fire perimeters.
}
\description{
	Analysis function for streamlining small area estimation strategies for 
	fire perimeters from the USDA Forest Service, Rapid Assessment of Vegetation 
	Condition (RAVG) program. The RAVG post-fire product delineates fire 
	perimeters that include burn areas within at least 1000 acres of National 
	Forest System (NFS) land (500 acres for Regions 8 and 9, as of 2016).
}
\usage{
anSAest_RAVG(RAVG, RAVG_dsn = NULL, RAVG.fire = NULL, RAVG.year = NULL, 
	RAVG.state = NULL, RAVG.ecoprov = NULL, RAVG.minacre = NULL, 
	datsource = "sqlite", SQLitefn, RS = NULL, largebnd.threshold = 10, 
	nbrdom.min = 10, rastlst.cont = NULL, rastlst.cont.name = NULL, 
	rastlst.cat = NULL, rastlst.cat.name = NULL, SApackage = "JoSAE", 
	SAmethod = "unit", pcfilter = NULL, landarea = "FOREST", 
	estvarlst, savedata = FALSE, saveobj = TRUE, showsteps = FALSE, 
	outfolder = NULL, multest_outfolder = NULL, multest_fmt = "sqlite", 
	multest_dsn="RAVG_SAmultest", multest.append = FALSE, 
	overwrite_dsn = FALSE, overwrite_layer = TRUE, append_layer = FALSE, 
	barplot.compare = FALSE, title.ref = NULL, save4testing = FALSE, 
	SAdomdat = NULL, SAdata = NULL, SApopdat = NULL)
}
\arguments{
  \item{RAVG}{ String. RAVG layer. } 
  \item{RAVG_dsn}{ String. RAVG data source name. } 
  \item{RAVG.fire}{ String. Fire name(s) to estimate. } 
  \item{RAVG.year}{ String. Fire year(s) to filter estimate and filter smallbnd. } 
  \item{RAVG.state}{ String. States to apply a spatial filter to smallbnd. } 
  \item{RAVG.ecoprov}{ String. Ecomap ecoregion province to apply a spatial 
		filter to smallbnd. } 
  \item{RAVG.minacre}{ Number. Minimum acre size for fires. } 
  \item{datsource}{ String. Name of datasource ('sqlite', 'datamart'). }
  \item{SQLitefn}{ String. Full pathname to SQLite database with plot data. }
  \item{RS}{ String. Name of FIA research station to restrict states to. } 
  \item{largebnd.threshold}{ Integer. Percent threshold to use for selecting 
	largebnds. } 
  \item{nbrdom.min}{ Integer. Set number for minimum domains for modeling. If NULL,
	all domains within largebnd are selected. }
  \item{rastlst.cont}{ String vector or list. Continuous rasters (file names 
	or raster objects). }
  \item{rastlst.cont.name}{ String vector. Output names for continuous rasters. 
	Optional. If NULL, name of raster is default or name+'_'+layer number 
	for multi-band layers. }
  \item{rastlst.cat}{ String vector or list. Categorical rasters (file names 
	or raster objects). }
  \item{rastlst.cat.name}{ String vector. Output names for categorical rasters. }
  \item{SApackage}{ String. small area package to use ('JoSAE', 'sae') }
  \item{SAmethod}{ String. small area method to use ('unit', 'area') }
  \item{pcfilter}{ String. A filter for plot or cond attributes (including pltassgn). 
	Must be R logical syntax. }
  \item{landarea}{ String. The sample area filter for estimates ('ALL', 'FOREST', 'TIMBERLAND'). 
	If landarea=FOREST, filtered to COND_STATUS_CD = 1; If landarea=TIMBERLAND, filtered 
	to SITECLCD in(1:6) and RESERVCD = 0. }
  \item{estvarlst}{ String vector. Name(s) of estimation variable 
	(e.g., c("VOLCFNET", "BA"). }
  \item{savedata}{ Logical. If TRUE, saves data to outfolder. }
  \item{saveobj}{ Logical. If TRUE, saves object to outfolder. }
  \item{showsteps}{ Logical. If TRUE, shows steps for creating SAdoms. }
  \item{outfolder}{ String. Name of output folder. If NULL, the working directory is used. }
  \item{multest_outfolder}{ String. If multest=TRUE, name of output folder for 
	output with multiple estimates. If NULL, multest_outfolder=outfolder. }
  \item{multest_fmt}{ String. Format for multest output tables ('csv', 'sqlite', 'gpkg').}
  \item{multest_dsn}{ String. Name of dsn to output multest table. }
  \item{multest.append}{ Logical. If TRUE, appends dunit.multest rawdata to out_dsn. }
  \item{overwrite_dsn}{ Logical. If TRUE, overwrites the out_dsn, if exists. }
  \item{overwrite_layer}{ Logical. If TRUE, overwrites the out_layer, if exists. }
  \item{append_layer}{ Logical. If TRUE, append layer to output. }
  \item{barplot.compare}{ Logical. If TRUE, creates barplot to compare estimate. }
  \item{title.ref}{ String. Reference information to use in title. }
  \item{save4testing}{ Logical. If TRUE, saves intermediate files for testing. }
  \item{SAdomdat}{ R list object. SA domain data. }
  \item{SAdata}{ R list object. FIA plot data and model data within SA domain. }
  \item{SApopdat}{ R list object. FIA population data within SA domain. }
}
 
\value{
  Estimate.
}
\author{
	Tracey S. Frescino
}
\keyword{ data }

