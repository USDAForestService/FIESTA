\name{anSAest_RAVG}
\alias{anSAest_RAVG}
\title{
	Analysis - Generate unit-level and area-leve estimates for fire perimeters.
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
	SAmethod = "unit", plt.filter = NULL, landarea = "FOREST", 
	cond.filter = NULL, estvarlst, savedata = FALSE, showsteps = FALSE, 
	outfolder = NULL, multest_dsn="RAVG_SAmultest", multest.append = FALSE, 
	overwrite = FALSE, barplot.compare = FALSE, title.ref = NULL, 
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
  \item{plt.filter}{ String A filter for plot-level data (e.g., "INVYR == 2012"). }
  \item{landarea}{ String. The sample area filter for estimates ('ALL', 'FOREST', 'TIMBERLAND'). 
	If landarea=FOREST, filtered to COND_STATUS_CD = 1; If landarea=TIMBERLAND, filtered 
	to SITECLCD in(1:6) and RESERVCD = 0. }
  \item{cond.filter}{ String. A filter for condition-level data (e.g., "FORTYPCD == 180"). 
	Must be R syntax. }
  \item{estvarlst}{ String vector. Name(s) of estimation variable 
	(e.g., c("VOLCFNET", "BA"). }
  \item{savedata}{ Logical. If TRUE, saves data to outfolder. }
  \item{showsteps}{ Logical. If TRUE, shows steps for creating SAdoms. }
  \item{outfolder}{ String. Name of output folder. If NULL, the working directory is used. }
  \item{multest_dsn}{ String. Name of dsn to output multest table. }
  \item{multest.append}{ Logical. If TRUE, appends dunit.multest rawdata to out_dsn. }
  \item{overwrite}{ Logical. If TRUE, overwrites output. }
  \item{barplot.compare}{ Logical. If TRUE, creates barplot to compare estimate. }
  \item{title.ref}{ String. Reference information to use in title. }
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

