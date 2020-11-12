\name{modSAtree}
\alias{modSAtree}
\title{
	Small area module - Generate small area tree estimates.
}
\description{
	Generates small area estimates by domain and/or tree domain (and estimation unit). 	 
}
\usage{    
modSAtree(SAdomsdf = NULL, tree = NULL, cond = NULL, plt = NULL, pltassgn = NULL, 
	seed = NULL, dsn = NULL, tuniqueid = "PLT_CN", cuniqueid = "PLT_CN", 
	condid = "CONDID", puniqueid = "CN", pltassgnid = "CN", measCur = FALSE, 
	measEndyr = NULL, invyrs = NULL, ACI=FALSE, adj = "plot", plt.nonsamp.filter = NULL, 
	cond.nonsamp.filter = NULL, dunitvar = "DOMAIN", dunitvar2 = NULL, 
	dunitarea = NULL, areavar = NULL, dunitlut = NULL, prednames = NULL, 
	predfac = NULL, SApackage = "JoSAE", SAmethod = "unit", largebnd.att = NULL, 
	landarea = "ALL", plt.filter = NULL, cond.filter = NULL, estvar = NULL, 
	estvar.filter = NULL, smallbnd.att = NULL, allin1 = FALSE, estround = 0, 
	pseround = 3, estnull = 0, psenull = "--", divideby = NULL, savedata = FALSE, 
	rawdata = FALSE, multest = TRUE, addSAdomsdf = TRUE, SAdomvars = NULL, 
	outfolder = NULL, outfn.pre = NULL, raw_fmt = "csv", raw_dsn = "rawdata", 
	multest_fmt = "csv", multest_outfolder = NULL, multest_dsn = NULL, multest_layer = NULL, 
	multest.append = FALSE, multest.AOIonly = FALSE, outfn.date = FALSE, overwrite = FALSE, 
	addtitle = TRUE, returntitle = FALSE, title.main = NULL, title.ref = NULL, 
	title.dunitvar = NULL, title.estvar = NULL, title.filter = NULL, 
	SApopdat = NULL, SAdata = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SAdomsdf}{ DF/DT or comma-delimited file(*.csv). Dataframe from SAdoms
	with attributes from smallbnd to add to estimation table. }
  \item{tree}{ DF/DT or comma-delimited file(*.csv). The tree table with tree data to 
	aggregate to condition-level. See details for necessary variables to include. }
  \item{cond}{ DF/DT or comma-delimited file (*.csv). The condition-level table with one 
	record per condtions and including nonsampled conditions. 
	Plot variables and strata/estimation unit variable(s) may be included for 
	post-stratification. See details for necessary variables to include. }
  \item{plt}{ DF/DT, comma-separated values (CSV) file(*.csv), or layer in dsn,
	Can also be a shapefile(*.shp) with one record per plot, a spatial layer in dsn,
	or a sf R object. Plot-level variables. If nonsampled plots are included, 
	PLOT_STATUS_CD variable must be in table. Optional. }
  \item{pltassgn}{ DF/DT, comma-delimited file(*.csv), SpatialDataFrame, or 
	shapefile(*.shp) with one record per plot, estimation unit variable(s) and 
	mean (continuous rasters) or proportions (categorical rasters) by 
	estimation unit. Optional. }
  \item{seed}{ DF/DT or comma-delimited file (*.csv). Seedling table. }
  \item{dsn}{ Sting. Data Source name of database with tree, cond, pltassgn. }
  \item{tuniqueid}{ String. Unique identifier of plot in tree and seed. }
  \item{cuniqueid}{ String. Unique identifier of plot in cond. }
  \item{condid}{ String. Unique identifier of plot conditions. If no condid in 
	cond, assumed to have 1 condition per plot. A CONDID=1 is automatically added. }
  \item{puniqueid}{ String. Unique identifier of plot. }
  \item{pltassgnid}{ String. Unique identifier of plot. }
  \item{measCur}{ Logical. If TRUE, extract plots with most current measurement for 
	state(s). }
  \item{measEndyr}{ Logical. If TRUE, extract plots with most current measurement for 
	state(s) for years measured before measEndyr. }
  \item{invyrs}{ Integer vector. Inventory year(s) (e.g., c(2000, 2001, 2002)). }
  \item{ACI}{ Logical. If TRUE, including All Condition Inventory (ACI) plots. Removes 
	nonsampled nonforest lands (NF_COND_STATUS_CD = 5). Tree data must be included. }
  \item{adj}{ String. How to calculate adjustment factors for nonsampled 
	(nonresponse) conditions based on summed proportions for by plot ('samp', 'plot').
	'samp' - adjustments are calculated at strata/estimation unit (i.e., domain unit) level; 
	'plot' - adjustments are calculated at plot-level. Adjustments 
	are only calculated for annual inventory plots (designcd=1). } 
  \item{plt.nonsamp.filter}{ String. A logical expression for filtering nonsampled plots. 
	Must be R syntax. If plt.nonsamp.filter=NULL, a default is set to: 'PLOT_STATUS_CD != 3'. } 
  \item{cond.nonsamp.filter}{ String. A logical expression for filtering nonsampled conditions. 
	Must be R syntax. If cond.nonsamp.filter=NULL, a default is set to: 'COND_STATUS_CD != 5'; 
	if ACI: 'is.na(NF_COND_STATUS_CD) | NF_COND_STATUS_CD != 5'. }
  \item{dunitvar}{ String. Name of the domain unit variable in cond, plt, or pltassgn 
	with domain unit assignment for each plot. }
  \item{dunitvar2}{ String. Name of a second domain unit variable in cond or 
	pltassgn with assignment for each plot (e.g., 'STATECD'). }
  \item{dunitarea}{ Numeric or DF. Total area by domain unit. }
  \item{areavar}{ String. Name of acre variable in unitacres. Default="ACRES". }
  \item{dunitlut}{ DF/DT. Data frame with auxiliary information by domain unit. For 
	continuous data, means by domain unit; for categorical data, proportion of 
	class by domain unit. }
  \item{prednames}{ String vector. Name(s) of predictor variables to use in model. }
  \item{predfac}{ String vector. Name(s) of factor predictor variables to use in model. }
  \item{SApackage}{ String. small area package to use ('JoSAE', 'sae') }
  \item{SAmethod}{ String. small area method to use ('unit', 'area') }
  \item{largebnd.att}{ String. Name of the large boundary attribute to define plots
	within a model extent. If NULL, all plots are used for model extent. }
  \item{landarea}{ String. The sample area filter for estimates ('ALL', 'FOREST', 'TIMBERLAND'). 
	If landarea=FOREST, filtered to COND_STATUS_CD = 1; If landarea=TIMBERLAND, filtered 
	to SITECLCD in(1:6) and RESERVCD = 0. }
  \item{plt.filter}{ String. A filter for plt. Must be R syntax. }
  \item{cond.filter}{ String. A filter for cond. Must be R syntax. }
  \item{estvar}{ String. Name of the tree estimate variable. }
  \item{estvar.filter}{ String. A tree filter for estimate variable. Must be R syntax 
	(e.g., "STATUSCD == 1"). }
  \item{smallbnd.att}{ String. Attribute in smallbnd, alternative to DOMAIN, to output
	in estimation table. }
  \item{allin1}{ Logical. If TRUE, both estimates and percent sample error are output
	in one table as: estimates (percent sample error). }
  \item{estround}{ Integer. Number of decimal places for estimates. }
  \item{pseround}{ Integer. Number of decimal places for percent sampling error. }
  \item{estnull}{ Number or character. The number or symbol to use to indicate 'not sampled'
	for estimate. }
  \item{psenull}{ Number or character. The number or symbol to use to indicate 'not sampled'
	for percent standard errror. }
  \item{divideby}{ String. Conversion number for output ('hundred', 'thousand', 'million'). }
  \item{savedata}{ Logical. If TRUE, saves table(s) to outfolder. }
  \item{rawdata}{ Logical. If TRUE, returns a list of raw data tables that are used
	for estimation (See Value). If savedata = TRUE, also written to outfolder. }
  \item{multest}{ Logical. If TRUE, returns a data frame of SA estimates using both
	unit-level and area-level estimates. }
  \item{addSAdomsdf}{ Logical. If TRUE, sppends SAdomdf to unit.multest table for output. }
  \item{SAdomvars}{ String vector. List of attributes from SAdoms to include in multest output. }
  \item{outfolder}{ String. The outfolder to write files to. If NULL, files are 
	written to working directory, or if gui, a window to browse. }
  \item{outfn.pre}{ String. A prefix for outfile name, if savedata=TRUE. }
  \item{raw_fmt}{ String. Format for raw output tables ('csv', 'sqlite', 'gpkg').}
  \item{raw_dsn}{ String. Name of database if raw_fmt = c('sqlite', 'gpkg'). }
  \item{multest_fmt}{ String. Format for multest output tables ('csv', 'sqlite', 'gpkg').}
  \item{multest_outfolder}{ String. Outfolder for multest. If NULL, same as outfolder.}
  \item{multest_dsn}{ String. Name of database if multest_fmt = c('sqlite', 'gpkg'). }
  \item{multest_layer}{ String. Name of database layer if multest_fmt = c('sqlite', 'gpkg'). }
  \item{multest.append}{ Logical. If TRUE, appends multest dataframe to output. }
  \item{multest.AOIonly}{ Logical. If TRUE, appends multest dataframe (AOI=1) to output. }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile (e.g., outfn_'date'.csv). }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
  \item{addtitle}{ Logical. If TRUE and savedata=TRUE, adds title to outfile. }
  \item{returntitle}{ Logical. If TRUE, returns title(s) of the estimation table(s). }
  \item{title.main}{ String. TITLE, if savedata=TRUE and/or returntitle=TRUE: the complete 
	title used for table. If title.main=NULL, the title.* parameters are used to 
	generate title string. Note: if title.ref is not NULL, it is added to title.main. }
  \item{title.ref}{ String. TITLE, if savedata=TRUE and/or returntitle=TRUE: the ending 
	text of the table title (e.g., Nevada, 2004-2005). If NULL, = "". }
  \item{title.dunitvar}{ String. TITLE, if savedata=TRUE and/or returntitle=TRUE: pretty  
	name for the estimation unit variable. If NULL, = unitvar. }
  \item{title.estvar}{ String. TITLE: if savedata=TRUE and/or returntitle=TRUE: pretty 
	name for the estimate variable. If NULL, title.estvar = estvar.name. }
  \item{title.filter}{ String. TITLE, if savedata=TRUE and/or returntitle=TRUE: pretty 
	name for filter(s). If NULL, = "". }
  \item{SApopdat}{ List. Population data objects returned from modSApop(). }
  \item{SAdata}{ R List object. Output data list components from FIESTA::SAdata(). }
}
\details{
	If variables are NULL, then it will prompt user to input variables.

     	Necessary variables:\cr
	\tabular{llll}{
		\tab \bold{Data} \tab \bold{Variable}	\tab \bold{Description}\cr
	 	\tab tree \tab tuniqueid	\tab Unique identifier for each plot, to link to pltstrat 
								(ex. PLT_CN).\cr
		\tab  \tab CONDID		\tab Unique identifier of each condition on plot, to link to cond. 
								Set CONDID=1, if only 1 condition per plot.\cr
		\tab  \tab TPA_UNADJ		\tab Number of trees per acre each sample tree represents
								(ex. DESIGNCD=1: TPA_UNADJ=6.018046 for trees on subplot;
								74.965282 for trees on microplot).\cr
	 	\tab cond \tab cuniqueid	\tab Unique identifier for each plot, to link to pltstrat 
								(ex. PLT_CN).\cr
		\tab  \tab CONDID		\tab Unique identfier of each condition on plot. 
								Set CONDID=1, if only 1 condition per plot.\cr
		\tab  \tab CONDPROP_UNADJ	\tab Unadjusted proportion of condition on each plot. 
								Set CONDPROP_UNADJ=1, if only 1 condition per plot.\cr
		\tab  \tab COND_STATUS_CD	\tab Status of each forested condition on plot 
							(i.e. accessible forest, nonforest, water, etc.)\cr
		\tab  \tab NF_COND_STATUS_CD	\tab If ACI=TRUE. Status of each nonforest condition on plot 
							(i.e. accessible nonforest, nonsampled nonforest)\cr
  		\tab  \tab SITECLCD		\tab If landarea=TIMBERLAND. Measure of site productivity.\cr
  		\tab  \tab RESERVCD		\tab If landarea=TIMBERLAND. Reserved status.\cr
		\tab  \tab SUBPROP_UNADJ	\tab Unadjusted proportion of subplot conditions on each plot. 
								Set SUBPROP_UNADJ=1, if only 1 condition per subplot.\cr
		\tab  \tab MICRPROP_UNADJ	\tab If microplot tree attributes. Unadjusted proportion 
							of microplot conditions on each plot. Set MICRPROP_UNADJ=1, 
							if only 1 condition per microplot.\cr
		\tab  \tab MACRPROP_UNADJ	\tab If macroplot tree attributes. Unadjusted proportion 
							of macroplot conditions on each plot. Set MACRPROP_UNADJ=1, 
							if only 1 condition per macroplot.\cr
	 	\tab pltassign \tab puniqueid	\tab Unique identifier for each plot, to link to cond 
								(ex. CN).\cr
	 	\tab  \tab STATECD		\tab Identifies state each plot is located in.\cr
	 	\tab  \tab INVYR		\tab Identifies inventory year of each plot.\cr
	 	\tab  \tab PLOT_STATUS_CD	\tab Status of each plot (i.e. sampled, nonsampled). 
								If not included, all plots are assumed as sampled.\cr							
	}

	Reference names are available for the following variables: \cr
		ADFORCD, AGENTCD, CCLCD, DECAYCD, DSTRBCD, KINDCD, OWNCD, OWNGRPCD, FORTYPCD, 
		FLDTYPCD, FORTYPCDCALC, TYPGRPCD, FORINDCD, RESERVCD, LANDCLCD, STDSZCD, FLDSZCD, 
		PHYSCLCD, MIST_CL_CD, PLOT_STATUS_CD, STATECD, TREECLCD, TRTCD, SPCD, SPGRPCD 

}
\value{
  \item{est}{ Data frame. Tree estimates and percent sampling error by domain. 
	Estimates are based on the SApackage and SAmethod parameters defined. }
  \item{titlelst}{ List. List of titles used for table output. }
  \item{raw}{ List of raw data. If rawdata=TRUE, a list including raw data components
	used for calculating estimate. }
  \item{dunit.multest}{ Data frame. Table comparing different estimation strategies for SAE. }


  Raw data

  \item{domdat}{ Data frame. Domain-level data used for estimation. }
  \item{estvar}{ String. Name of estimation variable. }
  \item{estvar.filter}{ String. Logical filter specified for tree data. }
  \item{dunit.totest}{ String. Table of estimates, including more details. }
  
}
\author{
	Tracey S. Frescino, Paul L. Patterson, Elizabeth A. Freeman
}
\references{
	Breidenbach, J. 2018. JoSAE: Unit-Level and Area-Level Small Area Estimation. 
	R package version 0.3.0. https://CRAN.R-project.org/package=JoSAE.

	Molina I, Marhuenda Y. 2015. sae: An R Package for Small Area Estimation. 
	The R Journal 7(1), 81-98. https://journal.r-project.org/archive/2015/RJ-2015-007/RJ-2015-007.
}

\note{
 
	ADJUSTMENT FACTOR:\cr
	The adjustment factor is necessary to account for nonsampled conditions. 
	For model-based estimation, we calculate adjustment factors by plot. 

	It is calculated by dividing 1 / summed condition proportions by plot. An 
	adjustment factor is determined for each tree based on the size of the plot 
	it was measured on. This is identified using TPA_UNADJ as follows: 

	\tabular{llr}{
	\tab \bold{PLOT SIZE}	\tab \bold{TPA_UNADJ} \cr
	\tab SUBPLOT	\tab 6.018046 \cr
	\tab MICROPLOT	\tab 74.965282 \cr
	\tab MACROPLOT	\tab 0.999188 \cr }

	If ACI=FALSE, only nonsampled forest conditions are accounted for in the adjustment factor. \cr
	If ACI=TRUE, the nonsampled nonforest conditions are removed as well and accounted 
		for in adjustment factor.
	This is if you are interested in estimates for all lands or nonforest lands in the 
		All-Condition-Inventory.


	Common tree filters for estvar.filter: \cr

	\tabular{llr}{
	\tab \bold{FILTER}		\tab \bold{DESCRIPTION} \cr
 	\tab "STATUSCD == 1"		\tab Live trees \cr	
	\tab "STATUSCD == 2"		\tab Dead trees \cr	
	\tab "TPAMORT_UNADJ > 0"	\tab Mortality trees \cr
	\tab "STATUSCD == 2 & DIA >= 5.0"	\tab Dead trees >= 5.0 inches diameter \cr
	\tab "STATUSCD == 2 & AGENTCD == 30"	\tab Dead trees from fire \cr 
 	}
}

\keyword{ data }
