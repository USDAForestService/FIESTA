\name{modSApop}
\alias{modSApop}
\title{
	Small-Area module - Compile population data for SA module.
}
\description{
	Compile population data for input to the modSA* modules. }

\usage{    
modSApop(SAdoms = NULL, cond = NULL, tree = NULL, seed = NULL, plt = NULL, 
	pltassgn = NULL, dsn = NULL, tuniqueid="PLT_CN", cuniqueid = "PLT_CN", 
	condid = "CONDID", puniqueid = "CN", pltassgnid = "CN", pjoinid = "CN", 
	measCur = FALSE, measEndyr = NULL, measEndyr.filter = NULL, invyrs = NULL, 
	ACI = FALSE, adj = "plot", plt.nonsamp.filter = NULL, cond.nonsamp.filter = NULL, 
	dunitvar = "DOMAIN", dunitvar2 = NULL, dunitarea = NULL, areavar = "ACRES", 
	unitcombine = FALSE, dunitlut = NULL, prednames = NULL, predfac = NULL, 
	pvars2keep = NULL, cvars2keep = NULL, saveobj = FALSE, savedata = FALSE, 
	outfolder = NULL, out_fmt = "csv", out_dsn = NULL, outfn = NULL, 
	outfn.pre = NULL, outfn.date = FALSE, overwrite_dsn = FALSE, 
	overwrite_layer = TRUE, SAdata = NULL, gui = FALSE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SAdoms}{sf object. SA domains with attributes for joining. }
  \item{cond}{ DF/DT, comma-separated values (CSV) file (*.csv), or layer in dsn. 
	The condition-level variables with one record per condition, including or 
	excluding nonsampled conditions. Plot variables and strata/estimation unit 
	variable(s) may be included if plt and pltassgn=NULL. See details for necessary 
	variables to include. }
  \item{tree}{ DF/DT, comma-delimited file(*.csv), or layer in dsn. If esttype="TREE",
	tree-level variables to aggregate to condition-level. See details for necessary 
	variables to include. }
  \item{seed}{ DF/DT, R object, comma-delimited file(*.csv), or layer in dsn. 
	Seedling data with one record for each seedling count. }
  \item{plt}{ DF/DT, comma-separated values (CSV) file(*.csv), or layer in dsn,
	Can also be a shapefile(*.shp) with one record per plot, a spatial layer in dsn,
	or a sf R object. Plot-level variables. If nonsampled plots are included, 
	PLOT_STATUS_CD variable must be in table. Optional. }
  \item{pltassgn}{ DF/DT, comma-separated values (CSV) file(*.csv), or layer in dsn,
	Can also be a shapefile(*.shp) with one record per plot, a spatial layer in dsn,
	or a sf R object. Plot-level assignment of estimation unit and/or strata. Optional. }
  \item{dsn}{ String. Name of database where tree, cond, and pltassgn tables reside. 
	The dsn varies by driver. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html). }
  \item{cuniqueid}{ String. Unique identifier of plot in cond. }
  \item{condid}{ String. Unique identifier of plot conditions. If no condid in 
	cond, assumed to have 1 condition per plot. A CONDID=1 is automatically added. }
  \item{puniqueid}{ String. Unique identifier of plot. }
  \item{tuniqueid}{ String. Unique identifier of plot in tree and seed. }
  \item{pltassgnid}{ String. Unique identifier of plot in pltassgn. }
  \item{pjoinid}{ String. Join variable in plot to match pltassgnid. Does not need to
	be uniqueid. }
  \item{measCur}{ Logical. If TRUE, extract plots with most current measurement for 
	state(s). }
  \item{measEndyr}{ Logical. If TRUE, extract plots with most current measurement for 
	state(s) for years measured before measEndyr. }
  \item{measEndyr.filter}{ Filter for extracting plots using measEndyr. Must be in
	R syntax (e.g., 'AOI == 1'). }
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
  \item{dunitvar2}{ String. Name of a second domain unit variable in cond, plt, or 
	pltassgn with assignment for each plot (e.g., 'STATECD'). }
  \item{dunitarea}{ Numeric or DF. Total area by domain unit. }
  \item{areavar}{ String. Name of acre variable in unitarea. Default="ACRES". }
  \item{unitcombine}{ Logical. If TRUE, automatically combines domain units 
	if less than 2 plots in any one domain unit. }
  \item{dunitlut}{ DF/DT. Data frame with auxiliary information by domain 
	unit. For continuous data, means by domain unit; for categorical data, 
	proportion of class by domain unit. }
  \item{prednames}{ String vector. Name(s) of predictor variables to use in model. }
  \item{predfac}{ String vector. Name(s) of factor predictor variables to use in model. }
  \item{pvars2keep}{ String vector. Additional plot variables to keep in dataset. }
  \item{cvars2keep}{ String vector. Additional condition variables to keep in dataset. }
  \item{saveobj}{ Logical. If TRUE, save SApopdat object to outfolder. }
  \item{savedata}{ Logical. If TRUE, saves table(s) to outfolder. }
  \item{outfolder}{ String. The outfolder to write files to. If NULL, files are 
	written to working directory, or if gui, a window to browse. }
  \item{out_fmt}{ String. Format for output tables ('csv', 'sqlite', 'gpkg').}
  \item{out_dsn}{ String. Name of database if out_fmt = c('sqlite', 'gpkg'). }
  \item{outfn}{ String. Name of the output file(s) if savedata=TRUE. Do not include
	extension. Default file name includes parameter inputs 
	(e.g., tree_'estvar.name'_'rowvar'_'date') }
  \item{outfn.pre}{ String. Add a prefix to output name (e.g., "01"). }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile (e.g., outfn_'date'.csv). }
  \item{overwrite_dsn}{ Logical. If TRUE, overwrites the out_dsn, if exists. }
  \item{overwrite_layer}{ Logical. If TRUE, overwrites the out_layer, if exists. }
  \item{SAdata}{ R List object. Output data list components from FIESTA::SAdata(). }
  \item{gui}{ Logical. If gui, user is prompted for parameters. }
}
\details{
	If variables are NULL, then it will prompt user to input variables.

     	Necessary variables:\cr
	\tabular{llll}{
		\tab \bold{Data} \tab \bold{Variable}	\tab \bold{Description}\cr
	 	\tab tree \tab tuniqueid	\tab Unique identifier for each plot, to link to pltassgn 
								(ex. PLT_CN).\cr
		\tab  \tab CONDID		\tab Unique identifier of each condition on plot, to link to cond. 
								Set CONDID=1, if only 1 condition per plot.\cr
		\tab  \tab TPA_UNADJ		\tab Number of trees per acre each sample tree represents
								(ex. DESIGNCD=1: TPA_UNADJ=6.018046 for trees on subplot;
								74.965282 for trees on microplot).\cr
	 	\tab cond \tab cuniqueid	\tab Unique identifier for each plot, to link to pltassgn 
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
	 	\tab pltassgn \tab puniqueid	\tab Unique identifier for each plot, to link to cond 
								(ex. CN).\cr
	 	\tab  \tab STATECD		\tab Identifies state each plot is located in.\cr
	 	\tab  \tab INVYR		\tab Identifies inventory year of each plot.\cr
	 	\tab  \tab PLOT_STATUS_CD	\tab Status of each plot (i.e. sampled, nonsampled). 
								If not included, all plots are assumed as sampled.\cr							
	}

	For available reference tables: sort(unique(FIESTA::ref_codes$VARIABLE)) \cr

}
\value{
  A list with population data for Small-Area estimates. 

  \item{SAdomsdf}{ Data frame. Attribute table from SAdoms spatial layer. 
	Includes DOMAIN and AOI attributes. DOMAIN represents modeling domains. 
	AOI identifies the small area of interest. }
  \item{condx}{ Data frame. Condition-level data with condition proportions,
	domain and predictor assignments, and adjusted condition proportions,
	if adj in('samp', 'plot').  
  \item{pltcondx}{ Data frame. Plot/Condition data used for estimation. }
  \item{cuniqueid}{ String. Unique identifier of plot in condx and pltcondx. }
  \item{condid}{ String. Unique identifier of condition in condx and pltcondx. }
  \item{treex}{ Data frame. If esttype='TREE', tree-level data, including  
	adjustment factors, if adj in('samp', 'plot'). }
  \item{tuniqueid}{ String. If esttype='TREE', unique identifier of plot in treex. }
  \item{ACI.filter}{ String. If ACI=FALSE, ACI.filter="COND_STATUS_CD == 1" . }
  \item{dunitarea}{ Data frame. Area by model domain unit. }
  \item{areavar}{ String. Name of area variable in dunitarea. }
  \item{dunitvar}{ String. Name of variable defining model domain units in dunitarea. }
  \item{dunitlut}{ Data frame. Table of model domain units with zonal statistics 
	of predictor values, number of plots by domain unit. }
  \item{prednames}{ String vector. Name of variables in dunitlut and condx 
	defining potential predictors for small area estimation. }
  \item{plotsampcnt}{ Data frame. Number of plots by PLOT_STATUS_CD. }
  \item{condsampcnt}{ Data frame. Number of conditions by COND_STATUS_CD. }
  \item{states}{ String. State names in dataset. }
  \item{invyrs}{ String. Range of inventory years in dataset. }
  \item{adjtree}{ Logical. If TRUE, treex includes adjustment factors. }
}
\author{
	Tracey S. Frescino, Paul L. Patterson, Elizabeth A. Freeman
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
}

\keyword{ data }
