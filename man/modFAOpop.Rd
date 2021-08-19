\name{modFAOpop}
\alias{modFAOpop}
\title{
	FAO module - Generate population data for FAO module.
}
\description{
	Generates population data for generating FAO estimates.

}
\usage{
modFAOpop(FAOdata = NULL, base = NULL, cluster = NULL, tree = NULL, seed = NULL,
	clustassgn = NULL, dsn = NULL, clustid = "CN", clustassgnid = "PLT_CN",
	clustjoinid = "CN", buniqueid = "PLT_CN", baseid = "CONDID", tuniqueid = NULL,
	basewt = "CONDPROP_UNADJ", invyrs = NULL, adj = "none", diavar = "DIA",
	MICRO_BREAKPOINT_DIA= 5, MACRO_BREAKPOINT_DIA = NULL,
	areawt_micr = "MICRPROP_UNADJ", areawt_subp = "SUBPPROP_UNADJ",
	areawt_macr = "MACRPROP_UNADJ", strata = TRUE, nonsamp.clustfilter = NULL,
	nonsamp.basefilter = NULL, unitlevel1 = NULL, unitlevel2 = NULL,
	unitarea = NULL, areavar = "ACRES", areaunits = "acres", minplotnum.unit = 10,
	unit.action = "keep", stratalut = NULL, strvar = "STRATUMCD",
	strwtvar="strwt", getwt = TRUE, getwtvar = "P1POINTCNT", stratcombine = TRUE,
	minplotnum.strat = 2, saveobj = FALSE, objnm = "FAOpopdat",
	savedata = FALSE, outfolder = NULL, out_fmt = "csv", out_dsn = NULL,
	outfn.pre = NULL, outfn.date = FALSE, overwrite_dsn = FALSE,
	overwrite_layer = TRUE, append_layer = FALSE, gui = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FAOdata}{ R List object. Output data list components from FAOdata. }
  \item{base}{ DF/DT, comma-separated values (CSV) file (*.csv), or layer in dsn.
	The base-level data (e.g., COND, SUBP). Cluster variables and strata/estimation unit
	variable(s) may be included if cluster and clustassgn=NULL. See details for necessary
	variables to include. }
  \item{cluster}{ DF/DT, comma-separated values (CSV) file(*.csv), or layer in dsn,
	Can also be a shapefile(*.shp) with one record per cluster, a spatial layer in dsn,
	or a sf R object. Cluster-level data. Optional. }
  \item{tree}{ DF/DT, comma-delimited file(*.csv), or layer in dsn. If esttype="TREE",
	tree-level variables to aggregate to condition-level. See details for necessary
	variables to include. }
  \item{seed}{ DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
	Seedling data with one record for each seedling count. }
  \item{clustassgn}{ DF/DT, comma-separated values (CSV) file(*.csv), or layer in dsn,
	Can also be a shapefile(*.shp) with one record per cluster, a spatial layer in dsn,
	or a sf R object. Cluster-level assignment of estimation unit and/or strata. Optional. }
  \item{dsn}{ String. Name of database where tree, cond, and pltassgn tables reside.
	The dsn varies by driver. See gdal OGR vector formats
	(https://www.gdal.org/ogr_formats.html). }
  \item{clustid}{ String. Unique identifier of cluster. }
  \item{clustassgnid}{ String. Unique identifier of cluster. }
  \item{clustjoinid}{ String. Join variable in cluster to match clustassgnid. Does not need to
	be uniqueid. For example, if using most current XY coordinates for plot assignments. }
  \item{buniqueid}{ String. Unique identifier of cluster. }
  \item{baseid}{ String. Unique identifier of base unit records (e.g., CONDID, SUBPID). }
  \item{tuniqueid}{ String. Unique identifier of cluster. }
  \item{basewt}{ String. The variable in base to calculate area weights (e.g, CONDPROP_UNADJ).
    	If NULL, the basewt is assumed to be equal to 1. }
  \item{invyrs}{ Integer vector. Inventory year(s) (e.g., c(2000, 2001, 2002)). }
  \item{adj}{ String. How to calculate adjustment factors for nonsampled
	(nonresponse) conditions based on summed proportions for by plot ('none', 'samp', 'plot').
	'samp' - adjustments are calculated at strata/estimation unit level;
	'plot' - adjustments are calculated at plot-level. Adjustments
	are only calculated for annual inventory plots (designcd=1). }
  \item{diavar}{ String. Name of variable with tree diameters (e.g., DIA). Used for to
	define different plot sizes (SUPB, MICR, MACR). }
  \item{MICRO_BREAKPOINT_DIA}{ Number. If a different plot size for measuring seedlings/saplings,
	identify diameter breakpoint to include trees less than (e.g., 5). }
  \item{MACRO_BREAKPOINT_DIA}{ Number. If a different plot size for measuring large trees,
	identify diameter breakpoint to include trees greater than or equal to. }
  \item{areawt_micr}{ String. Name of variable to identify area for microplot (i.e., seedling/saplings).
	If areawt_micr is NULL or not in dataset, areawt is used. }
  \item{areawt_subp}{ String. Name of variable to identify area for subplot.
	If areawt_subp is NULL or not in dataset, areawt is used. }
  \item{areawt_macr}{ String. Name of variable to identify area for macroplot.
	If areawt_macr is NULL or not in dataset, areawt is used. }
  \item{strata}{ Logical. If TRUE, add data information for stratification. }
  \item{nonsamp.clustfilter}{ String. A logical expression for filtering nonsampled clusters.
	Must be R syntax (e.g., 'PLOT_STATUS_CD != 3'. }
  \item{nonsamp.basefilter}{ String. A logical expression for filtering nonsampled conditions.
	Must be R syntax (e.g., 'COND_STATUS_CD != 5'. }
  \item{unitlevel1}{ String. Name of the estimation unit variable in base or clustassgn
	with estimation unit assignment for each plot (e.g., 'ESTN_UNIT').
	If one estimation unit, set unitvar=NULL. }
  \item{unitlevel2}{ String. Name of a second estimation unit variable in base or
	clustassgn with assignment for each plot (e.g., 'STATECD'). }
  \item{unitarea}{ Numeric or DF. Total area by estimation unit. If only 1 estimation
	unit, include number of total acreage for the area of interest or a data frame
	with areavar. If more than one estimation unit, provide a data frame of total
	area by estimation unit, including unitvar and areavar. }
  \item{areavar}{ String. Name of acre variable in unitarea. Default="ACRES". }
  \item{areaunits}{ String. Units of areavar in unitarea ('acres', 'hectares'). }
  \item{minplotnum.unit}{ Integer. Minimum number of plots for estimation unit. }
  \item{unit.action}{ String. What to do if number of plots in an estimation unit
	is less than minplotnum.unit ('keep', 'remove' 'combine'). If unit.action='combine',
	combines estimation unit to the following estimation unit in unitlut. }
  \item{stratalut}{ DF/DT. If strata=TRUE, look-up table with strata
	proportions ('strwt') by strata (and estimation unit). To calculate
	'strwt', set getwt=TRUE and getwtvar= name of variable with information
	to calculate weights from (e.g., pixel counts) }.
  \item{strvar}{ String. If strata=TRUE, name of the strata variable in stratalut and
	cond or pltassgn data frame with stratum assignment for each plot
	(Default = 'STRATUMCD'). }
  \item{getwt}{ Logical. If TRUE, calculates strata weights from stratatlut getwtvar.
	If FALSE, strwtvar variable must be in stratalut. }
  \item{getwtvar}{ String. If getwt=TRUE, name of variable in stratalut to calculate
	weights (Default = 'P1POINTCNT'). }
  \item{strwtvar}{ String. If getwt=FALSE, name of variable in stratalut with calculated
	weights (Default = 'strwt'). }
  \item{stratcombine}{ Logical. If TRUE, automatically combines estimation units if less
	than 2 plots in any one estimation unit. See notes for more info. }
  \item{minplotnum.strat}{ Integer. Minimum number of plots for a stratum within an
	estimation unit. }
  \item{saveobj}{ Logical. If TRUE, saves SApopdat object to outfolder. }
  \item{objnm}{ String. Name of *.rda object. }
  \item{savedata}{ Logical. If TRUE, saves table(s) to outfolder. }
  \item{outfolder}{ String. The outfolder to write files to. If NULL, files are
	written to working directory, or if gui, a window to browse. }
  \item{out_fmt}{ String. Format for output tables ('csv', 'sqlite', 'gpkg').}
  \item{out_dsn}{ String. Name of database if out_fmt = c('sqlite', 'gpkg'). }
  \item{outfn.pre}{ String. Add a prefix to output name (e.g., "01"). }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile (e.g., outfn_'date'.csv). }
  \item{overwrite_dsn}{ Logical. If TRUE, overwrites the out_dsn, if exists. }
  \item{overwrite_layer}{ Logical. If TRUE, overwrites the out_layer, if exists. }
  \item{append_layer}{ Logical. If TRUE, appends layers to existing out_dsn
	or files if out_fmt = 'csv'. Note: currently cannot append layers if
	out_fmt = "gdb". }
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
  A list with population data for Green-Book estimates.

  \item{condx}{ Data frame. Condition-level data including plot-level assignment of
	estimation unit and stratum (if strata=TRUE), condition proportion adjustment
	factor (cadjfac), and adjusted condition proportions (CONDPROP_ADJ). }
  \item{cuniqueid}{ String. Unique identifier of plot in condx and pltcondx. }
  \item{condid}{ String. Unique identifier of condition in condx and pltcondx. }
  \item{treex}{ Data frame. Tree data within population, used for estimation,
	including trees per acre adjustment factor (tadjfac), and adjusted
	trees per acre (TPA_ADJ) (if treef is included). }
  \item{tuniqueid}{ String. Unique identifier of plot in treex (if treef is included). }
  \item{ACI.filter}{ String. If ACI=FALSE, ACI.filter="COND_STATUS_CD == 1" . }
  \item{unitarea}{ String. Returned table of area by estimation unit. }
  \item{unitvar}{ String. Variable name for estimation unit. }
  \item{strlut}{ String. Strata-level table with pixel counts by strata (P1POINTCNT),
	strata weights (strwt), number of plots by strata (n.strata), total number of
	plots in estimation unit (n.total), sum of condition proportions (*_UNADJ_SUM),
	area adjustments (*_ADJFAC), total area, and area expansion by strata (EXPNS). }
  \item{strvar}{ String. Variable name for strata. If strata=FALSE, strvar="ONESTRAT". }
  \item{expcondtab}{ String. If ACI=FALSE, ACI.filter="COND_STATUS_CD == 1" . }
  \item{plotsampcnt}{ Data frame. Number of plots by PLOT_STATUS_CD. }
  \item{condsampcnt}{ Data frame. Number of conditions by COND_STATUS_CD. }
  \item{states}{ String. State names in dataset. }
  \item{invyrs}{ String. Range of inventory years in dataset. }


  \item{stratdat}{ Data frame. Strata information by estimation unit. }
	\tabular{lll}{
		\tab \bold{Variable}	\tab \bold{Description}\cr
	 	\tab unitvar		\tab estimation unit \cr
	 	\tab strvar		\tab stratum value \cr
	 	\tab strwtvar	\tab number of pixels by strata and estimation unit \cr
	 	\tab n.strata	\tab number of plots in strata (after totally nonsampled
			plots removed) \cr
	 	\tab n.total	\tab number of plots for estimation unit \cr
	 	\tab strwt		\tab proportion of area (or plots) by strata and
			estimation unit (i.e., strata weight) \cr
	 	\tab CONDPROP_UNADJ_SUM	\tab summed condition proportion by strata and
			estimation unit \cr
	 	\tab CONDPROP_ADJFAC	\tab adjusted condition proportion by strata after
			nonsampled plots removed \cr
	 	\tab AREA_USED	\tab total area of estimation unit \cr
	 	\tab expfac		\tab strata-level expansion factor after nonsampled plots
			and conditions removed (AREA_USED/n.strata) \cr
	 	\tab EXPNS		\tab strata-level area expansions (expfac * strwt)\cr
	}

  Table(s) are also written to outfolder.
}
\author{
	Tracey S. Frescino, Paul L. Patterson, Elizabeth A. Freeman
}
\references{
	Scott, Charles T.; Bechtold, William A.; Reams, Gregory A.; Smith, William D.; Westfall, James A.;
	Hansen, Mark H.; Moisen, Gretchen G. 2005. Sample-based estimators used by the Forest Inventory and
	Analysis national information management system. Gen. Tech. Rep. SRS-80. Asheville, NC: U.S. Department
	of Agriculture, Forest Service, Southern Research Station, p.53-77.
}

\note{

	ADJUSTMENT FACTOR:\cr
	The adjustment factor is necessary to account for nonsampled conditions. It is calculated
		for each estimation unit by strata. by summing the unadjusted proportions of the
		subplot, microplot, and macroplot (i.e. *PROP_UNADJ) and dividing by the number
		of plots in the strata/estimation unit).

	An adjustment factor is determined for each tree based on the size of the plot it was
	measured on. This is identified using TPA_UNADJ as follows:

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


	unitcombine:\cr
	If TRUE and less than 2 plots in any one estimation unit, all estimation units with
	10 or less plots are combined. The current method for combining is to group the
	estimation unit with less than 10 plots with the estimation unit following in
	consecutive order (numeric or alphabetical), restrained by survey unit (UNITCD) if
	included in dataset, and continuing until the number of plots equals 10. If there are
	no estimation units following in order, it is combined with the estimation unit previous
	in order.

	stratcombine:\cr
	If TRUE and less than 2 plots in any one strata class within an esimation unit,
	all strata classes with 2 or less plots are combined. The current method for combining
	is to group the strata with less than 2 plots with the strata class following in
	consecutive order (numeric or alphabetical), restrained by estimation unit
	(if unitcombine=FALSE), and continuing until the number of plots equals 10. If there
	are no strata classes following in order, it is combined with the estimation unit
	previous in order.

}

\keyword{ data }
