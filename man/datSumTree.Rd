\name{datSumTree}
\alias{datSumTree}
\title{
	Data - Aggregates numeric tree data to the plot or condition-level.
}
\description{
	Aggregates numeric tree-level data (e.g., VOLCFNET) to plot or condition, 
	including options for filtering tree data or extrapolating to plot acre 
	by multiplying by TPA.
}
\usage{
datSumTree(tree = NULL, cond = NULL, plt = NULL, plt_dsn = NULL, tuniqueid = "PLT_CN", 
	cuniqueid = "PLT_CN", puniqueid = "CN", bycond = FALSE, condid = "CONDID", 
	tsumvarlst = NULL, tsumvarnmlst = NULL, TPA = TRUE, tfun = sum, 
	ACI = FALSE, tfilter = NULL, lbs2tons = TRUE, getadjplot = FALSE, 
	adjtree = FALSE, adjTPA = 1, savedata = FALSE, outfolder = NULL, outfn = NULL, 
	outfn.date = TRUE, overwrite = FALSE, tround = 16, checkNA = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{ Dataframe or comma-delimited file (*.csv). The tree-level table. }
  \item{cond}{ Dataframe or comma-delimited file (*.csv). Condition-level table to join the 
	aggregated tree data to, if bycond=TRUE. This table also may be used for condition 
	proportion or strata variables used if adjcond or adjstrata = TRUE (See details below). 
	This table is optional. }
  \item{plt}{ Dataframe, comma-delimited file (*.csv), or shapefile (*.shp). Plot-level 
	table to join the aggregated tree data to, if bycond=FALSE. This table is optional. }
  \item{plt_dsn}{ String. The data source name (dsn; i.e., folder or database name)
	of plt. The dsn varies by driver. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html). Optional. } 
  \item{tuniqueid}{ String. Unique identifier of the tree table. }
  \item{cuniqueid}{ String. Unique identifier of the cond table if cond is NOT NULL. }
  \item{puniqueid}{ String. Unique identifier of the plt table if plt is NOT NULL. }
  \item{bycond}{ Logical. If TRUE, the data are aggregated to the condition level 
	(by: uniqueid, condid). If FALSE, the data are aggregated to the plot level 
	(by: uniqueid). }
  \item{condid}{ String. Unique identifier for conditions. }
  \item{tsumvarlst}{ String (vector). Tree-level variable(s) to aggregate 
	(e.g., "TPA_UNADJ", "BA"). Use tsumvar="PLT_CN" (tfun=sum) for summed tree count. }
  \item{tsumvarnmlst}{ String (vector). Name of the tree-level variable(s) to aggregate 
	(e.g., "TPALIVE", "BALIVE"). This list must have the same number of variables as tsumvarlst 
	and be in respective order. If NULL, the default names will be tsumvar'_tfun' 
	(e.g., "TPA_UNADJ_SUM", "BA_SUM"). }
  \item{TPA}{ Logical. If TRUE, tsumvarlst variable(s) are multiplied by the respective 
	trees-per-acre variable (see details) to get per-acre measurements. }
  \item{tfun}{ Function. The name of the function to use to aggregate the data 
	(e.g., sum, mean, max). }
  \item{ACI}{ Logical. If TRUE, if ACI (All Condition Inventory) plots exist, any trees on 
	these plots will be included in summary. If FALSE, you must include condition table. }
  \item{tfilter}{ String. A filter to subset the tree data before aggregating 
	(e.g., "STATUSCD == 1"). This must be in R syntax. If tfilter=NULL, user is prompted. 
	Use tfilter="NONE" if no filters. }
  \item{lbs2tons}{ Logical. If TRUE, converts biomass or carbon variables from pounds to tons. }
  \item{getadjplot}{ Logical. If TRUE, adjustments are calculated for nonsampled 
	conditions on plot. }
  \item{adjtree}{ Logical. If TRUE, trees are individually adjusted by adjustment factors.
	Adjustment factors must be included in tree table (tadjfac). }
  \item{adjTPA}{ Numeric. A tree-per-acre adjustment. Use for DESIGNCD=1 (annual inventory),
	if using less than 4 subplots. If using only 1 sublot for estimate, adjTPA=4. The 
	default is 1. }
  \item{savedata}{ Logical. If TRUE, writes output data to outfolder. }
  \item{outfolder}{ String. The name of the output folder. If savedata=TRUE, all output 
	saved to the outfolder. If savedata=FALSE, only a text file of input parameters is saved. }
  \item{outfn}{ String. The name of the output file if savedata=TRUE (*.csv). Do not 
	include extension. If NULL, the file will be named tsum_'date'.csv }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite}{ Logical. If TRUE, overwrite output file. }
  \item{tround}{ Number. The number of digits to round to. If NULL, default=6. }
  \item{checkNA}{ Logical. If TRUE, checks if NA values exist in necessary variables. }
}
\details{ If variable = NULL, then it will prompt user for input. 

  Dependent external functions: datFilter
  Dependent internal functions: addcommas, fileexistsnm, getadjfactor 

  For adjcond (bycond=FALSE): \cr
  If you want to get trees-per-acre information aggregated to plot or condition level, 
	you need to include a TPA variable in tree table. \cr
  For tsumvars = GROWCFGS, GROWBFSL, GROWCFAL, FGROWCFGS, FGROWBFSL, or FGROWCFAL, 
	you must have TPAGROW_UNADJ \cr
  For tsumvars = MORTCFGS, MORTBFSL, MORTCFAL, FMORTCFGS, FMORTBFSL, or FMORTCFAL, 
	you must have TPAMORT_UNADJ \cr
  For tsumvars = REMVCFGS, REMVBFSL, REMVCFAL, FREMVCFGS, FREMVBFSL, or FREMVCFAL, 
	you must have TPAREMV_UNADJ \cr

  If you want to adjust plot-level information by condition proportions (adjplot), 
	you need to include CONDID & CONDPROP_UNADJ in cond or tree table and COND_STATUS_CD 
	and FORTYPCD if adjplot="FVS". For adjplot="WEIGHTED" - each tree is weighted 
	(or multiplied) by the condition proportions (CONDPROP_UNADJ). For adjplot="FVS", 
	each tree is weighted (or multiplied) by the sum of the condition proportions 
	(CONDPROP_UNADJ) that are forested and not nonstocked (COND_STATUS_CD = 1 and 
	FORTYPCD != 999).  \cr
}

\value{
 A list of the following items:
   \item{treedat}{ Data frame. Plot or condition-level table with aggregated tree attributes. }
   \item{sumvars}{ String vector. Name(s) of the output aggregated tree attributes. } 

   If savedata=TRUE\cr
	- treedat will be saved to the outfolder. \cr
	- a text file of input parameters is saved to outfolder ('outfn'_parameters_'date'.txt). 
}
\author{
	Tracey S. Frescino
}

\note{
	If a dat table is provided, the aggregated tree data will be merged to table and NULL 
	values will be output as 0.	

}

\keyword{ data }
