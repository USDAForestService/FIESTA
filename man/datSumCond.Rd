\name{datSumCond}
\alias{datSumCond}
\title{
	Data - Aggregates numeric condition data to plot level.
}
\description{
	Aggregates CONDPROP_UNADJ variable or other continuous condition variables
	to plot level with option to apply condition filters. If condition variable
	is not CONDPROP_UNADJ the variable is multiplied by CONDPROP_UNADJ for 
	weighted sum.
}
\usage{
datSumCond(cond = NULL, plt = NULL, plt_dsn = NULL, cuniqueid = "PLT_CN", 
	puniqueid = "CN", csumvar = NULL, csumvarnm = NULL, cfilter = NULL, 
	savedata = FALSE, outfolder = NULL, outfn = NULL, outfn.date = TRUE, 
	overwrite = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cond}{ Data frame or comma-delimited file (*.csv). Condition-level table with 
	aggregate variable and CONDPROP_UNADJ. }
  \item{plt}{ Data frame, comma-delimited file (*.csv), shapefile (*.shp), or 
	database file. Plot-level table to join the aggregated tree data to 
	(if bycond=FALSE). Nonsampled plots (PLOT_STATUS_CD = 3) are removed. Optional. }
  \item{plt_dsn}{ String. The data source name (dsn; i.e., folder or database name)
	of plt. The dsn varies by driver. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html). Optional. } 
  \item{cuniqueid}{ String. Unique identifier of cond (default = "PLT_CN"). }
  \item{puniqueid}{ String. Unique identifier of plt (default = "CN"). }
  \item{csumvar}{ String. One or more variable names to sum to plot level. }
  \item{csumvarnm}{ String. Name of the resulting aggregated plot-level variable(s).
	Default = csumvar + '_PLT'. }
  \item{cfilter}{ String. A filter to subset the cond data before aggregating 
	(ex. "COND_STATUS_CD == 1"). Must be R syntax. }
  \item{savedata}{ Logical. If TRUE, writes output data to outfolder. }
  \item{outfolder}{ String. Name of the output folder. If savedata=TRUE, output is
	saved to the outfolder. }
  \item{outfn}{ String. Name of output file if savedata=TRUE (*.csv). Do not 
	include extension. If NULL, the file will be named sumdat.csv. }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite}{ Logical. If TRUE, overwrite output file. }
}
\details{ 
	If variable = NULL, then it will prompt user for input. 
}

\value{
  A list of the following items:
  \item{condsum}{ Data frame. Plot-level table with aggregated condition attribute. }
  \item{cfilter}{ Condition filter. }
	
  If savedata=TRUE, condsum is saved to the outfolder.
  
}
\note{
  Nonsampled plots are removed from table.
}

\author{
	Tracey S. Frescino
}

\examples{

  ## Aggregate LIVE_CANOPY_CVR_PCT to plot, weighted by CONDPROP_UNADJ
  condsum <- datSumCond(cond=WYcond, csumvar="LIVE_CANOPY_CVR_PCT")$condsum

  ## Check results
  condsum[condsum$PLT_CN == 40404737010690,]
  WYcond[WYcond$PLT_CN == 40404737010690,]
}

\keyword{ data }
