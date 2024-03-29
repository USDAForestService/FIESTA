% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datSumCond.R
\name{datSumCond}
\alias{datSumCond}
\title{Data - Aggregates numeric condition data to plot level.}
\usage{
datSumCond(
  cond = NULL,
  datsource = "obj",
  data_dsn = NULL,
  plt = NULL,
  subp_cond = NULL,
  subplot = NULL,
  cuniqueid = "PLT_CN",
  puniqueid = "CN",
  condid = "CONDID",
  bycond = FALSE,
  bysubp = FALSE,
  subpid = "SUBP",
  csumvar = NULL,
  csumvarnm = NULL,
  cfilter = NULL,
  getadjplot = FALSE,
  adjcond = FALSE,
  NAto0 = FALSE,
  cround = 5,
  returnDT = TRUE,
  savedata = FALSE,
  savedata_opts = NULL,
  dbconn = NULL,
  dbconnopen = FALSE,
  gui = FALSE
)
}
\arguments{
\item{cond}{Data frame or comma-delimited file (*.csv). Condition-level
table with aggregate variable and CONDPROP_UNADJ.}

\item{datsource}{String. Source of data ('obj', 'csv', 'sqlite', 'gdb').}

\item{data_dsn}{String. If datsource='sqlite', the name of SQLite database
(*.sqlite).}

\item{plt}{Data frame, comma-delimited file (*.csv), shapefile (*.shp), or
database file. Plot-level table to join the aggregated tree data to (if
bycond=FALSE). Nonsampled plots (PLOT_STATUS_CD = 3) are removed. Optional.}

\item{subp_cond}{Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
Subplot condition-level table to use to sum condition proportions, 
if bysubp=TRUE.}

\item{subplot}{Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
Subplot-level table to used to calculate adjustment factors, to remove 
nonsampled conditions (SUBP_STATUS_CD = 3). This table is optional.}

\item{cuniqueid}{String. Unique identifier of cond (default = "PLT_CN").}

\item{puniqueid}{String. Unique identifier of plt (default = "CN").}

\item{condid}{String. Unique identifier for conditions.}

\item{bycond}{Logical. If TRUE, the data are aggregated to the condition
level (by: cuniqueid, condid). If FALSE, the data are aggregated to the plot
level (by: puniqueid).}

\item{bysubp}{Logical. If TRUE, data are aggregated to the subplot level.}

\item{subpid}{String. Unique identifier of each subplot.}

\item{csumvar}{String. One or more variable names to sum to plot level.}

\item{csumvarnm}{String. Name of the resulting aggregated plot-level
variable(s).  Default = csumvar + '_PLT'.}

\item{cfilter}{String. A filter to subset the cond data before aggregating
(e.g., "COND_STATUS_CD == 1"). Must be R syntax.}

\item{getadjplot}{Logical. If TRUE, adjustments are calculated for
nonsampled conditions on plot.}

\item{adjcond}{Logical. If TRUE, csumvar condition variables are adjusted
for nonsampled conditions by plot.}

\item{NAto0}{Logical. If TRUE, convert NA values to 0.}

\item{cround}{Number. The number of digits to round to. If NULL, default=5.}

\item{returnDT}{Logical. If TRUE, returns data.table object(s). If FALSE,
returns data.frame object(s).}

\item{savedata}{Logical. If TRUE, saves data to outfolder.}

\item{savedata_opts}{List. See help(savedata_options()) for a list
of options. Only used when savedata = TRUE. If out_layer = NULL,
default = 'condsum'.}

\item{dbconn}{Open database connection.}

\item{dbconnopen}{Logical. If TRUE, keep database connection open.}

\item{gui}{Logical. If gui, user is prompted for parameters.}
}
\value{
A list of the following items: \item{condsum}{ Data frame.
Plot-level table with aggregated condition attribute. } \item{cfilter}{
Condition filter. }

If savedata=TRUE, condsum is saved to the outfolder.
}
\description{
Aggregates CONDPROP_UNADJ variable or other continuous condition variables
to plot level with option to apply condition filters. If condition variable
is not CONDPROP_UNADJ the variable is multiplied by CONDPROP_UNADJ for
weighted sum.
}
\details{
If variable = NULL, then it will prompt user for input.
}
\note{
Nonsampled plots are removed from table.
}
\examples{
# Aggregate LIVE_CANOPY_CVR_PCT to plot, weighted by CONDPROP_UNADJ
condsum <- datSumCond(cond = FIESTA::WYcond,
                      csumvar = "LIVE_CANOPY_CVR_PCT")$condsum

# Check results
condsum[condsum$PLT_CN == 40404737010690,]
FIESTA::WYcond[FIESTA::WYcond$PLT_CN == 40404737010690,]
}
\author{
Tracey S. Frescino
}
\keyword{data}
