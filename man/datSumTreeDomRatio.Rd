\name{datSumTreeDomRatio}
\alias{datSumTreeDomRatio}
\title{
	Data - Generates ratio of tree domain variables.
}
\description{
	Generates ratio of tree domain summaries from FIESTA::datSumTreedom().
}
\usage{
datSumTreeDomRatio(ndat, ddat, uniqueid = "PLT_CN", nprefix = NULL, dprefix = NULL, 
	rprefix = "r", datround = NULL, savedata = FALSE, out_layer = NULL, outfolder = NULL,
 	out_fmt = "csv", out_dsn = NULL, append_layer = FALSE, outfn.pre = NULL, 
	outfn.date = FALSE, overwrite = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ndat}{ Dataframe, comma-delimited file (*.csv), or shapefile (*.shp). 
	Table from FIESTA::datSumTreeDomRatio() with numerator tree domain variables. }
  \item{ddat}{ Dataframe, comma-delimited file (*.csv), or shapefile (*.shp). 
	Table from FIESTA::datSumTreeDomRatio() with numerator tree domain variables. }
  \item{uniqueid}{ String. The unique identifier of both tables (default = "PLT_CN"). }
  \item{nprefix}{ String. The prefix variable identifier of numerator domain variables in ndat. }
  \item{dprefix}{ String. The prefix variable identifier of denominator domain variables in ddat. }
  \item{rprefix}{ String. The prefix variable identifier of new ratio variables (default="r"). }
  \item{datround}{ Integer. Number of digits to round ratio values to. }
  \item{savedata}{ Logical. If TRUE, writes output data to outfolder. }
  \item{outfolder}{ String. The output folder path. If NULL and savedata=TRUE, 
	outfolder is the working directory. }
  \item{out_layer}{ String. The output name of data table. If NULL, 'rprefix_data'. } 
  \item{out_fmt}{ String. File format for output ('csv', 'sqlite','gpkg', 'gdb').
	If out_fmt %in% c('sqlite','gpkg'), RSQLite package must be installed. If
	out_fmt='gdb', arcgisbinding package and R-Bridge must be installed. }
  \item{out_dsn}{ String. Data source name for output. If extension is not
	included, out_fmt is used. Use full path if outfolder=NULL. }
  \item{append_layer}{ Logical. If TRUE, appends to existing out_dsn. The out_dsn
	a database or shapefile. If FALSE, the out_dsn will be overwritten if exists. }
  \item{outfn.pre}{ String. The name used for prefix of outfiles (e.g., outfn.pre'_plt*'). }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile (e.g., outfn_'date'.csv). }
  \item{overwrite}{ Logical. If TRUE, overwrite data. }
}
\value{
  \item{rdat}{ Data frame. Table with ratio values (ndat / ddat). }
  \item{rvars}{ String vector. Variable names in rdat. }

  If savedata=TRUE, the data table will be saved to the outfolder: \cr
}
\author{
	Tracey S. Frescino
}


\keyword{ data }
