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
	rprefix = "r", datround = NULL, savedata = FALSE, outfolder = NULL,
 	out_fmt = "csv", out_dsn = NULL, out_layer = NULL, outfn.pre = NULL, 
	layer.pre = NULL, outfn.date = FALSE, overwrite_dsn = FALSE, 
	overwrite_layer=FALSE, append_layer = FALSE)
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
  \item{outfolder}{ String. Name of the output folder. If savedata=TRUE, output is
	saved to the outfolder. }
  \item{out_fmt}{ String. Format for output tables ('csv', 'sqlite', 'gpkg'). }
  \item{out_dsn}{ String. Data source name for output. If extension is not
	included, out_fmt is used. Use full path if outfolder=NULL. }
  \item{out_layer}{ String. Name of output layer in database or *.csv file, 
	if savedata=TRUE. If NULL, the file will be named tsum_'date'.csv. }
  \item{outfn.pre}{ String. Prefix for out_dsn. }
  \item{layer.pre}{ String. Prefix for out_layer. }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite_dsn}{ Logical. If TRUE, overwrites raw_dsn, if exists. }
  \item{overwrite_layer}{ Logical. If TRUE, overwrites the out_layer in raw_dsn
	or *.csv raw data layer, if datsource="csv". }
  \item{append_layer}{ Logical. If TRUE, and rawdata=TRUE, appends raw data data frames 
	to existing out_dsn layer or *.csv file. }  
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
