\name{datExportData}
\alias{datExportData}
\title{
	Spatial - Exports a data frame object.
}
\description{
	Exports a data frame object to a specified output.
}

\usage{
datExportData(dfobj, outfolder = NULL, out_fmt = "csv", out_dsn = NULL, 
	out_layer = NULL, outfn.pre = NULL, layer.pre = NULL, outfn.date = FALSE, 
	create_dsn = FALSE, overwrite_dsn = FALSE, overwrite_layer = FALSE, 
	add_layer = TRUE, append_layer = FALSE, index.unique = NULL, index = NULL)
}
\arguments{
  \item{dfobj}{ Data.frame class R object. Data frame object to export. }
  \item{outfolder}{ String. Optional. Name of output folder. If NULL, export
	to working directory. }
  \item{out_fmt}{ String. File format for output ('csv', 'sqlite','gpkg', 'gdb').
	If out_fmt %in% c('sqlite','gpkg'), RSQLite package must be installed. If
	out_fmt='gdb', arcgisbinding package and R-Bridge must be installed. }
  \item{out_dsn}{ String. Data source name for output. If extension is not
	included, out_fmt is used. Use full path if outfolder=NULL. }
  \item{out_layer}{ String. Name of layer in out_dsn. If NULL, basename of 
	out_dsn is used. }
  \item{outfn.pre}{ String. Prefix for out_dsn or csv file, if out_fmt=".csv". }
  \item{layer.pre}{ String. Prefix for layer in database, if out_fmt != ".csv". }
  \item{outfn.date}{ Logical. If TRUE, add current date to out_dsn. }
  \item{create_dsn}{ Logical. If TRUE, creates new database. }
  \item{overwrite_dsn}{ Logical. If TRUE, overwrites the out_dsn, if exists. }
  \item{overwrite_layer}{ Logical. If TRUE, overwrites the out_layer, if exists. }
  \item{add_layer}{ Logical. If TRUE, adds to existing out_dsn 
	(if out_fmt != c('csv','shp')). }
  \item{append_layer}{ Logical. If TRUE, appends to a layer in an existing out_dsn
	or a csv or shapefile. Note: currently cannot append to layer in out_dsn 
	when out_fmt="gdb". }
  \item{index.unique}{ String. Name of variable(s) in dfobj to make unique index. }
  \item{index}{ String. Name of variable(s) in dfobj to make (non-unique) index. }
}
\details{
	Wrapper for sf::st_write function.
}
\value{
  	An sf spatial object is written to the out_dsn. 
}
\author{
	Tracey S. Frescino
}
\note{
  If out_fmt='shp':\cr
 	The ESRI shapefile driver truncates variable names to 10 characters or less.
	Variable names are changed before export using an internal function
	(trunc10shp). Name changes are output to the outfolder, 'outshpnm'_newnames.csv.

  If sf object has more than 1 record, it cannot be exported to a shapefile. 

}
\keyword{ data }
