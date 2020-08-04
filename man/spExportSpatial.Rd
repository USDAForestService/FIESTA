\name{spExportSpatial}
\alias{spExportSpatial}
\title{
	Spatial - Exports an sf object.
}
\description{
	Exports an sf object to a specified output.
}

\usage{
spExportSpatial(sfobj, out_layer = NULL, out_fmt = "shp", outfolder = NULL, 
	out_dsn = NULL, outfn.pre = NULL, outfn.date = FALSE, 
	overwrite_dsn = FALSE, overwrite_layer = TRUE, append = FALSE)
}
\arguments{
  \item{sfobj}{ sf class R object. Spatial object to export. }
  \item{out_fmt}{ String. File format for output ('sqlite','gpkg','shp'). }
	If out_fmt %in% c('sqlite','gpkg'), RSQLite package must be installed. }
  \item{outfolder}{ String. Optional. Name of output folder. If NULL, export
	to working directory. }
  \item{out_dsn}{ String. Data source name for output. If extension is not
	included, out_fmt is used. Use full path if outfolder=NULL. }
  \item{out_layer}{ String. Name of layer in out_dsn. If NULL, basename of 
	out_dsn is used. }
  \item{outfn.pre}{ String. Prefix for out_dsn. }
  \item{outfn.date}{ Logical. If TRUE, add current date to out_dsn. }
  \item{overwrite_dsn}{ Logical. If TRUE, overwrites the out_dsn, if exists. }
  \item{overwrite_layer}{ Logical. If TRUE, overwrites the out_layer, if exists. }
  \item{append}{ Logical. If TRUE, appends to existing out_dsn. The out_dsn
		a database or shapefile. }
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
