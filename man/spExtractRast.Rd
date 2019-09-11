\name{spExtractRast}
\alias{spExtractRast}
\title{
	Spatial - Extracts point attribute values from raster layer(s).
}
\description{
	Extracts values from one or more raster layers and appends to input 
	SpatialPoints layer or data frame. Points are reprojected on-the-fly to 
	projection of raster(s) using PROJ.4 transformation parameters and rgdal 
	spTransform function. Includes options to use bilinear interpolation or 
	summarize over a window of n pixels using a specified statistic. 
}

\usage{
spExtractRast(spplt = NULL, spplt_dsn = NULL, xyplt = NULL, uniqueid = "PLT_CN", 
	rastlst = NULL, rastfolder = NULL, rast.prjstr = NULL, bandlst = NULL, 
	var.name = NULL, interpolate = FALSE, windowsize = 1, windowstat = NULL, 
	keepnull = TRUE, showext = FALSE, savedata = FALSE, exportshp = FALSE, 
	exportna = FALSE, outfolder = NULL, outfn = NULL, outfn.date = TRUE, 
	overwrite = FALSE, ...)
}

\arguments{
  \item{spplt}{ S4 SpatialPoints object or shapefile filename (*.shp), including 
	full path and extension. The spatial layer must have a defined projection.
	If NULL, include xyplt. }
  \item{spplt_dsn}{ String. Name of the folder or database with spplt. }
  \item{xyplt}{ Data frame, data.table, matrix, or comma-delimited file (*.csv). 
	Includes unique identifier of rows (uniqueid) and x and y coordinates. 	
	If spplt=NULL, x/y coordinates are used for data extraction. Must include 
	projection information in respective parameters. }
  \item{uniqueid}{ String.* Unique identifier of xyplt rows. }
  \item{rastlst}{ String vector or list or strings and/or rasters. File name(s)
	with extensions, or raster object(s). Note: raster objects must be written 
	to file. }
  \item{rastfolder}{ String. Name of the folder with raster layers. Optional. 
	Useful if all raster layers are in same folder. }
  \item{rast.prjstr}{ PRJ4STRING. Name of projection string for rasters with no
	projection defined. If more than one raster has no projection defined, the
	same proj4string will be used. }
  \item{bandlst}{ Numeric named list. If rastfnlst includes a multi-layer raster 
	and only 1 or some layers are desired, specify layer numbers in a named list 
	format with names matching the base names in rastfnlst (e.g., list(rast1=5, 
	rast3=1:3)). If NULL, all layers are extracted. }
  \item{var.name}{ String vector. Extracted variable name(s). If NULL, uses the
	basename of raster layer, including band number for multi-band rasters. }
  \item{interpolate}{ Logical vector. If TRUE, uses bilinear interpolation of pixel
	values, weighted average of 4 nearest pixels (i.e., continuous data). }
  \item{windowsize}{ Number vector. The size of window for summarizing data. }
  \item{windowstat}{ Character vector. If windowsize is greater than one, the
	statistic to use for summarizing data ("mean", "min", "max", "median", 
	"sum", "range", "var", "sd", "rsd", "mode", "value"). If windowstat="value",
	all pixel values are returned, otherwise 1 value per row in xyplt is returned. }
  \item{keepnull}{ Logical. If TRUE, keep null values. }
  \item{showext}{ Logical. If TRUE, layer extents are displayed in plot window. }
  \item{savedata}{ Logical. If TRUE, the input data with extracted values are
	saved to outfolder. }
  \item{exportshp}{ Logical. If TRUE, exports Spatial object to outfolder as an 
	ArcGIS shapefile (*.shp). Variable names are truncated to 10 characters or less
	(See note below). }
  \item{exportna}{ Logical. If TRUE, exports Spatial object of NA values to outfolder. }
  \item{outfolder}{ String. If savedata=TRUE or exportshp=TRUE, name of output folder. 
	If NULL, the working directory is used. }
  \item{outfn}{ String. Name of the output shapefile/.csv file (Do not include 
	extension). If NULL, default name is datext_'date'. }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite}{ Logical. If TRUE, overwrite files in outfolder. }
  \item{...}{ Other parameters for spMakeSpatialPoints. }
}
\details{
 *If variable = NULL, then it will prompt user for input.

}
\value{
  \item{pltdat}{ SpatialPointsDataFrame object or data frame. Input point data with
	extracted raster values appended. }
  \item{var.name}{ String vector. Variable names of extracted variables. }

  If savedata=TRUE, outdat data frame is saved to outfolder (Default name: datext_'date'.csv).
  If exportshp=TRUE, the SpatialPointsDataFrame object is exported to outfolder
	(Default name: datext_'date'.shp). Variable names are truncated to 10 characters or 
	less (See note below). Name changes are output to 'outfn'_newnames_'data'.csv in
	outfolder.
   
}
\author{
	Tracey S. Frescino, Chris Toney
}
\note{

  If exportshp=TRUE:\cr
 	The writeOGR (rgdal) function is called. The ArcGIS driver truncates variable 
	names to 10 characters or less. Variable names are changed before export using 
	an internal function (trunc10shp). If Spatial object has more than 1 record, 
	it will be returned but not exported. 

  
  The spTransform (rgdal) method is used for on-the-fly map projection conversion and 
	datum transformation using PROJ.4 arguments. Datum transformation only occurs if 
	the +datum tag is present in the both the from and to PROJ.4 strings. The +towgs84 
	tag is used when no datum transformation is needed. PROJ.4 transformations assume 
	NAD83 and WGS84 are identical unless other transformation parameters are specified. 
	Be aware, providing inaccurate or incomplete CRS 	information may lead to erroneous 
	data shifts when reprojecting. See spTransform help documentation for more details.
}

\keyword{ data }
