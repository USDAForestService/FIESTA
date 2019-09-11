\name{spExtractPoly}
\alias{spExtractPoly}
\title{
	Spatial - Extracts point attribute values from SpatialPolygons layer(s).
}
\description{
	Extracts values from one or more polygon layers and appends to input 
	SpatialPoints layer or data frame. Points are reprojected on-the-fly to 
	projection of SpatialPolygons using PROJ.4 transformation parameters and 
	rgdal spTransform function. 
}

\usage{
spExtractPoly(spplt = NULL, spplt_dsn = NULL, xyplt = NULL, uniqueid = "PLT_CN", 
	polylst, poly_dsn = NULL, polyvarlst = NULL, polyvarnmlst = NULL, 
	keepnull = TRUE, showext = FALSE, savedata = FALSE, exportshp = FALSE, 
	outfolder = NULL, outfn = NULL, outfn.date = TRUE, overwrite = FALSE, ...)
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
  \item{polylst}{ String vector. Name(s) of polygon layers. Must be either
	shapefiles or SpatialPolygonsDataFrame objects. Include extensions. }
  \item{poly_dsn}{ String. Name of the folder or database with polygon layers. }
  \item{polyvarlst}{String vector or list. The name(s) of variable(s) to extract from polygon 
	layer. If extracting multiple variables from more than one polygon, specify the names in 
	a list format, corresponding to polylst. }
  \item{polyvarnmlst}{String vector or list. The output name(s) of variable(s) extracted 
	from polygon layer. If extracting multiple variables from more than one polygon, 
	specify the names in a list format, corresponding to polylst. The number of names
	must match the number of variables in polyvarlst. }
  \item{showext}{ Logical. If TRUE, layer extents are displayed in plot window. }
  \item{keepnull}{ Logical. If TRUE, keep NULL values after extraction. }
  \item{savedata}{ Logical. If TRUE, the input data with extracted values are
	saved to outfolder. }
  \item{exportshp}{ Logical. If TRUE, exports Spatial object to outfolder as an 
	ArcGIS shapefile (*.shp). Variable names are truncated to 10 characters or less
	(See note below). }
  \item{outfolder}{ String. If savedata=TRUE or exportshp=TRUE, name of output folder. 
	If NULL, the working directory is used. }
  \item{outfn}{ String. Name of the output shapefile/.csv file (Do not include 
	extension). If NULL, default name is datext_'date'. }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
  \item{...}{ Other parameters for spMakeSpatialPoints. }
}
\details{
 *If variable = NULL, then it will prompt user for input.

  keepnull\cr
  If keepnull=FALSE, points are excluded when all extracted variables from any 
	one SpatialPolygons are NULL, returning the points that fall within the '
	intersecting polygons.

}
\value{
  \item{pltdat}{ SpatialPointsDataFrame object or data frame. Input point data with
	extracted raster values appended. For multi-part polygons, more than 1 row
	per point may be output. }
  \item{var.name}{ String vector. Variable names of extracted variables. }

  If savedata=TRUE, outdat data frame is saved to outfolder (Default name: datext_'date'.csv).
  If exportshp=TRUE, the SpatialPointsDataFrame object is exported to outfolder
	(Default name: datext_'date'.shp). Variable names are truncated to 10 characters or 
	less (See note below). Name changes are output to 'outfn'_newnames_'data'.csv in
	outfolder.
   
}
\author{
	Tracey S. Frescino
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
	Be aware, providing inaccurate or incomplete CRS information may lead to erroneous 
	data shifts when reprojecting. See spTransform help documentation for more details.
}

\keyword{ data }
