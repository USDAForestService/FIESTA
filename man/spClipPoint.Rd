\name{spClipPoint}
\alias{spClipPoint}
\title{
	Spatial - Subsets an S4 SpatialPoints layer with a SpatialPolygon layer.
}
\description{
	Subsets an S4 SpatialPoints or point file with a SpatialPolygon layer with
	option to export the clipped SpatialPoints layer to a shapefile (*.shp). 
}				

\usage{
spClipPoint(spplt = NULL, spplt_dsn = NULL, xyplt = NULL, uniqueid = "PLT_CN", 
	clippolyv_layer, clippolyv_dsn = NULL, showext = FALSE, keepnull = FALSE, 
	savedata = FALSE, outfolder = NULL, outfn = NULL, outfn.pre = NULL, 
	outfn.date = FALSE, overwrite = FALSE, othertabnms = NULL, ...)
}

\arguments{
  \item{spplt}{ S4 SpatialPoints object or shapefile filename (*.shp), including 
	full path and extension. The spatial layer must have a defined projection.
	If NULL, include xyplt. }
  \item{spplt_dsn}{ String. The data source name (dsn; i.e., folder or database name)
	of spplt. The dsn varies by driver. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html). Optional. } 
  \item{xyplt}{ Data frame, data.table, matrix, or comma-delimited file (*.csv). 
	Includes unique identifier of rows (uniqueid) and x and y coordinates. 	
	If spplt=NULL, x/y coordinates are used for data extraction. Must include 
	projection information in respective parameters. }
  \item{uniqueid}{ String.* Unique identifier of xyplt rows. }
  \item{clippolyv_layer}{ String or S4 object. The name of the polygon layer 
	to use for clipping. The layer name may be a full pathname to a file, the 
	basename to a file, a spatial layer name from a database, or a SpatialPolygons 
	object with a defined projection. }
  \item{clippolyv_dsn}{ String. The data source name (dsn; i.e., folder or database name)
	of clippolyv_layer. The dsn varies by driver. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html). Optional. } 
  \item{showext}{ Logical. If TRUE, layer extents are displayed in plot window. }
  \item{keepnull}{ Logical. If TRUE, keep values that do not intersect clippolyv. }
  \item{savedata}{ Logical. If TRUE, the input data clipped to clippolyv are
	saved to outfolder. The spatial object is exported as an ArcGIS shapefile (*.shp)
	with variable names truncated to 10 characters or less (See note below). }
  \item{outfolder}{ String. Name of output folder. }
  \item{outfn}{ String. Name of output file name. Default="pntclip". } 
  \item{outfn.pre}{ String. A prefix for output table names. }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite}{ Logical. If TRUE, overwrites file if exists. }
  \item{othertabnms}{ String vector. The names of the objects or comma-delimited files to subset.
	 The names must be in quotes (ex. othertables=c("tree", "cond")). }
  \item{...}{ Other parameters for spMakeSpatialPoints. }

}
\value{
  A list of the following objects:

  \item{clip_spplt}{ SpatialPointsDataFrame. The input SpatialPoints or xyplt
	data frame converted to SpatialPoints, clipped to polygon boundary layer. 
      The projection will be same as clippolyv projection. }
  \item{clip_xyplt}{ Data frame. The input Spatialpoints data or xyplt data frame, 
	clipped to polygon boundary layer. }
  \item{clip_poly}{ SpatialPolygonsDataFrame. The polygon boundary layer used for 
	clipping. }
  \item{sppltnull}{ SpatialPointsDataFrame. Non-intersecting points. Only 
	included if keepnull=TRUE. }
  \item{clip_tabs}{ Data frame(s). Other tables in intabs clipped to boundary. }
  
  If savedata=TRUE, point data will be written to outfolder.\cr
  If exportshp=TRUE, the Spatial points will be written to outfolder. Variable
	name will be truncated to 10 characters or less. 
}

\author{
	Tracey S. Frescino
}

\details{
	The rgeos gCovers function is used to return intersecting points. \cr 

	If the projection of clippoly is not the same as the Spatial Points, the Spatial 
	Points layer will be reprojected to the same projection as the clippoly before the 
	evaluation points.
}


\note{
  	The spTransform (rgdal) method is used for on-the-fly map projection conversion and 
	datum transformation using PROJ.4 arguments. Datum transformation only occurs if 
	the +datum tag is present in the both the from and to PROJ.4 strings. The +towgs84 
	tag is used when no datum transformation is needed. PROJ.4 transformations assume 
	NAD83 and WGS84 are identical unless other transformation parameters are specified. 
	Be aware, providing inaccurate or incomplete CRS 	information may lead to erroneous 
	data shifts when reprojecting. See spTransform help documentation for more details.
}



\keyword{ data }
