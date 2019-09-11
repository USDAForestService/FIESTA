\name{spClipRast}
\alias{spClipRast}
\title{
	Spatial - Subsets a raster to a polygon extent or boundary.
}
\description{
	Subsets a raster to the extent or masked boundary of a SpatialPolygons object 
	or shapefile (*.shp) with option to write the new file to the outfolder with 
	specified format (fmt). 
}				

\usage{
spClipRast(polyv_layer, polyv_dsn = NULL, rast, rastfolder = NULL, rast.prjstr = NULL, 
	bands = NULL, NODATA = NULL, buffdist = NULL, maskByPolygons = TRUE, 
	showext = FALSE, setNODATA = FALSE, fmt = "HFA", outfolder = NULL, outfn = NULL,
 	overwrite=FALSE)
}

\arguments{
  \item{polyv_layer}{ String or S4 object. The name of the polygon layer 
	to be clipped. The layer name may be a full pathname to a file, the basename 
	to a file, a spatial layer name from a database, or a SpatialPolygons object 
	with a defined projection. }
  \item{polyv_dsn}{ String. The data source name (dsn; i.e., folder or database name)
	of poly_layer. The dsn varies by driver. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html). Optional. } 
  \item{rast}{ String or Raster. File name(s) with extensions, or raster object(s). }
  \item{rastfolder}{ String. Name of the folder with raster layers. Optional. 
	Useful if all raster layers are in same folder. }
  \item{rast.prjstr}{ PRJ4STRING. Name of projection string for rasters with no
	projection defined. If more than one raster has no projection defined, the
	same proj4string will be used. }
  \item{bands}{ Numeric vector. If rast is a multi-layer raster and only 1 or some 
	layers are desired, specify layer number(s) in a vector format. If NULL, all 
	layers are summed. }
  \item{NODATA}{ Number. The NODATA value for background values. If NODATA is set
	for input raster it is default, else 0 is default. }
  \item{buffdist}{ Number. The distance to buffer the polygon before clipping raster, in 
	units of raster. }
  \item{maskByPolygons}{ Logical. If TRUE, raster is clipped to the boundary of the polygon.
	If FALSE, raster is clipped to the extent of polygon. }
  \item{showext}{ Logical. If TRUE, layer extents are displayed in plot window. }
  \item{setNODATA}{ Logical. If TRUE, background NODATA values are set as NODATA. }
  \item{fmt}{ String. Format for exported raster. Default is format of unput raster. 
	("raster", "ascii", "SAGA", "IDRISI", "CDF", "GTiff", "ENVI", "EHdr", "HFA") }
  \item{outfolder}{ String. The output folder. }
  \item{outfn}{ String. Name of output data file. If NULL, default is 'rastclip'.
	If no extension, a default is provided to match output format. }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
}
\details{
  Use spClipRast() to prompt for input.

  If the projection of polyv is different than the projection of rast, the polyv
	SpatialPolygons object is converted to the projection of rast (See note about 
	on-the-fly projection conversion).

}
\value{
  \item{value}{ Spatial S4 object. A clipped raster file. }

  The clipped raster is written to outfolder with specified format or same format as input raster.
}

\note{
  On-the-fly projection conversion\cr 
  The spTransform (rgdal) method is used for on-the-fly map projection conversion and 
	datum transformation using PROJ.4 arguments. Datum transformation only occurs if 
	the +datum tag is present in the both the from and to PROJ.4 strings. The +towgs84 
	tag is used when no datum transformation is needed. PROJ.4 transformations assume 
	NAD83 and WGS84 are identical unless other transformation parameters are specified. 
	Be aware, providing inaccurate or incomplete CRS 	information may lead to erroneous 
	data shifts when reprojecting. See spTransform help documentation for more details.
}

\author{
	Tracey S. Frescino
}

\keyword{ data }
