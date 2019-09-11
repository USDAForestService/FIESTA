\name{spZonalRast}
\alias{spZonalRast}
\title{
	Spatial - Extracts summary statistics by polygon (i.e., zone) for a raster.
}
\description{
	Extracts summary statistics by polygon, or zone for a raster 
	(single or multi-band). 
}

\usage{
spZonalRast(polyv_layer, polyv_dsn = NULL, attribute = NULL, rast, 
	rastfolder = NULL, bands = NULL, zonalstat = NULL, pixelfun = NULL, 
	outname = NULL, showext = FALSE, rastlut = NULL, rast.NODATA = 0, 
	savedata = FALSE, outfolder = NULL, outfn = NULL, overwrite = FALSE)
}

\arguments{
  \item{polyv_layer}{ String or S4 object. The name of the zonal
	spatial polygons layer. The layer name may be a full pathname to a file, 
	the basename to a file, a spatial layer name from a database, or a 
	SpatialPolygons object with a defined projection. }
  \item{polyv_dsn}{ String. The data source name (dsn; i.e., folder or database name)
	of polyv_layer. The dsn varies by driver. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html). Optional. } 
  \item{attribute}{ String. Name of attribute in polyv to identify polygons to 
	summarize statistics for. }
  \item{rast}{ String or Raster. File name(s) with extensions, or raster object(s). 
	Note: raster objects must be written to file. }
  \item{rastfolder}{ String. Name of the folder with raster layers. Optional. 
	Useful if all raster layers are in same folder. }
  \item{bands}{ Numeric vector. If rast is a multi-layer raster and only 1 or some 
	layers are desired, specify layer number(s) in a vector format. If NULL, all 
	layers are summed. }
  \item{zonalstat}{ String vector. Zonal statistic(s) to return for rasters with 
	continuous data ("mean", "sum", "majority", "minority", "variety", "npixels")
	or rasters with discrete data ("count", "proportion"). }
  \item{pixelfun}{ Function. A function to apply to the individual pixel values
	before calculating sum and mean. The function should accept a single 
	numeric argument (pixel value) and return a single numeric argument. }
  \item{outname}{ String. Variable name for output. The output names will use
	outname as a prefix to summary statistics (i.e., 'outname'.mean, 'outname'.sum). }
  \item{showext}{ Logical. If TRUE, layer extents are displayed in plot window. }
  \item{rastlut}{ Data frame. A look up table to recode raster values. Must be
	2 columns: Column 1 with raster values and column 2 with recode values.  }
  \item{rast.NODATA}{ Numeric. NODATA value if stratlayer is raster. These
	values will be removed from output strata table, assuming outside boundary. }
  \item{savedata}{ Logical. If TRUE, the zonal data are saved to outfolder. }
  \item{outfolder}{ String. The output folder. }
  \item{outfn}{ String. Name of output data file. If NULL, default is 'zonalext.csv'. }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
}
\details{
  Use spZonalRast() to prompt for input.

  If the projection of polyv is different than the projection of rast, the polyv
	SpatialPolygons object is converted to the projection of rast (See note about 
	on-the-fly projection conversion).

}
\value{
  \item{zonalext}{ Data frame. Zonal statistics by polygon attribute (attribute). }
  \item{outname}{ String vector. Names of zonal statistic variables generated in 
	zonalext data frame. }
  \item{rasterfile}{ String vector. Names of raster file(s) associated with zonal
	statistic. }

  If savedata=TRUE, outdat data frame is saved to outfolder (Default name: zonalext_'date'.csv).
   
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
	Tracey S. Frescino, Chris Toney
}

\keyword{ data }
