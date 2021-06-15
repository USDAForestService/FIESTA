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
spExtractRast(xyplt, xyplt_dsn = NULL, uniqueid = "PLT_CN", rastlst, 
	rastfolder = NULL, rast.crs = NULL, bandlst = NULL, var.name = NULL, 
	interpolate = FALSE, windowsize = 1, windowstat = NULL, 
	rast.NODATA = NULL, keepNA = TRUE, showext = FALSE, savedata = FALSE, 
	exportsp = FALSE, exportNA = FALSE, outfolder = NULL, out_fmt = "shp", 
	out_dsn = NULL, out_layer = "rastext", outfn.pre = NULL, outfn.date = TRUE, 
	overwrite_dsn = FALSE, overwrite_layer = TRUE, ...)
}

\arguments{
  \item{xyplt}{ Data frame object or String. Name of layer with xy coordinates
	and unique identifier. Can be layer with xy_dsn, full pathname, including 
	extension, or file name (with extension) in xy_dsn folder. }
  \item{xyplt_dsn}{ String. Name of database where xyplt is. The dsn varies by
	driver. See gdal OGR vector formats (https://www.gdal.org/ogr_formats.html).}
  \item{uniqueid}{ String. Unique identifier of xyplt rows. }
  \item{rastlst}{ String vector or list or strings and/or rasters. File name(s)
	with extensions, or raster object(s). Note: raster objects must be written 
	to file. }
  \item{rastfolder}{ String. Name of the folder with raster layers. Optional. 
	Useful if all raster layers are in same folder. }
  \item{rast.crs}{ EPSG code or PROJ.4 String. Name of coordinate reference 
	system for rasters with no projection defined. If more than one raster has 
	no projection defined, the same crs will be used. }
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
  \item{rast.NODATA}{ Numeric vector. NODATA value(s) of raster if not predefined
	(See notes below). This value will be converted to NA and removed if keepNA=FALSE. 
	If rastfnlst includes more than one raster, the rast.NODATA value should coincide 
	with number of rasters in rastfnlst. If only one rast.NODATA, the same NODATA 
	value is used for all rasters. }
  \item{keepNA}{ Logical. If TRUE, keeps NA values after data extraction. }
  \item{showext}{ Logical. If TRUE, layer extents are displayed in plot window. }
  \item{savedata}{ Logical. If TRUE, the input data with extracted values are
	saved to outfolder. }
  \item{exportsp}{ Logical. If TRUE, the extracted raster point data are 
	exported to outfolder. }
  \item{exportNA}{ Logical. If TRUE, NA values are exported to outfolder. }
  \item{outfolder}{ String. If savedata=TRUE or exportsp=TRUE, name of output folder. 
	If NULL, the working directory is used. }
  \item{out_fmt}{ String. Format for output tables ('csv', 'sqlite', 'gpkg').}
  \item{out_dsn}{ String. Name of database if out_fmt = c('sqlite', 'gpkg'). }
  \item{out_layer}{ String. Name of layer in out_dsn if database. }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile 
	(e.g., outfn_'date'.csv). }
  \item{overwrite_dsn}{ Logical. If TRUE, overwrite dsn. }
  \item{overwrite_layer}{ Logical. If TRUE, overwrite layer(s) in dsn. }
  \item{outfn.pre}{ String. Add a prefix to output name (e.g., "01"). }
  \item{...}{ Other parameters for spMakeSpatialPoints. }
}
\details{
 *If variable = NULL, then it will prompt user for input.

}
\value{
  \item{sppltext}{ sf object or data frame. Input xyplt data with extracted raster 
	values appended. }
  \item{outnames}{ String vector. Raster output names. }
  \item{rastfnlst}{ String vector. Raster pathnames. }
  \item{inputdf}{ Data frame. Raster information input to zonal summaries. }
  \item{NAlst}{ sf List. If NA values exist after data extraction, the spatial NA
	points are returned. }

  If savedata=TRUE, pltassgn and unitarea are saved to outfolder.\cr
  If exportsp=TRUE, the spatial sf points object is exported to outfolder.\cr.
  If exportNA=TRUE and NA values exist after data extraction, the spatial NA points
	are exported to outfolder.
   
}
\author{
	Tracey S. Frescino
}
\note{

  rast.NODATA\cr
  NODATA values are raster pixel values that have no data of interest, including pixels 
	within the extent of the layer, but outside the area of interest. Sometimes these 
	pixels have been defined previously. The defined NODATA pixels are imported to R 
	as NULL values. When not previously defined, the pixels outside the area of interest 
	will be the minimum or maximum value depending on the data type 
	(e.g., 16-bit signed: min=-32,768; max=32,768) or byte size (1 byte: min=0; max=255).
      These NODATA values will be added to the zonal statistic calculations if not
	specified in rast.NODATA. 

  
  The spTransform (rgdal) method is used for on-the-fly map projection conversion and 
	datum transformation using PROJ.4 arguments. Datum transformation only occurs if 
	the +datum tag is present in the both the from and to PROJ.4 strings. The +towgs84 
	tag is used when no datum transformation is needed. PROJ.4 transformations assume 
	NAD83 and WGS84 are identical unless other transformation parameters are specified. 
	Be aware, providing inaccurate or incomplete CRS 	information may lead to erroneous 
	data shifts when reprojecting. See spTransform help documentation for more details.
}

\examples{

  ## Get point data from WYplt data in FIESTA
  WYplt <- FIESTA::WYplt

  ## Get raster layers from FIESTA external data
  fornffn <- system.file("extdata", "sp_data/WYbighorn_forest_nonforest_250m.tif", package="FIESTA")
  demfn <- system.file("extdata", "sp_data/WYbighorn_dem_250m.img", package="FIESTA")


  ## Extract points from raster
  xyext <- spExtractRast(xyplt=WYplt, rastlst=c(fornffn, demfn), var.name=c("fornf", "dem"),
		uniqueid="CN", xvar="LON_PUBLIC", yvar="LAT_PUBLIC", xy.crs=4269)
  names(xyext)
  xyext$outnames
  sppltext <- xyext$sppltext
  head(sppltext)
  xyext$inputdf

  ## Plot extracted values of forest/nonforest
  plot(sppltext["fornf"])

  ## Plot extracted values of dem (i.e., elevation)
  plot(sppltext["dem"])
}

\keyword{ data }
