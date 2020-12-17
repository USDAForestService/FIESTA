# functions for raster data
# Chris Toney, christoney at fs.fed.us

# depends on packages rgdal, Rcpp, sf
# package xml2 is required for GDAL Virtual Raster (VRT) manipulation
# r_rasterize.cpp implements RasterizePolygon function
# r_cmb_table.cpp implements CmbTable class
# r_running_stats.cpp implements RunningStats class
# for stand-alone use:
#library(rgdal)
#library(Rcpp)
#library(sf)
#library(xml2)
#sourceCpp("r_rasterize.cpp")
#sourceCpp("r_cmb_table.cpp")
#sourceCpp("r_running_stats.cpp")
#source("Iraster_analysis.R")

#TODO: functions with raster output overwrite silently, make this an argument

## getGDALDataTypeName
## getDefaultNodata
## getOffset
## getGDALformat
## basename.NoExt
## Mode
## northness
## eastness
## roughness
## TRI
## TPI
## getPixelValue
## extractPtsFromRaster
## extractPtsFromRasterList
## rasterInfo
## reprojectRaster
## rasterFromRaster
## rasterizePolygons
## polygonizeRaster
## clipRaster
## rasterCalc
## rasterCombine
## recodeRaster
## pixelCount
## focalRaster
## zonalStats
## zonalMean
## zonalFreq
## zonalMajority
## zonalMinority
## zonalVariety
## ptCsvToVRT
## rasterToVRT

GDT_NAMES <- c('Unknown', 'Byte', 'UInt16', 'Int16', 'UInt32',
				'Int32', 'Float32', 'Float64', 'CInt16', 'CInt32',
				'CFloat32', 'CFloat64')
				
DEFAULT_NODATA = list('Byte'= 255, 'UInt16'= 65535, 'Int16'= -32767,
						'UInt32'= 4294967293, 'Int32'= -2147483647, 
						'Float32'= 3.402823466E+38, 
						'Float64'= .Machine$double.xmax)

getGDALDataTypeName <- function(GDT_number) {
	GDT_NAMES[GDT_number+1]
}

getDefaultNodata <- function(GDT_name) {
	DEFAULT_NODATA[[GDT_name]]
}

getOffset <- function(coord, origin, gt_pixel_size) {
	(coord-origin)/gt_pixel_size
}

getGDALformat <- function(file) {
# GDAL format code from file extension for common output formats
	file <- as.character(file)
	if (endsWith(file, ".img")) {
		return("HFA")
	}
	if (endsWith(file, ".tif")) {
		return("GTiff")
	}
	if (endsWith(file, ".vrt")) {
		return("VRT")
	}
	
	return(NULL)
}

basename.NoExt <- function(filepath) {
	sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(filepath))
}

Mode <- function(x, na.rm=FALSE) {
	#Ties handled arbitrarily
	#TODO: return a vector with 1 or modal values
	if(na.rm) x = na.omit(x)
	ux <- unique(x)
	ux[which.max(tabulate(match(x, ux)))]
}

northness <- function(asp_deg) {
	#transform aspect degrees to northness
	#set flat to east for neutral value
	asp_deg[asp_deg == -1] = 90
	cos(asp_deg*pi/180)
}

eastness <- function(asp_deg) {
	#transform aspect degrees to eastness
	#set flat to north for neutral value
	asp_deg[asp_deg == -1] = 0
	sin(asp_deg*pi/180)
}

roughness <- function(x, na.rm=FALSE, asInt=TRUE) {
	# a terrain variability parameter
	# For example, applied to a neighborhood of elevation pixel values
	# Wilson et al. 2007. Marine Geodesy, 30: 3–35.
	# Calculated as the difference between the maximum and minimum values
	r = max(x, na.rm=na.rm) - min(x, na.rm=na.rm)
	ifelse(asInt, round(r), r)
}

TRI <- function(x, na.rm=FALSE, asInt=TRUE) {
	# terrain ruggedness index
	# Wilson et al. 2007. Marine Geodesy, 30: 3–35.
	# Calculated by comparing a central pixel with its neighbors, taking the
	# absolute values of the differences, and averaging the result.
	if(length(x) < 3) return(NA_real_)
	center = ceiling(length(x)/2)
	i = mean(abs(x[-center] - x[center]), na.rm=na.rm)
	ifelse(asInt, round(i), i)
}

TPI <- function(x, na.rm=FALSE, asInt=TRUE) {
	# topographic position index
	# Provides an indication of whether any	particular pixel forms part of a
	# positive (e.g., crest) or negative (e.g., trough) feature of the
	# surrounding terrain.	
	# Calculated as the difference between a central pixel and the mean of its
	# surrounding cells. The surrounding neighborhood is often defined in terms
	# of a circle or annulus.
	# see Wilson et al. 2007. Marine Geodesy, 30: 3–35.
	if(length(x) < 3) return(NA_real_)
	center = ceiling(length(x)/2)
	i = x[center] - mean(x[-center], na.rm=na.rm)
	ifelse(asInt, round(i), i)
}

getPixelValue <- function(pt, ds, band=1, interpolate=FALSE, windowsize=1, statistic=NULL, na.rm=TRUE) {
	# pt - vector containing a single coordinate c(x,y)
	# ds - GDAL dataset object for the raster
	# if interpolate is TRUE return an interpolated value for pt
	# (if TRUE bilinear interpolation is done, windowsize and statistic args ignored)
	# windowsize - extract a square window of pixels centered on pt
	# (for example, windowsize=3 for a 3x3 window of pixels)
	# statistic - summary statistic to calculate on the window of pixels
	# statistic one of: 'mean', 'min', 'max', 'median', 'sum', 'var', 'sd', 'rsd', 'mode'
	# if windowsize > 1 and statistic is NULL, returns a vector of the raw pixel values in window
	# na.rm only applies to summary statistics on windowsize > 1

	# getPixelValue() is not normally called directly
	# see extractPtsFromRaster() and extractPtsFromRasterList() below for typical uses
	
	if (interpolate) {
		if (windowsize > 2 || !is.null(statistic)) {
			warning("interpolate is TRUE, ignoring windowsize and statistic", call.=FALSE)
		}
		windowsize = 2
		statistic = NULL
	}

	ptX = as.numeric(pt[1])
	ptY = as.numeric(pt[2])

	nrows = .Call("RGDAL_GetRasterYSize", ds, PACKAGE="rgdal")
	ncols = .Call("RGDAL_GetRasterXSize", ds, PACKAGE="rgdal")
	gt = .Call("RGDAL_GetGeoTransform", ds, PACKAGE="rgdal")
	xmin = gt[1]
	xmax = xmin + gt[2] * ncols
	ymax = gt[4]
	ymin = ymax + gt[6] * nrows
	cellsizeX = gt[2]
	cellsizeY = gt[6]

	#check that point is inside extent rectangle
	if (ptX > xmax || ptX < xmin) {
		warning("point X value is outside raster extent", call.=FALSE)
		return(NA)
	}
	if (ptY > ymax || ptY < ymin) {
		warning("point Y value is outside raster extent", call.=FALSE)
		return(NA)
	}

	#get pixel offsets
	if (windowsize == 1) {
		offX = trunc(getOffset(ptX, xmin, cellsizeX))
		offY = trunc(getOffset(ptY, ymax, cellsizeY))
	}
	else {
		if (interpolate) {
			# 2x2 window for bilinear interpolation
			offX = trunc(getOffset(ptX, xmin, cellsizeX) - 0.5)
			offY = trunc(getOffset(ptY, ymax, cellsizeY) - 0.5)
		}
		else {
			offX = trunc(getOffset(ptX, xmin, cellsizeX)) - trunc(windowsize/2)
			offY = trunc(getOffset(ptY, ymax, cellsizeY)) - trunc(windowsize/2)
		}
		#entire window should be inside else return NA
		#TODO: option to use a partial window
		if (offX < 0 || offX > ncols || offY < 0 || offY > nrows) {
			warning("window is not completely within raster extent", call.=FALSE)
			return(NA)
		}
	}

	#get pixel value(s)
	a = rgdal::getRasterData(ds, band=band, offset=c(offY,offX), 
							region.dim=c(windowsize,windowsize), as.is=TRUE)

	#make sure nodata is set to NA
	#TODO: this is probably unnecessary, confirm in rgdal
	b = rgdal::getRasterBand(ds, band)
	noDataValue = .Call("RGDAL_GetNoDataValue", b, PACKAGE="rgdal")
	if(is.numeric(noDataValue)) {
		a[a==noDataValue] = NA
	}

	if (interpolate) {
		#return interpolated value at pt

		#convert to unit square coordinates for the 2x2 window
		#center of ll pixel in the window is 0,0
		x = getOffset(ptX, xmin, cellsizeX) - (offX + 0.5)
		y = (offY + 1.5) - getOffset(ptY, ymax, cellsizeY)
		
		#pixel values in the square
		#0,0: a[1,2]
		#1,0: a[2,2]
		#0,1: a[1,1]
		#1,1: a[2,1]
		pixelValue = ( a[1,2]*(1-x)*(1-y) + a[2,2]*x*(1-y) + 
						a[1,1]*(1-x)*y + a[2,1]*x*y )
	}
	else if (windowsize==1) {
		#return value of the single pixel containing pt
		pixelValue = a[1]
	}
	else if (is.null(statistic) && windowsize > 1) {
		#return a vector of the raw pixel values in the window centered on pt
		pixelValue = as.vector(a)
	}
	else if (!is.null(statistic) && windowsize > 1) {
		#return a summary statistic for the window
		if (statistic == "mean") {
			pixelValue = mean(a, na.rm=na.rm)
		}
		else if (statistic == "min") {
			pixelValue = min(a, na.rm=na.rm)
		}
		else if (statistic == "max") {
			pixelValue = max(a, na.rm=na.rm)
		}
		else if (statistic == "median") {
			pixelValue = median(a, na.rm=na.rm)
		}
		else if (statistic == "sum") {
			pixelValue = sum(a, na.rm=na.rm)
		}
		else if (statistic == "range") {
			r = range(a, na.rm=na.rm)
			pixelValue = r[2]-r[1]
		}
		else if (statistic == "var") {
			pixelValue = var(a, na.rm=na.rm)
		}
		else if (statistic == "sd") {
			pixelValue = sd(a, na.rm=na.rm)
		}
		else if (statistic == "rsd") {
			pixelValue = sd(a, na.rm=na.rm) / mean(a, na.rm=na.rm)
		}
		else if (statistic == "mode") {
			# TODO: NA handling
			if (length(a[!is.na(a)]) > 0) {
				pixelValue = Mode(a)
			}
			else {
				pixelValue = NA
			}
		}
		else {
			warning("invalid summary statistic", call.=FALSE)
			pixelValue = NA
		}
	}
	else {
		warning("invalid option", call.=FALSE)
		pixelValue = NA
	}

	return(pixelValue)
}

extractPtsFromRaster <- function(ptdata, rasterfile, band=NULL, var.name=NULL,
						interpolate=FALSE, windowsize=1, statistic=NULL, na.rm=TRUE) {
	# ptdata is a dataframe with three columns: col1=point id, col2=x, col3=y
	# see getPixelValue()

	ds = rgdal::GDAL.open(rasterfile, read.only=TRUE, silent=TRUE)
	
	df.out = data.frame(pid = ptdata[,1])
	
	if (is.null(var.name)) {
		var.name = basename.NoExt(rasterfile)
	}
	if (is.null(band)) {
		nbands = .Call("RGDAL_GetRasterCount", ds, PACKAGE="rgdal")
		bands = 1:nbands
	}
	else {
		bands = band
	}

	for (b in bands) {
		this.name = var.name
		if (length(bands) > 1) {
			this.name = paste0(var.name,"_b",b)
		}
		values = apply(ptdata[-1], 1, getPixelValue, ds=ds, band=b, 
					interpolate=interpolate, windowsize=windowsize, statistic=statistic, na.rm=na.rm)
		if (windowsize > 1 && is.null(statistic)) {
			# raw pixel values from a window, values as an array
			for (p in 1:(windowsize*windowsize)) {
				df.out[paste0(this.name,"_",p)] = values[p,]
			}
		}
		else {
			# a vector of values
			df.out[this.name] = values
		}
	}
	
	rgdal::GDAL.close(ds)

	return(df.out)
}


extractPtsFromRasterList <- function(ptdata, rasterfiles, bands=NULL, var.names=NULL,
							interpolate=FALSE, windowsizes=NULL, statistics=NULL, na.rm=TRUE) {
	# call extractPtsFromRaster for a list of rasters
	# allows for specific bands, or specific var.names by band, etc.

	if ( !all(file.exists(rasterfiles)) ) {
		print( rasterfiles[which(!file.exists(rasterfiles))] )
		stop("file not found")
	}
	
	nrasters = length(rasterfiles)

	# argument lengths must all be the same
	if (!is.null(bands)) {
		if (length(bands) != nrasters) {
			stop("list of band numbers must be same length as raster list")
		}
	}
	if (!is.null(var.names)) {
		if (length(var.names) != nrasters) {
			stop("list of variable names must be same length as raster list")
		}
	}		
	if (!is.null(windowsizes)) {
		if (length(windowsizes) != nrasters) {
			stop("list of window sizes must be same length as raster list")
		}
	}
	if (!is.null(statistics)) {
		if (length(statistics) != nrasters) {
			stop("list of statistics must be same length as raster list")
		}
	}

	if (is.null(windowsizes)) {
		# default to windowsize=1
		windowsizes = rep(1,nrasters)
	}

	df.out = data.frame(pid = ptdata[,1])

	for (n in 1:nrasters) {
		df.tmp = extractPtsFromRaster(ptdata, rasterfiles[n], band=bands[n], var.name=var.names[n], 
					interpolate=interpolate, windowsize=windowsizes[n], statistic=statistics[n], na.rm=na.rm)
		df.out = merge(df.out, df.tmp)
	}

	return(df.out)
}

rasterInfo <- function(srcfile) {
	
	src_ds <- tryCatch(
		rgdal::GDAL.open(srcfile, read.only=TRUE, silent=TRUE),
		
		error=function(err) {
			print(err)
			return(NULL)
		}
	)
	if (is.null(src_ds)) return(NULL)
	
	ri = list()
	ri$xsize = .Call("RGDAL_GetRasterXSize", src_ds, PACKAGE="rgdal")
	ri$ysize = .Call("RGDAL_GetRasterYSize", src_ds, PACKAGE="rgdal")
	gt = .Call("RGDAL_GetGeoTransform", src_ds, PACKAGE="rgdal")
	ri$geotransform = gt
	xmin = gt[1]
	xmax = xmin + gt[2] * ri$xsize
	ymax = gt[4]
	ymin = ymax + gt[6] * ri$ysize
	ri$bbox = c(xmin,ymin,xmax,ymax)
	ri$cellsize = c(gt[2], -gt[6])
	#ri$crs = .Call("RGDAL_GetProjectionRef", src_ds, PACKAGE="rgdal")
	ri$crs = rgdal::getProjectionRef(src_ds)
	ri$nbands = .Call("RGDAL_GetRasterCount", src_ds, PACKAGE="rgdal")
	ri$datatype = c()
	ri$nodata_value = c()
	for (b in 1:ri$nbands) {
		band = rgdal::getRasterBand(src_ds, b)
		dtNum = .Call("RGDAL_GetBandType", band, PACKAGE="rgdal")
		ri$datatype = c(ri$datatype, getGDALDataTypeName(dtNum))
		ri$nodata_value = c(ri$nodata_value, .Call("RGDAL_GetNoDataValue", band, PACKAGE="rgdal"))
	}
	rgdal::GDAL.close(src_ds)
	return(ri)
}

reprojectRaster <- function(srcfile, dstfile, t_srs, s_srs=NULL, of=NULL, ot=NULL,
							te=NULL, tr=NULL, r=NULL, 
							dstnodata=NULL, co=NULL, addOptions=NULL) {

# Wrapper of the sf interface to GDAL utils with arguments for common options.
# See full documentation for gdalwarp at gdal.org/programs/gdalwarp.html.

# Arguments that require multiple values should be given as a vector of values.
# For example, target resolution: tr=c(30,30)
# Arguments will be coerced to character.

#t_srs: <srs def> target spatial reference. EPSG:code, PROJ.4 declaration, or 
#	.prj file containing WKT. For example, PROJ.4:
#	t_srs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#of: <format> output raster format, e.g., HFA, GTiff, ...
#ot: <type> Force an output data type (i.e., Byte, Int16, ...)
#te: <xmin ymin xmax ymax> georeferenced extents of output file to be created.
#tr: <xres> <yres> output file resolution (in target georeferenced units)
#r: <resampling_method> Available methods are: near (the default), bilinear, 
#	cubic, cubicspline, lanczos, average, mode, min, max, med, q1, q3
#dstnodata: <value> Set nodata values for output raster. New files will be 
#	initialized to this value and if possible the nodata value will be recorded
#	in the output file. Use a value of None to ensure that nodata is not 
#	defined. If this argument is not used then nodata values will be copied 
#	from the source.
#co: <NAME=VALUE> Format-specific creation options 
#	(e.g., enable compression, see format driver documentation at www.gdal.org)
#addOptions: additional options. See gdalwarp documentation. Pass additional 
#	options as a character vector of command-line switches and their values, 
#	for example, addOptions=c("-multi","-wo","NUM_THREADS=ALL_CPUS")


	opt = c("-t_srs", t_srs)
	if (!is.null(s_srs)) {
		opt = c(opt, "-s_srs", s_srs)
  	}  
	if(!is.null(of)) {
		opt = c(opt, "-of", as.character(of))
	}
	if(!is.null(ot)) {
		opt = c(opt, "-ot", as.character(ot))
	}
	if(!is.null(te)) {
		opt = c(opt, "-te", as.character(te))
	}
	if(!is.null(tr)) {
		opt = c(opt, "-tr", as.character(tr))
	}
	if(!is.null(r)) {
		opt = c(opt, "-r", as.character(r))
	}
	if(!is.null(dstnodata)) {
		opt = c(opt, "-dstnodata", as.character(dstnodata))
	}
	if(!is.null(co)) {
		opt = c(opt, "-co", as.character(co))
	}
	opt = c(opt, "-overwrite")

	opt = c(opt, addOptions)
	#print(opt)
	
	return(sf::gdal_utils(util="warp", source=srcfile, destination=dstfile, options=opt))

}

rasterFromRaster <- function(srcfile, dstfile, fmt=NULL, nbands=NULL,
								dtName=NULL, options=NULL, init=NULL,
								dstnodata=init) {
# create a new blank raster (dst) using existing raster as template (src)
# coordinate system, extent and cell size taken from the src raster
# optionally change the format, number of bands, data type
# optinally pass driver-specific dataset creation options to GDAL
# optinally initialize to a value
	
	if (is.null(fmt)) {
		fmt = getGDALformat(dstfile)
		if (is.null(fmt)) {
			stop("Use fmt argument to specify a GDAL raster format code.")
		}
	}
	
	src_ds = rgdal::GDAL.open(srcfile, read.only=TRUE, silent=TRUE)
	if (is.null(nbands)) {
		nbands = .Call("RGDAL_GetRasterCount", src_ds, PACKAGE="rgdal")
	}
	nrows = .Call("RGDAL_GetRasterYSize", src_ds, PACKAGE="rgdal")
	ncols = .Call("RGDAL_GetRasterXSize", src_ds, PACKAGE="rgdal")
	gt = .Call("RGDAL_GetGeoTransform", src_ds, PACKAGE="rgdal")
	#xmin = gt[1]
	#xmax = xmin + gt[2] * ncols
	#ymax = gt[4]
	#ymin = ymax + gt[6] * nrows
	#cellsize = gt[2] #assuming square pixels
	if (is.null(dtName)) {
		b = rgdal::getRasterBand(src_ds)
		dtNum = .Call("RGDAL_GetBandType", b, PACKAGE="rgdal")
		dtName = getGDALDataTypeName(dtNum)
	}
	#srs = .Call("RGDAL_GetProjectionRef", src_ds, PACKAGE="rgdal")
	srs = getProjectionRef(src_ds)
	rgdal::GDAL.close(src_ds)

	drv = new("GDALDriver", fmt)
	dst_ds <- new("GDALTransientDataset", driver=drv, rows=nrows, cols=ncols, bands=nbands, type=dtName, options=options)
	.Call("RGDAL_SetGeoTransform", dst_ds, gt, PACKAGE="rgdal")
	.Call("RGDAL_SetProject", dst_ds, srs, PACKAGE="rgdal")
	if(!is.null(dstnodata)) {
		for (b in 1:nbands) {
			band = rgdal::getRasterBand(dst_ds, b)
			ret <- tryCatch(
				.Call("RGDAL_SetNoDataValue", band, dstnodata, PACKAGE="rgdal"),
				
				error=function(err) {
					print(err)
				}
			)
		}
	}
	rgdal::saveDataset(dst_ds, dstfile, options=options)
	rgdal::GDAL.close(dst_ds)

	if (!is.null(init)) {
		dst_ds = rgdal::GDAL.open(dstfile, read.only=FALSE, silent=TRUE)
		a <- array(init, dim=c(ncols, 1))
		print("Initializing destination raster...")
		for (b in 1:nbands) {
			for (r in 0:(nrows-1)) {
				rgdal::putRasterData(dst_ds, a, band=b, offset=c(r,0))
			}
		}
		rgdal::GDAL.close(dst_ds)
	}

	return(dstfile)
}

rasterFromVectorExtent <- function(src, dstfile, res, fmt=NULL, nbands=1,
								dtName="Int16", options=NULL, init=NULL,
								dstnodata=init) {
# create a new blank raster (dst) using existing vector layer as template
# src layer can be either an sf or Spatial object
# coordinate system and extent are taken from the src layer
# must specify the pixel resolution in layer CRS units
# dtName is often one of 'Byte','UInt16','Int16','UInt32','Int32','Float32'
# optinally pass driver-specific dataset creation options to GDAL
# optinally initialize to a value

	if(canCoerce(src, "sf")) {
		src = sf::st_as_sf(src)
	}
	else {
		stop("Cannot coerce src object to sf.")
	}
	
	if (is.null(fmt)) {
		fmt = getGDALformat(dstfile)
		if (is.null(fmt)) {
			stop("Use fmt argument to specify a GDAL raster format code.")
		}
	}
	
	ext = as.numeric(sf::st_bbox(src))
	srs = sf::st_crs(src)$proj4string
	src = NULL
	
	xmin = ext[1] - res
	ncols = ceiling((ext[3] - xmin) / res)
	xmax = xmin + res * ncols
	ymax = ext[4] + res
	nrows = ceiling((ymax - ext[2]) / res)
	ymin = ymax - res * nrows	
	gt = c(xmin, res, 0, ymax, 0, -res)

	drv = new("GDALDriver", fmt)
	dst_ds <- new("GDALTransientDataset", driver=drv, rows=nrows, cols=ncols, bands=nbands, type=dtName, options=options)
	.Call("RGDAL_SetGeoTransform", dst_ds, gt, PACKAGE="rgdal")
	.Call("RGDAL_SetProject", dst_ds, srs, PACKAGE="rgdal")
	if(!is.null(dstnodata)) {
		for (b in 1:nbands) {
			band = rgdal::getRasterBand(dst_ds, b)
			ret <- tryCatch(
				.Call("RGDAL_SetNoDataValue", band, dstnodata, PACKAGE="rgdal"),
				
				error=function(err) {
					print(err)
				}
			)
		}
	}
	rgdal::saveDataset(dst_ds, dstfile, options=options)
	rgdal::GDAL.close(dst_ds)

	if (!is.null(init)) {
		dst_ds = rgdal::GDAL.open(dstfile, read.only=FALSE, silent=TRUE)
		a <- array(init, dim=c(ncols, 1))
		print("Initializing destination raster...")
		for (b in 1:nbands) {
			for (r in 0:(nrows-1)) {
				rgdal::putRasterData(dst_ds, a, band=b, offset=c(r,0))
			}
		}
		rgdal::GDAL.close(dst_ds)
	}

	return(dstfile)
}

rasterizePolygons <- function(dsn, layer, burn_value, rasterfile, src=NULL) {
	# dsn, layer - a polygon layer (non-overlapping polygons assumed)
	# burn_value - an integer, or field name from layer (integer attribute)
	# rasterfile - existing raster for output, make with rasterFromRaster()

	ds = rgdal::GDAL.open(rasterfile, read.only=FALSE, silent=TRUE)
	nrows = .Call("RGDAL_GetRasterYSize", ds, PACKAGE="rgdal")
	ncols = .Call("RGDAL_GetRasterXSize", ds, PACKAGE="rgdal")
	gt = .Call("RGDAL_GetGeoTransform", ds, PACKAGE="rgdal")
	xmin = gt[1]
	xmax = xmin + gt[2] * ncols
	ymax = gt[4]
	ymin = ymax + gt[6] * nrows

	if (is.null(src)) {
		src <- rgdal::readOGR(dsn=dsn, layer=layer, stringsAsFactors=FALSE, verbose=FALSE)
	}
	else {
		if("sf" %in% class(src)) {
			src = sf::as_Spatial(src)
		}
	}
	
	burn_this = burn_value

	# write function for the C++ rasterizer
	writeRaster <- function(yoff, xoff1, xoff2, burn_value, attrib_value) {
		a <- array(burn_value, dim=(c((xoff2-xoff1)+1, 1)))
		rgdal::putRasterData(ds, a, band=1, offset=c(yoff,xoff1))
		return()
	}
	
	pb <- txtProgressBar(min=0, max=length(src))
	for (i in 1:length(src)) {
		if (!is.numeric(burn_value)) {
			# assume burn_value is a field name
			burn_this = as.numeric(src[[burn_value]][i])
		}
		part_sizes = vapply(src@polygons[i][[1]]@Polygons, function(p) nrow(p@coords), 0)
		coords = do.call(rbind, lapply(src@polygons[i][[1]]@Polygons, function(p) p@coords))
		grid_xs = vapply(coords[,1], getOffset, 0.0, origin=xmin, gt_pixel_size=gt[2])
		grid_ys = vapply(coords[,2], getOffset, 0.0, origin=ymax, gt_pixel_size=gt[6])
		RasterizePolygon(ncols, nrows, part_sizes, grid_xs, grid_ys, writeRaster, burn_this)
		setTxtProgressBar(pb, i)
	}
	close(pb)
	src <- NULL
	rgdal::GDAL.close(ds)
	
	invisible()
}


polygonizeRaster <- function(rasterfile, maskfile=character(0), options=character(0)) {

# GDALPolygonize via the Rcpp wrapper in package sf:
#Rcpp::List CPL_polygonize(Rcpp::CharacterVector raster, Rcpp::CharacterVector mask_name,
#		Rcpp::CharacterVector raster_driver, 
#		Rcpp::CharacterVector vector_driver, Rcpp::CharacterVector vector_dsn,
#		Rcpp::CharacterVector options, Rcpp::IntegerVector iPixValField,
#		Rcpp::CharacterVector contour_options, bool use_contours = false,
#		bool use_integer = true)

# rasterfile - thematic raster to polygonize based on adjacent pixels with same value
# maskfile - pixel value 0 masked out from polygonizing, nonzero values included
#	(maskfile can be same as rasterfile)
# options - "8CONNECTED=8" (4-connectedness is the default)
# returns an sf polygon layer with attribute "Value"
# see gdal.org/api/gdal_alg.html and gdal.org/programs/gdal_polygonize.html

	fmt = getGDALformat(rasterfile)
	vector_fmt = "ESRI Shapefile"
	dsn = tempfile("raster", fileext=".shp")
	fld_idx = 0
	
	print("Polygonizing...")
	x = .Call('_sf_CPL_polygonize', PACKAGE = 'sf', rasterfile, maskfile, 
			fmt, vector_fmt, dsn, options, fld_idx, character(0), F, T)
	geom = x[2]
	x = as.data.frame(x[-2], stringsAsFactors = F)
	x[["geometry"]] = sf::st_sfc(geom[[1]], crs = attr(geom[[1]], "crs"))
	return( sf::st_as_sf(x, sf_column_name="geometry") )
}


clipRaster <- function(dsn=NULL, layer=NULL, src=NULL, 
				srcfile, src_band=NULL, 
				dstfile, fmt=NULL, options=NULL, init=0,
				maskByPolygons=TRUE) {
# Clip a larger raster to the extent of polygon layer.
# Polygon layer from dsn/layer, or src if already open as Spatial object.
# srcfile - source raster (big raster)
# dstfile - destination raster (will be created)
# fmt - GDAL format string, if NULL will use format of srcfile if possible
# options - GDAL dataset creation options (driver-specific)
# init - initialize pixels to a background/nodata value
# Output extent will be set to maintain pixel alignment with src raster.
# Pixels in dst raster will be masked by polygons in the layer.

## TODO: handle nodata in the source raster
## TODO: add arg to set nodata value in dst raster

	if (is.null(fmt)) {
		fmt = getGDALformat(dstfile)
		if (is.null(fmt)) {
			stop("Use fmt argument to specify a GDAL raster format code.")
		}
	}

	if (is.null(src)) {
		src <- rgdal::readOGR(dsn=dsn, layer=layer, stringsAsFactors=FALSE, verbose=FALSE)
	}
	else {
		if("sf" %in% class(src)) {
			src = sf::as_Spatial(src)
		}
	}

	if (fmt == "VRT") {
		# handle VRT as a special case
		if (!is.null(src_band)) {
			message("individual band selection not supported for clipping to VRT.")
		}
		if (maskByPolygons) {
			message("maskByPolygons not available for clipping to VRT.")
		}
		rasterToVRT(srcfile=srcfile, vrtfile=dstfile, subwindow=src@bbox)
		return(invisible())
	}

	if (is.null(init) && maskByPolygons) {
		stop("init value must be specified when masking by polygons")
	}
	if (!is.null(init) && !maskByPolygons) {
		message("init value ignored when maskByPolygons=FALSE")
	}
	
	#open the source raster	
	src_ds = rgdal::GDAL.open(srcfile, read.only=TRUE, silent=TRUE)
	nrows = .Call("RGDAL_GetRasterYSize", src_ds, PACKAGE="rgdal")
	ncols = .Call("RGDAL_GetRasterXSize", src_ds, PACKAGE="rgdal")
	gt = .Call("RGDAL_GetGeoTransform", src_ds, PACKAGE="rgdal")
	xmin = gt[1]
	xmax = xmin + gt[2] * ncols
	ymax = gt[4]
	ymin = ymax + gt[6] * nrows
	cellsizeX = gt[2]
	cellsizeY = gt[6]

	#bounding box should be inside the raster extent
	#xmin, xmax: src@bbox[1,1], src@bbox[1,2]
	#ymin, ymax: src@bbox[2,1], src@bbox[2,2]
	if (src@bbox[1,1] < xmin || src@bbox[1,2] > xmax || src@bbox[2,1] < ymin || src@bbox[2,2] > ymax) {
		stop("polygon bounding box is not completely within source raster extent")
	}

	#srcwin offsets
	xminOff = floor(getOffset(src@bbox[1,1], xmin, cellsizeX))
	ymaxOff = floor(getOffset(src@bbox[2,2], ymax, cellsizeY))
	xmaxOff = ceiling(getOffset(src@bbox[1,2], xmin, cellsizeX))
	yminOff = ceiling(getOffset(src@bbox[2,1], ymax, cellsizeY))

	#lay out the clip raster so it is pixel-aligned with src raster
	clip_ncols = xmaxOff - xminOff
	clip_nrows = yminOff - ymaxOff
	clip_xmin = xmin + xminOff * cellsizeX
	clip_xmax = (xmin + xmaxOff * cellsizeX) + cellsizeX
	clip_ymax = ymax + ymaxOff * cellsizeY
	clip_ymin = (ymax + yminOff * cellsizeY) + cellsizeY
	clip_gt = c(clip_xmin, cellsizeX, 0, clip_ymax, 0, cellsizeY)

	if (is.null(src_band)) {
		nbands = .Call("RGDAL_GetRasterCount", src_ds, PACKAGE="rgdal")
		src_band = 1:nbands
	}
	else {
		nbands = 1
	}

	b = rgdal::getRasterBand(src_ds)
	dtNum = .Call("RGDAL_GetBandType", b, PACKAGE="rgdal")
	dtName = getGDALDataTypeName(dtNum)
	
	#srs = .Call("RGDAL_GetProjectionRef", src_ds, PACKAGE="rgdal")
	srs = getProjectionRef(src_ds)

	#create dst raster file
	drv = new("GDALDriver", fmt)
	dst_ds <- new("GDALTransientDataset", driver=drv, rows=clip_nrows, 
				cols=clip_ncols, bands=nbands, type=dtName, options=options)
	.Call("RGDAL_SetGeoTransform", dst_ds, clip_gt, PACKAGE="rgdal")
	.Call("RGDAL_SetProject", dst_ds, srs, PACKAGE="rgdal")
	rgdal::saveDataset(dst_ds, dstfile, options=options)
	rgdal::GDAL.close(dst_ds)
	dst_ds = rgdal::GDAL.open(dstfile, read.only=FALSE, silent=TRUE)
	
	#raster io function for the C++ rasterizer
	writeRaster <- function(yoff, xoff1, xoff2, burn_value, attrib_value) {
		for (b in 1:nbands) {
			a = rgdal::getRasterData(src_ds, band=src_band[b], 
									offset=c(yoff+ymaxOff,xoff1+xminOff), 
									region.dim=c(1,(xoff2-xoff1)+1), 
									as.is=TRUE)
			rgdal::putRasterData(dst_ds, a, band=b, offset=c(yoff,xoff1))
		}
		return()
	}
	
	if (maskByPolygons) {
		print("Initializing destination raster...")
		a <- array(init, dim=c(clip_ncols, 1))
		for (b in 1:nbands) {
			for (r in 0:(clip_nrows-1)) {
				rgdal::putRasterData(dst_ds, a, band=b, offset=c(r,0))
			}
		}
		print("Clipping to polygon layer...")
		pb <- txtProgressBar(min=0, max=length(src))
		for (i in 1:length(src)) {
			part_sizes = vapply(src@polygons[i][[1]]@Polygons, function(p) nrow(p@coords), 0)
			coords = do.call(rbind, lapply(src@polygons[i][[1]]@Polygons, function(p) p@coords))
			grid_xs = vapply(coords[,1], getOffset, 0.0, origin=clip_xmin, gt_pixel_size=clip_gt[2])
			grid_ys = vapply(coords[,2], getOffset, 0.0, origin=clip_ymax, gt_pixel_size=clip_gt[6])
			RasterizePolygon(clip_ncols, clip_nrows, part_sizes, grid_xs, grid_ys, writeRaster, 0)
			setTxtProgressBar(pb, i)
		}
		close(pb)
	}
	else {
		print("Clipping to polygon layer extent...")
		lapply(c(0:(clip_nrows-1)), writeRaster, xoff1=0, xoff2=clip_ncols-1, 0)
	}
	print(paste("clipRaster output written to:", dstfile))
	
	src <- NULL
	rgdal::GDAL.close(src_ds)
	rgdal::GDAL.close(dst_ds)
	
	invisible(dstfile)
}


rasterCalc <- function(calc, rasterfiles, bands=NULL, var.names=NULL,
						dstfile=tempfile("rastcalc", fileext=".img"),
						fmt=NULL, dtName="Int16", options=NULL,
						nodata_value=NULL, setRasterNodataValue=FALSE,
						usePixelLonLat=FALSE) {
						
#Raster calculater
#Evaluate an R expression for each pixel in a raster layer or stack of layers.

#Layers are defined by: raster file, band, variable name.
#Band defaults to 1 for each file in rasterfiles.
#Variable names default to "A" (layer 1), "B" (layer 2), ..., but
#can be set in var.names.

#To refer to specific bands in a multi-band file, repeat the file name in 
#rasterfiles and specify corresponding band numbers in bands, along with
#optional variable names in var.names, for example,
#rasterfiles=c("file.img", "file.img"), bands=c(3,4), var.names=c("b3", "b4")

#calc - R expression as character (e.g., calc="A+B")
#Variables in calc are vectors of length n (rows of raster). Calc expresion
#should return a vector also of length n (an output row).
#Two special variable names are available in the calc expression by default:
#"pixelX" and "pixelY" provide the pixel center coordinate in georeferenced
#units. If usePixelLonLat=TRUE, the pixel XY coordinates will also be inverse
#projected to geographical coordinates and available for use in calc as
#"pixelLon" and "pixelLat".

#rasterfiles - character vector of raster file names
#bands - integer vector of band numbers
#var.names - character vector of variable names
#dstfile - destination raster (will be created)
#fmt - GDAL raster format string, guessed from file extension if possible
#dtName - GDAL data type name (Byte, Int16, UInt16, Int32, UInt32, Float32)
#options - GDAL dataset creation options (driver-specific)
#nodata_value - value to assign if calc returns an NA value
#setRasterNodataValue - attempt to set the raster format nodata = nodata_value
#usePixelLonLat = pixelX and pixelY will be inverse projected to geographic
#coordinates and available as "pixelLon" and "pixelLat" in calc expression.

	
	calc_expr = parse(text=calc)

	if ( !all(file.exists(rasterfiles)) ) {
		print( rasterfiles[which(!file.exists(rasterfiles))] )
		stop("file not found")
	}
	
	nrasters = length(rasterfiles)

	if (!is.null(bands)) {
		if (length(bands) != nrasters) {
			stop("list of band numbers must be same length as raster list")
		}
	}
	else {
		bands = rep(1, nrasters)
	}
	
	if (!is.null(var.names)) {
		if (length(var.names) != nrasters) {
			stop("list of variable names must be same length as raster list")
		}
	}
	else {
		var.names=LETTERS[1:nrasters]
	}
	
	if (is.null(fmt)) {
		fmt = getGDALformat(dstfile)
		if (is.null(fmt)) {
			stop("Use fmt argument to specify a GDAL raster format code.")
		}
	}
	
	if (is.null(nodata_value)) {
		nodata_value = getDefaultNodata(dtName)
		if (is.null(nodata_value)) {
			stop("Invalid output data type (dtName).")
		}
	}
	
	# use first raster as reference
	ref = rasterInfo(rasterfiles[1])
	if(is.null(ref)) stop(paste("Could not read raster info:", rasterfiles[1]))
	nrows = ref$ysize
	ncols = ref$xsize
	cellsizeX = ref$cellsize[1]
	cellsizeY = ref$cellsize[2]
	xmin = ref$bbox[1]
	ymax = ref$bbox[4]
	
	if(nrasters > 1) {
		for(r in rasterfiles) {
			ri = rasterInfo(r)
			if(is.null(ri)) stop(paste("Raster info failed:", rasterfiles[r]))
			if(ri$ysize != nrows || ri$xsize != ncols) {
				print(rasterfiles[r])
				stop("All input rasters must have the same dimensions.")
			}
		}
	}
	
	#create the output raster
	dstnodata = NULL
	if(setRasterNodataValue) dstnodata = nodata_value
	rasterFromRaster(rasterfiles[1], dstfile, fmt=fmt, nbands=1, dtName=dtName,
						options=options, dstnodata=dstnodata)
	dst_ds = rgdal::GDAL.open(dstfile, read.only=FALSE, silent=TRUE)
	
	# list of GDAL dataset objects for each raster
	gd_list <- list()
	for (r in 1:nrasters) {
		gd_list[[r]] <- rgdal::GDAL.open(rasterfiles[r], read.only=TRUE, silent=FALSE)
	}
	
	x = seq(from=xmin+(cellsizeX/2), by=cellsizeX, length.out=ncols)
	assign("pixelX", x)
	
	process_row <- function(row) {
		y = rep(ymax-(cellsizeY/2)-(cellsizeY*row), ncols)
		assign("pixelY", y)
		
		if(usePixelLonLat) {
			lonlat = rgdal::project(cbind(x,y), ref$crs, inv=TRUE)
			assign("pixelLon", lonlat[,1])
			assign("pixelLat", lonlat[,2])
		}
		
		for (r in 1:nrasters) {
			inrow = rgdal::getRasterData(gd_list[[r]], band=bands[r], 
					offset=c(row,0), region.dim=c(1,ncols), as.is=TRUE)
			assign(var.names[r], inrow)
		}
		
		outrow = eval(calc_expr)
		outrow = ifelse(is.na(outrow), nodata_value, outrow)
		rgdal::putRasterData(dst_ds, outrow, band=1, offset=c(row,0))
		
		setTxtProgressBar(pb, row+1)
		return()
	}
	
	print(paste("Calculating from", nrasters, "input layers..."))
	pb <- txtProgressBar(min=0, max=nrows)
	lapply(0:(nrows-1), process_row)
	close(pb)

	print(paste("Output written to:", dstfile))
	rgdal::GDAL.close(dst_ds)
	lapply(gd_list, rgdal::GDAL.close)
	
	invisible(dstfile)
}


rasterCombine <- function(rasterfiles, var.names=NULL, bands=NULL, 
					dstfile=NULL, fmt=NULL, dtName="UInt32", options=NULL) {
# find and count unique combinations of pixel values across a set of thematic rasters
# returns a data frame of combination ID, count, var1, var2, ...
# all input rasters are assumed to have the same coordinate system, extent and cell size
# input rasters will be read as integer
# optionally write the combination IDs to an output raster
# output data type should be Byte, UInt16, or UInt32, appropriate for number of combinations

	if (!is.null(dstfile) && is.null(fmt)) {
		fmt = getGDALformat(dstfile)
		if (is.null(fmt)) {
			stop("Use fmt argument to specify a GDAL raster format code.")
		}
	}
	
	nrasters = length(rasterfiles)
	if (is.null(var.names)) {
		var.names = vapply(1:nrasters, function(n) paste0("v",n), "v#")
	}
	if (is.null(bands)) {
		bands = rep(1,nrasters)
	}
	
	raster_lut <- data.frame(rasterfiles,var.names,bands,check.names=FALSE,stringsAsFactors=FALSE)

	# open first raster and get extent, cell size, srs
	ds = rgdal::GDAL.open(raster_lut[1,1], read.only=TRUE, silent=TRUE)
	nrows = .Call("RGDAL_GetRasterYSize", ds, PACKAGE="rgdal")
	ncols = .Call("RGDAL_GetRasterXSize", ds, PACKAGE="rgdal")
	gt = .Call("RGDAL_GetGeoTransform", ds, PACKAGE="rgdal")
	#xmin = gt[1]
	#xmax = xmin + gt[2] * ncols
	#ymax = gt[4]
	#ymin = ymax + gt[6] * nrows
	cellsize = gt[2] #assuming square pixels
	#srs = .Call("RGDAL_GetProjectionRef", ds, PACKAGE="rgdal")
	srs = getProjectionRef(ds)
	rgdal::GDAL.close(ds)

	# list of GDAL dataset objects for each raster	
	gd_list <- list()
	for (r in 1:nrasters) {
		gd_list[[as.character(raster_lut[r,2])]] <- rgdal::GDAL.open(raster_lut[r,1], read.only=TRUE, silent=FALSE)
	}

	if (!is.null(dstfile)) {
		# create an output raster for combination ids
		drv = new("GDALDriver", fmt)
		dst_ds <- new("GDALTransientDataset", driver=drv, rows=nrows, cols=ncols, bands=1, type=dtName, options=options)
		.Call("RGDAL_SetGeoTransform", dst_ds, gt, PACKAGE="rgdal")
		.Call("RGDAL_SetProject", dst_ds, srs, PACKAGE="rgdal")
		rgdal::saveDataset(dst_ds, dstfile, options=options)
		rgdal::GDAL.close(dst_ds)
		dst_ds = rgdal::GDAL.open(dstfile, read.only=FALSE, silent=TRUE)
	}

	# CmbTable to count the unique combinations
	tbl = new(CmbTable, keyLen=nrasters, varNames=var.names)

	rowdata = matrix(NA_integer_, nrow = nrasters, ncol = ncols)
	
	process_row <- function(row) {
		for (r in 1:nrasters) {
			b <- raster_lut[r,3]
			rowdata[r,] = as.integer(rgdal::getRasterData(gd_list[[r]], band=b, 
										offset=c(row,0), 
										region.dim=c(1,ncols), 
										as.is=TRUE))
		}
		row_cmbid = tbl$updateFromMatrix(rowdata, 1)
		if (!is.null(dstfile)) {
			rgdal::putRasterData(dst_ds, row_cmbid, band=1, offset=c(row,0))
		}
		setTxtProgressBar(pb, row+1)
		return()
	}

	if(nrasters == 1) {
		print("Scanning raster...")
	}
	else {
		print(paste("Combining", nrasters, "input files..."))
	}
	pb <- txtProgressBar(min=0, max=nrows)
	lapply(0:(nrows-1), process_row)
	close(pb)
	
	df_out <- tbl$asDataFrame()
	tbl <- NULL
	
	lapply(gd_list, rgdal::GDAL.close)
	if (!is.null(dstfile)) {
		print(paste("Combination IDs written to:", dstfile))
		rgdal::GDAL.close(dst_ds)
	}
	
	return(df_out)
}

recodeRaster <- function(srcfile, dstfile, lut, srcband=1, ...) {
#Recode a raster by values in a lookup table
#srcfile - filename of source raster
#lut - dataframe with two columns containing original value, new value
#dstfile - output raster file, will be created
#... additional arguments passed to rasterFromRaster (e.g., change data type)
# rasterFromRaster <- function(srcfile, dstfile, fmt=NULL, nbands=NULL,
								# dtName=NULL, options=NULL, init=NULL,
								# dstnodata=init) {
#Raster values not in the lookup table will be brought through unchanged.
#Raster data assumed to be integer.

	#open the source raster	
	src_ds = rgdal::GDAL.open(srcfile, read.only=TRUE, silent=TRUE)
	nrows = .Call("RGDAL_GetRasterYSize", src_ds, PACKAGE="rgdal")
	ncols = .Call("RGDAL_GetRasterXSize", src_ds, PACKAGE="rgdal")
	
	#create the destination raster
	rasterFromRaster(srcfile, dstfile, nbands=1, ...)
	dst_ds = rgdal::GDAL.open(dstfile, read.only=FALSE, silent=TRUE)
	
	process_row <- function(row) {
		a = as.integer(rgdal::getRasterData(src_ds, band=srcband, 
						offset=c(row,0), 
						region.dim=c(1,ncols), 
						as.is=TRUE))
		a2 = lut[,2][match(a, lut[,1])]
		a2 = ifelse(is.na(a2), a, a2)
		rgdal::putRasterData(dst_ds, a2, band=1, offset=c(row,0))
		setTxtProgressBar(pb, row+1)
		return()
	}

	print("Recoding...")
	pb <- txtProgressBar(min=0, max=nrows)
	lapply(0:(nrows-1), process_row)
	close(pb)
	
	print(paste("Output written to:", dstfile))
	rgdal::GDAL.close(dst_ds)
	rgdal::GDAL.close(src_ds)
	
	invisible(dstfile)
}

pixelCount <- function(rasterfile) {
#Convenience function to get pixel counts from one raster using rasterCombine
#Scans the whole raster

	df = rasterCombine(rasterfile)
	out = df[,c(3,2)]
	names(out) = c("value","count")
	out = out[order(out$value),]
	row.names(out) = NULL
	return(out)
}


focalRaster <- function(srcfile, dstfile, w, fun=sum, na.rm=FALSE, ...,
						fmt=NULL, dtName=NULL, options=NULL,
						nodata_value=NULL, setRasterNodataValue=FALSE,
						srcband=NULL) {

	ref = rasterInfo(srcfile)
	if(is.null(ref)) stop(paste("Could not read raster info:", srcfile))
	nrows = ref$ysize
	ncols = ref$xsize
	if (is.null(dtName)) dtName=ref$datatype[1]
	if (is.null(nodata_value)) {
		nodata_value = getDefaultNodata(dtName)
		if (is.null(nodata_value)) {
			stop("Invalid output data type (dtName).")
		}
	}
	nbands = ifelse(is.null(srcband), ref$nbands, 1)
	
	# kernel info
	if (!is.matrix(w)) {
		stop("kernel must be a square matrix with odd-number dimensions.")
	}
	if (dim(w)[1] != dim(w)[2]) {
		stop("kernel must be a square matrix with odd-number dimensions.")
	}
	kernelsize = dim(w)[1]
	if ((kernelsize %% 2) == 0) {
		stop("kernel must be a square matrix with odd-number dimensions.")
	}
	
	N = as.integer( (kernelsize - 1) / 2 )
	
	if(kernelsize > nrows || kernelsize > ncols) {
		stop("kernel must be smaller than raster size")
	}
	
	#create the output raster
	dstnodata = NULL
	if(setRasterNodataValue) dstnodata = nodata_value
	rasterFromRaster(srcfile, dstfile, fmt=fmt, nbands=nbands, dtName=dtName,
						options=options, dstnodata=dstnodata)
	dst_ds = rgdal::GDAL.open(dstfile, read.only=FALSE, silent=TRUE)
	
	# source dataset
	src_ds = rgdal::GDAL.open(srcfile, read.only=TRUE, silent=FALSE)

	# raster input buffer for nrows = kernelsize
	# N marginal columns contain NA for when kernel is outside the raster
	rowdata = matrix(NA_real_, nrow = kernelsize, ncol = ncols+2*N)
	# define columns of the data region in rowdata (columns inside the raster)
	rowdata.cols = (1+N):(ncols+N)
	
	process_row <- function(row) {
		for (b in 1:nbands) {
			# start/end row numbers for raster input
			inrow.start = row-N
			inrow.end = row+N
			
			if (row == 0) {
				# fully populate the input buffer
				i = 1
				for (this.row in inrow.start:inrow.end) {
					if (this.row >= 0) {
						rowdata[i,rowdata.cols] <<- rgdal::getRasterData(
										src_ds, band=b, 
										offset=c(this.row,0), 
										region.dim=c(1,ncols), as.is=T)
					}
					i = i+1
				}
			}
			else {
				# update the input buffer adding one new row
				for (i in 1:(kernelsize-1)) {
					rowdata[i,] <<- rowdata[(i+1),]
				}
				if (inrow.end > (nrows-1)) {
					# outside the raster
					rowdata[kernelsize,] <<- NA_real_
				}
				else {
					rowdata[kernelsize,rowdata.cols] <<- rgdal::getRasterData(
											src_ds, band=b, 
											offset=c(inrow.end,0), 
											region.dim=c(1,ncols), as.is=T)
				}
			}
			
			# move the kernel across rowdata and apply fun
			outrow = vapply(1:ncols, 
						function(p, ...) {fun((rowdata[,p:(p+kernelsize-1)] * w), ...)},
						0, na.rm=na.rm, ...)
			outrow = ifelse(is.na(outrow), nodata_value, outrow)
			
			# write a row of output
			rgdal::putRasterData(dst_ds, outrow, band=b, offset=c(row,0))
			
			setTxtProgressBar(pb, row+1)
			return()
		}
	}
	
	print("Calculating focal raster...")
	pb <- txtProgressBar(min=0, max=nrows)
	lapply(0:(nrows-1), process_row)
	close(pb)

	print(paste("Output written to:", dstfile))
	rgdal::GDAL.close(dst_ds)
	rgdal::GDAL.close(src_ds)
	
	invisible(dstfile)
}


zonalStats <- function(dsn=NULL, layer=NULL, src=NULL, attribute, 
						rasterfile, band = 1, lut=NULL, pixelfun=NULL, 
						na.rm=TRUE, ignoreValue=NULL, resampling = "nearest") {
# zoneid, npixels, mean, min, max, sum, sampVar, sampSD, popVar, popSD
# TODO: raster zones/resampling is not complete yet

	ds = rgdal::GDAL.open(rasterfile, read.only=TRUE, silent=TRUE)
	nrows = .Call("RGDAL_GetRasterYSize", ds, PACKAGE="rgdal")
	ncols = .Call("RGDAL_GetRasterXSize", ds, PACKAGE="rgdal")
	gt = .Call("RGDAL_GetGeoTransform", ds, PACKAGE="rgdal")
	xmin = gt[1]
	xmax = xmin + gt[2] * ncols
	ymax = gt[4]
	ymin = ymax + gt[6] * nrows
	
	raster_zones = F
	if (is.null(src)) {
		if (is.null(layer)) {
			# dsn should be a zonal raster file
			ri = rasterInfo(dsn)
			if(!is.null(ri)) {
				raster_zones = T
			}
			else {
				stop("Could not read dsn as a raster source.")
			}	
		}
		else {
			# dsn, layer should be a polygon data source
			src <- rgdal::readOGR(dsn=dsn, layer=layer, stringsAsFactors=FALSE, verbose=FALSE)
		}
	}
	else {
		# src should be sf or Spatial object
		if("sf" %in% class(src)) {
			src = sf::as_Spatial(src)
		}
	}
	
	if(raster_zones) {
		# using zonal raster...
		# TODO: this is not complete
		rs_list = list()
		
		updateRunningStats <- function(x) {
			zoneid = as.character(x[1])
			invisible( rs_list[[zoneid]]$update(x[2]) )
		}
		
		rowdata = matrix(NA_real_, nrow = 2, ncol = ncols)
		
		process_row <- function(row) {
			# read the zonal raster
			rowdata[1,] = rgdal::getRasterData(zone_ds, band=1, 
										offset=c(row,0), 
										region.dim=c(1,ncols), 
										as.is=TRUE)
			# read the data raster
			rowdata[2,] = rgdal::getRasterData(ds, band=band, 
										offset=c(row,0), 
										region.dim=c(1,ncols), 
										as.is=TRUE)
										
			# update running stats
			apply(rowdata, c(2), updateRunningStats)
			setTxtProgressBar(pb, row+1)
			return()
		}
		pb <- txtProgressBar(min=0, max=nrows)
		lapply(0:(nrows-1), process_row)
		close(pb)		
		
		zoneid = names(rs_list)

	}
	else {
		zoneid = unique(as.character(src[[attribute]]))
		
		# list of RunningStats objects for the zones
		rs_list = list()
		for (z in zoneid) {
			rs_list[[z]] = new(RunningStats, na_rm_in=na.rm)
		}

		# raster io function for RasterizePolygon
		readRaster <- function(yoff, xoff1, xoff2, burn_value, attrib_value) {
			a <- rgdal::getRasterData(ds, band=band, offset=c(yoff,xoff1), 
									region.dim=c(1,(xoff2-xoff1)+1), 
									as.is=TRUE)
			if (!is.null(ignoreValue)) a = a[a!=ignoreValue]
			if (!is.null(lut)) {
				a2 = lut[,2][match(a, lut[,1])]
				a = ifelse(is.na(a2), a, a2)
			}
			if (!is.null(pixelfun)) a = pixelfun(a)
			rs_list[[attrib_value]]$update(a)
			return()
		}

		pb <- txtProgressBar(min=0, max=length(src))
		for (i in 1:length(src)) {
			this_attr = as.character(src[[attribute]][i])
			part_sizes = vapply(src@polygons[i][[1]]@Polygons, function(p) nrow(p@coords), 0)
			coords = do.call(rbind, lapply(src@polygons[i][[1]]@Polygons, function(p) p@coords))
			grid_xs = vapply(coords[,1], getOffset, 0.0, origin=xmin, gt_pixel_size=gt[2])
			grid_ys = vapply(coords[,2], getOffset, 0.0, origin=ymax, gt_pixel_size=gt[6])
			RasterizePolygon(ncols, nrows, part_sizes, grid_xs, grid_ys, readRaster, NA, this_attr)
			setTxtProgressBar(pb, i)
		}
		close(pb)
	}
	
	npixels = rep(0, length(zoneid))
	zone.stats = data.frame(zoneid, npixels, stringsAsFactors=FALSE)
	zone.stats$mean = rep(NA_real_, length(zoneid))
	zone.stats$min = rep(NA_real_, length(zoneid))
	zone.stats$max = rep(NA_real_, length(zoneid))
	zone.stats$sum = rep(NA_real_, length(zoneid))
	zone.stats$sampVar = rep(NA_real_, length(zoneid))
	zone.stats$popVar = rep(NA_real_, length(zoneid))
	for (z in zoneid) {
		zone.stats$npixels[zone.stats$zoneid==z] = rs_list[[z]]$get_count()
		zone.stats$mean[zone.stats$zoneid==z] = rs_list[[z]]$get_mean()
		zone.stats$min[zone.stats$zoneid==z] = rs_list[[z]]$get_min()
		zone.stats$max[zone.stats$zoneid==z] = rs_list[[z]]$get_max()
		zone.stats$sum[zone.stats$zoneid==z] = rs_list[[z]]$get_sum()
		zone.stats$sampVar[zone.stats$zoneid==z] = rs_list[[z]]$get_sampVar()
		zone.stats$popVar[zone.stats$zoneid==z] = rs_list[[z]]$get_popVar()
	}
	zone.stats$sampSD = sqrt(zone.stats$sampVar)
	zone.stats$popSD = sqrt(zone.stats$popVar)

	for (z in zoneid) {
		rs_list[[z]] <- NULL
	}
	rs_list <- NULL
	src <- NULL
	rgdal::GDAL.close(ds)
	#return(zone.stats[order(zone.stats$zoneid),])
	return(zone.stats)
}

zonalMean <- function(dsn=NULL, layer=NULL, src=NULL, attribute, 
						rasterfile, band = 1, 
						lut=NULL, pixelfun=NULL, na.rm=TRUE, ...) {

	zone.stats = zonalStats(dsn=dsn, layer=layer, src=src, attribute=attribute,
							rasterfile=rasterfile, band=band, 
							lut=lut, pixelfun=pixelfun, na.rm=na.rm, ...)
	return(zone.stats[,1:3])
}

zonalFreq <- function(dsn=NULL, layer=NULL, src=NULL, attribute, 
						rasterfile, band=1, aggfun=NULL, lut=NULL,
						na.rm=FALSE, ignoreValue=NULL) {
	# aggfun is an aggregate function applied to the counts by 
	# zoneid, like max for majority value

	ds = rgdal::GDAL.open(rasterfile, read.only=TRUE, silent=TRUE)
	nrows = .Call("RGDAL_GetRasterYSize", ds, PACKAGE="rgdal")
	ncols = .Call("RGDAL_GetRasterXSize", ds, PACKAGE="rgdal")
	gt = .Call("RGDAL_GetGeoTransform", ds, PACKAGE="rgdal")
	xmin = gt[1]
	xmax = xmin + gt[2] * ncols
	ymax = gt[4]
	ymin = ymax + gt[6] * nrows

	if (is.null(src)) {
		src <- rgdal::readOGR(dsn=dsn, layer=layer, stringsAsFactors=FALSE, verbose=FALSE)
	}
	else {
		if("sf" %in% class(src)) {
			src = sf::as_Spatial(src)
		}
	}
	
	zoneid = unique(as.character(src[[attribute]]))
	
	# CmbTable to count the unique combinations
	tbl = new(CmbTable, keyLen=2, varNames=c("idx","value"))

	# raster io function for RasterizePolygon
	readRaster <- function(yoff, xoff1, xoff2, burn_value, attrib_value) {
		rowlength <- (xoff2-xoff1)+1
		rowdata <- matrix(NA_integer_, nrow = 2, ncol = rowlength)
		rowdata[1,] <- rep(burn_value, rowlength)
		rowdata[2,] <- as.integer(rgdal::getRasterData(ds, band=band, 
									offset=c(yoff,xoff1), 
									region.dim=c(1,rowlength), 
									as.is=TRUE))
		if (!is.null(lut)) {
			a2 = lut[,2][match(rowdata[2,], lut[,1])]
			rowdata[2,] = ifelse(is.na(a2), rowdata[2,], a2)
		}
		tbl$updateFromMatrix(rowdata, 1)
		return()
	}

	pb <- txtProgressBar(min=0, max=length(src))
	for (i in 1:length(src)) {
		this_attr = as.character(src[[attribute]][i])
		this_attr_idx = match(this_attr,zoneid)
		part_sizes = vapply(src@polygons[i][[1]]@Polygons, function(p) nrow(p@coords), 0)
		coords = do.call(rbind, lapply(src@polygons[i][[1]]@Polygons, function(p) p@coords))
		grid_xs = vapply(coords[,1], getOffset, 0.0, origin=xmin, gt_pixel_size=gt[2])
		grid_ys = vapply(coords[,2], getOffset, 0.0, origin=ymax, gt_pixel_size=gt[6])
		RasterizePolygon(ncols, nrows, part_sizes, grid_xs, grid_ys, readRaster, this_attr_idx)
		setTxtProgressBar(pb, i)
	}
	close(pb)
	
	df_out = tbl$asDataFrame()
	tbl <- NULL
	df_out$cmbid = NULL
	df_out$zoneid = zoneid[df_out$idx]
	df_out$idx = NULL
	firstcols = c("zoneid","value")
	df_out <- df_out[, c(firstcols, setdiff(names(df_out), firstcols))]
	if(na.rm) df_out = df_out[!is.na(df_out$value),]
	if(!is.null(ignoreValue)) df_out = df_out[!(df_out$value %in% ignoreValue),]
	if (!is.null(aggfun)){
		df_agg <- aggregate(count ~ zoneid, df_out, aggfun)
		df_out <- merge(df_agg, df_out)
	}
	else {
		df_out <- transform(df_out, zoneprop = ave(count, zoneid, FUN = function(x) round(x/sum(x), 4)))
	}
	
	src <- NULL
	rgdal::GDAL.close(ds)
	
	#return( df_out[with(df_out, order(zoneid, -count)), ] )
	df_out = df_out[with(df_out, order(zoneid, -count)), ]
	row.names(df_out) = NULL
	return(df_out)
}

zonalMajority <- function(dsn=NULL, layer=NULL, src = NULL, attribute, 
							rasterfile, band = 1, lut=NULL, ...) {
	return( zonalFreq(dsn=dsn, layer=layer, src=src, attribute=attribute, 
							rasterfile=rasterfile, band=band, 
							aggfun = max, lut=lut, ...) )
}

zonalMinority <- function(dsn=NULL, layer=NULL, src = NULL, attribute, 
							rasterfile, band = 1, lut=NULL, ...) {
	return( zonalFreq(dsn=dsn, layer=layer, src=src, attribute=attribute, 
							rasterfile=rasterfile, band=band, 
							aggfun = min, lut=lut, ...) )
}

zonalVariety <- function(dsn=NULL, layer=NULL, src = NULL, attribute, 
							rasterfile, band = 1, lut=NULL, ...) {
							
	zf = zonalFreq(dsn=dsn, layer=layer, src=src, attribute=attribute, 
							rasterfile=rasterfile, band=band, lut=lut, ...)
							
	df_out =  aggregate(value ~ zoneid, zf, length)
	colnames(df_out)[2] = "number_of_unique_values"
	return(df_out)
}

ptCsvToVRT <- function(csvfile, layer_srs, xfield="Lon", yfield="Lat", readvrt=TRUE) {
# csvfile - full path to a point csv file, first row containing field names
# layer_srs - spatial reference for the point layer. The value may be
#				WKT, PROJ.4 definitions, or any other input accepted by
#				OGRSpatialReference::SetFromUserInput() method. For example,
#				"WGS84", "NAD27", "NAD83",
#				"EPSG:n" (where n is an EPSG code)
#				"EPSG:5070" (NAD83 CONUS Albers USGS version),
#				"EPSG:5069" (NAD27 CONUS Albers USGS version)
#				(see additional notes below for geographical coordinates)
# readvrt - read the VRT and return SpatialPointsDataFrame (from rgdal::readOGR)

	# write a virtual OGR data source for the CSV point file to support srs
	vrtfile = file.path(dirname(csvfile), paste0(basename.NoExt(csvfile), ".vrt"))
	filecon = file(vrtfile, "w+")
	writeLines("<OGRVRTDataSource>", filecon)
	txt = paste0("\t<OGRVRTLayer name='", basename.NoExt(csvfile), "'>")
	writeLines(txt, filecon)
	txt = paste0("\t\t<SrcDataSource relativeToVRT='1'>", basename(csvfile), "</SrcDataSource>")
	#txt = paste0("\t\t<SrcDataSource>", gsub("\\\\", "/", csvfile), "</SrcDataSource>")
	writeLines(txt, filecon)
	txt = paste0("\t\t<GeometryType>wkbPoint</GeometryType>")
	writeLines(txt, filecon)
	txt = paste0("\t\t<LayerSRS>", layer_srs, "</LayerSRS>")
	writeLines(txt, filecon)
	txt = paste0("\t\t<GeometryField encoding='PointFromColumns' x='", xfield, "' y='", yfield, "'/>")
	writeLines(txt, filecon)
	writeLines("\t</OGRVRTLayer>", filecon)
	writeLines("</OGRVRTDataSource>", filecon)
	close(filecon)
	
	if (readvrt) {
		ptdf <- rgdal::readOGR(dsn=vrtfile, layer=basename.NoExt(vrtfile), 
								stringsAsFactors=FALSE, verbose=TRUE)
		# readOGR does not reliably assign the spatial reference for longlat coords other than WGS84.
		# This may be related to an issue described in the documentation for CRS-class in package sp:
		# "Note that only "+proj=longlat +ellps=WGS84" is accepted for geographical coordinates, 
		# which must be ordered (eastings, northings); the "+ellps=" definition must be given (or
		# expanded internally from a given "+datum=" value) for recent versions of the PROJ.4 library,
		# and should be set to an appropriate value."

		# fix the proj4 string for common longlat cases:
		if (layer_srs=="NAD83") {
			sp::proj4string(ptdf) <- sp::CRS("+init=epsg:4269")
		}
		else if (layer_srs=="NAD27") {
			sp::proj4string(ptdf) <- sp::CRS("+init=epsg:4267")
		}	
	}
	else {
		ptdf = NULL
	}
	
	return(ptdf)
}


### No longer needed, using sf::st_read: ###
#readOGRfromSQL <- function(dsn, sql, dialect=NULL, stringsAsFactors=FALSE, ...) {
## A workaround since rgdal::readOGR does not support SQL query
## dsn - Full path to a file data source, or a connection string.
## sql - An SQL statement to execute to generate the desired layer result.
## dialect - SQL "dialect" to use: currently OGRSQL or SQLITE. 
##			If dialect is not specified, the default dialect of the datasource 
##			will be used. Useful to query a non-SpatiaLite datasource using 
##			SpatiaLite SQL. GDAL must be compiled with SQLite and SpatiaLite.
## stringsAsFactors - passed to rgdal::readOGR
## ... additional arguments passed to rgdal::readOGR
## Returns a Spatial object from rgdal::readOGR.

#	tmpvrt = tempfile("ogrsql", fileext=".vrt")
#	filecon = file(tmpvrt, "w+")
#	writeLines("<OGRVRTDataSource>", filecon)
#	writeLines("\t<OGRVRTLayer name='tmpSrcSQL'>", filecon)
#	txt = paste0("\t\t<SrcDataSource>", dsn, "</SrcDataSource>")
#	writeLines(txt, filecon)
#	if (is.null(dialect)) {
#		txt = paste0("\t\t<SrcSQL>", sql, "</SrcSQL>")
#	}
#	else {
#		txt = paste0("\t\t<SrcSQL dialect='", dialect, "'>", sql, "</SrcSQL>")
#	}
#	writeLines(txt, filecon)
#	writeLines("\t</OGRVRTLayer>", filecon)
#	writeLines("</OGRVRTDataSource>", filecon)
#	close(filecon)
#	rgdal::readOGR(dsn=tmpvrt, layer='tmpSrcSQL', stringsAsFactors=stringsAsFactors, ...)
#}


#' Write a GDAL virtual raster file (VRT)
#'
#' Write a GDAL VRT file for a source raster with options for repositioning and
#' and resampling the source data at a different pixel resolution
#'
#' \code{rasterToVRT} is useful for virtually clipping a raster to a subwindow,
#' or virtually resampling at a different pixel resolution. The output VRT file
#' will have the same coordinate system as the source raster.
#'
#' @param srcfile Source raster file name.
#' @param relativeToVRT Integer. Should \code{srcfile} be interpreted as 
#' 	relative to the .vrt file (value is 1) or not relative to the .vrt file
#' 	(value is 0)? If value is 1, the .vrt file is assumed to be in the same 
#' 	directory as \code{srcfile} and \code{basename(srcfile)} is used in .vrt.
#' @param vrtfile Output VRT file name.
#' @param resolution A numeric vector of length two, with xres, yres. The pixel
#' size must be expressed in georeferenced units. Both must be positive values.
#' The source pixel size is used if resolution is not specified.
#' @param subwindow A numeric vector of length four, with xmin, ymin, xmax and 
#' 	ymax values (e.g., sp::bbox or sf::st_bbox). Selects a subwindow of the 
#'	source raster with corners given in georeferenced coordinates (in the 
#'	source CRS). If not given, the upper left corner of the VRT will be the 
#'	same as source, and the VRT extent will be the same or larger than source 
#'	depending on resolution.
#' @param align Logical scalar. If TRUE, the upper left corner of the VRT 
#'	extent will be set to the upper left corner of the source pixel that 
#'	contains subwindow xmin, ymax. The VRT will be pixel-aligned with source 
#'	if the VRT resolution is the same as the source pixel size, otherwise VRT 
#'	extent will be the minimum rectangle that contains subwindow for the given 
#'	pixel size. If FALSE, the VRT upper left corner be exactly subwindow xmin, 
#'	ymax, and the VRT extent will be the minimum rectangle that contains 
#'	subwindow for the given pixel size. If subwindow is not given, the source 
#'	window is the source raster extent in which case align=FALSE has no effect.
#' @param resampling The resampling method to use if xsize, ysize of the VRT is
#'	different from the size of the underlying source rectangle (in number of
#'	pixels). The values allowed are nearest, bilinear, cubic, cubicspline, 
#'	lanczos, average and mode.
rasterToVRT <- function(srcfile, relativeToVRT=0, 
				vrtfile = tempfile("tmprast", fileext=".vrt"), 
				resolution=NULL, 
				subwindow=NULL, 
				align=TRUE, 
				resampling="nearest") {

	if (!isNamespaceLoaded("xml2")) {
		stop("rasterToVRT requires package xml2.")
	}

	#open the source raster	
	src_ds = rgdal::GDAL.open(srcfile, read.only=TRUE, silent=TRUE)
	src_nrows = .Call("RGDAL_GetRasterYSize", src_ds, PACKAGE="rgdal")
	src_ncols = .Call("RGDAL_GetRasterXSize", src_ds, PACKAGE="rgdal")
	src_gt = .Call("RGDAL_GetGeoTransform", src_ds, PACKAGE="rgdal")
	src_xmin = src_gt[1]
	src_xmax = src_xmin + src_gt[2] * src_ncols
	src_ymax = src_gt[4]
	src_ymin = src_ymax + src_gt[6] * src_nrows
	src_xres = src_gt[2]
	src_yres = src_gt[6]
	#nbands = .Call("RGDAL_GetRasterCount", src_ds, PACKAGE="rgdal")
	
	#save the source raster as a temporary VRT
	vrt_ds <- rgdal::copyDataset(src_ds, driver="VRT")
	tmp_vrtfile <- tempfile("src", fileext=".vrt")
	rgdal::saveDataset(vrt_ds, filename=tmp_vrtfile)
	rgdal::GDAL.close(vrt_ds)
	rgdal::GDAL.close(src_ds)
	
	if (is.null(resolution)) {
		vrt_xres = src_xres
		vrt_yres = src_yres
	}
	else {
		vrt_xres = resolution[1]
		vrt_yres = -resolution[2]
	}

	# src offsets
	if (is.null(subwindow)) {
		subwindow = c(src_xmin,src_ymin,src_xmax,src_ymax)
	}
	else {
		#subwindow should be inside the source raster extent
		#TODO: allow subwindow to be partially outside source raster extent
		if (subwindow[1] < src_xmin || subwindow[3] > src_xmax || 
			subwindow[2] < src_ymin || subwindow[4] > src_ymax) {
			stop("subwindow is not completely within source raster extent")
		}
	}
	src_xoff = floor(getOffset(subwindow[1], src_xmin, src_xres))
	src_yoff = floor(getOffset(subwindow[4], src_ymax, src_yres))
	
	#get vrt geotransform and rectangle sizes
	if (align) {
		#lay out the vrt raster so it is aligned with ul corner of ul src pixel
		vrt_xmin = src_xmin + src_xoff * src_xres
		vrt_ymax = src_ymax + src_yoff * src_yres
	}
	else {
		#lay out the vrt raster with origin at the subwindow origin
		vrt_xmin = subwindow[1]
		vrt_ymax = subwindow[4]
	}
	vrt_ncols = ceiling(getOffset(subwindow[3], vrt_xmin, vrt_xres))
	vrt_xmax = (vrt_xmin + vrt_ncols * vrt_xres)
	vrt_nrows = ceiling(getOffset(subwindow[2], vrt_ymax, vrt_yres))
	vrt_ymin = (vrt_ymax + vrt_nrows * vrt_yres)
	vrt_gt = c(vrt_xmin, vrt_xres, 0, vrt_ymax, 0, vrt_yres)
	srcwin_xsize = ceiling(getOffset(vrt_xmax, vrt_xmin, src_xres))
	srcwin_ysize = ceiling(getOffset(vrt_ymin, vrt_ymax, src_yres))

	#xml for the output vrt file
	x <- xml2::read_xml(tmp_vrtfile)
	
	#VRTDataset size
	xds <- xml2::xml_find_first(x, "/VRTDataset")
	xml2::xml_attrs(xds) <- c("rasterXSize" = as.character(vrt_ncols),
							"rasterYSize" = as.character(vrt_nrows))
	
	#Geotransform
	xgt <- xml2::xml_find_first(x, "/VRTDataset/GeoTransform")
	xml2::xml_text(xgt) <- paste(vrt_gt, collapse=", ")
	
	#SrcRect
	xpath <- "/VRTDataset/VRTRasterBand/SimpleSource/SrcRect"
	xsrcrect <- xml2::xml_find_all(x, xpath)
	xml2::xml_attrs(xsrcrect) <- c("xOff" = as.character(src_xoff),
								"yOff" = as.character(src_yoff),
								"xSize" = as.character(srcwin_xsize),
								"ySize" = as.character(srcwin_ysize))
	
	#DstRect
	xpath <- "/VRTDataset/VRTRasterBand/SimpleSource/DstRect"
	xdstrect <- xml2::xml_find_all(x, xpath)
	xml2::xml_attrs(xdstrect) <- c("xOff" = "0",
								"yOff" = "0",
								"xSize" = as.character(vrt_ncols),
								"ySize" = as.character(vrt_nrows))
	
	#resample if required
	if (vrt_ncols != srcwin_xsize || vrt_nrows != srcwin_ysize) {
		xpath <- "/VRTDataset/VRTRasterBand/SimpleSource"
		xssrc <- xml2::xml_find_all(x, xpath)
		xml2::xml_attr(xssrc, "resampling") <- resampling
	}
	
	#SourceFilename, relativeToVRT
	xpath <- "/VRTDataset/VRTRasterBand/SimpleSource/SourceFilename"
	xfn <- xml2::xml_find_all(x, xpath)
	if (relativeToVRT) {
		#this assumes the VRT file will be in the same directory as source file
		xml2::xml_attr(xfn, "relativeToVRT") <- "1"
		xml2::xml_text(xfn) <- basename(srcfile)
	}
	else {
		xml2::xml_attr(xfn, "relativeToVRT") <- "0"
		xml2::xml_text(xfn) <- srcfile
	}
	
	# zero-out statistics metadata if present
	xsrcstats <- xml2::xml_find_all(x, "//MDI[starts-with(@key, 'STATISTICS')]")
	if (length(xsrcstats) != 0) {
		xml2::xml_text(xsrcstats) <- rep("0", length(xsrcstats))
	}
	
	xml2::write_xml(x, vrtfile)
	
	invisible(vrtfile)
}


