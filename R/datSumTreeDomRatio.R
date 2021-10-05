#' Data - Generates ratio of tree domain variables.
#' 
#' Generates ratio of tree domain summaries from FIESTA::datSumTreedom().
#' 
#' 
#' @param ndat Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Table from FIESTA::datSumTreeDomRatio() with numerator tree domain
#' variables.
#' @param ddat Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Table from FIESTA::datSumTreeDomRatio() with numerator tree domain
#' variables.
#' @param uniqueid String. The unique identifier of both tables (default =
#' "PLT_CN").
#' @param nprefix String. The prefix variable identifier of numerator domain
#' variables in ndat.
#' @param dprefix String. The prefix variable identifier of denominator domain
#' variables in ddat.
#' @param rprefix String. The prefix variable identifier of new ratio variables
#' (default="r").
#' @param datround Integer. Number of digits to round ratio values to.
#' @param savedata Logical. If TRUE, writes output data to outfolder.
#' @param outfolder String. Name of the output folder. If savedata=TRUE, output
#' is saved to the outfolder.
#' @param out_fmt String. Format for output tables ('csv', 'sqlite', 'gpkg').
#' @param out_dsn String. Data source name for output. If extension is not
#' included, out_fmt is used. Use full path if outfolder=NULL.
#' @param out_layer String. Name of output layer in database or *.csv file, if
#' savedata=TRUE. If NULL, the file will be named tsum_'date'.csv.
#' @param outfn.pre String. Prefix for out_dsn.
#' @param layer.pre String. Prefix for out_layer.
#' @param outfn.date Logical. If TRUE, adds current date to outfile name.
#' @param overwrite_dsn Logical. If TRUE, overwrites raw_dsn, if exists.
#' @param overwrite_layer Logical. If TRUE, overwrites the out_layer in raw_dsn
#' or *.csv raw data layer, if datsource="csv".
#' @param append_layer Logical. If TRUE, and rawdata=TRUE, appends raw data
#' data frames to existing out_dsn layer or *.csv file.
#' @return \item{rdat}{ Data frame. Table with ratio values (ndat / ddat). }
#' \item{rvars}{ String vector. Variable names in rdat. }
#' 
#' If savedata=TRUE, the data table will be saved to the outfolder: \cr
#' @author Tracey S. Frescino
#' @keywords data
#' @export datSumTreeDomRatio
datSumTreeDomRatio = function(ndat, ddat, uniqueid="PLT_CN", nprefix=NULL, dprefix=NULL, 
	rprefix="r", datround=NULL, savedata=FALSE, outfolder=NULL, out_fmt="csv", 
	out_dsn=NULL, out_layer=NULL, outfn.pre=NULL, layer.pre=NULL, outfn.date=FALSE, 
	overwrite_dsn=FALSE, overwrite_layer=FALSE, append_layer=FALSE){

  ## DESCRIPTION: Generates ratio of tree domain summaries from FIESTA::datSumTreedom().
  ##
  ## ARGUMENTS:
  ## ndat	Dataframe of numerator tree domain variables 
  ## ddat	Dataframe of denominator tree domain variables 
  ## uniqueid	The uniqueid of both dataframes
  ## nprefix	The prefix variable identifier of numerator dataframe (Default = 'n')
  ## dprefix 	The prefix variable identifier of denominator dataframe (Default = 'd')
  ## newprefix	The new prefix identifier of index variables (Default is 'I')

  ## Function to get variables in dataframe with specified prefix
  getprefix = function(x, prefix){substr(x, 1, nchar(prefix))}


  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ### ndat TABLE
  ndatx <- pcheck.table(ndat, caption="Numerator table?")

  ### ndat TABLE
  ddatx <- pcheck.table(ddat, caption="Denominator table?")

  ## If no prefix is specified, adds n to ndatx variables and d to ddatx variables
  if (is.null(nprefix)) { 
    nprefix <- "n" 
    names(ndatx)[which(names(ndatx) != uniqueid)] = 
			sapply(names(ndatx)[which(names(ndatx) != uniqueid)], 
				function(x) { paste0(nprefix, x) })
  }
  if(is.null(dprefix)){ 
    dprefix <- "d" 	
    names(ddatx)[which(names(ddatx) != uniqueid)] = 
			sapply(names(ddatx)[which(names(ddatx) != uniqueid)], 
				function(x) { paste0(dprefix, x) })
  }
	
  if (is.null(newprefix)) {
    newprefix <- "r"
  }

  ## GET outfolder
  if (savedata) {
    outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_dsn=overwrite_dsn, append_layer=append_layer)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt

    ## out_layer
    if (is.null(out_layer)) {
      out_layer <- paste(newprefix, "dat", sep="_")
    }
    if (!is.null(layer.pre)) {
      out_layer <- paste(layer.pre, out_layer, sep="_")
    }
  }

	
  ################################################################################	
  ### DO WORK
  ################################################################################  
  setkeyv(ndatx, uniqueid)
  setkeyv(ddatx, uniqueid)

     #dat <- merge(ndat, ddat, by=uniqueid, all.x=TRUE, all.y=TRUE)	

  ## Get columns in dataframe that have prefix
  nvars <- names(ndatx)[grep(nprefix, sapply(names(ndatx), getprefix, nprefix))]
  dvars <- names(ddatx)[grep(dprefix, sapply(names(ddatx), getprefix, dprefix))]


  ndatx <- cbind(uniqueid=ndatx[[uniqueid]], 
	ndatx[, grep(nprefix, sapply(names(ndatx), getprefix, nprefix)), with=FALSE] )
  names(ndatx)[names(ndatx) == "uniqueid"] <- uniqueid

  ## Get variables that match
  ndoms <- substr(names(ndatx), nchar(nprefix)+1, nchar(names(ndatx)))[-1]
  ddoms <- substr(names(ddatx), nchar(dprefix)+1, nchar(names(ddatx)))[-1]
  if (!any(suppressWarnings(is.na(as.numeric(ndoms))))) {
    rdoms <- sort(unique(as.numeric(c(ndoms, ddoms))))
  } else {
    rdoms <- sort(unique(as.numeric(c(ndoms, ddoms))))
  } 
  rvars <- paste0(rprefix, rdoms)

  ## Merge numerator and denominator variables to one table
  datx = merge(ndatx[, c(uniqueid, nvars), with=FALSE], 
			ddatx[, c(uniqueid, dvars), with=FALSE], by=uniqueid)


  ## Use data.table Map function to divide all nvars by dvars, including differences
  datx[, (rvars) := Map( "/", mget(nvars), mget(dvars))]

  ## Change NA values to 0. Note, division by 0 results in Inf values
  datx <- DT_NAto0(datx, rvars, changeto=0)
  datx <- datx[, c(uniqueid, rvars), with=FALSE]
  if (!is.null(datround)) {
    datx[, (rvars) := lapply(.SD, round, datround), .SDcols=rvars]
  }
  


  if (savedata) {
    datExportData(datx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer=out_layer, 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		index.unique=uniqueid)
  }


  return(list(rdat=setDF(datx), rvars=rvars))
}


