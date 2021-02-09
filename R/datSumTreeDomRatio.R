datSumTreeDomRatio = function(ndat, ddat, uniqueid="PLT_CN", nprefix=NULL, dprefix=NULL, 
	rprefix="r", datround=NULL, savedata=FALSE, out_layer=NULL, outfolder=NULL,
 	out_fmt="csv", out_dsn=NULL, append_layer=FALSE, outfn.pre=NULL, outfn.date=FALSE,
 	overwrite=FALSE){

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
		overwrite=overwrite, append_layer=append_layer)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt

    ## out_layer
    if (is.null(out_layer)) {
      out_layer <- paste(newprefix, "_dat", sep="")
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
		outfn.date=outfn.date, overwrite_layer=overwrite,
		index.unique=uniqueid)
  }


  return(list(rdat=setDF(datx), rvars=rvars))
}


