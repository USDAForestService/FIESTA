datSumTreeDomRatio = function(ndat, 
                              ddat, 
                              uniqueid = "PLT_CN", 
                              nprefix = NULL, 
                              dprefix = NULL, 
                              rprefix = "r", 
                              datround = NULL, 
                              savedata = FALSE, 
                              savedata_opts = NULL,
                              gui = FALSE){
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
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(datSumTreeDomRatio)) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts)
  
  ## Set savedata defaults
  savedata_defaults_list <- formals(savedata_options)[-length(formals(savedata_options))]
  
  for (i in 1:length(savedata_defaults_list)) {
    assign(names(savedata_defaults_list)[[i]], savedata_defaults_list[[i]])
  }
  
  ## Set user-supplied savedata values
  if (length(savedata_opts) > 0) {
    if (!savedata) {
      message("savedata=FALSE with savedata parameters... no data are saved")
    }
    for (i in 1:length(savedata_opts)) {
      if (names(savedata_opts)[[i]] %in% names(savedata_defaults_list)) {
        assign(names(savedata_opts)[[i]], savedata_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(savedata_opts)[[i]]))
      }
    }
  }
  
  ##################################################################
  ## CHECK PARAMETER INPUTS
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
	
  if (is.null(rprefix)) {
    rprefix <- "r"
  }

  ## Check savedata 
  savedata <- pcheck.logical(savedata, varnm="savedata", title="Save data table?", 
                             first="NO", gui=gui)
  
  ## Check output parameters
  if (savedata) {
    outlst <- pcheck.output(outfolder=outfolder, out_dsn=out_dsn, 
                            out_fmt=out_fmt, outfn.pre=outfn.pre, outfn.date=outfn.date, 
                            overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
                            add_layer=add_layer, append_layer=append_layer, gui=gui)
    outfolder <- outlst$outfolder
    out_dsn <- outlst$out_dsn
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    append_layer <- outlst$append_layer
    outfn.date <- outlst$outfn.date
    outfn.pre <- outlst$outfn.pre
    if (is.null(out_layer)) {
      out_layer <- "tsumrat"
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

  nmiss <- sub("r", "n", rvars)[!sub("r", "n", rvars) %in% nvars]
  dmiss <- sub("r", "d", rvars)[!sub("r", "d", rvars) %in% dvars]
  datx[, c(nmiss, dmiss) := 0]
  
  ## Use data.table Map function to divide all nvars by dvars, including differences
  datx[, (rvars) := Map( "/", mget(sub("r", "n", rvars)), mget(sub("r", "d", rvars)))]
  
  
  ## Change NA values to 0. Note, division by 0 results in Inf values
  datx <- DT_NAto0(datx, rvars, changeto=0)
  datx <- datx[, c(uniqueid, rvars), with=FALSE]
  if (!is.null(datround)) {
    datx[, (rvars) := lapply(.SD, round, datround), .SDcols=rvars]
  }
  

  if (savedata) {
   datExportData(datx, 
          savedata_opts=list(outfolder=outfolder, 
                              out_fmt=out_fmt, 
                              out_dsn=out_dsn, 
                              out_layer=out_layer,
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer,
                              add_layer=TRUE,
                              index.unique=uniqueid)) 
    
  }


  return(list(rdat=setDF(datx), rvars=rvars))
}


