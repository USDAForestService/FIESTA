#' Data - Generates a pivot table.
#' 
#' Generates a pivot table of values by x row and y column.
#' 
#' 
#' @param x Dataframe. Table with pivot variables.
#' @param pvar String. The name of the variable for pivot table values.
#' @param xvar String. The name of the variable for rows.
#' @param yvar String. The name of the variable for columns.
#' @param pfun Function. The name of the function to use for pivot table values
#' (ex. sum, mean, max).
#' @param xfilter String. A filter to subset the datatable table x before
#' pivoting (ex. "STATUSCD == 1").
#' @param NAto0 Logical. If TRUE, converts NA values to 0.
#' @param dropNAxvar Logical. If TRUE, removes columns that are NA.
#' @param dropNAyvar Logical. If TRUE, removes rows that have NA values.
#' @param pvar.round Integer. Number to round pvar values to.
#' @param returnDT Logical. If TRUE, returns a datatable.
#' @param savedata Logical. If TRUE, writes output data to outfolder.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  If out_layer = NULL, 
#' default = 'datpivot'.
#' @param gui Logical. If TRUE, pop-up windows will appear for user-interface.
#' 
#' @return
#' 
#' \item{ptab}{ Matrix. The pivot table. }
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' # Pivot WYcond table
#' datPivot(x = FIESTA::WYcond,
#'          pvar = "CONDPROP_UNADJ",
#'          xvar = "FORTYPCD",
#'          yvar = "STDSZCD")
#' 
#' # Pivot WYtree table
#' datPivot(x = FIESTA::WYtree,
#'          pvar = "TPA_UNADJ",
#'          xvar = "SPCD",
#'          yvar = "STATUSCD",
#'          pfun = mean,
#'          NAto0 = TRUE)
#' @export datPivot
datPivot <- 
  function(x, 
           pvar, 
           xvar, 
           yvar, 
           pfun = sum, 
           xfilter = NULL, 
           NAto0 = TRUE, 
           dropNAxvar = TRUE, 
           dropNAyvar = TRUE, 
           pvar.round = 6,
           returnDT = FALSE,
           savedata = FALSE, 
           savedata_opts = NULL, 
           gui = FALSE){
    
    #####################################################################################
    ## DESCRIPTION: Generates a pivot table.   
    #####################################################################################
    
    ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
    gui <- ifelse(nargs() == 0, TRUE, FALSE)
    if (gui) savedata <- NULL
    
    ## set global variables
    concatx <- NULL
    
    
    ##################################################################
    ## CHECK PARAMETER NAMES
    ##################################################################
    
    ## Check input parameters
    input.params <- names(as.list(match.call()))[-1]
    formallst <- names(formals(datPivot)) 
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
    
    ## Check x
    datx <- pcheck.table(x, gui=gui, tabnm="x", caption="Table with variable(s)?",
                         stopifnull=TRUE)
    xnamelst <- names(datx)
    
    ## Check pvar
    pvar <- pcheck.varchar(var2check=pvar, varnm="pvar", checklst=xnamelst, 
                           caption="Pivot variable", warn="pvar not in data table", stopifnull=TRUE) 
    
    ## Check xvar
    if (is.null(xvar)) {
      xvar <- pcheck.varchar(var2check=xvar, varnm="xvar", checklst=xnamelst, 
                             caption="X variable", warn="xvar not in data table", multiple=TRUE, 
                             stopifnull=TRUE, gui=gui)
    } else if (!all(xvar %in% xnamelst)) {
      xvar.miss <- xvar[!xvar %in% xnamelst]
      stop("xvar is invalid: ", toString(xvar.miss))
    }
    xvar.class <- lapply(datx[,xvar, with=FALSE], class)
    
    ## Check yvar
    yvar <- pcheck.varchar(var2check=yvar, varnm="yvar", checklst=xnamelst, 
                           caption="Y variable", warn="yvar not in data table", stopifnull=TRUE) 
    
    ## Check NAto0
    NAto0 <- pcheck.logical(NAto0, varnm="NAto0", title="Convert NA to 0?", 
                            first="YES", gui=gui)
    
    ## Check function (pfun) used for aggregation
    pfunlst <- c("sum", "mean", "max", "min", "length", "I")
    if (is.null(pfun)) {
      pfunstr <- select.list(pfunlst, title="Pivot value", multiple=FALSE)
      if (pfunstr == "") stop("")
      pfun <- get(pfunstr)
    } else if (!is.function(pfun)) {
      stop("pfun is not a function")
    } else {
      pfunnm <- noquote(strsplit(deparse(pfun), ".Primitive")[[1]][2])
      if (is.na(pfunnm)) 
        pfunnm <- noquote(strsplit(deparse(pfun), "UseMethod"))[[2]][2]
      pfunstr <- substr(pfunnm, 3, nchar(pfunnm)-2)
    }
    
    ## Check xfilter
    ##################################################################
    datxf <- datFilter(x=datx, xfilter=xfilter, gui=gui)$xf
    
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
        out_layer <- "datpivot"
      }
    }
    
    ################################################################################	
    ## DO WORK
    ################################################################################
    if (dropNAyvar) 
      datxf <- na.omit(datxf, cols=yvar)
    if (dropNAxvar)
      datxf <- na.omit(datxf, cols=xvar)
    
    ## Concatenate multiple columns
    datxf[, concatx := do.call(paste, c(.SD, sep="|")), .SDcols=xvar]
    
    if (NAto0) {
      pfill <- ifelse(NAto0, 0, NA)  
    }  
    
    ptab <- data.table::dcast(datxf, concatx ~ get(yvar), value.var=pvar, 
                              fun.aggregate=pfun, na.rm=TRUE, pfill=pfill)
    if (NAto0) { 
      ptab[is.na(ptab)] <- 0
    }
    
    ## Un-concatenate multiple columns
    ptab[, (xvar) := tstrsplit(concatx, "|", fixed=TRUE)][][, concatx :=NULL]
    
    cols <- names(ptab)[!names(ptab) %in% xvar]
    setcolorder(ptab, c(xvar, cols))
    ptab[, (cols) := round(.SD, pvar.round), .SDcols=cols]
    setkeyv(ptab, xvar)
    
    ## Define class of xvar and yvar
    #for (x in xvar) {
    #  class(ptab[[x]]) <- xvar.class[[x]]
    #} 
    
    #### WRITE TO FILE 
    #############################################################
    if (savedata) {
      datExportData(ptab, 
                    savedata_opts=list(outfolder=outfolder, 
                                       out_fmt=out_fmt, 
                                       out_dsn=out_dsn, 
                                       out_layer=out_layer,
                                       outfn.pre=outfn.pre, 
                                       outfn.date=outfn.date, 
                                       overwrite_layer=overwrite_layer,
                                       append_layer=append_layer,
                                       add_layer=TRUE))
    }
    
    if (returnDT) {
      return(data.table(ptab))
    } else {	
      return(data.frame(ptab))
    }
  }
