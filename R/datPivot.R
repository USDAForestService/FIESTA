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
#' @param savedata Logical. If TRUE, writes output data to outfolder.
#' @param outfolder String. The name of the output folder. If savedata=TRUE,
#' all output saved to the outfolder. If savedata=FALSE, only a text file of
#' input parameters is saved.
#' @param outfn String. The name of the output file if savedata=TRUE (*.csv).
#' Do not include extension. If NULL, the file will be named pivot_'date'.csv
#' @param outfn.date Logical. If TRUE, add current date to out_dsn.
#' @param overwrite Logical. If TRUE, overwrite existing file.
#' @return
#' 
#' \item{ptab}{ Matrix. The pivot table. }
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' 
#' 	datPivot(x=FIESTA::WYcond, pvar="CONDPROP_UNADJ", xvar="FORTYPCD", yvar="STDSZCD")
#' 
#' 	datPivot(x=FIESTA::WYtree, pvar="TPA_UNADJ", xvar="SPCD", yvar="STATUSCD", 
#' 		pfun=mean, NAto0=FALSE)
#' 
#' 
#' @export datPivot
datPivot <- function(x, pvar, xvar, yvar, pfun=sum, xfilter=NULL, 
	NAto0=TRUE, dropNAxvar=TRUE, dropNAyvar=TRUE, pvar.round=2, 
	savedata=FALSE, outfolder=NULL, outfn=NULL, outfn.date=FALSE,
	overwrite=TRUE){

  #####################################################################################
  ## DESCRIPTION: Generates a pivot table.   
  #####################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)


  if (gui) savedata <- NULL
  concatx <- NULL

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## Check x
  datx <- FIESTA::pcheck.table(x, gui=gui, tabnm="x", caption="Table with variable(s)?",
		stopifnull=TRUE)
  xnamelst <- names(datx)
  
  ## Check pvar
  pvar <- FIESTA::pcheck.varchar(var2check=pvar, varnm="pvar", checklst=xnamelst, 
	caption="Pivot variable", warn="pvar not in data table", stopifnull=TRUE) 

  ## Check xvar
  if (is.null(xvar)) {
    xvar <- FIESTA::pcheck.varchar(var2check=xvar, varnm="xvar", checklst=xnamelst, 
		caption="X variable", warn="xvar not in data table", multiple=TRUE, 
		stopifnull=TRUE, gui=gui)
  } else if (!all(xvar %in% xnamelst)) {
    xvar.miss <- xvar[!xvar %in% xnamelst]
    stop("xvar is invalid: ", toString(xvar.miss))
  }
  xvar.class <- lapply(datx[,xvar, with=FALSE], class)

  ## Check yvar
  yvar <- FIESTA::pcheck.varchar(var2check=yvar, varnm="yvar", checklst=xnamelst, 
	caption="Y variable", warn="yvar not in data table", stopifnull=TRUE) 

  ## Check NAto0
  NAto0 <- FIESTA::pcheck.logical(NAto0, varnm="NAto0", title="Convert NA to 0?", 
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
  datxf <- FIESTA::datFilter(x=datx, xfilter=xfilter, gui=gui)$xf

  ## Check outfolder
  ###########################################################
  if (savedata) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui=gui)
    if (is.null(outfn) || gsub(" ", "", outfn) == "") 
      outfn <- "pivot"
  }
 
  ################################################################################	
  ## DO WORK
  ################################################################################
  if (dropNAyvar) 
    datxf <- na.omit(datxf, cols=yvar)
  if (dropNAxvar)
    datxf <- na.omit(datxf, cols=xvar)
  datxf[, concatx := do.call(paste, c(.SD, sep="#")), .SDcols=xvar]
  
  if (NAto0) {
    pfill <- ifelse(NAto0, 0, NA)  
  }  
 
  ptab <- data.table::dcast(datxf, concatx ~ get(yvar), value.var=pvar, 
		fun.aggregate=pfun, na.rm=TRUE, pfill=pfill)
  if (NAto0) { 
    ptab[is.na(ptab)] <- 0
  }

  ptab[, (xvar) := tstrsplit(concatx, "#", fixed=TRUE)][][, concatx :=NULL]
  datxf[, concatx := NULL]
   
  cols <- names(ptab)[!names(ptab) %in% xvar]
  setcolorder(ptab, c(xvar, cols))
  ptab[, (cols) := round(.SD, pvar.round), .SDcols=cols]
  setkeyv(ptab, xvar)

  ## Define class of xvar and yvar
  for (x in xvar) {
    class(ptab[[x]]) <- xvar.class[[x]]
  } 

  if (savedata) {
    FIESTA::write2csv(ptab, outfolder=outfolder, outfilenm=outfn, 
		outfn.date=outfn.date, overwrite=overwrite)
  }
    
  return(ptab)
}
