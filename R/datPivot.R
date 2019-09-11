datPivot <- function(x, pvar, xvar, yvar, pfun=sum, xfilter=NULL, 
	pvar.round=2, savedata=FALSE, outfolder=NULL, outfn=NULL){

  #####################################################################################
  ## DESCRIPTION: Generates a pivot table.   
  #####################################################################################


  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) savedata <- NULL


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
  xvar <- FIESTA::pcheck.varchar(var2check=xvar, varnm="xvar", checklst=xnamelst, 
	caption="X variable", warn="xvar not in data table", stopifnull=TRUE) 

  ## Check yvar
  yvar <- FIESTA::pcheck.varchar(var2check=yvar, varnm="yvar", checklst=xnamelst, 
	caption="Y variable", warn="yvar not in data table", stopifnull=TRUE) 


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

    pivotfn <- FIESTA::fileexistsnm(outfolder, outfn, "csv")
    pivotfnout <- paste0(outfolder, "/", pivotfn, ".csv")
  }


  ################################################################################	
  ## DO WORK
  ################################################################################	

  ## Generate pivot table
  ptab <- tapply(datxf[[pvar]], datxf[,c(xvar, yvar), with=FALSE], pfun)
  ptab[is.na(ptab)] <- 0
  ptab <- round(ptab, pvar.round)


  return(ptab)
}
