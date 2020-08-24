datFilter <- function(x, xfilter=NULL, xfiltervar=NULL, othertabnms=NULL, 
	uniqueid="PLT_CN", vardelete=NULL, title.filter=NULL, savedata=FALSE, 
	outfolder=NULL, outfn="datf", outfn.pre=NULL, outfn.date=FALSE, 
	overwrite=FALSE, filternm=NULL, stopifnull=FALSE, returnDT=TRUE, xnm=NULL,
	gui=FALSE) {

  ## DESCRIPTION: subsets data table x with filter(s), and other tables if specified.
  ## VALUE: Return:
  ##		xf - data table with applied filter
  ##		xfilter - filter
  ## If gui and 

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0 | gui, TRUE, FALSE)
  if (gui) {uniqueid=savedata <- NULL}

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type == "windows")
    Filters = rbind(Filters,csv = c("Comma-delimited files (*.csv)", "*.csv"))

  isdt <- FALSE
  if (is.data.table(x) && !is.null(key(x))) {
    isdt <- TRUE
    xkey <- key(x)
  }

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  intabs <- NULL


  ## Check filternm
  if (is.null(filternm)) filternm <- "filter"

  ## Check title.filter
  if (is.null(title.filter)) title.filter <- filternm

  ## Check inputs
  ##############################################
  datx <- pcheck.table(x, caption = "Data table?", gui=gui, stopifnull=TRUE, 
			tabnm=xnm)


  ### Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, "Save data tables?", "NO")
  if (savedata) 
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)
  

  ################################################################################  
  ### DO WORK
  ################################################################################
  xfilters <- {}

  if (!is.null(xfilter)) {
    if (!is.character(xfilter)) stop("xfilter must be a character string")
    ## Check logical statement
    xfilter <- FIESTA::check.logic(datx, xfilter, filternm=filternm)

    ## Apply filter
    #indat <- datx[eval(parse(text = xfilter)),]
    indat <- subset(datx, eval(parse(text = xfilter)))
    xfilters <- xfilter

    ## If filter removes all records, print warning. 
    ## If gui=TRUE, use gui to prompt user; if gui=FALSE, return NULL values
    if (nrow(indat) == 0) {
      message("filter removed all records")
      if (stopifnull) stop("")
      if (!gui) {
        return(list(xf = NULL, xfilters = NULL))
      } else { 
        xfilter <- xfilters <- NULL
      }
    } else if(nrow(indat) == nrow(datx)) {
      if (title.filter != "ACI.filter")
        message(paste("filter did not change number of records:", xfilter))
      if (gui) gui <- FALSE
    } else {
      message(paste("filter removed", nrow(datx)-nrow(indat), "records:", xfilter))
    }
  }

  # FILTER DATA
  if (is.null(xfilter) || xfilter == "") {
    if (gui) {
      ## Check if vardelete variable are ok
      if (any(!vardelete %in% names(datx))) {
        varnotin <- vardelete[which(!vardelete %in% names(datx))]
        message(paste("vardelete variables not in data table:", paste(varnotin, collapse=", ")))
        vardelete <- vardelete[which(vardelete %in% names(datx))]
      }

      once <- FALSE
      response <- "YES"
      xfilterlst <- names(datx)
      xfiltervars <- {vardelete}
      cnt <- 0
      while (response == "YES") {
        cnt <- cnt + 1
        if (!is.null(xfiltervar)) response <- "NO"
        xfilterlst <- xfilterlst[!xfilterlst %in% xfiltervars]
    
        if (is.null(xfiltervar)) {
          title <- "Filter variable"
          if (!is.null(title.filter)) title <- paste(title, "-", title.filter)
          varlst <- c("NONE", sort(xfilterlst))
          xfiltervar <- select.list(varlst, title = title, multiple = FALSE)
          stopifnot(xfilter != "")
          if (xfiltervar == "NONE") {
            if (is.null(xfilters)) {
              message("no filter applied")
              return(list(xf = datx, xfilters = NULL))
            }
          }
        } else {
          once <- TRUE
        }
        if (xfiltervar != "NONE") {
          xfiltervars <- c(xfiltervars, xfiltervar)
          if (xfiltervar != "SPCD" && is.numeric(datx[[xfiltervar]]) && length(xfiltervals) > 50) {
            xfiltervals <- as.character(sort(unique(datx[[xfiltervar]])))
            filterval_min <- select.list(xfiltervals, title = paste("Select MIN", xfiltervar), 
              	multiple=FALSE)
            if (filterval_min == "") stop("")
            xfiltervals <- xfiltervals[as.numeric(xfiltervals) >= as.numeric(filterval_min)]
            filterval_max <- select.list(xfiltervals, title = paste("Select MAX", xfiltervar), 
              	multiple = FALSE)
            if (filterval_max == "") stop("")

            xfiltertxt <- paste0("(", xfiltervar, " >= ", filterval_min, " & ", xfiltervar, 
            	" <= ", filterval_max, ")")
          } else {
            xfiltervals <- sort(unique(datx[[xfiltervar]]))
            title <- "Filter value(s)"
            if (!is.null(title.filter)) title <- paste(title, "-", title.filter)
            xfilterval <- select.list(xfiltervals, title = title, multiple = TRUE)
            xfiltertxt <- paste(xfiltervar, "%in% c(", FIESTA::addcommas(xfilterval, 
				quotes = TRUE), ")")
          }
		
          # Subset indat
          indat <- subset(datx, eval(parse(text = xfiltertxt)))
          #indat <- datx[eval(parse(text = xfiltertxt)),]
 
          if (length(xfilters > 1)) {
            xfilters <- paste(xfilters, "&", xfiltertxt)
          } else {
            xfilters <- c(xfilters, xfiltertxt)
          }

          titleresp <- "Another filter?"
          if (!is.null(title.filter)) titleresp <- paste(titleresp, "-", title.filter)
          if (!once) {
            response <- select.list(c("NO","YES"), title=titleresp, multiple=FALSE)
            if (response == "") stop("")
          }
          xfiltervar <- NULL
        }else{
          response <- "NO"
        }
      }
    } else {
      indat <- datx
    }
  }
  indatids <- indat[[uniqueid]] 

  ## Clip othertables
  if (!is.null(othertabnms)) {
    if (!all(sapply(othertabnms, exists))) {
      miss <- othertabnms[which(!sapply(othertabnms, exists))]
      stop("invalid othertabnms: ", paste(miss, collapse=", "))
    }
    othertabs <- lapply(othertabnms, function(x) get(x, envir=environment()))
    intabs <- FIESTA::clip.othertables(indatids, othertabnms, othertabs=othertabs, 
		savedata=savedata, outfolder=outfolder, overwrite=overwrite, 
		outfn.pre=outfn.pre, outfn.date=outfn.date)
  }

  #### WRITE TO FILE 
  #############################################################
  if (savedata)
    write2csv(indat, outfilenm=outfn, overwrite=overwrite, outfn.date=outfn.date)

  if (!returnDT) {
    indat <- setDF(indat)  
  } else {
    if (isdt && !is.null(xkey)) 
      setkeyv(indat, xkey)
  }
    
  returnlst <- list(xf = indat, xfilter = xfilters)
  if (!is.null(intabs)) returnlst$cliptabs <- intabs
  
  return(returnlst)
}
