datLUTclass <- function(x, xvar=NULL, LUT=NULL, minvar="MIN", maxvar="MAX", 
	LUTclassnm=NULL, savedata=FALSE, outfolder=NULL, outfn=NULL){
  #################################################################################
  ## DESCRIPTION: Function to get variable name from a table stored within FIESTA 
  ##      or a look-up table (*.csv FILE).
  #################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) x=xvar=LUT=xLUT=LUTvar=LUTnewvar=varclass=minvar=maxvar=VALUE <- NULL 

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") 
    Filters <- rbind(Filters, csv=c("Comma-delimited files (*.csv)", "*.csv"))


  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## Check datx
  ########################################################
  dat <- FIESTA::pcheck.table(x, gui=gui, caption="Data table?", returnDT=TRUE)
  isshp <- FALSE
  if (typeof(dat) == "S4") {
    isshp <- TRUE
    datsp <- datx
    datx <- data.table(datx@data)
  } else {
    datx <- dat
  }

  ## Check xvar
  ##########################################
  datnmlst <- names(datx)
  xvar <- FIESTA::pcheck.varchar(xvar, "xvar", datnmlst, gui=gui,
		caption="Join variable in dat", stopifnull=TRUE)

  ## Check LUT
  ########################################################
  if (is.vector(LUT) && length(LUT) > 1) {
    LUTx <- data.table(LUT)
    setnames(LUTx, xvar)
  } else {
    LUTx <- FIESTA::pcheck.table(LUT, gui=gui, tabnm="LUT", caption="Look up table?")
  }


  ## Check minvar and maxvar
  ########################################################
  LUTnmlst <- names(LUTx)
  minvar <- FIESTA::pcheck.varchar(minvar, "minvar", LUTnmlst, gui=gui,
		caption="LUT min variable", stopifnull=TRUE)
  LUTnmlst <- LUTnmlst[LUTnmlst != minvar]
  maxvar <- FIESTA::pcheck.varchar(maxvar, "maxvar", LUTnmlst, gui=gui,
		caption="LUT max variable", stopifnull=TRUE)

  ## Check LUTclassnm
  ########################################################
  LUTnmlst <- LUTnmlst[LUTnmlst != maxvar]
  LUTclassnm <- FIESTA::pcheck.varchar(LUTclassnm, "LUTclassnm", LUTnmlst, gui=gui,
		caption="LUT class name")

  ## If LUTclassnm=NULL, create a class
  if (is.null(LUTclassnm)) {
    LUTclassnm <- paste0(xvar, "CL")
    LUTclassnm <- checknm(LUTclassnm, datnmlst)

    LUTx[[LUTclassnm]] <- paste(LUTx[[minvar]], LUTx[[maxvar]], sep="-")
  }

  ### Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", title="Save data tables?", 
		first="NO", gui=gui)

  ## GET OUTFOLDER IF NULL
  if (savedata) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)

    if (is.null(outfn) || gsub(" ", "", outfn) == "") { 
      outfn <- "datlut"
    } else if (!is.character(outfn)) {
      stop("outfn must be character")
    }

    outdatfnbase <- paste0(outfn, "_", format(Sys.time(), "%Y%m%d"))
    outdatfn <- fileexistsnm(outfolder, outdatfnbase, "csv")
    outfilenm <- paste0(outfolder, "/", outdatfn, ".csv")
  }


  ############################################################################
  ## DO THE WORK 
  ############################################################################

  ### SELECT RELEVANT COLUMNS FROM LUT & MERGE TO x (xvar >= MIN & xvar <= MAX)
  if (!is.factor(LUTx[[LUTclassnm]])) 
    LUTx[[LUTclassnm]] <- factor(LUTx[[LUTclassnm]], levels=LUTx[[LUTclassnm]])
  datx[[LUTclassnm]] <- as.character(datx[[xvar]]) 
  for (i in 1:nrow(LUTx))
    datx[[LUTclassnm]][datx[[xvar]] >= LUTx[i, minvar, with=FALSE][[1]] & 
		datx[[xvar]] <= LUTx[i, maxvar, with=FALSE][[1]] ] <- 
			as.character(LUTx[i, LUTclassnm, with=FALSE][[1]])
  if (is.factor(LUTx[[LUTclassnm]]))
    datx[[LUTclassnm]] <- factor(datx[[LUTclassnm]], levels=LUTx[[LUTclassnm]])
  xvar <- LUTclassnm

  
  if (isshp) {
    for (nm in names(datx)) {
      if (nchar(nm) > 10){
        newnm <- substr(nm, 0, 10)
        names(datx)[names(datx) == nm] <- newnm
      }
    }
    coordvars <- names(data.frame(sp::coordinates(x)))
    xvar <- coordvars[1]
    yvar <- coordvars[2]
    prj4str <- sp::proj4string(x)
    datxsp <- sp::SpatialPointsDataFrame(datx[,c(xvar,yvar)], datx, 
				proj4string=sp::CRS(prj4str))
    datx <- datxsp
  }

  ## Output list
  ########################################################
  if (isshp) {
    xLUTlst <- list(xLUT = methods::as(datx, "SpatialPointsDataFrame"))
  } else {
    xLUTlst <- list(xLUT=datx)
  }
  xLUTlst$LUTclassnm <- LUTclassnm
  xLUTlst$LUT <- LUTx 

  
  if (savedata) {
    if (isshp) {
      FIESTA::spExportShape(datxsp, outfolder=outfolder, outshpnm=outfn)
    } else {
      ## WRITE DATA TO OUTFOLDER
      write.csv(xLUT, outfilenm, row.names=TRUE)
      cat(
      " #################################################################################", 
      "\n", paste("Table written to: "), "\n", paste(" ", outfilenm), "\n", 
      "#################################################################################",
      "\n" )
    }
  }

  return(xLUTlst)
}
