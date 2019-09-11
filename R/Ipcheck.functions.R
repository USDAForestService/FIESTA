#pcheck.logical	- Checks logical function parameters
#pcheck.varchar	- Checks string variable parameter
#pcheck.table
#pcheck.outfolder
#pcheck.states
#pcheck.DBtabs	- Checks for existing database tables
#pcheck.xlsx


pcheck.logical <- function (var2check, varnm=NULL, title=NULL, first="YES", gui=FALSE,
	stopifnull=FALSE) {
  ## DESCRIPTION: Checks logical function parameters

  msg <- ifelse (!is.null(varnm), paste(varnm, "must be logical"), "variable must be logical")
  second <- ifelse(toupper(first) == "YES", "NO", "YES") 
  if (is.null(var2check)) {
    if (gui) {
      resp <- select.list(c(first, second), title=title, multiple=FALSE)
      if (resp == "") stop("")
      var2check <- ifelse(resp == "YES", TRUE, FALSE)
    } else { 
      if (stopifnull) { 
        stop(paste(varnm, "is invalid"))
      } else {
        return(NULL)
      }
    }
  } else if (!is.logical(var2check)) { 
    stop(varnm, " must be logical")
  }
  return(var2check)
}


pcheck.varchar <- function(var2check, varnm=NULL, checklst, gui=FALSE, caption=NULL, 
	warn=NULL, stopifnull=FALSE, multiple=FALSE, ...){
  ## DESCRIPTION: Checks string variable parameter
  
  if (is.null(varnm)) { 
    varnm <- "varnm" 
  } else {
    if (!is.character(varnm)) warning("varnm must be a string")
  } 
  if (is.null(caption)) caption <- paste0(varnm, "?") 
  if (is.null(warn)) 
    warn <- ifelse(!is.null(checklst) && length(checklst) < 6, 
		paste(varnm, "must be in following list:", paste(checklst, collapse=", ")),
		paste(varnm, "is invalid"))

  if (is.null(var2check) || gsub(" ", "", var2check) == "") {
    if (gui) {
      var2check <- select.list(checklst, title=caption, multiple=multiple, ...)
      if (length(var2check) == 0 || var2check == "") stop("NULL")
    } else {
      if (stopifnull) { 
        stop(paste(varnm, "is NULL"))
      } else {
        return(NULL)
      }
    }
  } else if (!is.vector(var2check)) {
    stop(varnm, " must be a vector")
  } else if (!multiple && length(var2check) > 1) {
    stop(varnm, " must be a vector of length 1")
  } else if (!is.character(var2check)) {
    stop(varnm, " must be a string vector")
  } else if (!all(var2check %in% checklst)) {
    if (all(toupper(var2check) %in% checklst)) {
      var2check <- toupper(var2check)
    } else if (all(tolower(var2check) %in% checklst)) {
      var2check <- tolower(var2check)
    } else if (all(capfirst(var2check) %in% checklst)) {
      var2check <- capfirst(var2check)
    } else if (gui) {
      message(warn)
      var2check <- select.list(checklst, title=caption, multiple=multiple, ...)
      if (length(var2check) == 0 || var2check == "") stop("")
    } else {
      if (stopifnull) { 
        if (multiple)
          warn <- message("invalid variable: ", 
				paste(var2check[which(!var2check %in% checklst)], collapse=", "))
        stop(warn)
      } else {
        if (multiple) 
          warn <- message("invalid variable: ", 
				paste(var2check[which(!var2check %in% checklst)], collapse=", "))
        message(warn)
        return(NULL)
      }
    }
  }
  return(var2check)
}



pcheck.table <- function(tab, gui=TRUE, tabnm=NULL, caption=NULL, returnshp=TRUE, 
	factors=FALSE, returnDT=TRUE, selectlst=NULL, nullcheck=FALSE, warn=NULL,
	stopifnull=FALSE){

  ## DESCRIPTION: This function checks the table parameter..  if NULL, it prompts the 
  ##      user with gui options to select the table of interest.
  ## ARGUMENTS: 
  ## tab  Dataframe. Table parameter to check.
  ## caption  String. Caption 
  ## shp  Logical. If TRUE and tab is a shapefile, return a shapefile.

  if (!factors) {
    options.old <- options()
    options(stringsAsFactors=FALSE)
    on.exit(options(options.old), add=TRUE)
  } 
  

  ## Define function
#  checkrows <- function(x) {
#    check <- apply(x, 1, function(x){all(x == "" | x == " " | is.na(x))})
#    if (sum(check) > 0) {
#      writeLines(paste("removed NA rows: ", sum(check)))
#      x <- x[-which(check),] }
#    return(x)
#  }

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") {
    Filters=rbind(Filters,shp=c("Shapefiles (*.shp)", "*.shp"))
    Filters=rbind(Filters,csv=c("Comma-delimited files (*.csv)", "*.csv")) }

  if (is.null(tabnm)) tabnm <- "tab" 
  if (is.null(caption)) caption <- "Table?" 
   
  if (is.null(selectlst)) {
    selectlst <- c("NONE", "R Object", "*.csv")
  } else if (!is.character(selectlst)) {
    stop("invalid selectlst")
  }

  if (returnshp) selectlst <- c(selectlst, "*.shp" ) 

  if (is.null(tab)) {
    if (gui) {
      tabresp <- select.list(selectlst, title=caption, multiple=FALSE)
      if (tabresp=="") {
        stop("")
      } else if (tabresp == "NONE") {
        tabx <- NULL
      } else if (tabresp == "R Object") {
        objlst <- c(ls(pos=1, all.names=TRUE), 
		ls(envir=as.environment("package:FIESTA"), pattern="WY"))
        objlst <- objlst[sapply(objlst, function(x) is.data.frame(get(x)))]
        tabobj <- select.list(objlst, title=caption, multiple=FALSE)
        if (tabobj == "") stop("") 
        tab <- get(tabobj, pos=1)
        if (isS4(tab)) {
          tabx <- if (returnshp) { tabx <- tab } else { tabx <- data.table(tab@data) }
        } else {
          tabx <- setDT(tab)
          rm(tab)
        }
      } else if (tabresp == "*.csv" && .Platform$OS.type=="windows") {
        tabfn <- choose.files(default=getwd(), caption=caption, filters=Filters["csv",], 
		multi=FALSE)
        if (tabfn == "") stop("")
        tabx <- fread(tabfn, integer64="numeric")
      } else if (tabresp == "*.shp" && .Platform$OS.type=="windows") {
        shpfn <- choose.files(default=getwd(), caption="Select point shapefile", 
            filters=Filters["shp",], multi=FALSE)
        if (is.null(shpfn)) stop("")
        shpx <- readOGR(dirname(shpfn), FIESTA::basename.NoExt(tab))
        tabx <- if (returnshp) { tabx <- shpx } else { tabx <- data.table(shpx@data) }
      } else {
        tabx <- NULL
      }
    } else {
      tabx <- NULL
    }  
  } else if (is.data.frame(tab)) {
    if (nrow(tab) == 0) {
      warn <- ifelse (!is.null(warn), warn, paste(tabnm, "is a data frame with 0 rows"))
      stop(warn)
    } else {
      tabx <- data.table(tab)

      ## Check for NULL rows or rows with ""
      if (nullcheck) {
        tabx <- tabx[!apply(is.na(tabx) | tabx == "", 1, all),]
        tabx <- tabx[, names(tabx)[!apply(is.na(tabx), 2, all)], with=FALSE]
      }
    }
  } else if (isS4(tab)) {
    if (!returnshp) { tabx <- data.table(tab@data) } else { tabx <- tab }
  } else if (!is.character(tab)) {
    stop(tabnm, " must be a data frame/data table object, Spatialdataframe, or file name") 
  } else if (file.exists(tab)) {
    if (raster::extension(tab) == ".shp") {
      shpx <- readOGR(dirname(tab), FIESTA::basename.NoExt(tab))
      if (!returnshp) { tabx <- data.table(shpx@data) } else { tabx <- shpx }
    } else if (raster::extension(tab) == ".csv") {
      tabx <- data.table::fread(tab, integer64="numeric")

      ## Check for NULL rows or rows with ""
      #if (nullcheck) if (!is.null(tabx)) tabx <- checkrows(tabx)
      if (nullcheck) {
        tabx <- tabx[!apply(is.na(tabx) | tabx == "", 1, all),]
        tabx <- tabx[, names(tabx)[!apply(is.na(tabx), 2, all)], with=FALSE]
      }
    } 
  } else if (file.exists(paste0(tab, ".csv"))) {
      tabx <- data.table::fread(tab, integer64="numeric")

      ## Check for NULL rows or rows with ""
      #if (nullcheck) if (!is.null(tabx)) tabx <- checkrows(tabx)
      if (nullcheck) {
        tabx <- tabx[!apply(is.na(tabx) | tabx == "", 1, all),]
        tabx <- tabx[, names(tabx)[!apply(is.na(tabx), 2, all)], with=FALSE]
      }
  } else if (file.exists(paste0(tab, ".shp"))) {
      shpx <- readOGR(dirname(tab), FIESTA::basename.NoExt(tab))
      if (!returnshp) { tabx <- data.table(shpx@data) } else { tabx <- shpx }
  } else if (exists(tab, envir=.GlobalEnv)) {
    tabx <- get(tab)
  } else {
      stop(paste(tabnm, "does not exist"))
  }
  if (is.null(tabx) && stopifnull) stop(paste(tabnm, "is NULL"))

  if (!is.null(tabx) && !returnDT) {
    return(setDF(tabx))
  } else {
    return(tabx)
  }
}


pcheck.outfolder <- function(outfolder, gui=FALSE){

  if (is.null(outfolder)) {
    if (gui && .Platform$OS.type=="windows") {
      outfolder <- choose.dir(default=getwd(), caption="Select folder")
      if (is.na(outfolder)) stop("")
    } else {
      message("outfolder is NULL, files will be written to working directory")
      outfolder <- getwd()
    }
  }else if (!file.exists(outfolder)) {
    stop("outfolder does not exist")
    if (gui && .Platform$OS.type=="windows") {
      outfolder <- choose.dir(default=getwd(), caption="Select output folder")
      if (is.na(outfolder)) stop("")
    }
  }
  return(outfolder)
}



pcheck.states <- function (states, statereturn="MEANING", gui=FALSE, rs=NULL, 
	stopifnull=FALSE, ...) {
  ## DESCRIPTION: 
  ## Check states and return in proper format
  ##
  ## ARGUMENTS:
  ## states		String or Numeric Vector: Name or code of states
  ## statereturn	String. Format to return state in ("VALUE", "MEANING", "ABBR", "RSCD", "RS")
  ## gui		Logical. TRUE, if gui is allowed.
  ## rs		String Vector: Research unit (optional).
  ## ...		Other parameters to select.list

  ref_state <- FIESTA::ref_statecd
  if (!statereturn %in% names(ref_state)) stop("statereturn is invalid")

  if (!is.null(rs)) {
    if (all(rs %in% ref_state$RS)) {
      ref_state <- ref_state[ref_state$RS %in% rs, ]
    } else {
      warning("rs is invalid")
    }
  } 

  ## If NULL and gui=TRUE, prompt user
  if (is.null(states)) {
    if (gui) {
      states <- select.list(ref_state[[statereturn]], title="States", multiple=TRUE, ...)
      if (length(states) == 0) stop("")
    } else {
      if (!is.null(rs)) { 
        states <- ref_state[[statereturn]]
        message(paste("returning all states in", paste(rs, collapse=",")))
      } else {
        if (stopifnull) stop("invalid state")
        return(NULL)
      }
    }
  } 

  ## Check state name(s)
  if (!is.vector(states)) 
    stop("states must be vector of codes or names")

  if (!all(states %in% c(ref_state$VALUE, ref_state$ABBR, ref_state$MEANING))) {
    states2 <- sub("_", " ", states)

    if (!all(states2 %in% c(ref_state$VALUE, ref_state$ABBR, ref_state$MEANING))) {
      ## Make sure all states have first letter capital
      states2 <- capfirst(states, allwords=TRUE)

      if (!all(states2 %in% c(ref_state$VALUE, ref_state$ABBR, ref_state$MEANING))) {
        if (stopifnull) {
          states.miss <- states[which(!states %in% c(ref_state$VALUE, ref_state$ABBR, 
		  ref_state$MEANING))] 
          stop("invalid states: ", states.miss)
        } else {
          return(NULL)
        }
      }     
    }
    states <- states2
  }

  if (all(states %in% ref_state$VALUE) && is.character(states))
    states <- as.numeric(states) 

  ## Set column where state name is from
  col <- ifelse(all(states %in% ref_state$VALUE), "VALUE", 
		ifelse(all(states %in% ref_state$ABBR), "ABBR", 
			ifelse(all(states %in% ref_state$MEANING), "MEANING")))
  if (col == "VALUE") states <- as.numeric(states)

  ## Get states to return
  if (statereturn != col) {
    states2return <- ref_state[ref_state[,col] %in% states, 
		statereturn]
  } else {
    states2return <- states
  }

  return(states2return)
}


pcheck.DBtabs <- function(stcd, tables) {
  ## DESCRIPTION: Checks for existing database tables
  texist <- tables[which(sapply(tables, exists))]
  texist <- texist[sapply(texist, function(x) !is.null(get(x)))]
  tnoexist <- tables[!tables %in% texist]
  
  ## Check if existing table has correct state
  for (tab in texist) {
    tabobj <- get(tab)
    if ("STATECD" %in% names(tabobj)) {
      if (!all(stcd %in% unique(tabobj[["STATECD"]]))) {
        message(tab, " does not include data for state")
        tnoexist <- c(tnoexist, tab)
      }
    }
  }
  return(tnoexist)
}



pcheck.xlsx <- function(wbnm, savewb=TRUE, outfn=NULL, outfolder=NULL,
	outfn.date=TRUE, overwrite=FALSE)  {
  ## CREATE EXCEL WORKBOOK AND SHEET
  ###############################################################
  if (is.null(outfn)) outfn <- "WORKBOOK"

  newwb <- FALSE
  if (is.null(wbnm)) {
    wb <- xlsx::createWorkbook(type="xlsx")
    newwb <- TRUE
  } else {
    if (!file.exists(wbnm)) {
 
      if (!file.exists(dirname(wbnm))) stop("invalid directory")
      wb.basenm <- basename(wbnm)
      outfolder <- dirname(wbnm)

      ## Check if there is an extension
      if (raster::extension(wbnm) == "") {
        message("wbnm must end in xlsx.. adding to wbnm")
        wbnm <- paste0(wbnm, ".xlsx")
        if (file.exists(wbnm)) {
          message(paste("creating workbook:", wbnm))
          wb <- xlsx::loadWorkbook(file=wbnm)
          outfn <- wb.basenm
        }        
      } else {  
        message(paste(wbnm, "does not exist... creating new workbook"))
        wb <- xlsx::createWorkbook(type="xlsx")
        outfn <- FIESTA::basename.NoExt(wb.basenm)
        newwb <- TRUE
      } 
    } else {     
      wb <- xlsx::loadWorkbook(file=wbnm)
    }
  }
  if (savewb) {
 
    ## GET NAME FOR WORKBOOK
    ###############################################################
    if (newwb) {

      ## Check outfn.date
      outfn.date <- FIESTA::pcheck.logical(outfn.date, varnm="outfn.date", 
		title="Add date to filenm?", first="YES")
      if (outfn.date)
        outfn <- paste0(outfn, "_", format(Sys.time(), "%Y%m%d"))

      ## Check overwrite
      overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite file?", first="YES")

      if (!overwrite) {
        outfn <- FIESTA::fileexistsnm(outfolder, outfn, "xlsx")
        if (!is.null(outfolder))
          outfn <- paste(outfolder, outfn, sep="/")
      } else {
        if (is.null(outfolder)) outfolder <- dirname(outfn)
        outfn <- paste(outfolder, outfn, sep="/")
      }
      outfilenm <- paste0(outfn, ".xlsx")

      #outallin1base <- paste0(outfn, "_", format(Sys.time(), "%Y%m%d"))
      #outallin1fn <- fileexistsnm(outfolder, outfn, "xlsx")
      #outfilenm <- paste0(outfolder, "/", outallin1fn, ".xlsx")
    } else {
      outfilenm <- wbnm
    }

    ## SAVE EXCEL WORKBOOK
    ###############################################################
    xlsx::saveWorkbook(wb, outfilenm)  
    
    cat(
    " ###############################################################################", 
    "\n", paste("Table written to: "), "\n", paste(" ", outfilenm), "\n", 
    "###############################################################################",
    "\n" )
  }

  return(wbnm=outfilenm)
}



