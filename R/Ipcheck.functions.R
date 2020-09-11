#pcheck.logical	- Checks logical function parameters
#pcheck.varchar	- Checks string variable parameter
#pcheck.table
#pcheck.outfolder
#pcheck.states


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
	warn=NULL, stopifnull=FALSE, stopifinvalid=TRUE, multiple=FALSE, ...){
  ## DESCRIPTION: Checks string variable parameter

  if (is.null(varnm)) { 
    varnm <- "varnm" 
  } else {
    if (!is.character(varnm)) warning("varnm must be a string")
  } 
  #if (is.null(var2check) && stopifnull) stop(paste(varnm, "is NULL"))
  if (is.null(caption)) caption <- paste0(varnm, "?") 
  if (is.null(warn)) 
    warn <- ifelse(!is.null(checklst) && length(checklst) < 6, 
		paste(varnm, "must be in following list:", paste(checklst, collapse=", ")),
		paste(varnm, "is invalid"))

  if (is.null(var2check) || is.na(var2check) || length(var2check) == 0 || gsub(" ", "", var2check) == "") {
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
      if (stopifinvalid) { 
        if (multiple)
          warn <- message("invalid variable: ", 
				paste(var2check[which(!var2check %in% checklst)], collapse=", "))
        stop(warn)
      } else {
        return(NULL)
      }
    }
  }
  return(var2check)
}


pcheck.dsn <- function(dsn, dbconnopen=TRUE) {
  if (is.null(dsn)) stop("dsn is null")
  if (!file.exists(dsn)) {
    extlst <- c("shp", "csv", "sqlite", "gpkg", "gdb")
    ext <- extlst[sapply(extlst, function(x, dsn) 
				file.exists(paste(dsn, x, sep=".")), dsn)]
    if (length(ext) == 1)
      dsn <- paste(dsn, ext, sep=".")
  }
  tabext <- getext(dsn)
  if (is.na(tabext) || tabext == "NA")
    stop("dsn must include extension")

  if (tabext %in% c("sqlite", "gpkg")) {
    return(DBtestSQLite(dsn, dbconnopen=dbconnopen))
  } else if (tabext == "shp") {
    return(dsn) 
  } else {
    if (!file.exists(dsn)) stop("file does not exist")
    #stop("file format currently not supported")
  }
}


pcheck.table <- function(tab=NULL, tab_dsn=NULL, tabnm=NULL, tabqry=NULL, 
	caption=NULL, returnsf=TRUE, factors=FALSE, returnDT=TRUE, warn=NULL, 
	stopifnull=FALSE, nullcheck=FALSE, obj=FALSE, gui=FALSE){

  ## DESCRIPTION: This function checks the table parameter..  if NULL, it prompts the 
  ##      user with gui options to select the table of interest.
  ## ARGUMENTS: 
  ## tab - Dataframe or layer name in tab_dsn
  ## tab_dsn - String. data source name where tab resides
  ## tabqry - Database query for extracting tab (if tab_dsn != NULL)
  ## caption  String. Caption 
  ## shp  Logical. If TRUE and tab is a shapefile, return a shapefile.

  ## Set global variables
  x <- NULL

  if (!factors) {
    options.old <- options()
    options(stringsAsFactors=FALSE)
    on.exit(options(options.old), add=TRUE)
  } 

  ## Check for installed packages
  if (!"sf" %in% rownames(installed.packages()))
    stop("importing spatial layers requires package sf")

 
  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") {
    Filters=rbind(Filters,shp=c("Shapefiles (*.shp)", "*.shp"))
    Filters=rbind(Filters,csv=c("Comma-delimited files (*.csv)", "*.csv"))
    Filters=rbind(Filters,sqlite=c("Comma-delimited files (*.sqlite)", "*.sqlite"))
    Filters=rbind(Filters,gpkg=c("Comma-delimited files (*.gpkg)", "*.gpkg")) }
  tabdblst <- c("sqlite", "gpkg")

  if (is.null(tabnm)) tabnm <- "tab" 
  if (is.null(caption)) caption <- "Table?" 
   
  selectlst <- c("NONE", "R Object", "csv", "database")
  if (returnsf) selectlst <- c(selectlst, "*.shp" ) 

  ## Check gui
  if (gui && !.Platform$OS.type=="windows") 
    stop("gui not supported")
  if (is.null(tab) && is.null(tab_dsn)) {
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
        tabx <- get(tabobj, pos=1)
        if (sum(grepl("Spatial", class(tab)) > 0))
          if (!returnsf) tabx <- tabx$data
      } else if (tabresp == "csv") {
        tabfn <- choose.files(default=getwd(), caption=caption, 
			filters=Filters[c("csv", "All"),], multi=FALSE)
        if (tabfn == "") stop("")
      } else if (tabresp == "shp") {
        shpfn <- choose.files(default=getwd(), caption="Select point shapefile", 
            filters=Filters[c("shp", "All"),], multi=FALSE)
        if (is.null(shpfn)) stop("")
      } else if (tabresp == "database") {
        tab_dsn <- choose.files(default=getwd(), caption="Select database file", 
            filters=Filters[c(tabdblst, "All"),], multi=FALSE)
        if (tab_dsn == "") stop("")
      } else {
        tabx <- NULL
      }
    } else {
      if (stopifnull) stop(paste(tabnm, "is NULL"))
      return(NULL)
    } 
  } 
 
  if (!is.null(tab)) {
    if (is.character(tab)) {
      if (obj && exists(tab, envir=.GlobalEnv) && is.data.frame(get(tab))) {
        #message(tab, " exists in Global Environment")
        return(get(tab))  
      } else if (file.exists(tab)) {
        tab_dsn <- tab
      }
    } 
    if ("sf" %in% class(tab)) {
      if (returnsf) {
        return(tab)
      } else {
        tab <- sf::st_drop_geometry(tab)
        if (returnDT) tab <- setDT(tab)
        return(tab)
      }
    } else if (canCoerce(tab, "sf")) {
      tabx <- sf::st_as_sf(tab)
      if (returnsf) {
        return(tabx)
      } else {
        tab <- sf::st_drop_geometry(tab)
        if (returnDT) tab <- setDT(tab)
        return(tab)
      }
    } else if (is.data.frame(tab)) {
      if (nrow(tab) == 0) {
        warn <- ifelse (!is.null(warn), warn, paste(tabnm, "is a data frame with 0 rows"))
        if (stopifnull) stop(warn)
        message(warn)
        return(NULL)
      } else {
        if (returnDT) {
          if (!"data.table" %in% class(tab)) tab <- data.table(tab)
        } else { 
          if ("data.table" %in% class(tab)) tab <- setDF(tab)
        }
        return(tab)
      }
    } else if (!is.character(tab)) {
      stop(tabnm, " must be an sf object or character layer name") 
    } 
  }
  if (is.null(tab_dsn))
    tab_dsn <- tab

  if (!file.exists(tab_dsn)) {
    extlst <- c("shp", "csv", "sqlite", "gpkg", "gdb")
    ext <- extlst[sapply(extlst, function(x, tab_dsn) 
				file.exists(paste(tab_dsn, x, sep=".")), tab_dsn)]
    if (length(ext) == 1)
        tab_dsn <- paste(tab_dsn, ext, sep=".")
  } else {
    if (is.null(tab)) return(NULL)
  }
  tabext <- getext(tab_dsn)
  if (is.na(tabext) || tabext == "NA") {
    if (dir.exists(tab_dsn) && file.exists(paste(tab_dsn, tab, sep="/"))) {
      tab_dsn <- paste(tab_dsn, tab, sep="/")
      tabext <- getext(tab_dsn)
    } 
  }

  if (tabext == "shp") {
    tabx <- sf::st_read(tab_dsn, quiet=TRUE)
  } else if (tabext == "gdb") {
    tabx <- pcheck.spatial(tab, dsn=tab_dsn)
  } else if (tabext %in% tabdblst) {
    if (is.null(tab) || !is.character(tab)) 
      stop("tab is invalid")
    if (tabext %in% c("sqlite", "gpkg")) {
      if (!"RSQLite" %in% rownames(installed.packages()))
        stop("importing spatial layers requires package RSQLite")
      dbconn <- DBtestSQLite(tab_dsn, dbconnopen=TRUE, showlist=FALSE) 
      tablst <- DBI::dbListTables(dbconn)
      if (!tab %in% tablst) {
        if (tolower(tab) %in% tablst) {
          tab <- tolower(tab)
        } else {
          stop(tab, " not in ", tab_dsn)
        }
      }
      if (!is.null(tabqry)) {
        tabx <- setDT(DBI::dbGetQuery(dbconn, tabqry))
      } else {
        tabx <- setDT(DBI::dbReadTable(dbconn, tab))
      }
      DBI::dbDisconnect(dbconn)
    } else {
      stop("file format currently not supported")
    }
  } else {
    tabx <- tryCatch(data.table::fread(tab_dsn, integer64="numeric"),
			error=function(e) {
			print(e)
			return(NULL)})
    if (is.null(tabx)) stop("file format is currently not supported")
  }

  if (nullcheck) { 
    if (sum(apply(tabx, 1, function(x) sum(is.na(x) | x=="NA" | x=="")) == ncol(x)) > 0)
      tabx <- tabx[apply(tabx, 1, function(x) sum(is.na(x) | x=="NA" | x=="")) != ncol(x),]
  }

  if (returnsf && "sf" %in% class(tabx)) {
    return(tabx)
  } else {
    if (returnDT) {
      if (!is.data.table(tabx)) {
        return(setDT(tabx))
      } else {
        return(tabx)
      }
    } else {
      return(setDF(tabx))
    }
  }
}


pcheck.outfolder <- function(outfolder, default=getwd(), gui=FALSE) {
  if (is.null(outfolder)) {
    if (gui && .Platform$OS.type=="windows") {
      outfolder <- choose.dir(default=getwd(), caption="Select folder")
      if (is.na(outfolder)) stop("")
    } else {
      if (is.null(default)) return(NULL)
      if (default == getwd()) {
        message("outfolder is NULL, files will be written to working directory")
        outfolder <- getwd()
      }
    }
  } else {
    if (!is.character(outfolder)) {
      stop("outfolder must be character string")
    } else if (!dir.exists(outfolder)) {
      stop("invalid outfolder")
    }
  }
  return(outfolder)
}


pcheck.states <- function (states, statereturn="MEANING", gui=FALSE, RS=NULL, 
	stopifnull=FALSE, ...) {
  ## DESCRIPTION: 
  ## Check states and return in proper format
  ##
  ## ARGUMENTS:
  ## states		String or Numeric Vector: Name or code of states
  ## statereturn	String. Format to return state in ("VALUE", "MEANING", "ABBR", "RSCD", "RS")
  ## gui		Logical. TRUE, if gui is allowed.
  ## RS		String Vector: Research unit (optional).
  ## ...		Other parameters to select.list

  ref_state <- FIESTA::ref_statecd
  if (!statereturn %in% names(ref_state)) stop("statereturn is invalid")

  if (!is.null(RS)) {
    if (all(RS %in% ref_state$RS)) {
      ref_state <- ref_state[ref_state$RS %in% RS, ]
    } else {
      warning("RS is invalid")
    }
  } 

  ## If NULL and gui=TRUE, prompt user
  if (is.null(states)) {
    if (gui) {
      states <- select.list(ref_state[[statereturn]], title="States", multiple=TRUE, ...)
      if (length(states) == 0) stop("")
    } else {
      if (!is.null(RS)) { 
        states <- ref_state[[statereturn]]
        message(paste("returning all states in", paste(RS, collapse=",")))
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
