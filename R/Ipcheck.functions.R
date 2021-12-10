# pcheck.logical	- Checks logical function parameters
# pcheck.unique	- Check for unique records
# pcheck.varchar	- Checks string variable parameter
# pcheck.table
# pcheck.outfolder
# pcheck.states
# pcheck.object
# pcheck.output
# pcheck.colors
# pcheck.areaunits
# pcheck.spatial - checks or gets Vector layer from file name or spatial object
# pcheck.params - function to check input list parameters


pcheck.logical <- function (var2check, varnm=NULL, title=NULL, first="YES",
	gui=FALSE, stopifnull=FALSE) {
  ## DESCRIPTION: Checks logical function parameters

  msg <- ifelse (!is.null(varnm), paste(varnm, "must be logical"),
	"variable must be logical")
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


pcheck.unique <- function(tab, uniqueid, gui=FALSE, tabnm=NULL,
	warn=NULL, stopifnull=FALSE, stopifinvalid=TRUE, multiple=FALSE, ...){
  ## DESCRIPTION: Checks string variable parameter

  tab <- pcheck.table(tab)
  uniqueid <- c("PLT_CN", "CONDID")

  if (is.null(tabnm)) {
    tabnm <- "data frame"
  }
  setkeyv(tab, uniqueid)

  if (!tab[, uniqueN(.SD) == .N, .SDcols=key(tab)]) {
    stop("uniqueid for ", tabnm, " is not unique: ", toString(uniqueid))
  }
  return(tab)
}


pcheck.varchar <- function(var2check, varnm=NULL, checklst, gui=FALSE, caption=NULL,
	warn=NULL, stopifnull=FALSE, stopifinvalid=TRUE, multiple=FALSE, ...){
  ## DESCRIPTION: Checks string variable parameter

  if (is.null(varnm)) {
    varnm <- "varnm"
  } else {
    if (!is.character(varnm)) {
      warning("varnm must be a string")
    }
  }
  #if (is.null(var2check) && stopifnull) stop(paste(varnm, "is NULL"))
  if (is.null(caption)) {
    caption <- paste0(varnm, "?")
  }
  if (is.null(warn)) {
    warn <- ifelse(!is.null(checklst) && length(checklst) < 6,
		paste(varnm, "must be in following list:", toString(checklst)),
		paste(varnm, "is invalid"))
  }

  if (is.null(var2check) || is.na(var2check) || length(var2check) == 0 || gsub(" ", "", var2check) == "") {
    if (gui) {
      var2check <- select.list(checklst, title=caption, multiple=multiple, ...)
      if (length(var2check) == 0 || var2check == "") {
        stop("NULL")
      }
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
        if (multiple) {
          warn <- message("invalid variable: ",
				toString(var2check[which(!var2check %in% checklst)]),
				"\n possible values: ", toString(checklst))
        }
        stop(warn)
      } else {
        return(NULL)
      }
    }
  }
  return(var2check)
}


pcheck.dsn <- function(dsn, dbconnopen=TRUE) {
  if (is.null(dsn)) {
    stop("dsn is null")
  }
  if (!file.exists(dsn)) {
    extlst <- c("shp", "csv", "sqlite", "gpkg", "gdb", "db", "db3")
    ext <- extlst[sapply(extlst, function(x, dsn)
				file.exists(paste(dsn, x, sep=".")), dsn)]
    if (length(ext) == 1)
      dsn <- paste(dsn, ext, sep=".")
  }
  tabext <- getext(dsn)
  if (is.na(tabext) || tabext == "NA") {
    stop("dsn must include extension")
  }
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
  x=tabx <- NULL

  ## Define accepted file format extents
  extlst <- c("shp", "csv", "sqlite", "sqlite3", "db", "db3", "gpkg", "gdb")

  if (!factors) {
    options.old <- options()
    options(stringsAsFactors=FALSE)
    on.exit(options(options.old), add=TRUE)
  }

  ## Check for installed packages
  if (!"sf" %in% rownames(installed.packages())) {
    message("importing spatial layers requires package sf")
  }

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") {
    Filters=rbind(Filters,shp=c("Shapefiles (*.shp)", "*.shp"))
    Filters=rbind(Filters,csv=c("Comma-delimited files (*.csv)", "*.csv"))
    Filters=rbind(Filters,sqlite=c("SQLite database (*.sqlite)", "*.sqlite"))
    Filters=rbind(Filters,sqlite=c("SQLite database (*.db)", "*.db"))
    Filters=rbind(Filters,sqlite=c("SQLite database (*.db3)", "*.db3"))
    Filters=rbind(Filters,gpkg=c("GeoPackage SQLite database (*.gpkg)", "*.gpkg")) }
  tabdblst <- c("sqlite", "sqlite3", "db", "db3", "gpkg")

  if (is.null(tabnm)) {
    tabnm <- "tab"
  }
  if (is.null(caption)) {
    caption <- "Table?"
  }

  selectlst <- c("NONE", "R Object", "csv", "database")
  if (returnsf) {
    selectlst <- c(selectlst, "*.shp" )
  }
  ## Check gui
  if (gui && !.Platform$OS.type=="windows") {
    stop("gui not supported")
  }
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
      }
    }
    if (is.null(tabx) && stopifnull) {
      stop(paste(tabnm, "is NULL"))
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
        if (returnDT) tab <- data.table(tab)
        return(tab)
      }
    } else if (canCoerce(tab, "sf")) {
      tabx <- sf::st_as_sf(tab)
      if (returnsf) {
        return(tabx)
      } else {
        tab <- sf::st_drop_geometry(tab)
        if (returnDT) tab <- data.table(tab)
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
  if (is.null(tab_dsn)) {
    tab_dsn <- tab
  }

  if (!is.null(tab_dsn) && !file.exists(tab_dsn)) {
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
    } else {
      stop(tabnm, " is invalid")
    }
  }

  if (tabext == "shp") {
    tabx <- sf::st_read(tab_dsn, quiet=TRUE)
  } else if (tabext == "gdb") {
    tabx <- pcheck.spatial(tab, dsn=tab_dsn)
  } else if (tabext %in% tabdblst) {
    if (is.null(tab) || !is.character(tab)) {
      stop("tab is invalid")
    }
    if (tabext %in% c("sqlite", "sqlite3", "db", "db3", "gpkg")) {
      if (!"RSQLite" %in% rownames(installed.packages())) {
        message("importing spatial layers requires package RSQLite")
      }
      dbconn <- DBtestSQLite(tab_dsn, dbconnopen=TRUE, showlist=FALSE)
      tablst <- DBI::dbListTables(dbconn)
      if (!tab %in% tablst) {
        if (tolower(tab) %in% tablst) {
          tab <- tolower(tab)
        } else {
          stop(tab, " not in ", tab_dsn)
        }
      }
      if (!is.null(tabqry) && !is.na(tabqry)) {
        tabx <- data.table(DBI::dbGetQuery(dbconn, tabqry))
      } else {
        tabx <- data.table(DBI::dbReadTable(dbconn, tab))
      }
      DBI::dbDisconnect(dbconn)
    } else {
      stop("file format currently not supported")
    }
  } else {
    tabx <- tryCatch(data.table::fread(tab_dsn, integer64="numeric"),
			error=function(e) {
			#print(e)
			return(NULL)})
    if (is.null(tabx)) {
      if (!tabext %in% extlst) {
        stop("file format is currently not supported")
      } else {
        stop("file is invalid or does not exist")
      }
    }
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
        return(data.table(tabx))
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
      if (is.null(default)) {
        return(NULL)
      } else if (is.null(outfolder)) {
        message("outfolder is NULL, defaulting to working directory")
        outfolder <- getwd()
      }
      #return(NULL)
    }
  } else {
    if (!is.character(outfolder)) {
      stop("outfolder must be character string")
    } else if (!dir.exists(outfolder)) {
      stop("invalid outfolder")
    }
  }
  return(normalizePath(outfolder))
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



pcheck.object <- function(obj=NULL, objnm=NULL, warn=NULL, caption=NULL,
	stopifnull=FALSE, gui=FALSE, list.items=NULL){
  ## DESCRIPTION: checks object name

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") {
    Filters=rbind(Filters,shp=c("R Objects (*.rda)", "*.rda")) }


  ## Set global variables
  objx <- NULL

  if (is.null(objnm)) objnm <- "obj"
  if (is.null(caption)) caption <- "Object?"

  selectlst <- c("NONE", "object", "rda")

  ## Check gui
  if (gui && !.Platform$OS.type=="windows")
    stop("gui not supported")
  if (is.null(obj)) {
    if (gui) {
      objresp <- select.list(selectlst, title=caption, multiple=FALSE)
      if (objresp=="") {
        stop("")
      } else if (objresp == "NONE") {
        objx <- NULL
      } else if (objresp == "R Object") {
        objlst <- c(ls(pos=1, all.names=TRUE),
		ls(envir=as.environment("package:FIESTA"), pattern="WY"))
        objlst <- objlst[sapply(objlst, function(x) is.data.frame(get(x)))]
        obj <- select.list(objlst, title=caption, multiple=FALSE)
        if (obj == "") stop("")
        objx <- get(obj, pos=1)
        if (!is.list(objx)) stop("must be list object")
      } else if (objresp == "rda") {
        objfn <- choose.files(default=getwd(), caption=caption,
			filters=Filters[c("rda", "All"),], multi=FALSE)
        if (objfn == "") stop("")
        objx <- get(load(objfn))
        if (!is.list(objx)) stop("must be list object")
      }
    }
    if (is.null(objx) && stopifnull) stop(paste(objnm, "is NULL"))
      return(NULL)
  }

  if (!is.null(obj)) {
    if (is.character(obj)) {
      if (exists(obj, envir=.GlobalEnv) && is.list(get(obj))) {
        #message(tab, " exists in Global Environment")
        return(get(obj))
      } else if (!is.na(getext(obj)) && file.exists(obj)) {
        objx <- get(load(obj))
        if (!is.list(objx)) stop("must be list object")
      } else if (!is.na(getext(obj)) && !file.exists(obj)) {
        stop("file does not exist")
      } else if (is.na(getext(obj))) {
        stop(objnm, " must be a list object or filename")
      } else {
        stop(objnm, " must be a list object or filename")
      }
    } else if (!is.list(obj)) {
      stop(objnm, " must be a list object or filename")
    } else {
      objx <- obj
    }
  }

  if (!is.null(list.items)) {
    if (!all(list.items %in% names(objx))) {
      missitems <- list.items[!list.items %in% names(objx)]
      stop(objnm, " must include the following item in list: ", toString(missitems))
    }
  }

  return(objx)
}


pcheck.output <- function(out_fmt="csv", out_dsn=NULL, outfolder=NULL,
	outfn.pre=NULL, outfn.date=FALSE, overwrite_dsn=FALSE,
	overwrite_layer=TRUE, add_layer=TRUE, append_layer=FALSE,
	createSQLite=TRUE, gui=FALSE) {

  ## Check out_fmt
  ###########################################################
  out_fmtlst <- c('sqlite', 'sqlite3', 'db', 'db3', 'gpkg', 'csv', 'gdb', 'shp')
  out_fmt <- pcheck.varchar(out_fmt, varnm="out_fmt", checklst=out_fmtlst,
		caption="Out format", gui=gui)

  ## Check for necessary packages
  ###########################################################
  if (out_fmt == "shp") {
    if (!"sf" %in% rownames(installed.packages())) {
      message("sf package is required for spExportSpatial")
    }
  } else if (out_fmt %in% c('sqlite', 'sqlite3', 'db', 'db3')) {
    if (!"RSQLite" %in% rownames(installed.packages())) {
      message("RSQLite package is required for exporting to sqlite or gpkg formats")
    }
  } else if (out_fmt %in% c('gdb')) {
    if (!"arcgisbinding" %in% rownames(installed.packages())) {
      message("arcgisbinding package is required for exporting to gdb format")
    }
    arcgisbinding::arc.check_product()
  }

  ## check outfn.date
  outfn.date <- pcheck.logical(outfn.date, varnm="outfn.date",
		title="outfn.date", first="NO", gui=gui)

  ## check overwrite_dsn
  overwrite_dsn <- pcheck.logical(overwrite_dsn, varnm="overwrite_dsn",
		title="overwrite_dsn", first="NO", gui=gui)

  ## check overwrite_layer
  overwrite_layer <- pcheck.logical(overwrite_layer, varnm="overwrite_layer",
		title="overwrite_layer", first="NO", gui=gui)

  ## check add_layer
  add_layer <- pcheck.logical(add_layer, varnm="add_layer",
		title="add data to dsn", first="NO", gui=gui)

  ## check append_layer
  append_layer <- pcheck.logical(append_layer, varnm="append_layer",
		title="append data", first="NO", gui=gui)


  ## check outfn.pre
  if (!is.null(outfn.pre) && (!is.vector(outfn.pre) || length(outfn.pre) > 1)) {
    stop("invalid outfn.pre")
  }

  ## check layer.pre
  #if (!is.null(layer.pre) && (!is.vector(layer.pre) || length(layer.pre) > 1)) {
  #  stop("invalid layer.pre")
  #}

  if (out_fmt %in% c("csv", "shp")) {
    outfolder <- pcheck.outfolder(outfolder)
    return(list(out_dsn=NULL, outfolder=outfolder, out_fmt=out_fmt,
		overwrite_layer=overwrite_layer, append_layer=append_layer,
		outfn.date=outfn.date, outfn.pre=outfn.pre))
  }
 
  ## Check file name
  ###########################################################
  chkfn <- checkfilenm(out_dsn, outfolder=outfolder)


  if (is.null(chkfn)) {
    ext <- "db"
    if (is.null(out_dsn)) {
      stop("out_dsn is NULL")
    }
    if (is.na(getext(out_dsn))) {
      if (out_fmt == "sqlite") {
        extlst <- c("sqlite", "db", "sqlite3", "db3")
      } else {
        extlst <- out_fmt
      }
      i <- 1
      while (is.null(chkfn) && i <= length(extlst)) {
        exttest <- extlst[i]
        chkfn <- checkfilenm(out_dsn, outfolder=outfolder, ext=exttest)
        if (!is.null(chkfn)) {
          ext <- exttest
        }
        i <- i + 1
      }
    }
  } else {
    ext <- getext(chkfn)
  }

  if (is.null(chkfn) || overwrite_dsn || !overwrite_dsn) {
    out_dsn <- getoutfn(out_dsn, outfn.pre=outfn.pre, outfolder=outfolder,
		outfn.date=outfn.date, overwrite=overwrite_dsn, outfn.default="data",
		ext=ext, add=add_layer, append=append_layer)

    if (out_fmt %in% c("sqlite", "gpkg") && createSQLite) {
      gpkg <- ifelse(out_fmt == "gpkg", TRUE, FALSE)
      out_dsn <- DBcreateSQLite(out_dsn, gpkg=gpkg)
    }
  } else {
    out_dsn <- chkfn
  }

  outfolder <- normalizePath(dirname(out_dsn))
  out_dsn <- basename(out_dsn)

  ## check append_layer
  if (append_layer) {
    overwrite_layer <- FALSE
  }
  if (overwrite_layer) {
    overwrite_dsn <- FALSE
  }

  return(list(out_fmt=out_fmt, outfolder=outfolder, out_dsn=out_dsn,
	overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
	add_layer=add_layer, append_layer=append_layer, outfn.date=outfn.date))
}


pcheck.colors <- function(colorlst, n) {

   if (!"RColorBrewer" %in% rownames(installed.packages())) {
     message("must install RColorBrewer package")
   }

   ## Check colorlst
   brewerlst <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3")
   brewercblst <- c("Dark2", "Paired", "Set2")
   if (!is.vector(colorlst) || !is.character(colorlst)) {
     stop("colorlst must be a character vector of color names, hexadecimal codes, or brewer palettes")
   }
   if (length(colorlst) == 1 && colorlst %in% brewerlst) {
     if (n < 3) {
       stop("minimum number for brewer palettes is 3")
     }
     colorlst <- RColorBrewer::brewer.pal(n, colorlst)
   } else if (length(colorlst) != n) {
     stop("number of colors is invalid")
   }
   return(colorlst)
}


pcheck.areaunits <- function(unitarea, areavar, areaunits, metric=FALSE) {
  ## Description: check areaunits for conversions

  gui <- FALSE
#  ## Check outunits
#  outunits <- pcheck.varchar(var2check=outunits, varnm="outunits",
#	gui=gui, checklst=c("ENGLISH", "METRIC"), caption="Area output units?",
#	stopifnull=TRUE)

  ## check metric
  metric <- pcheck.logical(metric, varnm="metric",
		title="Metric units", first="NO", gui=gui)
  if (metric) {
    outunits <- "hectares"
  } else {
    outunits <- "acres"
  }

  areausedvar <- checknm("AREAUSED", names(unitarea))
  unitarea[[areausedvar]] <- unitarea[[areavar]]
  if (areaunits != outunits) {
    if (areaunits == "acres" && outunits == "hectares") {
      unitarea[[areausedvar]] <- unitarea[[areausedvar]] * 0.4046860
    } else if (areaunits == "hectares" && outunits == "acres") {
      unitarea[[areausedvar]] <- unitarea[[areausedvar]] / 0.4046860
    } else {
      stop("invalid units... cannot convert ", areaunits, " to ", outunits)
    }
  }
  areavar <- areausedvar
  return(list(unitarea=unitarea, areavar=areavar, outunits=outunits))
}

pcheck.spatial <- function(layer=NULL, dsn=NULL, sql=NA, fmt=NULL, tabnm=NULL,
	caption=NULL, stopifnull=FALSE, gui=FALSE, polyfix=FALSE, asSpatial=FALSE,
	dropgeom=FALSE, stopifnoCRS=TRUE, checkonly=FALSE) {
  ## DESCRIPTION: checks or gets Vector layer from file name or spatial object.
  ## ARGUMENTS:
  ## dsn		String. The name of the database or path of shapefile (data source name)
  ##				or R Spatial Object.
  ## layer  	String. The name of layer in database or R Spatial Object.
  ## caption  	String. The text to add for gui window caption.
  ## checkonly	Logical. If TRUE, check layer only, do not return.

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type == "windows") {
    Filters <- rbind(Filters,shp=c("Shapefiles (*.shp)", "*.shp"))
    Filters <- rbind(Filters,gpkg=c("GeoPackages (*.gpkg)", "*.gpkg"))
    Filters <- rbind(Filters,gdb=c("Esri file geodatabase (*.gdb)", "*.gdb"))
    Filters <- rbind(Filters,sqlite=c("SQLite/Spatialite (*.sqlite)", "*.sqlite"))
  }

  ## Check for installed packages
  if (!is.null(fmt)) {
    if (fmt == "gdb") {
      if (!"arcgisbinding" %in% rownames(utils::installed.packages())) {
        message("importing spatial layers from *gdb requires package arcgisbinding")
      }
    }
  }
  fmtlst <- c("shp", "sqlite", "gpkg", "gdb")
  stringsAsFactors <- FALSE

  if (is.null(tabnm)) tabnm <- "layer"
  if (is.null(caption)) caption <- ""

  ## Return NULL
  if (is.null(layer) && is.null(dsn)) {
    if (checkonly) {
      return(FALSE)
    } else {
      return(NULL)
    }
  }

  ## Check layer - if sf object
  if (!is.null(layer)) {
    if (length(class(layer) == "list") && class(layer) == "list") {
      if (length(layer) != 1) {
        stop("invalid layer")
      } else {
        layer <- layer[[1]]
      }
    }
    if (any(c("sf", "data.frame") %in% class(layer))) {
      if (nrow(layer) == 0) {
        if (checkonly) {
          return(FALSE)
        } else {
          stop("no rows in layer")
        }
      } else {
        if (checkonly) {
          return(TRUE)
        } else {
          return(layer)
        }
      }
    } else if (methods::canCoerce(layer, "sf")) {
      return(sf::st_as_sf(layer, stringsAsFactors=stringsAsFactors))
    } else if (is.character(layer) && file.exists(layer)) {
      dsn <- layer
    }
  }
 
  if (!is.null(dsn)) {
    ext.dsn <- getext(dsn)
    if (!file.exists(dsn) && (is.na(ext.dsn) || ext.dsn == "NA")) {
      if (!is.null(fmt) && fmt %in% fmtlst)
        dsn <- paste(dsn, fmt, sep=".")
      if (!file.exists(dsn)) dsn <- NULL
    }
    if (ext.dsn %in% c("shp")) {
      if (checkonly) {
        return(TRUE)
      } else {
        layer <- basename.NoExt(dsn)
        return(spImportSpatial(layer=layer, dsn=dsn))
      }
    } else if (ext.dsn %in% c("csv")) {
      layer <- basename.NoExt(dsn)
    } else if (!gui && is.null(layer)) {
      stop("layer is NULL")
    }
  } else {
    ext.layer <- getext(layer)
    if (!is.na(ext.layer) && ext.layer != "NA" && file.exists(layer)) {
      if (ext.layer %in% c("shp", "csv")) {
        dsn <- layer
        layer <- basename.NoExt(dsn)
      } else {
        stop(ext.layer, " not supported")
      }
    } else {
      stop("layer is invalid")
    }
  }

  ######################################################
  ## Check dsn
  ######################################################
  if (gui && .Platform$OS.type=="windows") {
    if (is.null(dsn)) {
      fmt <- utils::select.list(fmtlst, title="dsn format?", multiple=FALSE)
      if (fmt == "gdb") {
        dsn <- choose.dir(default=getwd(), caption="Select database")
      } else if (fmt %in% c("sqlite", "gpkg")) {
        dsn <- choose.files(default=getwd(), caption="Select database")
      } else if (fmt == "shp") {
        dsn <- choose.files(default=getwd(), caption="Select database")
      } else {
        stop("format not currently supported")
      }
    } else {
      fmt <- ext.dsn
    }
    if (fmt %in% c("shp", "csv")) {
      layer <- basename.NoExt(dsn)
    } else {
      layerlst <- sf::st_layers(dsn)$name
      layer <- utils::select.list(layerlst, title="Select layer", multiple=FALSE)
    }
  }
 
  if (is.null(dsn)) {
    message("dsn is NULL")
    if (checkonly) {
      return(FALSE)
    } else {
      stop("dsn is NULL")
    }
  } 
 
  ######################################################
  ## Check layer
  ######################################################
  layerlst <- tryCatch(sf::st_layers(dsn),
				error=function(err) {
					#message("", "\n")
					return(NULL)
				} )
  if (is.null(layerlst)) {
    if (file.exists(dsn)) {
      stop("file exists... but not spatial")
    } else {
      stop("file does not exist")
    }
  }

  ## Note: if dsn is a SpatiaLite database, only spatial layers are listed
  if (!layer %in% layerlst$name) {
    if (checkonly) {
      return(FALSE)
    }
    if (ext.dsn == "sqlite") {
      return(pcheck.table(tab=layer, tab_dsn=dsn))
    } else {
      stop(layer, " is not in database")
    }
  } else {
    if (checkonly) {
      return(TRUE)
    }
  }
  geomtype <- layerlst$geomtype[layerlst$name == layer][[1]]

  if (!is.na(geomtype)) {
    if (!checkonly) {
      if (ext.dsn == "gdb") {
        if ("arcgisbinding" %in% rownames(utils::installed.packages())) {
          tabS4 <- arcgisbinding::arc.open(paste0(dsn, "/", layer))
          sql <- check.logic(names(tabS4@fields), sql, xvect=TRUE)
          tab <- tryCatch(arcgisbinding::arc.select(tabS4, where_clause=sql),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
          if (!is.null(tab)) {
            return(tab)
          } else {
            stop(layer, " is invalid")
          }
        } else {
          message("sql query not used")
          return(suppressWarnings(sf::st_read(dsn=dsn, layer=layer,
				stringsAsFactors=stringsAsFactors, quiet=TRUE)))
        }
      }
    } else {
      return(list(dsn=dsn, layer=layer))
    }
  } else {
    if (ext.dsn == "gdb" && !is.null(sql) && !is.na(sql) &&
		"arcgisbinding" %in% rownames(utils::installed.packages())) {
      arcgisbinding::arc.check_product()

      tabS4 <- arcgisbinding::arc.open(paste0(dsn, "/", layer))
      if (!is.na(sql) && !is.null(sql)) {
        sql <- check.logic(names(tabS4@fields), sql, xvect=TRUE)
        tab <- tryCatch(arcgisbinding::arc.select(tabS4, where_clause=sql),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
        if (is.null(tab)) {
          stop(layer, " is invalid")
        }
        splayer <- arcgisbinding::arc.data2sf(tab)
      } else {
        splayer <- suppressWarnings(sf::st_read(dsn=dsn, layer=layer,
				stringsAsFactors=stringsAsFactors, quiet=TRUE))
      }
    } else {
      splayer <- suppressWarnings(sf::st_read(dsn=dsn, layer=layer,
				stringsAsFactors=stringsAsFactors, quiet=TRUE))
    }
  }

  if ("sf" %in% class(splayer)) {

    if (nrow(splayer) == 0 && stopifnull) {
      msg <- "layer has 0 records"
      if (stopifnull) {
        stop(msg)
      } else {
        message(msg)
      }
    }

    ## Check if projection
    ############################################################
    if (is.na(sf::st_crs(splayer))) {
      msg <- paste("projection is not defined for:", dsn)
      if (stopifnoCRS) {
        stop(msg)
      } else {
        message(msg)
      }
    }

    ## If polyfix
    ############################################################
    if (polyfix) {
      splayer <- polyfix.sf(splayer)
    }

    ## Drop geometry in table
    ############################################################
    if (dropgeom) {
      splayer <- sf::st_drop_geometry(splayer)
    }
  }

  if (checkonly) {
    return(list(dsn=dsn, layer=layer))
  } else {
    return(splayer)
  }
}



pcheck.params <- function(input.params, strata_opts=NULL, 
			unit_opts=NULL, table_opts=NULL, title_opts=NULL, 
			savedata_opts=NULL, multest_opts=NULL) {
  ## DESCRIPTION: function to check input list parameters

  if (!is.null(strata_opts)) {
    if ("strata_opts" %in% input.params) {
      if (!is.list(strata_opts)) {
        strata_opts <- as.list(strata_opts)
      }
      if (is.null(names(strata_opts))) {
        stop("invalid strata_opts... see strata_options()")
      }
      formallst.strata <- names(formals(FIESTA::strata_options))[-length(formals(FIESTA::strata_options))]
      strata.params <- names(strata_opts)[!names(strata_opts) %in% c("formallst", "input.params")]
      if (!all(strata.params %in% formallst.strata)) {
        miss <- strata.params[!strata.params %in% formallst.strata]
        stop("invalid parameter: ", toString(miss))
      }
    }
  }
 
  if (!is.null(unit_opts)) {
    if ("unit_opts" %in% input.params) {
      if (!is.list(unit_opts)) {
        unit_opts <- as.list(unit_opts)
      }
      if (is.null(names(unit_opts))) {
        stop("invalid unit_opts... see unit_options()")
      }
      formallst.unit <- names(formals(FIESTA::unit_options))[-length(formals(FIESTA::unit_options))]
      unit.params <- names(unit_opts)[!names(unit_opts) %in% c("formallst", "input.params")]
      if (!all(unit.params %in% formallst.unit)) {
        miss <- unit.params[!unit.params %in% formallst.unit]
        stop("invalid parameter: ", toString(miss))
      }
    }
  }
  if (!is.null(table_opts)) {
    if ("table_opts" %in% input.params) {
      if (!is.list(table_opts)) {
        table_opts <- as.list(table_opts)
      }
      if (is.null(names(table_opts))) {
        stop("invalid table_opts... see table_options()")
      }
      formallst.table <- names(formals(FIESTA::table_options))[-length(formals(FIESTA::table_options))]
      table.params <- names(table_opts)[!names(table_opts) %in% c("formallst", "input.params")]
      if (!all(table.params %in% formallst.table)) {
        miss <- table.params[!table.params %in% formallst.table]
        stop("invalid parameter: ", toString(miss))
      }
    }
  }
  if (!is.null(title_opts)) {
    if ("title_opts" %in% input.params) {
      if (!is.list(title_opts)) {
        title_opts <- as.list(title_opts)
      }
      if (is.null(names(title_opts))) {
        stop("invalid title_opts... see title_options()")
      }
      formallst.title <- names(formals(FIESTA::title_options))[-length(formals(FIESTA::title_options))]
      title.params <- names(title_opts)[!names(title_opts) %in% c("formallst", "input.params")]
      if (!all(title.params %in% formallst.title)) {
        miss <- title.params[!title.params %in% formallst.title]
        stop("invalid parameter: ", toString(miss))
      }
    }
  }
  if (!is.null(savedata_opts)) {
    if ("savedata_opts" %in% input.params) {
      if (!is.list(savedata_opts)) {
        savedata_opts <- as.list(savedata_opts)
      }
      if (is.null(names(savedata_opts))) {
        stop("invalid savedata_opts... see savedata_options()")
      }
      formallst.savedata <- names(formals(FIESTA::savedata_options))[-length(formals(FIESTA::savedata_options))]
      savedata.params <- names(savedata_opts)[!names(savedata_opts) %in% c("formallst", "input.params")]
      if (!all(savedata.params %in% formallst.savedata)) {
        miss <- savedata.params[!savedata.params %in% formallst.savedata]
        stop("invalid parameter: ", toString(miss))
      }
    }
  }
  
  if (!is.null(multest_opts)) {
    if ("multest_opts" %in% input.params) {
      if (!is.list(multest_opts)) {
        multest_opts <- as.list(multest_opts)
      }
      if (is.null(names(multest_opts))) {
        stop("invalid multest_opts... see multest_options()")
      }
      formallst.multest <- names(formals(FIESTA::multest_options))[-length(formals(FIESTA::multest_options))]
      multest.params <- names(multest_opts)[!names(multest_opts) %in% c("formallst", "input.params")]
      if (!all(multest.params %in% formallst.multest)) {
        miss <- multest.params[!multest.params %in% formallst.multest]
        stop("invalid parameter: ", toString(miss))
      }
    }
  }

}

