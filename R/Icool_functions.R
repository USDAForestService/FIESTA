# checkfilenm
# getoutfn
# addcommas
# pastevars
# stopQ
# removecols
# DT_NAto0
# changeNull
# getdups
# getlistparam
# getnm
# checknm			If nm exists, changes to nm_*
# check.namedlist 
# capfirst
# nbrdecimals
# nbrdigits
# getfilter
# filter2qry
# wraptitle
# addclass
# xtabf
# recodelut
# findnm
# strat.pivot

checkfilenm <- function(fn, outfolder=NULL, ext=NULL, 
	stopifnull=FALSE) {

  if (is.null(fn)) {
    #message("file name is NULL")
    if (stopifnull) {
      stop()
    } else {
      return(NULL)
    }
  }
  if (!is.character(fn)) {
    message("file name must be a character string")
  }
  if (is.null(outfolder)) {
    if (file.exists(fn)) {
      return(normalizePath(fn)) 
    } else {
      outfolder <- normalizePath(dirname(fn))
      fn <- basename(fn)
    } 
  }

  outfolder <- pcheck.outfolder(outfolder)
  if (file.exists(file.path(outfolder, fn))) {
    return(file.path(outfolder, fn))
  } else if (!is.null(ext)) {
    if (substring(ext, 1, 1) != ".") {
      ext <- paste0(".", ext)
    }
    if (!file.exists(file.path(outfolder, paste0(fn, ext)))) {
      if (stopifnull) {
        stop("file name does not exist")
      } else {
        return(NULL)
      }
    } else {
      return(file.path(outfolder, paste0(fn, ext)))
    }
  } else {
    return(NULL)
  }
}
    

getoutfn <- function(outfn, outfolder=NULL, outfn.pre=NULL, outfn.date=FALSE, 
	overwrite=FALSE, ext=NULL, baseonly=FALSE, noext=FALSE, 
	outfn.default="outfile", add=TRUE, append=FALSE, gui=FALSE) {
  ## DESCRIPTION: get full pathname 

  ## Check outfn
  if (is.null(outfn)) {
    if (!is.null(outfn.default)) {
      outfn <- outfn.default
    } else {
      stop("outfn and outfn.default is null")
    }
  }
  if (!is.character(outfn))
    stop("outfn must be a character string")
  extfn <- getext(outfn) 

  ## Check ext
  extlst <- c("sqlite", "sqlite3", "db", "db3", "csv", "txt", "gdb", 
		"shp", "gpkg", "jpg", "png", "tif", "img", "pdf", "rda")
  if (!is.na(extfn) && extfn %in% extlst) {
    ext <- extfn
  } else if (!is.null(ext)) {
    if (startsWith(ext, ".")) 
      ext <- sub(".", "", ext)
    if (!ext %in% extlst)
      stop("ext not supported")

    if (is.na(getext(outfn)) || !extfn %in% extlst)
      outfn <- paste0(outfn, ".", ext)
  } else if (is.na(getext(outfn))) {
    stop("specify out_format") 
  } else {
    stop(extfn, " not supported")
  }     

  ## Get basename
  outfn.base <- basename.NoExt(outfn)
  outfn.dir <- dirname(outfn)
  if (outfn.dir != ".") {
    if (is.null(outfolder)) {
      outfolder <- outfn.dir
    } else {
      if (dir.exists(file.path(outfolder, outfn.dir))) {
        outfolder <- file.path(outfolder, outfn.dir)
      }
    }
  }  

  ## Check if outfolder
  if ((is.na(extfn) || extfn=="NA") && dir.exists(outfn)) {
    message("outfn is a folder name... must be a file name")
    return(outfn)
  } 
  if (is.null(outfolder)) {
    if (!dir.exists(dirname(outfn)) && !dir.exists(dirname(normalizePath(outfn)))) {
      stop(outfn, " does not exist")
    } else if (dir.exists(dirname(outfn))) {
      outfolder <- dirname(outfn)
    } else {
      outfolder <- dirname(normalizePath(outfn))
    }
  }
     
  ## Check outfn.pre
  if (!is.null(outfn.pre) && is.character(outfn.pre)) {
    outfn.base <- paste(outfn.pre, outfn.base, sep="_")
  }
  ## DESCRIPTION: gets outfile name
  if (outfn.date) {
    outfn.base <- paste0(outfn.base, "_", format(Sys.time(), "%Y%m%d"))
  }
  ## Get full path filename
  outfolder <- pcheck.outfolder(outfolder, gui=gui)  
  outfilenm <- file.path(outfolder, outfn.base)
   
  if (overwrite) {
    nm <- paste0(outfilenm, ".", ext)
    if (file.exists(nm)) {
      test <- tryCatch(
        file.remove(nm),
			warning=function(war) {
             			#stop(war,"\n")
             			stop("cannot overwrite file... permission denied\n")
			}, error=function(err) {
					message(err)
			} )
      if (is.null(test)) 
        stop("permission denied") 
      message("overwriting ", nm, "...")
    } 
  } else if (!append && !add) {
    outfn.base <- FIESTA::fileexistsnm(outfolder, outfn.base, ext)
  }
  if (!baseonly) {
    ## Check outfolder
    #outfolder <- pcheck.outfolder(outfolder, gui=gui) 
    outfilenm <- file.path(normalizePath(outfolder), outfn.base)
  } else {
    outfilenm <- outfn.base
  } 
  if (!noext) {
    ext <- ifelse(ext=="sqlite", "db", ext)
    if (substring(ext, 1, 1) == ".") {
      outfilenm <- paste0(outfilenm, ext)
    } else {
      outfilenm <- paste0(outfilenm, ".", ext)
    }
  }
  return(outfilenm)
}


addcommas <- function(vars, ALIAS=NULL, sepchar=",", quotes=FALSE, paren=FALSE){
  ## DESCRIPTION:
  ## Internal function to input a vector of string variables and outputs a string of  
  ##  variables separated by commas. If an alias is included, the function will 
  ##   concatenate the alias before each variable before a dot. 
  ## ARGUMENTS:
  ##   vars - Sring vector. 
  ##  ALIAS -String. A shortname to put in front of variable before a dot (ex. ALIAS.var)
  ## VALUE:
  ##  A string of variables separated by commas and with or without alias.
  ## EXAMPLE:
  ##   avector <- c("CAT", "DOG", "MOUSE")
  ##  addcommas(avector)
  ##  addcommas(avector, "a")

  if (is.null(vars)) return(NULL)

  if (!is.null(ALIAS)) {
    xvars <- paste(ALIAS, vars, sep=".")
  } else {
    xvars <- vars
  }
  if (quotes) {
    newvars <- paste0("'", xvars[1], "'")
    if (length(xvars) > 1)
      for (j in 2:length(xvars)) newvars <- paste0(newvars, sepchar, "'", xvars[j], "'") 

  } else {
    newvars <- paste(xvars, collapse=sepchar)

    #newvars <- xvars[1]
    #if(length(xvars) > 1){
    #  for(j in 2:length(xvars)) { newvars <- paste(newvars, sepchar, xvars[j], sep=" ") }
    #}
  }
  if (paren) newvars <- paste0("(", newvars, ")")

  return(newvars)
}

pastevars <- function(vars1, vars2, sep=",") {
  if (is.null(vars1) && is.null(vars2)) {
    return(NULL)
  } else if (is.null(vars1)) { 
    return(vars2)
  } else if (is.null(vars2)) {
    return(vars1)
  } else {
    paste(vars1, vars2, sep=sep)
  }
}

stopQ <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

removecols <- function (x, vars) {
  ## DESCRIPTION: Removes columns by name from data frame
  x[,which(!names(x) %in% vars)]
}

DT_NAto0 <- function (DT, cols, changeto=0) {
  ## DESCRIPTION: Change NA values to 0 values in data.table

  if (!any(class(DT) %in% "data.table")) DT <- setDT(DT)

  cols <- cols[which(cols %in% names(DT))]

  for (col in cols) {
    if (is.logical(DT[[col]]) && all(is.na(DT[[col]])))
      DT[[col]] <- as.numeric(DT[[col]])

    if (is.numeric(DT[[col]])) {
      if (is.null(changeto)) changeto <- 0
      if (is.character(changeto)) {
        DT[[col]] <- as.character(DT[[col]])
        message("changed ", col, " to character")
      }
      set(DT, which(is.na(DT[[col]])), col, changeto) 
    } else {
      if (is.null(changeto)) changeto <- 0
      if (is.factor(DT[[col]])) {
        DT[[col]] <- as.character(DT[[col]])
        set(DT, which(is.na(DT[[col]])), col, changeto) 
      } else {
        set(DT, which(is.na(DT[[col]])), col, changeto) 
      }
    } 
  }
  return(data.table(DT))
}


changeNULL <- function(x, xvar, changeto=NULL){
  ## DESCRIPTION: changes NULL values to the value of changeto

  for(var in xvar){
    if(is.null(changeto)){
      changeto <- "NONFOREST"

      ## CHANGE rowvar domain values from NA values to 0 values
      if(is.numeric(x[,var])){
        x[is.na(x[,var]), var] <- 0
      }else if(is.factor(x[,var])){
        x[,var] <- as.character(x[,var])
        x[is.na(x[,var]),var] <- changeto
      }else{
        x[is.na(x[,var]),var] <- changeto
      }
    }else{
      if(is.factor(x[,var])){
        x[,var] <- as.character(x[,var])
        x[is.na(x[,var]),var] <- changeto
      }else{
        x[is.na(x[,var]) | is.null(x[,var]) | x[,var] == "", var] <- changeto
      }
    }
  }

  return(x)
}
 

getdups <- function(cx, cuniqueid="PLT_CN", varnm, fun) {
  ## DESCRIPTION: FUNCTION TO GET DUPLICATE PLOTS
 
  undups <- aggregate(cx[,varnm], list(cx[,cuniqueid]), fun)
  names(undups) <- c(cuniqueid, varnm)
  conddups <- merge(cx, undups, by=c(cuniqueid, varnm))
  return(conddups)
}


getlistparam <- function(lst) 
  paste0("list(",toString(paste(names(lst), lst, sep="=")),")")


getnm <- function (xvar, group=FALSE) { 
  ## DESCRIPTION: creates a name variable from a code variable. 
  ## If 'CD' is at the end of the variable name, changes CD to NM, else adds NM 
  ## to variable name.

  CDchar <- as.vector(gregexpr("CD", xvar)[[1]])
  if (CDchar > 0) {
    xvarnm <- xvar
    substring(xvarnm, CDchar[length(CDchar)], CDchar[length(CDchar)]+1) <- "NM"
    if (group) {
      pre <- substr(xvar, 1, CDchar[length(CDchar)]-1)
      post <- substr(xvar, CDchar[length(CDchar)]+2, nchar(xvar))
      grpcode <- paste0(pre, "GRPCD", post)
      grpname <- paste0(pre, "GRPNM", post)
    }
  } else {
    xvarnm <- paste0(xvar, "NM")
    if (group) {
      grpcode <- paste0(xvar, "GRPCD")
      grpname <- paste0(xvar, "GRPNM")
    }
  }
  xnames <- list(xvarnm = xvarnm)
  if (group) {
    xnames$grpcode <- grpcode
    xnames$grpname <- grpname
  }
  return(xnames)
}


checknm <- function(nm, nmlst) {
  ## if nm already exists in nmlst, change nm to nm_*
  i <- 0
  while (nm %in% nmlst) {
    i <- i + 1
    nm <- paste(nm, i, sep="_")
    message("name exists... changed name to ", nm)
  } 
  return(nm)
}


  

check.namedlist <- function(xlst, checknms=NULL, modetype="numeric") {
  # xlst		String. Name of list.
  # checknms	String vector. Names to check with list names.

  x <- get(xlst)
  if (class(x)[1] != "list") 
    stop(xlst, " must be a named list")

  ## Check if xlst is a list
  xnms <- names(x)
  if (any(is.null(xnms))) 
    stop(paste(xnms, "must be a named list"))

  ## Check if all names in checknms
  if (!is.null(checknms)) {
    if (!all(xnms %in% checknms))
      stop(xlst, " has invalid names")
  }

  ## Check if all values have correct modetype
  if (any(lapply(x, mode) != modetype))
    stop(xlst, " must be ", modetype)
}


capfirst <- function(x, allwords=FALSE){
  ## DESCRIPTION: Internal function to capitalize first character.
  ## allwords - If allwords=TRUE, capitalize first letter of each word

  capfun <- function(y) {
    substring(y, 1, 1) <- toupper(substring(y, 1, 1))
    return(y)
  }

  x <- tolower(x)
  if (allwords) {
    x2 <- strsplit(x, " ")
    x2 <- lapply(x2, capfun)
    x <- unlist(lapply(x2, paste, collapse=" "))
  } else {
    x <- capfun(x)
  }
  return(x)
}


nbrdecimals <- function(x) {
#  if ((x %% 1) != 0) {
#    strs <- strsplit(as.character(format(x, scientific = F)), "\\.")
#    n <- nchar(strs[[1]][2])
#  } else {
#    n <- 0
#  }
  strs <- strsplit(as.character(format(x, scientific = F)), "\\.")
  n <- nchar(strs[[1]][2])
  if (is.na(n)) n <- 0
  return(n) 
}

nbrdigits <- function(x) {
  ## DESCRIPTION: get max number of digits in front of decimal point
  ## x - vector of numbers

  max(nchar(sapply(strsplit(as.character(x), "\\."), "[[", 1)), na.rm=TRUE) 
}



getfilter <- function(att, val, syntax="R") {
## DESCRIPTION: create filter string from att and val
## syntax - ('R', 'sql')
  if (is.character(val)) {
    val <- encodeString(val, quote="'")
  }
  filter <- paste0(att, " %in% c(", toString(val), ")")

  if (syntax == 'sql') {
    filter <- gsub("%in% c", "in", filter)
  }
  return(filter)
}


filter2qry <- function(filt, layernm) {
  if (grepl("==", filt)) {
    part2 <- sub("==", "=", filt)
  } else if (grepl("%in%", filt)) {
    part2 <- sub("%in% c", "in", filt)
  } else if (grepl("!=", filt)) {
    part2 <- sub("!=", "<>", filt)
  }
  paste("select * from", layernm, "where", part2)
}


wraptitle <- function(x, len=10) {
  sapply(x, function(y) paste(strwrap(y, len), 
         collapse = "\n"), USE.NAMES = FALSE)
}


addclass <- function(x, xtab, xvar, brks) {
  ## DESCRIPTION: Adds a class based on breaks to a table in modGBpop
  x[[xtab]] <- datLUTclass(x=x[[xtab]], xvar=xvar, cutbreaks=brks)$xLUT
  return(x)
}

xtabf <- function(x, y, levels) {
  table(factor(x, levels=levels), factor(y, levels=levels))
}


recodelut <- function(lut, minvar="min", maxvar="max", classvar="class") {
  ## DESCRIPTION: converts lut with min/max values for continuous data to a 
  ## lookup table by value
  lut2 <- lapply(lut[[classvar]], function(x, lut) {
          data.frame(value=c(lut[lut[[classvar]] == x, minvar]:lut[lut[[classvar]] == x, maxvar]),
 				class=rep(x))
      	}, lut)
  lut2 <- do.call(rbind, lut2)
  return(lut2)  
}

findnm <- function(x, xvect, returnNULL=FALSE) {
  test <- grepl(x, xvect, ignore.case=TRUE)
  if (sum(test) == 0) {
    if (returnNULL) {
      return(NULL)
    }
    stop("name not found")
  } else if (sum(test) > 1) {
    testnames <- xvect[test]
    test <- match(x, testnames)
    if (length(test) == 1) {
      return(testnames[test])
    } else {
      stop("more than 1 name found")
    }
  } else {
    return(xvect[test])
  }
}


strat.pivot <- function(x, strvar, unitvars, strwtvar="Prop", strat.levels=NULL) {
  ## DESCRIPTION: translates strata table from spGetAuxiliary() to spGetStrata() format 

  if (!"data.table" %in% class(x)) {
    x <- setDT(x)
  } 
  nmlst <- names(x)
  PScols <- nmlst[grep(strvar, nmlst)]
  PSvalslst <- sapply(strsplit(PScols, paste0(strvar, ".")), "[[", 2)

  strlut <- data.table(PSvalslst, x[, t(.SD), by=unitvars, .SDcols=PScols])
  setnames(strlut, c(strvar, unitvars, strwtvar))
  setcolorder(strlut, c(unitvars, strvar, strwtvar))
  if (is.null(strat.levels)) {
    strlut[[strvar]] <- factor(strlut[[strvar]])
  } else {
    strlut[[strvar]] <- factor(strlut[[strvar]], levels=strat.levels)
  }
  strvars <- strvar
  return(strlut)
}    

  