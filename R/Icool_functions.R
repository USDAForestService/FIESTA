# removecols
# DT_NAto0
# changeNull
# getdups
# getlistparam
# getnm
# checknm			If nm exists, changes to nm_*
# check.namedlist 
# capfirst



removecols <- function (x, vars) {
  ## DESCRIPTION: Removes columns by name from data frame
  x[,which(!names(x) %in% vars)]
}


DT_NAto0 <- function (DT, cols, changeto=NULL) {
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

check.numeric <- function(x, cols) {
  for (col in cols) {
    if (!is.numeric(x[[col]])) {
      message(paste(col, "is not numeric, converting..."))
      if (is.factor(x[[col]])) {
        x[[col]] <- as.character(x[[col]])
        x[[col]] <- as.numeric(x[[col]])
      } else {
        x[[col]] <- as.numeric(x[[col]])
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
  if (length(CDchar) > 0) {
    xvarnm <- xvar
    substring(xvarnm, CDchar[length(CDchar)], CDchar[length(CDchar)]+1) <- "NM"
    if (group) {
      pre <- substr(xvar, 1, CDchar[length(CDchar)]-1)
      post <- substr(xvar, CDchar[length(CDchar)]+2, nchar(xvar))
      grpcode <- paste0(pre, "GRPCD", post)
      grpname <- paste0(pre, "GRPNM", post)
    }
  } else {
    newname <- paste0(xvar, "NM")
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



