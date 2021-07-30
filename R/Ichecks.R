## check.numeric
## check.logic
## check.matchclass
## check.matchval

check.numeric <- function(x) {
  ## DESCRIPTION: check if vector x is numeric. If not, change to numeric.

  if (!all(is.na(x)) && !is.numeric(x)) {
    if (is.factor(x)) {
      x <- as.numeric(as.character(x))
    } else {
      x <- as.numeric(x)
    }
  }
  return(x)
}

#check.numeric <- function(x, cols) {
#  for (col in cols) {
#    if (!is.numeric(x[[col]])) {
#      message(paste(col, "is not numeric, converting..."))
#      if (is.factor(x[[col]])) {
#        x[[col]] <- as.numeric(as.character(x[[col]]))
#      } else {
#        x[[col]] <- as.numeric(x[[col]])
#      }
#    }
#  }
#  return(x)
#}


check.logic <- function(x, statement, filternm=NULL, stopifnull=FALSE, 
	stopifinvalid=TRUE, removeinvalid=FALSE, returnvar=FALSE){
  ## DESCRIPTION: checks logical statement
  ## ARGUMENTS"
  ## x 	- data frame to check column names
  ## statement - logical statement
  ## filternm  - name to use in messages
  ## stopifnull - if statement=NULL, stop
  ## stopifinvalid - if statement is not valid (i.e., columns names do not 
  ##		match any name in logical statement)
  ## removeinvalid - if TRUE, and a part of a multipart logical statement 
  ## 		is invalid, the invalid part is removed, returning only valid part.
  ## 	    	Note: an example is if using statement for trees and seedlings:
  ##		"STATUSCD == 1 & SPCD = 475". Since there is no STATUSCD == 1 in 
  ##		the seedling table, it is removed from logical statement.
 
  if (is.null(statement)) return(NULL)
  
  ## Define potential characters in logical statement		
  logic.chars <- c("==", "<=", ">=", "%in%", "<", ">", "!=", "is.na", "is.null")

  ## Set warning response
  filternm <- ifelse(!is.null(filternm), filternm, "filter")
  fwarning <- paste(filternm, "is invalid")

  ## Check statement, assuming not NULL
  if (!is.character(statement) | statement == "") stop(fwarning)
  if (statement != "NONE") {
    if (!any(unlist(sapply(logic.chars, 
		function(x, statement){grep(x, statement)}, statement)))) {
      if (grepl("=", statement) && sum(gregexpr("==", statement)>0) == 0) {
        message("must be R syntax.. changing '=' to '==' ")
        statement <- gsub("=", "==", statement)
      }
    }
    if (grepl("&&", statement)) {
      statement <- gsub("&&", "&", statement)
    }
    if (grepl("\\|\\|", statement)) {
      statement <- gsub("\\|\\|", "\\|", statement)
    }
    if (grepl("&", statement) || grepl("\\|", statement)) {
      parts <- {}
      if (grepl("&", statement)) {
        parts <- unlist(strsplit(statement, "&"))
      } else if (grepl("\\|", statement)) {
        parts <- unlist(strsplit(statement, "\\|"))
      }
      ## Check if there are any variables in x that match filter
      chkparts <- sapply(parts, function(y, x){ 
				sum(sapply(names(x), 
				function(xnm, y){ grepl(xnm, y) }, y)) }, x)
      if (any(chkparts == 0)) {
        if (any(chkparts > 0) && removeinvalid) {
          statement <- trimws(names(chkparts)[chkparts > 0])
        } else if (stopifinvalid) {
          stop(fwarning)
        } else {
          return(NULL)
        }
      }
    } else {
      ## Check if there are any variables in x that match filter
      chk <- sum(sapply(names(x), function(x, y){ grepl(x, y) }, statement))
          
      if (chk == 0) {
        if (stopifinvalid) {
          stop(fwarning)
        } else {
          return(NULL)
        }
      } else {
        if (returnvar) {
          return(names(x)[sapply(names(x), function(x, y){ grepl(x, y) }, statement)])
        }
      }
    } 
  }
  return(statement)
}

check.matchclass <- function(tab1, tab2, matchcol, var2=NULL, tab1txt=NULL, tab2txt=NULL) {
  ## Description: check that the class of matchcol in tab2 matches class in tab1
  ## 	  If different, changes class2 to class1
  ##	  For multiple variables with same name in both tables,
  ##		or, 1 variable with different name in tables (var2=NULL)
  ## ARGUMENTS:
  ## tab1 - data frame of first table
  ## tab2 - data frame of matching table
  ## matchcol - column to match
  ## var2 - if the matching variable is named different
  ## tab1txt - text for table 1
  ## tab2txt - text for table 2

  if (is.null(tab1txt)) tab1txt <- "tab1"
  if (is.null(tab2txt)) tab2txt <- "tab2"

  if (is.null(tab1) || is.null(tab2)) stop("invalid tables")
  
  if (length(matchcol) > 1 && !all(matchcol %in% names(tab1))) {
    nomatch <- matchcol[which(!matchcol %in% names(tab1))]  
    stop(paste(paste(nomatch, collapse=", "), "not in", tab1txt))
  }
  if (is.null(var2) && !matchcol %in% names(tab2)) 
    stop(paste(matchcol, "not in", tab2txt))
  if (!is.null(var2) && !var2 %in% names(tab2)) 
    stop(paste(var2, "not in", tab2txt))

  if (is.data.table(tab1)) {
    tab1key <- key(tab1)
    setkey(tab1, NULL)
  }
  if (is.data.table(tab2)) {
    tab2key <- key(tab2)
    setkey(tab2, NULL)
  }

  if (!is.null(var2)) {
    if (length(matchcol) > 1) stop("only 1 matchcol allowed if different names")
  }
  for (v in matchcol) {
    if (!is.null(var2)) {
      v1 <- v
      v2 <- var2
    } else {
      v1 <- v
      v2 <- v
    }
    ## if both classes are factors, check levels
    if (class(tab1[[v1]]) == "factor" && class(tab2[[v2]]) == "factor") {
      v1levels <- levels(tab1[[v1]])
      v2levels <- levels(tab2[[v2]])
      if (!identical(v1levels, v2levels)) {
        message("factor levels do not match")
        if (all(v2levels %in% v1levels)) 
          levels(tab2[[v2]]) <- v1levels
      }        
    } else if (class(tab1[[v1]]) == "factor") {
      v1levels <- levels(tab1[[v1]])
      if (all(v1levels %in% tab2[[v2]])) {
        tab2[[v2]] <- factor(tab2[[v2]], levels=levels(tab1[[v1]])) 
      } else {
        #message("missing factor levels")
        tab2[[v2]] <- factor(tab2[[v2]], levels=levels(tab1[[v1]])) 
      }  
    } else if (class(tab2[[v2]]) == "factor") {  
      tab2[[v2]] <- as.character(tab2[[v2]])
    }
    
    tab1.class <- class(tab1[[v1]])
    tab2.class <- class(tab2[[v2]])
 
    ## coercion order: logical-integer-numeric-complex-character
    coerce.order <- c("logical", "integer", "integer64", "numeric", "complex", "character")
    tab.order <- na.omit(match(c(tab1.class, tab2.class), coerce.order)) 

    if (length(tab.order) == 2) {
      if (tab.order[1] < tab.order[2]) {
        if (coerce.order[tab.order[2]] == "integer64") {
          fun2 <- as.integer
        } else {        
          fun2 <- get(paste0("as.", coerce.order[tab.order[2]]))
        }
        tab1[[v1]] <- fun2(tab1[[v1]]) 
      } else if (tab.order[2] < tab.order[1]) {
        if (coerce.order[tab.order[1]] == "integer64") {
          fun1 <- as.integer
        } else {        
          fun1 <- get(paste0("as.", coerce.order[tab.order[1]]))
        }
        tab2[[v2]] <- fun1(tab2[[v2]]) 
      }
    } else if (tab1.class %in% coerce.order) {
      if (tab1.class == "integer64") {
        fun1 <- as.integer
      } else {        
        fun1 <- get(paste0("as.", tab1.class))
      }
      tab1[[v1]] <- fun1(tab1[[v1]])          
    } else if (tab2.class != tab1.class) {
      class(tab2[[v2]]) <- tab1.class
    }
  }

  if (is.data.table(tab1))
    setkeyv(tab1, tab1key)
  if (is.data.table(tab2))
    setkeyv(tab2, tab2key)

  return(list(tab1=tab1, tab2=tab2))
}


check.matchval <- function(tab1, tab2, var1, var2=NULL, tab1txt=NULL, tab2txt=NULL, 
	gui=FALSE, stopifmiss=FALSE, subsetrows=FALSE) {

  ## Description: check that the values of var1 in tab1 are all in matchcol in tab2
  ## 	  If missing values, stops and sends error.
  ##	  var1 and var2 must be the same length, if greater than 1, columns are concatenated
  ##   Does not return anything
  ## var1 - variable name in tab1
  ## var2 - variable name in tab2
  ## stopifmiss - stop if the number of missing values is greater than 1
  ## subsetrows - remove rows of tab1 with missing values and return table

  nbr.missval=missval=MATCH <- NULL

  ## Check tables
  tab1 <- FIESTA::pcheck.table(tab1, gui=gui)
  if (is.null(tab1)) stop("tab1 is NULL")

  tab2 <- FIESTA::pcheck.table(tab2, gui=gui)
  if (is.null(tab2)) stop("tab2 is NULL")

  if (is.null(tab1txt)) tab1txt <- "tab1"
  if (is.null(tab2txt)) tab2txt <- "tab2"

  ## Check names in tables
  ## Check var1 and var2 parameters
  if (!all(var1 %in% names(tab1))) {
    misscol <- var1[which(!var1 %in% names(tab1))]
    stop(paste0("missing variables in ", tab1txt, ": ", paste(misscol, collapse=", ")))
    #if (is.factor(tab1[[var1]])) tab1[[var1]] <- as.character(tab1[[var1]])
  } 
  if (!is.null(var2)) {
    if (!all(var2 %in% names(tab2))) {
      misscol <- var2[which(!var2 %in% names(tab2))]
      stop(paste0("missing variables in ", tab2txt, ": ", paste(misscol, collapse=", ")))
      #if (is.factor(tab2[[var2]])) tab2[[var2]] <- as.character(tab2[[var2]])
    } 
    if (length(var2) != length(var1)) 
      stop("number of variables in var2 must equal number of variables in var1")
  } else {
    var2 <- var1
  }
 
  ## Get unique values of matchcol in tab1
  if (length(var1) > 1) {
    tab1.vals <- unique(na.omit(tab1[, do.call(paste, .SD), .SDcols=var1]))
    tab2.vals <- unique(na.omit(tab2[, do.call(paste, .SD), .SDcols=var2]))

    missval <- tab1.vals[which(!tab1.vals %in% tab2.vals)]
    nbr.missval <- length(missval)
    if (nbr.missval == length(tab1.vals)) {
      stop("no matching values in ", tab1txt, " and ", tab2txt)
    } else if (nbr.missval > 0) {
      if (nbr.missval < 20) {
        message(paste0(nbr.missval, " values in ", tab1txt, " not in ", tab2txt, ": ", 
	 	paste(missval, collapse=", "))) 
      } else {
        message(paste("there are", nbr.missval, "values in", tab1txt, 
		"not in", tab2txt))
      }
      if (stopifmiss) {
        message("ERROR"); stopQ()  }
    }
    if (subsetrows) {
      tab1[, MATCH:= do.call(paste, .SD), .SDcols=var1]
      tab1 <- tab1[!MATCH %in% missval, ]
      tab1[, MATCH := NULL]
    }
  } else {
    tab1.vals <- na.omit(unique(tab1[[var1]]))
    tab2.vals <- na.omit(unique(tab2[[var2]]))

    missval <- tab1.vals[which(!tab1.vals %in% tab2.vals)]
    nbr.missval <- length(missval)

    if (nbr.missval == length(tab1.vals)) {
      stop("no matching values in ", tab1txt, " and ", tab2txt)
    } else if (nbr.missval > 0) {
      if (nbr.missval < 20) {
        message(paste0(nbr.missval, " values in ", tab1txt, " not in ", 
			tab2txt, ": ", paste(missval, collapse=", "))) 
      } else {
        message(paste("there are", nbr.missval, "missing values in", 
			tab1txt))
      }
      if (stopifmiss) {
        message("ERROR"); stopQ()  }

      if (subsetrows) {
        tab1n <- nrow(tab1)
        tab1 <- tab1[tab1[[var1]] %in% tab2.vals, ]
        message(paste("subsetting", tab1n, "rows of", tab1txt, "to", nrow(tab1), "rows"))
      }
    }
  } 
  return(tab1)
}  


