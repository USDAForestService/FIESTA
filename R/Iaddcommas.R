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

