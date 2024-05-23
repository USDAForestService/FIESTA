check.unitarea <- function(unitarea, pltx, unitvars, areavar="ACRES",
	areaunits="acres", removeunits=TRUE, removetext="unitarea", gui=FALSE,
	vars2keep=NULL) {

  ## DESCRIPTION: Checks unitarea
  ## Check acres by estimation unit
  ## If 1 estimation unit, unitarea can be a number of a data frame with areavar.
  ## Check if variables match variables in pltx
  ## removeunits:  If TRUE, removes units that do not have plots
  ## removetext: If removeunits=TRUE and there are units that do not have plots,
  ##		the text to show user (e.g., for Small Area, this is changed to dunitarea")

  ## Set global variables
  MATCH <- NULL

  ## Get pltx names
  pltx <- pcheck.table(pltx, gui=gui, tabnm="plt", returnsf=FALSE)
  pltnames <- names(pltx)

  ## Check estimation unit variables
  if (is.null(unitvars)) {
    stop("no unitvars")
  } else if (!is.character(unitvars)) {
    stop("unitvars must be a character vector")
  } else if (any(!unitvars %in% pltnames)) {
    missvars <- unitvars[which(!unitvars %in% pltnames)]
    stop("missing unitvars: ", paste(missvars, collapse=", "))
  }

  ## Get unique values of unitvars in pltx
  unit.vals <- unique(do.call(paste, pltx[, unitvars, with=FALSE]))
  nbrunits <- length(unit.vals)

  ## Check areaunits
  areaunits <- pcheck.varchar(var2check=areaunits, varnm="areaunits",
	gui=gui, checklst=c("acres", "hectares"), caption="Area units?",
	stopifnull=TRUE)

  ## Check unitarea and areavar
  ######################################################################
  if (nbrunits == 1) {
    if (!any(class(unitarea) %in% c("data.table", "data.frame"))) {
      if (is.null(unitarea)) {
        stop("unitarea is invalid")
      }
      if (is.character(unitarea) && is.vector(unitarea) && length(unitarea) == 1) {
        if (sum(grepl(",", unitarea)) > 0) {
          unitarea <- as.numeric(gsub(",", "", unitarea))
        } else {
          ## Check unitarea
          unitarea <- pcheck.table(unitarea, gui=gui, tabnm="unitarea",
			                nullcheck=TRUE, stopifnull=TRUE)
        }
      }
    }
    if (!any(class(unitarea) %in% c("data.table", "data.frame"))) {
      if (is.vector(unitarea) && length(unitarea) == 1) {
        if (is.na(unitarea)) {
          stop("invalid unitarea.. must be a number")
        }
      } else {
        stop("need a numeric vector of length 1 with unitarea")
      }
      unitarea <- data.table(unique(pltx[, unitvars, with=FALSE]), AREA=unitarea)
      areavar <- "AREA"
      setnames(unitarea, "AREA", areavar)
      setkeyv(unitarea, unitvars)
    } else {
      ## Check unitarea
      unitarea <- pcheck.table(unitarea, gui=gui, tabnm="unitarea",
			nullcheck=TRUE, stopifnull=TRUE)
      if (length(unitvars) == 1 && unitvars == "ONEUNIT" && !unitvars %in% names(unitarea)) {
        unitarea$ONEUNIT <- 1
      }
      ## Check areavar from strata table.
      areavar <- pcheck.varchar(var2check=areavar, varnm="areavar", gui=gui,
		                checklst=names(unitarea), caption="Area variable?", stopifnull=TRUE)

      if (nrow(unitarea) >  1) {
        if (length(unitvars) == 1) {
          if (!unitvars %in% names(unitarea)) {
            if (grepl("ONEUNIT", unitvars)) {
              unitarea[[unitvars]] <- 1
              unitarea <- unitarea[, sum(.SD), by=unitvars, .SDcols=areavar]
              setnames(unitarea, "V1", areavar)
            } else {
              stop("invalid unitarea")
            }
          } else if (unitvars %in% names(unitarea)) {
            unitarea <- unitarea[, sum(.SD), by=unitvars, .SDcols=areavar]
            setnames(unitarea, "V1", areavar)
          } else {
            if (nrow(unitarea) != 1) {
              stop("invalid unitarea")
            }
          }
        }
      }
      if (is.null(areavar) || !areavar %in% names(unitarea)) {
        stop("invalid areavar")
      }
    }
  } else {   ## nbrunits > 1
    if (!is.data.frame(unitarea) && is.numeric(unitarea))
      stop("plots have more than one estimation unit with no acres specified")

    ## Check unitarea
    unitarea <- pcheck.table(unitarea, gui=gui, tabnm="unitarea",
		nullcheck=TRUE, stopifnull=TRUE)

    ## Check areavar from strata table.
    areavar <- pcheck.varchar(var2check=areavar, varnm="areavar", gui=gui,
		checklst=names(unitarea), caption="Area variable?", stopifnull=TRUE)

    ## Check if areavar column is numeric
    if (!is.numeric(unitarea[[areavar]])) {
      if(sum(grepl(",", unitarea[[areavar]])) > 0)
        unitarea[[areavar]] <- as.numeric(gsub(",", "", unitarea[[areavar]]))
      if (!is.na(unitarea[[areavar]])) {
        stop("invalid areavar in unitarea.. must be a number")
      }
    }

    ## Check for NA values in areavar
    if (any(is.na(unitarea[[areavar]]))) {
      navals <- unitarea[is.na(get(areavar)), ]
      message("there are NA values in area.. removing from table")
      print(navals)
      unitarea <- unitarea[!is.na(get(areavar)), ]
    }

    ## Check the class of unitvars in unitarea matches unitvars in pltx
    tabs <- check.matchclass(pltx, unitarea, unitvars)
    pltx <- tabs$tab1
    unitarea <- tabs$tab2

    ## Check that the values of unitvars in pltx are all in unitarea
    pltx <- check.matchval(tab1=pltx, tab2=unitarea, var1=unitvars,
		tab1txt="plt", tab2txt=removetext, stopifmiss=FALSE)

    ## Check that the values of unitvars in unitarea are all in pltx
    unitarea <- check.matchval(unitarea, pltx, unitvars,
		tab1txt=removetext, tab2txt="plt", subsetrows=removeunits)

    vars2keep <- vars2keep[vars2keep %in% names(unitarea)]
    if (length(vars2keep) > 0) {
      ## Sum area by unitvars
      unitarea <- unitarea[, lapply(.SD, sum, na.rm=TRUE), 
           by=c(unitvars, vars2keep), .SDcols=areavar]
    } else {
      ## Sum area by unitvars
      unitarea <- unitarea[, lapply(.SD, sum, na.rm=TRUE), 
           by=unitvars, .SDcols=areavar]
    }
    setkeyv(unitarea, unitvars)
  }

  return(list(unitarea=unitarea, areavar=areavar, areaunits=areaunits))
}

