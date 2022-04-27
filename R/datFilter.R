#' Data - Filters data table.
#'
#' Subsets a data table by specified filter(s).
#'
#' If no parameters, then user is prompted for input. If partial parameters,
#' default parameter values are used.
#'
#' @param x Data frame, sf data frame or comma-delimited file (*.csv). A data
#' frame to filter.
#' @param xfilter String. A filter expression. Must be R syntax.  (e.g.,
#' "STATUSCD == 1", "INVYR %in% 2002:2005"). Use single quotes for strings
#' within double quotes (e.g., "SPP == 'Lodgepole'"). If NULL, a window pops up
#' to select filter variable(s) and filter value(s).
#' @param xfiltervar String. The filtervar if you know what it is. If NULL, a
#' window will pop up to select filter value(s).
#' @param othertabnms String vector. Name(s) of the objects or comma-delimited
#' files to subset.  Names must be in quotes (e.g., othertables=c('tree',
#' 'cond')).
#' @param uniqueid String. Unique identifier of x. Only needed if othertables
#' != NULL.  The uniqueid must be the same for all tables except if PLT_CN and
#' CN.
#' @param vardelete String vector. Vector of variables you would like deleted
#' from filter list. Mostly used for internal queries.
#' @param title.filter String. Title of the filter query window. Mostuly used
#' for internal queries.
#' @param savedata Logical. If TRUE, writes output data to outfolder.
#' @param filternm String. Optional. Name of filter, for feedback purposes.
#' @param stopifnull Logical. If TRUE, stop if output is NULL.
#' @param returnDT Logical. If TRUE, returns a data table. If FALSE, returns a
#' data frame.
#' @param xnm String. Name for filter attribute. Used for warning messages.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  If out_layer = NULL,
#' default = 'datf'.
#' @param gui Logical. If TRUE, pop-up windows will appear for user-interface.
#'
#' @return A list of the following items:
#'
#' \item{xf}{ A data frame of filtered x. } \item{xfilter}{ The xfilter. } If
#' othertables != NULL, the other tables, named with 'in' prefix
#' @note If message returned is 'filter removed all records', either the filter
#' removed all records in x or the filter is incorrect.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' # Set up data for example
#' tab <- data.frame(cbind(CONDCLASS=c(1, 1, 2, 1, 3, 3, 3, 1, 1, 1, 2, 1),
#' 			                   FORTYPCD = c(182, 184, 201, 221, 221, 184, 221, 182,
#' 			                                182, 201, 182, 221)))
#' 			                    
#' 	# Filter for value not equal to 182
#' 	datFilter(x = tab, xfilter = "FORTYPCD != 182")
#'
#' # Filter on two conditions, grab xf object from list
#' datFilter(x = WYcond, xfilter = "FORTYPCD == c(221) & STDSZCD == 3")$xf
#' @export datFilter
datFilter <- function(x,
                      xfilter = NULL,
                      xfiltervar = NULL,
                      othertabnms = NULL, 
                      uniqueid = "PLT_CN",
                      vardelete = NULL,
                      title.filter = NULL,
                      savedata = FALSE, 
                      filternm = NULL,
                      stopifnull = FALSE,
                      returnDT = TRUE,
                      xnm = NULL, 
                      savedata_opts = NULL,
                      gui = FALSE) {


  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0 | gui, TRUE, FALSE)
  if (gui) {uniqueid=savedata <- NULL}

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type == "windows")
    Filters = rbind(Filters,csv = c("Comma-delimited files (*.csv)", "*.csv"))


  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(datFilter))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts)

  ## Set savedata defaults
  savedata_defaults_list <- formals(savedata_options)[-length(formals(savedata_options))]

  for (i in 1:length(savedata_defaults_list)) {
    assign(names(savedata_defaults_list)[[i]], savedata_defaults_list[[i]])
  }

  ## Set user-supplied savedata values
  if (length(savedata_opts) > 0) {
    if (!savedata) {
      message("savedata=FALSE with savedata parameters... no data are saved")
    }
    for (i in 1:length(savedata_opts)) {
      if (names(savedata_opts)[[i]] %in% names(savedata_defaults_list)) {
        assign(names(savedata_opts)[[i]], savedata_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(savedata_opts)[[i]]))
      }
    }
  }

  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  intabs <- NULL

  ## Check filternm
  if (is.null(filternm)) filternm <- "filter"

  ## Check title.filter
  if (is.null(title.filter)) title.filter <- filternm

  ## Check xfilter
  if (!is.null(xfilter) && xfilter == "") {
    xfilter <- NULL
  }

  ## Check x
  isdt <- FALSE
  if (is.data.table(x) && !is.null(key(x))) {
    isdt <- TRUE
    xkey <- key(x)
  }
  datx <- pcheck.table(x, caption = "Data table?", gui=gui, stopifnull=TRUE,
			tabnm=xnm)


  ## Check savedata
  savedata <- pcheck.logical(savedata, varnm="savedata", title="Save data table?",
		first="NO", gui=gui)

  ## Check output parameters
  if (savedata) {
    outlst <- pcheck.output(outfolder=outfolder, out_dsn=out_dsn,
        out_fmt=out_fmt, outfn.pre=outfn.pre, outfn.date=outfn.date,
		    overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
		    add_layer=add_layer, append_layer=append_layer, gui=gui)
    outfolder <- outlst$outfolder
    out_dsn <- outlst$out_dsn
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    append_layer <- outlst$append_layer
    outfn.date <- outlst$outfn.date
    outfn.pre <- outlst$outfn.pre
    if (is.null(out_layer)) {
      out_layer <- "datf"
    }
  }

  ################################################################################
  ### DO WORK
  ################################################################################
  xfilters <- {}

  if (!is.null(xfilter)) {
    if (!is.character(xfilter)) stop("xfilter must be a character string")
    ## Check logical statement
    xfilter <- check.logic(datx, xfilter, filternm=filternm)

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
            xfiltertxt <- paste(xfiltervar, "%in% c(", addcommas(xfilterval,
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
    intabs <- clip.othertables(indatids, othertabnms, othertabs=othertabs,
		      savedata=savedata, outfolder=outfolder, overwrite_layer=overwrite_layer,
		      outfn.pre=outfn.pre, outfn.date=outfn.date)
  }

  #### WRITE TO FILE
  #############################################################
  if (savedata) {
    datExportData(indat,
            savedata_opts=list(outfolder=outfolder,
                                out_fmt=out_fmt,
                                out_dsn=out_dsn,
                                out_layer=out_layer,
                                outfn.pre=outfn.pre,
                                outfn.date=outfn.date,
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                add_layer=TRUE))
  }

  if (!returnDT) {
    indat <- data.frame(indat)
  } else {
    if (isdt && !is.null(xkey)) {
      indat <- data.table(indat)
      setkeyv(indat, xkey)
    }
  }

  returnlst <- list(xf = indat, xfilter = xfilters)
  if (!is.null(intabs)) returnlst$cliptabs <- intabs

  return(returnlst)
}
