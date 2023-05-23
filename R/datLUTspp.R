#' Data - Gets variable description or class for SPCD.
#' 
#' Merge the ref_species table to append new variables, names, or categories to x.
#' 
#' 
#' @param x Data frame or comma-delimited file (*.csv). The data table with
#' variable to classify.
#' @param xvar String. Name of variable in the data table to join to.
#' @param NAclass String. NA values in xvar will be changed to NAclass.
#' @param group Logical. If TRUE, the group variable in ref_species
#' are merged to data table (E_SPGRPCD, W_SPGRPCD), depending on state(s) 
#' specified. If states overlap both E and W regions, the region with 
#' majority is used or E if equal. The group name is merged from 
#' ref_codes, SPGRPCD Variable.
#' @param states String. Name of state(s) the x table is from.
#' @param name String. Name for species output type ('COMMON', 'SCIENTIFIC', 
#' 'SYMBOL').
#' @param add0 Logical. IF TRUE, keep all codes in look up table. If FALSE,
#' only include codes that are in x.
#' @param stopifmiss Logical. IF TRUE, stops function if missing codes in LUTx.
#' @param xtxt String.* Name of x table for more useful information in
#' warnings.
#' @param savedata Logical. If TRUE, saves data to outfolder.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. If out_layer = NULL,
#' default = 'datlut'. 
#' @param gui Logical. If gui, user is prompted for parameters.
#'
#' @return \item{xLUT}{ The input data table with look-up table variable(s). }
#' \item{xLUTnm}{ Name of the new variable(s). } \item{LUT}{ Look up table with
#' categories. }
#' 
#' If savedata = TRUE, a comma-delimited file is output to the outfolder as
#' outfn.  If outfn = NULL, the name of the file will be datlut_'date'.csv.
#' @note For available reference tables:
#' sort(unique(ref_codes$VARIABLE))
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' WYtreelut <- datLUTspp(WYtree)
#' names(WYtreelut)
#' WYtree2 <- WYtreelut$xLUT
#' head(WYtree2)
#' @export datLUTspp
datLUTspp <- function(x, 
                      xvar = "SPCD",
                      NAclass = "Other", 
                      group = FALSE, 
                      states = NULL,
                      name = "COMMON",
                      add0 = FALSE, 
                      stopifmiss = FALSE, 
                      xtxt = NULL,
                      savedata = FALSE, 
                      savedata_opts = NULL, 
                      gui = FALSE){
  #################################################################################
  ## DESCRIPTION: Merge variable(s) from a reference table stored within FIESTA  
  ##      (ref_codes) or a comma-delimited file (*.csv).
  #################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) x=group=sciname <- NULL 

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") 
    Filters <- rbind(Filters, csv=c("Comma-delimited files (*.csv)", "*.csv"))

  
  ## Set global variables
  VALUE=LUTnewvarnm <- NULL 

  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(datLUTspp)) 
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

  ## Check datx
  #############################################################
  isdatatable <- FALSE
  datx <- pcheck.table(x, gui=gui, caption="Data table?")
  if ("data.table" %in% class(datx)) {
    isdatatable <- TRUE
    datkey <- key(datx)
#    datx <- setDF(datx)
  }

  ## Check xvar
  #############################################################
  datnmlst <- names(datx)
  xvar <- pcheck.varchar(xvar, "xvar", datnmlst, gui=gui,
		caption="Join variable in dat", stopifnull=TRUE)

  ## Check group
  group <- pcheck.logical(group, varnm="group", title="Variable group?", 
		first="NO", gui=gui)

  ## Check name
  #############################################################
  namelst <- c("COMMON", "SCIENTIFIC", "SYMBOL")
  name <- pcheck.varchar(name, "name", namelst, gui=gui,
		caption="Name type", stopifnull=TRUE)


  ## Check if all species codes in datx are in ref table
  #############################################################
  ref_spp <- FIESTAutils::ref_species[FIESTAutils::ref_species$SPCD %in% unique(datx[[xvar]]), 
			c("SPCD", "COMMON_NAME", "GENUS", "SPECIES", "SPECIES_SYMBOL",
                 "E_SPGRPCD", "C_SPGRPCD", "P_SPGRPCD", "MAJOR_SPGRPCD", "SCIENTIFIC_NAME")]
  if (length(ref_spp) == 0) {
    stop("SPCD values do not match ref_species values")
  }
  if (length(ref_spp$SPCD) < length(unique(datx[[xvar]]))) {
    spmiss <- unique(datx[[xvar]])[!unique(datx[[xvar]]) %in% ref_spp$SPCD] 
    message("SPCD not in ref table: ", toString(spmiss))
  }
 
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
      out_layer <- "datlut"
    }
  }
  

  ############################################################################
  ## DO THE WORK 
  ############################################################################
  LUTvar <- "SPCD"
  LUTnewvar <- ifelse(name == "COMMON", "COMMON_NAME", 
			ifelse(name == "SCIENTIFIC", "SCIENTIFIC_NAME", "SPECIES_SYMBOL"))
  lutvars <- c("SPCD", LUTnewvar)


  ## Get grpvar
  #############################################################
  if (group) {
    grpname <- "SPGRPNM"

    states <- FIESTAutils::pcheck.states(states)
    ref_state <- ref_statecd[ref_statecd$MEANING %in% states, ]

    ref_state$REGION <- "E"
    ref_state[ref_state$RS == "RMRS", "REGION"] <- "W"
    ref_state[ref_state$RS == "PNWRS", "REGION"] <- "W"

    if (length(unique(ref_state$REGION)) == 1) {
      grpnames <- paste0(unique(ref_state$REGION), "_SPGRPCD")
      grpcode <- grpnames
    } else {
      grpnames <- paste0(names(table(ref_state$REGION))[
			table(ref_state$REGION) == max(table(ref_state$REGION))], "_SPGRPCD")
      grpcode <- grpnames[1]
    } 
    lutvars <- c(lutvars, grpnames, "MAJOR_SPGRPCD")

    ## Merge SPGRPCD names from ref_codes
    ref_spgrpcd <- ref_codes[ref_codes$VARIABLE == "SPGRPCD", c("VALUE", "MEANING")]

    ref_spp <- merge(ref_spp, ref_spgrpcd, by.x=grpcode, by.y="VALUE", all.x=TRUE)
    if (any(is.na(ref_spp$MEANING))) {
      if (length(grpnames) > 1) {
        ref_spp <- merge(ref_spp, ref_spgrpcd, by.x=grpnames[2], by.y="VALUE", all.x=TRUE)
        ref_spp$MEANING <- ref_spp$MEANING.x
        ref_spp[is.na(ref_spp), "MEANING"] <- "MEANING.y"
      } else {
        missgrps <- sort(unique(ref_spp[is.na(ref_spp), "VALUE"]))
        message("missing group variables: ", toString(missgrps))
      }
    }
    setnames(ref_spp, "MEANING", grpname)
  } 
 
  ## Subset ref_spp table
  ###############################################################
  LUTx <- unique(ref_spp[, lutvars]) 
  if (any(duplicated(LUTx[[LUTnewvar]]))) {
    dups <- LUTx[[LUTnewvar]][duplicated(LUTx[[LUTnewvar]])]
    message("duplicated values exist in data: ", toString(dups))

    for (dup in dups) {
      duprows <- which(LUTx[[LUTnewvar]] == dup)
      nbrdups <- length(duprows)
      for (i in 2:nbrdups) {
        row <- duprows[i]
        newnm <- paste0(dup, "_", i - 1)
        LUTx[duprows[i], ][[LUTnewvar]] <- paste0(dup, "_", i - 1)
        if (is.factor(LUTx[[LUTnewvar]])) {
          levels(LUTx[[LUTnewvar]]) <- c(levels(LUTx[[LUTnewvar]]), newnm)
        }
      }
    }
  }


  ## Merg ref_spp to datx
  ###############################################################

  ## Check if class of xvar in datx matches class of xvar in LUTx
  tabs <- check.matchclass(datx, LUTx, xvar, LUTvar, 
		tab1txt=xtxt, tab2txt=LUTvar)
  datx <- tabs$tab1
  LUTx <- tabs$tab2
 
  all.x <- ifelse(add0, TRUE, FALSE)
  xLUT <- merge(datx, LUTx, by.x=xvar, by.y=LUTvar, all.x=all.x)
 
  
  ## Get all values of LUTx newvars
  LUTnewvar.vals <- unique(unlist(lapply(LUTx[,LUTnewvar, with=FALSE], as.character)))

  ## If NA values and NAclass != NULL, add NA to LUT
  if (!is.null(NAclass) && sum(is.na(xLUT[[xvar]])) > 0 && all(!is.na(LUTx[[LUTvar]]))) {
    NAclass <- checknm(NAclass, LUTnewvar.vals) 

    LUTxrow <- rep(NA, ncol(LUTx))
    for (v in LUTnewvar) {
      if (!is.numeric(LUTx[[v]])) {
         if (is.factor(LUTx[[v]]))
           levels(LUTx[[v]]) <- c(levels(LUTx[[v]]), NAclass)
         LUTxrow[which(names(LUTx) == v)] <- NAclass
      }
    }
    LUTx <- rbind(LUTx, as.list(LUTxrow))

    ## change NA values in xLUT
    DT_NAto0(xLUT, LUTnewvar, changeto=NAclass)
  }

  ## Add records if not other values exist in xLUT
  if (!all(unique(xLUT[[xvar]]) %in% LUTx[[xvar]])) {
    xvals <- unique(na.omit(xLUT[[xvar]]))
    missvals <- xvals[which(!xvals %in% unique(LUTx[[LUTvar]]))]

    if (length(missvals) > 0) {
      otherx <- checknm("Other", LUTnewvar.vals) 
      #otherx <- "Other"
      message("adding unclassified values to class ", otherx)

      LUTxrow <- data.table(matrix(data=NA, nrow=length(missvals), ncol=ncol(LUTx)))
      names(LUTxrow) <- names(LUTx)
      LUTxrow[[LUTvar]] <- missvals

      for (v in names(LUTx)[which(names(LUTx) != LUTvar)]) {
        if (!is.numeric(LUTx[[v]])) {
          if (is.factor(LUTx[[v]]))
             levels(LUTx[[v]]) <- c(levels(LUTx[[v]]), otherx)
          LUTxrow[[v]] <- rep(otherx, nrow(LUTxrow))
        } else {
          LUTxrow[[v]] <- rep(9999, nrow(LUTxrow))
        }        
      }
      LUTx <- rbind(LUTx, as.list(LUTxrow))
    }
  }  
          
  ## Return list
  ########################################################
  if (isdatatable) {
    xLUT <- data.table(xLUT)
    setkeyv(xLUT, datkey)
  }
  xLUTlst <- list(xLUT=xLUT)

  if (!is.null(LUTnewvarnm) || !is.null(LUTnewvar)) {
    xLUTlst$xLUTnm <- ifelse (is.null(LUTnewvarnm), LUTnewvar, LUTnewvarnm)
  } else {
    xLUTlst$xLUTnm <- NULL
  }
  #xLUTlst$LUT <- LUTx
  xLUTlst$LUT <- LUTx
  xLUTlst$ref_spp <- ref_spp

  if (group) {
    xLUTlst$grpcode <- grpcode
    xLUTlst$grpname <- grpname
  }  
 
  #### WRITE TO FILE 
  #############################################################
  if (savedata) {
    if ("sf" %in% class(datx)) {
      spExportSpatial(xLUT, 
              savedata_opts=list(outfolder = outfolder, 
                                  out_fmt = outsp_fmt, 
                                  out_dsn = out_dsn, 
                                  out_layer = out_layer,
                                  outfn.pre = outfn.pre, 
                                  outfn.date = outfn.date, 
                                  overwrite_layer = overwrite_layer,
                                  append_layer = append_layer, 
                                  add_layer = TRUE))
    } else {
      datExportData(xLUT, 
            savedata_opts=list(outfolder = outfolder, 
                                  out_fmt = out_fmt, 
                                  out_dsn = out_dsn, 
                                  out_layer = out_layer,
                                  outfn.pre = outfn.pre, 
                                  outfn.date = outfn.date, 
                                  overwrite_layer = overwrite_layer,
                                  append_layer = append_layer,
                                  add_layer = TRUE))
                    
    }
  }
  
  
  return(xLUTlst)
}
