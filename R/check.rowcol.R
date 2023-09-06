check.rowcol <- function(gui=FALSE, esttype, conn=NULL, treef=NULL, seedf=NULL, condf,
	cuniqueid="PLT_CN", tuniqueid="PLT_CN", condid="CONDID", estseed="none",
	rowvar=NULL, colvar=NULL, row.FIAname=FALSE, col.FIAname=FALSE, 
      row.orderby=NULL, col.orderby=NULL, row.add0=FALSE, col.add0=FALSE, 
	domvarlst=NULL, domlut=NULL, title.rowvar=NULL, title.colvar=NULL, 
 	rowlut=NULL, collut=NULL, rowgrp=FALSE, rowgrpnm=NULL, rowgrpord=NULL, 
	title.rowgrp=NULL, landarea=NULL, states=NULL, cvars2keep=NULL){

  ####################################################################################
  ## CHECKS ROW AND COLUMN INFO
  ## 1. Checks for domlut. domlut is an optional lookup table to define potential
  ##    domain variables for table row/columns. This table must have 3 columns:
  ## 		DOMCODE - variable codes
  ##		DOMNAME - variable code names
  ##		DOMTITLE - pretty name for variable to use for output table titles
  ##    The DOMCODE and DOMNAME variables must be dataset. The info is used to populate
  ##    rowvar/colvar, row.orderby/col.orderby, and title.rowvar/title.colvar.
  ## 2. Define variables to exclude as potential domains
  ## 3. Check rowvar. If rowvar = NULL or "NONE", exit and only estimate totals.
  ## 4. Check for lookup tables, row.orderby, and uniquerow.
  ## 3. Check colvar.
  ## 4. Check for lookup tables, col.orderby, and uniquecol.
  ## 5. Check row and column filters.
  ## 6. Create uniquerow/uniquecol if NULL and set keys
  ## 7. Get title.rowvar/title.colvar from ref_titles
  ## 8. Concatenate variables:
  ##	   If rowvar and colvar in cond table, concatenate columns and add to cond table.
  ## 	   For tree/ratio esttypes:
  ##	   If colvar in tree table, concatenate columns and add to tree table.
  ## 8. Define domain.
  ## 9. Define cvars2keep
  ####################################################################################

  ## Set global variables
  SITECLCD=GSSTKCD=domainlst=tdomvar=tdomvar2=grpvar=tnames <- NULL
  isdb <- FALSE

  ## Check database connection (conn)
  ###############################################
  if (!is.null(conn)) {
    if (!DBI::dbIsValid(conn)) {
      stop("invalid database connection") 
    }
    isdb <- TRUE
    tablst <- DBI::dbListTables(conn)
  }

  ## Get column names
  cnames <- names(condf)
  
  if (!is.null(treef)) {
    if (is.character(treef) && isdb) {
      if (!treef %in% tablst) {
        stop(seedf, " not in database")
      } else {
        tnames <- DBI::dbListFields(conn, treef)
      }
    } else {
      tnames <- names(treef)
    }
  }
  if (!is.null(seedf)) {
    if (is.character(seedf) && isdb) {
      if (!seedf %in% tablst) {
        stop(seedf, " not in database")
      } else {
        snames <- DBI::dbListFields(conn, seedf)
      }
    } else {
      snames <- names(seedf)
    }
  }
  if (estseed == "only") {
    tnames <- snames
  }

  ## Check for condid
  if (!is.null(condid) && !condid %in% c(tnames, cnames)) condid <- NULL
  if (!is.null(cuniqueid) && !cuniqueid %in% names(condf)) stop("invalid cuniqueid")
  if (!is.null(treef) && !is.null(tuniqueid) && !tuniqueid %in% tnames)
    stop("invalid tuniqueid")
  ref_titles <- FIESTAutils::ref_titles
  concat <- FALSE
  bytdom <- FALSE
  seedclnm <- "<1"

  ##################################################################
  ## SET UP VARIABLE LISTS
  ##################################################################
  ## DEFINE DOMAIN VARIABLES LISTS (VARIABLES TO KEEP AND EXCLUDE)

  ## CHECK domlut
  domlut <- pcheck.table(domlut, tabnm="domlut", nullcheck=TRUE)

  if (!is.null(domlut)) {
    domlutvars <- c("DOMCODE", "DOMNAME")
    if (!all(domlutvars %in% names(domlut))){
      missvars <- domlutvars[which(!domlutvars %in% names(domlut))]
      warning("missing columns in domlut: ", addcommas(missvars))
    }
    if (is.null(domvarlst))
      domvarlst <- c(domlut[["DOMCODE"]], domlut[["DOMNAME"]])
  } else {
    domvarlst <- cnames[!cnames %in%
		c(cuniqueid, condid, "LON", "LAT", "PLOT")]
  }
  ## Append DSTRBGRP to condf if not in table and rowvar or colvar = DSTRBGRP
  if (any(c(rowvar, colvar) == "DSTRBGRP") && !"DSTRBGRP" %in% cnames &&
	"DSTRBCD1" %in% cnames) {
    condf <- merge(condf,
	FIESTAutils::ref_codes[FIESTAutils::ref_codes$VARIABLE == "DSTRBCD", c("VALUE", "GROUPCD")],
			by.x="DSTRBCD1", by.y="VALUE")
    names(condf)[names(condf) == "GROUPCD"] <- "DSTRBGRP"
    cnames <- names(condf)
    domvarlst <- c(domvarlst, "DSTRBGRP")
  }
  domvarlst.not <- cnames[!cnames %in% domvarlst]

  ## DEFINE other variables
  varlst <- sort(domvarlst)


  if(esttype %in% c("TREE", "RATIO")){
    ## DEFINE TREE VARIABLE LISTS
    tpavars <- c("TPA_UNADJ", "TPAMORT_UNADJ", "TPAGROW_UNADJ", "TPAREMV_UNADJ")
    volvars <- c("VOLCFNET", "VOLCSNET", "VOLBFNET", "VOLCFGRS", "VOLBFGRS", "VOLCFSND")
    mortvars <- c("TPAMORT_UNADJ", "MORTCFGS", "MORTBFSL", "MORTCFAL", "FMORTCFAL",
		"FMORTCFGS")
    growvars <- c("TPAGROW_UNADJ", "GROWCFGS", "GROWBFSL", "GROWCFAL", "FGROWCFAL",
 		"FGROWCFGS")
    remvvars <- c("TPAREMV_UNADJ", "REMVCFGS", "REMVBFSL", "REMVCFAL", "FREMVCFAL",
 		"FREMVCFGS")
    biovars <- c("DRYBIO_AG", "DRYBIO_BG", "DRYBIO_WDLD_SPP", "DRYBIO_SAPLING",
 		"DRYBIO_STUMP", "DRYBIO_TOP", "DRYBIO_BOLE", "DRYBIOT", "DRYBIOM",
		"DRYBIOTB", "JBIOTOT")
    carbvars <- c("CARBON_BG", "CARBON_AG")

    ## DEFINE TREE DOMAIN VARIABLE LISTS (VARIABLES TO EXCLUDE)
    tdomvarlst.not <- c("TREE", condid, "PREV_TRE_CN", "SUBP", "GROWBA",
		"RADGRW_RMRS", "BA", "TREEAGE", tpavars, volvars, mortvars, growvars,
		remvvars, biovars, carbvars,
		paste(volvars, "TPA", sep="_"), paste(mortvars, "TPA", sep="_"),
		paste(growvars, "TPA", sep="_"), paste(remvvars, "TPA", sep="_"),
		paste(biovars, "TPA", sep="_"), paste(carbvars, "TPA", sep="_"))

    ## DEFINE TREE DOMAIN VARIABLE LISTS (VARIABLES TO KEEP)
    tdomvarlst <- tnames[!tnames %in% tdomvarlst.not] 	## Tree domain variables

    varlst <- unique(c(varlst, sort(tdomvarlst)))
  }

  ## Check row.add0 and col.add0
  ########################################################
  row.add0 <- pcheck.logical(row.add0, varnm="row.add0",
		title="Add 0 for row?", first="NO")
  col.add0 <- pcheck.logical(col.add0, varnm="col.add0",
		title="Add 0 for column?", first="NO")
  rowgrp <- pcheck.logical(rowgrp, varnm="rowgrp", title="Row groups?",
		first="NO")
  row.FIAname <- pcheck.logical(row.FIAname, varnm="row.FIAname",
		title="Row names?", first="NO")
  if (rowgrp && is.null(rowgrpnm) && !row.FIAname) {
     stop("either row.FIAname must be TRUE or rowgrpnm != NULL to add row groups")
  }

  ##############################################################
  ### ROW VARIABLE
  ##############################################################
  uniquerow <- NULL
  rowvar <- pcheck.varchar(var2check=rowvar, varnm="rowvar",
		checklst=c("NONE", varlst), caption="Row variable",
		warn=paste(rowvar, "not found"))
  if (is.null(rowvar)) rowvar <- "NONE"

  ## If rowvar == "NONE", set rowvar = "TOTAL" and exit, returning short list
  if (rowvar == "NONE") {
    rowvar <- "TOTAL"
    colvar <- "NONE"
    domainlst <- rowvar
    row.add0 <- FALSE
    col.add0 <- FALSE
    row.FIAname <- FALSE

    ## Add a column for totals
    condf$TOTAL <- 1

    if (!is.null(cvars2keep) && length(cvars2keep) > 0) {
      if (!all(cvars2keep %in% names(condf))) {
        cvars2keep <- cvars2keep[cvars2keep %in% names(condf)]
        if (length(cvars2keep) == 0) {
          cvars2keep <- NULL
        }
      }
    }

    returnlst <- list(treef=treef, seedf=seedf, 
          condf=condf[,unique(c(cuniqueid, condid, cvars2keep, "TOTAL")), with=FALSE],
          uniquerow=NULL, uniquecol=NULL, domainlst=domainlst, bytdom=bytdom,
          rowvar=rowvar, colvar=colvar, row.orderby=row.orderby, col.orderby=col.orderby,
          row.add0=row.add0, col.add0=col.add0,
          title.rowvar=title.rowvar, title.colvar=title.colvar,
          tdomvar=tdomvar, concat=concat)
    return(returnlst)
  }

  if (rowvar != "NONE") {

    if (!is.null(row.FIAname) && row.FIAname) {
      ## Get FIA reference table for xvar
      xvar.ref <- getRefobject(toupper(rowvar))
      if (is.null(xvar.ref) && toupper(rowvar) != "SPCD") {
        message(paste("no reference name for", rowvar))
        row.FIAname <- FALSE
      }
    }

    ## GET row titles defined in FIESTA
    ###################################################
    if (is.null(title.rowvar)) {
     title.rowvar <- ifelse (rowvar %in% ref_titles[["DOMVARNM"]],
		ref_titles[ref_titles[["DOMVARNM"]] == rowvar, "DOMTITLE"],
		ifelse (sub("PREV_", "", rowvar) %in% ref_titles[["DOMVARNM"]],
		paste0("Previous ", tolower(ref_titles[ref_titles[["DOMVARNM"]] ==
			sub("PREV_", "", rowvar), "DOMTITLE"])), rowvar))
    }

    ## Check row groups
    if (rowgrp && is.null(rowgrpnm)) {
      vargrp <- unique(FIESTAutils::ref_codes[!is.na(FIESTAutils::ref_codes[["GROUPNM"]]) &
		FIESTAutils::ref_codes[["GROUPNM"]] != "", "VARIABLE"])
      if (!rowvar %in% vargrp) {
        message("row group not available for rowvar")
        rowgrp <- FALSE
      }
    }

    ## Check rowlut
    if (!is.null(rowlut)) {
      if (is.vector(rowlut) && length(rowlut) > 1) {
        rowlut <- data.table(rowlut)
        setnames(rowlut, rowvar)
      } else {
        rowlut <- pcheck.table(rowlut, tabnm=rowlut, caption="Row look up?")
      }
    }

    ##################################################################################
    ## Check for lookup tables
    ##################################################################################

    ## domlut defines columns in cond to use for codes, code names, and table titles
    ##################################################################################
    if (rowvar %in% cnames) {

      if (row.FIAname || !is.null(rowlut)) {
        if (!is.null(rowlut) && ncol(rowlut) > 1 &&  all(names(rowlut) %in% cnames)) {
          if (is.null(row.orderby) || row.orderby == "NONE") {
            message("row.orderby is not defined... ordering by rowvar")
          } else {

            if (row.orderby == rowvar) {
              row.name <- names(rowlut)[names(rowlut) != rowvar]
              if (length(row.name) > 1) stop("invalid rowlut... only 2 columns allowed")
              rowvar <- row.name
            }
          }
        } else {
          rowLUTgrp <- FALSE

          if (rowgrp) {
            if (!is.null(rowgrpnm)) {
              if (!rowgrpnm %in% cnames) stop(paste(rowgrpnm, "not in cond"))
              if (is.null(title.rowgrp)) title.rowgrp <- rowgrpnm

              if (!is.null(rowgrpord))
                if (!rowgrpord %in% cnames) stop(paste(rowgrpord, "not in cond"))
            } else {
              rowLUTgrp <- TRUE
            }
          }

          if (!is.null(rowlut)) row.add0 <- TRUE
          rowLUT <- datLUTnm(x=condf, xvar=rowvar, 
                             LUT=rowlut, FIAname=row.FIAname, 
                             group=rowLUTgrp, add0=row.add0)
          condf <- setDT(rowLUT$xLUT)
          rowlut <- setDT(rowLUT$LUT)
          rowLUTnm <- rowLUT$xLUTnm
          if (rowgrp) {
            rowgrpord <- rowLUT$grpcode
            rowgrpnm <- rowLUT$grpname
            if (all(sapply(rowlut[[rowgrpnm]], function(x) x == "")) || 								all(is.na(rowlut[[rowgrpnm]])))
              stop("no groups for ", rowvar)

            title.rowgrp <- ifelse (rowgrpord %in% ref_titles[["DOMVARNM"]], 
                ref_titles[ref_titles[["DOMVARNM"]] == rowgrpord, "DOMTITLE"], rowgrpnm)
          }

          if (is.null(row.orderby) || row.orderby == "NONE") {
            if (!is.null(rowLUTnm)) {
              row.orderby <- rowvar
              rowvar <- rowLUTnm
            }
            if (row.orderby == rowvar) {
              row.name <- names(rowlut)[names(rowlut) != rowvar]
              if (length(row.name) > 1) stop("invalid rowlut... only 2 columns allowed")
              rowvar <- row.name
            }

          } else if (row.orderby == rowvar) {
             rowvar <- rowLUTnm
          } else {
            if (!row.orderby %in% names(rowlut))
              stop("row.orderby not in rowlut")
          }
        }
      } else if (!is.null(row.orderby) && row.orderby != "NONE") {

        if (!row.orderby %in% cnames) stop("row.orderby must be in cond")
        if (row.orderby == rowvar) stop("row.orderby must be different than rowvar")

        ## If NULL or empty values, substitute with 0 values
        if (length(condf[[row.orderby]][is.na(condf[[row.orderby]])]) > 0) {
          if (is.numeric(condf[[row.orderby]])) {
            condf[is.na(get(row.orderby)), (row.orderby) := 0]
          } else {
            condf[is.na(get(row.orderby)), (row.orderby) := "Undefined"]
          }
        }
        if (length(condf[[row.orderby]][condf[[row.orderby]] == ""]) > 0) {
          if (is.numeric(condf[[row.orderby]])) {
            condf[get(row.orderby) == "", (row.orderby) := 0]
          } else {
            condf[get(row.orderby) == "", (row.orderby) := "Undefined"]
          }
        }
        condf <- DT_NAto0(DT=condf, cols=rowvar)
        condf <- DT_NAto0(DT=condf, cols=row.orderby)
      }

      #if (sum(is.na(condf[[rowvar]])) > 0) {
      #  rowvar.na.filter <- paste0("!is.na(", rowvar, ")")
      #  condf <- subset(condf, eval(parse(text = rowvar.na.filter)))
      #}

      ## add rowvar to cvars2keep
      cvars2keep <- c(cvars2keep, rowvar, row.orderby)

    } else if (rowvar %in% tnames) {

      tuniquex <- NULL
      suniquex <- NULL
      if (isdb) {
        tunique.qry <- paste("select distinct", rowvar, "from", treef, 
                              "order by", rowvar)
        tunique <- DBI::dbGetQuery(conn, tuniquex.qry)
        tuniquex <- tunique[[1]]

        if (estseed %in% c("add", "only")) {
          sunique.qry <- paste("select distinct", rowvar, "from", seedf, 
                              "order by", rowvar)
          sunique <- DBI::dbGetQuery(conn, suniquex.qry)
          suniquex <- sunique[[1]]
        } 
      }

      bytdom <- TRUE
      if (row.FIAname || !is.null(rowlut)) {
        if (!is.null(rowlut) && ncol(rowlut) > 1) {
          if (is.null(row.orderby) || row.orderby == "NONE") {
            message("row.orderby is not defined... ordering by rowvar")
          } else {
            if (row.orderby == rowvar) {
              row.name <- names(rowlut)[names(rowlut) != rowvar]
              if (length(row.name) > 1) stop("invalid rowlut... only 2 columns allowed")
              rowvar <- row.name
            }
          }
        } else {
          rowLUTgrp <- FALSE
          if (rowgrp) {
            if (!is.null(rowgrpnm)) {
              if (!rowgrpnm %in% tnames) stop(paste(rowgrpnm, "not in tree"))
              if (is.null(title.rowgrp)) title.rowgrp <- rowgrpnm

              if (!is.null(rowgrpord))
                if (!rowgrpord %in% tnames) stop(paste(rowgrpord, "not in tree"))
            } else {
              rowLUTgrp <- TRUE
            }
          }

          if (!is.null(rowlut)) row.add0 <- TRUE

          if (estseed != "only") {
            if (rowvar == "SPCD") {
              rowLUT <- datLUTspp(x=treef, add0=row.add0, xtxt="tree", uniquex=tuniquex)
            } else { 
              if (!is.data.frame(treef)) { x <- tnames } else { x <- treef } 
              rowLUT <- datLUTnm(x=x, xvar=rowvar, LUT=rowlut, FIAname=row.FIAname,
		  		group=rowLUTgrp, add0=row.add0, xtxt="tree", uniquex=tuniquex)
            }
            if (!isdb) {
              treef <- setDT(rowLUT$xLUT)
            }
            rowlut <- setDT(rowLUT$LUT)
            rowLUTnm <- rowLUT$xLUTnm
          }
          if (estseed %in% c("add", "only") && !is.null(seedf)) {
            if (rowvar %in% snames) {
               if (rowvar == "SPCD") {
                 rowLUT <- datLUTspp(x=seedf, add0=row.add0, xtxt="seed", uniquex=suniquex)
               } else {            
                 rowLUT <- datLUTnm(x=seedf, xvar=rowvar, LUT=NULL, FIAname=row.FIAname, 
				group=rowLUTgrp, add0=row.add0, xtxt="seed", uniquex=suniquex)
               }  
               rowluts <- setDT(rowLUT$LUT)
               rowLUTnm <- rowLUT$xLUTnm
               rowluts <- rowluts[!rowluts[[rowvar]] %in% rowlut[[rowvar]],]
               if (nrow(rowluts) > 0) {
                 rowlut <- rbind(rowlut, rowluts)
               }
               rowlut <- setorderv(rowlut, rowvar)
               if (!isdb) {
                 seedf <- rowLUT$xLUT
               }       
             } else if (rowvar == "DIACL") {
               if (!isdb) {
                 seedf$DIACL <- seedclnm
               }
             }
           }

           if (rowgrp) {
             rowgrpord <- rowLUT$grpcode
             rowgrpnm <- rowLUT$grpname
             if (all(sapply(rowlut[[rowgrpnm]], function(x) x == "")) ||
			all(is.na(rowlut[[rowgrpnm]])))
               stop("no groups for ", rowvar)

             title.rowgrp <- ifelse (rowgrpord %in% ref_titles[["DOMVARNM"]],
		  	ref_titles[ref_titles[["DOMVARNM"]] == rowgrpord, "DOMTITLE"], rowgrpnm)
           }

           if (is.null(row.orderby) || row.orderby == "NONE") {
             if (!is.null(rowLUTnm)) {
               row.orderby <- rowvar
               rowvar <- rowLUTnm
             }
             if (row.orderby == rowvar) {
               row.name <- names(rowlut)[names(rowlut) != rowvar]
               if (length(row.name) > 1) stop("invalid rowlut... only 2 columns allowed")
               if (length(row.name) == 0) {
                 row.orderby <- "NONE"
               } else {
                 rowvar <- row.name
               }
             }
           } else if (row.orderby == rowvar) {
             if (estseed %in% "add") {
               estseed[[row.orderby]] <- min(treef[[row.orderby]]) - 0.5
             }
             rowvar <- rowLUTnm
           } else {
             if (!row.orderby %in% names(rowlut)) {
               stop("row.orderby not in rowlut")
             }
           }
         }

      } else if (!is.null(row.orderby) && row.orderby != "NONE" && !isdb) {

        if (!row.orderby %in% tnames) stop("row.orderby must be in tree")
        if (row.orderby == rowvar) stop("row.orderby must be different than rowvar")

        ## If NULL or empty values, substitute with 0 values
        if (length(treef[[row.orderby]][is.na(treef[[row.orderby]])]) > 0) {
          if (is.numeric(treef[[row.orderby]])) {
            treef[is.na(get(row.orderby)), (row.orderby) := 0]
          } else {
            treef[is.na(get(row.orderby)), (row.orderby) := "Undefined"]
          }
        }
        if (length(treef[[row.orderby]][treef[[row.orderby]] == ""]) > 0) {
          if (is.numeric(treef[[row.orderby]])) {
            treef[get(row.orderby) == "", (row.orderby) := 0]
          } else {
            treef[get(row.orderby) == "", (row.orderby) := "Undefined"]
          }
        }

        if (estseed == "add" && !is.null(seedf) && rowvar=="DIACL" &&
		!row.orderby %in% snames) {
          seedclord <- min(treef[[row.orderby]]) - 0.5
          estseed[[row.orderby]] <- seedclord
        }

        treef <- DT_NAto0(DT=treef, cols=rowvar)
        treef <- DT_NAto0(DT=treef, cols=row.orderby)
      } else {
        if (estseed == "add" && !is.null(seedf) && rowvar=="DIACL" &&
		!"DIACL" %in% snames) {
          seedf$DIACL <- seedclnm
        }
      }

      ## Set tdomvar
      tdomvar <- rowvar

      if (!isdb) {
        ## Remove NA values in rowvar
        if (sum(is.na(treef[[rowvar]])) > 0) {
          rowvar.na.filter <- paste0("!is.na(", rowvar, ")")
          treef <- subset(treef, eval(parse(text = rowvar.na.filter)))
        }
      }
    }
  }

  ##############################################################
  ## COLUMN VARIABLE
  ##############################################################
  uniquecol <- NULL
  varlst <- varlst[which(!varlst %in% rowvar)]
  colvar <- pcheck.varchar(var2check=colvar, varnm="colvar",
		checklst=c("NONE", varlst), caption="Column variable",
		warn=paste(colvar, "not found"))
  if (is.null(colvar)) colvar <- "NONE"

  if (colvar != "NONE") {

    if (!is.null(col.FIAname) && col.FIAname) {
      ## Get FIA reference table for xvar

      xvar.ref <- getRefobject(toupper(colvar))
      if (is.null(xvar.ref) && toupper(colvar) != "SPCD") {
        message(paste("no reference name for", colvar))
        col.FIAname <- FALSE
      }
    }

    ## Check to make sure there is a rowvar when there is a colvar
    if (rowvar == "TOTAL") stop("no rowvar, use colvar as rowvar")
    if (is.null(col.orderby)) col.orderby <- "NONE"

    ## GET column titles defined in FIESTA
    ###################################################
    if (is.null(title.colvar)) {
      title.colvar <- ifelse (colvar %in% ref_titles[["DOMVARNM"]],
		ref_titles[ref_titles[["DOMVARNM"]] == colvar, "DOMTITLE"],
		ifelse (sub("PREV_", "", colvar) %in% ref_titles[["DOMVARNM"]],
		paste0("Previous ", tolower(ref_titles[ref_titles[["DOMVARNM"]] ==
			sub("PREV_", "", colvar), "DOMTITLE"])), colvar))
    }

    ## Check collut
    if (!is.null(collut)) {
      if (is.vector(collut) && length(collut) > 1) {
        collut <- data.table(collut)
        setnames(collut, colvar)
      } else {
        collut <- pcheck.table(collut, tabnm=collut, caption="Column look up?")
      }
    }

    ## domlut defines columns in cond to use for codes, code names, and table titles
    ##################################################################################
    if (!is.null(domlut)) {
      if (!colvar %in% domvarlst) stop(paste(colvar, "is not in domlut"))
      if (colvar %in% domlut[["DOMCODE"]]) {
        col.orderby <- colvar
        title.colvar <- as.character(domlut[match(colvar, domlut[["DOMCODE"]]), "DOMTITLE"])
        colvar <- as.character(domlut[match(colvar, domlut[["DOMCODE"]]), "DOMNAME"])
        if (!colvar %in% names(condf)) {
          warning(paste(colvar, "not in cond table... using code"))
          colvar <- col.orderby
          col.orderby <- NULL
        }
      } else if (colvar %in% domlut[["DOMNAME"]]) {
        col.orderby <- as.character(domlut[match(colvar, domlut[["DOMNAME"]]), "DOMCODE"])
        title.colvar <- as.character(domlut[match(colvar, domlut[["DOMNAME"]]), "DOMTITLE"])
        if (!col.orderby %in% names(condf)) {
          warning(paste(col.orderby, "not in cond table... ordering by name"))
          col.orderby <- NULL
        }
      }

    } else if (colvar %in% cnames) {

      if (col.FIAname || !is.null(collut)) {
        if (!is.null(collut) && ncol(collut) > 1 && all(names(collut) %in% cnames)) {
          if (is.null(col.orderby) || col.orderby == "NONE") {
            message("col.orderby is not defined... ordering by colvar")
          } else {
            if (col.orderby == colvar) {
              col.name <- names(collut)[names(collut) != colvar]
              if (length(col.name) > 1) stop("invalid collut... only 2 columns allowed")
              colvar <- col.name
            }
          }
        } else {
          if (!is.null(collut)) col.add0 <- TRUE
          colLUT <- datLUTnm(x=condf, xvar=colvar, LUT=collut, FIAname=col.FIAname,
			add0=col.add0)
          condf <- setDT(colLUT$xLUT)
          collut <- setDT(colLUT$LUT)
          colLUTnm <- colLUT$xLUTnm

          if (is.null(col.orderby) || col.orderby == "NONE") {
            if (!is.null(colLUTnm)) {
              col.orderby <- colvar
              colvar <- colLUTnm
            }
          } else if (col.orderby == colvar) {
             colvar <- colLUTnm
          }
        }

      } else if (!is.null(col.orderby) && col.orderby != "NONE") {

        if (!col.orderby %in% cnames) stop("col.orderby must be in cond")
        if (col.orderby == colvar) stop("col.orderby must be different than colvar")

        ## If NULL or empty values, substitute with 0 values
        if (length(condf[[col.orderby]][is.na(condf[[col.orderby]])]) > 0) {
          if (is.numeric(condf[[col.orderby]])) {
            condf[is.na(get(col.orderby)), (col.orderby) := 0]
          } else {
            condf[is.na(get(col.orderby)), (col.orderby) := "Undefined"]
          }
        }
        if (length(condf[[col.orderby]][condf[[col.orderby]] == ""]) > 0) {
          if (is.numeric(condf[[col.orderby]])) {
            condf[get(col.orderby) == "", (col.orderby) := 0]
          } else {
            condf[get(col.orderby) == "", (col.orderby) := "Undefined"]
          }
        }

        condf <- DT_NAto0(DT=condf, cols=colvar)
        condf <- DT_NAto0(DT=condf, cols=col.orderby)
      }

#      if (sum(is.na(condf[[colvar]])) > 0) {
#        colvar.na.filter <- paste0("!is.na(", colvar, ")")
#        condf <- subset(condf, eval(parse(text = colvar.na.filter)))
#      }

      ## add colvar to cvars2keep
      cvars2keep <- c(cvars2keep, colvar, col.orderby)

    } else if (colvar %in% tnames) {
      tuniquex <- NULL
      if (isdb) {
        tuniquex.qry <- paste("select distinct", colvar, "from", treef, 
                              "order by", colvar)
        tuniquex <- DBI::dbGetQuery(conn, tuniquex.qry)[[1]]

        if (estseed %in% c("add", "only")) {
          suniquex.qry <- paste("select distinct", colvar, "from", seedf, 
                              "order by", colvar)
          suniquex <- DBI::dbGetQuery(conn, suniquex.qry)[[1]]
        } 
      }

      bytdom <- TRUE
      if (col.FIAname || !is.null(collut)) {
        if (!is.null(collut) && ncol(collut) > 1) {
          if (is.null(col.orderby) || col.orderby == "NONE") {
            message("col.orderby is not defined... ordering by colvar")
          } else {
            if (col.orderby == colvar) {
              col.name <- names(collut)[names(collut) != colvar]
              if (length(col.name) > 1) stop("invalid collut... only 2 columns allowed")
              colvar <- col.name
            }
          }
        } else {

          if (estseed != "only") {

            #if (!is.null(collut)) col.add0 <- TRUE
            if (colvar == "SPCD") {
              colLUT <- datLUTspp(x=treef, add0=col.add0, xtxt="treef", uniquex=tuniquex)
            } else {            
              colLUT <- datLUTnm(x=treef, xvar=colvar, LUT=collut, FIAname=col.FIAname,
				add0=col.add0, xtxt="treef", uniquex=tuniquex)
            }
            if (!isdb) {
              treef <- setDT(colLUT$xLUT)
            }
            collut <- setDT(colLUT$LUT)
            colLUTnm <- colLUT$xLUTnm
          }
          if (estseed %in% c("add", "only") && !is.null(seedf)) {
            if (colvar %in% snames) {
              if (colvar == "SPCD") {
                colLUT <- datLUTspp(x=seedf, add0=col.add0, xtxt="treef", uniquex=suniquex)
              } else {            
                colLUT <- datLUTnm(x=seedf, xvar=colvar, LUT=collut,
				FIAname=col.FIAname, add0=col.add0, xtxt="seed", uniquex=suniquex)
              }
              if (!isdb) {
                seedf <- colLUT$xLUT
              }
            } else if (colvar == "DIACL") {
              if (!isdb) {
                seedf$DIACL <- seedclnm
              }
            }
          }
          if (!is.null(col.orderby) && col.orderby == "NONE") {
            if (!is.null(colLUTnm)) {
              col.orderby <- colvar
              colvar <- colLUTnm
            }
          } else if (col.orderby == colvar) {
            if (estseed == "add" && !isdb) {
              seedclord <- min(treef[[col.orderby]]) - 0.5
              estseed[[col.orderby]] <- seedclord
            }
            colvar <- colLUTnm
          }
          setkeyv(collut, col.orderby)
        }

      } else if (!is.null(col.orderby) && col.orderby != "NONE") {

        if (!col.orderby %in% names(treef)) stop("col.orderby must be in tree")
        if (col.orderby == colvar) stop("col.orderby must be different than colvar")

        ## If NULL or empty values, substitute with 0 values
        if (length(treef[[col.orderby]][is.na(treef[[col.orderby]])]) > 0) {
          if (is.numeric(treef[[col.orderby]])) {
            treef[is.na(get(col.orderby)), (col.orderby) := 0]
          } else {
            treef[is.na(get(col.orderby)), (col.orderby) := "Undefined"]
          }
        }
        if (length(treef[[col.orderby]][treef[[col.orderby]] == ""]) > 0) {
          if (is.numeric(treef[[col.orderby]])) {
            treef[get(col.orderby) == "", (col.orderby) := 0]
          } else {
            treef[get(col.orderby) == "", (col.orderby) := "Undefined"]
          }
        }

        if (estseed == "add" && !is.null(seedf) && colvar=="DIACL" &&
		!col.orderby %in% names(seedf)) {
          seedclord <- min(treef[[col.orderby]]) - 0.5
          estseed[[col.orderby]] <- seedclord
        }

        treef <- DT_NAto0(DT=treef, cols=colvar)
        treef <- DT_NAto0(DT=treef, cols=col.orderby)
      } else {
        if (estseed == "add" && !is.null(seedf) && colvar=="DIACL" &&
		!"DIACL" %in% names(seedf)) {
          seedf$DIACL <- seedclnm
        }
      }


      ## Set tdomvar
      if (is.null(tdomvar)) {
        tdomvar <- colvar
      } else {
           tdomvar2 <- colvar
      }
      
      if (!isdb) {
        if (sum(is.na(treef[[colvar]])) > 0) {
          colvar.na.filter <- paste0("!is.na(", colvar, ")")
          treef <- subset(treef, eval(parse(text = colvar.na.filter)))
        }
      }
    }
  }
 
  ###################################################################################
  ## GET DOMAIN. CONCATENATE ROWVAR & COLVAR VARIABLES IF THEY ARE IN THE SAME TABLE.
  ###################################################################################
  if (colvar != "NONE") {
    concat <- TRUE
    grpvar <- c(rowvar, colvar)
  }
  domainlst <- unique(c(domainlst, rowvar, colvar))
  domainlst <- domainlst[domainlst != "NONE"]

  ############################################################################
  ## Get uniquerow and uniquecol
  ############################################################################

  ## uniquerow
  #########################################################
  if (!is.null(rowlut)) {
#    if (sum(unlist(lapply(rowlut, duplicated))) > 0) {
#      print(rowlut)
#      stop("invalid rowlut... no duplicates allowed")
#    }
    uniquerow <- rowlut
  } else if (!is.null(uniquerow)) {
    uniquerow <- setDT(uniquerow)
    if (!is.null(row.orderby) && row.orderby != "NONE" && row.orderby %in% names(uniquerow))
      setkeyv(uniquerow, c(rowgrpnm, row.orderby))
  } else if (rowvar %in% cnames) {
    if (!is.null(row.orderby) && row.orderby != "NONE") {
      uniquerow <- unique(condf[,c(rowgrpord, rowgrpnm, row.orderby, rowvar), with=FALSE])
      setkeyv(uniquerow, c(rowgrpord, rowgrpnm, row.orderby))
    } else {
      if (is.factor(condf[[rowvar]])) {
        uniquerow <- as.data.table(levels(condf[[rowvar]]))
        names(uniquerow) <- rowvar
        uniquerow[[rowvar]] <- factor(uniquerow[[rowvar]], levels=levels(condf[[rowvar]]))
      } else {
        #rowvals <- na.omit(unique(condf[, rowvar, with=FALSE]))
        rowvals <- unique(condf[, rowvar, with=FALSE])
        uniquerow <- as.data.table(rowvals)
        names(uniquerow) <- rowvar
        setkeyv(uniquerow, rowvar)
      }
    }
  } else if (rowvar %in% tnames) {
    if (!is.null(row.orderby) && row.orderby != "NONE") {
      if (isdb) {
        uniquerow.qry <- 
          paste0("select distinct ", toString(c(rowgrpord, rowgrpnm, row.orderby, rowvar)),
                  " from ", treef, 
                  " order by ", toString(c(rowgrpord, rowgrpnm, row.orderby, rowvar)))
        uniquerow <- as.data.table(DBI::dbGetQuery(conn, uniquerow.qry))
      } else {
        uniquerow <- unique(treef[,c(rowgrpord, rowgrpnm, row.orderby, rowvar), with=FALSE])
      }
      setkeyv(uniquerow, c(rowgrpord, rowgrpnm, row.orderby))

      if (estseed == "add" && !is.null(seedf)) {
        if (all(c(rowvar, row.orderby) %in% snames) && rowvar == "DIACL") {
          if (is.factor(uniquerow[[rowvar]])) {
            levels(uniquerow[[rowvar]]) <- c(seedclnm, levels(uniquerow[[rowvar]]))
          }
          if (is.factor(uniquerow[[row.orderby]])) {
            levels(uniquerow[[row.orderby]]) <- c(seedclord, levels(uniquerow[[row.orderby]]))
          }
          uniqueseed <- data.table(seedclord, seedclnm)
          setnames(uniqueseed, c(col.orderby, colvar))
          uniquerow <- rbindlist(list(uniqueseed, uniquerow))
        }
      }
    } else {
      if (is.factor(treef[[rowvar]])) {
        if ((estseed == "add" && !is.null(seedf)) && 
          (rowvar %in% names(seedf) && rowvar == "DIACL")) {
          rowlevels <- c(seedclnm, levels(treef[[rowvar]]))
        } else {
          rowlevels <- levels(treef[[rowvar]])
        }
        uniquerow <- as.data.table(rowlevels)
        names(uniquerow) <- rowvar
        uniquerow[[rowvar]] <- factor(uniquerow[[rowvar]], levels=rowlevels)
      } else {
        if ((estseed == "add" && !is.null(seedf)) && 
          (rowvar %in% names(seedf) && rowvar == "DIACL")) {
          rowvals <- c(seedclnm, sort(na.omit(unique(treef[, rowvar, with=FALSE][[1]]))))
        } else {
          rowvals <- sort(na.omit(unique(treef[, rowvar, with=FALSE][[1]])))
        }
        uniquerow <- as.data.table(rowvals)
        names(uniquerow) <- rowvar
        uniquerow[[rowvar]] <- factor(uniquerow[[rowvar]], levels=rowvals)
        setkeyv(uniquerow, rowvar)
      }
    }
  }

  ## Check for duplicate values
  if (any(duplicated(uniquerow[[rowvar]]))) {
    dupvals <- uniquerow[[rowvar]][duplicated(uniquerow[[rowvar]])]
    for (dup in dupvals) {       
      vals <- uniquerow[uniquerow[[rowvar]] == dup, row.orderby, with=FALSE][[1]]
      val <- vals[length(vals)]
      vals2chg <- vals[-length(vals)]

      if (any(c(rowvar, row.orderby) %in% names(condf))) {
        if (row.orderby %in% names(condf)) {
          if (class(condf[[row.orderby]]) != class(val)) {
            class(val) <- class(condf[[row.orderby]])
          } 
          condf[condf[[row.orderby]] %in% vals2chg, row.orderby] <- val
        } else {
          if (class(condf[[rowvar]]) != class(val)) {
            class(val) <- class(condf[[rowvar]])
          } 
          condf[condf[[rowvar]] %in% vals2chg, rowvar] <- val
        }
      }
      if (any(c(rowvar, row.orderby) %in% names(treef))) {
        if (row.orderby %in% names(treef)) {
          if (class(treef[[row.orderby]]) != class(val)) {
            class(val) <- class(treef[[row.orderby]])
          } 
          treef[treef[[row.orderby]] %in% vals2chg, row.orderby] <- val
        } else {
          if (class(treef[[rowvar]]) != class(val)) {
            class(val) <- class(treef[[rowvar]])
          } 
          treef[condf[[rowvar]] %in% vals2chg, rowvar] <- val
        }
      }
      uniquerow <- uniquerow[!uniquerow[[row.orderby]] %in% vals2chg, ] 
    }
  }


  #if (!is.null(landarea) && landarea %in% c("FOREST", "TIMBERLAND")) {
  #  uniquerow2 <- uniquerow[!uniquerow[[rowvar]] %in% c(0, "Nonforest"),]
  #}

  ## uniquecol
  #########################################################
  if (!is.null(collut)) {
#    if (sum(unlist(lapply(collut, duplicated))) > 0) {
#      print(collut)
#      stop("invalid collut... no duplicates allowed")
#    }
    uniquecol <- collut
  } else if (!is.null(uniquecol)) {
    uniquecol <- setDT(uniquecol)
    if (col.orderby != "NONE" && col.orderby %in% names(uniquecol))
      setkeyv(uniquecol, col.orderby)
  } else if (colvar %in% names(condf)) {
    if (!is.null(col.orderby) && col.orderby != "NONE") {
      uniquecol <- unique(condf[, c(colvar, col.orderby), with=FALSE])
      setkeyv(uniquecol, col.orderby)
    } else {
      if (is.factor(condf[[colvar]])) {
        uniquecol <- as.data.table(levels(condf[[colvar]]))
        names(uniquecol) <- colvar
        uniquecol[[colvar]] <- factor(uniquecol[[colvar]], levels=levels(condf[[colvar]]))
      } else {
        #colvals <- na.omit(unique(condf[, colvar, with=FALSE]))
        colvals <- unique(condf[, colvar, with=FALSE])
        uniquecol <- as.data.table(colvals)
        names(uniquecol) <- colvar
        setkeyv(uniquecol, colvar)
      }
    }
  } else if (colvar %in% names(treef)) {
    if (!is.null(col.orderby) && col.orderby != "NONE") {
      uniquecol <- unique(treef[,c(colvar, col.orderby), with=FALSE])
      setkeyv(uniquecol, col.orderby)

      if (estseed == "add" && !is.null(seedf)) {
        if (all(c(colvar, col.orderby) %in% names(seedf)) && colvar == "DIACL") {
          if (is.factor(uniquecol[[colvar]])) {
            levels(uniquecol[[colvar]]) <- c(seedclnm, levels(uniquecol[[colvar]]))
          }
          if (is.factor(uniquecol[[col.orderby]])) {
            levels(uniquecol[[col.orderby]]) <- c(seedclord, levels(uniquecol[[col.orderby]]))
          }
          uniqueseed <- data.table(seedclord, seedclnm)
          setnames(uniqueseed, c(col.orderby, colvar))
          uniquecol <- rbindlist(list(uniqueseed, uniquecol))
        }
      }
    } else {
      if (is.factor(treef[[colvar]])) {
        if ((estseed == "add" && !is.null(seedf)) && 
          (colvar %in% names(seedf) && colvar == "DIACL")) {
          collevels <- c(seedclnm, levels(treef[[colvar]]))
        } else {
          collevels <- levels(treef[[colvar]])
        }
        uniquecol <- as.data.table(collevels)
        names(uniquecol) <- colvar
        uniquecol[[colvar]] <- factor(uniquecol[[colvar]], levels=collevels)
      } else {
        if ((estseed == "add" && !is.null(seedf)) && 
          (colvar %in% names(seedf) && colvar == "DIACL")) {
          colvals <- c(seedclnm, sort(na.omit(unique(treef[, colvar, with=FALSE][[1]]))))
        } else {
          colvals <- sort(na.omit(unique(treef[, colvar, with=FALSE][[1]])))
        }
        uniquecol <- as.data.table(colvals)
        names(uniquecol) <- colvar
        uniquecol[[colvar]] <- factor(uniquecol[[colvar]], levels=colvals)
        setkeyv(uniquecol, colvar)
      }
    }
  }

  if (any(duplicated(uniquecol[[colvar]]))) {
    dupvals <- uniquecol[[colvar]][duplicated(uniquecol[[colvar]])]
    for (dup in dupvals) {       
      vals <- uniquecol[uniquecol[[colvar]] == dup, col.orderby, with=FALSE][[1]]
      val <- vals[length(vals)]
      vals2chg <- vals[-length(vals)]

      if (any(c(colvar, col.orderby) %in% names(condf))) {
        if (col.orderby %in% names(condf)) {
          if (class(condf[[col.orderby]]) != class(val)) {
            class(val) <- class(condf[[col.orderby]])
          } 
          condf[condf[[col.orderby]] %in% vals2chg, col.orderby] <- val
        } else {
          if (class(condf[[colvar]]) != class(val)) {
            class(val) <- class(condf[[colvar]])
          } 
          condf[condf[[colvar]] %in% vals2chg, colvar] <- val
        }
      }
      if (any(c(colvar, col.orderby) %in% names(treef))) {
        if (col.orderby %in% names(treef)) {
          if (class(treef[[col.orderby]]) != class(val)) {
            class(val) <- class(treef[[col.orderby]])
          } 
          treef[treef[[col.orderby]] %in% vals2chg, col.orderby] <- val
        } else {
          if (class(treef[[colvar]]) != class(val)) {
            class(val) <- class(treef[[colvar]])
          } 
          treef[treef[[colvar]] %in% vals2chg, colvar] <- val
        }
      }
      uniquecol <- uniquecol[!uniquecol[[col.orderby]] %in% vals2chg, ] 
    }
  }

  #if (!is.null(landarea) && landarea %in% c("FOREST", "TIMBERLAND")) {
  #  if (any(uniquecol[[colvar]] %in% c(0, "Nonforest"))) {
  #    message("0 values are assumed to represent nonforest land and are removed from analysis")
  #    uniquecol <- uniquecol[!uniquecol[[colvar]] %in% c(0, "Nonforest"),]
  #  }
  #}

  ## Define cvars2keep
  cvars2keep <- unique(c(cuniqueid, condid, cvars2keep))
  cvars2keep <- cvars2keep[cvars2keep %in% names(condf)]
  condf <- condf[, cvars2keep, with=FALSE]
  setkeyv(condf, c(cuniqueid, condid))

  ## Create factors for ordering tables
  ##############################################################################
  if (!is.null(uniquerow)) {
    ## Change SITECLCD to descending order
    if (row.FIAname && "SITECLCD" %in% names(uniquerow))
      uniquerow <- setorder(uniquerow, -SITECLCD)
    if (row.FIAname && "GSSTKCD" %in% names(uniquerow))
      uniquerow <- setorder(uniquerow, -GSSTKCD)
    if (!is.null(rowgrpnm))
      uniquerow[[rowgrpnm]] <- factor(uniquerow[[rowgrpnm]],
			levels=unique(uniquerow[[rowgrpnm]]))
    uniquerow[[rowvar]] <- factor(uniquerow[[rowvar]],
			levels=unique(uniquerow[[rowvar]]))
  }

  if (!is.null(uniquecol)) {
    ## Change SITECLCD to descending order
    if (col.FIAname && "SITECLCD" %in% names(uniquecol))
      uniquecol <- setorder(uniquecol, -SITECLCD)
    if (col.FIAname && "GSSTKCD" %in% names(uniquecol))
      uniquecol <- setorder(uniquecol, -GSSTKCD)
    uniquecol[[colvar]] <- factor(uniquecol[[colvar]],
			levels=unique(uniquecol[[colvar]]))
  }

  ## Add a column for totals
  condf$TOTAL <- 1

  returnlst <- list(condf=condf, uniquerow=uniquerow, uniquecol=uniquecol,
	domainlst=domainlst, bytdom=bytdom, rowvar=rowvar, colvar=colvar,
	row.orderby=row.orderby, col.orderby=col.orderby, row.add0=row.add0,
	col.add0=col.add0, title.rowvar=title.rowvar, title.colvar=title.colvar,
	rowgrpnm=rowgrpnm, title.rowgrp=title.rowgrp, tdomvar=tdomvar,
	tdomvar2=tdomvar2, grpvar=grpvar)

  if (esttype %in% c("TREE", "RATIO", "SEED")) {   
    if (!isdb && estseed != "only") {
      ## Filter tree data for any cond filters
      treef <- treef[paste(get(tuniqueid), get(condid), sep="_") %in%
		condf[,paste(get(cuniqueid), get(condid), sep="_")]]
      returnlst <- append(list(treef=treef), returnlst)
    }

    if (!isdb && !is.null(seedf)) {
      seedf <- seedf[paste(get(tuniqueid), get(condid), sep="_") %in%
		condf[,paste(get(cuniqueid), get(condid), sep="_")]]
      returnlst <- append(list(seedf=seedf), returnlst)
    }
  }

  return(returnlst)
}

