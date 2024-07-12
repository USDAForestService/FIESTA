#' Data - Aggregates numeric tree data to the plot or condition-level.
#' 
#' Aggregates numeric tree-level data (e.g., VOLCFNET) to plot or condition,
#' including options for filtering tree data or extrapolating to plot aseedonlycre by
#' multiplying by TPA.
#' 
#' If variable = NULL, then it will prompt user for input.
#' 
#' Dependent external functions: datFilter Dependent internal functions:
#' addcommas, fileexistsnm, getadjfactor
#' 
#' For adjcond (bycond=FALSE): \cr If you want to summarize trees-per-acre
#' information aggregated to plot or condition level, you need to include a TPA
#' variable in tree table. \cr For tsumvars = GROWCFGS, GROWBFSL, GROWCFAL,
#' FGROWCFGS, FGROWBFSL, or FGROWCFAL, you must have TPAGROW_UNADJ \cr For
#' tsumvars = MORTCFGS, MORTBFSL, MORTCFAL, FMORTCFGS, FMORTBFSL, or FMORTCFAL,
#' you must have TPAMORT_UNADJ \cr For tsumvars = REMVCFGS, REMVBFSL, REMVCFAL,
#' FREMVCFGS, FREMVBFSL, or FREMVCFAL, you must have TPAREMV_UNADJ \cr
#' 
#' If you want to adjust plot-level or subplot-level information by condition 
#' proportions (adjplot), you need to include CONDID & CONDPROP_UNADJ in cond 
#' or tree table and COND_STATUS_CD. \cr
#' 
#' @param tree Dataframe or comma-delimited file (*.csv). The tree-level table.
#' @param seed Dataframe or comma-delimited file (*.csv). The seedling table.
#' @param cond Dataframe or comma-delimited file (*.csv). Condition-level table
#' to join the aggregated tree data to, if bycond=TRUE. This table also may be
#' used for condition proportion or strata variables used if adjcond or
#' adjstrata = TRUE (See details below).  This table is optional.
#' @param plt Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Plot-level table to join the aggregated tree data to, if bycond=FALSE. This
#' table is optional.
#' @param subp_cond Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Subplot condition-level table to use to sum condition proportions, 
#' if bysubp=TRUE. 
#' @param subplot Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Subplot-level table to used to calculate adjustment factors, to remove 
#' nonsampled conditions (SUBP_STATUS_CD = 3). This table is optional. If 
#' included the aggregated tree data are joined to subplot before returning.
#' @param datsource String. Source of data ('obj', 'csv', 'sqlite', 'gdb').
#' @param dsn String. If datsource='sqlite', the name of SQLite database
#' (*.sqlite).
#' @param tuniqueid String. Unique identifier of plt in tree table.
#' @param cuniqueid String. Unique identifier of plt in cond table if cond is
#' NOT NULL.
#' @param puniqueid String. Unique identifier of plt table if plt is NOT NULL.
#' @param bycond Logical. If TRUE, the data are aggregated to the condition
#' level (by: cuniqueid, condid). If FALSE, the data are aggregated to the plot
#' level (by: puniqueid). If bysubp = TRUE and bycond = TRUE, data are 
#' aggregated by subpuniqueid, subpid, condid.
#' @param condid String. Unique identifier for conditions.
#' @param bysubp Logical. If TRUE, data are aggregated to the subplot level.
#' @param subpuniqueid String. Unique identifier of plot in subplot and 
#' subp_cond table.
#' @param subpid String. Unique identifier of each subplot.
#' @param tsumvarlst String (vector). Tree-level variable(s) to aggregate
#' (e.g., "TPA_UNADJ", "BA"). Use "TPA_UNADJ" (tfun=sum) for summed tree
#' count.
#' @param tsumvarnmlst String (vector). Name of the tree-level variable(s) to
#' aggregate (e.g., "TPALIVE", "BALIVE"). This list must have the same number
#' of variables as tsumvarlst and be in respective order. If NULL, the default
#' names will be tsumvar'_tfun' (e.g., "TPA_UNADJ_SUM", "BA_SUM").
#' @param addseed Logical. If TRUE, add seedling counts to tree counts. Note:
#' tdomvar must be 'SPCD' or 'SPGRPCD'.
#' @param seedonly Logical. If TRUE, seedling counts only. Note: tdomvar
#' must be 'SPCD' or 'SPGRPCD'.
#' @param TPA Logical. If TRUE, tsumvarlst variable(s) are multiplied by the
#' respective trees-per-acre variable (see details) to get per-acre
#' measurements.
#' @param tfun Function. The name of the function to use to aggregate the data
#' (e.g., sum, mean, max).
#' @param ACI Logical. If TRUE, if ACI (All Condition Inventory) plots exist,
#' any trees on these plots will be included in summary. If FALSE, you must
#' include condition table.
#' @param tfilter String. Filter to subset the tree data before aggregating
#' (e.g., "STATUSCD == 1"). This must be in R syntax. If tfilter=NULL, user is
#' prompted.  Use tfilter="NONE" if no filters.
#' @param lbs2tons Logical. If TRUE, converts biomass or carbon variables from
#' pounds to tons (1 pound = 0.0005 short tons). If metric=TRUE, converts to  
#' metric tons, else short tons.
#' @param metric Logical. If TRUE, converts response to metric units based on
#' ref_conversion, if any variable in tsumvarlst is in
#' FIESTAutils::ref_estvar.  Note: if TPA, TPA is converted to trees per hectare
#' (TPH: (1/ tpavar * 0.4046860)).
#' @param getadjplot Logical. If TRUE, and adj='plot', adjfactors are 
#' calculated for nonsampled conditions at plot-level.
#' @param adjtree Logical. If TRUE, trees are individually adjusted by
#' adjustment factors.  Adjustment factors must be included in tree table (see
#' adjvar).
#' @param adjvar String. If adjtree=TRUE, the name of the variable to use for
#' multiplying by adjustment (e.g., tadjfac).
#' @param pltassgn Data.frame. If adj="samp", a data.frame with plot 
#' assignments of Estimation Unit and Stratum.
#' @param stratalut Data.frame. If adj="samp", a data.frame with strata-level
#' adjustment factors (e.g., ADJ_FACTOR_SUBP, ADJ_FACTOR_MICR, ADJ_FACTOR_MACR)
#' @param adjTPA Numeric. A tree-per-acre adjustment. Use for DESIGNCD=1
#' (annual inventory), if using less than 4 subplots. If using only 1 subplot
#' for estimate, adjTPA=4. The default is 1.
#' @param tderive List. List of derivative to add to output data (e.g., 
#' list(MEAN_DIA = 'AVG(DIA)'))
#' @param NAto0 Logical. If TRUE, convert NA values to 0.
#' @param tround Number. The number of digits to round to. If NULL, default=5.
#' @param pltidsWITHqry SQL query. A query identifying plots to sum (e.g., 
#' 'WITH pltids AS (SELECT cn AS PLT_CN FROM plot WHERE statecd=49 and INVYR=2018)')
#' @param pjoinid String. Name of unique identifier from pltidsWITHqry.
#' @param checkNA Logical. If TRUE, checks if NA values exist in necessary
#' variables.
#' @param returnDT Logical. If TRUE, returns data.table object(s). If FALSE,
#' returns data.frame object(s).
#' @param savedata Logical. If TRUE, saves data to outfolder.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. If out_layer = NULL,
#' default = 'treesum'. 
#' @param gui Logical. If gui, user is prompted for parameters.
#' 
#' @return A list of the following items: \item{treedat}{ Data frame. Plot or
#' condition-level table with aggregated tree attributes. } \item{sumvars}{
#' String vector. Name(s) of the output aggregated tree attributes. }
#' 
#' If savedata=TRUE\cr - treedat will be saved to the outfolder. \cr - a text
#' file of input parameters is saved to outfolder
#' ('outfn'_parameters_'date'.txt).
#' @note If a dat table is provided, the aggregated tree data will be merged to
#' table and NULL values will be output as 0.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' # Aggregate LIVE_CANOPY_CVR_PCT to plot
#' treesum <- datSumTree(tree = FIESTA::WYtree,
#'                       tsumvarlst = "TPA_UNADJ")$treedat
#' 
#' # Check results
#' treesum[treesum$PLT_CN == 40404737010690,]
#' FIESTA::WYtree[FIESTA::WYtree$PLT_CN == 40404737010690,]
#' 
#' @export datSumTree
datSumTree <- function(tree = NULL, 
                       seed = NULL, 
                       cond = NULL,
                       plt = NULL, 
                       subp_cond = NULL,  
                       subplot = NULL, 
                       datsource = "obj", 
                       dsn = NULL, 
                       dbconn = NULL,
                       tuniqueid = "PLT_CN", 
                       cuniqueid = "PLT_CN", 
                       puniqueid = "CN", 
                       bycond = FALSE, 
                       condid = "CONDID", 
                       bysubp = FALSE, 
                       subpuniqueid = "PLT_CN",
                       subpid = "SUBP", 
                       tsumvarlst = NULL, 
                       tsumvarnmlst = NULL, 
                       addseed = FALSE, 
                       seedonly = FALSE,
                       woodland = "Y",
                       TPA = TRUE, 
                       tfun = sum, 
                       ACI = FALSE, 
                       tfilter = NULL, 
                       lbs2tons = TRUE, 
                       metric = FALSE, 
                       getadjplot = FALSE, 
                       adjtree = FALSE, 
                       adjvar = "tadjfac",
                       adjTPA = 1, 
                       tderive = NULL,
                       NAto0 = FALSE, 
                       tround = 5,
                       pltidsWITHqry = NULL,
                       pjoinid = "PLT_CN",
                       checkNA = FALSE, 
                       returnDT = TRUE,
                       savedata = FALSE, 
                       savedata_opts = NULL,
                       gui = FALSE) {
  ####################################################################################
  ## DESCRIPTION: Aggregates tree variable(s) to plot(/cond)-level, 
  ##        using specified tree filters (ex. live trees only)
  ####################################################################################
  
  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## Set global variables  
  treex=seedx=cond.nonsamp.filter=meta=tvars2convert=
    ssumvarlst=cntvar <- NULL


  ## If gui.. set variables to NULL
  if (gui) ACI=bycond=tuniqueid=puniqueid=cuniqueid=TPA=tfun=adjtree=adjsamp=
	savedata=outfolder <- NULL

  ref_estvar <- FIESTAutils::ref_estvar
  twhereqry=swhereqry=tfromqry=sfromqry=pcwhereqry=pcfromqry=pcselectvars <- NULL
  datindb <- FALSE
  
  ## Query alias.
  talias. <- "t."
  salias. <- "s."

 
  ## For documentation
  # subplot Dataframe or comma-delimited file (*.csv). If getadjplot=TRUE, 
  # The subplot-level table with SUBP_STATUS_CD variable for calculating
  # adjustment factors by subplot.
  adjvarlst <- unlist(list(COND="ADJ_FACTOR_COND", SUBP="ADJ_FACTOR_SUBP", 
                           MICR="ADJ_FACTOR_MICR", MACR="ADJ_FACTOR_MACR"))

  ## SET VARIABLE LISTS
  biovars <- c("DRYBIO_BOLE", "DRYBIO_STUMP", "DRYBIO_BG", "DRYBIO_SAWLOG", 
               "DRYBIO_AG", "DRYBIO_STEM", "DRYBIO_STEM_BARK", "DRYBIO_STUMP_BARK", 
               "DRYBIO_BOLE_BARK", "DRYBIO_BRANCH", "DRYBIO_FOLIAGE", "DRYBIO_SAWLOG_BARK",
               "DRYBIOT", "DRYBIOM", "DRYBIOTB", "JBIOTOT")
  carbvars <- c("CARBON_BG", "CARBON_AG")

  ## SET VARIABLES TO CONVERT (from pounds to short tons.. * 0.0005)
  vars2convert <- c(biovars, carbvars, paste(biovars, "TPA", sep="_"), paste(carbvars, "TPA", sep="_"))
	
  timberonly <- FIESTAutils::ref_units[FIESTAutils::ref_units$WOODLAND == "N", "VARIABLE"]
  woodlandvars <- FIESTAutils::ref_units[FIESTAutils::ref_units$WOODLAND == "Y", "VARIABLE"]
 
  growvars <- c("TPAGROW_UNADJ", "GROWCFGS", "GROWBFSL", "GROWCFAL", "FGROWCFGS", 
                "FGROWBFSL", "FGROWCFAL")
  mortvars <- c("TPAMORT_UNADJ", "MORTCFGS", "MORTBFSL", "MORTCFAL", "FMORTCFGS", 
                "FMORTBFSL", "FMORTCFAL")
  remvars <- c("TPAREMV_UNADJ", "REMVCFGS", "REMVBFSL", "REMVCFAL", "FREMVCFGS", 
               "FREMVBFSL", "FREMVCFAL")
  tpavars <- c("TPA_UNADJ", "TPAMORT_UNADJ", "TPAGROW_UNADJ", "TPAREMV_UNADJ")
  propvar <- "CONDPROP_UNADJ"


  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(datSumTree)) 
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
  noplt=nocond <- TRUE
  pltsp <- FALSE
  
  ## Check dbconn
  ########################################################
  if (!is.null(dbconn)) {
    if (!DBI::dbIsValid(dbconn)) {
      message("invalid database dbconnection") 
      return(NULL)
    }
    datindb <- TRUE
  } else {  

    ## Check datsource
    datsourcelst <- c("obj", "csv", "sqlite", "gdb")
    datsource <- pcheck.varchar(var2check=datsource, varnm="datsource", 
		          checklst=datsourcelst, gui=gui, caption="Data source?") 
    if (is.null(datsource)) {
      if (!is.null(dsn) && file.exists(dsn)) {
        dsn.ext <- getext(dsn)
        if (!is.na(dsn.ext) && dsn.ext != "") {
          datsource <- ifelse(dsn.ext == "gdb", "gdb", 
		        ifelse(dsn.ext %in% c("db", "db3", "sqlite", "sqlite3"), "sqlite", 
                    ifelse(dsn.ext == "csv", "csv",
			                    ifelse(dsn.ext == "shp", "shp", "datamart"))))
        } 
      } else {
        stop("datsource is invalid")
      }
    }

    ## Check dsn
    if (datsource == "sqlite") {
      if (!is.null(dsn)) {
        dbconn <- DBtestSQLite(dsn, dbconnopen = TRUE, 
                             createnew = FALSE, returnpath = FALSE)
        if (is.null(dbconn)) {
          message("invalid database")
		      return(NULL)
        } else {
          datindb <- TRUE
        }
      } else {
        message("datsource = 'sqlite' and dsn is NULL")
        return(NULL)
      }
    } 
  }
	if (datindb) {
	  dbtablst <- DBI::dbListTables(dbconn)
	}
  

  ## Check bycond
  ###########################################################################
  bycond <- pcheck.logical(bycond, varnm="bycond", title="By condition?", 
		first="YES", gui=gui, stopifnull=TRUE)

  ## Check bysubp
  ###########################################################################
  bysubp <- pcheck.logical(bysubp, varnm="bysubp", title="By subplot?", 
		first="YES", gui=gui, stopifnull=TRUE)


  ## Check addseed
  addseed <- pcheck.logical(addseed, varnm="addseed", title="Add seeds?", 
		first="NO", gui=gui)

  ## Check seedonly
  seedonly <- pcheck.logical(seedonly, varnm="seedonly", title="Seed only?", 
		first="NO", gui=gui)
		
  ## Check woodland
  woodlandlst <- c("Y", "N", "only")
  woodland <- pcheck.varchar(var2check=woodland, varnm="woodland", 
		checklst=woodlandlst, gui=gui, caption="Woodland?") 


  if (addseed && seedonly) {
    message("addseed and seedonly are TRUE")
	  return(NULL)
  }
  
  ## Check tree, seedling tables
  ## If woodland in('N','only'), check for ref_species table to determine
  ## which species are woodland.
  ###########################################################################
  treenm=seednm=dbname=ref_sppnm=woodlandnm <- NULL

  if (!is.null(dbconn)) {

    if (!seedonly) {
      treex <- chkdbtab(dbtablst, tree)
      if (!is.null(treex)) {
        treeflds <- DBI::dbListFields(dbconn, treex)
        treenm <- treex
      }
    }

	  if (seedonly || addseed) {
      seedx <- chkdbtab(dbtablst, seed)
      if (!is.null(seedx)) {
        seedflds <- DBI::dbListFields(dbconn, seedx)
        seednm <- seedx
      } else if (addseed) {
        stop("must include seed data if addseed=TRUE")
      }
	  }
	  if (woodland %in% c("N", "only")) {
	    if (!seedonly) {
	      twoodlandref <- FALSE
	      twoodlandnm <- findnm("WOODLAND", treeflds, returnNULL=TRUE)
	      if (is.null(twoodlandnm)) {
	        twoodlandref <- TRUE	  
          ref_sppnm <- chkdbtab(dbtablst, "REF_SPECIES")
          if (!is.null(ref_sppnm)) {
            refflds <- DBI::dbListFields(dbconn, ref_sppnm)
            twoodlandnm <- findnm("WOODLAND", refflds, returnNULL=TRUE)
		        refspcdnm <- findnm("SPCD", refflds)
		        tspcdnm <- findnm("SPCD", treeflds)
		        if (is.null(twoodlandnm)) {
		          warning("WOODLAND attribute not in ref_species table... returning NULL")
		          return(NULL)
		        }
          } else {
		        warning("ref_species table not in database... returning NULL")
		        return(NULL)
          }
        }	
	    }
      if (seedonly || addseed) {
	      swoodlandref <- FALSE
	      swoodlandnm <- findnm("WOODLAND", seedflds, returnNULL=TRUE)
	      if (is.null(swoodlandnm)) {
	        swoodlandref <- TRUE	  
          ref_sppnm <- chkdbtab(dbtablst, "REF_SPECIES")
          if (!is.null(ref_sppnm)) {
            refflds <- DBI::dbListFields(dbconn, ref_sppnm)
            swoodlandnm <- findnm("WOODLAND", refflds, returnNULL=TRUE)
		        refspcdnm <- findnm("SPCD", refflds)
		        sspcdnm <- findnm("SPCD", seedflds)
		        if (seedonly && is.null(swoodlandnm)) {
		          warning("WOODLAND attribute not in ref_species table... returning NULL")
		          return(NULL)
		        }
          } else if (seedonly) {
		        warning("ref_species table not in database... returning NULL")
		        return(NULL)
          }
		    }
      }	  	  
    }	
  } else if (datsource %in% c("obj", "csv")) {
    if (!seedonly) {
      treex <- pcheck.table(tree, gui=gui, tabnm="tree", caption="Tree table?")
      if (!is.null(treex)) {
        treex <- setDT(int64tochar(treex))
        treeflds <- names(treex)
        treenm <- "treex"
      }
    }
    if (seedonly || addseed) {
      seedx <- pcheck.table(seed, gui=gui, tabnm="seed", caption="Seed table?")
      if (!is.null(seedx)) {
        seedx <- setDT(int64tochar(seedx))
        seedflds <- names(seedx)
        seednm <- "seedx"
      } else {
        if (addseed) {
          stop("must include seed data if addseed = TRUE")
        } else {
          stop("must include seed data if seedonly = TRUE")
        }
      }
    }
    if (woodland %in% c("N", "only")) {
      if (!seedonly) {
        twoodlandref <- FALSE
        twoodlandnm <- findnm("WOODLAND", treeflds, returnNULL=TRUE)
        if (is.null(twoodlandnm)) {
          twoodlandref <- TRUE	  
          ref_sppnm <- "ref_species"
          twoodlandnm <- "WOODLAND"
          refspcdnm <- "SPCD"
          tspcdnm <- findnm("SPCD", treeflds)
        }
      }
      if (seedonly || addseed) {
        swoodlandref <- FALSE
        swoodlandnm <- findnm("WOODLAND", seedflds, returnNULL=TRUE)
        if (is.null(swoodlandnm)) {
          swoodlandref <- TRUE	  
          ref_sppnm <- "ref_species"
          swoodlandnm <- "WOODLAND"
          refspcdnm <- "SPCD"
          sspcdnm <- findnm("SPCD", seedflds)
        }
      }		
    }	
  }  
    
  if (woodland %in% c("N", "only")) {
    if (!seedonly) {
      if (twoodlandref) {
	      wtfromqry <- paste0("\n JOIN ", ref_sppnm, 
					" ref ON (ref.", refspcdnm, " = ", talias., tspcdnm, ")")
	    } else {
	      wtfromqry <- NULL
	    }
	    if (woodland == "only") {
	      wtwhereqry <- paste(twoodlandnm, "= 'Y'")
      } else if (woodland == "N") {
	      wtwhereqry <- paste(twoodlandnm, "= 'N'")
      }
	  }
	  if (seedonly || addseed) {
      if (swoodlandref) {
	      wsfromqry <- paste0("\n JOIN ", ref_sppnm, 
					" ref ON (ref.", refspcdnm, " = ", salias., sspcdnm, ")")
	    } else {
	      wsfromqry <- NULL
	    }
	    if (woodland == "only") {
	      wswhereqry <- paste(swoodlandnm, "= 'Y'")
      } else if (woodland == "N") {
	      wswhereqry <- paste(swoodlandnm, "= 'N'")
      }
	  }
  }
  if ((seedonly || addseed) && is.null(seednm)) {
    message("must include seed table")
	  return(NULL)
  }
  if (!seedonly && is.null(treenm)) {
    message("must include tree table")
	  return(NULL)
  }
   
  ## Check unique identifiers and set unique keys if R objects
  ###########################################################################

  ## Check tuniqueid
  if (!seedonly) {
    tuniqueid <- pcheck.varchar(var2check = tuniqueid, varnm = "tuniqueid", 	
                                checklst = treeflds, caption = "UniqueID variable - tree", 
                                warn = paste(tuniqueid, "not in tree table"), stopifnull = TRUE)
    tsumuniqueid <- tuniqueid

    if (addseed) {
      if (!tuniqueid %in% seedflds) {
        stop(tuniqueid, " not in seedx")
      }
    }
    if (bysubp) {
      if (!subpid %in% treeflds) {
        stop(subpid, " not in tree")
      }
      if (addseed) {
        if (!subpid %in% seedflds) {
          stop("bysubp=TRUE but ", subpid, " is not in seed table") 
        }
      }
      tsumuniqueid <- c(tsumuniqueid, subpid)
    }
    if (bycond) {
      if (!condid %in% treeflds) {
        message(condid, " not in tree... assuming only 1 condition")
        treex[[condid]] <- 1
      }
      if (addseed) {
        if (!condid %in% seedflds) {
          message(condid, " not in seed table")
          seedx[[condid]] <- 1
        }
      }
      tsumuniqueid <- c(tsumuniqueid, condid)
    }
  }

  if (seedonly) {
    tuniqueid <- pcheck.varchar(var2check = tuniqueid, varnm = "tuniqueid", 	
		      checklst = seedflds, caption="UniqueID variable - seed", 
		      warn = paste(tuniqueid, "not in seed table", stopifnull = TRUE))
    tsumuniqueid <- tuniqueid

    if (bysubp) {
      if (!subpid %in% seedflds) {
        stop("bycond=TRUE but ", subpid, " is not in seed table") 
      }
      tsumuniqueid <- c(tsumuniqueid, subpid)
    }
    if (bycond) {
      if (!condid %in% seedflds) {
        message(condid, " not in seed... assuming only 1 condition")
        seedx[[condid]] <- 1
      }
      tsumuniqueid <- c(tsumuniqueid, condid)
    }
  }
     
  
  ### Check tsumvarlst
  ###########################################################################
  if (!seedonly) {
    tsumvarlst <- pcheck.varchar(var2check = tsumvarlst, varnm = "tsumvarlst", 
          checklst = treeflds, caption = "Aggregate variable(s)", 
          multiple = TRUE, stopifnull = FALSE)
    if (is.null(tsumvarlst) && is.null(tderive)) {
      stop("must include tsumvarlst or tderive variables")
    }
    if (!is.null(tsumvarlst) && any(tsumvarlst == tuniqueid)) {
      tsumvarlst[tsumvarlst == tuniqueid] <- "TPA_UNADJ"
    }
  }
  
  ## check seed table
  ## Note: to get counts measured, use TPA_UNADJ and TPA = FALSE
  if (addseed) {
    if (!is.null(tsumvarlst) && !any(tsumvarlst %in% c("TPA_UNADJ"))) {	  
      message("tsumvarlst must include TPA_UNADJ for seedonly or addseed")
      return(NULL)
    } 
  }
  
  ## Get name for summed tree variable(s)
  getnm <- FALSE
  if (is.null(tsumvarnmlst)) {
    getnm <- TRUE
  } else {
    if (!is.null(tsumvarlst) && length(tsumvarnmlst) != length(tsumvarlst)) {
      message(paste("number of names in tsumvarnmlst does not match number of tsumvars.",
 		                "using default names."))
      getnm <- TRUE
    }
  } 
  
  ## Get name for summed tree variable(s)
  getnm <- FALSE
  if (is.null(tsumvarnmlst)) {
    getnm <- TRUE
  } else {
    if (seedonly) {
	    if (length(tsumvarnmlst) != 1) {
	      message("tsumvarnmlst must be a vector of length 1 with name for number of seedlings")
		    getnm <- TRUE
	    } else {
	  	  ssumvarnmlst <- tsumvarnmlst
      }
    } else if (addseed) {
      tsumvarnmlst[tsumvarlst == "TPA_UNADJ"]
    }	  
    if (!is.null(tsumvarlst) && length(tsumvarnmlst) != length(tsumvarlst)) {
      message(paste("number of names in tsumvarnmlst does not match number of tsumvars.",
 		              "using default names."))
      getnm <- TRUE
    }
  } 
  
  ## Check tderive
  ###########################################################  
  if (!is.null(tderive)) {
    if (!is.list(tderive) || is.null(names(tderive))) {
      message("tderive must be a named list object...")
      message("e.g., tderive = list(SDI = '(POWER(DIA / 10, 1.605)) * TPA_UNADJ')")
      stop()
    } 
    if (!seedonly) {
      tderivevars  <- lapply(tderive, 
          function(x) treeflds[sapply(treeflds, function(y) grepl(y, x))])
      tderive_invalid <- names(tderivevars)[(lapply(tderivevars, length) == 0)]
      if (length(tderive_invalid) > 0) {
        message("invalid tderive... variable not in tree data...")
        message(toString(treeflds))
        stop()
      } else {
        tderivevars <- unique(unlist(tderivevars, use.names = FALSE))
      }
    }
  }
  
  ## CHECK TPA and tsumvars
  ###########################################################  
  TPA <- pcheck.logical(TPA, varnm="TPA", title="Calculate TPA?", first="NO", 
		                    stopifnull=TRUE, gui=gui)
 
  if (TPA) {
    if (!seedonly && !is.null(tsumvarlst)) {
      if (any(tsumvarlst %in% mortvars)) {
        if (!"TPAMORT_UNADJ" %in% treeflds) {
          stop("you must have TPAMORT_UNADJ in tree table to calculate trees per acre")
        }
        tpavar <- "TPAMORT_UNADJ"
      } else if (any(tsumvarlst %in% growvars)) {
        if (!"TPAGROW_UNADJ" %in% treeflds) {
          stop("you must have TPAGROW_UNADJ in tree table to calculate trees per acre")
        }
        tpavar <- "TPAGROW_UNADJ"
      } else if (any(tsumvarlst %in% remvars)){
        if (!"TPAREMV_UNADJ" %in% treeflds) {
          stop("you must have TPAREMV_UNADJ in tree table to calculate trees per acre")
        }
        tpavar <- "TPAREMV_UNADJ"
      } else {  
        if (!"TPA_UNADJ" %in% treeflds) {
          stop("you must have TPA_UNADJ in tree table to calculate trees per acre")
        }
        tpavar <- "TPA_UNADJ"
	    }
    }
	  if (seedonly || addseed) {
	    tpavar <- "TPA_UNADJ"
	    ssumvarlst <- "TPA_UNADJ"
	  }
  } else {
	  if (seedonly || addseed) {
      ssumvarlst <- "TREECOUNT_CALC"
	  }
  }
 
  ## Check cond table
  ###########################################################################
  condnm=plotnm <- NULL
  if (!is.null(cond) && is.data.frame(cond)) {
    condx <- pcheck.table(cond, gui=gui, tabnm="cond", caption="Condition table?")
    if (!is.null(condx)) {
      condx <- setDT(int64tochar(condx))
      condflds <- names(condx)
      condnm <- "condx"
      calias. <- "c."
    }	
  } else {
    condx <- chkdbtab(dbtablst, cond)
    if (!is.null(condx)) {
      condflds <- DBI::dbListFields(dbconn, condx)
      condnm <- condx
    }
  }  

  ## Check plt table
  ###########################################################################
  if (!is.null(plt) && is.data.frame(plt)) {
    pltx <- pcheck.table(plt, gui=gui, tabnm="plot", caption="Plot table?")
    if (!is.null(pltx)) {
      pltx <- setDT(int64tochar(pltx))
      pltflds <- names(pltx)
      plotnm <- "pltx"
    }
  } else {
    pltx <- chkdbtab(dbtablst, plt)
    if (!is.null(pltx)) {
      pltflds <- DBI::dbListFields(dbconn, pltx)
      plotnm <- pltx
    }
  }

  ## Check subplot tables
  ###########################################################################
  if (bysubp) {
    subpcondnm=subplotnm <- NULL
    if (!is.null(subp_cond) && is.data.frame(subp_cond)) {
      subpcondx <- pcheck.table(subp_cond, gui=gui, tabnm="subp_cond", 
				                        caption="Subpcond table?")
      if (!is.null(subpcondx)) {
        subpcondx <- setDT(int64tochar(subpcondx))
        subpcflds <- names(subpcondx)
        subpcondnm <- "subpcondx"
      }
      subplotx <- pcheck.table(subplot, gui=gui, tabnm="subplot", 
                               caption="Subplot table?")
      if (!is.null(subplotx)) {
        subplotx <- setDT(int64tochar(subplotx))
        subpflds <- names(subplotx)
        subplotnm <- "subplotx"
      }

    } else {
      subpcondx <- chkdbtab(dbtablst, subp_cond)
      if (!is.null(subpcondx)) {
        subpcflds <- DBI::dbListFields(dbconn, subpcondx)
        subpcondnm <- subpcondx
      }
      subplotx <- chkdbtab(dbtablst, subplot)
      if (!is.null(subplotx)) {
        subpflds <- DBI::dbListFields(dbconn, subplotx)
        subplotnm <- subplotx
      }
    }
  }

  ## Check uniqueids
  ##########################################################################
  if (!is.null(condnm)) {
    calias <- "c"
    if (!cuniqueid %in% condflds) {
      stop(cuniqueid, " not in cond")
    }
    if (bycond) {
      if (!condid %in% condflds) {
        stop("bycond=TRUE but ", condid, " is not in cond")
      }
      tjoinid <- c(tuniqueid, condid)
      cjoinid <- c(cuniqueid, condid)
    } else {
      tjoinid <- tuniqueid
      cjoinid <- cuniqueid  
    }
  }

  if (bysubp) {
    subpuniqueid <- cuniqueid
    subpids <- c(subpuniqueid, subpid)
  
    ## Check subpids
    if (!is.null(subpcondnm)) {
      if (!all(subpids %in% subpcflds)) {
        stop("uniqueids not in subp_cond: ", toString(subpids))
      }
      setkeyv(subpcondx, subpids)
    }
    if (!is.null(subplotnm)) {
      if (!all(subpids %in% subpflds)) {
        stop("uniqueids not in subplot: ", toString(subpids))
      }
      setkeyv(subplotx, subpids)
    }

    ## Set pltx to NULL   
    pltx <- NULL
  }


  ## Check lbs2tons
  ##########################################################################
  if (!seedonly) {
    lbs2tons <- pcheck.logical(lbs2tons, varnm="lbs2tons", title="Pounds to tons?", 
		                           first="YES", gui=gui, stopifnull=TRUE)
  }
  
  ## Check metric
  ##########################################################################
  metric <- pcheck.logical(metric, varnm="metric", title="Metric?", 
		                       first="NO", gui=gui, stopifnull=TRUE)

  ## Check checkNA
  ##########################################################################
  NAto0 <- pcheck.logical(NAto0, varnm="NAto0", title="Convert NA to 0?", 
		                      first="YES", gui=gui)
  if (is.null(NAto0)) NAto0 <- FALSE

  ## Check checkNA
  ##########################################################################
  checkNA <- pcheck.logical(checkNA, varnm="checkNA", title="Check NA values?", 
		                        first="YES", gui=gui)
  if (is.null(checkNA)) checkNA <- FALSE


  ## Check ACI. If TRUE, include all trees, If FALSE, filter for forested plots only 
  ## (COND_STATUS_CD = 1)
  ######################################################################################
  ACI <- pcheck.logical(ACI, varnm="ACI", title="Include ACI tree data?", 
		                    first="NO", gui=gui)

  ## Check getadjplot
  ###########################################################  
  getadjplot <- pcheck.logical(getadjplot, varnm="getadjplot", 
		                           title="Get plot adjustment?", first="NO", gui=gui)
  
  ## Check adjtree
  adjtree <- pcheck.logical(adjtree, varnm="adjtree", title="Adjust trees", 
		                        first="NO", gui=gui)
  if (getadjplot) adjtree <- TRUE
  
  ## Check pltidsWITHqry
  if (!is.null(pltidsWITHqry)) {
    if (!all(grepl("WITH", pltidsWITHqry))) {
      message("must include WITH in pltidsWITHqry...")
      message("e.g. \nWITH",
                    "\npltids AS", 
                    "\nSELECT CN FROM plt",
                    "\nWHERE countycd = 1")
      stop()
    }
    if (!all(grepl("pltids", pltidsWITHqry))) {
      message("must include pltids in pltidsWITHqry...")
      message("e.g. \nWITH",
              "\npltids AS", 
              "\nSELECT CN FROM plt",
              "\nWHERE countycd = 1")
      stop()
    }
    
    ## Check pjoinid
    if (!all(sapply(pjoinid, grepl, pltidsWITHqry))) {
      stop("pjoinid (", toString(pjoinid), ") not in pltidsWITHqry: \n", pltidsWITHqry)
    }
  }
  

  ## if adjtree = TRUE, first check if 'tadjfac' is in treeflds
  ## If 'tadjfac' is not in treeflds, 
  if (adjtree) {
    if (!seedonly) {
      if (!adjvar %in% treeflds) {
        if (is.null(condnm) && is.null(pltidsWITHqry)) {
          message("must include cond or pltidsWITHqry or ", adjvar, " in tree table when adj != 'none'")
          stop()
        } else {
          if (!is.null(pltidsWITHqry) && grepl("pltidsadj", pltidsWITHqry)) {
            getadjplot <- FALSE
          } else {
            getadjplot <- TRUE
          }
        }
        if (addseed) {
          if (!adjvar %in% seedflds) {
            if (is.null(condnm)) {
              message("must include cond or ", adjvar, " in seed table when adj != 'none'")
              stop()
            } 
          }
        }
      }
    } else if (seedonly) {
      if (!adjvar %in% seedflds) {
        if (is.null(condnm) && is.null(pltidsWITHqry)) {	  
          message("must include cond or ", adjvar, " in tree table when adj != 'none'")
          stop()
        } else {
          if (is.null(condnm)) {
            getadjplot <- TRUE
          }
        }
      }
    }
  }
  
  ### Get tfun used for aggregation
  ###########################################################################
  tfunlst <- c("sum", "mean", "max", "min", "length", "median")
  sqltfun_ver <- c("SUM", "AVG", "MAX", "MIN", "COUNT", "MEDIAN")

  if (is.null(tfun)) {
    if (gui) {
      tfunnm <- select.list(tfunlst, title="Aggregate function", multiple=FALSE)
      if (tfunnm == "") stop("")
      tfun <- get(tfunnm)
      tfunstr <- sqltfun_ver[which(tfunlst == tfunnm)]
    } else {
      tfun <- sum
      tfunnm <- "sum"
      tfunstr <- "SUM"
    }
  }

  if (!is.function(tfun)) {
    stop("tfun is not a function")
  } else {
    if(!is.null(tsumvarlst) && tuniqueid %in% tsumvarlst && !identical(tfun, sum))
      stop("use sum with PLT_CN for getting number of trees.")
    tfunnm <- deparse(substitute(tfun))
    if (!tfunnm %in% tfunlst) {
      stop("invalid tfun supplied")
    } else {
      tfunstr <- sqltfun_ver[which(tfunlst == tfunnm)]
    }
    
  }
  message(paste0("aggregating data using '", tfunnm, "'"))

  ## CHECK tround
  if (is.null(tround) || !is.numeric(tround)) {
    warning("tround is invalid.. rounding to 5 digits")
    tround <- 5
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
    outlst$out_layer <- "treesum"
  }
   
  
  #########################################################################################
  #########################################################################################
  ## Build queries
  #########################################################################################
  #########################################################################################
  
  ## Build fromqry for twithqry/swithqry
  #############################################################################
  if (!seedonly) {
    tfromqry <- paste0("\n FROM ", treenm, " t")
  }
  if (seedonly || addseed) {
    sfromqry <- paste0("\n FROM ", seednm, " s")
  }
  
    
  #############################################################################
  ## Check and build where queries for filtering tree data
  #############################################################################

  ### Check tfilter and build WHERE qry for twithqry/swithqry
  ###############################################################
  if (!is.null(tfilter)) {
    if (!seedonly) {
      twhereqry <- paste("\n WHERE", RtoSQL(tfilter, x=treeflds))
    }
    if (addseed || seedonly) {
      sfilter <- suppressMessages(check.logic(seedflds, statement=tfilter, stopifinvalid=FALSE))
      if (!is.null(sfilter)) {
        swhereqry <- paste("\n WHERE", RtoSQL(tfilter))
      }
    }
  }

  ## Build queries for adjfactors
  ##########################################################################
  if (adjtree) {
    adjalias <- "adj"
    adjalias. <- paste0(adjalias, ".")
    adjjoinid <- pjoinid
    if (getadjplot) {
      adjjoinid <- cuniqueid
      nonsamp.filter <- NULL
      
      ## Build WHERE query for removing nonsampled conditions 
      condstatusnm <- findnm("COND_STATUS_CD", condflds, returnNULL=TRUE)
      if (is.null(condstatusnm)) {
        message("COND_STATUS_CD not in dataset... assuming all sampled conditions in cond")
      } else {
        cond.nonsamp.filter <- paste(condstatusnm, "<> 5")
      }
      if (ACI) {
        nfcondstatusnm <- findnm("NF_COND_STATUS_CD", condflds, returnNULL=TRUE)
        if (is.null(nfcondstatusnm)) {
          message("NF_COND_STATUS_CD not in dataset... assuming all sampled nonforest conditions in cond")
        } else {
          cond.nonsamp.filter.ACI <- paste0("(", nfcondstatusnm, " IS NULL OR ", nfcondstatusnm, " <> 5)")
        }
        if (!is.null(cond.nonsamp.filter)) {
          cond.nonsamp.filter <- paste(cond.nonsamp.filter, "AND", cond.nonsamp.filter.ACI)
        } else {
          cond.nonsamp.filter <- cond.nonsamp.filter.ACI
        }
      }	
      nonsamp.filter <- cond.nonsamp.filter
     
      if (bysubp) {
        ## Build WHERE query for removing nonsampled subplots 
        if (sum(is.null(subpcondx), is.null(condx)) < 3) {
          if (sum(is.null(subpcondx), is.null(condx)) == 2) {
            message("must include subp_cond and cond to adjust to plot")
			      return(NULL)
          } else if (is.null(condx)) {
            message("must include cond to adjust to plot")
			      return(NULL)
          } else if (is.null(subpcondx)) {
            message("must include subp_cond to adjust to plot")
			      return(NULL)
          }
        } 
        
        ## Build query for removing nonsampled conditions 
        if (!is.null(subplotnm)) {
          subpstatusnm <- findnm("SUBP_STATUS_CD", subpflds, returnNULL=TRUE)
          if (is.null(subpstatusnm)) {
            message("SUBP_STATUS_CD not in dataset... assuming all sampled conditions in subplot")
          } else {
            subp.nonsamp.filter <- paste(subpstatusnm, "<> 3")
          }
          if (ACI) {
            nfsubpstatusnm <- findnm("NF_SUBP_STATUS_CD", subpflds, returnNULL=TRUE)
            if (is.null(nfsubpstatusnm)) {
              message("NF_SUBP_STATUS_CD not in dataset... assuming all sampled nonforest conditions in subplot")
            } else {
              subp.nonsamp.filter.ACI <- paste0("(", nfsubpstatusnm, " IS NULL OR ", nfsubpstatusnm, " <> 3)")
            }
            if (!is.null(subp.nonsamp.filter)) {
              subp.nonsamp.filter <- paste0(subp.nonsamp.filter, " AND ", subp.nonsamp.filter.ACI)
            } else {
              subp.nonsamp.filter <- subp.nonsamp.filter.ACI
            }
          }
        
          if (!is.null(nonsamp.filter)) {
            nonsamp.filter <- paste0(nonsamp.filter, " AND ", subp.nonsamp.filter)
          } else {
            nonsamp.filter <- subp.nonsamp.filter
          }
        }
        ## Check PROP names and build query for calculating adjustment factors
        if (seedonly) {
          subppropnames <- "MICRCOND_PROP"
        } else {
          subppropnames <- c("SUBPPROP_UNADJ", "MICRPROP_UNADJ", "MACRPROP_UNADJ")
        }
        subppropnameschk <- sapply(subppropnames, findnm, condflds, returnNULL = TRUE)
        if (length(subppropnameschk) < length(subppropnames)) {
          subppropmiss <- subppropnames[!subppropnames %in% subpflds]
          if (length(subppropmiss) > 0) {
            message("*COND_PROP variables missing: ", toString(subppropmiss))
            return(NULL)
          }
        } else {
          subppropnames <- subppropnameschk
        }
        
        subpadj.qry <- paste("SELECT", toString(c(cuniqueid, subpid)))
        for (i in 1:length(subppropnames)) {
          nm <- subppropnames[i]
          padj <- paste0(", \n  COALESCE(1 / sum(", nm, "), 0) ADJ_FACTOR_", substr(nm, 1,4))
          padj.qry <- paste0(padj.qry, padj)
        }	
        
        subpcx <- subpsamp(cond = condx, 
                           subp_cond = subpcondx, 
                           subplot = subplotx, 
                           subpuniqueid = subpuniqueid, 
                           subpid = subpid, ACI=ACI)

        adjfacdata <- getadjfactorPLOTqry(cond = subpcx,
                                          datsource = datsource, 
                                          dbconn = dbconn, 
                                          cuniqueid = c(subpuniqueid, subpid),
                                          areawt = "CONDPROP_UNADJ")
      } else {  ## if (bysubp = FALSE)

	  
	      ## Check PROP names and build query for calculating adjustment factors
        if (seedonly) {
          propnames <- "MICRPROP_UNADJ"
        } else {
	        propnames <- c("CONDPROP_UNADJ", "SUBPPROP_UNADJ", "MICRPROP_UNADJ", "MACRPROP_UNADJ")
        }
        propnameschk <- sapply(propnames, findnm, condflds, returnNULL = TRUE)
        if (length(propnameschk) < length(propnames)) {
	        propmiss <- propnames[!propnames %in% condflds]
	        if (length(propmiss) > 0) {
	          message("*PROP_UNADJ variables missing: ", toString(propmiss))
		        return(NULL)
	        }
        } else {
          propnames <- propnameschk
        }
        conda <- paste0(condnm, ".")
	      padj.qry <- paste0("SELECT ", conda, cuniqueid)
	      for (i in 1:length(propnames)) {
	        nm <- propnames[i]
		      padj <- paste0(", \n  COALESCE(1 / sum(", nm, "), 0) ADJ_FACTOR_", substr(nm, 1,4))
		      padj.qry <- paste0(padj.qry, padj)
	      }	
	      
	      ## Build from query
	      padjfromqry <- paste0("\n FROM ", condnm)
	      if (!is.null(pltidsWITHqry)) {
	        padjjoinqry <- getjoinqry(cuniqueid, pjoinid, conda, "pltids.")
	        padjfromqry <- paste0(padjfromqry,
	                              "\n JOIN pltids ", padjjoinqry)
	      }
	  
	      ## Build final adjustment query
	      padj.qry <- paste0(padj.qry, 
	                         padjfromqry, 
	                         "\n WHERE ", cond.nonsamp.filter, 
					                 "\n GROUP BY ", conda, cuniqueid)
        #message(padj.qry)	  
        
      } ## END bysubp
      
	    if (addseed || seedonly) {
	      sadjcase <- paste0("adj.", adjvarlst[["MICR"]])
	    }	  
    }  ## END getadjplot
  
  
    ## Build query for select CASE statement to add adjfactors
	  ######################################################################################
    adjalias. <- NULL
	  if (!seedonly) {
	    if ("tadjfac" %in% treeflds) {
	      tadjcase <- "tadjfac"
	    } else if ("TPROP_BASIS" %in% treeflds) {
	      tadjcase <- paste0(
	           "\n      CASE WHEN t.TPROP_BASIS = 'MICR' THEN ", adjalias., adjvarlst[["MICR"]],
             "\n           WHEN t.TPROP_BASIS = 'MACR' THEN ", adjalias., adjvarlst[["MACR"]],
             "\n           ELSE ", adjalias., adjvarlst[["SUBP"]], " END")
	    } else if ("DIA" %in% treeflds) {
	      tadjcase <- paste0(
	           "\n      CASE WHEN t.DIA IS NULL THEN ", adjalias., adjvarlst[["SUBP"]],
             "\n           WHEN MIN(t.DIA, 5 - 0.001) THEN ", adjalias., adjvarlst[["MICR"]],
	           "\n           WHEN MIN(t.DIA, 9999 - 0.001) THEN ", adjalias., adjvarlst[["SUBP"]],
             "\n           ELSE ", adjalias., adjvarlst[["MACR"]], " END")
 	    } else {
	      message("for adjfactors, need TPROP_BASIS or DIA in tree data")
		    return(NULL)
	    }
	  }
	  if (addseed || seedonly) {
	    if ("tadjfac" %in% seedflds) {
	      sadjcase <- "tadjfac"
	    } else {
	      sadjcase <- paste0("adj.", adjvarlst[["MICR"]])
	    }
	  }
	}	  
   
  
  
  #############################################################################
  ## Build tsumvardf for tree SELECT statement
  #############################################################################
  tsumvardf <- 
       data.frame(TSUMVAR = character(), # tsumvar
                  TABLE = character(),   # which table tsumvar is from ('TREE', 'SEED')
						      NEW = character(),     # new tsumvar, after manipulations
						      NAME = character(),    # name of output variable (tsumvarnm)
						      TUNITS = character(),  # units (from ref_units)
						      DERIVE = logical())    # if from tderive
		
  if (!is.null(tsumvarlst)) {
    
    ## Add tsumvarlst to tsumvardf				  
    if (TPA) {
      #if (!seedonly) {
	    tpavarid <- which(tsumvarlst == tpavar)
  	  tpavarnm <- tpavar
	  #}
    } else {
      tpavarnm <- NULL
    } 
    if (any(tsumvarlst %in% tpavars)) {
      cntvar <- tsumvarlst[tsumvarlst %in% tpavars]
      cntvarid <- which(tsumvarlst %in% tpavars)
	    tsumvarlst <- tsumvarlst[-cntvarid]
	    if (!is.null(tsumvarnmlst)) {
	      cntnm <- tsumvarnmlst[cntvarid]
	      tsumvarnmlst <- tsumvarnmlst[-cntvarid]
	    } else {
	      if (length(cntvar) > 1) {
	        cntnm <- sapply(cntvar, function(x) strsplit(x, "_"))
          cntnm <- paste0("COUNT_", sapply(cntnm, '[', 2))
		      #cntnm[cntnm == "COUNT_ADJ"] <- "COUNT"
	      } else {
	        cntnm <- "COUNT"
	      }
	      tsumvarnmlst <- tsumvarlst
	    }
    } else {
      if (is.null(tsumvarnmlst)) {
	      tsumvarnmlst <- tsumvarlst
	    }
    }
  
    if (seedonly) {
      NEWt <- ifelse(TPA, cntvar, paste0(cntvar, " > 0)"))
      tsumvardf <- rbind(tsumvardf, 
                         data.frame(TSUMVAR = "TPA_UNADJ", 
	                                  TABLE = "SEED", 
								                    NEW = NEWt,
                                    NAME = "COUNT",
						                        TUNITS = "trees",
	  	                              DERIVE = FALSE))
    } else { 
      if (addseed) {  
        NEWt <- ifelse(TPA, cntvar, 
		                     paste0(cntvar, " > 0)"))
	      #NAMEs <- ifelse(getadjplot, paste0(NAMEs, "_ADJ"), NAMEs)
	      NAMEs <- ifelse(TPA, paste0(cntnm, "_SEED"), cntnm)

	      tsumvardf <- rbind(tsumvardf, 
	                         data.frame(TSUMVAR = cntvar, 
							                        TABLE = "TREE",
									                    NEW = NEWt,
						                          NAME = paste0(cntnm, "_TREE"),
									                    TUNITS = "trees",
									                    DERIVE = FALSE))
 	      tsumvardf <- rbind(tsumvardf, 
                           data.frame(TSUMVAR = cntvar, 
                                      TABLE = "SEED",
                                      NEW = NEWt,
                                      NAME = NAMEs,
                                      TUNITS = "trees",
                                      DERIVE = FALSE))						
	      tsumvardf <- rbind(tsumvardf, 
	                         data.frame(TSUMVAR = cntvar, 
							                        TABLE = "TREESEED",
	                                    NEW = NEWt,
						                          NAME = cntnm,
									                    TUNITS = "trees",
									                    DERIVE = FALSE))
      } else {
	      if (!is.null(cntvar)) {
	        for (cvar in cntvar) {
		        NEW <- ifelse(TPA, cvar, 
		                     paste0(cvar, " > 0)"))
	          tsumvardf <- rbind(tsumvardf, 
                               data.frame(TSUMVAR = cvar, 
                                          TABLE = "TREE",
                                          NEW = NEW,
                                          NAME = cntnm,
                                          TUNITS = "trees",
                                          DERIVE = FALSE))
	 	      }							
	      }
      }
	    if (length(tsumvarlst) > 0) {
        tsumvardf <- rbind(tsumvardf,
                           data.frame(TSUMVAR = tsumvarlst, 
                                      TABLE = "TREE", 
                                      NEW = tsumvarlst,
                                      NAME = tsumvarnmlst,
                                      TUNITS = "trees",
                                      DERIVE = FALSE))
	    }			
    }
  

    ### Convert variables from pound to tons if lbs2tons=TRUE
    ########################################################################### 
    if (any(tsumvarlst %in% vars2convert)) {
      tvars2convert <- tsumvarlst[which(tsumvarlst %in% vars2convert)]
	
	    tvarnew <- tvars2convert
	    tunits <- "pounds"
      if (lbs2tons) {
	      tvarnew <- paste0(tvarnew, "_TON")
	      message("converting pounds to tons: ", toString(tvars2convert))
	      convfac <- 0.0005
	      tunits <- "tons"
	    if (metric) {
	      message("converting tons to metric tons: ", toString(tvars2convert))
		    tvarnew <- paste0(tvarnew, "m")
	      convfac <- 0.0005 * 0.90718474
	      tunits <- "metric tons"
      }
	  } else {
	    if (metric) {
	      message("converting pounds to kilograms: ", toString(tvars2convert))
		    tvarnew <- paste0(tvarnew, "_KG")
	      convfac <- 0.45359237
	      tunits <- "kilograms"
      } else {
	      convfac <- 1
		    tunits <- "pounds"
	    }
    }	
	  if (convfac != 1) {
      tsumvardf[tsumvardf$TSUMVAR %in% tvars2convert,"NEW"] <- 
           sapply(tvars2convert, function(x) paste0(x, " * ", convfac))
      }		   
      tsumvardf[tsumvardf$TSUMVAR %in% tvars2convert,"NAME"] <- tvarnew
      tsumvardf[tsumvardf$TSUMVAR %in% tvars2convert,"TUNITS"] <- tunits
	
	    if (TPA) {
        tsumvardf[tsumvardf$TSUMVAR %in% tvars2convert,"NEW"] <- 
	         paste0(tsumvardf[tsumvardf$TSUMVAR %in% tvars2convert,"NEW"],
                                 " * ", tpavar)
 	    }  
    }	    
  
    ## Add conversion to metric to tsumvardf
    if (any(!tsumvarlst %in% vars2convert)) {
      tvar2convert <- tsumvarlst[!tsumvarlst %in% vars2convert]
	
	    for (tvar in tvar2convert) {
	      tvarnew <- tvar
 	      if (tvar %in% ref_units$VARIABLE) { 
          tunits <- unique(ref_units$UNITS[ref_units$VARIABLE == tvar])
        } else {
          message(tvar, " not in ref_estvar... no units found")
		      metric <- FALSE
        }
	  
	      if (metric) {
          munits <- ref_units$METRICUNITS[ref_units$VARIABLE == tvar]
          convfac <- ref_conversion$CONVERSION[ref_conversion$METRIC == munits]
          tvarnew <- paste0(tvarnew, "_m")
 	        message("converting ", tunits, " to ", munits, ": ", tvar)
		      tunits <- munits
	  
          tsumvardf[tsumvardf$TSUMVAR == tvar,"NEW"] <- 
                                 paste0(tvar, " * ", convfac)
      		   
          tsumvardf[tsumvardf$TSUMVAR == tvar,"NAME"] <- tvarnew
          tsumvardf[tsumvardf$TSUMVAR == tvar,"TUNITS"] <- tunits
	      }
      }	
	    if (TPA) {
        tsumvardf[tsumvardf$TSUMVAR %in% tvar2convert,"NEW"] <- 
	          paste0(tsumvardf[tsumvardf$TSUMVAR %in% tvar2convert,"NEW"],
                                 " * ", tpavar)
 	    }  
    }
    
    ### Add adjTPA to tsumvardf
    ###########################################################################
    if (TPA) {

      ## Check adjTPA (default = 1)
      ## (e.g., if adjTPA=4 (only 1 subplot measured), multiply TPA* by 4)
      if (adjTPA) {
        if (is.null(adjTPA)) {
          warning("adjTPA is invalid, assuming no adjustments")
          adjTPA <- 1
        } else if (!is.numeric(adjTPA)) {
          warning("adjTPA must be a numeric number from 1 to 4")
          return(NULL)
        } else if (!adjTPA %in% 1:4) {
          warning("adjTPA must be between 1 and 4")
          return(NULL)
        } else if (adjTPA > 1) {
	        if (seedonly) {
		        subpnm <- findnm("SUBP", seedflds, returnNULL=TRUE)
          } else {
            subpnm <- findnm("SUBP", treeflds, returnNULL=TRUE)
		      }
          if (is.null(subpnm)) {
            warning("SUPB variable not in table")
            return(NULL)
          }
          if (datindb) {
            message("multiplying ", tpavar, " by ", adjTPA)
          } else {
            if (adjTPA == 2 && any(treex[, unique(get(subpnm)), by=tuniqueid][[2]] > 3)) {
              warning("more than 3 SUBP in dataset")
              return(NULL)
            } else if (adjTPA == 3 && any(treex[, unique(get(subpnm)), by=tuniqueid][[2]] > 2)) {
              warning("more than 2 SUBP in dataset")
              return(NULL)
            } else if (adjTPA == 4 && any(treex[, unique(get(subpnm)), by=tuniqueid][[2]] > 1)) {
              warning("more than 1 SUBP in dataset")
              return(NULL)
            }
          }
        }
      }        
   
	    ## If metric=TRUE, convert trees per acre to trees per hectare
	    if (metric) {
	      ac2ha <- 0.40468564
        message("converting ", tpavar, " from acres to hectares")
        tsumvardf$NEW <- paste0(tsumvardf$NEW, " * 1 / ", ac2ha,")")
	  
	      if (getnm) {
	        tsumvardf$NAME <- paste0(tsumvardf$NAME, "_TPH")
        }
	    } else {	  
	      if (getnm) {
	        tsumvardf$NAME <- paste0(tsumvardf$NAME, "_TPA")
        }
	    }	
	    if (adjTPA > 1) {
        tsumvardf$NEW <- paste0(tsumvardf$NEW, " * ", adjTPA) 
 
 	      if (getnm) {
	        tsumvardf$NAME <- paste0(tsumvardf$NAME, adjTPA)
        }
	    }
    }
  
    ## Define name - add _ADJ to name if adjusting
    if (adjtree) {
      if (getnm) {
        tsumvardf$NAME <- paste0(tsumvardf$NAME, "_ADJ") 
      }
      tsumvardf$NEW <- paste0(tsumvardf$NEW, " * tadjfac")
    }   

    ## Add a new column to SUM variables
    ###########################################################################
  #  tsumvardf$SELECT <- paste0("\n  COALESCE(SUM(", tsumvardf$NEW, "),0) AS ", 
  #                           tsumvardf$NAME)
    tsumvardf$SELECT <- paste0("COALESCE(", tfunstr, "(", tsumvardf$NEW, "),0)")
  
  }
  
  ## Add derived variables to tsumvardf
  ###########################################################################
  if (!is.null(tderive)) {
    for (i in 1: length(tderive)) {
      tderivedf <- data.frame(TSUMVAR = tderive[[i]], 
                              TABLE = "TREE", 
                              NEW = tderive[[i]], 
                              NAME = names(tderive)[i], 
                              TUNITS = "trees",
                              DERIVE = TRUE,
                              SELECT = tderive[[i]])
      tsumvardf <- rbind(tsumvardf, tderivedf)
      tsumvarlst <- unique(c(tsumvarlst, tderive[[i]]))
    }
  }

  ## Round variables
  ###########################################################################
  if (!is.null(tround)) {
    ## Add a new column with SELECT statement
    tsumvardf$SELECT <- paste0("\n  ROUND(", tsumvardf$SELECT, ", ", tround, ") AS ", 
                               tsumvardf$NAME)
  } else {
    tsumvardf$SELECT <- paste0(tsumvardf$SELECT, " AS ", tsumvardf$NAME)
  }
  
  ### Define name - adding tfilter 
  ###########################################################################
  if (!is.null(tfilter) && getnm) {
    ref <- ref_estvar[ref_estvar$ESTVAR %in% tsumvarlst, ] 
    ref <- ref[grep(gsub(" ", "", tfilter), gsub(" ", "", ref$ESTFILTER)), ]
    fname <- ref[, "FILTERNM"][1]
    if (!is.na(fname)) {
      if (fname == "standing-dead") fname <- "dead"
      tsumvardf$NAME <- paste0(tsumvardf$NAME, "_", fname)
    }
  }
  
  if (addseed) {
    if (!is.null(tround)) {
      tsumvardf$SELECT[tsumvardf$TABLE == "TREE"] <- 
           paste0("\n  ROUND(COALESCE(SUM(CASE WHEN src = 'TREE' THEN ", 
	                 tsumvardf$NEW[tsumvardf$TABLE == "TREE"], " ELSE 0 END),0),", tround, ")",
					 " AS ", tsumvardf$NAME[tsumvardf$TABLE == "TREE"])
  
      tsumvardf$SELECT[tsumvardf$TABLE == "SEED"] <- 
           paste0("\n  ROUND(COALESCE(SUM(CASE WHEN src = 'SEED' THEN ", 
	                 tsumvardf$NEW[tsumvardf$TABLE == "SEED"], " ELSE 0 END),0),", tround, ")",
					 " AS ", tsumvardf$NAME[tsumvardf$TABLE == "SEED"])
    }    
  }
  
  ## If getadjplot, create pltidsWITHqry or append to if not NULL
  ################################################################
  if (adjtree && getadjplot) {
    if (is.null(pltidsWITHqry)) {
      pltidsWITHqry <- paste0("WITH ",
                   "\npltidsadj AS \n(", padj.qry, ")")
	  } else {
      pltidsWITHqry <- paste0(pltidsWITHqry, ", ",
                   "\n----- calculate adjustment factors",
                   "\npltidsadj AS \n(", padj.qry, ")")
    }	
  }	

  
  #############################################################################
  ## Build SELECT query
  #############################################################################
  
    
  ## Build WITH query - get tree data (tdat)
  #################################################################################
  adjalias. <- "adj."
  twithalias <- "tdat"
  
  if (!seedonly) {
    
    ## Build twithqry
    twithqry <- "SELECT 'TREE' src,"
	  twithSelect <- unique(c(tsumuniqueid, "SUBP", "TREE", 
         unique(c(tpavarnm, tsumvardf$TSUMVAR[tsumvardf$TABLE == "TREE" & !tsumvardf$DERIVE]))))
	  if (!is.null(tderive)) {
      tderivevars <- tderivevars[!tderivevars %in% twithSelect]
      if (length(tderivevars) > 0) {
	      twithSelect <- c(twithSelect, tderivevars)
  	  }

    }
  
	  twithqry <- paste(twithqry, toString(paste0(talias., twithSelect)))	

	  if (adjtree) {
	    tadjjoinqry <- getjoinqry(adjjoinid, cuniqueid, adjalias., talias.)
	    if (adjvar %in% treeflds) {
	      tadjcase <- adjvar
	      twithfromqry <- tfromqry
	    } else {
        tadjcase <- paste0(
          "\n      (CASE WHEN t.DIA IS NULL THEN ", adjalias., adjvarlst[["SUBP"]],
          "\n           WHEN MIN(t.DIA, 5 - 0.001) THEN ", adjalias., adjvarlst[["MICR"]],
          "\n           WHEN MIN(t.DIA, 9999 - 0.001) THEN ", adjalias., adjvarlst[["SUBP"]],
          "\n           ELSE ", adjalias., adjvarlst[["MACR"]], " END) AS tadjfac")
        twithfromqry <- paste0(tfromqry,
                               "\n JOIN pltidsadj adj ", tadjjoinqry)
	    }
	    twithqry <- paste0(twithqry, ", ", tadjcase)
	  } else {
	    if (!is.null(pltidsWITHqry)) {
	      tjoinqry <- getjoinqry(tuniqueid, pjoinid, talias., "pltids.")
	      twithfromqry <- paste0(tfromqry,
	                             "\n JOIN pltids ", tjoinqry)
	    } else {
	      twithfromqry <- tfromqry
	    }
	  }
	  
	  ## WHERE statement - Woodland
	  twithwhereqry <- twhereqry
	  if (woodland %in% c("N", "only")) {
	    twithfromqry <- paste0(twithfromqry, wtfromqry)
	    if (is.null(twithwhereqry)) {
	      twithwhereqry <- paste0("\n WHERE ", wtwhereqry)
	    } else {
	      twithwhereqry <- paste0(twithwhereqry, " AND ", wtwhereqry)
	    }
	  }	
	  
	  ## Build final tree WITH query
	  twithqry <- paste0(twithqry,
	                     twithfromqry,
	                     twithwhereqry)

	  ## Build WITH query - seedling data (sdat)
	  ################################################################
	  if (addseed) {
	    swithalias <- "sdat"
	    
	    nbrvar <- length(tsumvardf$TSUMVAR[tsumvardf$TABLE == "TREE"][
	      !tsumvardf$TSUMVAR[tsumvardf$TABLE == "TREE" & !tsumvardf$DERIVE] %in% tpavarnm])  
	    swithqry <- paste0("\n SELECT 'SEED' src, ", 
	                       toString(paste0(salias., unique(c(tsumuniqueid, "SUBP")))))
	    swithqry <- paste0(swithqry, ", 0, ", paste0(salias., "TPA_UNADJ"))
	    if (!is.null(tderive)) {
        if (length(tderivevars) > 0) {
	        nbrvar <- nbrvar + length(tderivevars)
	      }
      }
	    if (nbrvar > 0) {
	      swithqry <- paste0(swithqry, ", ", toString(rep('null', nbrvar)))
	    }
	    
	    if (adjtree) {
	      sadjjoinqry <- getjoinqry(adjjoinid, cuniqueid, adjalias., salias.)
	      if (adjvar %in% seedflds) {
	        sadjcase <- adjvar
	        swithfromqry <- sfromqry
	      } else {
	        sadjcase <- paste0(adjvarlst[["MICR"]], " AS tadjfac")
	        swithfromqry <- paste0(sfromqry,
	                               "\n INNER JOIN pltidsadj adj ", sadjjoinqry)
	      }
	      swithqry <- paste0(swithqry, ", ", sadjcase)
	    } else {
	      if (!is.null(pltidsWITHqry)) {
	        sjoinqry <- getjoinqry(tuniqueid, pjoinid, salias., "pltids.")
	        swithfromqry <- paste0(sfromqry,
	                               "\n JOIN pltids ", sjoinqry)
	      } else {
	        swithfromqry <- sfromqry
	      }
	    }
	    
	    ## WHERE statement - Woodland
	    swithwhereqry <- swhereqry
	    if (woodland %in% c("N", "only")) {
	      swithfromqry <- paste(swithfromqry, wsfromqry)
	      if (is.null(swithwhereqry)) {
	        swithwhereqry <- paste0("\n WHERE ", wswhereqry)
	      } else {
	        swithwhereqry <- paste0(swithwhereqry, " AND ", wswhereqry)
	      }
	    }
	  
	    ## Build final seedling WITH query
	    swithqry <- paste0(swithqry,
	                       swithfromqry,
	                       swithwhereqry)
	    
	    ## Build UNION WITH query, including tree and seedling data
	    twithqry <- paste0(twithqry, 
                         "\n UNION",
                         swithqry)
	  }
	  
  } else {  ## if seedonly
  
    swithalias <- "tdat"
    
    swithqrySelect <- c(tsumuniqueid, "SUBP", "TPA_UNADJ")
    swithqry <- paste0("SELECT ", toString(paste0(salias., swithqrySelect))) 	
    
    if (adjtree) {
      #sadjcase <- paste0(adjvarlst[["MICR"]], " AS tadjfac")
      sadjcase <- adjvar
      swithqry <- paste0(swithqry, ", ", sadjcase)

      seedadjjoinqry <- getjoinqry(tuniqueid, cuniqueid, adjalias., salias.)
#      swithfromqry <- paste0(sfromqry,
#                         "\n JOIN adjfactors adj ", seedadjjoinqry)
      swithfromqry <- sfromqry
    }  

    ## WHERE statement - Woodland
    swithwhereqry <- swhereqry
    if (woodland %in% c("N", "only")) {
      swithfromqry <- paste(swithfromqry, wsfromqry)
      if (is.null(swithwhereqry)) {
        swithwhereqry <- paste0("\n WHERE ", wswhereqry)
      } else {
        swithwhereqry <- paste0(swithwhereqry, " AND ", wswhereqry)
      }
    }
 
    ## Build final seedling WITH query
    twithqry <- paste0(swithqry,
                       swithfromqry,
                       swithwhereqry)
  }

  ## Append to pltidsWITHqry
  if (!is.null(pltidsWITHqry)) {
    pltidsWITHqry <- paste0(pltidsWITHqry, ", ",
                       "\n----- get tree data",
                       "\ntdat AS",
                       "\n(", twithqry, ")")
  } else {
    pltidsWITHqry <- paste0("WITH tdat AS", 
                       "\n(", twithqry, ")")
  }
  
  ## Build SELECT statement for summarizing tree data.
  ################################################################
  if (!seedonly) {
    
    ## Build select tree query
    tselectqry <- paste0("\nSELECT ", toString(tsumuniqueid), 
                         ",   ", paste(tsumvardf$SELECT[tsumvardf$TABLE == "TREE"], 
                                       collapse=",  ")) 
    if (addseed) {
      ## Append to select tree query 
      tselectqry <- paste0(tselectqry, ", ", 
                         paste(tsumvardf$SELECT[tsumvardf$TABLE == "SEED"])) 
      tselectqry <- paste0(tselectqry, ", ", 
                         paste(tsumvardf$SELECT[tsumvardf$TABLE == "TREESEED"])) 
    }
  } else {
    
    ## Build select
    tselectqry <- paste0("\nSELECT ", toString(tsumuniqueid), 
                         ",   ", paste(tsumvardf$SELECT[tsumvardf$TABLE == "SEED"], 
                                       collapse=",\n  "))  
    
  }

  ## Build query to summarize tree data
  ################################################################
  tqry <- paste0(tselectqry,
                 "\nFROM ", twithalias,
                 "\nGROUP BY ", toString(tsumuniqueid))	
    
    
  ## Build final query to summarize tree data including WITH queries
  ################################################################
  tree.qry <- paste(pltidsWITHqry, 
                    "\n-------------------------------------------",
                    tqry)						
  #message(tree.qry)
    
  if (datindb) {
    treedat <- tryCatch(
                    {
                      DBI::dbGetQuery(dbconn, tree.qry)
                    },
                    error = function(e) {
                      warning(e)
                      return(NULL)
                    }
                )
  } else {
	  treedat <- tryCatch(
                   {
                     sqldf::sqldf(tree.qry)
                   },
                   error = function(e) {
                     warning(e)
                     return(NULL)
                   }
                )
  }

  setkeyv(setDT(treedat), tuniqueid)
  
 
 
  ######################################################################## 
  ######################################################################## 
 
  ## Merge to cond or plot
  ###################################
  if (bycond && !is.null(condx) && is.data.frame(condx)) {
    ## Check if class of tuniqueid matches class of cuniqueid
    tabs <- check.matchclass(treedat, condx, tjoinid, cjoinid)
    treedat <- tabs$tab1
    condx <- tabs$tab2

    ## Merge to condx
    sumdat <- merge(condx, treedat, by.x=c(cuniqueid, condid),
		by.y=c(tuniqueid, condid), all.x=TRUE)
		
  } else if (!noplt) {
    if ("sf" %in% class(pltx)) {
      pltsp <- TRUE
    }
  
    ## Check if class of tuniqueid matches class of puniqueid
    tabs <- check.matchclass(treedat, pltx, tjoinid, puniqueid)
    treedat <- tabs$tab1
    pltx <- tabs$tab2
    
	## Merge to pltx
    sumdat <- merge(pltx, treedat, by.x=puniqueid, by.y=tuniqueid, all.x=TRUE)
	
  } else if (bysubp && !is.null(subplotx) && is.data.frame(subplotx)) {
    sumdat <- merge(subplotx, datvars, by=tsumuniqueid, all.x=TRUE)
	
  } else {
    sumdat <- treedat
  }

  ## Change NA values TO 0
  if (NAto0) {
    for (col in tsumvardf$NAME) set(sumdat, which(is.na(sumdat[[col]])), col, 0) 
  }


  ## Get metadata
  #############################################################  
  sumdatcols <- names(sumdat)
 
  if (bycond) {  
    meta <- ref_cond[ref_cond$VARIABLE %in% sumdatcols, ]
    missnames <- names(sumdat)[!names(sumdat) %in% meta$VARIABLE]
    meta2 <- FIESTA::ref_plt[FIESTA::ref_plt$VARIABLE %in% missnames, ]
    if (nrow(meta2) > 0) {
      meta <- rbind(meta, meta2)
    } 
  } else {
    meta <- ref_plt[names(sumdat) %in% ref_plt$VARIABLE, ]
  }

  metanames <- names(sumdat)[which(names(sumdat) %in% meta$VARIABLE)]
  meta <- meta[meta$VARIABLE %in% metanames, ]
  meta <- meta[match(metanames, meta$VARIABLE),]

  if (!is.null(tsumvarlst) && nrow(meta) > 0) {
    tree_ref <- FIESTAutils::ref_tree[match(tsumvarlst, FIESTAutils::ref_tree$VARIABLE),]
    tree_ref$VARIABLE[tree_ref$VARIABLE == "TPA_UNADJ"] <- "COUNT"

    if (nrow(tree_ref) > 0) {
      if (TPA) {
        tree_ref$VARIABLE <- paste0(tree_ref$VARIABLE, "_TPA")
      } 
      if (!is.null(fname)) {
        tree_ref$VARIABLE <- paste0(tree_ref$VARIABLE, "_", fname)
        tree_ref$DESCRIPTION <- paste0(tree_ref$DESCRIPTION, " (", tfilter, ")")
      }
      if (adjtree) {
        tree_ref$VARIABLE <- paste0(tree_ref$VARIABLE, "_ADJ")
        tree_ref$DESCRIPTION <- paste(tree_ref$DESCRIPTION, "- adjusted for partial nonresponse at plot-level")
      }

    ## Check for biomass and/or carbon variables to add units
      if (any(tsumvarlst %in% c(biovars, carbvars))) {
        bcvars <- tsumvarlst[tsumvarlst %in% c(biovars, carbvars)]
        refbcvars <- unlist(lapply(bcvars, function(x) tree_ref$VARIABLE[grepl(x, tree_ref$VARIABLE)]))

        unittxt <- ifelse(lbs2tons, "tons", "lbs")
        tree_ref[tree_ref$VARIABLE %in% refbcvars, "DESCRIPTION"] <- 
			           paste0(tree_ref[tree_ref$VARIABLE %in% refbcvars, "DESCRIPTION"], " (in ", unittxt, ")")    
        tree_ref[tree_ref$VARIABLE %in% refbcvars, "VARIABLE"] <- 
			           paste0(tree_ref[tree_ref$VARIABLE %in% refbcvars, "VARIABLE"], "_", toupper(unittxt))
      } 
   
      meta <- rbind(meta, tree_ref)
    }
  }


  #### WRITE TO FILE 
  #############################################################
  if (savedata) {
    message("saving ", out_layer, "...")
    
    if (pltsp) {
      spExportSpatial(sumdat, 
                      savedata_opts = outlst)
    } else {
      datExportData(sumdat, 
                    savedata_opts = outlst) 
      
      outlst$out_layer <- "meta"
      message("saving meta...")
      datExportData(meta, 
                    savedata_opts = outlst) 

    }
  } 

  if (!returnDT) {     
    sumdat <- setDF(sumdat)
  }
  sumtreelst <- list(treedat=sumdat, sumvars=tsumvardf$NAME, treeqry=tree.qry)
  #sumtreelst$estunits <- estunits
  if (!is.null(tfilter)) {
    sumtreelst$tfilter <- tfilter
  }
  if (!is.null(meta) && nrow(meta) > 0) {
    sumtreelst$meta <- meta
  }
  return(sumtreelst)
} 
