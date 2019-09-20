## DBgetvars
## DBgetfn			Gets file name for Plot and Strata files.
## clip.othertables 


DBgetvars <- function(invtype, defaultVars, istree, isseed, isveg, isdwm, 
	regionVars, isRMRS, FS_FIADB, NIMS_UNIT, datsource, dbconn) {
  ########################################################################
  ##  DEFINE VARIABLES
  ########################################################################
 
  # Including board-foot volumes
  #volvars <- c("VOLCFNET", "VOLCFGRS", "VOLCSNET", "VOLCSGRS", "VOLBFNET", "VOLBFGRS")
  #growvars <- c("GROWCFGS", "GROWBFSL", "GROWCFAL", "FGROWCFGS", "FGROWBFSL", "FGROWCFAL")
  #mortvars <- c("MORTCFGS", "MORTBFSL", "MORTCFAL", "FMORTCFGS", "FMORTBFSL", "FMORTCFAL")
  #remvars <- c("REMVCFGS", "REMVBFSL", "REMVCFAL", "FREMVCFGS", "FREMVBFSL", "FREMVCFAL")
  #tpavars <- c("TPA_UNADJ", "TPAGROW_UNADJ", "TPAMORT_UNADJ", "TPAREMV_UNADJ")

  # Excluding board-foot volumes
  volvars <- c("VOLCFNET", "VOLCFGRS", "VOLBFNET", "VOLBFGRS", "VOLCSNET", "VOLCSGRS")
  growvars <- c("GROWCFGS", "GROWCFAL", "FGROWCFGS", "FGROWCFAL")
  mortvars <- c("MORTCFGS", "MORTCFAL", "FMORTCFGS", "FMORTCFAL")
  remvars <- c("REMVCFGS", "REMVCFAL", "FREMVCFGS", "FREMVCFAL")
  tpavars <- c("TPA_UNADJ", "TPAGROW_UNADJ", "TPAMORT_UNADJ", "TPAREMV_UNADJ")
  biovars <- c("DRYBIO_BOLE", "DRYBIO_STUMP", "DRYBIO_TOP", "DRYBIO_SAPLING", 
		"DRYBIO_WDLD_SPP", "DRYBIO_BG", "DRYBIO_AG")
  carbonvars <- c("CARBON_BG", "CARBON_AG")

  ## TREE summary variables
  tsumvarlst <- c(volvars, growvars, mortvars, remvars, tpavars, biovars, carbonvars)
  #tsumvarlst <- c(volvars, growvars, mortvars, remvars, biovars, carbonvars)    

  ## SEED summary variables
  ssumvarlst <- c("TREECOUNT", "TREECOUNT_CALC")


  filterpvarlst <- {}
  filterpvarflst <- {}
  filtercvarlst <- {}
  filtercvarflst <- {}
  filterpvar10lst <- {}
  filtercvar10lst <- {}

  if (defaultVars) {
    ################################  PLOT VARIABLES ##################################
    ## Variables from FIADB.PLOT 
    pvarFIADBlst <- c("CN", "PREV_PLT_CN", "INVYR", "STATECD", "CYCLE", "SUBCYCLE", 
	"UNITCD", "COUNTYCD", "PLOT", "PLOT_STATUS_CD", "PLOT_NONSAMPLE_REASN_CD", 
	"SAMP_METHOD_CD", "SUBP_EXAMINE_CD", "MANUAL", "MACRO_BREAKPOINT_DIA", 
	"INTENSITY", "MEASYEAR", "MEASMON", "MEASDAY", "REMPER", "KINDCD", "DESIGNCD", 
	"RDDISTCD", "WATERCD", "LON", "LAT", "ELEV", "GROW_TYP_CD", "MORT_TYP_CD", 
	"P2PANEL", "P3PANEL", "SUBPANEL", "ECOSUBCD", 
	"NF_PLOT_STATUS_CD", "NF_PLOT_NONSAMPLE_REASN_CD", "NF_SAMPLING_STATUS_CD",
 	"P2VEG_SAMPLING_STATUS_CD", "QA_STATUS")

    if (isRMRS && regionVars)
      pvarFIADBlst <- c(pvarFIADBlst, "COLOCATED_CD_RMRS", "CONDCHNGCD_RMRS", 
		"FUTFORCD_RMRS", "MANUAL_RMRS", "PREV_PLOT_STATUS_CD_RMRS")


    ################################  COND VARIABLES  ##################################
    ## Variables from FS_NIMS_FIADB_RMRS.COND
    cvarFIADBlst <- c("PLT_CN", "CONDID", "COND_STATUS_CD", "COND_NONSAMPLE_REASN_CD", 
	"RESERVCD", "OWNCD", "OWNGRPCD", "ADFORCD", "FORTYPCD", "FLDTYPCD", "MAPDEN", 
	"STDAGE", "STDSZCD", "FLDSZCD", "SITECLCD", "SICOND", "SIBASE", "SISP", 
	"STDORGCD", "STDORGSP", "CONDPROP_UNADJ", "MICRPROP_UNADJ", "SUBPPROP_UNADJ", 
	"MACRPROP_UNADJ", "SLOPE", "ASPECT", "GSSTKCD", "ALSTKCD", "DSTRBCD1", "DSTRBYR1", 
	"DSTRBCD2", "DSTRBYR2", "DSTRBCD3", "DSTRBYR3", "TRTCD1", "TRTYR1", "TRTCD2", 
	"TRTYR2", "TRTCD3", "TRTYR3", "PRESNFCD", "BALIVE", "FLDAGE", "FORTYPCDCALC", 
	"HABTYPCD1", "HABTYPCD2", "LIVE_CANOPY_CVR_PCT", "LIVE_MISSING_CANOPY_CVR_PCT", 
	"CANOPY_CVR_SAMPLE_METHOD_CD", "CARBON_DOWN_DEAD", "CARBON_LITTER", 
	"CARBON_SOIL_ORG", "CARBON_STANDING_DEAD", "CARBON_UNDERSTORY_AG",
	"CARBON_UNDERSTORY_BG", "NF_COND_STATUS_CD", "NF_COND_NONSAMPLE_REASN_CD",
	"LAND_COVER_CLASS_CD") 
	
    if (isRMRS && regionVars)
      cvarFIADBlst <- c(cvarFIADBlst, "LAND_USECD_RMRS", "PCTBARE_RMRS", 
		"CRCOVPCT_RMRS", "COND_STATUS_CHNG_CD_RMRS", "QMD_RMRS", 
		"RANGETYPCD_RMRS", "SDIMAX_RMRS", "SDIPCT_RMRS", "SDI_RMRS") 
    

    if (istree) {
      ################################  TREE VARIABLES  ##################################
      ### Variables from FS_NIMS_FIADB_RMRS.TREE (these variables change from NIMSf to FIADB)
      tvarFIADBlst <- c("CN", "PLT_CN", "PREV_TRE_CN", "SUBP", "TREE", "CONDID", 
		"AZIMUTH", "DIST", "STATUSCD", "SPCD", "SPGRPCD", "DIA", "HT", "ACTUALHT", 
		"HTCD", "TREECLCD", "CR", "CCLCD", "AGENTCD", "CULL", "DECAYCD", "STOCKING", 
		"WDLDSTEM", "MORTYR", "UNCRCD", "BHAGE", "TOTAGE", "MORTCD", "MIST_CL_CD", 
		"STANDING_DEAD_CD", "PREV_STATUS_CD", "PREV_WDLDSTEM", "RECONCILECD", "PREVDIA")

      ### Tree sum variables list
      tsumvarFIADBlst <- tsumvarlst

    if (isRMRS && regionVars)
        tvarFIADBlst <- c(tvarFIADBlst, "TREECLCD_RMRS", "DAMAGE_AGENT_CD1",
		"DAMAGE_AGENT_CD2", "DAMAGE_AGENT_CD3", "RADGRW_RMRS", "DIA_1YRAGO_RMRS", 
		"HT_1YRAGO_RMRS", "PREV_ACTUALHT_RMRS", "PREV_TREECLCD_RMRS")
      
    }

    if (isseed) 
      ################################  SEED VARIABLES  ##################################
      ### Variables from FS_NIMS_RMRS.SEEDLING
      svarFIADBlst <- c("PLT_CN", "SUBP", "CONDID", "SPCD", "SPGRPCD", "TREECOUNT_CALC", 
		"TOTAGE", "TPA_UNADJ")
    

  } else {

    ## Define unique identifiers
    puniqueid <- "CN"
    cuniqueid <- c("PLT_CN", "CONDID")
    tuniqueid <- c("PLT_CN", "CONDID", "SUBP", "TREE")
    suniqueid <- c("PLT_CN", "CONDID", "SUBP")
 
    ## Define variables to delete and variables to keep          
    FIADBvar2delete <- c("STATECD", "COUNTYCD", "PLOT", "UNITCD", "INVYR", "CYCLE", 
	"SUBCYCLE", "CN")
    FIADBcvar2keep <- c(cuniqueid, "CONDPROP_UNADJ", "MICRPROP_UNADJ", "MACRPROP_UNADJ", 
	"COND_STATUS_CD")
    FIADBpvar2keep <- c(puniqueid, "STATECD", "INVYR")
    FIADBtvar2keep <- tuniqueid
    FIADBsvar2keep <- suniqueid

    vardelete <- c("STATECD", "COUNTYCD", "PLOT", "UNITCD", "INVYR", "CN", "CYCLE", 
	"SUBCYCLE")
  }

  ############################  UNDERSTORY VEG VARIABLES  ############################
  if (isveg) {    ## FS_NIMS_FIADB_RMRS or FS_FIADB

    ### Variables from P2VEG_SUBPLOT_SPP
    vspsppvarFIADBlst <- c("PLT_CN", "SUBP", "CONDID", "VEG_FLDSPCD", "UNIQUE_SP_NBR", 
	"VEG_SPCD", "GROWTH_HABIT_CD", "LAYER", "COVER_PCT")

    ### Variables from P2VEG_SUBP_STRUCTURE
    vspstrvarFIADBlst <- c("PLT_CN", "SUBP", "CONDID", "GROWTH_HABIT_CD", "LAYER", 
	"COVER_PCT")
  }

  ##############################  DOWN WOODY MATERIAL  ###############################
  isdwm.fhm <- FALSE
  if (isdwm.fhm) {
    ### Variables from FS_FIADB.DWM_COARSE_WOODY_DEBRIS
    cwdvarFIADBlst <- c("PLT_CN", "SUBP", "CONDID", "TRANSECT", "CWDID", "SLOPDIST", 
	"HORIZ_DIST", "SPCD", "DECAYCD", "TRANSDIA", "SMALLDIA", "LARGEDIA", "LENGTH", "HOLLOWCD", 
	"CWDHSTCD", "VOLCF", "DRYBIO", "CARBON", "COVER_PCT", "LPA_UNADJ", "LPA_PLOT", "LPA_COND",
	"LPA_UNADJ_RGN", "LPA_PLOT_RGN", "LPA_COND_RGN", "VOLCF_AC_UNADJ", "VOLCF_AC_PLOT",
	"VOLCF_AC_COND", "DRYBIO_AC_UNADJ", "DRYBIO_AC_PLOT", "DRYBIO_AC_COND", "CARBON_AC_UNADJ",
	"CARBON_AC_PLOT", "CARBON_AC_COND")
    #fhmcwdvars <- addcommas(cwdvarFIADBlst, "dN")

    ### Variables from FS_FIADB.DUFF_LITTER_FUEL
    dlvarFIADBlst <- c("PLT_CN", "SUBP", "CONDID", "TRANSECT", "SMPLOCCD", "DUFFDEP",
	"LITTDEP", "FUELDEP", "DLF_SAMPLE_METHOD", "DUFF_METHOD", "DUFF_NONSAMPLE_REASN_CD",
	"LITTER_METHOD", "LITTER_NONSAMPLE_REASN_CD", "FUELBED_METHOD", 
	"FUELBED_NONSAMPLE_REASN_CD")
    #fhmdlvars <- addcommas(dlvarFIADBlst, "dN")

    ### Variables from FS_FIADB.FINE_WOODY_DEBRIS
    fwdvarlst <- c("PLT_CN", "SUBP", "CONDID", "TRANSECT", "SMALLCT", "MEDIUMCT", 
	"LARGECT", "RSNCTCD", "PILESCD", "SMALL_TL_COND", "SMALL_TL_PLOT", "SMALL_TL_UNADJ", 
	"MEDIUM_TL_COND", "MEDIUM_TL_PLOT", "MEDIUM_TL_UNADJ", "LARGE_TL_COND", "LARGE_TL_PLOT",
	"LARGE_TL_UNADJ", "FWD_STATUS_CD", "FWD_NONSAMPLE_REASN_CD", "FWD_SAMPLE_METHOD", "SLOPE")
    #fhmfwdvars <- addcommas(fwdvarFIADBlst, "dN")

    ### Variables from FS_FIADB.MICROPLOT_FUEL
    micvarFIADBlst <- c("PLT_CN", "SUBP", "LVSHRBCD", "DSHRBCD", "LVHRBCD", "DHRBCD", 
	"LITTERCD", "LVSHRBHT", "DSHRBHT", "LVHRBHT", "DHRBHT", "MICR_SAMPLE_METHOD")
    #fhmmicvars <- addcommas(micvarFIADBlst, "dN")
  }

  if (isdwm) {
    ### Variables from COND_DWM_CALC
    dwmlst <- c("PLT_CN", "CONDID", "CONDPROP_CWD", "CONDPROP_FWD_SM", "CONDPROP_FWD_MD", 
	"CONDPROP_FWD_LG", "CONDPROP_DUFF", "CWD_TL_COND", "CWD_TL_UNADJ", "CWD_TL_ADJ", 
	"CWD_LPA_COND", "CWD_LPA_UNADJ", "CWD_LPA_ADJ", "CWD_VOLCF_COND", "CWD_VOLCF_UNADJ", 
	"CWD_VOLCF_ADJ", "CWD_DRYBIO_COND", "CWD_DRYBIO_UNADJ", "CWD_DRYBIO_ADJ", 
	"CWD_CARBON_COND", "CWD_CARBON_UNADJ", "CWD_CARBON_ADJ", "FWD_SM_TL_COND", 
	"FWD_SM_TL_UNADJ", "FWD_SM_TL_ADJ", "FWD_SM_CNT_COND", "FWD_SM_VOLCF_COND", 
	"FWD_SM_VOLCF_UNADJ", "FWD_SM_VOLCF_ADJ", "FWD_SM_DRYBIO_COND", "FWD_SM_DRYBIO_UNADJ", 
	"FWD_SM_DRYBIO_ADJ", "FWD_SM_CARBON_COND", "FWD_SM_CARBON_UNADJ", "FWD_SM_CARBON_ADJ", 
	"FWD_MD_TL_COND", "FWD_MD_TL_UNADJ", "FWD_MD_TL_ADJ", "FWD_MD_CNT_COND", 
	"FWD_MD_VOLCF_COND", "FWD_MD_VOLCF_UNADJ", "FWD_MD_VOLCF_ADJ", "FWD_MD_DRYBIO_COND", 
	"FWD_MD_DRYBIO_UNADJ", "FWD_MD_DRYBIO_ADJ", "FWD_MD_CARBON_COND", "FWD_MD_CARBON_UNADJ", 
	"FWD_MD_CARBON_ADJ", "FWD_LG_TL_COND", "FWD_LG_TL_UNADJ", "FWD_LG_TL_ADJ", 
	"FWD_LG_CNT_COND", "FWD_LG_VOLCF_COND", "FWD_LG_VOLCF_UNADJ", "FWD_LG_VOLCF_ADJ", 
	"FWD_LG_DRYBIO_COND", "FWD_LG_DRYBIO_UNADJ", "FWD_LG_DRYBIO_ADJ", "FWD_LG_CARBON_COND", 
	"FWD_LG_CARBON_UNADJ", "FWD_LG_CARBON_ADJ", "PILE_SAMPLE_AREA_COND", 
	"PILE_SAMPLE_AREA_UNADJ", "PILE_SAMPLE_AREA_ADJ", "PILE_VOLCF_COND", 
	"PILE_VOLCF_UNADJ", "PILE_VOLCF_ADJ", "PILE_DRYBIO_COND", "PILE_DRYBIO_UNADJ", 
	"PILE_DRYBIO_ADJ", "PILE_CARBON_COND", "PILE_CARBON_UNADJ", "PILE_CARBON_ADJ", 
	"FUEL_DEPTH", "FUEL_BIOMASS", "FUEL_CARBON", "DUFF_DEPTH", "DUFF_BIOMASS", 
	"DUFF_CARBON", "LITTER_DEPTH", "LITTER_BIOMASS", "LITTER_CARBON", "DUFF_TC_COND", 
	"DUFF_TC_UNADJ", "DUFF_TC_ADJ", "AVG_WOOD_DENSITY")
    dvars <- addcommas(dwmlst, "d")
  }

  ## Variables to convert from NA to 0
  ########################################################################
    
  ## SEED table
  seednavars <- c(ssumvarlst, "TOTAGE", "STOCKING")
  treenavars <- c(tsumvarlst, "TOTAGE", "BHAGE", "CULLDEAD", "CULLFORM", "CFSND", 
	"CULLCF", "MIST_CL_CD", "DAMLOC1", "DAMTYP1", "DAMSEV1", "DAMLOC2", "DAMTYP2", 
	"DAMSEV2", "STOCKING", "RADGRW_RMRS")

  #####################################################################################
  #############################      VARIABLE LISTS     ###############################
  #####################################################################################

  ### GET FS_FIADB
  ###########################################################
  if (datsource == "ORACLE") {
    if (!FS_FIADB) {
      SCHEMA <- paste0("FS_NIMS_FIADB_", NIMS_UNIT)	## NIMS FIADB REGIONAL TABLES
    } else {
      SCHEMA <- "FS_FIADB"          	## FIADB NATIONAL TABLES
    }
    SCHEMA. <- paste0(SCHEMA, ".")
  } else {
    SCHEMA <- "" 
    SCHEMA. <- ""
    FIADBa <- NULL
  }

  ##########################################
  ## NO REGION VARIABLES; ANNUAL OR PERIODIC
  ##########################################

  if (defaultVars) {

    #########################################################
    ## DEFINE VARIABLES
    #########################################################

    ## PLOT and COND variables
    pltvarlst <- pvarFIADBlst
    condvarlst <- cvarFIADBlst

    ## PLOT and COND filter list
    filtervarlst <- list(filterpvarlst=pltvarlst, filtercvarlst=condvarlst)

    ## TREE AND SEEDLING VARIABLES
    if (istree) {
      treevarlst <- tvarFIADBlst

      ## FOR TPA CALCULATIONS
      tsumvarTPAlst <- tsumvarFIADBlst
    } 
    if (isseed)
      seedvarlst <- svarFIADBlst

  } else {

    #########################################################
    ## SELECT VARIABLES
    #########################################################

    ## PLOT variables
    #####################################
    pvarkeep <- FIADBpvar2keep
    FIADBpltvarlst <- FIESTA::selectvars(dbconn, schema=SCHEMA, 
			tabnm="PLOT", 
			varkeep=pvarkeep, titletab="plot")
    pltvarlst <- FIADBpltvarlst$vars
    pfiltvarlst <- FIADBpltvarlst$filtervarlst
    filterpvarlst <- c(pvarkeep, pfiltvarlst)

    ## COND variables
    #####################################
    cvarkeep <- FIADBcvar2keep
    if (!"INVYR" %in% pltvarlst) cvarkeep <- unique(c(cvarkeep, "INVYR"))
    cvardelete <- FIADBvar2delete
    FIADBcondvarlst <- FIESTA::selectvars(dbconn, schema=SCHEMA, 
			tabnm="COND",
              	varkeep=cvarkeep, vardelete=cvardelete, titletab="cond")
    condvarlst <- FIADBcondvarlst$vars
    cfiltvarlst <- FIADBcondvarlst$filtervarlst
    filtercvarlst <- c(cvarkeep, cfiltvarlst)

    ## PLOT and COND filter list
    filtervarlst <- list(filterpvarlst=filterpvarlst, filtercvarlst=condvarlst)
 
    ## TREE variables
    #####################################
    if (istree) {
      tvarkeep <- FIADBtvar2keep
      tvardelete <- FIADBvar2delete
      FIADBtreevarlst <- FIESTA::selectvars(dbconn, schema=SCHEMA, 		
			tabnm="TREE", 
              	varkeep=tvarkeep, vardelete=tvardelete, titletab="tree")
      treevarlst <- FIADBtreevarlst$vars
      tfiltvarlst <- FIADBtreevarlst$filtervarlst
      filtertvarlst <- c(tvarkeep, tfiltvarlst)

      ## FOR TPA CALCULATIONS
      tsumvarTPAlst <- treevarlst[treevarlst %in% tsumvarlst]
      if (length(tsumvarTPAlst >= 1))
        treevarlst <- treevarlst[-which(treevarlst %in% tsumvarTPAlst)]
    }

    ## SEED variables
    #####################################
    if (isseed) {
      svarkeep <- FIADBsvar2keep
      svardelete <- FIADBvar2delete
      FIADBseedvarlst <- FIESTA::selectvars(dbconn, schema=SCHEMA, 
			tabnm="SEEDLING", 
              	varkeep=svarkeep, vardelete=svardelete, titletab="seed")
      seedvarlst <- FIADBseedvarlst$vars
      sfiltvarlst <- FIADBseedvarlst$filtervarlst

      filtersvarlst <- c(svarkeep, sfiltvarlst)
    }
  }

  ## Variable lists
  plotvars <- pltvarlst
  condvars <- condvarlst

  #########################################################
  ## ADD COMMAS
  #########################################################

  ## PLOT and COND variables
  pvars <- addcommas(pltvarlst, "p")
  cvars <- addcommas(condvarlst, "c")
 
  ## Append pvars and cvars
  vars <- pastevars(pvars, cvars)

  ## TREE and SEED and ACTUAL variables
  if (istree) {
    tvars <- addcommas(treevarlst, "t")
    tsumvars <- addcommas(tsumvarlst, "t")
  }
  if (isseed)
    svars <- addcommas(seedvarlst, "s")
  if (isveg) {
    vspsppvars <- addcommas(vspsppvarFIADBlst, "v")
    vspstrvars <- addcommas(vspstrvarFIADBlst, "v")
  }


  varlst <- list(plotvars=plotvars, condvars=condvars, pvars=pvars, cvars=cvars, vars=vars)
  if (istree) {
    varlst$tvars <- tvars
    varlst$tsumvars <- tsumvars
    varlst$tsumvarlst <- tsumvarlst
    varlst$treenavars <- treenavars
  } 
  if (isseed) {
    varlst$svars <- svars
    varlst$seednavars <- seednavars
  }
  if (isveg) {
    varlst$vspsppvars <- vspsppvars
    varlst$vspstrvars <- vspstrvars
  }
  if (isdwm) {
    varlst$dvars <- dvars
#    varlst$dlvars <- dlvars
#    varlst$fwdvars <- fwdvars
#    varlst$tsvars <- tsvars
  }

  varlst$filtervarlst <- filtervarlst

  return(varlst)
}


DBgetfn <- function(tab, invtype, outfn.pre, stabbrlst, evalid=NULL, qry=FALSE, 
	othertxt=NULL, outfn.date=FALSE, addslash=FALSE) {

  invtypenm <- substr(invtype, 1, 3)

  fn <- ifelse(addslash, "/", "")
  if (!is.null(outfn.pre) && outfn.pre != "") fn <- paste0(fn, outfn.pre, "_")
  fn <- paste0(fn, tab, "_", invtypenm, "_", paste(stabbrlst, collapse=""))
  if (!is.null(evalid) && length(evalid) > 0 && length(stabbrlst == 1)) {
    if (length((unique(unlist(evalid)))) == 1) {   
      if (class(evalid) == "list") {
        evalid <- evalid[[1]][1]
      } else {
        evalid <- evalid[1]
      }
      fn <- paste0(fn, "_eval", paste0("20", substr(evalid, nchar(evalid)-3, nchar(evalid)-2)))
    }
  }

  if (!is.null(othertxt)) fn <- paste0(fn, "_", othertxt)
  if (qry) fn <- paste0(fn, "_", "qry")

  if (outfn.date) 
    fn <- paste0(fn, "_", format(Sys.time(), "%Y%m%d"))

  return(fn)
}


clip.othertables <- function(inids, othertabnms, othertabs, uniqueid="PLT_CN", 
	savedata=FALSE, outfn.pre=NULL, outfolder=NULL, outfn.date=FALSE, 
	overwrite=FALSE, gui=FALSE) {

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows")
    Filters=rbind(Filters,csv=c("Comma-delimited files (*.csv)", "*.csv"))

 
  if (is.null(outfn.pre)) outfn.pre <- "clip"

  # GET A LIST AND CHECK THE OTHER TABLES
  if (is.null(othertabnms)) {
    if (gui) {
      othertabnms.resp <- select.list(c("NO", "YES"), title = "Other tables to subset?", 
		    multiple = FALSE)
      if (othertabnms.resp == "") stop("")
      while (othertabnms.resp == "YES") {
        tabresp <- select.list(c("RObject", "CSV"), title = "RObject or CSV?", 
          multiple = FALSE)
        if (tabresp == "RObject") { 
          otabnmlst <- c(ls(pos=1, all.names = TRUE), 
			ls(envir = as.environment("package:FIESTA"), pattern = "WY"))
          otabnm <- select.list(otabnmlst, title = "Other table", multiple = TRUE)
          if (length(otabnm) == 0) stop("")
          for (tabnm in otabnm)
            otablst[[tabnm]] <- get(tabnm) 
        } else if (tabresp == "CSV" && .Platform$OS.type == "windows") {
          otabnm <- choose.files(default = getwd(), caption = "Other table", 
                filters = Filters["csv",], multi = TRUE)
          if (length(otabnm) == 0) stop("")
          for (tabnm in otabnm) {
            nm <- unlist(strsplit(basename(tabnm), "\\.shp"))[1]
            otablst[[nm]] <- read.csv(tabnm, header=TRUE, stringsAsFactors = FALSE)
          }
        }
        othertabnms.resp <- select.list(c("NO", "YES"), title = "Other tables to clip?", 
		      multiple = FALSE)
        if (othertabnms.resp == "") stop("")
      }
    }
  } 
  if (length(othertabnms) > 0 && length(othertabs) == 0) stop("othertabs is invalid")
 
  # Clips tables in othertabs to inids
  if (class(othertabs) != "list") stop("othertabs must be a list")
  if (length(othertabs) > 0) {
    intablst <- list()
    namesintablst <- {}

    for (i in 1:length(othertabs)) {
      otab <- othertabs[[i]]
      otabnm <- othertabnms[i]
      message(paste("Clipping", otabnm, ".."))

      # Set new name of return table
      returnnm <- paste("clip", otabnm, sep="_")
      outnm <- paste(outfn.pre, otabnm, sep="_")
      if (substr(returnnm, nchar(returnnm) - 3, nchar(returnnm)) == ".csv")
        returnnm <- strsplit(returnnm, ".csv")[[1]]
      
      namesintablst <- c(namesintablst, returnnm)
      if (!is.null(otab)) {
        # Check uniqueid of other table.. change if PLT_CN/CN conflict
        otabnmlst <- names(otab)
        if (!uniqueid %in% otabnmlst) {
          if (uniqueid == "PLT_CN" && "CN" %in% otabnmlst) {
            otabid <- "CN"
          } else if (uniqueid == "CN" && "PLT_CN" %in% otabnmlst) {
            otabid <- "PLT_CN"
          } else {
            stop("uniqueid not in", otabnm)
          }
        } else {
          otabid <- uniqueid
        }
        ## SUBSET DATA TABLE 
        if (isS4(otab)) {
          assign(returnnm, otab[otab@data[,otabid] %in% inids, ])
          if (savedata) {
            intabfn <- FIESTA::fileexistsnm(outfolder, returnnm, "shp")
            FIESTA::spExportShape(otab, outfolder=outfolder, outshpnm=outnm,
			overwrite=overwrite, outfn.date=outfn.date)
          }
        } else {
          assign(returnnm, otab[otab[[otabid]] %in% inids, ])
          if (savedata)
            FIESTA::write2csv(otab, outfolder=outfolder, outfilenm=outnm,
			outfn.date=outfn.date, overwrite=overwrite)
        }
        intablst[[returnnm]] <- get(returnnm)

      } else {
          intablst[returnnm] <- otab
      }
    }
  } else {
    intablst <- NULL
  }
  return(intablst)
}



