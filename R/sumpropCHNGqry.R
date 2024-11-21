sumpropCHNGqry <- function(fromqry = NULL,
                           whereqry = NULL,
                           selectvars = NULL,
                           ACI = FALSE,
                           frompltcondx = FALSE,
                           SCHEMA. = "",
                           suffix = "") {
					 
  ## DESCRIPTION: Summarize sampled subplot condition proportions.
  
  ############################################################
  ## Assemble FROM statement
  ############################################################
  conda. = ifelse(frompltcondx, "pc.", "c.")

  if (!frompltcondx && is.null(fromqry)) {
    
    ## PLOT/COND from query
    pcfromqry <- paste0(
      "\n FROM ", SCHEMA., "PLOT", suffix, " p",
      "\n JOIN ", SCHEMA., "PLOT", suffix, " pplot ON (pplot.CN = p.CN)",
      "\n JOIN ", SCHEMA., "COND", suffix, " c ON (c.PLT_CN = c.CN)",
      "\n JOIN ", SCHEMA., "COND", suffix, " pcond ON (pcond.PLT_CN = pplot.PREV_PLT_CN)")
    
    ## CHANGE from query		 
    fromqry <- paste0(pcfromqry,  
      "\n JOIN ", SCHEMA., "SUBP_COND_CHNG_MTRX sccm ON(sccm.PLT_CN = c.PLT_CN)", 
      "\n   AND sccma.PREV_PLT_CN = pcond.PLT_CN",
      "\n   AND sccma.PLT_CN = c.PLT_CN", 
      "\n   AND sccma.PREV_PLT_CN = pcond.PLT_CN") 
    
  }

  ############################################################
  ## Assemble WHERE statement
  ############################################################
  if (!frompltcondx && is.null(whereqry)) {

    ## Remove nonsampled conditions
    whereqry <- "\n WHERE c.COND_STATUS_CD <> 5"
    if (ACI) {
      whereqry <- paste0(whereqry, 
            "\n    AND (c.NF_COND_STATUS_CD IS NULL OR NF_COND_STATUS_CD != 5)")
    }
  
    ## Other filters
    whereqry <- paste0(whereqry,
	         "\n   AND c.COND_PROP_UNADJ IS NOT NULL",
	         "\n   AND ((sccma.SUBTYP = 3 AND c.PROP_BASIS = 'MACR')",
	         "\n         OR (sccma.SUBTYP = 1 AND c.PROP_BASIS = 'SUBP'))",
	         "\n   AND COALESCE(c.COND_NONSAMPLE_REASN_CD, 0) = 0",  
	         "\n   AND COALESCE(pcond.COND_NONSAMPLE_REASN_CD, 0) = 0")  
  }


  ## Sum condition proportions by subplot
  ############################################################
  if (!frompltcondx) { 
    selectgrpvars <- "c.PLT_CN, pcond.PLT_CN AS PREV_PLT_CN, pcond.CONDID AS PREVCOND, c.CONDID"
    grpvars <- "c.PLT_CN, pcond.PLT_CN, pcond.CONDID, c.CONDID"
    if (!is.null(selectvars)) {
      selectgrpvars <- paste0(selectvars, ", ",
                              selectgrpvars)
      grpvars <- paste0(selectvars, ", ",
                        grpvars)
    }
  } else {
    selectgrpvars <- "pc.PREV_PLT_CN, pc.PLT_CN, pc.PREV_CONDID, pc.CONDID"
    if (!is.null(selectvars)) {
      selectgrpvars <- paste0(selectvars, ", ",
                              selectgrpvars)
    }
    grpvars <- selectgrpvars
  }
  selectqry <- paste0("SELECT ", selectgrpvars, ", 
      SUM(sccm.SUBPTYP_PROP_CHNG * 
              (CASE WHEN ((sccm.SUBPTYP = 3 AND ", conda., "PROP_BASIS = 'MACR') OR
			         (sccm.SUBPTYP = 1 AND ", conda., "PROP_BASIS = 'SUBP'))
			         THEN 1 ELSE 0 END) /4) AS CONDPROP_UNADJ,
      SUM(sccm.SUBPTYP_PROP_CHNG * 
              (CASE WHEN sccm.SUBPTYP = 1 THEN 1 ELSE 0 END) /4) AS SUBPPROP_UNADJ,
      SUM(sccm.SUBPTYP_PROP_CHNG * 
              (CASE WHEN sccm.SUBPTYP = 2 THEN 1 ELSE 0 END) /4) AS MICRPROP_UNADJ,
      SUM(sccm.SUBPTYP_PROP_CHNG * 
              (CASE WHEN sccm.SUBPTYP = 3 THEN 1 ELSE 0 END) /4) AS MACRPROP_UNADJ")
  
  sumpropqry <- paste0(
    selectqry,
    fromqry,  
    whereqry, 
    "\n GROUP BY ", grpvars)
  #message(sumpropqry)
  
  return(sumpropqry)
}



  