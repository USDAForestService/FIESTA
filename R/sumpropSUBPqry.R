sumpropSUBPqry <- function(fromqry = NULL,
                           whereqry = NULL,
                           selectvars = NULL,
                           ACI = FALSE,
                           SCHEMA. = "",
                           suffix = "") {
					 

  ## DESCRIPTION: Summarize sampled subplot condition proportions.
  
  ############################################################
  ## Assemble from statement
  ############################################################

  # if (is.null(fromqry)) {
  #   
  #   ## PLOT/COND from query
  #   pcfromqry <- paste0(
  #     "\n FROM ", SCHEMA., "PLOT", suffix, " p",
  #     "\n JOIN ", SCHEMA., "COND", suffix, " c ON (c.PLT_CN = c.CN)")
  # 
  #   fromqry <- paste0(pcfromqry, 
  #     "\n JOIN ", SCHEMA., "SUBPLOT", suffix, " subp ON (subp.PLT_CN = p.CN)",
  #     "\n  JOIN ", SCHEMA., "SUBP_COND", suffix, " subpc ON (subpc.PLT_CN = c.PLT_CN AND subpc.CONDID = c.CONDID AND subpc.SUBP = subp.SUBP)")
  # }
  # 
  # 
  # ############################################################
  # ## Assemble where statement
  # ############################################################
  # if (is.null(whereqry)) {
  # 
  #   ## Remove nonsampled conditions
  #   whereqry <- "\n WHERE c.COND_STATUS_CD <> 5"
  #   if (ACI) {
  #     whereqry <- paste0(whereqry, 
  #           "\n    AND (c.NF_COND_STATUS_CD IS NULL OR NF_COND_STATUS_CD != 5)")
  #   }
  # 
  #   ## Remove nonsampled subplots
  #   whereqry <- paste0(whereqry, 
  #           "\n    AND subp.SUBP_STATUS_CD <> 3")
  #   if (ACI) {
  #     subpwhereqry <- paste0(whereqry, 
  #           "\n    AND (c.NF_SUBP_STATUS_CD IS NULL OR NF_SUBP_STATUS_CD != 3)")
  #   }
  # 
  # }
  
  ## Sum condition proportions by subplot
  ############################################################
  if (!is.null(selectvars)) {
    selectgrpvars <- paste0(selectvars, ", 
          c.PLT_CN, subp.SUBP, c.CONDID")
    grpvars <- paste0(selectvars, ", 
          c.PLT_CN, subp.SUBP, c.CONDID")
  } else {
    selectgrpvars <- "c.PLT_CN, subp.SUBP, c.CONDID"
    grpvars <- "c.PLT_CN, subp.SUBP, c.CONDID"
  }
  
  
  
  ## Sum condition proportions by subplot
  ############################################################
  selectqry <- paste0("SELECT ", selectgrpvars, ", ",
    "\n    SUM(COALESCE(subpc.SUBPCOND_PROP, 0)) AS SUBPPROP_UNADJ,",
    "\n    SUM(COALESCE(subpc.MICRCOND_PROP, 0)) AS MICRPROP_UNADJ,",
    "\n    SUM(COALESCE(subpc.MACRCOND_PROP, 0)) AS MACRPROP_UNADJ,",
    "\n    SUM(CASE WHEN subpc.MACRCOND_PROP IS NOT NULL 
                THEN subpc.MACRCOND_PROP
                ELSE subpc.SUBPCOND_PROP end) AS CONDPROP_UNADJ")
  
  
  sumpropqry <- paste0(
    selectqry,
    fromqry,  
    whereqry, 
    "\n GROUP BY ", grpvars)
  #message(sumpropqry)
  
  
  return(sumpropqry)
}



  