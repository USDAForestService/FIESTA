
getADJwherePLOT <- function(condflds, ACI = FALSE, conda.="c.", adjwhereqry = NULL) {
  
  ## Description:
  ## Checks nonresponse filters for plots

  ## Filter for nonsampled conditions
  ## (COND_STATUS_CD <> 5)
  cstatusnm <- findnm("COND_STATUS_CD", condflds, returnNULL = TRUE)
  if (is.null(cstatusnm)) {
    message("COND_STATUS_CD is not in dataset... assuming all conditions are sampled")
  } else {
    ## Build where query to remove conditions that were not sampled
    cstatus.filter <- paste0(conda., cstatusnm, " < 5")
    if (is.null(adjwhereqry)) {
      adjwhereqry <- paste0("\n WHERE ", cstatus.filter)
    } else {
      adjwhereqry <- paste0(adjwhereqry, 
                          "\n    AND ", cstatus.filter)
    }	
  }

  ## If ACI, filter for nonsampled nonforest conditions 
  ## (NF_COND_STATUS_CD is NULL or NF_COND_STATUS_CD <> 5)
  if (ACI) {
    cnfstatusnm <- findnm("NF_COND_STATUS_CD", condflds, returnNULL = TRUE)
    if (is.null(cnfstatusnm)) {
      message("NF_COND_STATUS_CD is not in dataset... assuming all nonforest conditions are sampled")
    } else {
      ## Build where query to remove nonforest conditions that were not sampled
      cnfstatus.filter <- paste0("(", conda., cnfstatusnm, " is NULL or ", conda., cnfstatusnm, " <> 5)")
      if (is.null(adjwhereqry)) {
        adjwhereqry <- paste0("\n WHERE ", cnfstatus.filter)
      } else {
        adjwhereqry <- paste0(adjwhereqry, 
                            "\n    AND ", cnfstatus.filter)
      }	
    }
  }
  return(adjwhereqry)
}



getADJwhereSUBP <- function(subplotflds, ACI = FALSE, adjwhereqry = NULL) {
  
  ## Description: Checks nonresponse filters for subplots
  subpa. <- "subp."
  
  ## Filter for nonsampled subplots in subplot
  ## (SUBP_STATUS_CD <> 3)
  subpstatusnm <- findnm("SUBP_STATUS_CD", subplotflds, returnNULL = TRUE)
  if (is.null(subpstatusnm)) {
    message("SUBP_STATUS_CD is not in dataset... assuming all subplots are sampled")
  } else {
    ## Build where query to remove subplots that wasn't sampled
    subpstatus.filter <- paste0(subpa., subpstatusnm, " < 3")
    if (is.null(adjwhereqry)) {
      adjwhereqry <- paste0("\n WHERE ", subpstatus.filter)
    } else {
      adjwhereqry <- paste0(adjwhereqry, 
                            "\n    AND ", subpstatus.filter)
    }	
  }
  
  ## If ACI, filter for nonsampled nonforest subplots in subplot
  ## (NF_SUBP_STATUS_CD is NULL or NF_SUBP_STATUS_CD <> 3)
  if (ACI) {
  subpnfstatusnm <- findnm("SUBP_STATUS_CD", subplotflds, returnNULL = TRUE)
    if (is.null(subpnfstatusnm)) {
      message("NF_SUBP_STATUS_CD is not in dataset... assuming all nonforest subplots are sampled")
    } else {
      ## Build where query to remove nonforest subplots that were not sampled
      subpnfstatus.filter <- paste0("(", subpa., subpnfstatusnm, " IS NULL OR ", subpa., subpnfstatusnm, " <> 3)")
      if (is.null(adjwhereqry)) {
        adjwhereqry <- paste0("\n WHERE ", subpnfstatus.filter)
      } else {
        adjwhereqry <- paste0(adjwhereqry, 
                            "\n    AND ", subpnfstatus.filter)
      }	
    }
  }
  
  return(adjwhereqry)
}



getADJwhereCHNG <- function(condflds, sccmflds, adjwhereqry = NULL) {
  
  ## Description: Checks nonresponse filters for change
  conda. <- "c."
  pconda. <- "pcond."
  sccma. <- "sccm."
  
  ## Change filters 
  #################################################################
  ## c.CONDPROP_UNADJ IS NOT NULL
  ##   AND ((sccm.SUBPTYP = 3 AND c.PROP_BASIS = 'MACR')
  ##     OR (sccm.SUBPTYP = 1 AND c.PROP_BASIS = 'SUBP'))
  ##   AND COALESCE(c.COND_NONSAMPLE_REASN_CD, 0) = 0
  ##   AND COALESCE(pcond.COND_NONSAMPLE_REASN_CD, 0) = 0
  #################################################################
  condpropnm <- findnm("CONDPROP_UNADJ", condflds, returnNULL = TRUE)
  nonsampreasonnm <- findnm("COND_NONSAMPLE_REASN_CD", condflds, returnNULL = TRUE)
  propbasisnm <- findnm("PROP_BASIS", condflds, returnNULL = TRUE)
  subtypnm <- findnm("SUBPTYP", sccmflds, returnNULL = TRUE)
  
  if (any(is.null(condpropnm), is.null(propbasisnm), is.null(subtypnm), 
          is.null(nonsampreasonnm))) {
    message("must include SUBTYP for CHNG estimates")
  } else {
    chg.filter <- paste0(
      conda., condpropnm, " IS NOT NULL",
      "\n   AND ((", sccma., subtypnm, " = 3 AND ", conda., propbasisnm, " = 'MACR')",
      "\n       OR (", sccma., subtypnm, " = 1 AND ", conda., propbasisnm, " = 'SUBP'))",
      "\n   AND COALESCE(", conda., nonsampreasonnm, ", 0) = 0",  
      "\n   AND COALESCE(", pconda., nonsampreasonnm, ", 0) = 0")
    if (is.null(adjwhereqry)) {
      adjwhereqry <- chg.filter
    } else {
      adjwhereqry <- paste0(adjwhereqry, 
                            "\n   AND ", chg.filter)
    }	
  }
  
  return(adjwhereqry)
}



getADJwhereP2VEG <- function(subplotflds, pltflds, adjwhereqry = NULL) {
  
  ## Description: Checks nonresponse filters for P2VEG
  plota. <- "p."
  subpa. <- "subp."

  ## P2 vegetation filters
  #################################################################
  ## P2VEG_SAMPLING_STATUS_CD < 3 AND
  ## ((SAMP_METHOD_CD = 1 AND P2VEG_SAMPLING_STATUS_CD = 1) OR SAMP_METHOD_CD = 2)
  #################################################################
  p2vegstatusnm <- findnm("P2VEG_SUBP_STATUS_CD", subplotflds, returnNULL = TRUE)
  if (is.null(p2vegstatusnm)) {
    message("P2VEG_SUBP_STATUS_CD is not in dataset... assuming all subplots sample P2VEG")
  } else {
    ## Build where query to remove subplots that didn't sample P2VEG
    # p2vegstatus.filter <- paste0(subpa., p2vegstatusnm, " < 3")
    # sampmethodnm <- findnm("SAMP_METHOD_CD", pltflds, returnNULL = TRUE)
    # if (!is.null(sampmethodnm)) {
    #   p2vegstatus.filter <- paste0(p2vegstatus.filter,
    #         "\n    AND ((", plota., sampmethodnm, " = 1 AND ", subpa., p2vegstatusnm, " = 1)",
    #         "\n          OR ", sampmethodnm, " = 2)")
    # }
    sampmethodnm <- findnm("SAMP_METHOD_CD", pltflds, returnNULL = TRUE)
    if (!is.null(sampmethodnm)) {
      p2vegstatus.filter <- paste0("((", plota., sampmethodnm, " = 1 AND ", subpa., p2vegstatusnm, " = 1)",
                                   "\n          OR ", sampmethodnm, " = 2)")
    }
    
    if (is.null(adjwhereqry)) {
      adjwhereqry <- paste0("\n WHERE ", p2vegstatus.filter)
    } else {
      adjwhereqry <- paste0(adjwhereqry, 
                            "\n    AND ", p2vegstatus.filter)
    }	
  }
  
  return(adjwhereqry)
}

