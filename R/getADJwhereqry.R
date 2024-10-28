
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
    cstatus.filter <- paste0(conda., cstatusnm, " <> 5")
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
  
  ## Description:
  ## Checks nonresponse filters for subplots
  subpa. <- "subp."
  
  ## Filter for nonsampled subplots in subplot
  ## (SUBP_STATUS_CD <> 3)
  subpstatusnm <- findnm("SUBP_STATUS_CD", subplotflds, returnNULL = TRUE)
  if (is.null(subpstatusnm)) {
    message("SUBP_STATUS_CD is not in dataset... assuming all subplots are sampled")
  } else {
    ## Build where query to remove subplots that wasn't sampled
    subpstatus.filter <- paste0(subpa., subpstatusnm, " <> 3")
    if (is.null(adjwhereqry)) {
      adjwhereqry <- paste0("\n WHERE ", subpstatus.filter)
    } else {
      adjwhereqry <- paste0(adjwhereqry, 
                            "\n    AND ", subpstatus.filter)
    }	
  }
  
  ## If ACI, filter for nonsampled nonforest subplots in subplot
  ## (NF_SUBP_STATUS_CD is NULL or NF_SUBP_STATUS_CD <> 3)
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
  
  return(adjwhereqry)
}
