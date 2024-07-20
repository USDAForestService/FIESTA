#' @rdname internal_desc
#' @export
getADJqry <- function(popType,
                      adj,
                      propvars,
							        adjfromqry,
							        pwhereqry,
							        cuniqueid,
							        pltassgnid,
							        strunitvars = NULL,
							        plta. = "",
							        propqry = NULL) {

  ####################################################################################
  ## DESCRIPTION:
  ## Calculates adjustment factors for plots to account for nonsampled conditions.
  ## Creates an adjusted condition proportion by dividing 1 by summed proportions in plot.
  ## NOTE: The following variables must be included in your dataset:
  ##    TPA_UNADJ;
  ##    SUBPPROP_UNADJ (if you have TPA_UNADJ values > 5 and < 10);
  ##    MICRPROP_UNADJ (if you have TPA_UNADJ values > 50);
  ##    MACRPROP_UNADJ (if you have TPA_UNADJ values < 5)
  ##
  ## ARGUMENTS:
  ## popType: population Type
  ## adj: adjustment type ('samp', 'plot', 'none')
  ## propvars: *PROP_UNADJ variables
  ## adjfromqry: plot from query
  ## pwhereqry: plot where query
  ## cuniqueid: unique id of plot in cond table
  ## pltassgnid: unique id of pltassgn table
  ## strunitvars: estimation unit/strata variables
  ## propqry: subquery to use for summarizing condition proportions
  ##
  ## VALUE:
  ##  1. Summed proportions (*PROP_UNADJ_SUM) and adjustment factors (*PROP_ADJFAC) by
  ##     estunit (*PROP_UNADJ_SUM / n.total)
  ##  2. Adjusted condition proportion (CONDPROP_ADJ) appended to condx
  ####################################################################################

  ## 1. Get list of condition proportion variables for calculating adjustments
  ##########################################################################
  propvars <- unique(propvars)

  if (popType == "DWM") {
    adjvarlst <- lapply(propvars, 
		function(x) paste0("ADJ_FACTOR_", sub("CONDPROP_", "", x)))
  } else if (popType == "P2VEG") {
    adjvarlst <- list("ADJ_FACTOR_P2VEG_SUBP")
  } else {
    adjvarlst <- lapply(propvars, 
		function(x) paste0("ADJ_FACTOR_", sub("PROP_UNADJ", "", x)))
  } 
  areawtadj <- adjvarlst[[1]][1]
  popwtadj <- unlist(adjvarlst[!adjvarlst %in% areawtadj])
  
  
  ## 2. Build query to calculate adjustment factors
  ##########################################################################

  ## 4.1 Set different grouping variables depending in adj
  ##########################################################
  if (adj == "plot") {
    grpvars <- toString(paste0(plta., pltassgnid))
  } else {  ## if (adj == "samp")
    grpvars <- toString(paste0(plta., strunitvars))
  }

  ## 4.2 Build select statement with propvars
  ##########################################################
  selectqry <- paste("SELECT", grpvars)   
  
  for (i in 1:length(propvars)) {
    if (adj == "plot") {
      selectqry <- paste0(selectqry, ",", 
		    "\n   COALESCE(1 / NULLIF(SUM(", propvars[[i]], "),0), 0) AS ", 
                           adjvarlst[[i]])
    } else if (adj == "samp") {  
      selectqry <- paste0(selectqry, ",",  
        "\n   COALESCE(COUNT(DISTINCT c.", cuniqueid, ") / NULLIF(SUM(", propvars[[i]], "),0), 0) AS ", 
                           adjvarlst[[i]])
    } else { ## if (adj == "none")
      selectqry <- paste0(selectqry, ",",  
         "\n   1 AS ", adjvarlst[[i]])
    }
  } 
  
  ## 4.5 Build final query to calculate adjustment factors
  ##########################################################
  adjqry <- paste0(selectqry,
                   adjfromqry)
  
  if (!is.null(propqry)) {
    adjjoinqry <- getjoinqry(cuniqueid, pltassgnid, "c.", plta.)
    adjqry <- paste0(adjqry, 
                "\nLEFT JOIN",
                "\n (", propqry, ") c ", adjjoinqry)
  }
  adjqry <- paste0(adjqry, 
                   pwhereqry,
                   "\n GROUP BY ", grpvars)
  

  ## Return query 
  return(adjqry)
}

