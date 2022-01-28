## ----setup, include = F-------------------------------------------------------
library(knitr)
knitr::opts_chunk$set(message = F, warning = F)

## ---- include=FALSE-----------------------------------------------------------
# Sets up output folding
hooks = knitr::knit_hooks$get()
hook_foldable = function(type) {
  force(type)
  function(x, options) {
    res = hooks[[type]](x, options)
    
    if (isFALSE(options[[paste0("fold.", type)]])) return(res)
    
    paste0(
      "<details><summary>", type, "</summary>\n\n",
      res,
      "\n\n</details>"
    )
  }
}
knitr::knit_hooks$set(
  output = hook_foldable("output"),
  plot = hook_foldable("plot")
)

## ---- results = 'asis', echo=FALSE--------------------------------------------

#stratdat.lut <- read.csv("C:/_tsf/_GitHub/meta/stratdat_variables.csv", stringsAsFactors=FALSE)
#source("C:/_tsf/_GitHub/meta/undataframe.R")
#stratdat.lut <- undataframe(stratdat.lut)

stratdat.lut <- data.frame(
Variable = c("ESTN_UNIT", "STRATUMCD", "P1POINTCNT", "P2POINTCNT", "n.strata", "n.total", "strwt", "CONDPROP_UNADJ_SUM", "cadjfac", "ACRES", "expfac", "EXPNS"), 
Description = c("Estimation unit", "Strata value", "Number of pixels by strata and estimation unit", "Number of P2 plots in population data", "Number of sampled plots in strata", "Number of sampled plots for estimation unit", "Proportion of pixels in strata (strata weight)", "Summed condition proportion in strata", "Adjustment factor for nonsampled plots in strata (CONDPROP_UNADJ_SUM/n.strata)", "Total acres for estimation unit", "Expansion factor, in acres, area in strata divided by number of sampled plots", "Expanded area, in acres, expfac multiplied by strwt"), 
    stringsAsFactors = FALSE)


kable(stratdat.lut,
  format = "pandoc",   # default
  caption = "Description of variables in stratdat.",
  col.names = names(stratdat.lut),
  row.names = FALSE,
  align = c("l"),       # align = c("c", "c", "c", "r")
  # padding = 2         # inner spacing
) 


## ---- results = 'asis', echo=FALSE--------------------------------------------

#required.lut <- read.table("C:/_tsf/_GitHub/meta/required_variables.txt", header=TRUE, sep="\t")

required.lut <- data.frame(
  Table = c("tree", "", "cond", "", "", "", "", "", "", "", "", "plot", "", ""), 
  Variable = c("PLT_CN", "TPA_UNADJ", "PLT_CN", "CONDPROP_UNADJ", 
               "COND_STATUS_CD", "NF_COND_STATUS_CD", "SITECLCD", "RESERVCD", "SUBPROP_UNADJ",
               "MICRPROP_UNADJ", "MACRPROP_UNADJ", "CN", "STATECD", "INVYR"), 
  Description = c("popTableIDs - Unique identifier for each plot, for joining tables (e.g. PLT_CN)", 
    "Number of trees per acre each sample tree represents (e.g. DESIGNCD=1: TPA_UNADJ=6.018046 for trees on subplot; 74.965282 for trees on microplot)", "popTableIDs - Unique identifier for each plot, for joining tables (e.g., PLT_CN)", 
    "Unadjusted proportion of condition on each plot. Optional if only 1 condition (record) per plot", 
    "Status of each forested condition on plot (i.e. accessible forest, nonforest, water, etc.)", 
    "Only if ACI=TRUE. Status of each nonforest condition plot (i.e. accessible nonforest, nonsampled nonforest)", 
    "Only if landarea=TIMBERLAND. Measure of site productivity", 
    "If landarea=TIMBERLAND. Reserved status", 
    "Unadjusted proportion of subplot conditions on each plot. Optional if only 1 condition (record) per plot", 
    "If microplot tree attributes. Unadjusted proportion of microplot conditions on each plot. Optional if only 1 condition (record) per plot", 
    "If macroplot tree attributes. Unadjusted proportion of macroplot conditions on each plot. Optional if only 1 condition (record) per plot", 
    "popTableIDs - Unique identifier for each plot, for joining tables (e.g. CN)", 
    "Identifies state each plot is located in. Optional if only 1 state", 
    "Identifies inventory year of each plot. Optional. Assumes estimation time span is less than inventory cycle"), 
    stringsAsFactors = FALSE)

kable(required.lut,
  format = "pandoc",   # default
  # caption = "Title of the table",
  col.names = names(required.lut),
  row.names = FALSE,
  align = c("l"),       # align = c("c", "c", "c", "r")
  # padding = 2         # inner spacing
)

