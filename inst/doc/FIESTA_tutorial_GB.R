## ---- results = 'asis', echo=FALSE---------------------------------------

#required.lut <- read.table("C:/_tsf/_GitHub/meta/required_variables.txt", header=TRUE, sep="\t")

required.lut <- data.frame(
  Table = c("tree", "", "", "cond", "", "", "", "", "", "", "", "", "", "plot", "", ""), 
  Variable = c("*tuniqueid*", "CONDID", "TPA_UNADJ", "*cuniqueid*", "CONDID", "CONDPROP_UNADJ", 
               "COND_STATUS_CD", "NF_COND_STATUS_CD", "SITECLCD", "RESERVCD", "SUBPROP_UNADJ",
               "MICRPROP_UNADJ", "MACRPROP_UNADJ", "*puniqueid*", "STATECD", "INVYR"), 
  Description = c("Unique identifier for each plot, for joining tables (ex. PLT_CN)", 
    "Unique identifier for each condition on plot, for joining tables. Optional if only 1 condition (record)   per plot", 
    "Number of trees per acre each sample tree represents (ex. DESIGNCD=1: TPA_UNADJ=6.018046 for trees on subplot; 74.965282 for trees on microplot)", "Unique identifier for each plot, for joining tables (ex. PLT_CN)", 
    "Unique identifier for each condition on plot, for joining tables. Optional if only 1 condition (record) per plot", 
    "Unadjusted proportion of condition on each plot. Optional if only 1 condition (record) per plot", 
    "Status of each forested condition on plot (i.e. accessible forest, nonforest, water, etc.)", 
    "Only if ACI=TRUE. Status of each nonforest condition plot (i.e. accessible nonforest, nonsampled nonforest)", 
    "Only if landarea=TIMBERLAND. Measure of site productivity", 
    "If landarea=TIMBERLAND. Reserved status", 
    "Unadjusted proportion of subplot conditions on each plot. Optional if only 1 condition (record) per plot", 
    "If microplot tree attributes. Unadjusted proportion of microplot conditions on each plot. Optional if only 1 condition (record) per plot", 
    "If macroplot tree attributes. Unadjusted proportion of macroplot conditions on each plot. Optional if only 1 condition (record) per plot", 
    "Unique identifier for each plot, for joining tables (ex. CN)", 
    "Identifies state each plot is located in. Optional if only 1 state", 
    "Identifies inventory year of each plot. Optional. Assumes estimation time span is less than inventory cycle"), 
    stringsAsFactors = FALSE)

library(knitr)
kable(required.lut,
  format = "pandoc",   # default
  # caption = "Title of the table",
  col.names = names(required.lut),
  row.names = FALSE,
  align = c("l"),       # align = c("c", "c", "c", "r")
  # padding = 2         # inner spacing
)


## ---- results = 'asis', echo=FALSE---------------------------------------

#stratdat.lut <- read.csv("C:/_tsf/_GitHub/meta/stratdat_variables.csv", stringsAsFactors=FALSE)
#source("C:/_tsf/_GitHub/meta/undataframe.R")
#stratdat.lut <- undataframe(stratdat.lut)

stratdat.lut <- data.frame(Variable = c("ESTN_UNIT", "STRATUMCD", "P1POINTCNT", "n.strata", "n.total", "strwt", "CONDPROP_UNADJ_SUM", 
    "CONDPROP_ADJFAC"), Description = c("Estimation unit", "Strata value", "Number of pixels by strata and estimation unit ", 
    "Number of plots in strata (and estimation unit)", "Number of plots for estimation unit", "Total acres for estimation unit", 
    "Summed condition proportion by strata and estimation unit ", "Adjustment factor for nonsampled plots by strata and/or estimation unit (CONDPROP_UNADJ_SUM/n.strata)"), 
    stringsAsFactors = FALSE)

library(knitr)
kable(stratdat.lut,
  format = "pandoc",   # default
  caption = "Description of variables in stratdat.",
  col.names = names(stratdat.lut),
  row.names = FALSE,
  align = c("l"),       # align = c("c", "c", "c", "r")
  # padding = 2         # inner spacing
) 


## ---- results = 'asis', echo=FALSE---------------------------------------


nonratio <- data.frame(Variable = c("nhat", "nhat.var", "est", "est.var"), Description = c("Estimated proportion", "Variance estimate of estimated proportion", "Estimated acres { nhat * ACRES }", "Variance estimate of estimated acres { nhat * ACRES^2 }"), stringsAsFactors = FALSE)

ratio <- data.frame(Variable = c("nhat", "nhat.var", "dhat", "dhat.var", "covar", "estn", "estd", "estn.var", "estn.se", "estn.cv", "estn.pse", "estd.var", "estd.se", "estd.cv", "estd.pse", "est.covar", "rhat", "rhat.var", "rhat.se", "rhat.cv", "est", "est.var"), Description = c("Estimated proportion of land, for numerator", "Variance estimate of estimated proportion of land, for numerator", "Estimated proportion of land, for denominator", "Variance estimate of estimated proportion of land, for denominator", "Covariance of estimated proportion of numerator and denominator", "Estimated acres, for numerator", "Estimated acres, for denominator", "Variance estimate of estimate acres, for numerator", "Standard error estimated acres, for numerator", "Coeffiecient of variation of estimated acres, for numerator", "Percent sampling error of estimate, for numerator", "Variance estimate of estimate acres, for denominator", "Standard error estimated acres, for denominator", "Coefficient of variation of estimated acres, for denominator", "Percent sampling error of estimate, for denominator", "Covariance of estimated acres of numerator and denominator", "Ratio of estimated proportions (numerator/denominator)", "Variance of ratio of estimated proportions", "Standard error of ratio of estimated proportions { rhat.se/rhat }", "Coefficient of variation of ratio of estimated proportions { sqrt(rhat.var) }", "Estimated percent cover of land { rhat*100 }", "Variance of estimated percent cover of land { rhat.var*100^2 }"), stringsAsFactors = FALSE)

all <- data.frame(Variable = c("NBRPLT", "NBRPLT.gt0", "ACRES", "est.se", "est.cv", "pse", "CI99left", "CI99right", "CI95left", "CI95right", "CI68left", "CI68right"), Description = c("Number of total plots used in estimate", "Number of nonzero plots used in estimate", "Total acres for estimation unit", "Standard error of estimated acres { sqrt(est.var) }", "Coefficient of variation of estimated acres { est.se/est }", "Percent sampling error of estimate { est.cv * 100 }", "Left tail of 99% confidence interval for estimate { est - (2.58*est.se) }", "Right tail of 99% confidence interval for estimate { est + (2.58*est.se) }", "Left tail of 95% confidence interval for estimate { est - (1.96*est.se) }", "Right tail of 95% confidence interval for estimate { est + (1.96*est.se) }", "Left tail of 68% confidence interval for estimate { est - (0.97*est.se) }", "Right tail of 68% confidence interval for estimate { est + (0.97*est.se) }"), stringsAsFactors = FALSE)


library(knitr)
kable(nonratio,
  format = "pandoc",   # default
  caption = "Description of variables in nonratio tables.",
  col.names = names(nonratio),
  row.names = FALSE,
  align = c("l"),       # align = c("c", "c", "c", "r")
  # padding = 2         # inner spacing
) 

library(knitr)
kable(ratio,
  format = "pandoc",   # default
  caption = "Description of variables in nonratio tables.",
  col.names = names(ratio),
  row.names = FALSE,
  align = c("l"),       # align = c("c", "c", "c", "r")
  # padding = 2         # inner spacing
) 

library(knitr)
kable(all,
  format = "pandoc",   # default
  caption = "Description of variables in nonratio and ratio tables.",
  col.names = names(all),
  row.names = FALSE,
  align = c("l"),       # align = c("c", "c", "c", "r")
  # padding = 2         # inner spacing
) 


