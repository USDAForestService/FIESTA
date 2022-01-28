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
nonratio <- data.frame(Variable = c("nhat", "nhat.var", "est", "est.var"), Description = c("Estimated proportion", "Variance estimate of estimated proportion", "Estimated acres { nhat * ACRES }", "Variance estimate of estimated acres { nhat * ACRES^2 }"), stringsAsFactors = FALSE)

ratio <- data.frame(Variable = c("nhat", "nhat.var", "dhat", "dhat.var", "covar", "estn", "estd", "estn.var", "estn.se", "estn.cv", "estn.pse", "estd.var", "estd.se", "estd.cv", "estd.pse", "est.covar", "rhat", "rhat.var", "rhat.se", "rhat.cv", "est", "est.var"), Description = c("Estimated proportion of land, for numerator", "Variance estimate of estimated proportion of land, for numerator", "Estimated proportion of land, for denominator", "Variance estimate of estimated proportion of land, for denominator", "Covariance of estimated proportion of numerator and denominator", "Estimated acres, for numerator", "Estimated acres, for denominator", "Variance estimate of estimate acres, for numerator", "Standard error estimated acres, for numerator", "Coeffiecient of variation of estimated acres, for numerator", "Percent sampling error of estimate, for numerator", "Variance estimate of estimate acres, for denominator", "Standard error estimated acres, for denominator", "Coefficient of variation of estimated acres, for denominator", "Percent sampling error of estimate, for denominator", "Covariance of estimated acres of numerator and denominator", "Ratio of estimated proportions (numerator/denominator)", "Variance of ratio of estimated proportions", "Standard error of ratio of estimated proportions { rhat.se/rhat }", "Coefficient of variation of ratio of estimated proportions { sqrt(rhat.var) }", "Estimated percent cover of land { rhat*100 }", "Variance of estimated percent cover of land { rhat.var*100^2 }"), stringsAsFactors = FALSE)

all <- data.frame(Variable = c("NBRPLT.gt0", "ACRES", "est.se", "est.cv", "pse", "CI99left", "CI99right", "CI95left", "CI95right", "CI68left", "CI68right"), Description = c("Number of nonzero plots used in estimate", "Total acres for estimation unit", "Standard error of estimated acres { sqrt(est.var) }", "Coefficient of variation of estimated acres { est.se/est }", "Percent sampling error of estimate { est.cv * 100 }", "Left tail of 99% confidence interval for estimate { est - (2.58*est.se) }", "Right tail of 99% confidence interval for estimate { est + (2.58*est.se) }", "Left tail of 95% confidence interval for estimate { est - (1.96*est.se) }", "Right tail of 95% confidence interval for estimate { est + (1.96*est.se) }", "Left tail of 68% confidence interval for estimate { est - (0.97*est.se) }", "Right tail of 68% confidence interval for estimate { est + (0.97*est.se) }"), stringsAsFactors = FALSE)



kable(nonratio,
  format = "pandoc",   # default
  caption = "Description of variables in nonratio tables.",
  col.names = names(nonratio),
  row.names = FALSE,
  align = c("l"),       # align = c("c", "c", "c", "r")
  # padding = 2         # inner spacing
) 


kable(ratio,
  format = "pandoc",   # default
  caption = "Description of variables in nonratio tables.",
  col.names = names(ratio),
  row.names = FALSE,
  align = c("l"),       # align = c("c", "c", "c", "r")
  # padding = 2         # inner spacing
) 


kable(all,
  format = "pandoc",   # default
  caption = "Description of variables in nonratio and ratio tables.",
  col.names = names(all),
  row.names = FALSE,
  align = c("l"),       # align = c("c", "c", "c", "r")
  # padding = 2         # inner spacing
) 


