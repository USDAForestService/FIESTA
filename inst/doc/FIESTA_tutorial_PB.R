## ---- results = 'asis', echo=FALSE---------------------------------------

stratdat.lut <- data.frame(Variable = c("ESTN_UNIT", "STRATUMCD", "P1POINTCNT", "n.strata", "n.total", "ACRES", "strwt"), Description = c("Estimation unit", "Strata value", "Number of pixels by strata and estimation unit", 
    "Number of plots in strata (and estimation unit)", "Number of plots for estimation unit", "Total acres for estimation unit", 
    "Summed proportions by strata and estimation unit"), stringsAsFactors = FALSE)

library(knitr)
kable(stratdat.lut,
  format = "pandoc",   # default
  caption = "Description of variables in stratdat.",
  col.names = names(stratdat.lut),
  row.names = FALSE,
  align = c("l"),       # align = c("c", "c", "c", "r")
  # padding = 2         # inner spacing
)



pltdom.lut <- data.frame(Variable = c("ESTN_UNIT", "STRATUMCD", "plot_id", "category", "nbrpts.pltdom", "PtsPerPlot", "p.pltdom"), Description = c("Estimation unit", "Strata value", "Unique identifier for ICE plot", 
    "Category (domain) for estimation", "Number of points by category (domain)", "Number of points interpreted", 
    "Proportion of plot by category"), stringsAsFactors = FALSE)

library(knitr)
kable(pltdom.lut,
  format = "pandoc",   # default
  caption = "Description of variables in pltdom.*.",
  col.names = names(pltdom.lut),
  row.names = FALSE,
  align = c("l"),       # align = c("c", "c", "c", "r")
  # padding = 2         # inner spacing
)



## ---- results = 'asis', echo=FALSE---------------------------------------

nonratio <- data.frame(Variable = c("phat", "phat.var", "phat.se", "phat.cv", "est", "est.var"), Description = c("Estimated proportion of land", "Variance estimate of estimated proportion of land", "Standard error of estimated proportion of land { sqrt(phat.var) }", "Coefficient of variance of estimated proportion of land { phat.se/phat }", "Estimated percent cover of land { phat*100 }", "Variance of estimated percent cover of land { phat.var*100^2 }"), stringsAsFactors = FALSE)

ratio <- data.frame(Variable = c("phat.n", "phat.var.n", "phat.d", "phat.var.d", "covar", "rhat", "rhat.var", "rhat.se", "rhat.cv", "est", "est.var"), Description = c("Estimated proportion of land, for numerator", "Variance of estimated proportion of land, for numerator", "Estimated proportion of land, for denominator", "Variance of estimated proportion of land, for denominator", "Covariance of estimated proportion of numerator and denominator", "Ratio of estimated proportions (numerator/denominator)", "Variance of ratio of estimated proportions", "Standard error of ratio of estimated proportions { rhat.se/rhat }", "Coefficient of variation of ratio of estimated proportions { sqrt(rhat.var) }", "Estimated percent cover of land { rhat*100 }", "Variance of estimated percent cover of land { rhat.var*100^2 }"), stringsAsFactors = FALSE)

both <- data.frame(Variable = c("nbrpts", "ACRES", "est.se", "est.cv", "pse"), Description=c("Number of points used in estimate", "Total acres for estimation unit (if tabtype='AREA')", "Standard error of estimated percent cover of land { sqrt(est.var) }", "Coefficient of variance of estimated percent cover of land { est.se/est }", "Percent sampling error of the estimated percent cover of land { est.cv*100 }"), stringsAsFactors = FALSE)
  
gainloss <- data.frame(Variable = c("gain.val", "loss.val", "gain.est", "gain.se", "loss.est", "loss.se", "diff.est", "diff.se"), Description = c("Binary class for gain (Not-class to class). For each class, all other values are grouped to Not-class", "Binary class for loss (Not-class to class). For each class, all other values are grouped to Not-class"), "Estimated percent cover where the Class went from Not-class to Class", "Standard error of estimated gain", "Estimated percent cover where the Class went from Class to Not-class", "Standard error of estimated loss", "Difference of estimated gain and estimate loss", "Standard error of the difference of estimated gain and estimated loss")

all <- data.frame(Variable = c("CI99left", "CI99right", "CI95left", "CI95right", "CI68left", "CI68right"), Description = c("Left tail of 99% confidence interval for estimate { est - (2.58*est.se) }", "Right tail of 99% confidence interval for estimate { est + (2.58*est.se) }", 
    "Left tail of 95% confidence interval for estimate { est - (1.96*est.se) }", "Right tail of 95% confidence interval for estimate { est + (1.96*est.se) }", 
    "Left tail of 68% confidence interval for estimate { est - (0.97*est.se) }", "Right tail of 68% confidence interval for estimate { est + (0.97*est.se) }"), 
    stringsAsFactors = FALSE)

library(knitr)
kable(nonratio,
  format = "pandoc",   # default
  caption = "Description of variables in processing tables for nonratio estimates.",
  col.names = names(nonratio),
  row.names = FALSE,
  align = c("l"),       # align = c("c", "c", "c", "r")
  # padding = 2         # inner spacing
)

library(knitr)
kable(ratio,
  format = "pandoc",   # default
  caption = "Description of variables in processing tables for ratio estimates.",
  col.names = names(ratio),
  row.names = FALSE,
  align = c("l"),       # align = c("c", "c", "c", "r")
  # padding = 2         # inner spacing
)

library(knitr)
kable(both,
  format = "pandoc",   # default
  caption = "Description of variables in processing tables for both nonratio and ratio estimates.",
  col.names = names(both),
  row.names = FALSE,
  align = c("l"),       # align = c("c", "c", "c", "r")
  # padding = 2         # inner spacing
)

library(knitr)
kable(all,
  format = "pandoc",   # default
  caption = "Description of variables in processing tables for all estimates.",
  col.names = names(all),
  row.names = FALSE,
  align = c("l"),       # align = c("c", "c", "c", "r")
  # padding = 2         # inner spacing
)


