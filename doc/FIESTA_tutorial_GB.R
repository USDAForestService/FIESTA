## ---- results = 'asis', echo=FALSE--------------------------------------------

#required.lut <- read.table("C:/_tsf/_GitHub/meta/required_variables.txt", header=TRUE, sep="\t")

required.lut <- data.frame(
  Table = c("tree", "", "", "cond", "", "", "", "", "", "", "", "", "", "plot", "", ""), 
  Variable = c("*tuniqueid*", "CONDID", "TPA_UNADJ", "*cuniqueid*", "CONDID", "CONDPROP_UNADJ", 
               "COND_STATUS_CD", "NF_COND_STATUS_CD", "SITECLCD", "RESERVCD", "SUBPROP_UNADJ",
               "MICRPROP_UNADJ", "MACRPROP_UNADJ", "*puniqueid*", "STATECD", "INVYR"), 
  Description = c("Unique identifier for each plot, for joining tables (ex. PLT_CN)", 
    "Unique identifier for each condition on plot, for joining tables. Optional if only 1 condition (record)   per plot", 
    "Number of trees per acre each sample tree represents (ex. DESIGNCD=1: TPA_UNADJ=6.018046 for trees on subplot; 74.965282 for trees on microplot)", "Unique identifier for each plot, for joining tables (e.g., PLT_CN)", 
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


## ---- results = 'asis', echo=FALSE--------------------------------------------

#stratdat.lut <- read.csv("C:/_tsf/_GitHub/meta/stratdat_variables.csv", stringsAsFactors=FALSE)
#source("C:/_tsf/_GitHub/meta/undataframe.R")
#stratdat.lut <- undataframe(stratdat.lut)

stratdat.lut <- data.frame(
Variable = c("ESTN_UNIT", "STRATUMCD", "P1POINTCNT", "P2POINTCNT", "n.strata", "n.total", "strwt", "CONDPROP_UNADJ_SUM", "cadjfac", "ACRES", "expfac", "EXPNS"), 
Description = c("Estimation unit", "Strata value", "Number of pixels by strata and estimation unit", "Number of P2 plots in population data", "Number of sampled plots in strata", "Number of sampled plots for estimation unit", "Proportion of pixels in strata (strata weight)", "Summed condition proportion in strata", "Adjustment factor for nonsampled plots in strata (CONDPROP_UNADJ_SUM/n.strata)", "Total acres for estimation unit", "Expansion factor, in acres, area in strata divided by number of sampled plots", "Expanded area, in acres, expfac multiplied by strwt"), 
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


## ---- results = 'asis', echo=FALSE--------------------------------------------


nonratio <- data.frame(Variable = c("nhat", "nhat.var", "est", "est.var"), Description = c("Estimated proportion", "Variance estimate of estimated proportion", "Estimated acres { nhat * ACRES }", "Variance estimate of estimated acres { nhat * ACRES^2 }"), stringsAsFactors = FALSE)

ratio <- data.frame(Variable = c("nhat", "nhat.var", "dhat", "dhat.var", "covar", "estn", "estd", "estn.var", "estn.se", "estn.cv", "estn.pse", "estd.var", "estd.se", "estd.cv", "estd.pse", "est.covar", "rhat", "rhat.var", "rhat.se", "rhat.cv", "est", "est.var"), Description = c("Estimated proportion of land, for numerator", "Variance estimate of estimated proportion of land, for numerator", "Estimated proportion of land, for denominator", "Variance estimate of estimated proportion of land, for denominator", "Covariance of estimated proportion of numerator and denominator", "Estimated acres, for numerator", "Estimated acres, for denominator", "Variance estimate of estimate acres, for numerator", "Standard error estimated acres, for numerator", "Coeffiecient of variation of estimated acres, for numerator", "Percent sampling error of estimate, for numerator", "Variance estimate of estimate acres, for denominator", "Standard error estimated acres, for denominator", "Coefficient of variation of estimated acres, for denominator", "Percent sampling error of estimate, for denominator", "Covariance of estimated acres of numerator and denominator", "Ratio of estimated proportions (numerator/denominator)", "Variance of ratio of estimated proportions", "Standard error of ratio of estimated proportions { rhat.se/rhat }", "Coefficient of variation of ratio of estimated proportions { sqrt(rhat.var) }", "Estimated percent cover of land { rhat*100 }", "Variance of estimated percent cover of land { rhat.var*100^2 }"), stringsAsFactors = FALSE)

all <- data.frame(Variable = c("NBRPLT.gt0", "ACRES", "est.se", "est.cv", "pse", "CI99left", "CI99right", "CI95left", "CI95right", "CI68left", "CI68right"), Description = c("Number of nonzero plots used in estimate", "Total acres for estimation unit", "Standard error of estimated acres { sqrt(est.var) }", "Coefficient of variation of estimated acres { est.se/est }", "Percent sampling error of estimate { est.cv * 100 }", "Left tail of 99% confidence interval for estimate { est - (2.58*est.se) }", "Right tail of 99% confidence interval for estimate { est + (2.58*est.se) }", "Left tail of 95% confidence interval for estimate { est - (1.96*est.se) }", "Right tail of 95% confidence interval for estimate { est + (1.96*est.se) }", "Left tail of 68% confidence interval for estimate { est - (0.97*est.se) }", "Right tail of 68% confidence interval for estimate { est + (0.97*est.se) }"), stringsAsFactors = FALSE)


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


## -----------------------------------------------------------------------------

## Set options
options(scipen=6)
options(stringsAsFactors=FALSE)

# Load library
library(FIESTA)

## If not on windows platform, set up an outfolder to output from examples
if (.Platform$OS.type == "windows") {
  # Set workspace to FIESTA folder (create if doesn't exist)
  if (!file.exists("C:/FIESTA"))  {
    dir.create("C:/FIESTA")
  }
  setwd("C:/FIESTA")

  if (!file.exists("outfolder")) {
    dir.create("outfolder")
  }
  outfolder <- "outfolder"
}

if (.Platform$OS.type == "unix") {
  dir.create("FIESTA")
  if (!file.exists("outfolder")) {
    dir.create(paste0(getwd(),"/FIESTA", "/outfolder"))
    }
  outfolder <- paste0(getwd(),"/FIESTA", "/outfolder")
}


## -----------------------------------------------------------------------------

## Get population data for Wyoming area and tree estimates, using post-stratification
GBpopdat <- modGBpop(
    cond=WYcond,               # FIA plot/condition data
    tree=WYtree,               # FIA tree data
    seed=WYseed,               # FIA seedling data
    pltassgn=WYpltassgn,       # plot assignments
    pltassgnid="CN",           # uniqueid of plots
    unitarea=WYunitarea,       # area by estimation units
    unitvar="ESTN_UNIT",       # name of estimation unit
    strata=TRUE,               # if using post-stratification
    stratalut=WYstratalut      # strata classes and pixels counts
    )

## Get names of list components
names(GBpopdat)

## Look at output from GBpopdat
GBpopdat$plotsampcnt	# Number of plots by plot status
GBpopdat$condsampcnt	# Number of conditions by condition status

# Strata-level population data, including number of plots and adjustment factors
head(GBpopdat$stratalut)    

## Adjustment factors added to condition-level data
head(GBpopdat$condx)

## Adjustment factors added to tree data
head(GBpopdat$treex)

## Adjustment factors added to seedling data
head(GBpopdat$seedx)


## Compare FIESTA output with FIADB pop_stratum table for WY, 2013 evaluation
qry <- "select estn_unit, stratumcd, p1pointcnt, p2pointcnt, expns, 
          adj_factor_macr, adj_factor_subp, adj_factor_micr from pop_stratum 
        where evalid = 561301 order by estn_unit, stratumcd"
pop_stratum <- DBqryCSV(
                  qry, 
                  states="Wyoming",
                  sqltables="pop_stratum"
                  )



head(pop_stratum)
head(GBpopdat$stratalut)



## -----------------------------------------------------------------------------

## Generate estimates by estimation unit (i.e., ESTN_UNIT) and sum to population (i.e., WY)
## Return raw data and titles
## Area of forest land, Wyoming, 2011-2013 (sum estimation units)
area1 <- modGBarea(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE           # out - return title information
    )

## Look at output list
names(area1)

## Estimate and percent sampling error of estimate
area1$est


## Raw data (list object) for estimate
raw <- area1$raw      # extract raw data list object from output
names(raw)
raw$unit_totest       # estimates by estimation unit (i.e., ESTN_UNIT)
raw$totest            # estimates for population (i.e., WY)


## Titles (list object) for estimate
titlelst <- area1$titlelst
names(titlelst)
titlelst



## Add rows to output:
## Area of forest land by forest type, Wyoming, 2011-2013
area2  <- modGBarea(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    rowvar="FORTYPCD",         # est - row domain
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE           # out - return title information
    )

## Look at output list
names(area2)

## Estimate and percent sampling error of estimate
area2$est


## Raw data (list object) for estimate
raw <- area2$raw      # extract raw data list object from output
names(raw)
raw$unit_totest       # estimates by estimation unit (i.e., ESTN_UNIT)
raw$totest            # estimates for population (i.e., WY)
raw$unit_rowest       # estimates by row, by estimation unit (i.e., ESTN_UNIT)
raw$rowest            # estimates by row for population (i.e., WY)


## Titles (list object) for estimate
titlelst <- area2$titlelst
names(titlelst)
titlelst




## Add row and columns to output, including FIA names
## Return output with estimates (est) and percent standard error (pse) in same cell - est(pse)
## Save data to outfolder:
## Area of forest land by forest type and stand-size class, Wyoming, 2011-2013
area3 <- modGBarea(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    rowvar="FORTYPCD",         # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    colvar="STDSZCD",          # est - column domain
    col.FIAname=TRUE,          # est - column domain names
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=TRUE,               # out - return output with est(pse)
    savedata=TRUE,             # out - save data to outfolder
    outfolder=outfolder,       # out - outfolder for saving data
    outfn.pre="WY"             # out - prefix for output files
    )

## Look at output list
names(area3)

## Estimate and percent sampling error of estimate
area3$est


## Raw data (list object) for estimate
raw <- area3$raw      # extract raw data list object from output
names(raw)
raw$unit_totest       # estimates by estimation unit (i.e., ESTN_UNIT)
raw$totest            # estimates for population (i.e., WY)
raw$unit_rowest       # estimates by row, by estimation unit (i.e., ESTN_UNIT)
raw$rowest            # estimates by row for population (i.e., WY)
raw$unit_colest       # estimates by column, by estimation unit (i.e., ESTN_UNIT)
raw$colest            # estimates by column for population (i.e., WY)
raw$unit_grpest       # estimates by row and column, by estimation unit (i.e., ESTN_UNIT)
raw$grpest            # estimates by row and column for population (i.e., WY)


## Titles (list object) for estimate
titlelst <- area3$titlelst
names(titlelst)
titlelst


## List output files in outfolder
list.files(outfolder, pattern="WY_area")
list.files(paste0(outfolder, "/rawdata"), pattern="WY_area")



## -----------------------------------------------------------------------------

## The following reference table can be used for defining estvar and estvar.filter
ref_estvar[, c("ESTTITLE", "ESTVAR", "ESTFILTER", "ESTUNITS")]


## -----------------------------------------------------------------------------


## Generate estimates by estimation unit (i.e., ESTN_UNIT) and sum to population (i.e., WY)
## Return raw data and titles
## Total net cubic-foot volume of live trees (at least 5 inches diameter), Wyoming, 2011-2013 
tree1 <- modGBtree(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvar="VOLCFNET",               # est - net cubic-foot volume
    estvar.filter="STATUSCD == 1",   # est - live trees only
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE           # out - return title information
    )


## Look at output list
names(tree1)

## Estimate and percent sampling error of estimate
tree1$est


## Raw data (list object) for estimate
raw <- tree1$raw      # extract raw data list object from output
names(raw)
raw$unit_totest       # estimates by estimation unit (i.e., ESTN_UNIT)
raw$totest            # estimates for population (i.e., WY)


## Titles (list object) for estimate
titlelst <- tree1$titlelst
names(titlelst)
titlelst




## Add rows to output
## Net cubic-foot volume of live trees (at least 5 inches diameter) by forest type, Wyoming, 2011-2013
## Return raw data and titles
tree2 <- modGBtree(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvar="VOLCFNET",               # est - net cubic-foot volume
    estvar.filter="STATUSCD == 1",   # est - live trees only
    rowvar="FORTYPCD",         # est - row domain 
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE           # out - return title information
    )

## Look at output list
names(tree2)

## Estimate and percent sampling error of estimate
tree2$est


## Raw data (list object) for estimate
raw <- tree2$raw      # extract raw data list object from output
names(raw)
raw$unit_totest       # estimates by estimation unit (i.e., ESTN_UNIT)
raw$totest            # estimates for population (i.e., WY)
raw$unit_rowest       # estimates by row, by estimation unit (i.e., ESTN_UNIT)
raw$rowest            # estimates by row for population (i.e., WY)


## Titles (list object) for estimate
titlelst <- tree2$titlelst
names(titlelst)
titlelst



## Add row and columns to output, including FIA names
## Return output with estimates (est) and percent standard error (pse) in same cell - est(pse)
## Save data to outfolder:
## Net cubic-foot volume of live trees (at least 5 inches diameter) by forest type and stand-size class, 
##    Wyoming, 2011-2013
tree3 <- modGBtree(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvar="VOLCFNET",               # est - net cubic-foot volume
    estvar.filter="STATUSCD == 1",   # est - live trees only
    rowvar="FORTYPCD",         # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    colvar="STDSZCD",          # est - column domain
    col.FIAname=TRUE,          # est - column domain names
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=TRUE,               # out - return output with est(pse)
    savedata=TRUE,             # out - save data to outfolder
    outfolder=outfolder,       # out - outfolder for saving data
    outfn.pre="WY"             # out - prefix for output files
    )

## Look at output list from modGBarea()
names(tree3)

## Estimate and percent sampling error of estimate
tree3$est


## Raw data (list object) for estimate
raw <- tree3$raw      # extract raw data list object from output
names(raw)
raw$unit_totest       # estimates by estimation unit (i.e., ESTN_UNIT)
raw$totest            # estimates for population (i.e., WY)
raw$unit_rowest       # estimates by row, by estimation unit (i.e., ESTN_UNIT)
raw$rowest            # estimates by row for population (i.e., WY)
raw$unit_colest       # estimates by column, by estimation unit (i.e., ESTN_UNIT)
raw$colest            # estimates by column for population (i.e., WY)
raw$unit_grpest       # estimates by row and column, by estimation unit (i.e., ESTN_UNIT)
raw$grpest            # estimates by row and column for population (i.e., WY)


## Titles (list object) for estimate
titlelst <- tree3$titlelst
names(titlelst)
titlelst


## List output files in outfolder
list.files(outfolder, pattern="WY_tree")
list.files(paste0(outfolder, "/rawdata"), pattern="WY_tree")



## -----------------------------------------------------------------------------

## Number of live trees (at least 1 inch diameter) by species
tree4a <- modGBtree(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvar="TPA_UNADJ",               # est - number of trees per acre 
    estvar.filter="STATUSCD == 1",    # est - live trees only
    rowvar="SPCD",             # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=FALSE,              # out - return output with est and pse
    )

## Look at output list
names(tree4a)

## Estimate and percent sampling error of estimate
tree4a$est



## -----------------------------------------------------------------------------

## Number of live trees by species, including seedlings
tree4b <- modGBtree(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    estseed="add",             # est - add seedling data
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvar="TPA_UNADJ",               # est - number of trees per acre 
    estvar.filter="STATUSCD == 1",    # est - live trees only
    rowvar="SPCD",             # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=FALSE,              # out - return output with est and pse
    )

## Look at output list
names(tree4b)

## Estimate and percent sampling error of estimate
tree4b$est


## Compare estimates with and without seedlings
head(tree4a$est)
head(tree4b$est)



## -----------------------------------------------------------------------------

## Number of live trees seedlings by species
tree4c <- modGBtree(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    estseed="only",            # est - add seedling data
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvar="TPA_UNADJ",               # est - number of trees per acre 
    rowvar="SPCD",             # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=FALSE,              # out - return output with est and pse
    )

## Look at output list
names(tree4c)

## Estimate and percent sampling error of estimate
tree4c$est


## Compare estimates with, without, and only seedlings
head(tree4a$est)
head(tree4b$est)
head(tree4c$est)



## -----------------------------------------------------------------------------

## Number of live trees (at least 1 inch diameter) by forest type and species
tree5 <- modGBtree(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvar="TPA_UNADJ",               # est - number of trees per acre 
    estvar.filter="STATUSCD == 1",    # est - live trees only
    rowvar="FORTYPCD",         # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    colvar="SPCD",             # est - column domain
    col.FIAname=TRUE,          # est - column domain names
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=TRUE,               # out - return output with est(pse)
    )

## Look at output list
names(tree5)

## Estimate and percent sampling error of estimate
tree5$est



## -----------------------------------------------------------------------------


## Net cubic-foot volume of dead trees (at least 5 inches diameter) by species and cause of death, 
##    Wyoming, 2011-2013
tree6 <- modGBtree(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvar="VOLCFNET",         # est - number of trees per acre 
    estvar.filter="STATUSCD == 2 & STANDING_DEAD_CD == 1",    # est - standing dead trees only
    rowvar="SPCD",             # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    colvar="AGENTCD",          # est - column domain
    col.FIAname=TRUE,          # est - column domain names
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=TRUE,               # out - return output with est(pse)
    )

## Look at output list
names(tree6)

## Estimate and percent sampling error of estimate
tree6$est



## -----------------------------------------------------------------------------

## Look at tree data in GBpopdat
head(GBpopdat$treex)

## Use reference data frame stored as an R object in FIESTA
head(ref_diacl2in)

## Appends a new column to GBpopdat$treex classifying the DIA variable based on MIN and MAX columns in ref_diacl2in
dat <- datLUTclass(x=GBpopdat$treex, xvar="DIA", LUT=ref_diacl2in, LUTclassnm="DIACL2IN")
GBpopdat$treex <- dat$xLUT

## Look at tree data, with new column (DIACL2IN)  
head(GBpopdat$treex)

## Look at table of new diameter classes (DIACL2IN)
table(GBpopdat$treex$DIACL2IN)



## Another way to append diameter classes
## First, create a new variable using cut function to define 4 diameter classes
dat <- datLUTclass(x=GBpopdat$treex, xvar="DIA", cutbreaks=c(0, 5, 10, 20, 100))
GBpopdat$treex <- dat$xLUT

## Look at tree data, with new column (DIACL2IN)  
head(GBpopdat$treex)

## Look at table of new diameter classes (DIACL)
table(GBpopdat$treex$DIACL)



## -----------------------------------------------------------------------------


## Number of live trees by species group and diameter class (DIACL2IN)
tree7 <- modGBtree(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvar="TPA_UNADJ",               # est - number of trees per acre 
    estvar.filter="STATUSCD == 1",    # est - live trees only
    rowvar="SPGRPCD",          # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    colvar="DIACL2IN",         # est - column domain
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=TRUE,               # out - return output with est(pse)
    )

## Look at output list
names(tree7)

## Estimate and percent sampling error of estimate
tree7$est


## Number of live trees by species group and diameter class (DIACL)
tree8 <- modGBtree(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvar="TPA_UNADJ",               # est - number of trees per acre 
    estvar.filter="STATUSCD == 1",    # est - live trees only
    rowvar="SPGRPCD",          # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    colvar="DIACL",            # est - column domain
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=TRUE,               # out - return output with est(pse)
    )

## Look at output list
names(tree8)

## Estimate and percent sampling error of estimate
tree8$est



## Number of live trees by species group and diameter class (DIACL), add seedlings
tree9 <- modGBtree(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    estseed="add",             # est - add seedling data
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvar="TPA_UNADJ",               # est - number of trees per acre 
    estvar.filter="STATUSCD == 1",    # est - live trees only
    rowvar="SPGRPCD",          # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    colvar="DIACL",            # est - column domain
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=TRUE,               # out - return output with est(pse)
    )

## Look at output list
names(tree9)

## Estimate and percent sampling error of estimate
tree9$est



## -----------------------------------------------------------------------------


## Generate estimates by estimation unit (i.e., ESTN_UNIT) and sum to population (i.e., WY)
## Return raw data and titles
## Total net cubic-foot volume of live trees (at least 5 inches diameter), Wyoming, 2011-2013 
ratio1 <- modGBratio(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvarn="VOLCFNET",               # est - net cubic-foot volume, numerator
    estvarn.filter="STATUSCD == 1",   # est - live trees only, numerator
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE           # out - return title information
    )


## Look at output list
names(ratio1)

## Estimate and percent sampling error of estimate
ratio1$est


## Raw data (list object) for estimate
raw <- ratio1$raw      # extract raw data list object from output
names(raw)
raw$unit_totest       # estimates by estimation unit (i.e., ESTN_UNIT)
raw$totest            # estimates for population (i.e., WY)


## Titles (list object) for estimate
titlelst <- ratio1$titlelst
names(titlelst)
titlelst




## Add rows to output
## Net cubic-foot volume of live trees (at least 5 inches diameter) by forest type, Wyoming, 2011-2013
## Return raw data and titles
ratio2 <- modGBratio(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvarn="VOLCFNET",               # est - net cubic-foot volume
    estvarn.filter="STATUSCD == 1",   # est - live trees only
    rowvar="FORTYPCD",         # est - row domain 
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE           # out - return title information
    )

## Look at output list
names(ratio2)

## Estimate and percent sampling error of estimate
ratio2$est


## Raw data (list object) for estimate
raw <- ratio2$raw      # extract raw data list object from output
names(raw)
raw$unit_totest       # estimates by estimation unit (i.e., ESTN_UNIT)
raw$totest            # estimates for population (i.e., WY)
raw$unit_rowest       # estimates by row, by estimation unit (i.e., ESTN_UNIT)
raw$rowest            # estimates by row for population (i.e., WY)


## Titles (list object) for estimate
titlelst <- ratio2$titlelst
names(titlelst)
titlelst



## Add row and columns to output, including FIA names
## Return output with estimates (est) and percent standard error (pse) in same cell - est(pse)
## Save data to outfolder:
## Net cubic-foot volume of live trees (at least 5 inches diameter) by forest type and stand-size class, 
##    Wyoming, 2011-2013
ratio3 <- modGBratio(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvarn="VOLCFNET",               # est - net cubic-foot volume, numerator
    estvarn.filter="STATUSCD == 1",   # est - live trees only, numerator
    rowvar="FORTYPCD",         # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    colvar="STDSZCD",          # est - column domain
    col.FIAname=TRUE,          # est - column domain names
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=TRUE,               # out - return output with est(pse)
    savedata=TRUE,             # out - save data to outfolder
    outfolder=outfolder,       # out - outfolder for saving data
    outfn.pre="WY"             # out - prefix for output files
    )

## Look at output list from modGBarea()
names(ratio3)

## Estimate and percent sampling error of estimate
ratio3$est


## Raw data (list object) for estimate
raw <- ratio3$raw      # extract raw data list object from output
names(raw)
raw$unit_totest       # estimates by estimation unit (i.e., ESTN_UNIT)
raw$totest            # estimates for population (i.e., WY)
raw$unit_rowest       # estimates by row, by estimation unit (i.e., ESTN_UNIT)
raw$rowest            # estimates by row for population (i.e., WY)
raw$unit_colest       # estimates by column, by estimation unit (i.e., ESTN_UNIT)
raw$colest            # estimates by column for population (i.e., WY)
raw$unit_grpest       # estimates by row and column, by estimation unit (i.e., ESTN_UNIT)
raw$grpest            # estimates by row and column for population (i.e., WY)


## Titles (list object) for estimate
titlelst <- ratio3$titlelst
names(titlelst)
titlelst


## List output files in outfolder
list.files(outfolder, pattern="WY_ratio")
list.files(paste0(outfolder, "/rawdata"), pattern="WY_ratio")



## -----------------------------------------------------------------------------

## Number of live trees (at least 1 inch diameter) by species
ratio4a <- modGBratio(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvarn="TPA_UNADJ",               # est - number of trees per acre, numerator 
    estvarn.filter="STATUSCD == 1",    # est - live trees only, numerator
    rowvar="SPCD",             # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=FALSE,              # out - return output with est and pse
    )

## Look at output list
names(ratio4a)

## Estimate and percent sampling error of estimate
ratio4a$est



## -----------------------------------------------------------------------------

## Number of live trees by species, including seedlings
ratio4b <- modGBratio(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    estseed="add",             # est - add seedling data
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvarn="TPA_UNADJ",               # est - number of trees per acre, numerator 
    estvarn.filter="STATUSCD == 1",    # est - live trees only, numerator
    rowvar="SPCD",             # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=FALSE,              # out - return output with est and pse
    )

## Look at output list
names(ratio4b)

## Estimate and percent sampling error of estimate
ratio4b$est


## Compare estimates with and without seedlings
head(ratio4a$est)
head(ratio4b$est)



## -----------------------------------------------------------------------------

## Number of live seedlings by species
ratio4c <- modGBratio(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    estseed="only",            # est - add seedling data
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvarn="TPA_UNADJ",       # est - number of trees per acre, numerator 
    rowvar="SPCD",             # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=FALSE,              # out - return output with est and pse
    )

## Look at output list
names(ratio4c)

## Estimate and percent sampling error of estimate
ratio4c$est


## Compare estimates with, without, and only seedlings
head(ratio4a$est)
head(ratio4b$est)
head(ratio4c$est)



## -----------------------------------------------------------------------------

## Number of live trees (at least 1 inch diameter) by forest type and species
ratio5 <- modGBratio(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvarn="TPA_UNADJ",               # est - number of trees per acre, numerator 
    estvarn.filter="STATUSCD == 1",    # est - live trees only, numerator
    rowvar="FORTYPCD",         # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    colvar="SPCD",             # est - column domain
    col.FIAname=TRUE,          # est - column domain names
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=TRUE,               # out - return output with est(pse)
    )

## Look at output list
names(ratio5)

## Estimate and percent sampling error of estimate
ratio5$est



## -----------------------------------------------------------------------------


## Net cubic-foot volume of dead trees (at least 5 inches diameter) by species and cause of death, 
##    Wyoming, 2011-2013
ratio6 <- modGBratio(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvarn="VOLCFNET",         # est - number of trees per acre, numerator 
    estvarn.filter="STATUSCD == 2 & STANDING_DEAD_CD == 1",    # est - standing dead trees only, numerator
    rowvar="SPCD",             # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    colvar="AGENTCD",          # est - column domain
    col.FIAname=TRUE,          # est - column domain names
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=TRUE,               # out - return output with est(pse)
    )

## Look at output list
names(ratio6)

## Estimate and percent sampling error of estimate
ratio6$est



## -----------------------------------------------------------------------------


## Number of live trees by species group and diameter class (DIACL2IN)
ratio7 <- modGBratio(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvarn="TPA_UNADJ",               # est - number of trees per acre, numerator 
    estvarn.filter="STATUSCD == 1",    # est - live trees only, numerator
    rowvar="SPGRPCD",          # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    colvar="DIACL2IN",         # est - column domain
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=TRUE,               # out - return output with est(pse)
    )

## Look at output list
names(ratio7)

## Estimate and percent sampling error of estimate
ratio7$est


## Number of live trees by species group and diameter class (DIACL)
ratio8 <- modGBratio(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvarn="TPA_UNADJ",               # est - number of trees per acre, numerator 
    estvarn.filter="STATUSCD == 1",    # est - live trees only, numerator
    rowvar="SPGRPCD",          # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    colvar="DIACL",            # est - column domain
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=TRUE,               # out - return output with est(pse)
    )

## Look at output list
names(ratio8)

## Estimate and percent sampling error of estimate
ratio8$est



## Number of live trees by species group and diameter class (DIACL), add seedlings
ratio9 <- modGBratio(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    estseed="add",             # est - add seedling data
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvarn="TPA_UNADJ",               # est - number of trees per acre, numerator 
    estvarn.filter="STATUSCD == 1",    # est - live trees only
    rowvar="SPGRPCD",          # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    colvar="DIACL",            # est - column domain
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=TRUE,               # out - return output with est(pse)
    )

## Look at output list
names(ratio9)

## Estimate and percent sampling error of estimate
ratio9$est



## -----------------------------------------------------------------------------

## Net cubic-foot volume of live trees (at least 5 inches diameter) divided by net cubic-foot volume of all trees 
##    by forest type, Wyoming, 2011-2013
ratio10 <- modGBratio(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    estseed="add",             # est - add seedling data
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvarn="VOLCFNET",                # est - net cubic-foot volume, numerator
    estvarn.filter="STATUSCD == 1",    # est - live trees only
    estvard="VOLCFNET",                # est - net cubic-foot volume, numerator
    rowvar="FORTYPCD",         # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=TRUE,               # out - return output with est(pse)
    )

## Look at output list
names(ratio10)

## Estimate and percent sampling error of estimate
ratio10$est



## -----------------------------------------------------------------------------


## Get population data for Wyoming estimates, with no post-stratification
GBpopdat.strat <- modGBpop(
    cond=WYcond,               # FIA plot/condition data
    tree=WYtree,               # FIA tree data
    seed=WYtree,               # FIA seedling data
    pltassgn=WYpltassgn,       # plot assignments
    pltassgnid="CN",           # uniqueid of plots
    unitarea=WYunitarea,       # area by estimation units
    unitvar="ESTN_UNIT",       # name of estimation unit
    strata=TRUE,               # if using post-stratification
    stratalut=WYstratalut      # strata classes and pixels counts
    )


## Get population data for Wyoming estimates, with no post-stratification
GBpopdat.nostrat <- modGBpop(
    cond=WYcond,               # FIA plot/condition data
    tree=WYtree,               # FIA tree data
    seed=WYtree,               # FIA seedling data
    pltassgn=WYpltassgn,       # plot assignments
    pltassgnid="CN",           # uniqueid of plots
    unitarea=WYunitarea,       # area by estimation units
    unitvar="ESTN_UNIT",       # name of estimation unit
    strata=FALSE               # if using post-stratification
    )


## Area of forest land by forest type and stand-size class, Wyoming, 2011-2013, with post-stratification
area.strat <- modGBarea( 
    GBpopdat=GBpopdat.strat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    rowvar="FORTYPCD",         # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    rawdata=TRUE,              # out - return raw data 
    allin1=FALSE               # out - return output with est(pse)
    )   

## Area of forest land by forest type and stand-size class, Wyoming, 2011-2013, no post-stratification
area.nostrat <- modGBarea( 
    GBpopdat=GBpopdat.nostrat, # pop - population calculations for WY, no post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    rowvar="FORTYPCD",         # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    rawdata=TRUE,              # out - return raw data 
    allin1=FALSE               # out - return output with est(pse)
    )   


## Compare estimates and percent standard errors with and without post-stratification
head(area.strat$est)
head(area.nostrat$est)



## Number of live trees by species, Wyoming, 2011-2013, with post-stratification
tree.strat <- modGBtree( 
    GBpopdat=GBpopdat.strat,   # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvar="TPA_UNADJ",               # est - number of trees per acre, numerator 
    estvar.filter="STATUSCD == 1",    # est - live trees only, numerator
    rowvar="FORTYPCD",         # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    rawdata=TRUE,              # out - return raw data 
    allin1=FALSE               # out - return output with est(pse)
    )   

## Number of live trees by species, Wyoming, 2011-2013, no post-stratification
tree.nostrat <- modGBtree( 
    GBpopdat=GBpopdat.nostrat, # pop - population calculations for WY, no post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvar="TPA_UNADJ",               # est - number of trees per acre, numerator 
    estvar.filter="STATUSCD == 1",    # est - live trees only, numerator
    rowvar="FORTYPCD",         # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    rawdata=TRUE,              # out - return raw data 
    allin1=FALSE               # out - return output with est(pse)
    )   


## Compare estimates and percent standard errors with and without post-stratification
head(tree.strat$est)
head(tree.nostrat$est)


## Number of live trees per acre by species, Wyoming, 2011-2013, with post-stratification
ratio.strat <- modGBratio( 
    GBpopdat=GBpopdat.strat,   # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvarn="TPA_UNADJ",               # est - number of trees per acre, numerator 
    estvarn.filter="STATUSCD == 1",    # est - live trees only, numerator
    rowvar="FORTYPCD",         # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    rawdata=TRUE,              # out - return raw data 
    allin1=FALSE               # out - return output with est(pse)
    )   

## Number of live trees per acre by species, Wyoming, 2011-2013, no post-stratification
ratio.nostrat <- modGBratio( 
    GBpopdat=GBpopdat.nostrat, # pop - population calculations for WY, no post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    estvarn="TPA_UNADJ",               # est - number of trees per acre, numerator 
    estvarn.filter="STATUSCD == 1",    # est - live trees only, numerator
    rowvar="FORTYPCD",         # est - row domain
    row.FIAname=TRUE,          # est - row domain names
    rawdata=TRUE,              # out - return raw data 
    allin1=FALSE               # out - return output with est(pse)
    )   


## Compare estimates and percent standard errors with and without post-stratification
head(ratio.strat$est)
head(ratio.nostrat$est)




## -----------------------------------------------------------------------------

## By estimation unit
## Area of forest land by forest type and stand-size class and Estimation Unit,
##    Wyoming, 2011-2013
##################################################################################
area.unit <- modGBarea(
    GBpopdat=GBpopdat,         # pop - population calculations for WY, post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=FALSE,            # est - sum estimation units to population
    rowvar="FORTYPCD",         # est - row domain
    colvar="STDSZCD",          # est - column domain
    rawdata=TRUE,              # out - return raw data 
    returntitle=TRUE,          # out - return title information
    allin1=TRUE                # out - return output with est(pse)
    )

## Estimate and percent sampling error of estimate (first 6 rows)
head(area.unit$est)

## Unique estimation units
unique(area.unit$est$unit)


## -----------------------------------------------------------------------------

## Get population data for Wyoming, with post-stratification
## Area of forest land by forest type and stand-size class, Wyoming, 2011-2013
area.strat <- modGBarea( 
    cond=WYcond,               # pop - FIA plot/condition data
    tree=WYtree,               # pop - FIA tree data
    pltassgn=WYpltassgn,       # pop - plot assignments
    pltassgnid="CN",           # pop - uniqueid of plots
    unitarea=WYunitarea,       # pop - area by estimation units
    unitvar="ESTN_UNIT",       # pop - name of estimation unit
    strata=TRUE,               # pop - if using post-stratification
    stratalut=WYstratalut,     # strata classes and pixels counts
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    rowvar="FORTYPCD",         # est - row domain
    colvar="STDSZCD",          # est - column domain
    allin1=TRUE                # out - return output with est(pse)
    )   


## Get population data for Wyoming, with no post-stratification
## Area of forest land by forest type and stand-size class, Wyoming, 2011-2013
area.nostrat <- modGBarea( 
    cond=WYcond,               # pop - FIA plot/condition data
    tree=WYtree,               # pop - FIA tree data
    pltassgn=WYpltassgn,       # pop - plot assignments
    pltassgnid="CN",           # pop - uniqueid of plots
    unitarea=WYunitarea,       # pop - area by estimation units
    unitvar="ESTN_UNIT",       # pop - name of estimation unit
    strata=FALSE,              # pop - if using post-stratification
    landarea="FOREST",         # est - forest land filter
    sumunits=TRUE,             # est - sum estimation units to population
    rowvar="FORTYPCD",         # est - row domain
    colvar="STDSZCD",          # est - column domain
    rawdata=TRUE,              # out - return raw data 
    allin1=TRUE                # out - return output with est(pse)
    )   


## Compare estimates and percent standard errors with and without post-stratification
head(area.strat$est)
head(area.nostrat$est)



