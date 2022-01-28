## -----------------------------------------------------------------------------
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




## -----------------------------------------------------------------------------
####################################################################################
# Usage:
# DBgetCSV(DBtable, states = NULL, ZIP = TRUE, returnDT = TRUE, stopifnull = TRUE,
#         noIDate = TRUE) 
# 
# Description:
# Downloads *.csv file or downloads and extracts a *.zip file from FIA DataMart.
# (https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html)
# Only 1 table can be specified, but multiple states may be included.
# Value:
# Returns a data table (returnDT=TRUE), or data.frame (returnDT=FALSE).
####################################################################################

## Get plot table for Wyoming
WYplots <- DBgetCSV("PLOT", "Wyoming")
dim(WYplots)

## Get plot table for Wyoming and Utah
WYUTplots <- DBgetCSV(DBtable="PLOT", states=c("Wyoming", "Utah"))
table(WYUTplots$STATECD)

## Get survey table for Wyoming
WYsurvey <- DBgetCSV("SURVEY", "Wyoming")
WYsurvey

## Get ref_unit table
ref_research_station <- DBgetCSV("ref_research_station")
head(ref_research_station)



