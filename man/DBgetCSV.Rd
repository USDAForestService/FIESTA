\name{DBgetCSV}
\alias{DBgetCSV}
\title{
	Database - Extracts data table(s) from FIA DataMart. 
}
\description{
	Downloads and extracts compressed comma-delimited file(s) (*.zip) from 
	FIA DataMart (https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html). 
	Only 1 table can be specified, but multiple states may be included.
}
\usage{
	DBgetCSV(DBtable, states = NULL, returnDT = FALSE, stopifnull = TRUE, 
		noIDate = TRUE)
}
 
\arguments{
  \item{DBtable}{ String. Name of table to download. Only 1 table allowed. }
  \item{states}{ String or numeric vector. Name (e.g., "Arizona", "New Mexico") 
	or code (e.g., 4, 35) of states to download data. If NULL, tables
	that are not state-level are downloaded. }
  \item{returnDT}{ Logical. If TRUE, a data table is returned, else, a data frame. }
  \item{stopifnull}{ Logical. If TRUE, stop if table is NULL. }
  \item{noIDate}{ Logical. If TRUE, do not include columns with type IDate. }
}
\value{
	Returns a data table (returnDT=TRUE), or data.frame (returnDT=FALSE) of 
	downloaded table(s). If more than one state, returned as one table. 
}
\details{
	The compressed data files are downloaded from FIA DataMart; saved to a 
	temporary space; extracted and imported; and deleted from temporary space. 
	Accessibility and download time depends on access and speed of internet connection.
}
\author{
	Tracey S. Frescino
}

\examples{

    # Get reference table for FIA research stations
    DBgetCSV(DBtable="ref_research_station")

    # Get plot data for multiple states
    FIAplots <- DBgetCSV("PLOT", c("Georgia", "Utah"))
    table(FIAplots$STATECD)

}