\name{DBgetCSV}
\alias{DBgetCSV}
\title{
	Database - Gets a comma-delimited (*.csv) file from FIA DataMart. 
}
\description{
	Downloads comma-delimited file (*.csv) or downloads and extracts a compressed csv file (*.zip) from FIA DataMart. Only 1 table can be specified, but multiple states may be included.
}
\usage{
	DBgetCSV(DBtable, states = NULL, ZIP = TRUE, returnDT = FALSE, 
		stopifnull = TRUE, noIDate = TRUE, gui = FALSE)
}
 
\arguments{
  \item{DBtable}{ String. The name of table to download. }
  \item{states}{ String or numeric vector. Name(s) (e.g., "Arizona","New Mexico") or 
	code(s) (e.g., 4, 35) of states to download data. }
  \item{ZIP}{ Logical. If datsource="CSV". If TRUE, compressed comma-separated values (CSV) 
	files (*.zip) are downloaded from FIA DataMart; saved to a temporary space;
	extracted; and deleted from temporary space. If FALSE, CSV files are directly
	downloaded. }
  \item{returnDT}{ Logical. If TRUE, a data table is returned, else, a data frame. }
  \item{stopifnull}{ Logical. If TRUE, stop if table is NULL. }
  \item{noIDate}{ Logical. If TRUE, do not include columns with type IDate. }
  \item{gui}{ Logical. If TRUE, gui windows pop up for parameter selection. }
}
\details{
	Accessibility and download time depends on access and speed of internet connection.
}
\value{
	Returns a data table (returnDT=TRUE), or data.frame (returnDT=FALSE) of downloaded table(s).
 	If more than one state, returned as one table. 
}
\author{
	Tracey S. Frescino
}

\examples{

  \dontrun{

    DBgetCSV("ref_research_station")
  }
}