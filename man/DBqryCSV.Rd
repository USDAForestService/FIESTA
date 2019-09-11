\name{DBqryCSV}
\alias{DBqryCSV}
\title{
	Database - Queries FIA Online Database. 
}
\description{
	Extracts and queries data from FIA's online DataMart, either CSV or ZIP files 
	(Note: must use SQL syntax).
}
\usage{
	DBqryCSV(sql, ZIP = TRUE, states = NULL, sqltables = NULL)
}
 
\arguments{
  \item{sql}{ String. A sql query. Must be appropriate sql syntax. }
  \item{ZIP}{ Logical. If TRUE, compressed comma-separated values (CSV) 
	files (*.zip) are downloaded from FIA DataMart; saved to a temporary space;
	extracted; and deleted from temporary space. If FALSE, CSV files are directly
	downloaded. }
  \item{states}{ String vector. Name of state(s) in query. If not by state, set to NULL.}
  \item{sqltables}{ String vector. Name of table(s) in sql statement to download. 
	Sqltables must match exactly (case-sensitive) the tables in the sql statement. }
}
\value{
	Returns a data frame from resulting query.
}
\author{
	Tracey S. Frescino
}

\examples{

  \dontrun{
    # QUERY1: Number of plots by inventory year for the state of Wyoming
    sql1b <- "select INVYR, count(*) AS NBRPLOTS 
          from plot 
          where statecd=56 group by INVYR"
    DBqryCSV(sql=sql1b, states="Wyoming", sqltables="plot")
  }
}

\keyword{ data }
