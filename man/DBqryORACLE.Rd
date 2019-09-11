\name{DBqryORACLE}
\alias{DBqryORACLE}
\title{
	Database - Queries FIA Oracle Database. 
}
\description{
	Extracts and queries data from FIA's Oracle Database (Note: must use SQL syntax).
}
\usage{
	DBqryORACLE(sql, dbconn=NULL, dbconnopen=FALSE)
}
 
\arguments{
  \item{sql}{ String. A sql query. Must be appropriate sql syntax. }
  \item{dbconn}{ Database connection. The connection information to the FIA Oracle database. }
  \item{dbconnopen}{ Logical. If TRUE, the dbconn connection is not closed. }
}
\details{
	You must have select permission for the tables in the query and the tables 
	must include the ORACLE schema as a prefix to each table.
}
\value{
	Returns a data frame from resulting query.
}
\author{
	Tracey S. Frescino
}


\keyword{ data }
