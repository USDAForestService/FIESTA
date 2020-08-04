\name{DBgetSQLite}
\alias{DBgetSQLite}
\title{
	Database - Queries a SQLite database table. 
}
\description{
	Extracts and queries data from a SQLite (*.sqlite) database (Note: must use SQL syntax).
}
\usage{
	DBgetSQLite(states = NULL, outfolder = NULL)
}
 
\arguments{
  \item{states}{ String. Vector of one or more state names. }
  \item{outfolder}{ String. The output folder path. If NULL, outfolder is the working directory. }
}

\value{
	Returns nothing.
}
\author{
	Tracey S. Frescino
}


\keyword{ data }
