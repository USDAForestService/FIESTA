\name{DBtestORACLE}
\alias{DBtestORACLE}
\title{
	Database - Test FIA Oracle database connection.
}
\description{
	Tests the FIA Oracle database connection and return an open channel.
}
\usage{
	DBtestORACLE(dbconnopen = FALSE)
}
\arguments{
  \item{dbconnopen}{ Logical. If TRUE, the open database connection is returned 
		from function. }
}
\value{
	Message if Oracle connection is successful or not. If channelout=TRUE,
	returns an open Oracle channel.
}
\details{
	You must have access to the FIA Oracle database and select permission for the
	table in query. Your computer must also have the required ODBC driver to access
	the FIA database (ex. Oracle in OraClient11g_home1). See your local Oracle 
	administrator if connection is unsuccessful. 
}
\author{
	Tracey S. Frescino
}


\keyword{ data }
