\name{anGBest_evalid}
\alias{anGBest_evalid}
\title{
	Analysis - Generate estimates for all evalid in list.
}
\description{
	Wrapper to get estimates for a list of evalids in a SQLite database.	
}
\usage{
anGBest_evalid(SQLitefn, state, evalidlst = NULL, ppsa = "pop_plot_stratum_assgn")
}
\arguments{
  \item{SQLitefn}{ String. Full pathname to SQLite database with plot data. }
  \item{state}{ String. State for estimates. }
  \item{evalidlst}{ Integer vector. List of evalids for state. } 
  \item{ppsa}{ String. Name of POP_PLOT_STRATUM_ASSGN table in database. }
}
 
\value{
  Estimates.
}
\author{
	Tracey S. Frescino
}
\keyword{ data }

