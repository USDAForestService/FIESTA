\name{datPBpnt2pct}
\alias{datPBpnt2pct}
\title{
	Data - Transpose points to plot-level as percent by domain.
}
\description{
	Calculates percent point by plot and domain and transpose to plot level.
}
\usage{
datPBpnt2pct(pnt, uniqueid, tvar, othervars = NULL)
}
\arguments{
  \item{pnt}{ DF/DT or comma-delimited file (*.csv). Point-level table with
	one record per point. If NULL, aggregated point counts must be in pntcnt. }
  \item{uniqueid}{ String. Name of unique identifier of plot in pnt. }
  \item{tvar}{ String. Name of variable to transpose. }
  \item{othervars}{ String vector. Name(s) of plot-level variables to merge with
		transposed data. }
}

\value{

  \item{pltdom.pct}{ Data frame with transposed data. }

}
\author{
	Tracey S. Frescino
}

\keyword{ data }
