\name{datFreq}
\alias{datFreq}
\title{
	Data - Get frequency table for specified variable(s).
}
\description{
	Generates a frequency table from a data frame, including number of records
	by a specified variable or variables in the data frame with optional
	totals and/or subtotals. 
}
\usage{
datFreq(x, xvar = NULL, total = FALSE, subtotal = FALSE, subtotalcol = NULL, 
	savedata = FALSE, outfolder = NULL, outfn = NULL)
}
\arguments{
  \item{x}{ Data frame or comma-delimited file (*.csv). The table with the variable(s). }
  \item{xvar}{ String (vector).* The name of the variable(s) to summarize. }
  \item{total}{ Logical. If TRUE, a row is added to bottom of table with a total for the 
	whole table. }
  \item{subtotal}{ Logical. If TRUE, a row is added to bottom of each section for subtotals. }
  \item{subtotalcol}{ Logical. If subtotal=TRUE, the column(s) to generate subtotals. }
  \item{savedata}{ Logical. If TRUE, writes output data to outfolder. }
  \item{outfolder}{ String. The name of the output folder, if savedata=TRUE. }
  \item{outfn}{ String. The name of the output file if savedata=TRUE (*.csv). Do not include 
	extension. If NULL, the file will be named Freq_'date'.csv }
}
\details{
	If no parameters, then user is prompted for input. If partial parameters, default 
	parameter values are used.
}
\value{
	\item{freqtable}{ Data frame. The frequency table. }
	If savedata=TRUE, a comma-delimited file of the frequency table is written to the outfolder.
}
\author{
	Tracey S. Frescino
}

\examples{
	tab <- data.frame(cbind(	CONDCLASS=c(1,1,2,1,3,3,3,1,1,1,2,1), 
		FORTYPCD=c(182,184,201,221,221,184,221,182,182,201,182,221)))
	datFreq(x=tab, xvar=c("CONDCLASS", "FORTYPCD"))
	datFreq(x=tab, xvar=c("CONDCLASS", "FORTYPCD"), total=TRUE, subtotal=TRUE)
	datFreq(x=tab, xvar="FORTYPCD")
}

\keyword{ data }
