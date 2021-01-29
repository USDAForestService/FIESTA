\name{datPivot}
\alias{datPivot}
\title{
	Data - Generates a pivot table.
}
\description{
	Generates a pivot table of values by x row and y column.
}
\usage{
datPivot(x, pvar, xvar, yvar, pfun=sum, xfilter = NULL, NAto0 = TRUE, 
	dropNAxvar = TRUE, dropNAyvar = TRUE, pvar.round = 2, savedata = FALSE, 
	outfolder = NULL, outfn = NULL, outfn.date = FALSE, overwrite = TRUE)
}
\arguments{
  \item{x}{ Dataframe. Table with pivot variables. }
  \item{pvar}{ String. The name of the variable for pivot table values. }
  \item{xvar}{ String. The name of the variable for rows. }
  \item{yvar}{ String. The name of the variable for columns. }
  \item{pfun}{ Function. The name of the function to use for pivot table values 
  		(ex. sum, mean, max). }
  \item{xfilter}{ String. A filter to subset the datatable table x before pivoting 
		(ex. "STATUSCD == 1"). }
  \item{NAto0}{ Logical. If TRUE, converts NA values to 0. }
  \item{dropNAxvar}{ Logical. If TRUE, removes columns that are NA. }
  \item{dropNAyvar}{ Logical. If TRUE, removes rows that have NA values. }

  \item{pvar.round}{ Integer. Number to round pvar values to. }
  \item{savedata}{ Logical. If TRUE, writes output data to outfolder. }
  \item{outfolder}{ String. The name of the output folder. If savedata=TRUE, 
		all output saved to the outfolder. If savedata=FALSE, only a text file 
		of input parameters is saved. }
  \item{outfn}{ String. The name of the output file if savedata=TRUE (*.csv). Do not include 
	extension. If NULL, the file will be named pivot_'date'.csv }
  \item{outfn.date}{ Logical. If TRUE, add current date to out_dsn. }
  \item{overwrite}{ Logical. If TRUE, overwrite existing file. }
}

\value{

  \item{ptab}{ Matrix. The pivot table. }

}
\author{
	Tracey S. Frescino
}

\examples{
	datPivot(x=FIESTA::WYcond, pvar="CONDPROP_UNADJ", xvar="FORTYPCD", yvar="STDSZCD")

	datPivot(x=FIESTA::WYtree, pvar="TPA_UNADJ", xvar="SPCD", yvar="STATUSCD", 
		pfun=mean, NAto0=FALSE)

}

\keyword{ data }
