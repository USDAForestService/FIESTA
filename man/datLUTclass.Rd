\name{datLUTclass}
\alias{datLUTclass}
\title{
	Data - Append variable description or class.
}
\description{
	Merge a look-up table to define categories of continuous data in x (e.g., DIACL).
	Adds a variable to x, setting the variable to VARCLNM where: xvar >= MIN and xvar < MAX.
}
\usage{
datLUTclass(x, xvar = NULL, LUT = NULL, minvar = "MIN", maxvar = "MAX", 
	LUTclassnm = NULL, savedata = FALSE, outfolder = NULL, outfn = NULL)
}
\arguments{
  \item{x}{ Data frame or comma-delimited file (*.csv). The data table with variable to classify. }
  \item{xvar}{ String. Name of variable in the data table to join to. }
  \item{LUT}{ Data frame or comma-delimited file (*.csv). Name of the look-up table with collapsed 
	classes. }
  \item{minvar}{ String. Name of variable with minimimum class value (>= minvar). }
  \item{maxvar}{ String. Name of variable with maximun class value (<= maxvar). }  
  \item{LUTclassnm}{ String. Name of variable in LUT with class names. If NULL, a class 
	name is generated from minvar and maxvar with default name equal to 'xvar'CL. }
  \item{savedata}{ Logical. If TRUE, saves data to outfolder as comma-delimited file (*.csv). }
  \item{outfolder}{ String.* The output folder path. If NULL and savedata=TRUE, the 
	outfolder is the working directory. }
  \item{outfn}{ String.* Output file name. If NULL and savedata=TRUE, outfn = datlut_'date'.csv. }

}
\details{
	Use datLUTclass() to prompt for input.

}
\value{
 	\item{xLUT}{ Input data table with look-up table variable(s). }
	\item{LUTclassnm}{ Name of the classified variable. }
	\item{LUT}{ Look-up table with categories. }

	If savedata = TRUE, a comma-delimited file is output to the outfolder as outfn. 
	If outfn = NULL, the name of the file will be datlut_'date'.csv.
}
\author{
	Tracey S. Frescino
}
\note{ 
	The look-up table format must be similar to the following table 
	Set LUTclassnm = VARCLNM.

	\tabular{llll}{
	 	\tab \bold{MIN}	\tab	\bold{MAX} \tab \bold{VARCLNM}  \cr
	 	\tab 1.1	\tab	5.0			\tab 1   	\cr	
	 	\tab 5.1	\tab	10.0			\tab 2 	\cr
	 	\tab 10.1	\tab	15.0			\tab 3 	\cr
	 	\tab 15.1	\tab	20.0 			\tab 4 	\cr
	 	\tab 20.1	\tab	25.0 			\tab 5 	\cr
	 	\tab 25.1	\tab	50.0 			\tab 6 	\cr
	}
	
}
\keyword{ data }

\examples{
	WYtreelut <- datLUTclass(WYtree, xvar="DIA", LUT=ref_diacl2in, LUTclassnm="DIACL2IN")
	names(WYtreelut)
	head(WYtreelut$xLUT)
}
