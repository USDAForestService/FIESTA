\name{anSApop_report}
\alias{anSApop_report}
\title{
	ANALYSIS - Generate a report small area population.
}
\description{
	Generates a standard report based on area of interest.
}
\usage{
anSApop_report(SApopdatlst, AOInm, pcfilter=NULL, fortypgrpcd=NULL,
	title.ref=NULL, domain="DOMAIN", estnm="JFH", totals=TRUE, outfolder=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SApopdatlst}{ List object. Population data from FIESTA::modGBpop(). }
  \item{AOInm}{ String. Name of area of interest. }
  \item{pcfilter}{ String. Name of variable or string to filter plots or conditions.
	Must be in R syntax (e.g., ADFORCD == 408, COUNTYCD == 1). Optional. }
  \item{fortypgrpcd}{ Integer. A forest type group code to use as focus in the report. }
  \item{title.ref}{ String. Reference information for title and output file names. }
  \item{domain}{ String. Name of domain to use in report tables. }
  \item{estnm}{ String. Name of estimate variable to use in tables. }
  \item{totals}{ Logical. If TRUE, per acre estimates are multiplied by acres. }
  \item{outfolder}{ String. The path of folder to output tables. }
}
\details{
	If variables are NULL, then it will prompt user to input variables.

}
\author{
	Tracey S. Frescino
}

\keyword{ data }
