\name{anPBpopICE_report}
\alias{anPBpopICE_report}
\title{
	ANALYSIS - Generate a report from ICE core tables for Green-Book custom population.
}
\description{
	Generates a report from a set of ICE core tables based on area of interest.
}
\usage{
anPBpopICE_report(rawfolder, AOInm, T1, T2, outfn.pre, ice.QAQCfn =  NULL, 
	photofn = NULL, outfolder = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawfolder}{ String. Full path name of rawdata folder with estimates. }
  \item{AOInm}{ String. Name of area of interest. }
  \item{T1}{ String. Time 1 description (e.g., YYYY). }
  \item{T2}{ String. Time 2 description (e.g., YYYY). }
  \item{outfn.pre}{ String. Short name of file name. If NULL, using AOInm. }
  \item{ice.QAQCfn}{ String. Name of file with summarize QAQC data
	(See details for file structure). If no file is given, the QAQC section
	is removed from report. }
  \item{photofn}{ String. Path name to photo to add to report. }
  \item{outfolder}{ String. The path of folder to output tables. }
}
\details{
   If variables are NULL, then it will prompt user to input variables.

   
   ice.QAQC - 2 columns (QAQC_check, Result):\cr
	\tabular{ll}{
	\tab PC_self_check\cr
	\tab PC_cross_check\cr
	\tab LC1_2_self_check\cr
	\tab LC1_2_cross_check\cr
	\tab LU1_2_self_check\cr
	\tab LU1_2_cross_check\cr
	}

		
}
\author{
	Tracey S. Frescino
}

\keyword{ data }
