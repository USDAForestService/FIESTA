\name{anGBpop_report}
\alias{anGBpop_report}
\title{
	ANALYSIS - Generate a report for Green-Book custom population.
}
\description{
	Generates a standard report based on area of interest.
}
\usage{
anGBpop_report(GBpopdat, AOInm, photofn = NULL, photo_author = NULL, 
	EVALIDator_match = FALSE, pcfilter = NULL, spcd = 746,
	title.ref = NULL, outfolder = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{GBpopdat}{ List object. Population data from FIESTA::modGBpop(). }
  \item{AOInm}{ String. Name of area of interest. }
  \item{photofn}{ String. Path name to photo to add to report. }
  \item{photo_author}{ String. Name of photo author (Optional). }
  \item{EVALIDator_match}{ Logical. If TRUE, estimates match EVALIDator. }
  \item{pcfilter}{ String. Name of variable or string to filter plots or conditions. 
	Must be in R syntax (e.g., ADFORCD == 408, COUNTYCD == 1). Optional. }
  \item{spcd}{ Integer. A species code to use as focus species of the report. 
	Multiple codes are accepted if the focus species is a combined group. 
	Default is 746 (quaking aspen). }
  \item{title.ref}{ String. Reference information for title and output file names. }
  \item{outfolder}{ String. The path of folder to output tables. }
}
\details{
	If variables are NULL, then it will prompt user to input variables.
		
}
\examples{
 
  ## Calculate population data for Utah, area evaluations 2015-2019
  GBpop <- anGBpop_eval(evalEndyrlst=2019, states="Utah", isseed=TRUE, 
	evalType="VOL")[[1]]

  ## Generate report for stat of Utah
  anGBpop_report(GBpop, AOInm="Utah", EVALIDator_match=TRUE)

  ## Change focus species to Engelmann spruce/subpalpine fir
  anGBpop_report(GBpop, AOInm="Utah", EVALIDator_match=TRUE, spcd=c(19,93))


  ## Get data for WY Bighorn administrative boundary using 
  WYbhfn <- system.file("extdata", "sp_data/WYbighorn_adminbnd.shp", package="FIESTA")
  fornffn <- system.file("extdata", "sp_data/WYbighorn_forest_nonforest_250m.tif", package="FIESTA")
  photofn <- system.file("rmd", "bighorn.jpg", package="FIESTA")

  ## Get population data for generating GB estimates
  GBpopbh <- anGBpop(WYbhfn, datsource="datamart", isseed=TRUE, popType="VOL", strat_layer=fornffn)
  names(GBpopbh)

  ## Generate report for Bighorn National Forest
  anGBpop_report(GBpopbh$GBpopdat, AOInm="Bighorn National Forest", EVALIDator_match=FALSE,
	photofn=photofn, photo_author="Tracey S. Frescino")

}
\author{
	Tracey S. Frescino
}

\keyword{ data }
