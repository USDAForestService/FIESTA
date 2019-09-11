\name{datPBplotchg}
\alias{datPBplotchg}
\title{
	Data - Generates barplot of photo-based change estimates.
}
\description{
	Generate a bar plot of net change for photo-based estimates of land use / land 
	cover change.
}
\usage{
datPBplotchg(gainloss, CI=95, figTitle = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gainloss}{ Data frame or comma-delimited file (*.csv) - table with gain loss estimates. }
  \item{CI}{ Number. Confidence Interval to include on plot. }
  \item{figTitle}{ String. Title of figure. }
}
\value{
	Outputs barplot to display window.
}
\author{
	Tracey S. Frescino
}
\note{
 If savedata = TRUE, writes a jpg and pdf of barplot to outfolder.

}

\keyword{ data }
