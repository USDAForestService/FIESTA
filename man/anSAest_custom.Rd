\name{anSAest_custom}
\alias{anSAest_custom}
\title{
	Analysis - Generate unit-level and area-leve estimates for small areas.
}
\description{
	Wrapper to get plot data, stratification data and estimates within
	a custom-defined boundary.	
}
\usage{
anSAest_custom(SApopdat, esttype = "TREE", SApackage = "JoSAE", SAmethod = "unit", 
	landarea = "FOREST", pcfilter = NULL, estvarlst = NULL, 
	tfilterlst = "live", showsteps = FALSE, savedata = FALSE, savemultest = FALSE, 
	outfolder = NULL, outfn.pre = NULL, outfn.date = FALSE, multest_outfolder = NULL,
	multest_fmt = "sqlite", multest_dsn = "SAmultest", multest.append = FALSE, 
	multest.AOIonly = TRUE, overwrite = FALSE, barplot.compare = FALSE, 
	smallbnd.att = NULL, title.ref = NULL, save4testing=FALSE, ...)
}

\arguments{
  \item{SApopdat}{ List. Population data objects returned from modSApop(). }
  \item{esttype}{ String. Type of estimation ("AREA", "TREE"). }
  \item{SApackage}{ String. small area package to use ('JoSAE', 'sae') }
  \item{SAmethod}{ String. small area method to use ('unit', 'area') }
  \item{landarea}{ String. The sample area filter for estimates ('ALL', 'FOREST', 'TIMBERLAND'). 
	If landarea=FOREST, filtered to COND_STATUS_CD = 1; If landarea=TIMBERLAND, filtered 
	to SITECLCD in(1:6) and RESERVCD = 0. }
  \item{pcfilter}{ String. A filter for plot or cond attributes (including pltassgn). 
	Must be R logical syntax. }
  \item{estvarlst}{ String vector. Name of the tree estimate variable(s). }
  \item{tfilterlst}{ String vector. Tree filter ("Live", "Dead"). }
  \item{showsteps}{ Logical. If TRUE, clipped points are displayed.
	If savedata=TRUE, the display is saved as *.jpg to outfolder. }
  \item{savedata}{ Logical. If TRUE, saves table(s) to outfolder. }
  \item{savemultest}{ Logical. If TRUE, saves multest table to multest_outfolder. }
  \item{outfolder}{ String. The outfolder to write files to. If NULL, files are 
	written to working directory, or if gui, a window to browse. }
  \item{outfn.pre}{ String. A prefix for outfile name, if savedata=TRUE. }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{multest_outfolder}{ String. The outfolder to write multest data to. If 
	NULL, saves to outfolder. }
  \item{multest_fmt}{ String. Format of multest output ('sqlite', 'csv', 'gpkg'). }
  \item{multest_dsn}{ String. Name of database if multest_fmt = c('sqlite', 'gpkg'). }
  \item{multest.append}{ Logical. If TRUE, appends multest dataframe to output. }
  \item{multest.AOIonly}{ Logical. If TRUE, save only AOI data. }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
  \item{barplot.compare}{ Logical. If TRUE, generates barplot comparing multest. }
  \item{smallbnd.att}{ String. Name of small attribute to include in output estimate table. }
  \item{title.ref}{ String. Reference string for title. }
  \item{save4testing}{ Logical. If TRUE, saves intermediate data as R objects 
	in outfolder. }
  \item{...}{ Parameters for anSApop_ecomap. }
}
 
\value{
  Estimate.
}
\author{
	Tracey S. Frescino
}
\keyword{ data }

