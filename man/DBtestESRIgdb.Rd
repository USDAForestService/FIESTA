\name{DBtestESRIgdb}
\alias{DBtestESRIgdb}
\title{
	Database - Test access to an ESRI geodatabase. 
}
\description{
	Checks gdb access (See note below).
}
\usage{
	DBtestESRIgdb(gdbfn = NULL, outfolder = NULL, outfn.pre = NULL, 
		outfn.date = FALSE, overwrite = FALSE, showlist = TRUE,
		returnpath = TRUE)
}
 
\arguments{
  \item{gdbfn}{ String. Name of ESRI geodatabase (*.gdb). }
  \item{outfolder}{ String. Optional. Name of output folder. If NULL, export
	to working directory. }
  \item{outfn.pre}{ String. Prefix for out_dsn. }
  \item{outfn.date}{ Logical. If TRUE, add current date to out_dsn. }
  \item{overwrite}{ Logical. If TRUE, overwrites the geodatabase, if exists. }
  \item{showlist}{ Logical. If TRUE, shows list of tables in database. }
  \item{returnpath}{ Logical. If TRUE, returns full path to gdb file name. 
		If FALSE, returns gdbfn}
}
\author{
	Tracey S. Frescino
}
\note{
	Must have a functioning installation of ArcGIS or environment variables for ArcGIS.
}

\keyword{ data }
