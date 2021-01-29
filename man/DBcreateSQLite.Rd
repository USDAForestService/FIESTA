\name{DBcreateSQLite}
\alias{DBcreateSQLite}
\title{
	Database - Create a new SQLite database. 
}
\description{
	Create a new SQLite database.
}
\usage{
	DBcreateSQLite(SQLitefn = NULL, gpkg = FALSE, dbconnopen = FALSE, 
		outfolder = NULL, outfn.pre = NULL, outfn.date = FALSE, overwrite = FALSE, 
		returnpath = TRUE, stopifnull = FALSE)
}
 
\arguments{
  \item{SQLitefn}{ String. Name of SQLite database (*.sqlite). }
  \item{gpkg}{ Logical. If TRUE, Sqlite geopackage database. }
  \item{dbconnopen}{ Logical. If TRUE, the dbconn connection is not closed. }
  \item{outfolder}{ String. Optional. Name of output folder. If NULL, export
	to working directory. }
  \item{outfn.pre}{ String. Prefix for out_dsn. }
  \item{outfn.date}{ Logical. If TRUE, add current date to out_dsn. }
  \item{overwrite}{ Logical. If TRUE, overwrites the sqlite database, if exists. }
  \item{returnpath}{ Logical. If TRUE, returns full path to SQLite file name. 
	If FALSE, returns SQLitefn}
  \item{stopifnull}{ Logical. If TRUE, and SQLitefn is NULL, stops execution of program.
	If FALSE, returns NULL}
}
\author{
	Tracey S. Frescino
}


\keyword{ data }
