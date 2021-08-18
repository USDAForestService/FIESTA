\name{DBgetCoords}
\alias{DBgetCoords}
\title{
	Database - Extracts actual plot coordinates from the FIA Oracle database.
}
\description{
	Extracts actual plot coodinates for the most current measurement 
	of each plot in the database.
}
\usage{
DBgetCoords(states = NULL, RS = NULL, invtype = "ANNUAL", 
	evalid = NULL, evalCur = FALSE, evalEndyr = NULL, evalAll = FALSE, 
	measCur = FALSE, measEndyr = NULL, allyrs = FALSE, invyrs = NULL, 
	intensity1 = FALSE, issp = FALSE, returndata = TRUE, 
	savedata = FALSE, outfolder = NULL, out_fmt = "csv", out_dsn = NULL, 
	out_layer = "xyplt", append_layer = FALSE, outfn.pre = NULL, 
	outfn.date = FALSE, overwrite_dsn = FALSE, overwrite_layer=TRUE)
}
\arguments{
  \item{states}{ String or numeric vector. Name (e.g., 'Arizona','New Mexico') or 
	code (e.g., 4, 35) of state(s) for evalid. If all states in one or more FIA Research
	Station is desired, set states=NULL and use RS argument to define RS. }
  \item{RS}{ String vector. Name of research station(s) ('RMRS','SRS','NCRS','NERS','PNWRS').
	Do not use if states is populated. }
  \item{invtype}{ String. Type of FIA inventory to extract ('PERIODIC', 'ANNUAL').  
	Only one inventory type (PERIODIC/ANNUAL) at a time. }
  \item{evalid}{ Integer. Inventory span defining variable. Extract data for a 
	specific evaluation period (See details for more information about FIA 
	Evaluations). }
  \item{evalCur}{ Logical. Inventory span defining variable. If TRUE, extract data 
	for the most current FIA Evalidation for each state. }
  \item{evalEndyr}{ YYYY. Inventory span defining variable. Extract data for 
	the Evaluation(s) ending in the specified evalEndyr(s). If more than one state 
	and different Evaluations by state are desired, input a named list object with 
	evalEndyr by state (e.g., list(Utah=2014, Colorado=2013). }
  \item{evalAll}{ Logical. Inventory span defining variable. If TRUE, extract data 
	for all Evaluations for each state. }
  \item{measCur}{ Logical. Inventory span defining variable. If TRUE, extract plots 
	with most current measurement for state(s). }
  \item{measEndyr}{ Logical. Inventory span defining variable. If TRUE, extract plots 
	with most current measurement for state(s) for years measured in or before measEndyr. }
  \item{allyrs}{ Logical. Inventory span defining variable. If TRUE, extract all annual 
	inventory years in database for each state. }
  \item{invyrs}{ YYYY vector. Inventory span defining variable. Extract data by
	state for the specified inventory year(s) (e.g., c(2000, 2001, 2002)). If more 
	than one state and different inventory years are desired, input a named list object 
	with years labeled by state (e.g., list(Utah=2000:2009, Colorado=c(2002,2003,2005)). }
  \item{intensity1}{ Logical. If TRUE, includes only plots where INTENSITY = 1. }
  \item{issp}{ Logical. If TRUE, returns spatial data as a list object with query. }
  \item{returndata}{ Logical. If TRUE, returns data as a list object with query. }
  \item{savedata}{ Logical. If TRUE, saves data to outfolder as comma-delimited file (*.csv). }
  \item{outfolder}{ String. The output folder path. If NULL and savedata=TRUE or parameters=TRUE 
	or isshp=TRUE, outfolder is the working directory. }
  \item{out_fmt}{ String. File format for output ('csv', 'sqlite','gpkg', 'gdb').
	If out_fmt %in% c('sqlite','gpkg'), RSQLite package must be installed. If
	out_fmt='gdb', arcgisbinding package and R-Bridge must be installed. }
  \item{out_dsn}{ String. Data source name for output. If extension is not
	included, out_fmt is used. Use full path if outfolder=NULL. }
  \item{out_layer}{ String. Name of file, out_fmt = 'csv', or name of layer in 
	out_dsn, if out_fmt != 'csv'. }
  \item{append_layer}{ Logical. If TRUE, appends to existing out_dsn. The out_dsn
	a database or shapefile. If FALSE, the out_dsn will be overwritten if exists. }
  \item{outfn.pre}{ String. The name used for prefix of outfiles (e.g., outfn.pre'_plt*'.) }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile (e.g., outfn_'date'.csv). }
  \item{overwrite_dsn}{ Logical. If TRUE and out_fmt = 'sqlite', 
	the out_dsn is overwritten. }
  \item{overwrite_layer}{ Logical. If TRUE and out_fmt = 'csv', files are overwritten. 
	If out_fmt != 'csv', the layer in database is overwritten. }
}
\note{

	If no parameters are included, the user is prompted for input. If partial parameters, 
	the default parameter values are used for those not specified. This function will 
	not function without a working ODBC connection to FIA's Oracle database or


	\bold{Data Access}

	Access to Oracle is only available through FIA's security policies and select 
	permission must be granted to query Oracle's tables. Contact your local Oracle 
	database manager to grant access. See details for more information.
	
	Access to FIA's Oracle database also requires a compliant ODBC (Open Database 
	Connectivity) connection with the local TNS (Transparent Network Substrate) name. 
	ODBC refers to a database driver on the client computer which translates
	queries from client applications into commands the database understands.
	Use DBtestOracle() to test your connection to the Oracle database. See your local 
	IT or Oracle administrator if connection is unsuccessful. 

	If states intersect more than one FIA Research Station (RS), you must have
	accesss to FS_FIADB::SDS_PLOT to extract coordinates. Include RS unit to use
	regional SDS tables ('FS_NIMS_FIADB_', RS, '.SDS_PLOT')

}
\value{
  fiadat - a list of the following objects:
  \item{xy*_ACTUAL}{ Data frame. XY data from FS_FIADB_NIMS_*.SDS_PLOT. 
	xyCur_ACTUAL - if measCur=TRUE, xy_ACTUAL otherwise. } 
  \item{xyqry}{ String. Query to extract coordinates } 

  If savedata=TRUE, outputs data as out_fmt to outfolder.
  If out_fmt = 'sqlite' and issp = TRUE, the output will be a SpataiLite database. 
    
}
\author{
	Tracey S. Frescino
}

\examples{

  # Most current evaluation and shapefile with public coordinates
  COxylst <- DBgetCoordsO(states="Colorado", measCur=TRUE, RS="RMRS")
  names(COxylst)

  head(COxylst$xyCur_ACTUAL)
  COxylst$xyqry
}


\keyword{data}
