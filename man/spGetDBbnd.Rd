\name{spGetDBbnd}
\alias{spGetDBbnd}
\title{
	Spatial - Extracts data within a given boundary.
}
\description{
	Wrapper to get FIA plots within the boundary population (area of interest):
  	- Intersect with state boundary 
  	- If RMRSonly=TRUE (i.e., actual coordinates), subset states to IW states only
  	- Get FIA plots for intesected states, including tree, and spatial (DBgetPlots)
  	- Merge coordinate data if included separately (e.g., coordinates from SDS)
  	- Clip spatial coordinates and other tables to boundary (spClipPoint)
}
\usage{
spGetDBbnd(bnd_layer, bnd_dsn = NULL, stbnd = NULL, states = NULL, 
	RMRSonly = TRUE, datsource = "ORACLE", SQLitefn = NULL, actual = FALSE, 
	istree = TRUE, coordsfn = NULL, coordsfn.uniqueid = NULL, 
	coordsfn.x = NULL, coordsfn.y = NULL, coordsfn.prj = NULL, 
	coordsfn.datum = NULL, savedata = FALSE, outfolder = NULL, outfn = NULL, 
	outfn.pre = NULL, outfn.date = FALSE, overwrite = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bnd_layer}{ String or S4 object. The name of the boundary 
	spatial polygons layer. The layer name may be a full pathname to a file, 
	the basename to a file, a spatial layer name from a database, or a 
	SpatialPolygons object with a defined projection. }
  \item{bnd_dsn}{ String. The data source name (dsn; i.e., folder or database name)
	of bnd_layer. The dsn varies by driver. See gdal OGR vector formats 
	(https://www.gdal.org/ogr_formats.html). Optional. } 
  \item{stbnd}{ String or S4object. The file name of a polygon vector shapefile or 
	object name of a polygon SpatialPolygon representing state boundaries. If NULL,
	and connected to internet, a SpatialPolygons layer is downloaded from the
	database of global administrative boundaries ('GADM') using the raster package
	getData function. } 
  \item{states}{ String. The name of state(s) for tables (ex. "Vermont", "Utah", etc.) }
  \item{RMRSonly}{ Logical. If TRUE, only states with RMRS FIA research station are retained. }
  \item{datsource}{ String. Source of FIA data ("ORACLE", "CSV", "SQLite"). 
	If datsource="ORACLE", select permission of FIA Oracle tables must be granted to 
	user and an ODBC connection set up on computer. Use DBtestORACLE function to 
	test connection to ORACLE. If datsource="CSV", *.csv or *.zip (if ZIP=TRUE) files 
	are downloaded and extracted from FIA DataMart 
	(http://apps.fs.usda.gov/fia/datamart/datamart.html). If datsource is equal to 
	CSV, the actual coordinates are not available. }
  \item{SQLitefn}{ String. Name of SQLite database. }
  \item{actual}{ Logical. If TRUE, get actual coordinates from FIA database. Must have
	select permission to SDS_PLOT table. }
  \item{istree}{ Logical. Extract tree data from FIA database. }
  \item{coordsfn}{ Data frame, data.table, matrix, or comma-delimited file (*.csv). 
	Includes unique identifier of rows (coordsfn.uniqueid) and x and y coordinates. }
  \item{coordsfn.uniqueid}{ String. Unique plot identifier of coordsfn (e.g., 'CN'). }
  \item{coordsfn.x}{ String. X coordinate for coordsfn. }
  \item{coordsfn.y}{ String. Y coordinate for coordsfn. }
  \item{coordsfn.prj}{ String. Projection for coordsfn (e.g., 'longlat'). }
  \item{coordsfn.datum}{ String. Datum for coordsfn (e.g., 'NAD83'). }
  \item{savedata}{ Logical. If TRUE, saves data to outfolder as comma-delimited file (*.csv). 
	The spatial object is exported as an ArcGIS shapefile (*.shp)
	with variable names truncated to 10 characters or less (See note below). }
  \item{outfolder}{ String. The output folder path. If NULL and savedata=TRUE or parameters=TRUE 
	or isshp=TRUE, outfolder is the working directory. }
  \item{outfn}{ String. Name of the output shapefile/.csv file (Do not include 
	extension). If NULL, default name is datext_'date'. }
  \item{outfn.pre}{ String. The name used for prefix of outfiles (e.g., outfn.pre'_plt*'.) }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
  \item{...}{ Other parameters for DBgetPlots. }
}
\details{
	If variables are NULL, then it will prompt user to input variables.
		
}
\author{
	Tracey S. Frescino
}

\keyword{ data }
