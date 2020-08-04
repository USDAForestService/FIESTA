\name{spGetSAdoms}
\alias{spGetSAdoms}
\title{
	Spatial - Generate spatial domains for small area estimation.
}
\description{
	Wrapper to get spatial domains for small area estimation. Small area polygons
	area unioned with helper polygons and subset to the extent of the large
	area. The smallbnd and largebnd are optional. If NULL, the helper polygons
	are used as small area domains. A variable named 'AOI' is appended to 
	attribute table to indicate the area of interest, or small area polygons.	
}
\usage{
spGetSAdoms(smallbnd, smallbnd_dsn = NULL, smallbnd.unique, 
	smallbnd.domain = NULL, smallbnd.filter = NULL, smallbnd.stfilter = NULL, 
	smallbnd.ecofilter = NULL, helperbnd = NULL, helperbnd_dsn = NULL, 
	helperbnd.unique = NULL, helperbnd.filter = NULL, largebnd = NULL, 
	largebnd_dsn = NULL, largebnd.unique = NULL, largebnd.filter = NULL, 
	maxbnd = NULL, maxbnd_dsn = NULL, maxbnd.unique = NULL, 
	maxbnd.filter = NULL, helper_autoselect = TRUE, nbrdom.min = NULL, 
	maxbnd.threshold = 30, largebnd.threshold = 20, multiSAdoms = TRUE, 
	showsteps = TRUE, savedata = FALSE, savesteps = FALSE, outfolder = NULL, 
	out_fmt = "shp", out_dsn = NULL, outfn.pre = NULL, outfn.date = FALSE, 
	overwrite_dsn = FALSE, overwrite_layer = TRUE)
}
\arguments{
  \item{smallbnd}{ sf R object or String. Small area of interest boundary. 
	Can be a spatial polygon object, full pathname to a shapefile, or name 
	of a layer within a database. }
  \item{smallbnd_dsn}{ String. Data source name (dsn; e.g., sqlite or shapefile 
	pathname) of smallbnd. The dsn varies by driver. See gdal OGR 
	vector formats (https://www.gdal.org/ogr_formats.html). Optional if smallbnd
	is an R object. } 
  \item{smallbnd.unique}{ String. The attribute in smallbnd that defines unique 
	identifier in smallbnd that defines the unique small area(s). If NULL,
	an attribute is appended to smallbnd attribute table and used as smallbnd.unique, 
	defining one polygon (SMALLAREA="SMALLAREA"). } 
  \item{smallbnd.domain}{ String. A different attribute to use as for modeling domains.
	If NULL, smallbnd.domain=smallbnd.unique. }
  \item{smallbnd.filter}{ String. A filter for smallbnd. Must be R syntax. } 
  \item{smallbnd.stfilter}{ String. A spatial filter for smallbnd to include
	only smallbnd polygons that intersect (or overlap >= 30%) the filter 
	boundary. The filter is based on the stunitco internal R object, with 
	attributes: STATECD, STATENM, UNITCD, UNITNM, COUNTYCD, COUNTYNM. The 
	filter should include one of these attributes and must be R syntax. } 
  \item{smallbnd.ecofilter}{ String. A spatial filter for smallbnd to include
	only smallbnd polygons that intersect (or overlap >= 30%) the filter 
	boundary. The filter is based on the ecomap internal R object, with 
	attributes: PROVINCE, SECTION, SUBSECTION. The filter should include 
	one of these attributes and must be R syntax. } 
  \item{helperbnd}{ sf R object or String. Name of polygon spatial layer 
	delineating helper polygons for small area models. Can be a spatial 
	polygon object, full pathname to a shapefile, or name of a layer within 
	a database. }
  \item{helperbnd_dsn}{ String. Data source name (dsn; e.g., sqlite or shapefile 
	pathname) of helperbnd. The dsn varies by driver. See gdal OGR 
	vector formats (https://www.gdal.org/ogr_formats.html). Optional if helperbnd
	is an R object. } 
  \item{helperbnd.unique}{ String. The attribute in helper polygon layer that 
	defines unique helper polygons. } 
  \item{helperbnd.filter}{ String. A filter for helperbnd. Must be R syntax. }
  \item{largebnd}{ sf R object or String. Name of large area polygon spatial 
	layer, defining the model data extent for building small are models. 
	Can be a spatial polygon object, full pathname to a shapefile, or name of 
	a layer within a database. }
  \item{largebnd_dsn}{ String. Data source name (dsn; e.g., sqlite or shapefile 
	pathname) of largebnd. The dsn varies by driver. See gdal OGR 
	vector formats (https://www.gdal.org/ogr_formats.html). Optional if largebnd
	is an R object. } 
  \item{largebnd.unique}{ String. The attribute in largebnd polygon layer that 
	defines unique large area polygon(s). } 
  \item{largebnd.filter}{ String. A filter for largebnd. Must be R syntax. } 
  \item{maxbnd}{ sf R object or String. Name of polygon spatial layer, 
	defining the maximum model data restraint for adding more helper polygons for 
	building small are models. Can be a spatial polygon object, full pathname 
	to a shapefile, or name of a layer within a database. }
  \item{maxbnd_dsn}{ String. Data source name (dsn; e.g., sqlite or shapefile 
	pathname) of maxbnd. The dsn varies by driver. See gdal OGR 
	vector formats (https://www.gdal.org/ogr_formats.html). Optional if maxbnd
	is an R object. } 
  \item{maxbnd.unique}{ String. The attribute in maxbnd polygon layer that 
	defines unique max restraint area(s). } 
  \item{maxbnd.filter}{ String. A filter for maxbnd. Must be R syntax. } 
  \item{helper_autoselect}{ Logical. If TRUE, the helper boundaries are automatically
	selected based on intersection with maxbnd and/or largebnd and number of
	helperbnds defined by nbrdom.min. } 
  \item{nbrdom.min}{ Integer. Set number for minimum domains for modeling. If NULL,
	all domains within largebnd are selected. }
  \item{maxbnd.threshold}{ Integer. Percent for including additional maxbnds for 
	selecting helperbnds. If multiSAdoms=FALSE, the maxbnd with greatest 
	percentage over the maxbnd.threshold is selected. }
  \item{largebnd.threshold}{ Integer. Percent for including additional largebnds
	for selecting helperbnds. }
  \item{multiSAdoms}{ Logical. If TRUE, and the percent intersect of smallbnd
	with maxbnd is greater than maxbnd.threshold, more than 1 SAdoms will be 
	output in list. }
  \item{showsteps}{ Logical. If TRUE, intermediate steps of selection process are
	displayed. }
  \item{savedata}{ Logical. If TRUE, save SAdoms spatial layer to outfolder. }
  \item{savesteps}{ Logical. If TRUE, save steps spatial intermediate layers. }
  \item{outfolder}{ String. If savedata=TRUE or exportsp=TRUE, name of output folder. 
	If NULL, the working directory is used. }
  \item{out_fmt}{ String. Format for output tables ('shp', 'sqlite', 'gpkg', 'gdb').
	If out_fmt='gdb', must install arcgisbinding package and ArcGIS R-Bridge. }
  \item{out_dsn}{ String. Name of database if out_fmt = c('sqlite', 'gpkg', 'gdb'). }
  \item{outfn.pre}{ String. If out_fmt = "shp", a new folder is created within outfolder
	named outfn.pre. If out_fmt != "shp", a prefix is concatenated to out_dsn, using
	an underscore as separator. }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite_dsn}{ Logical. If TRUE, overwrite dsn. }
  \item{overwrite_layer}{ Logical. If TRUE, overwrite layer(s) in dsn. }
}
 
\value{
  A list object with small area domains. 

  If exportsp=TRUE, the SAdoms spatial object(s) is exported to outfolder, with format
	specified by out_fmt.
}
\author{
	Tracey S. Frescino
}
\note{

  If exportsp=TRUE and out_fmt="shp":\cr
 	The writeOGR (rgdal) function is called. The ArcGIS driver truncates variable 
	names to 10 characters or less. Variable names are changed before export using 
	an internal function (trunc10shp). If Spatial object has more than 1 record, 
	it will be returned but not exported. 
  
  The spTransform (rgdal) method is used for on-the-fly map projection conversion and 
	datum transformation using PROJ.4 arguments. Datum transformation only occurs if 
	the +datum tag is present in the both the from and to PROJ.4 strings. The +towgs84 
	tag is used when no datum transformation is needed. PROJ.4 transformations assume 
	NAD83 and WGS84 are identical unless other transformation parameters are specified. 
	Be aware, providing inaccurate or incomplete CRS 	information may lead to erroneous 
	data shifts when reprojecting. See spTransform help documentation for more details.
}
\keyword{ data }

