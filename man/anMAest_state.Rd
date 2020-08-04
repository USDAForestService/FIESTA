\name{anMAest_state}
\alias{anMAest_state}
\title{
	Analysis - Generate estimates for state boundaries.
}
\description{
	Wrapper to get estimates using Model-assisted methods - generalized
		regression estimator (GREG).	
}
\usage{
anMAest_state(state = NULL, bycounty=FALSE, SQLitefn, plt.filter = NULL, 
	landarea = "FOREST", cond.filter = NULL, esttype = "TREE", 
	estvar = NULL, estvar.filter = NULL, rowvar = NULL, rastlst.cont = NULL, 
	rastlst.cont.name = NULL, rastlst.cat = NULL, rastlst.cat.name = NULL, 
	rastlst.cat.NODATA = 0, divideby = NULL, savedata = FALSE, 
	savexy = FALSE, outfolder = NULL, outfn.pre = NULL, outfn.date = FALSE, 
	overwrite = TRUE, MAdata = NULL)
}
\arguments{
  \item{state}{ Name of state boundary. }
  \item{bycounty}{ Logical. If TRUE, get estimates by county and sum to state. }
  \item{SQLitefn}{ String. Name of SQLite database with plot data. }
  \item{plt.filter}{ String A filter for plot-level data (e.g., "INVYR == 2012"). } 
  \item{landarea}{ String. The sample area filter for estimates ('ALL', 'FOREST', 'TIMBERLAND'). 
	If landarea=FOREST, filtered to COND_STATUS_CD = 1; If landarea=TIMBERLAND, filtered 
	to SITECLCD in(1:6) and RESERVCD = 0. }
  \item{cond.filter}{ String. A filter for the cond. Must be R syntax. }
  \item{esttype}{ String. Type of estimation ("AREA", "TREE", "RATIO"). }
  \item{estvar}{ String. Name of the tree estimate variable. }
  \item{estvar.filter}{ String. A tree filter for the estimate variable. Must be R syntax 
	(e.g,. "STATUSCD == 1"). }
  \item{rowvar}{ String. Name of row domain variable in cond or tree. If rowvar=NULL,
	only total estimates are output. }
  \item{rastlst.cont}{ String vector or list. Continuous rasters (file names 
	or raster objects). }
  \item{rastlst.cont.name}{ String vector. Output names for continuous rasters. 
	Optional. If NULL, name of raster is default or name+'_'+layer number 
	for multi-band layers. }
  \item{rastlst.cat}{ String vector or list. Categorical rasters (file names 
	or raster objects). }
  \item{rastlst.cat.name}{ String vector. Output names for categorical rasters. }
  \item{rastlst.cat.NODATA}{ Numeric. NODATA value for continuous rasters. These
	values will be removed from output strata table. If 1 number, the same
	value will be used for all categorical rasters. If more than 1 number, 
	the number of values must be equal to the number of rasters in rastlst.cat. }
  \item{divideby}{ String. Conversion number for output ('hundred', 'thousand', 'million'). }
  \item{savedata}{ Logical. If TRUE, the input data with extracted values are
	saved to outfolder. }
  \item{savexy}{ Logical. If TRUE, saves xy coordinates to outfolder. }
  \item{outfolder}{ String. If savedata=TRUE or exportshp=TRUE, name of output folder. 
	If NULL, the working directory is used. }
  \item{outfn.pre}{ String. Add a prefix to output name (e.g., "01"). }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile 
	(e.g., outfn_'date'.csv). }
  \item{overwrite}{ Logical. If TRUE, overwrite existing files. }
  \item{MAdata}{ R list object. MA plot data and model data from anMAest_data(). }
}
 
\value{
  Estimate.
}
\author{
	Tracey S. Frescino
}
\note{
  
  The spTransform (rgdal) method is used for on-the-fly map projection conversion and 
	datum transformation using PROJ.4 arguments. Datum transformation only occurs if 
	the +datum tag is present in the both the from and to PROJ.4 strings. The +towgs84 
	tag is used when no datum transformation is needed. PROJ.4 transformations assume 
	NAD83 and WGS84 are identical unless other transformation parameters are specified. 
	Be aware, providing inaccurate or incomplete CRS 	information may lead to erroneous 
	data shifts when reprojecting. See spTransform help documentation for more details.
}
\keyword{ data }

