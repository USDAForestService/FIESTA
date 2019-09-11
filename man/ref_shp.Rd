\name{ref_shp}
\docType{data}
\alias{ref_shp}
\title{ Reference table - Metadata for shp_* default variables output from DBgetPlots() }
\description{ Data frame with variable names and descriptions }
\usage{ ref_shp }

\format{ A dataframe with 63 rows and 4 columns
	VARIABLE - Variable in plt data frame
	DESCRIPTION - Description of variable in plt data frame
 	TABLE - Table in database where variable originates or if derived
 	SHPEXPORT - Name of variable for exported shapefile (<= 10 characters)
}
\source{ FIA look-up table  }
\references{
	O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer, D.; Wilson, A.M.; Waddell, K.L.;
	Christensen, G.; Conkling, B.L. 2012. The Forest Inventory and Analysis Database: Database
	Description and Users Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture. 
	(http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user manual_5-1-2_p2_07_2012.pdf)
}
\keyword{ datasets }
