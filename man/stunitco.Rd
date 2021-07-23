\name{stunitco}
\docType{data}
\alias{stunitco}
\title{ SpatialPolygonsDataFrame with FIA state, unit, county codes and names}
\description{ 
Polygon feature class with state and county boundaries defined by Census Bureau, including Federal Information Processing Standards (FIPS) codes. The FIA Survey Unit code and name attributes (UNITCD, UNITNM) were appended to dataset, with joining columns of STATECD and COUNTYCD. 

Derived from cb_2018_us_county_5m. 
STATEFP was converted to numeric and named STATECD
COUNTYFP was converted to numeric and named COUNTYCD
Lookup table for FIA Research Station (REF_RESEARCH_STATION) was downloaded from FIA DataMart on 20191105 (FIADB_1.6.1.00) and joined by STATECD.
A lookup table for UNITCD was created from plot data using unique STATECD, COUNTYCD, UNITCD and joined to table.

Converted to simple feature\cr
Transformed CRS from longlat(EPSG:4269) to Albers (EPSG:5070)\cr
Saved to R object, with compression='xz'

}
\usage{ stunitco}

\format{ 
A SpatialPolygonsDataFrame with 3233 features and 8 attributes 
	RS - FIA Research Station name
	RSCD - FIA Research Station code 
	STATECD - FIPS state code
	STATENM - FIPS state name
	STATEAB - FIPS state abbreviation
	UNITCD - FIA survey unit code
	UNITNM - FIA survey unit name
	COUNTYCD - FIPS county code
	COUNTYNM - FIPS county name
}

\source{ 
Downloaded from the United States Census Bureau on 2019 November 3, format Esri Shapefile
(https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html)
Projection: Geographic (GCS_North_American_1983)
EPSG: 4269}
\keyword{ datasets }

