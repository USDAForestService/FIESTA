FIESTA_2.4.0 (Release date: 2019-05-23)
==============


Changes:

1. Added 2 new packages in Suggests 
DBI, RSQLite

2. Changed all input parameters of vector spatial files from 1 parameter to 2 parameters
This change will allow flexibility for different inputs of vector layers. The past options were only for shapefiles or objects. Now we can use options such as GeoPackages or SQLite databases.



FIESTA_2.4.1 (Release date: 2019-07-11)
==============

1. Model-Assisted estimation module is functioning similar to Green-Book Module,
	with similar format of output.



FIESTA_2.4.2 (Release date: 2019-09-09)
==============

1. Changed Oracle database connections and queries from using RODBC package to DBI. 
	DBI package is more versatile for querying Oracle and spatial databases.

2. Added sf package to list in anticipation to use for vector spatial manipulations.

3. Fixed bugs in anGBest_core()



FIESTA_3.0.0 (Release date: 2019-04-09)
==============

1. Removed default connection to Oracle database (creating FIESTAO for FIA use only)

2. Removed RODBC from package Suggests (in DESCRIPTION file)

3. Added capability of writing tables and spatial layers to a SQLite database or geopackage,
	or geodatabase (if arcgisbinding package is installed).

3. Converted sp* functions from using class Spatial* to class sf objects.

4. Switched input of spatial object to dsn instead of layer and switched order of parameters.

4. Changed name of spReprojectSpatial function to spReprojectVector.

5. Changed pltstrat to pltassgn


Changes to functions:
spMakeSpatialPoints 
	- changed parameters: x to xvar; y to yvar
	- removed parameters: EPSGCD and prj4str
	- added parameter: crs (for EPSG or PROJ.4 string inputs) 

DBgetPlots - 
	- spconddat 	- plot-level, condition data (for displaying condition attributes)
	- spxy		- spatial xy coordinates (PLT_CN, x, y)



FIESTA_3.0.0 (Release date: 2019-04-09)
==============
