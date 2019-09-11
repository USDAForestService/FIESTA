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

