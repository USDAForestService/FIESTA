#' EcoMap SpatialPolygonsDataFrame
#' 
#' Contains regional geographic delineations for analysis of ecological
#' relationships across ecological units. ECOMAP is the term used for a USDA
#' Forest Service initiative to map ecological units and encourage their use in
#' ecosystem-based approaches to forest land conservation and management. It is
#' coordinated at the national and regional levels by USDA Forest Service staff
#' and implemented in cooperation with State forestry agencies and others.
#' ECOMAP mapping criteria are outlined in the National Hierarchical Framework
#' of Ecological Units (https://www.ncrs.fs.fed.us/gla/reports/hierarch-
#' y.htm). The framework systematically divides the country into progressively
#' smaller areas of land and water that have similar physical and biological
#' characteristics and ecological processes.
#' 
#' The EcoMap Provinces feature class contains ecological province polygons
#' attributed with names and descriptions. The EcomapSections 2007 data set
#' describes the ecological sections within the conterminous United States. The
#' EcomapSubections 2007 data set describes the ecological subsections within
#' the conterminous United States.
#' 
#' Converted to simple feature\cr Transformed CRS from longlat(EPSG:4269) to
#' Albers (EPSG:5070)\cr Saved to R object, with compression='xz'
#' 
#' 
#' @name ecomap
#' @docType data
#' @format A SpatialPolygonsDataFrame with 1233 features and 3 attributes
#' PROVINCE - Ecomap Province SECTION - EcoMap Section SUBSECTION - Ecomap
#' Subsection
#' @references Cleland, D.T.; Freeouf, J.A.; Keys, J.E., Jr.; Nowacki, G.J.;
#' Carpenter, C; McNab, W.H. 2007. Ecological Subregions: Sections and
#' Subsections of the Conterminous United States [1:3,500,000] [CD-ROM]. Sloan,
#' A.M., cartog. Gen. Tech. Report WO-76. Washington, DC: U.S. Department of
#' Agriculture, Forest Service.
#' @source Downloaded from the FSGeodata Clearinghouse on 2019 October 30,
#' format ESRI geodatabase (https://data.fs.usda.gov/geodata/edw/datasets.php)
#' @keywords datasets
NULL





#' FIESTA - Forest Inventory Estimation for Analysis
#' 
#' This package will extract data from Forest Inventory and Analysis Oracle
#' database.
#' 
#' The users are intended to be FIA analysts (for now) with personal access to
#' the database. The package will extract plot, condition, tree, and seedling
#' information from the NIMS database for public or regional variables with
#' user-defined filters of plot and/or condition data. The Tree and Seedling
#' table will include variables multiplied by trees per acre.
#' 
#' Most functions can be used independently with external FIA mapped-plot
#' datasets, but check function descriptions to determine necessary variables.
#' 
#' @name FIESTA-package
#' @aliases FIESTA-package FIESTA
#' @docType package
#' @author Tracey S. Frescino Maintainer: Tracey S. Frescino
#' @references
#' 
#' Bechtold, William A.; Patterson, Paul L.; [Editors] 2005. The enhanced
#' forest inventory and analysis program - national sampling design and
#' estimation procedures. Gen. Tech. Rep. SRS-80. Asheville, NC: U.S.
#' Department of Agriculture, Forest Service, Southern Research Station. 85p.
#' 
#' R Development Core Team (2011). R: A language and environment for
#' statistical computing. R Foundation for Statistical Computing, Vienna,
#' Austria. ISBN 3-900051-07-0, URL http://www.R-project.org/.
#' 
#' Woudenberg, S.W.; Conkling, B.L.; O'Connell, B.M.; LaPoint, E.B.; Turner,
#' J.A.; Waddell, K.L.; Boyer, D.; Christensen, G.; Ridley, T. 2011. The Forest
#' Inventory and Analysis Database: Database Description and Users Manual
#' Version 5.1 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5/FIADB_user%20manual_5-1_p2_10_2011.pdf)
#' @keywords package
NULL



#' Reference table - for Automated Land Program National Forests.
#' 
#' National Forest region and names from the USDA S_USA.BasicOwnershipFS
#' (Surface Ownership Parcels) attribute table. An area depicted as surface
#' ownership parcels dissolved on the same ownership classification
#' 
#' 
#' @name ref_ALP
#' @docType data
#' @format A dataframe with 2 columns, R1 and FORESTNAME.
#' @note ## ALP National Forest boundary alp_dsn <-
#' "S_USA.BasicOwnershipFS.gdb" alp_layer <- "BasicOwnershipFS" alp.att <-
#' "FORESTNAME" alp_bnd <- spImportSpatial(dsn=alp_dsn, layer=alp_layer)
#' alp_names <- unique(sf::st_drop_geometry(alp_bnd[, c("REGION",
#' "FORESTNAME")])) alp_names <- alp_names[order(alp_names$REGION,
#' alp_names$FORESTNAME),] alp_names <- alp_names[grepl("National Forest",
#' alp_names$FORESTNAME)==TRUE,] alp_names$FORESTNAME <- sub(" National
#' Forest", "", alp_names$FORESTNAME)
#' 
#' Ownership classes that are not USDA Forest Service were removed from the
#' layer.\cr OWNERCLASSIFICATION = "USDA FOREST SERVICE" subset with following
#' query alpfs <- alpfs[!is.na(alpfs$OWNERCLASSIFICATION) &
#' alpfs$OWNERCLASSIFICATION != 'NON-FS',]
#' @source Downloaded from the USDA Forest Service FSGeodata Clearinghose on
#' 2020 June 3, format ESRI geodatabase
#' (https://data.fs.usda.gov/geodata/edw/datasets.php?xmlKeyword=surface+ownership)
#' 
#' U.S. Forest Service, Automated Land Program (ALP) Publication Date:
#' 2020-05-26.
#' @keywords datasets
NULL





#' Reference table - Metadata for cond default variables output from
#' DBgetPlots()
#' 
#' Data frame with variable names and descriptions
#' 
#' 
#' @name ref_cond
#' @docType data
#' @format A data frame with 61 rows and 3 columns VARIABLE - Variable in cond
#' data frame DESCRIPTION - Description of variable in cond data frame TABLE -
#' Table in database where variable originates or if derived
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source FIA look-up table
#' @keywords datasets
NULL





#' Reference table - for generating tables.
#' 
#' Table conversion factors from English to metric units.
#' 
#' 
#' @name ref_conversion
#' @docType data
#' @format A dataframe with 4 columns: TYPE, ENGLISH, METRIC, CONVERSION.
#' @source Conversion table.
#' @keywords datasets
NULL



#' Reference table - Metadata for plt default variables output from
#' DBgetPlots()
#' 
#' Data frame with variable names and descriptions.
#' 
#' 
#' @name ref_plt
#' @docType data
#' @format A data frame with 43 rows and 3 columns VARIABLE - Variable in plt
#' data frame DESCRIPTION - Description of variable in plt data frame TABLE -
#' Table in database where variable originates or if derived
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source FIA look-up table
#' @keywords datasets
NULL




#' Reference table - Metadata for shp_* default variables output from
#' DBgetPlots()
#' 
#' Data frame with variable names and descriptions
#' 
#' 
#' @name ref_shp
#' @docType data
#' @format A dataframe with 63 rows and 4 columns VARIABLE - Variable in plt
#' data frame DESCRIPTION - Description of variable in plt data frame TABLE -
#' Table in database where variable originates or if derived SHPEXPORT - Name
#' of variable for exported shapefile (<= 10 characters)
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source FIA look-up table
#' @keywords datasets
NULL




#' Reference tables - Code definitions.
#' 
#' Table with species information.
#' 
#' 
#' @name ref_species
#' @docType data
#' @format A dataframe with 8 columns: SPCD, COMMON_NAME, GENUS, SPECIES,
#' EXISTS_IN_NRCS, EXISTS_IN_NERS, EXISTS_IN_PNWRS, EXISTS_IN_RMRS,
#' EXISTS_IN_SRS.
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source FIA look-up tables.
#' @keywords datasets
NULL



#' Reference table - Variable titles.
#' 
#' Table with variable name (VARNM) and associated title (TITLE).
#' 
#' 
#' @name ref_titles
#' @docType data
#' @format A dataframe with 2 columns, VARNM and TITLE.
#' @source Comma-delimited file.
#' @keywords datasets
NULL



#' Reference table - Metadata for tree default variables output from
#' DBgetPlots()
#' 
#' Data frame with variable names and descriptions
#' 
#' 
#' @name ref_tree
#' @docType data
#' @format A data frame with 72 rows and 3 columns VARIABLE - Variable in tree
#' data frame DESCRIPTION - Description of variable in tree data frame TABLE -
#' Table in database where variable originates
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source FIA look-up table
#' @keywords datasets
NULL





#' FIA data. Condition-level data from FIA public database.
#' 
#' FIA condition-level data for the state of Wyoming, inventory years 2011-2012
#' and was downloaded Sept 8, 2017 from FIADB_1.7.0.00.
#' 
#' 
#' @name WYcond
#' @docType data
#' @format A dataframe with 26 columns and 6710 rows.
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source FIA Database.
#' @keywords datasets
NULL





#' FIA data. Plot-level data from FIA public database.
#' 
#' FIA plot-level data for the state of Wyoming, inventory years 2011-2012 and
#' was downloaded Sept 8, 2017 from FIADB_1.7.0.00.
#' 
#' 
#' \tabular{lll}{ \tab \bold{VARIABLE} \tab \bold{DESCRIPTION} \cr \tab CN \tab
#' Unique FIADB identifier \cr \tab PREV_PLT_CN \tab Previous unique FIADB
#' identifier \cr \tab INVYR \tab Inventory year \cr \tab STATECD \tab State
#' code (FIPS) \cr \tab CYCLE \tab Inventory cycle number \cr \tab SUBCYCLE
#' \tab Inventory subcycle number (Do not use subcycle 99 for estimation) \cr
#' \tab UNITCD \tab Survey unit code \cr \tab COUNTYCD \tab County code \cr
#' \tab PLOT \tab Phase 2 plot number (Public) \cr \tab LON_PUBLIC \tab
#' Longitude - fuzzed/swapped (Decimal degrees; NAD83) \cr \tab LAT_PUBLIC \tab
#' Latitude - fuzzed/swapped (Decimal degrees; NAD83) \cr \tab
#' PLOT_NONSAMPLE_REASN_CD \tab Plot nonsampled reason \cr \tab SAMP_METHOD_CD
#' \tab Sample method code \cr \tab SUBP_EXAMINE_CD \tab Subplots examined code
#' \cr \tab MANUAL \tab Manual version number \cr \tab INTENSITY \tab Intensity
#' \cr \tab MEASYEAR \tab Measurement year \cr \tab MEASMON \tab Measurement
#' month \cr \tab MEASDAY \tab Measurement day \cr \tab REMPER \tab
#' Remeasurement period \cr \tab DESIGNCD \tab Plot design \cr \tab P2PANEL
#' \tab Phase 2 panel number \cr \tab SUBPANEL \tab Subpanel number \cr \tab
#' ELEV \tab Elevation (ft) \cr \tab KINDCD \tab Sample kind \cr \tab
#' MORT_TYP_CD \tab Type of annual mortality volume (1:Current annual;
#' 2:Periodic annual) \cr \tab GROW_TYP_CD \tab Type of annual volume growth
#' (1:Current annual; 2:Periodic annual) \cr \tab NF_PLOT_NONSAMPLE_REASN_CD
#' \tab Nonforest sampling status \cr \tab P2VEG_SAMPLING_STATUS_CD \tab P2
#' vegetation sampling status \cr \tab PLOT_STATUS_CD \tab Plot sampling status
#' \cr \tab NF_PLOT_STATUS_CD \tab Nonforest plot sampling status \cr \tab
#' NBRCND \tab DERIVED: Number of conditions for plot \cr \tab NBRCNDFOR \tab
#' DERIVED: Number of different forest type conditions for plot \cr \tab
#' FORNONSAMP \tab DERIVED: Plot status - sampled and nonsampled (Combination
#' of PLOT_NONSAMPLE_REASN_CD and PLOT_STATUS_CD) \cr \tab CCLIVEPLT \tab
#' DERIVED: Percent cover of live trees for plot (LIVE_CANOPY_CVR_PCT *
#' CONDPROP_UNADJ) \cr \tab UNIQUEID \tab DERIVED: Unique identifier for plot
#' location ('Z'+STATECD(2)+UNITCD(2)+COUNTYCD(3)+PLOT(5)) \cr }
#' 
#' @name WYplt
#' @docType data
#' @format A data frame with 24 columns and 6336 rows.
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source FIA national database (FIADB_1.6.0.02), downloaded September 18,
#' 2016.
#' @keywords datasets
NULL





#' FIA data. Plot-level data from FIA public database.
#' 
#' FIA plot-level data for the state of Wyoming, inventory years 2011-2012 and
#' was downloaded Sept 8, 2017 from FIADB_1.7.0.00. It includes strata
#' assignments for FIA plot center location (1:GREEN; 2:BROWN).
#' 
#' 
#' @name WYpltassgn
#' @docType data
#' @format A dataframe with 24 columns and 3047 rows.
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source FIA Database.
#' @keywords datasets
NULL





#' FIA data. Seedling data from FIA public database.
#' 
#' FIA seedling data for the state of Wyoming, inventory years 2011-2012 and
#' was downloaded May 17, 2017 from FIADB_1.7.0.00.
#' 
#' 
#' @name WYseed
#' @docType data
#' @format A dataframe with 10 columns and 1039 rows.
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source FIA Database.
#' @keywords datasets
NULL





#' FIA data. Strata data from FIA public database.
#' 
#' FIA condition-level data for the state of Wyoming, inventory years 2011-2012
#' and was downloaded Sept 8, 2017 from FIADB_1.7.0.00. It includes P1POINTCNT
#' and number of FIA plots by strata (1:GREEN; 2:BROWN) and by estimation unit
#' (ESTN_UNIT).
#' 
#' 
#' @name WYstratalut
#' @docType data
#' @format A dataframe with 6 columns and 39 rows.
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source FIA Database.
#' @keywords datasets
NULL





#' FIA data. Tree-level data from FIA public database.
#' 
#' FIA tree-level data for the state of Wyoming, inventory years 2011-2012 and
#' was downloaded Sept 8, 2017 from FIADB_1.7.0.00.
#' 
#' 
#' @name WYtree
#' @docType data
#' @format A dataframe with 45 columns and 37312 rows.
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source FIA Database.
#' @keywords datasets
NULL





#' FIA data. Acres data from FIA public database.
#' 
#' FIA acres by estimation unit for the state of Wyoming, inventory years
#' 2011-2012 and was downloaded Sept 8, 2017 from FIADB_1.7.0.00.
#' 
#' 
#' @name WYunitarea
#' @docType data
#' @format A data table with 4 columns and 23 rows.
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source FIA Database.
#' @keywords datasets
NULL



#' Zonal data. Zonal means for auxiliary data in counties in Wyoming.
#' 
#' Zonal means and pixel counts for certain auxiliary data in counties in
#' Wyoming. Includes county code variable to distinguish counties, and state
#' code variable to distinguish states. 
#' 
#' 
#' @name WYunitzonal
#' @docType data
#' @format A dataframe with 9 columns and 23 rows.
#' @keywords datasets
NULL

