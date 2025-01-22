#' FIESTA - Forest Inventory Estimation for Analysis
#' 
#' FIESTA is a research estimation tool for analysts that work with sample-based 
#' inventory data from the U.S. Department of Agriculture, Forest Service, 
#' Forest Inventory and Analysis (FIA) Program. 
#'
#' FIESTA can generate FIA's traditional state-wide estimates while also accommodate: 
#' unique population boundaries, different evaluation time periods, customized 
#' stratification schemes, non-standard variance equations, integration of 
#' multi-scale remotely-sensed data and other auxiliary information, and 
#' interaction with other modeling and estimation tools from CRAN's library of 
#' packages. 
#' 
#' FIESTA contains a collection of functions that can query FIA databases, 
#' summarize and compile plot and spatial data, and generate estimates with 
#' associated sampling errors.
#' 
#' @name FIESTA-package
#' @aliases FIESTA-package FIESTA
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
#' Burrill, E.A., Wilson, A.M., Turner, J.A., Pugh, S.A., Menlove, J.,
#' Christiansen, G., Conkling, B.L., Winnie, D., 2018. Forest Inventory and
#' Analysis Database [WWW Document].  St Paul MN US Dep. Agric. For. Serv.
#' North. Res. Stn.  URL http://apps.fs.fed.us/fiadb-downloads/datamart.html
#' (accessed 3.6.21).
#' @keywords package
"_PACKAGE"


#' FIA data. Plot assignment data from FIA public database.
#' 
#' FIA plot-level stratification assignments for the state of Wyoming, FIA 
#' Evaluation 561301, including inventory years 2011-2013.
#' 
#' 
#' @name WYpltassgn
#' @docType data
#' @format A dataframe with 24 columns and 3047 rows.
#' @references Burrill, E.A.; Wilson, A.M.; Turner, J.A.; Pugh, S.A.; Menlove, J.; 
#' Christiansen, G.; B.L. Conkling, B.L.: David, W. 2018. The Forest Inventory and 
#' Analysis Database: Database description and user guide version 8.0 for Phase 2. 
#. USDA Forest Service. 946 p. Available online at 
#. http://www.fia.fs.fed.us/library/database-documentation/.
#' @source FIA national database (FIADB_1.7.0.00), downloaded September 18, 2016.
#' @keywords datasets
NULL


#' FIA data. Post-stratification data from FIA public database.
#' 
#' FIA stratification data for the state of Wyoming, FIA Evaluation 561301, 
#' including inventory years 2011-2013.
#' 
#' 
#' @name WYstratalut
#' @docType data
#' @format A dataframe with 7 columns and 35 rows.
#' @references Burrill, E.A.; Wilson, A.M.; Turner, J.A.; Pugh, S.A.; Menlove, J.; 
#' Christiansen, G.; B.L. Conkling, B.L.: David, W. 2018. The Forest Inventory and 
#' Analysis Database: Database description and user guide version 8.0 for Phase 2. 
#. USDA Forest Service. 946 p. Available online at 
#. http://www.fia.fs.fed.us/library/database-documentation/.
#' @source FIA national database (FIADB_1.7.0.00), downloaded September 18, 2016.
#' @keywords datasets
NULL


#' FIA data. Acres data from FIA public database.
#' 
#' FIA acres by estimation unit for the state of Wyoming, FIA Evaluation 561301, 
#' including inventory years 2011-2013.
#' 
#' 
#' @name WYunitarea
#' @docType data
#' @format A data table with 5 columns and 23 rows.
#' @references Burrill, E.A.; Wilson, A.M.; Turner, J.A.; Pugh, S.A.; Menlove, J.; 
#' Christiansen, G.; B.L. Conkling, B.L.: David, W. 2018. The Forest Inventory and 
#' Analysis Database: Database description and user guide version 8.0 for Phase 2. 
#. USDA Forest Service. 946 p. Available online at 
#. http://www.fia.fs.fed.us/library/database-documentation/.
#' @source FIA national database (FIADB_1.7.0.00), downloaded September 18, 2016.
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



#' FIA data. Plot-level data from FIA public database.
#' 
#' FIA plot-level data for the state of Wyoming, FIA Evaluation 561301, 
#' including inventory years 2011-2013.
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
#' @format A data frame with 20 columns and 3047 rows.
#' @references Burrill, E.A.; Wilson, A.M.; Turner, J.A.; Pugh, S.A.; Menlove, J.; 
#' Christiansen, G.; B.L. Conkling, B.L.: David, W. 2018. The Forest Inventory and 
#' Analysis Database: Database description and user guide version 8.0 for Phase 2. 
#. USDA Forest Service. 946 p. Available online at 
#. http://www.fia.fs.fed.us/library/database-documentation/.
#' @source FIA national database (FIADB_1.7.0.00), downloaded September 18, 2016.
#' @keywords datasets
NULL



#' FIA data. Condition-level data from FIA public database.
#' 
#' FIA condition-level data for the state of Wyoming, FIA Evaluation 561301, 
#' including inventory years 2011-2013.
#'
#' 
#' @name WYcond
#' @docType data
#' @format A dataframe with 26 columns and 3224 rows.
#' @references Burrill, E.A.; Wilson, A.M.; Turner, J.A.; Pugh, S.A.; Menlove, J.; 
#' Christiansen, G.; B.L. Conkling, B.L.: David, W. 2018. The Forest Inventory and 
#' Analysis Database: Database description and user guide version 8.0 for Phase 2. 
#. USDA Forest Service. 946 p. Available online at 
#. http://www.fia.fs.fed.us/library/database-documentation/.
#' @source FIA national database (FIADB_1.7.0.00), downloaded September 18, 2016.
#' @keywords datasets
NULL


#' FIA data. Tree-level data from FIA public database.
#' 
#' FIA tree-level data for the state of Wyoming, FIA Evaluation 561301, 
#' including inventory years 2011-2013.
#' 
#' 
#' @name WYtree
#' @docType data
#' @format A dataframe with 19 columns and 18380 rows.
#' @references Burrill, E.A.; Wilson, A.M.; Turner, J.A.; Pugh, S.A.; Menlove, J.; 
#' Christiansen, G.; B.L. Conkling, B.L.: David, W. 2018. The Forest Inventory and 
#' Analysis Database: Database description and user guide version 8.0 for Phase 2. 
#. USDA Forest Service. 946 p. Available online at 
#. http://www.fia.fs.fed.us/library/database-documentation/.
#' @source FIA national database (FIADB_1.7.0.00), downloaded September 18, 2016.
#' @keywords datasets
NULL



#' FIA data. Seedling data from FIA public database.
#' 
#' FIA seedling data for the state of Wyoming, FIA Evaluation 561301, 
#' including inventory years 2011-2013.
#' 
#' 
#' @name WYseed
#' @docType data
#' @format A dataframe with 10 columns and 1607 rows.
#' @references Burrill, E.A.; Wilson, A.M.; Turner, J.A.; Pugh, S.A.; Menlove, J.; 
#' Christiansen, G.; B.L. Conkling, B.L.: David, W. 2018. The Forest Inventory and 
#' Analysis Database: Database description and user guide version 8.0 for Phase 2. 
#. USDA Forest Service. 946 p. Available online at 
#. http://www.fia.fs.fed.us/library/database-documentation/.
#' @source FIA national database (FIADB_1.7.0.00), downloaded September 18, 2016.
#' @keywords datasets
NULL



#' FIA data. Subplot data from FIA public database.
#' 
#' FIA subplot-level data for the state of Wyoming, FIA Evaluation 561301, 
#' including inventory years 2011-2013.
#' 
#' 
#' @name WYsubplot
#' @docType data
#' @format A dataframe with 9 columns and 20596 rows.
#' @references Burrill, E.A.; Wilson, A.M.; Turner, J.A.; Pugh, S.A.; Menlove, J.; 
#' Christiansen, G.; B.L. Conkling, B.L.: David, W. 2018. The Forest Inventory and 
#' Analysis Database: Database description and user guide version 8.0 for Phase 2. 
#. USDA Forest Service. 946 p. Available online at 
#. http://www.fia.fs.fed.us/library/database-documentation/.
#' @source FIA national database (FIADB_1.7.0.00), downloaded September 18, 2016.
#' @keywords datasets
NULL


#' FIA data. Subplot condition data from FIA public database.
#' 
#' FIA subplot condition-level data for the state of Wyoming, FIA Evaluation  
#' 561301, including inventory years 2011-2013.
#' 
#' 
#' @name WYsubp_cond
#' @docType data
#' @format A dataframe with 6 columns and 20641 rows.
#' @references Burrill, E.A.; Wilson, A.M.; Turner, J.A.; Pugh, S.A.; Menlove, J.; 
#' Christiansen, G.; B.L. Conkling, B.L.: David, W. 2018. The Forest Inventory and 
#' Analysis Database: Database description and user guide version 8.0 for Phase 2. 
#. USDA Forest Service. 946 p. Available online at 
#. http://www.fia.fs.fed.us/library/database-documentation/.
#' @source FIA national database (FIADB_1.7.0.00), downloaded September 18, 2016.
#' @keywords datasets
NULL


#' FIA data. P2 vegetation structure data from FIA public database.
#' 
#' FIA subplot-level P2 vegetation structure data for the state of Wyoming,  
#' FIA Evaluation 561301, including inventory years 2011-2013.
#' 
#' 
#' @name WYp2veg_subp_structure
#' @docType data
#' @format A dataframe with 6 columns and 96775 rows.
#' @references Burrill, E.A.; Wilson, A.M.; Turner, J.A.; Pugh, S.A.; Menlove, J.; 
#' Christiansen, G.; B.L. Conkling, B.L.: David, W. 2018. The Forest Inventory and 
#' Analysis Database: Database description and user guide version 8.0 for Phase 2. 
#. USDA Forest Service. 946 p. Available online at 
#. http://www.fia.fs.fed.us/library/database-documentation/.
#' @source FIA national database (FIADB_1.7.0.00), downloaded September 18, 2016.
#' @keywords datasets
NULL


#' FIA data. P2 vegetation species data from FIA public database.
#' 
#' FIA subplot-level P2 vegetation species data for the state of Wyoming,  
#' FIA Evaluation 561301, including inventory years 2011-2013.
#' 
#' 
#' @name WYp2veg_subplot_spp
#' @docType data
#' @format A dataframe with 9 columns and 14616 rows.
#' @references Burrill, E.A.; Wilson, A.M.; Turner, J.A.; Pugh, S.A.; Menlove, J.; 
#' Christiansen, G.; B.L. Conkling, B.L.: David, W. 2018. The Forest Inventory and 
#' Analysis Database: Database description and user guide version 8.0 for Phase 2. 
#. USDA Forest Service. 946 p. Available online at 
#. http://www.fia.fs.fed.us/library/database-documentation/.
#' @source FIA national database (FIADB_1.7.0.00), downloaded September 18, 2016.
#' @keywords datasets
NULL

