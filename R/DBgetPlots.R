#' @title
#' Database - Extracts inventory plot data from FIA DataMart.
#' 
#' @description 
#' Extracts data from FIA's online publicly-available DataMart
#' (https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html).
#' 
#' @details 
#' \bold{FIA forest land definition}
#' 
#' \emph{Current}\cr Forested plots include plots with >= 10 percent cover (or
#' equivalent stocking) by live trees of any size, including land that formerly
#' had such tree cover and that will be naturally or artificially regenerated.
#' To qualify, the area must be >= 1.0 acre in size and 120.0 feet wide (See
#' Burrill et al. 2018).
#' 
#' *ACI (All Condition Inventory)*\cr RMRS National Forest plots. For nonforest
#' conditions that have been visited in the field (NF_SAMPLING_STATUS_CD = 
#' if trees exist on the condition, the data exist in the tree table. If you do
#' not want these trees included, ACI=FALSE. This will filter the data to only
#' forested conditions (COND_STATUS_CD = 1)
#' 
#' *Nevada*\cr In 2016, the population area of Nevada changed to exclude the
#' large restricted area owned by Department of Defense (Area 51) from the
#' sample. Prior to 2016, the plots within this area were observed using aerial
#' photos and if they were definitely nonforest the plots were entered in the
#' database with nonforest information.  If they were observed as forested or
#' potentially forested, they were given a PLOT_STATUS_CD=3 because they were
#' Denied Access. From 2016 on, all plots within this area are removed from the
#' sample, and thus, removed from database.
#' 
#' \bold{FIA DataMart Data}
#' 
#' FIA data available on FIA DataMart include the following information.\cr
#' \tabular{ll}{ 
#' \tab - the PLOT variable is renumbered.\cr 
#' \tab - the LON/LAT coordinates are fuzzed & swapped.\cr 
#' \tab - the OWNERCD variable is based on fuzzed & swapped locations.\cr 
#' \tab - ECOSUBCD, CONGCD, ELEV, and EMAP_HEX are GIS-extracted values 
#' based on fuzzed & swapped locations.\cr 
#' \tab - For annual data, forested plots represent the current definition 
#' of >= 10 percent cover...\cr 
#' \tab - For periodic data, forested plots are defined by a definition 
#' of Other Wooded Land (OWL), including >= 5 percent cover...\cr
#' }
#' 
#' \bold{FIA Evaluations}
#' 
#' An evaluation is a group of plots within the FIA database that is used for
#' generating population estimates, representing different inventory spans of
#' data with different stratification or area adjustments. Each evaluation is
#' determined by the type of estimation (Type) including: area and tree
#' estimates; growth, removal, and mortality estimates; and area change
#' estimates (EVAL_TYPE). These plots are identified by an evalid, which is a
#' unique identifier in the format of a 2-digit State code, a 2-digit year
#' code, and a 2-digit evaluation type code. For example, EVALID '491601'
#' represents the Utah 2016 evaluation for current area estimates.
#' 
#' \bold{FIA Evaluation Types}
#' 
#' Define one or more Evaluation Type for Cur=TRUE or Endyr=YYYY. An
#' Evaluation type is used to identify a specific set of plots for a particular
#' response that can be used to a make a statistically valid sample-based
#' estimate. If Type='CURR', the evaluation includes all sampled and
#' nonsampled plots or plots that were missed in an inventory year.
#' 
#' Regional differences may occur on how missed plots are represented in a FIA
#' Evaluation.  For example, RMRS Evaluations are static; missed plots are
#' included in an Evaluation as nonsampled, and when measured, are included in
#' a following Evaluation.  Therefore, the number of nonsampled plots in
#' previous Evaluations may change, depending on when missed plot are measured.
#' In the PNW Research Station, plots are brought forward to replace missed
#' plots in an evaluation, depending on the Type.
#' 
#' EVAL_TYP\cr 
#' \tabular{llll}{ 
#' \tab \bold{EVALIDCD} \tab \bold{EVAL_TYP} \tab \bold{Description}\cr 
#' \tab 00 \tab EXPALL \tab All area\cr 
#' \tab 01 \tab EXPVOL/EXPCURR \tab Area/Volume\cr 
#' \tab 03 \tab EXPCHNG/EXPGROW/EXPMORT/EXPREMV \tab Area Change/GRM\cr 
#' \tab 07 \tab EXPDWM \tab DWM\cr 
#' \tab 08 \tab EXPREGEN \tab Regeneration\cr 
#' \tab 09 \tab EXPINV \tab Invasive\cr \tab 10 \tab EXPP2VEG \tab Veg profile\cr 
#' \tab 12 \tab EXPCRWN \tab Crown\cr }
#' 
#' \bold{Inventory span defining variables}
#' 
#' Data can be extracted using FIA Evaluations or a custom-defined Evaluation
#' for one or more states, one or more FIA Research Stations (RS), or all
#' available states in database (states=NULL, RS=NULL).
#' 
#' *FIA Evaluation (eval=FIA)*\cr
#' \tabular{lll}{ \tab \bold{eval_option} \tab \bold{Description}\cr
#' \tab evalid \tab Specified FIA EVALID (e.g., 491801)\cr
#' \tab Cur \tab Most current FIA Evaluation\cr
#' \tab Endyr \tab End year of an FIA Evaluation (e.g., 2018)\cr
#' \tab All \tab All evaluations in database\cr
#' \tab Type \tab Type of FIA Evaluation (response)\cr }
#'
#' *Custom evaluation (eval="custom")*\cr
#' \tabular{lll}{ \tab \bold{eval_option} \tab \bold{Description}\cr
#' \tab Cur \tab Most current measurement of plot in database\cr
#' \tab Endyr \tab Most current measurement of plot in database in or
#' before year\cr
#' \tab All \tab All years for invtype (ANNUAL or PERIODIC or BOTH)\cr
#' \tab Type \tab Type of custom Evaluation (response)\cr 
#' \tab invyrs \tab Specified inventory years (e.g., 2015:2018)\cr }
#' 
#' \bold{Spatial data}
#' 
#' If issp=TRUE, an sf spatial object of plot-level attributes is generated
#' from public coordinates, with NAD83 Geographic Coordinate Reference System.
#' 
#' *Exporting*\cr If savedata=TRUE and out_fmt="shp", the spatial object is
#' exported to the outfolder using the ESRI Shapefile driver. The driver
#' truncates variable names to 10 characters or less. Variable names are
#' changed using an internal function.  The name changes are written to a csv
#' file and saved to the outfolder (shpfile_newnames.csv).
#' 
#' *spcond*\cr Only one condition per plot is used for spatial representation
#' of condition attributes. IF CONDID1=TRUE, condition 1 is selected. If
#' CONDID1=FALSE, the condition is selected based on the following criteria. A
#' column named CONDMETHOD is added to the attribute table to show the method
#' and steps used, identified by the abbreviation in parentheses.
#' 
#' \tabular{ll}{ 
#' \tab (1) minimum COND_STATUS_CD (_ST)\cr 
#' \tab (2) maximum condition proportion (_CP)\cr 
#' \tab (3) maximum live_canopy_cvr_pct (_CC)\cr
#' \tab (4) minimum STDSZCD (_SZ)\cr 
#' \tab (5) minimum CONDID (_C1)\cr }
#' 
#' \bold{Derived Variables}
#' 
#' If defaultVars=TRUE, the following derived variables are calculated after
#' extracting data from the FIA database.
#' 
#' Plot-level variables:\cr 
#' \tabular{ll}{ 
#' \tab NBRCND - Number of conditions on plot, including nonsampled conditions 
#' (COND_STATUS_CD = 5)\cr 
#' \tab NBRCNDSAMP - Number of sampled conditions on plot.\cr 
#' \tab NBRCNDFOR - Number of sampled forested conditions on plot.\cr 
#' \tab NBRCNDFTYP - Number of sampled forested conditions with different 
#' forest types on plot.\cr 
#' \tab NBRCNDFGRP - Number of sampled forested conditions with different forest
#' type groups on plot.\cr 
#' \tab CCLIVEPLT - Percent live canopy cover of condition aggregated to plot-level 
#' (LIVE_CANOPY_CVR_PCT * CONDPROP_UNADJ).\cr 
#' \tab PLOT_ID - Unique Identifier for a plot ('ID' + STATECD(2) + UNITCD(2) + 
#' COUNTYCD(3) + PLOT(5)).  This variable can be used to identify multiple records 
#' for each measurement of plot.\cr }
#' 
#' Condition-level variables:\cr 
#' \tabular{ll}{ 
#' \tab FORTYPGRP - TYPGRPCD merged to FORTYPCD\cr 
#' \tab FLDTYPGRP - TYPGRPCD merged to FLDTYPCD\cr 
#' \tab FORNONSAMP - Combination of PLOT_STATUS_CD and PLOT_NONSAMPLE_REASN_CD\cr
#' \tab QMD - Quadratic Mean Diameter\cr }
#' 
#' Tree-level variables:\cr 
#' \tabular{ll}{ 
#' \tab BA - the basal area of a tree (BA = DIA * DIA * 0.005454)\cr }
#' 
#' \tabular{ll}{ 
#' \tab TREE AGE Notes:\cr 
#' \tab - Available for live timber and woodland trees in the following states: 
#' AZ,CO,ID,MT,NV,UT,OR,WA.\cr 
#' \tab - BHAGE - Breast height age (4.5' above ground) of timber trees.\cr 
#' \tab - PNW - one tree is sampled for each species, within each crown class, 
#' and for each condition class present on plot. Age of saplings (<5.0" DIA) 
#' may be aged by counting branch whorls above 4.5ft. No timber hardwood species 
#' other than red alder are bored for age.\cr 
#' \tab - RMRS - one tree is sampled for each species and broad diameter 
#' class present on plot.\cr }
#' 
#' \tabular{ll}{ \tab DRYBIO Notes:\cr 
#' \tab DRYBIO_AG - Aboveground oven-dry biomass, in pounds (DRYBIO_AG = 
#' (DRYBIO_BOLE + DRYBIO_STUMP + DRYBIO_TOP + DRYBIO_SAPLING + DRYBIO_WDLD_SPP).\cr 
#' \tab - Available for both timber and woodland species, live trees >= 1.0" 
#' DIA and dead trees >= 5.0" DIA. Summed dry biomass of the top, bole, and 
#' stump of a tree, excluding foliage based on component ratio method 
#' (Heath and others, 2009).\cr 
#' \tab - DRYBIO_BOLE - dry biomass of sound wood in live and dead trees, 
#' including bark, from a 1-foot stump to a min 4-inch top DIA of central stem 
#' (Calculated for timber trees >= 5.0" DIA).\cr 
#' \tab - DRYBIO_STUMP - dry biomass in the tree stump, including the portion 
#' of the tree from the ground to the bottom of merchantable bole, 1-foot 
#' (Calculated for live and dead trees >= 5.0" DIA).\cr 
#' \tab - DRYBIO_TOP - dry biomass in the top of the tree, including the 
#' portion of the tree above merchantable bole, 4-inch top, and all branches, 
#' excludes foliage (Calculated for live and dead trees >= 5.0" DIA).\cr 
#' \tab - DRYBIO_SAPLING - dry biomass of saplings, including aboveground 
#' portion, excluding foliage, of live timber trees >=1.0" and <5.0" DIA.\cr 
#' \tab - DRYBIO_WDLD_SPP - dry biomass of woodland trees, live or dead, 
#' including the aboveground portion, excluding foliage, the top of
#' the tree above 1.5" DIA, and a portion of the stump from ground to DRC
#' (Calculated for woodland trees >= 1.0" DIA.\cr }
#' 
#' ABOVEGROUND CARBON ESTIMATES (IN POUNDS)\cr 
#' Available for both timber and woodland species, live trees >= 1.0" DIA 
#' and dead trees >= 5.0" DIA. Calculated as 1/2 of the aboveground 
#' estimates of biomass: \cr CARBON_AG = 0.5 * (DRYBIO_AG)
#' 
#' TREE AGE DATA ONLY IN FOR ("AZ", "CO", "ID", "MT", "NV", "UT") \cr 
#' FMORTCFAL includes trees >= 5.0" DIA and greater and is not populated 
#' for states("CA", "OR", "WA", "OK") \cr Mortality variables only 
#' available in: AZ, CO, ID, MT, NV, NM, UT, WY, ND, SD, NE, KS, OK.
#' 
#' \bold{TPA} If TPA=TRUE and istree=TRUE or isseed=TRUE, the following
#' tree/seedling variables are multiplied by trees-per-acre (TPA_UNADJ).
#' TPA_UNADJ is set to a constant derived from the plot size and equals
#' 6.018046 for trees sampled on subplots, 74.965282 for trees sampled on
#' microplots, and 0.999188 for trees sampled on macroplots. Variable-radius
#' plots were often used in earlier inventories, so the value in TPA_UNADJ
#' decreases as the tree diameter increases (FIADB User Guide)
#' 
#' Variables: VOLCFNET, VOLCFGRS, GROWCFGS, GROWCFAL, FGROWCFGS, FGROWCFAL,
#' MORTCFGS, MORTCFAL, FMORTCFGS, FMORTCFAL, REMVCFGS, REMVCFAL, FREMVCFGS,
#' FREMVCFAL, DRYBIO_BOLE, DRYBIO_STUMP, DRYBIO_TOP, DRYBIO_SAPLING,
#' DRYBIO_WDLD_SPP, DRYBIO_BG, CARBON_BG, CARBON_AG
#' 
#' \bold{MISC}
#' 
#' For regions outside RMRS, there is no OWNCD attached to nonforest lands.
#' 
#' @param states String or numeric vector. Name (e.g., 'Arizona','New Mexico')
#' or code (e.g., 4, 35) of state(s) for evalid. If all states in one or more
#' FIA Research Station is desired, set states=NULL and use RS argument to
#' define RS.
#' @param RS String vector. Name of research station(s) to get public XY
#' coordinates for ('RMRS','SRS','NCRS','NERS','PNWRS'). Do not use if states 
#' is populated. See FIESTA::ref_statecd for reference to RS and states.
#' @param datsource String. Source of data ('datamart', 'sqlite').
#' @param data_dsn String. If datsource='sqlite', the name of SQLite database
#' (*.sqlite).
#' @param dbTabs List of database tables the user would like returned.
#'  See help(dbTables) for a list of options.
#' @param eval String. Type of evaluation time frame for data extraction 
#' ('FIA', 'custom'). See eval_opts for more further options. 
#' @param eval_opts List of evaluation options for 'FIA' or 'custom'
#' evaluations to determine the set of data returned. See help(eval_options)
#' for a list of options.
#' @param puniqueid String. Name of unique identifier in plot_layer in dbTabs.
#' @param invtype String. Type of FIA inventory to extract ('PERIODIC',
#' 'ANNUAL', 'BOTH').
#' @param intensity1 Logical. If TRUE, includes only XY coordinates where 
#' INTENSITY = 1 (FIA base grid).
#' @param issubp Logical. If TRUE, subplot tables are extracted from FIA
#' database (SUBPLOT, SUBP_COND).
#' @param istree Logical. If TRUE, include tree data.
#' @param isseed Logical. If TRUE, include seedling data.
#' @param biojenk Logical. If TRUE, Jenkins biomass is calculated.
#' @param greenwt Logical. If TRUE, green weight biomass is calculated.
#' @param addplotgeom Logical. If TRUE, variables from the PLOTGEOM table are
#' appended to the plot table.
#' @param othertables String Vector. Name of other table(s) in FIADB to include
#' in output. The table must have PLT_CN as unique identifier of a plot.
#' @param getxy Logical. If TRUE, gets separate XY table.
#' @param xy_datsource Source of XY data ('obj', 'csv', 'datamart', 'sqlite').
#' @param xy_dsn If datsource='sqlite', the file name (data source name) of
#' the sqlite database (*.sqlite) where XY data are.
#' @param xy sf R object or String. Table with xy coordinates. Can be a spatial
#' polygon object, data frame, full pathname to a shapefile, or name of a layer
#' within a database.
#' @param xy_opts List of xy data options to specify if xy is NOT NULL. 
#' See xy_options (e.g., xy_opts = list(xvar='LON', yvar='LAT').
#' @param xymeasCur Logical. If TRUE, include XY coordinates from the most 
#' current sampled measurement of each plot.
#' @param coordType String. Type of xy coordinates using ('PUBLIC', 'ACTUAL')
#' @param pjoinid String. Variable in plt to join to XY data. Not necessary to
#' be unique. If using most current XY coordinates, use identifier for a plot
#' (e.g., PLOT_ID).
#' @param issp Logical. If TRUE, an sf spatial object is generated from the
#' public X/Y coordinates in the plot table.
#' @param spcond Logical. If TRUE, a set of condition-level attributes (e.g.,
#' FORTYPCD) represented at the plot-level are extracted from FIA DataMart COND
#' table.  (See Notes for more info on how condition attributes were added).
#' @param spcondid1 Logical. If TRUE and issp=TRUE and spcond=TRUE, condition
#' variables are determined by condition 1 attributes. If FALSE, an algorithm
#' is used to select the condition to use (See details for alorithm used).
#' @param defaultVars Logical. If TRUE, a set of default variables are selected
#' in query.  See notes for variable descriptions.
#' @param regionVars Logical. If TRUE, regional variables are included in query
#' (e.g., SDI_RMRS, SDIPCT_RMRS, SDIMAX_RMRS, QMD_RMRS).
#' @param regionVarsRS String. Region for regionVars
#' ('RMRS','SRS','NCRS','NERS','PNWRS').
#' @param ACI Logical. If TRUE, the data from All Condition Inventories (ACI)
#' are included in dataset (NF_SAMPLING_STATUS_CD = 1). See below for more
#' details.
#' @param subcycle99 Logical. If TRUE, includes plots with SUBCYCLE = 99. These
#' plots are plots that are measured more than once and are not included in the
#' estimation process.
#' @param intensity1 Logical. If TRUE, includes only plots where INTENSITY = 1.
#' @param stateFilter Character string or Named list. Logical statement to use
#' as plot and filter in sql query. Must include plot alias ('p.') and be sql
#' syntax (e.g., 'p.COUNTYCD = 1'). If more than 1 state, stateFilter must be a
#' named list with names as state(s) (e.g., list(Utah='p.COUNTYCD = 1').
#' @param allFilter String. An overall filter for plot or condition data in all
#' states in query. The expression must be R syntax (e.g., 'PLOT_STATUS_CD ==
#' 1').
#' @param alltFilter String. If istree=TRUE, an overall filter for tree data in
#' all states (e.g., only Whitebark pine trees - 'SPCD == 101'). Note: returns
#' only plots with trees included in filter.
#' @param returndata Logical. If TRUE, returns data objects.
#' @param savedata Logical. If TRUE, saves data to outfolder as comma-delimited
#' file (*.csv).  No objects are returned. If FALSE, the data are saved as R
#' objects and returned to user.  See details for caveats.
#' @param saveqry Logical. If TRUE, saves queries to outfolder (by state).
#' @param savePOP Logical. If TRUE, save and return the POP_PLOT_STRATUM_ASSGN
#' table.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. If out_layer = NULL,
#' @param dbconn Open database connection.
#' @param dbconnopen Logical. If TRUE, the dbconn connection is not closed. 
#' @param evalInfo List. List object output from DBgetEvalid or DBgetXY 
#' @param ... For extendibility.
#' FIESTA functions. 
#' 
#' @return if returndata=TRUE, a list of the following objects: 
#' \item{states}{ Vector. Input state(s) (full state names: Arizona). } 
#' \item{tabs}{ List. A list of data frames from FIA database, including 
#' plt and cond; and tree (if Type='VOL'); seed (if isseed=TRUE), 
#' p2veg_subplot_spp, p2veg_subp_structure, and invasive_subplot_spp 
#' (if Type='P2VEG'). See below 'Output Tables - FIA Table Names' 
#' for reference to FIA database tables.
#' See FIESTA:ref_* for variable descriptions (e.g., FIESTAutils::ref_tree). 
#' If istree and the number of states > 3, tree data are saved to outfolder 
#' and not returned to accommodate R memory issues. } 
#' \item{xy*_PUBLIC}{ Data frame. XY data from FIA's public database. If 
#' measCur=TRUE, named xyCur_PUBLIC, else named xy_PUBLIC. The data frame 
#' has 10 columns ('PLT_CN', 'LON_PUBLIC', 'LAT_PUBLIC', 'STATECD', 'UNITCD',
#' 'COUNTYCD', 'PLOT', 'INTENSITY', 'PLOT_ID' (ID+STATECD+UNTCD+COUNTYCD+PLOT), 
#' 'COUNTYFIPS'. If issp=TRUE, returns an sf object. }
#' \item{spconddat}{ If spcond=TRUE, the condition variables representing 
#' each plot for spatial display. For plots with multiple conditions, 
#' the selected condition is based on CONDID=1 (if spcondid1=TRUE) or a 
#' set if criteria defined in Details - spcond (if spcondid1=FALSE). }
#' \item{evalid}{ Number. If evalCur=TRUE or evalEndyr is not NULL, the
#' Evalidation ID from the FIA database used to define the output data. } 
#' \item{pltcnt}{ Data frame. Number of plots (NBRPLOTS) by state, cycle, 
#' inventory year, and plot status. }
#' \item{pop_plot_stratum_assgn}{ Data frame. If savePOP=TRUE, and FIA Evaluations
#' are used to extract data from database, return the POP_PLOT_STRATUM_ASSGN
#' table or, if more than one Type and savePOP=FALSE. If more than one
#' Type, only the records for the evalTypes are returned, otherwise all
#' Types for the state evaluation are returned. }
#' 
#' *Output Tables - FIA Table Names*\cr \tabular{lll}{ 
#' \tab \bold{tab} \tab \bold{FIA Table}\cr 
#' \tab plt \tab plot\cr
#' \tab cond \tab cond\cr
#' \tab tree \tab tree\cr 
#' \tab p2veg_subplot_spp \tab P2VEG_SUBPLOT_SPP\cr 
#' \tab p2veg_subp_structure \tab P2VEG_SUBP_STRUCTURE\cr 
#' \tab invsubp \tab INVASIVE_SUBPLOT_SPP\cr 
#' \tab subplot \tab SUBPLOT\cr 
#' \tab subp_cond \tab SUBP_COND\cr 
#' \tab cond_dwm_calc \tab COND_DWM_CALC\cr 
#' \tab grm \tab TREE_GRM_COMPONENT\cr 
#' \tab issccm \tab SUBP_COND_CHNG_MTRX\cr }
#' 
#' 
#' #' Outputs to outfolder (if savedata=TRUE): 
#' \tabular{ll}{ 
#' \tab - If saveqry=TRUE, text file(s) of SQL queries used to extract data 
#' from database (_.txt). Note: one query is used for extracting both plt
#' and cond (pltcondqry*.txt). \cr
#' \tab - CSV file of plot and condition counts (pltcnt*.txt).\cr 
#' \tab - Layers in a database or CSV files of output tables.\cr 
#' \tab - If issp=TRUE, a feature class or ESRI shapefile of plot-level 
#' level attributes. If shapefile (.shp), variable names are truncated to 
#' 10 characters or less. See notes for more info.\cr 
#' \tab - If issp=TRUE and out_fmt='sqlite', the SQLite data is SpatiaLite.\cr }
#'
#' To deal with limitations of R object size and/or computer memory issues, if
#' istree=TRUE and more than three states are desired, the tree data are saved
#' to a CSV file, with no tree data object returned. \cr
#' @note
#' 
#' If no parameters are included, the user is prompted for input. If partial
#' parameters, the default parameter values are used for those not specified.
#' 
#' \bold{Data Access} All data are downloaded from FIA's publicly-available
#' online Datamart
#' (https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html).
#' 
#' Because of FIA's confidentiality agreement to protect the privacy of
#' landowners as well as protecting the scientific integrity of FIA's sample
#' design, the exact coordinates of the sample plot locations are not included.
#' The X/Y coordinates (LON_PUBLIC/LAT_PUBLIC) for download are perturbed up to
#' a mile from the original location
#' (https://www.fia.fs.fed.us/tools-data/spatial/Policy/index.php).  If the
#' exact location of the plots are necessary for your analysis, contact FIA's
#' Spatial Data Services
#' (https://www.fia.fs.fed.us/tools-data/spatial/index.php).
#' @author Tracey S. Frescino
#' @references DeBlander, Larry T.; Shaw, John D.; Witt, Chris; Menlove, Jim;
#' Thompson, Michael T.; Morgan, Todd A.; DeRose, R. Justin; Amacher, Michael,
#' C. 2010. Utah's forest resources, 2000-2005. Resour. Bull. RMRS-RB-10. Fort
#' Collins, CO; U.S. Department of Agriculture, Forest Service, Rocky Mountain
#' Research Station. 144 p.
#' 
#' Heath, L.S.; Hansen, M. H.; Smith, J.E. [and others]. 2009. Investigation
#' into calculating tree biomass and carbon in the FIADB using a biomass
#' expansion factor approach. In: Forest Inventory and Analysis (FIA) Symposium
#' 2008. RMRS-P-56CD. Fort Collins, CO: U.S. Department of Agriculture, Forest
#' Service, Rocky Mountain Research Station. 1 CD.
#' 
#' Burrill, E.A., Wilson, A.M., Turner, J.A., Pugh, S.A., Menlove, J.,
#' Christiansen, G., Conkling, B.L., Winnie, D., 2018. Forest Inventory and
#' Analysis Database [WWW Document].  St Paul MN US Dep. Agric. For. Serv.
#' North. Res. Stn.  URL http://apps.fs.fed.us/fiadb-downloads/datamart.html
#' (accessed 3.6.21).
#' @keywords data
#' @examples
#' \dontrun{
#' # Extract the most current evaluation of data for Utah
#' UTdat <- DBgetPlots(states = "Utah", 
#'                     eval = "FIA",
#'                     eval_opts = list(Cur = TRUE))
#' names(UTdat)
#' head(UTdat$plot)
#' UTdat$pltcnt
#' 
#' # Look at number of plots by inventory year
#' table(UTdat$plot$INVYR)
#' 
#' # Note: see FIESTAutils::ref_plot and FIESTAutils::ref_cond for variable descriptions
#' # Or consult FIA Database documentation
#' # \link{https://www.fia.fs.fed.us/library/database-documentation/index.php}
#' 
#' # Extract specified inventory years 2012:2014 and spatial information
#' UTdat2 <- DBgetPlots(states = "Utah",
#'                      eval = "custom",
#'                      eval_opts = list(invyrs = 2012:2014), 
#'                      issp = TRUE)
#' names(UTdat2)
#' UTdat2$pltcnt
#' UTdat2$xy_PUBLIC
#'
#' # Extract and display plots with aspen forest type
#' UTdat3 <- DBgetPlots(states = "Utah",
#'                      eval = "custom",
#'                      eval_opts = eval_options(invyrs = 2012:2014),
#'                      issp = TRUE,
#'                      allFilter = "FORTYPCD == 901")
#' names(UTdat3)
#' UTdat3$pltcnt
#' 
#' plot(sf::st_geometry(FIESTA::stunitco[FIESTA::stunitco$STATENM == "Utah",]),
#'                      border = "light grey")
#' plot(sf::st_geometry(UTdat3$xy_PUBLIC), add=TRUE, pch=18, cex=.5)
#' }
#' @export DBgetPlots
DBgetPlots <- function (states = NULL, 
                        RS = NULL,
                        datsource = "datamart",
                        data_dsn = NULL,
                        dbTabs = dbTables(), 
                        eval = "FIA",
                        eval_opts = NULL,
                        puniqueid = "CN", 
                        invtype = "ANNUAL", 
                        intensity1 = FALSE, 
                        issubp = FALSE, 
                        istree = TRUE,
                        isseed = FALSE,                      
                        biojenk = FALSE,
                        greenwt = FALSE,
                        addplotgeom = FALSE, 
                        othertables = NULL, 
                        getxy = TRUE,
                        xy_datsource = NULL, 
                        xy_dsn = NULL, 
                        xy = "PLOT",
                        xy_opts = xy_options(),
                        xymeasCur = FALSE,
                        coordType = "PUBLIC", 
                        pjoinid = NULL, 
                        issp = FALSE, 
                        spcond = FALSE, 
                        spcondid1 = FALSE, 
                        defaultVars = TRUE, 
                        regionVars = FALSE, 
                        regionVarsRS = "RMRS", 
                        ACI = FALSE, 
                        subcycle99 = FALSE, 
                        stateFilter = NULL, 
                        allFilter = NULL, 
                        alltFilter = NULL,
                        returndata = TRUE,
                        savedata = FALSE, 
                        saveqry = FALSE, 
                        savePOP = FALSE,
                        savedata_opts = NULL,
                        dbconn = NULL,
                        dbconnopen = FALSE,
                        evalInfo = NULL,
                        ...
                        ) {

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)
  saveSURVEY=isveg=ischng=isdwm=isgrm=islulc=isinv <- FALSE

  other_tables <- c("BOUNDARY", "COND_DWM_CALC", "COUNTY", "DWM_COARSE_WOODY_DEBRIS", 
	"DWM_DUFF_LITTER_FUEL", "DWM_FINE_WOODY_DEBRIS", "DWM_MICROPLOT_FUEL", 
	"DWM_RESIDUAL_PILE", "DWM_TRANSECT_SEGMENT", "DWM_VISIT", "GRND_CVR", 
	"INVASIVE_SUBPLOT_SPP", "LICHEN_LAB", "LICHEN_PLOT_SUMMARY", "LICHEN_VISIT", 
	"PLOTSNAP", "PLOT_REGEN", "SEEDLING_REGEN", "SITETREE", 
	"SOILS_EROSION", "SOILS_LAB", "SOILS_SAMPLE_LOC", "SOILS_VISIT", 
	"SUBPLOT_REGEN", "TREE_GRM_BEGIN", "TREE_GRM_ESTN", "TREE_GRM_MIDPT",
 	"TREE_GRM_THRESHOLD", "TREE_REGIONAL_BIOMASS", "TREE_WOODLAND_STEMS")

  pop_tables <- c("POP_ESTN_UNIT", "POP_EVAL", "POP_EVAL_ATTRIBUTE", "POP_EVAL_GRP", 
	"POP_EVAL_TYP", "POP_STRATUM", "SURVEY") 

  if (gui) {
    invtype=issp=spcondid1=defaultVars=regionVars=ACI=
	subcycle99=intensity1=allFilter=savedata=saveqry=parameters=out_fmt=
	overwrite=BIOJENK_kg=BIOJENK_lb=PREV_PLTCN=savePOP=xymeasCur=eval <- NULL
  }

  ## Set global variables  
  CN=CONDID=COND_STATUS_CD=PLT_CN=FORTYPCD=pltvarlst=condvarlst=pgeomvarlst=
	treevarlst=tsumvarlst=seedvarlst=ssumvarlst=vsubpsppvarlst=vsubpstrvarlst=
	invsubpvarlst=subpvarlst=subpcvarlst=dwmvarlst=grmvarlst=sccmvarlst=filtervarlst=
	SUBPPROP_UNADJ=MICRPROP_UNADJ=TPA_UNADJ=TPAMORT_UNADJ=TPAREMV_UNADJ=
	SEEDCNT6=TREECOUNT_CALC=SEEDSUBP=LIVE_CANOPY_CVR_PCT=CONDPROP_UNADJ=
	PLOT_NONSAMPLE_REASN_CD=PLOT_STATUS_CD=BA=DIA=CRCOVPCT_RMRS=TIMBERCD=
	SITECLCD=RESERVCD=JENKINS_TOTAL_B1=JENKINS_TOTAL_B2=POP_PLOT_STRATUM_ASSGN=
	NF_SAMPLING_STATUS_CD=NF_COND_STATUS_CD=ACI_NFS=OWNCD=OWNGRPCD=INVYR=
	FORNONSAMP=PLOT_ID=sppvarsnew=STATECD=UNITCD=COUNTYCD=SEEDSUBP6=
	PREV_PLT_CN=dbqueries=REF_SPECIES=PLOT=PLOTe=POP_PLOT_STRATUM_ASSGNe=TRE_CN <- NULL
  plotnm=plotgeomnm=ppsanm=condnm=treenm=seednm=
     vsubpsppnm=vsubpstrnm=invsubpnm=
	subplotnm=subpcondnm=sccmnm=grmnm=dwmnm=surveynm=evalidnm=
     pltcondx=GREENBIO_AG=DRYBIO_AG=DRYWT_TO_GREENWT_CONVERSION <- NULL


  ## Define functions
  ###########################################################
  getcoords <- function(coordType){
    switch(coordType,
      ACTUAL = c("LON_ACTUAL", "LAT_ACTUAL"),
      PUBLIC = c("LON_PUBLIC", "LAT_PUBLIC"))
  }  
  get_evalidtyp <- function(evalid, typcd) {
    ## DESCRIPTION: gets an FIA evalid from list if exists 
    if (any(evalid[endsWith(as.character(evalid), typcd)])) {
	  return(evalid[endsWith(as.character(evalid), typcd)])
    } else if (any(evalid[endsWith(as.character(evalid), "01")])) {
      return(evalid[endsWith(as.character(evalid), "01")])
    } else if (any(evalid[endsWith(as.character(evalid), "00")])) {
      return(evalid[endsWith(as.character(evalid), "00")])
    } else {
      return(evalid[1])
    }	 
  }  
 
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  matchargs <- as.list(match.call()[-1])

  dotargs <- c(list(...))
  args <- as.list(environment())
  #args <- as.list(.GlobalEnv)
  args <- args[!names(args) %in% names(dotargs)]
  args <- args[names(args) %in% names(matchargs)]
  args <- append(args, dotargs)

  #params <- formals(DBgetPlots)
  #params <- mget(names(params),ifnotfound="NULL",envir=as.environment(-1))

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1] 
  formallst <- unique(c(names(formals(DBgetPlots)), "isveg", "ischng", "isdwm"))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  if ("evalCur" %in% input.params) {
    eval_opts$Cur <- args$evalCur
  }
  
  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts, eval_opts=eval_opts,
				xy_opts=xy_opts)
 
  ## Set eval_options defaults
  eval_defaults_list <- formals(eval_options)[-length(formals(eval_options))] 
  for (i in 1:length(eval_defaults_list)) {
    assign(names(eval_defaults_list)[[i]], eval_defaults_list[[i]])
  } 
 
  ## Set user-supplied eval_opts values
  if (length(eval_opts) > 0) {
    for (i in 1:length(eval_opts)) {
      if (names(eval_opts)[[i]] %in% names(eval_defaults_list)) {
        assign(names(eval_opts)[[i]], eval_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(eval_opts)[[i]]))
      }
    }

  } else {
    if (gui) {
      evallst <- c("FIA", "custom")
      eval <- pcheck.varchar(var2check=eval, varnm="eval", 
		checklst=evallst, gui=gui, caption="Evaluation type?")
      if (eval == "FIA") {
        message("getting most current FIA evaluation")       
      } else {
        message("getting most current measurement in database")
      }
      eval_opts <- eval_options(Cur = TRUE)
    } else {
      warning("no evaluation timeframe specified...")
      message("see eval and eval_opts parameters (e.g., eval='custom', eval_opts=eval_options(Cur=TRUE))\n")
      stop()
    }
  }

  if ("isveg" %in% names(args)) {
    message("the parameter isveg is deprecated... use eval_options(Type='P2VEG'))\n")
    isveg <- args$isveg
  }

  ## Set xy_options defaults
  xy_defaults_list <- formals(xy_options)[-length(formals(xy_options))]
  for (i in 1:length(xy_defaults_list)) {
    assign(names(xy_defaults_list)[[i]], xy_defaults_list[[i]])
  }
  ## Set user-supplied xy_opts values
  if (length(xy_opts) > 0) {
    for (i in 1:length(xy_opts)) {
      if (names(xy_opts)[[i]] %in% names(xy_defaults_list)) {
        assign(names(xy_opts)[[i]], xy_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(xy_opts)[[i]]))
      }
    }
  } 

  ## Set dbTables defaults
  dbTables_defaults_list <- formals(dbTables)[-length(formals(dbTables))]
  for (i in 1:length(dbTables_defaults_list)) {
    assign(names(dbTables_defaults_list)[[i]], dbTables_defaults_list[[i]])
  }
  ## Set user-supplied dbTables values
  if (length(dbTabs) > 0) {
    for (i in 1:length(dbTabs)) {
      if (names(dbTabs)[[i]] %in% names(dbTables_defaults_list)) {
        assign(names(dbTabs)[[i]], dbTabs[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(dbTabs)[[i]]))
      }
    }
  }

  ## Set savedata defaults
  savedata_defaults_list <- formals(savedata_options)[-length(formals(savedata_options))]
  for (i in 1:length(savedata_defaults_list)) {
    assign(names(savedata_defaults_list)[[i]], savedata_defaults_list[[i]])
  } 
  ## Set user-supplied savedata values
  if (length(savedata_opts) > 0) {
    if (!savedata) {
      message("savedata=FALSE with savedata parameters... no data are saved")
    }
    for (i in 1:length(savedata_opts)) {
      if (names(savedata_opts)[[i]] %in% names(savedata_defaults_list)) {
        assign(names(savedata_opts)[[i]], savedata_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(savedata_opts)[[i]]))
      }
    }
  }

  ## Define variables
  actual=getinvyr <- FALSE
  SCHEMA <- ""
  SCHEMA. <- ""
  isRMRS <- FALSE
  xycoords = c("LON_PUBLIC", "LAT_PUBLIC")
  #coordType <- "PUBLIC"
  parameters <- FALSE
  islulc=isgrm=subcycle99 <- FALSE
  datamartType = "CSV" 

  ########################################################################
  ### GET PARAMETER INPUTS
  ########################################################################
  iseval <- FALSE
  subsetPOP <- TRUE

  ## Check invtype
  invtypelst <- c('ANNUAL', 'PERIODIC', 'BOTH')
  invtype <- pcheck.varchar(invtype, varnm="invtype", checklst=invtypelst, 
		caption="Inventory Type", gui=gui)

  #############################################################################
  ## Set datsource
  ########################################################
  datsourcelst <- c("datamart", "sqlite", "csv", "obj")
  datsource <- pcheck.varchar(var2check=datsource, varnm="datsource", 
		checklst=datsourcelst, gui=gui, caption="Data source?") 
  #if (datsource %in% c("sqlite", "gdb")) {
  #  data_dsn <- DBtestSQLite(data_dsn)
  #}
  if (!is.null(data_dsn)) {
    if (getext(data_dsn) %in% c("sqlite", "db", "db3")) {
      dbconn <- DBtestSQLite(data_dsn, dbconnopen=TRUE, showlist=FALSE)
      dbtablst <- DBI::dbListTables(dbconn)
      if (length(dbtablst) == 0) {
        stop("no data in ", datsource)
      }
    } else {
      stop("only sqlite databases available currently")
    }     
  }
 
  datamartTypelst <- c("CSV", "SQLITE")
  datamartType <- pcheck.varchar(var2check=datamartType, varnm="datamartType", 
		checklst=datamartTypelst, gui=gui, caption="Datamart Type?") 

  ## If Get SQlite file for state
  #################################################
  if (datamartType == "SQLITE") {
    dbfolder <- tempdir()
    statedbfn <- DBgetSQLite(state, dbfolder)
    dbconn <- DBtestSQLite(statedbfn, dbconnopen=TRUE, showlist=FALSE)
    dbtablst <- DBI::dbListTables(dbconn)
    datsource <- "sqlite"
  }

  ## GETS DATA TABLES (OTHER THAN PLOT/CONDITION) IF NULL
  ###########################################################
  getType <- ifelse (is.null(evalid), TRUE, FALSE)
  Typelst <- c("ALL", "CURR", "VOL", "P2VEG", "DWM", "INV", "CHNG", 
	             "GROW", "MORT", "REMV", "GRM")
  if (gui) {
    Type <- select.list(Typelst, title="eval type", 
		preselect="VOL", multiple=TRUE)
    if (length(Type)==0) Type <- "VOL"
  } 
  Typemiss <- Type[!Type %in% Typelst]
  if (length(Typemiss) > 0) {
    stop("Type must be in following list: \n", toString(Typelst))
  }
  if (any(Type == "VOL") && !istree) {
    message("eval Type includes 'VOL', but istree = FALSE... no trees included")
  }
  if (any(Type == "P2VEG")) isveg=issubp <- TRUE  # P2VEG_SUBPLOT_SPP, P2VEG_SUBP_STRUCTURE
  if (any(Type == "INV")) isinv=issubp <- TRUE  # INVASIVE_SUBPLOT_SPP
  if (any(Type == "CHNG")) ischng <- TRUE # SUBP_COND_CHNG_MTR
  if (any(Type == "DWM")) isdwm <- TRUE  # COND_DWM_CALC
  if (any(Type %in% c("GROW", "MORT", "REMV", "GRM"))) ischng=isgrm <- TRUE
  
  if (isveg && invtype == "PERIODIC") {
    message("understory vegetation data only available for annual data\n")
    isveg <- FALSE
  }
 
  biojenk <- pcheck.logical(biojenk, varnm="biojenk", 
		title="Jenkins biomass?", first="NO", gui=gui)

  greenwt <- pcheck.logical(greenwt, varnm="greenwt", 
		title="Green weight?", first="NO", gui=gui)

  if ((biojenk || greenwt) && !istree) {
    istree <- TRUE
  }
 
  ## Check coordType
  ####################################################################
  coordTypelst <- c("PUBLIC", "ACTUAL")
  coordType <- pcheck.varchar(var2check=coordType, varnm="coordType", 
		gui=gui, checklst=coordTypelst, caption="Coordinate Type?")
	
  if (!is.null(xy)) {
    if (xy %in% c("xy_PUBLIC", "xyCur_PUBLIC", "xy_ACTUAL", "xyCur_ACTUAL") && !grepl(coordType, xy)) {
      coordTypenot <- coordTypelst[!coordTypelst %in% coordType]
	  if (length(coordTypenot) > 1) stop("invalid coordType")
	  message("coordType does not match xy... changing ", coordType, " to ", coordTypenot)
	  coordType <- coordTypenot
	}
  }

  ########################################################################
  ### DBgetEvalid()
  ########################################################################

  ## Data warnings
  ## Note: Periodic data in database includes forested plots >= 5% cover 
  ## Note: Annual data in database includes forested plots >=10% cover

  ## Get DBgetEvalid parameters from eval_opts
  ################################################
  if (eval == "FIA") {
    evalCur <- ifelse (Cur, TRUE, FALSE) 
    evalAll <- ifelse (All, TRUE, FALSE) 
    evalEndyr <- Endyr
    measCur=allyrs <- FALSE
    measEndyr <- NULL
  } else {
    measCur <- ifelse (Cur, TRUE, FALSE) 
    allyrs <- ifelse (All, TRUE, FALSE) 
    if (length(Endyr) > 1) {
      stop("only one Endyr allowed for custom estimations")
    }
    measEndyr <- Endyr
    evalCur=evalAll <- FALSE
    evalEndyr <- NULL
  }

  if (allyrs) {
    saveSURVEY <- TRUE
  }

  ## Get states, Evalid and/or invyrs info
  ##########################################################
  if (!is.null(evalInfo)) {
    list.items <- c("states", "invtype")
    evalInfo <- pcheck.object(evalInfo, "evalInfo", list.items=list.items)

  } else {
    if (invtype == "BOTH") {
      message("getting evalution info for annual data...")
      evalInfoA <- tryCatch( DBgetEvalid(states = states, 
                          RS = RS, 
                          datsource = datsource, 
                          data_dsn = data_dsn, 
                          dbconn = dbconn,
                          dbconnopen = TRUE,
                          invtype = "ANNUAL", 
                          evalid = evalid, 
                          evalCur = evalCur, 
                          evalEndyr = evalEndyr, 
                          evalAll = evalAll, 
                          evalType = Type, 
                          dbTabs = dbTabs,
                          gui = gui),
			error = function(e) {
                  message(e,"\n")
                  return(NULL) })
      if (is.null(evalInfoA)) {
        iseval <- FALSE
      } else {

        message("getting evalution info for periodic data...")
        evalInfoP <- tryCatch( DBgetEvalid(states = states, 
                          RS = RS, 
                          datsource = datsource, 
                          data_dsn = data_dsn, 
                          dbconn = dbconn,
                          dbconnopen = TRUE,
                          invtype = "PERIODIC", 
                          evalid = evalid, 
                          evalCur = evalCur, 
                          evalEndyr = evalEndyr, 
                          evalAll = evalAll, 
                          evalType = Type, 
                          dbTabs = dbTabs,
                          gui = gui),
			error = function(e) {
                  message(e,"\n")
                  return(NULL) })
       
       evalInfo <- evalInfoA
       if (!is.null(evalInfoP)) {
         for (st in names(evalInfo$evalidlist)) {
           evalInfo$evalidlist[[st]] <- 
			c(evalInfo$evalidlist[[st]], evalInfoP$evalidlist[[st]])
         }
         evalInfo$invtype <- "BOTH"
         evalInfo$invyrtab <- rbind(evalInfoP$invyrtab, evalInfo$invyrtab)
         if (!is.null(evalInfoP$SURVEY)) {
           evalInfo$SURVEY <- rbind(evalInfoP$SURVEY, evalInfo$SURVEY)
         }         
       }  
     }    

    } else {
      evalInfo <- tryCatch( DBgetEvalid(states = states, 
                          RS = RS, 
                          datsource = datsource, 
                          data_dsn = data_dsn, 
                          dbconn = dbconn,
                          dbconnopen = TRUE,
                          invtype = invtype, 
                          evalid = evalid, 
                          evalCur = evalCur, 
                          evalEndyr = evalEndyr, 
                          evalAll = evalAll, 
                          evalType = Type, 
                          dbTabs = dbTabs,
                          gui = gui),
			error = function(e) {
                  message(e,"\n")
                  return(NULL) })

    }
    if (is.null(evalInfo)) {
      iseval <- FALSE
    }
  }
  
  if (is.null(evalInfo)) stop("no data to return")
  states <- evalInfo$states
  evalidlist <- evalInfo$evalidlist
  evalTypelist <- evalInfo$evalTypelist
  invtype <- evalInfo$invtype
  invyrtab <- evalInfo$invyrtab
  if (length(evalidlist) > 0) {
    invyrs <- evalInfo$invyrs
    iseval <- TRUE
    savePOP <- TRUE
  }
  dbconn <- evalInfo$dbconn
  SURVEY <- evalInfo$SURVEY
  if (!is.null(SURVEY)) {
    surveynm <- "SURVEY"
  }
 
  POP_PLOT_STRATUM_ASSGNe <- evalInfo$POP_PLOT_STRATUM_ASSGN
  PLOTe <- evalInfo$PLOT

  ## Get state abbreviations and codes 
  ###########################################################
  stabbrlst <- pcheck.states(states, statereturn="ABBR")
  stcdlst <- pcheck.states(states, statereturn="VALUE")

  ## Get number of states 
  nbrstates <- length(states)  ##  Check whether to return tree data

  ####################################################################
  ## Check custom Evaluation data
  ####################################################################
  if (!iseval) {
    evalchk <- customEvalchk(states = states, 
                             measCur = measCur, 
                             measEndyr = measEndyr, 
                             allyrs = allyrs, 
                             invyrs = invyrs, 
                             measyrs = measyrs,
                             invyrtab = invyrtab)
    if (is.null(evalchk)) {
      stop("must specify an evaluation timeframe for data extraction... \n", 
		"...see eval_opts parameter, (e.g., eval_opts=eval_options(Cur=TRUE))")
    }
    measCur <- evalchk$measCur
    measEndyr <- evalchk$measEndyr
    allyrs <- evalchk$allyrs
    invyrs <- evalchk$invyrs
    measyrs <- evalchk$measyrs
    invyrlst <- evalchk$invyrlst
    measyrlst <- evalchk$measyrlst

    ## Check intensity1
    ###########################################################
    ## For periodic data, the INTENSITY variable does not equal 1
    if (invtype == "ANNUAL") {
      intensity1 <- pcheck.logical(intensity1, 
                            varnm = "intensity1",
                            title = "Intensity = 1?", 
                            first = "YES", 
                            gui = gui)
    } else {
      message("note: periodic data includes forested plots >= 5% cover")
      intensity1 <- FALSE
    }

    ## Check subcycle99
    subcycle99 <- pcheck.logical(subcycle99, varnm="subcycle99", 
		title="Keep SUBCYCLE 99?", first="NO", gui=gui)

    ## Check ACI
    ACI <- pcheck.logical(ACI, varnm="ACI", 
		title="ACI conditions?", first="NO", gui=gui)

  } else {
    #message(evalidlist)

    subsetPOP <- TRUE

    if (!is.null(subcycle99) && subcycle99) {
      message("subcycle99 plots are not included in FIA evaluations") 
    } 
    subcycle99 <- FALSE
    
    if (!is.null(ACI) && ACI) {
      message("ACI plots are included in FIA evaluations") 
    } 
    ACI <- FALSE
    allyrs <- FALSE
  }
  
  ## Set maxstates 
  ###########################################################
  ##  The number of states to append together, while still small enough to return 
  ##  as objects (without memory issues). This includes all tables except tree table..  
  ##  If there is more than 1 state with more than 6 inventory years and no filters,  
  ##  the tree table will not be returned as an object.. only written to outfolder.
  maxstates.tree <- ifelse(allyrs && is.null(allFilter), 3, 
						ifelse(!is.null(allFilter), 10, 20))  

  ## Get maximum number of inventory years for states in query 
  ## (used to determine size of tree data)
  #nbrinvyrs <- length(unique(unlist(invyrs)))
    
  ## Check defaultVars
  defaultVars <- pcheck.logical(defaultVars, varnm="defaultVars", 
                                title="Default variables?", 
                                first="YES", gui=gui)
  ## Check regionalVars
  regionVars <- pcheck.logical(regionVars, varnm="regionVars", 
                               title="Regional variables?", 
                               first="NO", gui=gui)

  ## Check stateFilter
  if (!is.null(stateFilter)) {
    if (nbrstates > 1) {
      if (!is.list(stateFilter) || is.null(names(stateFilter))) {
        stop("if more than 1 state, stFilter must be a named list")
      } else if (length(stateFilter) > nbrstates) {
        stop("too many states in stFilter")
      } else if (!all(names(stateFilter) %in% states)) {
        stop("invalid stFilter names")
      }
    }
    ## Check for alias
#    if (!grepl("p.", stateFilter) && !grepl("c.", stateFilter)) 
#      stop("must include plot or condition alias to stateFilter ('p.' or 'c.')")
    if (!grepl("p.", stateFilter)) {
      stop("must include plot alias to stateFilter ('p.')")
    }

    ## change R syntax to sql syntax
    stateFilter <- gsub("==", "=", stateFilter)
    stateFilter <- gsub("!=", "<>", stateFilter)
    if (grepl("%in%", stateFilter)) {
      stop("stateFilter must be in sql syntax... change %in% to in(...)")
    }
  } 

  ## Check getxy
  getxy <- pcheck.logical(getxy, varnm="getxy",
    title="Save XY?", first="YES", gui=gui)
  if (getxy) {
    if (evalCur || measCur) {
      if (!xymeasCur) {
        message("getting most current data for XY")
      }
      xymeasCur <- TRUE
    } else {
      ## Check xymeasCur
      xymeasCur <- pcheck.logical(xymeasCur, varnm="xymeasCur", 
                         title="Most current XY?", first="YES", gui=gui)
    }
  }
  
  ## Check issp
  issp <- pcheck.logical(issp, varnm="issp", 
		title="SpatialPoints of plot vars?", first="NO", gui=gui)

  if (spcond) {
    ## Check spcondid1
    ###########################################################
    spcondid1 <- pcheck.logical(spcondid1, varnm="spcondid1", 
		title="Use cond1 for spatial?", first="YES", gui=gui)
  }

  ## Check othertables
  if (!is.null(othertables)) {
    if ("POP_PLOT_STRATUM_ASSGN" %in% othertables) {
      savePOP <- TRUE
      othertables <- othertables[othertables != "POP_PLOT_STRATUM_ASSGN"]
    }
  } 

  ########################################################################
  ### Saving data
  ########################################################################

  ## Check returndata
  returndata <- pcheck.logical(returndata, varnm="returndata", 
		title="Return data?", first="YES", gui=gui)

  ## Check savedata
  savedata <- pcheck.logical(savedata, varnm="savedata", 
		title="Save data to outfolder?", first="YES", gui=gui)

  if (!returndata && !savedata) {
    message("both returndata and savedata are FALSE... how would you like output")
    return(NULL)
  }

  ## Check saveqry
  saveqry <- pcheck.logical(saveqry, varnm="saveqry", 
		title="Save queries to outfolder?", first="YES", gui=gui)

  ## Check savePOP
  savePOP <- pcheck.logical(savePOP, varnm="savePOP", 
		title="Return POP table", first="NO", gui=gui)
 

  ##  Check whether to return tree data
  ###########################################################
  treeReturn <- TRUE
  if (istree && (nbrstates > maxstates.tree)) {
    warning("tree data object is too big.. writing to folder, no returned object")
    #savedata <- TRUE
    treeReturn <- FALSE
  } 
  
  ## Check outfolder, outfn.date, overwrite_dsn
  ###########################################################
  if (savedata | saveqry | parameters | !treeReturn | !returndata) {
    outlst <- pcheck.output(out_dsn = out_dsn, 
                            out_fmt = out_fmt, 
                            outfolder = outfolder, 
                            outfn.pre = outfn.pre, 
                            outfn.date = outfn.date, 
                            overwrite_dsn = overwrite_dsn, 
                            append_layer = append_layer, 
                            createSQLite = FALSE,
                            gui = gui)
    outfolder <- outlst$outfolder
    out_dsn <- outlst$out_dsn
    out_fmt <- outlst$out_fmt
  }
 
  ###########################################################################
  ### Default variables
  ###########################################################################
  if (defaultVars) {
    istree2 <- ifelse(istree || !is.null(alltFilter), TRUE, FALSE)
    DBvars <- DBvars.default(istree=istree2, isseed=isseed, isveg=isveg, 
		issubp=issubp, isdwm=isdwm, isgrm=isgrm, plotgeom=addplotgeom, 
		regionVars=regionVars)
    for (nm in names(DBvars)) assign(nm, DBvars[[nm]])
    for (nm in names(filtervarlst)) assign(nm, filtervarlst[[nm]])
  } 
  sppvars <- {}
  if (biojenk) sppvars <- c(sppvars, "JENKINS_TOTAL_B1", "JENKINS_TOTAL_B2")
  if (greenwt) sppvars <- c(sppvars, "DRYWT_TO_GREENWT_CONVERSION")


##############################################################################
##############################################################################
##############################################################################
  nbrcnds <- {}
  stcds <- {}
  stabbrfn <- ""
  pltcnt <- {}
  stateFilters <- {}
  filtervarlst <- c(pltvarlst, condvarlst)
  spcoords <- coordType
  spcoordslst <- coordType
  othertables2 <- othertables
  dbqueries <- list()

  if (returndata) {
    pltcond=spconddat <- {}
    tabs <- list() 
    tabIDs <- list()
    if(savePOP || iseval) ppsa <- {}  
    ## Create empty object for each spcoords
    if (getxy) {
      for (coordType in spcoordslst) {
        if (xymeasCur) {
          assign(paste0("xyCur_", coordType), {})
        } else {
          assign(paste0("xy_", coordType), {})
        }
      } 
      if (issp) {
        assign(paste0("spxyCur_", coordType), {})
      }
    }
  }

  ## Check data tables
  ##########################################################
  if (datsource == "sqlite" && !is.null(dbconn)) {
     
    ## Check to make sure layers are in database
    plotnm <- chkdbtab(dbtablst, plot_layer, stopifnull=FALSE)
    if (is.null(plotnm)) {
      message("there is no PLOT table in database")
    } else {
      pltflds <- DBI::dbListFields(dbconn, plotnm)
    }
	if (addplotgeom) {
      plotgeomnm <- chkdbtab(dbtablst, plotgeom_layer)
      if (is.null(plotgeomnm)) {
        message("there is no plotgeom table in database")
        addplotgeom <- FALSE
      } else {
        plotgeomflds <- DBI::dbListFields(dbconn, plotgeomnm)
		pgeomvarlst <- pgeomvarlst[pgeomvarlst %in% plotgeomflds]
        pltflds <- unique(c(pltflds, pgeomvarlst))
      }
    } 

    condnm <- chkdbtab(dbtablst, cond_layer, stopifnull=TRUE)
    condflds <- DBI::dbListFields(dbconn, condnm)
    if (is.null(condnm)) {
      pltcondflds <- pltflds
    } else {
      pltcondflds <- c(pltflds, condflds)
    }
    if (savePOP || iseval) {
	  if (!is.null(POP_PLOT_STRATUM_ASSGNe) && is.character(POP_PLOT_STRATUM_ASSGNe)) {
	    ppsanm <- POP_PLOT_STRATUM_ASSGNe
	  } else { 
        ppsanm <- chkdbtab(dbtablst, ppsa_layer)	  
        if (is.null(ppsanm)) {
          ppsanm <- chkdbtab(dbtablst, "ppsa", stopifnull=TRUE)
		}
      } 
	  if (!is.null(ppsanm)) {
        ppsaflds <- DBI::dbListFields(dbconn, ppsanm)
      }
    } 
    #if ((iseval || measCur) && is.null(surveynm)) {
    #  surveynm <- chkdbtab(dbtablst, survey_layer)
    #}

    ## Other tables
    if (!is.null(othertables)) {
      for (othertable in othertables) {
        othertable <- chkdbtab(dbtablst, othertable)
        if (is.null(othertable)) {
          othertables <- othertables[othertables != othertable]
          othertables2 <- othertables
        }
      }
    }

    ## Check for indices
    #########################################
    if (evalCur || measCur) {       
      chk <- checkidx(dbconn, plotnm, c("STATECD", "UNITCD", "COUNTYCD", "PLOT"))
      if (is.null(chk)) {
        message("to speed query... add an index to the plot table")
        message("createidx(dbconn, '", plotnm, 
                   "', c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT'))")
      }
    }
  }

  ###################################################################################
  ###################################################################################
  ## Loop through states
  ###################################################################################
  ###################################################################################
  pb <- utils::txtProgressBar(min=0, max=length(states))
  
  for (i in 1:length(states)) {
    utils::setTxtProgressBar(pb, i)

    if (savedata) {
      if (i > 1) { 
        append_layer <- TRUE
      }
      if (append_layer && overwrite_layer) {
        overwrite_layer <- FALSE
      }
    }

    evalid <- NULL
    state <- states[i]
    message("\ngetting data for ", state, "...")
    stcd <- pcheck.states(state, "VALUE")
    stabbr <- pcheck.states(state, "ABBR")
    pltx=condx=treex=seedx=
		p2veg_subplot_sppx=p2veg_subp_structurex=invasive_subplot_sppx=
		subpx=subpcx=cond_dwm_calcx=sccmx=
		ppsax=spconddatx=cond_chngx <- NULL   
    nochngdata <- FALSE

    if (!is.null(othertables)) {
      for (j in 1:length(othertables)) {
        assign(paste0("otherx", j), NULL)
      }
    } 

	if (returndata) {
	  dbqueries[[state]] <- list()
	}

    ## Get PLOT/COND data 
    ###################################################
    if (datsource == "datamart") {
      ## PLOT table
	  if (!is.null(PLOTe) && is.data.frame(PLOTe)) {
		PLOT <- PLOTe[PLOTe$STATECD == stcd, ]
	  } else {
        PLOT <- DBgetCSV("PLOT", stabbr, returnDT=TRUE, stopifnull=FALSE)
      } 
      if (is.null(PLOT)) {
        message("there is no PLOT table in datamart")
      } else {
        plotnm <- "PLOT"
        pltflds <- names(PLOT)
        setkey(PLOT, "STATECD", "UNITCD", "COUNTYCD", "PLOT", "INVYR")
      } 
      ## PLOTGEOM table  
      if (addplotgeom) {
        PLOTGEOM <- DBgetCSV("PLOTGEOM", stabbr, returnDT=TRUE, stopifnull=FALSE)
        if (!is.null(PLOTGEOM)) { 
          plotgeomnm <- "PLOTGEOM"
          plotgeomflds <- names(PLOTGEOM)
		  pgeomvarlst <- pgeomvarlst[pgeomvarlst %in% plotgeomflds]
          pltflds <- unique(pltflds, pgeomvarlst)
        } else {
          message("there is no plotgeom table in datamart")
        }
      }

      ## COND table 
      COND <- DBgetCSV("COND", stabbr, returnDT=TRUE, stopifnull=FALSE)
      condnm <- "COND"
      condflds <- names(COND)
      setkey(COND, "PLT_CN", "CONDID")

      ## Get pltcondflds
      if (!is.null(plotnm)) {
        pltcondflds <- unique(c(pltflds, condflds))
      } else {
        pltcondflds <- condflds
      }

      ## POP_PLOT_STRATUM_ASSGN table (ZIP FILE) - 
      if (iseval || savePOP) {
        if (!is.null(POP_PLOT_STRATUM_ASSGNe) && is.data.frame(POP_PLOT_STRATUM_ASSGNe)) {
		  POP_PLOT_STRATUM_ASSGN <- POP_PLOT_STRATUM_ASSGNe[POP_PLOT_STRATUM_ASSGNe$STATECD == stcd, ]
		} else { 
          POP_PLOT_STRATUM_ASSGN <- DBgetCSV("POP_PLOT_STRATUM_ASSGN", stabbr, 
		      returnDT=TRUE, stopifnull=FALSE) 
        }
		if (!is.null(POP_PLOT_STRATUM_ASSGN)) { 
          ppsanm <- "POP_PLOT_STRATUM_ASSGN"
          ppsaflds <- names(POP_PLOT_STRATUM_ASSGN)
		}
      }   

      ## SURVEY table
      if (iseval && measCur && is.null(surveynm)) {
        SURVEY <- DBgetCSV("SURVEY", stabbr, returnDT=TRUE, stopifnull=FALSE) 
        if (!is.null(SURVEY)) {
          surveynm <- "SURVEY"
        }
      }

      ## Other tables
      if (!is.null(othertables)) {
        for (othertable in othertables) {
          assign(othertable, 
 		        DBgetCSV(othertable, stabbr, returnDT=TRUE, stopifnull=FALSE))
          if (is.null(get(othertable))) {
            othertables <- othertables[othertables != othertable]
            othertables2 <- othertables
          }
        }
      }
    } else if (datsource %in% c("csv", "obj")) {

      ## PLOT table
	  if (!is.null(PLOTe) && is.data.frame(PLOTe)) {
		PLOT <- PLOTe[PLOTe$STATECD == stcd, ]
	  } else {
        PLOT <- pcheck.table(plot_layer, stopifnull=FALSE, stopifinvalid=FALSE)
	  }
      if (is.null(PLOT)) {
        message("the PLOT table does not exist")
      } else {
        plotnm <- "PLOT"
        pltflds <- names(PLOT)
      }
 
      ## PLOTGEOM table  
      if (addplotgeom) {
        PLOTGEOM <- pcheck.table(plotgeom_layer, stopifnull=FALSE, stopifinvalid=FALSE)
        if (!is.null(PLOTGEOM)) { 
          plotgeomnm <- "PLOTGEOM"
          plotgeomflds <- names(PLOTGEOM)
		  pgeomvarlst <- pgeomvarlst[pgeomvarlst %in% plotgeomflds]
          pltflds <- unique(pltflds, pgeomvarlst)
        } else {
          message("there is no plotgeom table")
        }
      }

      ## COND table
      COND <- pcheck.table(cond_layer, stopifnull=TRUE, stopifinvalid=TRUE)
      condnm <- "COND"
      condflds <- names(COND)

      ## Get pltcondflds
      if (!is.null(plotnm)) {
        pltcondflds <- unique(c(pltflds, condflds))
      } else {
        pltcondflds <- condflds
      }

      ## POP_PLOT_STRATUM_ASSGN table (ZIP FILE) - 
      if (iseval || savePOP) {
        if (!is.null(POP_PLOT_STRATUM_ASSGNe) && is.data.frame(POP_PLOT_STRATUM_ASSGNe)) {
		  POP_PLOT_STRATUM_ASSGN <- POP_PLOT_STRATUM_ASSGNe[POP_PLOT_STRATUM_ASSGNe$STATECD == stcd, ]
		} else { 
          POP_PLOT_STRATUM_ASSGN <- pcheck.table(ppsa_layer, 
					stopifnull=TRUE, stopifinvalid=TRUE)
		}
		if (!is.null(POP_PLOT_STRATUM_ASSGN)) {
          ppsanm <- "POP_PLOT_STRATUM_ASSGN" 
          names(POP_PLOT_STRATUM_ASSGN) <- toupper(names(POP_PLOT_STRATUM_ASSGN))
          ppsaflds <- names(POP_PLOT_STRATUM_ASSGN)  
		}
      } 

      ## SURVEY table
      if (iseval && measCur && is.null(surveynm)) {
        SURVEY <- pcheck.table(survey_layer, stopifnull=FALSE, stopifinvalid=FALSE)
        if (!is.null(SURVEY)) {
          surveynm <- "SURVEY"
        }
      }	  
    } 
	
	
    ###########################################################################
    ## From query
    ###########################################################################
    intensitynm <- findnm("INTENSITY", pltcondflds, returnNULL=TRUE)
    subcyclenm <- findnm("SUBCYCLE", pltcondflds, returnNULL=TRUE)

    ## PPSA query
    ################################################
    if (savePOP || iseval) {
      if (!is.null(ppsanm)) {
        ppsafromqry <- paste0(SCHEMA., ppsanm, " ppsa")
	  }
    }
 
    ## PLOT from/join query
    ################################################
    if (iseval) {
      pfrom_layer <- ifelse (is.null(plotnm), condnm, plotnm)
      pfromqry <- paste0(ppsafromqry, " \nJOIN ", SCHEMA., 
			pfrom_layer, " p ON (p.", puniqueid, " = ppsa.PLT_CN)")

    } else if (measCur) {
      popSURVEY <- ifelse(is.null(surveynm), FALSE, TRUE)
      subcycle99 <- ifelse(is.null(subcyclenm), TRUE, FALSE)
      plotobj <- NULL
      if (!is.null(plotnm) && exists(plotnm)) {
        plotobj <- get(plotnm)
      }
      pfromqry <- getpfromqry(Endyr = measEndyr, SCHEMA. = SCHEMA., 
                              varCur = varCur,
                              allyrs = allyrs,
                              subcycle99 = subcycle99, 
                              intensity1 = intensity1, 
                              popSURVEY = popSURVEY,
                              plotnm = plotnm, 
                              surveynm = surveynm,
                              plotobj = plotobj,
                              Type = Type)
    } else {
      if (is.null(plotnm)) {
        pfromqry <- paste0(SCHEMA., condnm, " p")
      } else {
        pfromqry <- paste0(SCHEMA., plotnm, " p")
      }
    }

    ## PLOT/COND from/join query
    ################################################
    pcfromqry <- paste0(pfromqry, " \nJOIN ", SCHEMA., 
				condnm, " c ON (c.PLT_CN = p.", puniqueid, ")")
    if (addplotgeom && !is.null(plotgeomnm)) {
      pcgeomfromqry <- paste0(pcfromqry, " \nJOIN ", SCHEMA., 
				plotgeomnm, " pg ON (pg.CN = p.", puniqueid, ")")
    }

    ###########################################################################
    ## State filter 
    ###########################################################################

    ## Create filter for state
    stFilter <- paste0("p.STATECD IN(", stcd, ")") 

    ## If FIA evaluation, get all plot from all evaluations.
    if (iseval) {
      evalid <- evalidlist[[state]]
      Types <- evalTypelist[[state]]
      evalFilter <- paste0("ppsa.EVALID IN(", toString(evalid), ")")

      if ("P2VEG" %in% Type) {
	    evalid.veg <- get_evalidtyp(evalid, "10")
        evalFilter.veg <- paste("ppsa.EVALID =", evalid.veg)
      } else {
        evalFilter.veg <- evalFilter
      }
      if ("INV" %in% Type) {
	    evalid.inv <- get_evalidtyp(evalid, "10")
        evalFilter.inv <- paste("ppsa.EVALID =", evalid.inv)
      } else {
        evalFilter.inv <- evalFilter
      }
      if ("DWM" %in% Type) {
	  	evalid.dwm <- get_evalidtyp(evalid, "07")
        evalFilter.dwm <- paste("ppsa.EVALID =", evalid.dwm)
      } else {
        evalFilter.dwm <- evalFilter
      }
      if (any(c("GROW", "MORT", "REMV", "GRM") %in% Type)) {
	  	evalid.grm <- get_evalidtyp(evalid, "03")
        evalFilter.grm <- paste("ppsa.EVALID =", evalid.grm)
      } else {
        evalFilter.grm <- evalFilter
      }
    } else {
      evalFilter <- stFilter 
      if (length(invyrs) > 0){
        invyr <- invyrlst[[state]]
        evalFilter <- paste0(stFilter, " and p.INVYR IN(", toString(invyr), ")")

      } else if (length(measyrs) > 0) {
        measyr <- measyrlst[[state]]
        evalFilter <- paste0(stFilter, " and p.MEASYEAR IN(", toString(measyr), ")")
      }
      if (!subcycle99) {
        subcyclenm <- findnm("SUBCYCLE", pltcondflds, returnNULL=TRUE)
        if (!is.null(subcyclenm)) {
          evalFilter <- paste(evalFilter, "and p.SUBCYCLE <> 99")
        }
      }
      if (isveg) {
        evalFilter.veg <- evalFilter
      }
      if (isinv) {
        evalFilter.inv <- evalFilter
      }
      if (isdwm) {
        evalFilter.dwm <- evalFilter 
      }      
      if (isgrm) {
        evalFilter.grm <- evalFilter
      }       
    } 
    if (intensity1) {
      if (!is.null(intensitynm)) {
        evalFilter <- paste(evalFilter, "and p.INTENSITY = '1'")
      } else {
        message("the INTENSITY variable is not in dataset")
      }
    }

    ####################################################################################
    #############################  ADDS FILTER (OPTIONAL)  #############################
    ####################################################################################

    ## Get stateFilter 
    if (is.null(stateFilter) && gui) {
      stateFilters <- ""
      addfilter <- "YES"
      filtervars <- {}
      #filterlstst <- c(pltvarlst, condvarlst)
      filterlstst <- c(pltvarlst)
      filterlst <- filterlstst

      while (addfilter == "YES") {
        filtervar <- select.list(c("NONE", sort(filterlst)), 
		      title=paste("Filter variable -", stabbr), multiple=FALSE)
        if (filtervar == "") stop("")
        if (filtervar == "NONE") {
          break
        } else if (filtervar %in% pltvarlst) {
          filterALIAS <- "p"
        } else {
          filterALIAS <- "c"
        }
        filterfromqry <- pfromqry
            
        filtervars <- c(filtervars, filtervar)
        filterlst <- filterlst[filterlst != filtervar]
        filtervarx <- paste0(filterALIAS, ".", filtervar)

        filterdbqry <- paste0("select distinct ", filtervarx, 
		                      " \nfrom ", filterfromqry,
							  " \nwhere ", evalFilter)
        filterdb <- sort(na.omit(sqldf::sqldf(filterdbqry)[[1]]))
        
        if (filtervar %in% c("ELEV", "CRCOVPCT_RMRS", "CRCOVPCT_LIVEMISS_RMRS", 
			  "CRCOVPCT_LIVE_RMRS", "LIVE_CANOPY_CVR_PCT", "LIVE_MISSING_CANOPY_CVR_PCT") ||
			      length(filterdb) > 20) {
          ## MINIMUM VALUE
          filtercd_min <- select.list(as.character(filterdb), 
			      title=paste("Select MIN", filtervar), multiple=FALSE)
          if (filtercd_min == "") stop("")
      
          filterdbmax <- filterdb[as.numeric(filterdb) >= as.numeric(filtercd_min)]
          ## MAXIMUM VALUE
          filtercd_max <- select.list(as.character(filterdbmax), 
			      title=paste("Select MAX", filtervar), multiple=FALSE)
          if (filtercd_max == "") stop("")
          
          stateFilters <- paste(stateFilters, "and (", filtervarx, ">=", filtercd_min, 
			      "and", filtervarx, "<=", filtercd_max, ")")
        } else {      
          filtercd <- select.list(as.character(filterdb), 
			      title="Select filter code(s)", multiple=TRUE)
          if (length(filtercd) == 0) stop("")
          stateFilters <- paste0(stateFilters, " and ", filtervarx, " in(", toString(filtercd), ")")
        }

        addfilter <- select.list(c("NO", "YES"), 
			title=paste("Another filter? -", stabbr), multiple=FALSE)
        if (addfilter == "") stop("")
      }
      if (i == 1 && length(states) > 1) {
        resp <- select.list(c("YES", "NO"), title="Same for all states", multiple=FALSE)
        if (resp == "YES") gui <- FALSE
      }
    } 
    if (!is.null(stateFilter)) {
      stateFilters <- paste(" and", stateFilter)
    } else {
      stateFilters <- ""
    }

    ## SET QUERY FILTER
    xfilter <- paste0(evalFilter, stateFilters)
    message(paste(stcd, "-", xfilter))


    #####################################################################################
    ###################################    RUN QUERIES   ################################
    #####################################################################################

    ## Run pltcond query
    #####################################################################################
    if (datsource == "datamart" && is.null(PLOT)) {
      pltcondx <- NULL
    } else {

      if (is.null(plotgeomnm)) {
        addplotgeom <- FALSE
      }

      if (!defaultVars) {
        pltvarlst <- pltflds
        condvarlst <- condflds
        #if (addplotgeom) {
        #  pgeomvarlst <- plotgeomflds
        #}
      }        

      ## Check variables in database
	  if (!"LON" %in% pltcondflds) {
	    lonfld <- findnm("LON_PUBLIC", pltcondflds, returnNULL=TRUE)
		if (!is.null(lonfld) && !"LON_PUBLIC" %in% pltvarlst && "LON" %in% names(pltvarlst)) { 
		  pltvarlst <- sub("LON", "LON_PUBLIC", pltvarlst)
		}
	  }
	  if (!"LAT" %in% pltcondflds) {
	    latfld <- findnm("LAT_PUBLIC", pltcondflds, returnNULL=TRUE)
		if (!is.null(latfld) && !"LAT_PUBLIC" %in% pltvarlst && "LAT" %in% names(pltvarlst)) { 
		  pltvarlst <- sub("LATN", "LAT_PUBLIC", pltvarlst)
		}
	  }
      pltvarlst <- pltvarlst[pltvarlst %in% pltcondflds]
      condvarlst <- condvarlst[condvarlst %in% pltcondflds]

      ## Add commas
      pcvars <- NULL
      if (length(pltvarlst) > 0) {
        pcvars <- toString(paste0("p.", pltvarlst))
        if (addplotgeom && !is.null(plotgeomnm)) {
          pgeomvarlst <- pgeomvarlst[pgeomvarlst %in% plotgeomflds]
          pcvars <- toString(c(paste0("p.", pltvarlst), paste0("pg.", pgeomvarlst))) 
        }           
      } 
      pcvars <- toString(c(pcvars, paste0("c.", condvarlst)))

      # if (iseval) { 
        # evalidnm <- findnm("EVALID", ppsaflds, returnNULL=TRUE)
        # if (is.null(evalidnm)) {
          # evalidnm <- findnm("EVALID", pltcondflds)
          # pcvars <- paste0(pcvars, ", p.", evalidnm)
        # } else {           
          # pcvars <- paste0(pcvars, ", ppsa.", evalidnm)
        # }
      # }

      ## Create pltcond query
      if (addplotgeom) {
        pltcond.qry <- paste("select distinct", pcvars, 
		                     "\nfrom", pcgeomfromqry, 
                             "\nwhere", xfilter)
      } else {  
        pltcond.qry <- paste("select distinct", pcvars, 
		                     "\nfrom", pcfromqry, 
                             "\nwhere", xfilter)
      }
	  if (!"pltcond" %in% names(dbqueries[[state]])) {
        dbqueries[[state]]$pltcond <- pltcond.qry
	  }

      ## Run pltcond query
      message(pltcond.qry)

      if (datsource == "sqlite") {
        tryCatch( pltcondx <- DBI::dbGetQuery(dbconn, pltcond.qry),
			#error=function(e) message("pltcond query is invalid"))
			error=function(e) message(e, "\n"))
      } else {
        tryCatch( pltcondx <- setDT(sqldf::sqldf(pltcond.qry, stringsAsFactors=FALSE)),
			error=function(e) message("pltcond query is invalid"))
      }

      ## Write query to outfolder
      if (saveqry) {
        pltcondqryfn <- DBgetfn("pltcond", invtype, outfn.pre, stabbr, 
		      evalid=evalid, qry=TRUE, outfolder=outfolder, overwrite=overwrite_layer, 
		      outfn.date=outfn.date, ext="txt")
        outfile <- file(pltcondqryfn, "w")
        cat(  pltcond.qry, "\n", file=outfile)
        close(outfile)
        message("saved pltcond query to:\n", pltcondqryfn)
      }
    }

    if (nrow(pltcondx) == 0) {
      message("no plots in database for ", state)
    } else {
      pltvarlst2 <- pltvarlst
      if (addplotgeom) {
        pltvarlst2 <- unique(c(pltvarlst2, pgeomvarlst))
      }
      #if (iseval) pltvarlst2 <- c(pltvarlst2, "EVALID")
      condvarlst2 <- condvarlst
 
      ## Filter pltcond with allFilter      
      ###########################################
      pltcondx <- datFilter(x=pltcondx, xfilter=allFilter)$xf
 
      ## Tag ACI plots
      ###########################################################
      if (ACI && all("NF_SAMPLING_STATUS_CD", "NF_COND_STATUS_CD") %in% names(pltcondx)) {
        pltcondx[, c("ACI", "ACI_NFS") := 0,]
        pltcondx[NF_SAMPLING_STATUS_CD == 1 &
			!is.na(NF_COND_STATUS_CD) & NF_COND_STATUS_CD == 2,
			ACI_NFS:= 1]
        pltcondx[NF_SAMPLING_STATUS_CD == 1 &
			!is.na(NF_COND_STATUS_CD) & NF_COND_STATUS_CD == 2 &
			OWNGRPCD == 10, ACI := 1]
        condvarlst2 <- c(condvarlst2, "ACI", "ACI_NFS")
      }

      ## Separate pltcondx into 2 tables (pltx, condx)
      ###########################################################
      if (!is.null(pltvarlst2)) {
        pltx <- unique(pltcondx[, pltvarlst2, with=FALSE])
        pltx[, CN := as.character(CN)]
        setkey(pltx, CN)
        if ("PREV_PLTCN" %in% names(pltx)) {
          pltx[, PREV_PLTCN := as.character(PREV_PLTCN)]  
        }           
      }         
      if (!is.null(condvarlst) && "CONDID" %in% names(pltcondx)) {
        condx <- unique(pltcondx[, condvarlst2, with=FALSE])
        condx[, PLT_CN := as.character(PLT_CN)]        
        setkey(condx, PLT_CN, CONDID)
      }
       
	  ## Change names of LON and LAT to LON_PUBLIC and LAT_PUBLIC
      ###########################################################
      if ("LON" %in% names(pltx)) {
        setnames(pltx, "LON", "LON_PUBLIC")
        pltvarlst2[pltvarlst2 == "LON"] <- "LON_PUBLIC"
        #xy_opts$xvar <- "LON_PUBLIC"
      }
      if ("LAT" %in% names(pltx)) {
        setnames(pltx, "LAT", "LAT_PUBLIC")
        pltvarlst2[pltvarlst2 == "LAT"] <- "LAT_PUBLIC"
        #xy_opts$xvar <- "LON_PUBLIC"
      }
      if ("ELEV" %in% names(pltx)) {
        setnames(pltx, "ELEV", "ELEV_PUBLIC")
        pltvarlst2[pltvarlst2 == "ELEV"] <- "ELEV_PUBLIC"
      }

      ## Create plot-level, number of condtion variables
      ###########################################################
      if ("COND_STATUS_CD" %in% names(condx) && !"NBRCND" %in% names(pltx)) {

        ## Number of conditions
        nbrcnd <- condx[, list(NBRCND = length(COND_STATUS_CD)), by="PLT_CN"]
        nbrcndsamp <- condx[COND_STATUS_CD != 5, 
			list(NBRCNDSAMP = length(COND_STATUS_CD)), by="PLT_CN"]
        nbrcndfor <- condx[COND_STATUS_CD == 1, 
			list(NBRCNDFOR = length(COND_STATUS_CD)), by="PLT_CN"]
        nbrcndftyp <- condx[COND_STATUS_CD == 1 & FORTYPCD > 0, 
			list(NBRCNDFTYP = length(FORTYPCD)), by="PLT_CN"]

        ## Merge new condition variables together
        nbrcnd <- nbrcndsamp[nbrcnd]
        nbrcnd <- nbrcndfor[nbrcnd]
        nbrcnd <- nbrcndftyp[nbrcnd]
        nbrcnd[is.na(nbrcnd)] <- 0
        setkeyv(nbrcnd, "PLT_CN")

        rm(nbrcndsamp)
        rm(nbrcndfor)
        rm(nbrcndftyp)

        ## Merge to plot table
        pltx <- nbrcnd[pltx]
        setkeyv(pltx, "PLT_CN")
        rm(nbrcnd)
        # gc()

        nbrcndlst <- c("NBRCND", "NBRCNDSAMP", "NBRCNDFOR", "NBRCNDFTYP")
        pltvarlst2 <- unique(c(pltvarlst2, nbrcndlst))      
      }

      ## CCLIVEPLT:
      ## A plot level canopy cover variable based on LIVE_CANOPY_CVR_PCT
      if (all(c("LIVE_CANOPY_CVR_PCT", "CONDPROP_UNADJ") %in% names(condx)) &&
			  (!"CCLIVEPLT" %in% names(pltx))) {
        ccliveplt <- condx[, 
			        round(sum(LIVE_CANOPY_CVR_PCT * CONDPROP_UNADJ, na.rm=TRUE),2), 
			        by=PLT_CN]
        setnames(ccliveplt, c("PLT_CN", "CCLIVEPLT"))

        pltx <- ccliveplt[pltx]
        pltvarlst2 <- c(pltvarlst2, "CCLIVEPLT")
      }
 
      ## Regional variables 
      ######################################################################
      if (isRMRS && regionVars) {
        ## CCRMRSPLT: plot level canopy cover variable based on CRCOVPCT_RMRS
        if (all(c("CRCOVPCT_RMRS", "CONDPROP_UNADJ") %in% names(pltx))) {
          ccRMRSplt <- condx[, list(round(sum(CRCOVPCT_RMRS * CONDPROP_UNADJ, 
			        na.rm=TRUE), 2)), by="PLT_CN"]
          setnames(ccRMRSplt, c("PLT_CN", "CCRMRSPLT"))
          pltx <- ccRMRSplt[pltx]
          pltvarlst2 <- c(pltvarlst2, "CCRMRSPLT")
        }
        ## CCPLT: plot level canopy cover variable based on CRCOV
        if (all(c("CRCOV", "CONDPROP_UNADJ") %in% names(pltx))) {
          ccplt <- condx[, list(round(sum(CRCOVPCT_RMRS * CONDPROP_UNADJ, 
			na.rm=TRUE), 2)), by="PLT_CN"]
          setnames(ccplt, c("PLT_CN", "CCPLT"))
          pltx <- ccplt[pltx]

          pltvarlst2 <- c(pltvarlst2, "CCRMRSPLT")
        }
      }  

      ## FORNONSAMP: 
      ## Plot-level variable based on PLOT_STATUS_CD and PLOT_NONSAMPLE_REASN_CD
      if ("PLOT_NONSAMPLE_REASN_CD" %in% names(pltx) && !"FORNONSAMP" %in% names(pltx)) {
        pltx[, FORNONSAMP := 
		      ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 2, 
			      "Nonsampled-Denied access",
		      ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 3, 
			      "Nonsampled-Hazardous",
		      ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD %in% c(5,6),
		 	      "Nonsampled-Lost data",
		      ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 7, 
			      "Nonsampled-Wrong location",
		      ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 8, 
			      "Nonsampled-Skipped visit",
		      ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 9, 
			      "Nonsampled-Dropped plot",
		      ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD %in% c(10,11),
 			      "Nonsampled-Other",
		      ifelse(PLOT_STATUS_CD == "1", "Sampled-Forest",
		      ifelse(PLOT_STATUS_CD == "2", "Sampled-Nonforest",
		            as.character(pltx$PLOT_STATUS_CD))))))))))]

          pltvarlst2 <- c(pltvarlst2, "FORNONSAMP")
      }

      ## Generate PLOT_ID, with STATECD, UNIT, COUNTYCD, PLOT to define
      if (all(c("STATECD", "UNITCD", "COUNTYCD", "PLOT") %in% names(pltx)) && 
		!"PLOT_ID" %in% names(pltx)) {
        pltx[, PLOT_ID := paste0("ID", 
		        formatC(pltx$STATECD, width=2, digits=2, flag=0), 
          	formatC(pltx$UNITCD, width=2, digits=2, flag=0),
          	formatC(pltx$COUNTYCD, width=3, digits=3, flag=0),
          	formatC(pltx$PLOT, width=5, digits=5, flag=0))] 
        pltvarlst2 <- c(pltvarlst2, "PLOT_ID")
      }

      ## Additional condition variables
      ######################################################################
      ref_fortypgrp <- FIESTAutils::ref_codes[FIESTAutils::ref_codes$VARIABLE == "FORTYPCD",]

      ## FORTYPGRP: condition level variable grouping FORTYPCD
      cndnames <- names(condx)
      if ("FORTYPCD" %in% cndnames && !"FORTYPGRPCD" %in% cndnames) {
        condx <- merge(condx, ref_fortypgrp[,c("VALUE", "GROUPCD")],
        		by.x="FORTYPCD", by.y="VALUE", all.x=TRUE)
        setnames(condx, "GROUPCD", "FORTYPGRPCD")
        setcolorder(condx, c(cndnames, "FORTYPGRPCD"))   
        condvarlst2 <- unique(c(condvarlst2, "FORTYPGRPCD"))
      }
      ## FLDTYPGRP: condition level variable grouping FLDTYPGRP
      if ("FLDTYPCD" %in% cndnames && !"FLDTYPGRPCD" %in% cndnames) {
        condx <- merge(condx, ref_fortypgrp[,c("VALUE", "GROUPCD")], 
               by.x="FLDTYPCD", by.y="VALUE", all.x=TRUE)
        setnames(condx, "GROUPCD", "FLDTYPGRPCD")
        setcolorder(condx, c(cndnames, "FLDTYPGRPCD"))
        condvarlst2 <- unique(c(condvarlst2, "FLDTYPGRPCD"))
      }
      setkey(condx, "PLT_CN", "CONDID")

      ## TIMBERCD condition level variable defining TIMBERLAND conditions
      if ("SITECLCD" %in% cndnames && !"TIMBERCD" %in% cndnames) {
        condx[COND_STATUS_CD == 1, TIMBERCD := 2]
        condx[SITECLCD %in% 1:6, TIMBERCD := 1]
        condvarlst2 <- unique(c(condvarlst2, "TIMBERCD"))
      }

      ## LANDUSECD
      ## A combination of PRESNFCD and COND_STATUS_CD
      if (all(c("PRESNFCD", "COND_STATUS_CD") %in% cndnames) && !"LANDUSECD" %in% cndnames) {
        condx$LANDUSECD <- with(condx, ifelse(is.na(PRESNFCD), COND_STATUS_CD, PRESNFCD))
        condvarlst2 <- unique(c(condvarlst2, "LANDUSECD"))
      }
 
      if ("PLT_CN" %in% names(pltx) && !"CN" %in% names(pltx)) {
        setnames(pltx, "PLT_CN", "CN")
      }
      pltx <- pltx[, pltvarlst2, with=FALSE]
      setkeyv(pltx, "CN")
 
      ## Create combined unique identifier to subset other tables
      pcondID <- condx[, paste(PLT_CN, CONDID)]
    }

    ## Check if change is possible
    if (ischng && (!"PREV_PLT_CN" %in% names(pltx) || all(is.na(pltx$PREV_PLT_CN)))) {
      if (!"PREV_PLT_CN" %in% names(pltx)) {
        message("must include PREV_PLT_CN in data")
      } else {
        message("no previous plots exist for this dataset")
      }
      nochngdata <- TRUE
    }

    ###############################################################
    ## Get unioned change tables 
    ###############################################################
    if (all(ischng, !nochngdata, !is.null(pltx))) {
     
      pcvarsa <- toString(c(paste0("p.", pltvarlst), paste0("c.", condvarlst)))
      pcvarsb <- toString(c(paste0("pplot.", pltvarlst), paste0("pcond.", condvarlst)))

      if (addplotgeom) {
        chgfromqry <- paste0(pcfromqry,
           " \nJOIN ", SCHEMA., plot_layer, " pplot ON (pplot.CN = p.PREV_PLT_CN)",
		   " \nJOIN ", SCHEMA., cond_layer, " pcond ON (pcond.PLT_CN = p.PREV_PLT_CN)")
      } else {
        chgfromqry <- paste0(pcfromqry,
           " \nJOIN ", SCHEMA., plot_layer, " pplot ON (pplot.CN = p.PREV_PLT_CN)",
		   " \nJOIN ", SCHEMA., cond_layer, " pcond ON (pcond.PLT_CN = p.PREV_PLT_CN)")
      }

      ## Unioned condition table
      pltconduqrya <- paste("select distinct", pcvarsa,
							"\nfrom", chgfromqry,
							"\nwhere", paste0(evalFilter.grm, stateFilters))
      pltconduqryb <- paste("select distinct", pcvarsb,
	                        "\nfrom", chgfromqry, 
							"\nwhere", paste0(evalFilter.grm, stateFilters))
      pltcondu.qry <- paste(pltconduqrya, "\nUNION\n", pltconduqryb)
	  
	  if (!"pltcondu" %in% names(dbqueries[[state]])) {
        dbqueries[[state]]$pltcondu <- pltcondu.qry
	  }


      ## Run pltcondu query
      #####################################################################################
      stat <- paste("## STATUS: GETTING PLOT/COND CHANGE DATA (", stabbr, ") ...")
      cat("\n", stat, "\n")

      if (datsource == "sqlite") {
        pltcondux <- tryCatch(DBI::dbGetQuery(dbconn, pltcondu.qry),
			error=function(e) message("pltcondu query is invalid"))
      } else {
        pltcondux <- tryCatch(setDT(sqldf::sqldf(pltcondu.qry, stringsAsFactors=FALSE)),
			error=function(e) message("pltcondu query is invalid"))
      }
	  if (is.null(pltcondux)) {
	    message(pltcondu.qry)
	  }

      ## Write query to outfolder
      if (saveqry) {
        pltconduqryfn <- DBgetfn("pltcondu", invtype, outfn.pre, stabbr,
		evalid=evalid, qry=TRUE, outfolder=outfolder, overwrite=overwrite_layer,
		outfn.date=outfn.date, ext="txt")
        outfile <- file(pltconduqryfn, "w")
        cat(  pltcondu.qry, "\n", file=outfile)
        close(outfile)
      }

      pltvarlst2 <- pltvarlst
      if (addplotgeom) {
        pltvarlst2 <- unique(c(pltvarlst2, pgeomvarlst))
      }
      #if (iseval) pltvarlst2 <- c(pltvarlst2, "EVALID")
      condvarlst2 <- condvarlst

      ## Filter pltcondu with allFilter
      ###########################################
      pltcondux <- datFilter(x=pltcondux, xfilter=allFilter)$xf

      ## Tag ACI plots
      ###########################################################
      if (ACI && all("NF_SAMPLING_STATUS_CD", "NF_COND_STATUS_CD") %in% names(pltcondux)) {
        pltcondux[, c("ACI", "ACI_NFS") := 0,]
        pltcondux[NF_SAMPLING_STATUS_CD == 1 &
			!is.na(NF_COND_STATUS_CD) & NF_COND_STATUS_CD == 2,
			ACI_NFS:= 1]
        pltcondux[NF_SAMPLING_STATUS_CD == 1 &
			!is.na(NF_COND_STATUS_CD) & NF_COND_STATUS_CD == 2 &
			OWNGRPCD == 10, ACI := 1]
        condvarlst2 <- c(condvarlst2, "ACI", "ACI_NFS")
      }

      ## Separate pltcondx into 2 tables (pltx, condx)
      ###########################################################
      if (!is.null(pltvarlst2)) {
        pltux <- unique(pltcondux[, pltvarlst2, with=FALSE])
        pltux[, CN := as.character(CN)]
        setkey(pltux, CN)
        if ("PREV_PLTCN" %in% names(pltux))
          pltux[, PREV_PLTCN := as.character(PREV_PLTCN)]
      }
      if (!is.null(condvarlst) && "CONDID" %in% names(pltcondux)) {
        condux <- unique(pltcondx[, condvarlst2, with=FALSE])
        condux[, PLT_CN := as.character(PLT_CN)]
        setkey(condux, PLT_CN, CONDID)
      }

      ## Change names of LON and LAT to LON_PUBLIC and LAT_PUBLIC
      ###########################################################
      if ("LON" %in% names(pltux)) {
        setnames(pltux, "LON", "LON_PUBLIC")
        pltvarlst2[pltvarlst2 == "LON"] <- "LON_PUBLIC"
      }
      if ("LAT" %in% names(pltux)) {
        setnames(pltux, "LAT", "LAT_PUBLIC")
        pltvarlst2[pltvarlst2 == "LAT"] <- "LAT_PUBLIC"
      }
      if ("ELEV" %in% names(pltux)) {
        setnames(pltux, "ELEV", "ELEV_PUBLIC")
        pltvarlst2[pltvarlst2 == "ELEV"] <- "ELEV_PUBLIC"
      }


      ## Create plot-level, number of condtion variables
      ###########################################################
      if (defaultVars) {

        ## Number of conditions
        nbrcnd <- condux[, list(NBRCND = length(COND_STATUS_CD)), by="PLT_CN"]
        nbrcndsamp <- condux[COND_STATUS_CD != 5,
			list(NBRCNDSAMP = length(COND_STATUS_CD)), by="PLT_CN"]
        nbrcndfor <- condux[COND_STATUS_CD == 1,
			list(NBRCNDFOR = length(COND_STATUS_CD)), by="PLT_CN"]
        nbrcndftyp <- condux[COND_STATUS_CD == 1 & FORTYPCD > 0,
			list(NBRCNDFTYP = length(FORTYPCD)), by="PLT_CN"]

        ## Merge new condition variables together
        nbrcnd <- nbrcndsamp[nbrcnd]
        nbrcnd <- nbrcndfor[nbrcnd]
        nbrcnd <- nbrcndftyp[nbrcnd]
        nbrcnd[is.na(nbrcnd)] <- 0
        setkeyv(nbrcnd, "PLT_CN")

        rm(nbrcndsamp)
        rm(nbrcndfor)
        rm(nbrcndftyp)

        ## Merge to plt table
        pltux <- nbrcnd[pltux]

        nbrcndlst <- c("NBRCND", "NBRCNDSAMP", "NBRCNDFOR", "NBRCNDFTYP")
        pltvarlst2 <- c(pltvarlst2, nbrcndlst)


        ## CCLIVEPLT:
        ## A plot level canopy cover variable based on LIVE_CANOPY_CVR_PCT
        if (all(c("LIVE_CANOPY_CVR_PCT", "CONDPROP_UNADJ") %in% names(condux))) {
          ccliveplt <- condux[,
			round(sum(LIVE_CANOPY_CVR_PCT * CONDPROP_UNADJ, na.rm=TRUE),2),
			by=PLT_CN]
          setnames(ccliveplt, c("PLT_CN", "CCLIVEPLT"))

          pltux <- ccliveplt[pltux]
          pltvarlst2 <- c(pltvarlst2, "CCLIVEPLT")
        }

        ## Regional variables
        ######################################################################
        if (isRMRS && regionVars) {
          ## CCRMRSPLT: plot level canopy cover variable based on CRCOVPCT_RMRS
          if (all(c("CRCOVPCT_RMRS", "CONDPROP_UNADJ") %in% names(condux))) {
            ccRMRSplt <- condux[, list(round(sum(CRCOVPCT_RMRS * CONDPROP_UNADJ,
                                                na.rm=TRUE), 2)), by="PLT_CN"]
            setnames(ccRMRSplt, c("PLT_CN", "CCRMRSPLT"))
            pltux <- ccRMRSplt[pltux]

            pltvarlst2 <- c(pltvarlst2, "CCRMRSPLT")
          }
          ## CCPLT: plot level canopy cover variable based on CRCOV
          if (all(c("CRCOV", "CONDPROP_UNADJ") %in% names(condux))) {
            ccplt <- condux[, list(round(sum(CRCOVPCT_RMRS * CONDPROP_UNADJ,
                                            na.rm=TRUE), 2)), by="PLT_CN"]
            setnames(ccplt, c("PLT_CN", "CCPLT"))
            pltux <- ccplt[pltux]

            pltvarlst2 <- c(pltvarlst2, "CCRMRSPLT")
          }
        }

        ## FORNONSAMP:
        ## Plot-level variable based on PLOT_STATUS_CD and PLOT_NONSAMPLE_REASN_CD
        if ("PLOT_NONSAMPLE_REASN_CD" %in% names(pltux)) {
          pltux[, FORNONSAMP :=
		ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 2,
			"Nonsampled-Denied access",
		ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 3,
			"Nonsampled-Hazardous",
		ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD %in% c(5,6),
			"Nonsampled-Lost data",
		ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 7,
			"Nonsampled-Wrong location",
		ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 8,
			"Nonsampled-Skipped visit",
		ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 9,
			"Nonsampled-Dropped plot",
		ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD %in% c(10,11),
			"Nonsampled-Other",
		ifelse(PLOT_STATUS_CD == "1", "Sampled-Forest",
		ifelse(PLOT_STATUS_CD == "2", "Sampled-Nonforest",
		as.character(pltux$PLOT_STATUS_CD))))))))))]

          pltvarlst2 <- c(pltvarlst2, "FORNONSAMP")
        }

        ## Generate PLOT_ID, with STATECD, UNITCD, COUNTYCD, PLOT to define
        pltux[, PLOT_ID := paste0("ID",
            formatC(pltux$STATECD, width=2, digits=2, flag=0),
          	formatC(pltux$UNITCD, width=2, digits=2, flag=0),
          	formatC(pltux$COUNTYCD, width=3, digits=3, flag=0),
          	formatC(pltux$PLOT, width=5, digits=5, flag=0))]
        pltvarlst2 <- c(pltvarlst2, "PLOT_ID")


        ## Additional condition variables
        ######################################################################
        ref_fortypgrp <- FIESTAutils::ref_codes[FIESTAutils::ref_codes$VARIABLE == "FORTYPCD",]

        ## FORTYPGRP: condition level variable grouping FORTYPCD
        cndnames <- names(condux)
        if ("FORTYPCD" %in% names(condux)) {
          condux <- merge(condux, ref_fortypgrp[,c("VALUE", "GROUPCD")],
        		by.x="FORTYPCD", by.y="VALUE", all.x=TRUE)
          setnames(condux, "GROUPCD", "FORTYPGRPCD")
          setcolorder(condux, c(cndnames, "FORTYPGRPCD"))

          condvarlst2 <- c(condvarlst2, "FORTYPGRPCD")
        }
        ## FLDTYPGRP: condition level variable grouping FLDTYPGRP
        if ("FLDTYPCD" %in% names(condux)) {
          condux <- merge(condux, ref_fortypgrp[,c("VALUE", "GROUPCD")],
               by.x="FLDTYPCD", by.y="VALUE", all.x=TRUE)
          setnames(condux, "GROUPCD", "FLDTYPGRPCD")
          setcolorder(condux, c(cndnames, "FLDTYPGRPCD"))

          condvarlst2 <- c(condvarlst2, "FLDTYPGRPCD")
        }
        setkey(condux, PLT_CN, CONDID)

        ## TIMBERCD condition level variable defining TIMBERLAND conditions
        if ("SITECLCD" %in% names(condux)) {
          condux[COND_STATUS_CD == 1, TIMBERCD := 2]
          condux[SITECLCD %in% 1:6, TIMBERCD := 1]

          condvarlst2 <- c(condvarlst2, "TIMBERCD")
        }
      }   ##  End (defaultVars)

      setnames(pltux, "PLT_CN", "CN")
      setkeyv(pltux, "CN")

      pltux <- pltux[, pltvarlst2, with=FALSE]

      if (returndata) {
	  	if ("pltu" %in% names(tabs)) {
          tabs$pltu <- rbind(tabs$pltu, data.frame(pltux))
	    } else {
	      tabs$pltu <- data.frame(pltux)
	    }
 	    if (!"pltu" %in% names(tabIDs)) {
          tabIDs$pltu <- "CN"
	    }
      }

      if (savedata) {
        message("saving pltu and condu tables...")
        index.unique.pltux = index.pltux <- NULL
        if (!append_layer) {
          index.unique.pltux <- c("CN")
          if (all(c("STATECD", "UNITCD", "COUNTYCD", "PLOT") %in% names(pltux))) {
            index.pltux <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT")
          }
        } 
        datExportData(pltux, 
            index.unique = index.unique.pltux,
            index = index.pltux,
            savedata_opts = list(outfolder = outfolder, 
                                   out_fmt = out_fmt, 
                                   out_dsn = out_dsn, 
                                   out_layer = "plotu",
                                   outfn.pre = outfn.pre, 
                                   overwrite_layer = overwrite_layer,
                                   append_layer = append_layer,
                                   outfn.date = outfn.date, 
                                   add_layer = TRUE))
        rm(pltux)

        index.unique.condux <- NULL
        if (!append_layer) index.unique.condux <- c("PLT_CN", "CONDID")
        datExportData(condux, 
            index.unique = index.unique.condux,
            savedata_opts = list(outfolder = outfolder, 
                                   out_fmt = out_fmt, 
                                   out_dsn = out_dsn, 
                                   out_layer = "condu",
                                   outfn.pre = outfn.pre, 
                                   overwrite_layer = overwrite_layer,
                                   append_layer = append_layer,
                                   outfn.date = outfn.date, 
                                   add_layer = TRUE))
        rm(condux)
        # gc()   
      } 
	  
	  ##############################################################
      ## Area Change Matrix (SUBP_COND_CHNG_MTRX)
      ##############################################################
      message("\n",
        "## STATUS: Getting change data from SUBP_COND_CHNG_MTRX (", stabbr, ") ...", "\n")

      ## ssmx data
      ############################################
      if (datsource == "sqlite") {
        sccmnm <- chkdbtab(dbtablst, sccm_layer)
        if (is.null(sccmnm)) {
          message("there is no subp_cond_chng_mtrx table in database")
          islulc=isgrm <- FALSE
        } else {
          ## Get SUBP_COND_CHNG_MTRX fields
          sccmflds <- DBI::dbListFields(dbconn, sccmnm)
        }
      } else if (datsource == "datamart") {
        SUBP_COND_CHNG_MTRX <- DBgetCSV("SUBP_COND_CHNG_MTRX", stabbr, 
		      returnDT=TRUE, stopifnull=FALSE)
        if (!is.null(SUBP_COND_CHNG_MTRX)) {
          sccmnm <- "SUBP_COND_CHNG_MTRX"
          sccmflds <- names(SUBP_COND_CHNG_MTRX)
        }
      } else if (datsource %in% c("csv", "obj")) {
        SUBP_COND_CHNG_MTRX <- pcheck.table(sccm_layer, 
					stopifnull=TRUE, stopifinvalid=TRUE)
        if (!is.null(SUBP_COND_CHNG_MTRX)) {
          sccmnm <- "SUBP_COND_CHNG_MTRX"
          names(SUBP_COND_CHNG_MTRX) <- toupper(names(SUBP_COND_CHNG_MTRX))
          sccmflds <- names(SUBP_COND_CHNG_MTRX)
        }
      }

      if (is.null(sccmnm)) {
        sccmx <- NULL
        ischng=islulc <- FALSE

      } else {
        ## SUBP_COND_CHNG_MTRX table
        sccmvarlst <- c("PLT_CN", "PREV_PLT_CN", "SUBP", "SUBPTYP",
                         "CONDID", "PREVCOND", "SUBPTYP_PROP_CHNG")

        ## Check variables in database
        if (defaultVars) {
          sccmvarlst <- sccmvarlst[sccmvarlst %in% sccmflds]
          sccmvars <- toString(paste0("sccm.", sccmvarlst))
        } else {
          sccmvars <- "sccm.*"
        }

        ## Create query for SUBP_COND_CHNG_MTRX
        sccmfromqry <- paste0(chgfromqry, " \nJOIN ", SCHEMA., 
				"SUBP_COND_CHNG_MTRX sccm ON (sccm.PLT_CN = c.PLT_CN
												AND sccm.PREV_PLT_CN = pcond.PLT_CN
												AND sccm.CONDID = c.CONDID 
												AND sccm.PREVCOND = pcond.CONDID)")

        sccm.qry <- paste("select", sccmvars, 
		                  " \nfrom", sccmfromqry, 
                          " \nwhere", paste0(evalFilter.grm, stateFilters))

 	    ## Query SQLite database or R object
        if (datsource == "sqlite") {
          sccmx <- tryCatch( setDT(DBI::dbGetQuery(dbconn, sccm.qry)),
			  error=function(e) {
                    message("SUBP_COND_CHNG_MTRX query is invalid")
                    return(NULL) })
        } else {
          sccmx <- tryCatch( setDT(sqldf::sqldf(sccm.qry, stringsAsFactors=FALSE)),
			  error=function(e) {
                    message("SUBP_COND_CHNG_MTRX query is invalid")
                    return(NULL) })
        }
        if (is.null(sccmx)) {
		  message(sccm.qry)
		}
        if (!is.null(sccmx) && nrow(sccmx) != 0) {
	  	  if (!"subp_cond_chng_mtrx" %in% names(dbqueries[[state]])) {
            dbqueries[[state]]$subp_cond_chng_mtrx <- sccm.qry
	      }

          sccmx <- setDT(sccmx)
          sccmx[, PLT_CN := as.character(PLT_CN)]
          setkey(sccmx, PLT_CN, CONDID)

          ## Subset overall filters from condx
          sccmx <- sccmx[paste(sccmx$PLT_CN, sccmx$CONDID) %in% pcondID,]

          if (returndata) {
		  	if ("subp_cond_chng_mtrx" %in% names(tabs)) {
              tabs$subp_cond_chng_mtrx <- rbind(tabs$subp_cond_chng_mtrx, data.frame(sccmx))
	        } else {
	          tabs$subp_cond_chng_mtrx <- data.frame(sccmx)
	        }
 	        if (!"subp_cond_chng_mtrx" %in% names(tabIDs)) {
              tabIDs$subp_cond_chng_mtrx <- "PLT_CN"
	        }
          }
          if (savedata) {
            index.unique.sccmx <- NULL
            if (!append_layer) index.unique.sccmx <- c("PLT_CN", "CONDID")
              datExportData(sccmx, 
                  index.unique = index.unique.sccmx,
                  savedata_opts = list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="sccm",
                                outfn.pre=outfn.pre, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                outfn.date=outfn.date, 
                                add_layer=TRUE)) 
            rm(sccmx)
            # gc() 
          } 
        }
        if (datsource == "datamart") {
          if (exists("SUBP_COND_CHNG_MTRX")) rm(SUBP_COND_CHNG_MTRX)
          # gc()
		}
      }
    }

    ##############################################################
    ## Tree data
    ##############################################################
    if ((istree || !is.null(alltFilter)) && !is.null(pltx)) {

      if (datsource == "sqlite") {
        treenm <- chkdbtab(dbtablst, tree_layer)
        if (is.null(treenm)) {
          message("there is no tree table in database")
        }
        ## Get TREE fields
        treeflds <- DBI::dbListFields(dbconn, treenm)

      } else if (datsource == "datamart") {
        ## TREE table
        if (istree || !is.null(alltFilter)) {
          TREE <- DBgetCSV("TREE", stabbr, returnDT=TRUE, stopifnull=FALSE)
          if (is.null(TREE)) {
            stop("there is no TREE table in datamart")
          } else {
            treenm <- "TREE"
            treeflds <- names(TREE)
          }
        }
      } else if (datsource %in% c("csv", "obj")) {
        TREE <- pcheck.table(tree_layer, stopifnull=TRUE, stopifinvalid=TRUE)
        if (is.null(TREE)) {
          message("the TREE table is invalid")
        } else {
          treenm <- "TREE"
          names(TREE) <- toupper(names(TREE))
          treeflds <- names(TREE)
        }
      }

      message("\n",
      	"## STATUS: Getting tree data from TREE (", stabbr, ") ...", "\n")
      if (is.null(treenm)) {
        treex <- NULL
        istree <- FALSE
      } else {

        ## Check if sppvars are in ref_species table
        if (!is.null(sppvars)) {
          if (!all(sppvars %in% names(FIESTAutils::ref_species))) {
            missvars <- sppvars[!sppvars %in% names(FIESTAutils::ref_species)]
            message("variables are not in ref_species table: ", toString(missvars))
            sppvars <- NULL
          } else {
            refspp.qry <- paste("select SPCD,", paste(sppvars, collapse=","), 
				          "from ref_species")
            refspp <- sqldf::sqldf(refspp.qry)
          }
        }

        if (defaultVars) {
          tcols <- unique(c(treevarlst, tsumvarlst))
          tcols <- tcols[tcols %in% treeflds]
          ttvars <- toString(paste0("t.", tcols))
          
          if (is.null(ttvars)) {
            stop("no columns in tree table")
          }         
        } else {
          ttvars <- "t.*"
        }
        
        ## Create query for SEEDLING
        tfromqry <- paste0(pfromqry, " \nJOIN ", SCHEMA., 
				treenm, " t ON (t.PLT_CN = p.", puniqueid, ")")

        tree.qry <- paste("select distinct", ttvars, 
		                 "\nfrom", tfromqry, 
						 "\nwhere", xfilter)

	    ## Query SQLite database or R object
        if (datsource == "sqlite") {
          treex <- tryCatch( DBI::dbGetQuery(dbconn, tree.qry),
			error=function(e) {
                    message("TREE query is invalid\n")
                    return(NULL) })
        } else {
          treex <- tryCatch( sqldf::sqldf(tree.qry, stringsAsFactors=FALSE),
			error=function(e) {
                    message("TREE query is invalid")
                    return(NULL) })
        }
        if (!is.null(treex) && nrow(treex) != 0) {
		  if (!"tree" %in% names(dbqueries[[state]])) {
            dbqueries[[state]]$tree <- tree.qry
	      }

          treex <- setDT(treex)
          treex[, PLT_CN := as.character(PLT_CN)]
          treex[, CN := as.character(CN)]
          setkey(treex, CN)

          ## Subset overall filters from condx
          treex <- treex[paste(treex$PLT_CN, treex$CONDID) %in% pcondID,]

          ## Filter treex with alltFilter      
          ###########################################
          if (!is.null(alltFilter)) {
            treex <- datFilter(x=treex, xfilter=alltFilter)$xf
            if (is.null(treex)) {
              pltx=condx <- NULL
            } else {
              pltx <- pltx[pltx$CN %in% treex$PLT_CN, ]
              condx <- condx[condx$PLT_CN %in% treex$PLT_CN, ]
            }
          }
		  
          ## Write query to outfolder
          if (saveqry) {
            treeqryfn <- DBgetfn("tree", invtype,
                                   outfn.pre, stabbr,
                                   evalid = evalid,
                                   qry = TRUE,
                                   outfolder = outfolder,
                                   overwrite = overwrite_layer,
                                   outfn.date = outfn.date, 
                                   ext = "txt")
            outfile <- file(treeqryfn, "w")
            cat(  treeqryfn, "\n", file=outfile)
            close(outfile)
          }

          ## Make sure these variables are numeric
          nbrvars <- c(FIESTAutils::ref_units$VARIABLE, "BHAGE")
          if (any(nbrvars %in% names(treex)))
             nbrvars <- nbrvars[which(nbrvars %in% names(treex))]
          treex[, (nbrvars) := lapply(.SD, check.numeric), .SDcols=nbrvars]

          ## Change NA values to 0 values
          #if (any(names(treex) %in% treenavars))
            #  treex <- DT_NAto0(treex, treenavars)

          if ("DIA" %in% names(treex)) {
            ## Create new tree variables - basal area
            treex[, BA := DIA * DIA * 0.005454]
          }
		  
          ## Create new biomass variables
          if (!is.null(sppvars)) {

            sppsql <- paste("select SPCD,", paste(sppvars, collapse=","),
                              "from ref_species")
            ref_spp <- tryCatch( sqldf::sqldf(sppsql),
                        error=function(e) stop("spp query is invalid"))
            if (!is.null(ref_spp)) {
              treenames <- names(treex)
              treex <- merge(treex, ref_spp, by="SPCD")

              if (biojenk) {
                sppvarsnew <- {}
                if (!"BIOJENK_kg" %in% treenames) {
                  treex[, BIOJENK_kg := exp(JENKINS_TOTAL_B1 + 
                                     JENKINS_TOTAL_B2 * log(DIA * 2.54))]
                  treex[, JENKINS_TOTAL_B1 := NULL][, JENKINS_TOTAL_B2 := NULL]                  
                  sppvarsnew <- c(sppvarsnew[!sppvarsnew %in% 
                          c("JENKINS_TOTAL_B1", "JENKINS_TOTAL_B2")], "BIOJENK_kg")
                }
                if (!"BIOJENK_lb" %in% treenames) {
                  treex[, BIOJENK_lb := BIOJENK_kg * 2.2046] ## Converts back to lbs
                  sppvarsnew <- c(sppvarsnew, "BIOJENK_lb")
                }
              }
              if (greenwt && "DRYBIO_AG" %in% treenames && 
                      !"GREENBIO_AG" %in% treenames) {
                treex[, GREENBIO_AG := DRYBIO_AG * DRYWT_TO_GREENWT_CONVERSION]
                sppvarsnew <- c(sppvarsnew, "DRYWT_TO_GREENWT_CONVERSION")		
              }  
              setcolorder(treex, c(treenames, sppvarsnew))
            }
          }
		  
          ## Append data
          if (treeReturn && returndata) {
			if ("tree" %in% names(tabs)) {
              tabs$tree <- rbind(tabs$tree, data.frame(treex))
			} else {
			  tabs$tree <- data.frame(treex)
			}
			if (!"tree" %in% names(tabIDs)) {
              tabIDs$tree <- "PLT_CN"
			}
          }
          if ((savedata || !treeReturn)) {
            message("saving tree table...")
            index.unique.treex <- NULL
            if (!append_layer) {
              index.unique.treex <- list(c("PLT_CN", "CONDID", "SUBP", "TREE"), "TREE_CN",
				                              "PREV_TREE_CN")
            }
            datExportData(treex, 
                 index.unique = index.unique.treex,
                 savedata_opts = list(outfolder = outfolder, 
                                out_fmt = out_fmt, 
                                out_dsn = out_dsn, 
                                out_layer = "tree",
                                outfn.pre = outfn.pre, 
                                overwrite_layer = overwrite_layer,
                                append_layer = append_layer,
                                outfn.date = outfn.date, 
                                add_layer = TRUE)) 
            #rm(treex)
            #gc()
          }		  
		  
		  ##############################################################
          ## Tree unioned data
          ##############################################################
		  if (all(isgrm, !nochngdata, !is.null(treex))) {
		
		    ttvarsa <- ttvars
		    ttvarsb <- gsub("t.", "ptree.", ttvars)

		    ## Create query for TREE
            tchgfromqry <- paste0(chgfromqry, 
		        " \nJOIN ", SCHEMA.,
				     "TREE t ON (t.CONDID = c.CONDID and t.PLT_CN = p.CN and t.PREVCOND = pcond.CONDID)",
				" \nLEFT OUTER JOIN ", SCHEMA.,
				     "TREE ptree ON (t.PREV_TRE_CN = ptree.CN)")
 

            treea.qry <- paste("select distinct", ttvarsa, 
		                 "\nfrom", tchgfromqry, 
						 "\nwhere", xfilter)
            treeb.qry <- paste("select distinct", ttvarsb, 
		                 "\nfrom", tchgfromqry, 
						 "\nwhere", xfilter)
            treeu.qry <- paste(treea.qry, "UNION", treeb.qry)
            cat("\n", "## STATUS: GETTING TREE CHANGE DATA (", stabbr, ") ...", "\n")


	        ## Query SQLite database or R object
            if (datsource == "sqlite") {
              treeux <- tryCatch( DBI::dbGetQuery(dbconn, treeu.qry),
			    error=function(e) {
                    message("TREE query is invalid\n")
                    return(NULL) })
            } else {
              treeux <- tryCatch( sqldf::sqldf(treeu.qry, stringsAsFactors=FALSE),
			    error=function(e) {
                    message("TREEU query is invalid")
                    return(NULL) })
            }

            if (!is.null(treeux) && nrow(treeux) != 0) {
		      if (!"treeu" %in% names(dbqueries[[state]])) {
                dbqueries[[state]]$treeu <- treeu.qry
	          }

              treeux <- setDT(treeux)
              treeux[, PLT_CN := as.character(PLT_CN)]
              treeux[, CN := as.character(CN)]
              setkey(treeux, CN)

              ## Subset overall filters from condx
              treeux <- treeux[paste(treeux$PLT_CN, treeux$CONDID) %in% pcondID,]

              ## Filter treex with alltFilter
              ###########################################
              if (!is.null(alltFilter)) {
                treeuxf <- datFilter(x=treeux, xfilter=alltFilter)$xf
                if (!is.null(treeuxf)) treeux <- treeuxf

                pltux <- pltux[pltux$CN %in% treeux$PLT_CN, ]
                condux <- condux[condux$PLT_CN %in% treeux$PLT_CN, ]
              }

              ## Check ACI
              #if (!ACI) {
              #  ACIpltID <- condux[COND_STATUS_CD == 1, paste(PLT_CN, CONDID)]
              #  treeux <- treeux[paste(treeux$PLT_CN, treeux$CONDID) %in% ACIpltID,]
              #}

              ## Write query to outfolder
              if (saveqry) {
                treeuqryfn <- DBgetfn("treeu", invtype,
                                   outfn.pre, stabbr,
                                   evalid = evalid,
                                   qry = TRUE,
                                   outfolder = outfolder,
                                   overwrite = overwrite_layer,
                                   outfn.date = outfn.date, 
                                   ext = "txt")
                outfile <- file(treeuqryfn, "w")
                cat(  treeuqryfn, "\n", file=outfile)
                close(outfile)
              }

              ## Make sure these variables are numeric
              nbrvars <- c("DIA", "DRYBIO_BOLE", "DRYBIO_STUMP", "DRYBIO_TOP",
                         "DRYBIO_SAPLING", "DRYBIO_WDLD_SPP", "BHAGE")
              if (any(nbrvars %in% names(treeux)))
                nbrvars <- nbrvars[which(nbrvars %in% names(treeux))]
              treeux[, (nbrvars) := lapply(.SD, check.numeric), .SDcols=nbrvars]

              if ("DIA" %in% names(treeux)) {
                ## Create new tree variables - basal area
                treeux[, BA := DIA * DIA * 0.005454]
              }
			  
              ## Create new biomass variables
              if (!is.null(sppvars)) {
                treenames <- names(treeux)
                treeux <- merge(treex, refspp, by="SPCD")
                sppvarsnew <- {}
                if (biojenk) {
                  if (!"BIOJENK_kg" %in% names(treeux)) {
                    treeux[, BIOJENK_kg := exp(JENKINS_TOTAL_B1 + 
                               JENKINS_TOTAL_B2 * log(DIA * 2.54))]
                    treeux[, JENKINS_TOTAL_B1 := NULL][, JENKINS_TOTAL_B2 := NULL]
                    sppvarsnew <- c(sppvarsnew[!sppvarsnew %in% 
                          c("JENKINS_TOTAL_B1", "JENKINS_TOTAL_B2")], "BIOJENK_kg")
                  }
                  if (!"BIOJENK_lb" %in% names(treeux)) {
                    treeux[, BIOJENK_lb := BIOJENK_kg * 2.2046]  ## Converts back to lbs
                    sppvarsnew <- c(sppvarsnew, "BIOJENK_lb")
                  }
                }
                if (greenwt && "DRYBIO_AG" %in% names(treeux) && 
                      !"GREENBIO_AG" %in% names(treeux)) {
                  treeux[, GREENBIO_AG := DRYBIO_AG * DRYWT_TO_GREENWT_CONVERSION]
                  sppvarsnew <- c(sppvarsnew, "DRYWT_TO_GREENWT_CONVERSION")		
                }                
                setcolorder(treeux, c(treenames, sppvarsnew)) 
              } 
			  
              ## Append data
              if (treeReturn && returndata) {
			    if ("treeu" %in% names(tabs)) {
                  tabs$treeu <- rbind(tabs$tree , data.frame(treeux))
			    } else {
			      tabs$treeu <- data.frame(treeux)
			    }
			    if (!"treeu" %in% names(tabIDs)) {
                  tabIDs$treeu <- "PLT_CN"
			    }
              }
			  
              if ((savedata || !treeReturn)) {
                message("saving treeu table...")
                index.unique.treeux <- NULL
                if (!append_layer) {
                  index.unique.treeux <- list(c("PLT_CN", "CONDID", "SUBP", "TREE"), 
				                               "TREE_CN")
                }
                datExportData(treeux, 
                   index.unique = index.unique.treeux,
                   savedata_opts = list(outfolder = outfolder, 
                                        out_fmt = out_fmt, 
                                        out_dsn = out_dsn, 
                                        out_layer = "treeu",
                                        outfn.pre = outfn.pre, 
                                        overwrite_layer = overwrite_layer,
                                        append_layer = append_layer,
                                        outfn.date = outfn.date, 
                                        add_layer = TRUE)) 
                rm(treex)
                # gc()
			  }
            }
			
            ##############################################################
            ## Tree Change, Growth, and Mortality 
		    ## (TREE_GRM_COMPONENT, TREE_GRM_BEGIN, TREE_GRM, MIDPT)
            ##############################################################
            message("\n",
             "## STATUS: Getting GRM data from TREE_GRM_COMPONENT (", stabbr, ") ...", "\n")
    
            ## GRM data
            if (datsource == "sqlite") {
              grmnm <- chkdbtab(dbtablst, grm_layer)
              if (!is.null(grmnm)) {
                ## Get TREE_GRM_COMPONENT fields
                grmflds <- DBI::dbListFields(dbconn, grmnm)
              }
			  grmbnm <- chkdbtab(dbtablst, grmb_layer)
              if (!is.null(grmbnm)) {
                ## Get TREE_GRM_BEGIN fields
                grmbflds <- DBI::dbListFields(dbconn, grmbnm)
              }
			  grmmnm <- chkdbtab(dbtablst, grmm_layer)
              if (!is.null(grmmnm)) {
                ## Get TREE_GRM_MIDPT fields
                grmmflds <- DBI::dbListFields(dbconn, grmmnm)
              }

            } else if (datsource == "datamart") {
              TREE_GRM_COMPONENT <- DBgetCSV("TREE_GRM_COMPONENT", stabbr, 
		            returnDT=TRUE, stopifnull=FALSE)
              if (!is.null(TREE_GRM_COMPONENT)) {
                grmnm <- "TREE_GRM_COMPONENT"
                grmflds <- names(TREE_GRM_COMPONENT)
              }
              TREE_GRM_BEGIN <- DBgetCSV("TREE_GRM_BEGIN", stabbr, 
		            returnDT=TRUE, stopifnull=FALSE)
              if (!is.null(TREE_GRM_BEGIN)) {
                grmbnm <- "TREE_GRM_BEGIN"
                grmbflds <- names(TREE_GRM_BEGIN)
              }
              TREE_GRM_MIDPT <- DBgetCSV("TREE_GRM_MIDPT", stabbr, 
		            returnDT=TRUE, stopifnull=FALSE)
              if (!is.null(TREE_GRM_MIDPT)) {
                grmmnm <- "TREE_GRM_MIDPT"
                grmmflds <- names(TREE_GRM_MIDPT)
              }
            } else if (datsource %in% c("csv", "obj")) {
              TREE_GRM_COMPONENT <- pcheck.table(grm_layer, 
					stopifnull=TRUE, stopifinvalid=TRUE)
              if (!is.null(TREE_GRM_COMPONENT)) {
                grmnm <- "TREE_GRM_COMPONENT"
                names(TREE_GRM_COMPONENT) <- toupper(names(TREE_GRM_COMPONENT))
                grmflds <- names(TREE_GRM_COMPONENT)
              }
              TREE_GRM_BEGIN <- pcheck.table(grmb_layer, 
					stopifnull=TRUE, stopifinvalid=TRUE)
              if (!is.null(TREE_GRM_BEGIN)) {
                grmbnm <- "TREE_GRM_BEGIN"
                names(TREE_GRM_BEGIN) <- toupper(names(TREE_GRM_BEGIN))
                grmbflds <- names(TREE_GRM_BEGIN)
              }
              TREE_GRM_MIDPT <- pcheck.table(grmm_layer, 
					stopifnull=TRUE, stopifinvalid=TRUE)
              if (!is.null(TREE_GRM_MIDPT)) {
                grmmnm <- "TREE_GRM_MIDPT"
                names(TREE_GRM_MIDPT) <- toupper(names(TREE_GRM_MIDPT))
                grmmflds <- names(TREE_GRM_MIDPT)
              }
            }

            if (is.null(grmnm)) {
              grmx=grmbx=grmmx <- NULL
              #isgrm <- NULL
            } else {

 	          ## TREE_GRM_COMPONENT
	          #######################################################################

              ## Create query for TREE_GRM_COMPONENT
              grmfromqry <- paste0(pfromqry, " JOIN ", SCHEMA.,
				    grmnm, " grm ON (grm.PLT_CN = p.", puniqueid, ")")

              grm.qry <- paste("select distinct grm.* \nfrom", grmfromqry, 
		                "\nwhere", paste0(evalFilter.grm, stateFilters))

	          ## Query SQLite database or R object
              if (datsource == "sqlite") {
                grmx <- tryCatch( DBI::dbGetQuery(dbconn, grm.qry),
			      error=function(e) {
                    message("TREE_GRM_COMPONENT query is invalid")
                    return(NULL) })

              } else {
                grmx <- tryCatch( sqldf::sqldf(grm.qry, stringsAsFactors=FALSE),
			      error=function(e) {
                    message("TREE_GRM_COMPONENT query is invalid")
                    return(NULL) })
              }
              if (!is.null(grmx) && nrow(grmx) != 0) {
	  	        if (!"tree_grm_component" %in% names(dbqueries[[state]])) {
                  dbqueries[[state]]$tree_grm_component <- grm.qry
	            }

                grmx <- setDT(grmx)
                grmx[, PLT_CN := as.character(PLT_CN)]
                grmx[, TRE_CN := as.character(TRE_CN)]
                setkey(grmx, TRE_CN)

                ## Subset overall filters from condx
                grmx <- grmx[treeux$CN,]

                if (returndata) {
		  	      if ("tree_grm_component" %in% names(tabs)) {
                    tabs$tree_grm_component <- rbind(tabs$tree_grm_component, 
						data.frame(grmx))
	              } else {
	                tabs$tree_grm_component <- data.frame(grmx)
	              }
 	              if (!"tree_grm_component" %in% names(tabIDs)) {
                    tabIDs$tree_grm_component <- "PLT_CN"
	              }
                }
                if (savedata) {
                  index.unique.grmx <- NULL
                  if (!append_layer) index.unique.grmx <- c("TRE_CN")
                  datExportData(grmx, 
                      index.unique = index.unique.grmx,
                      savedata_opts = list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="tree_grm_component",
                                outfn.pre=outfn.pre, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                outfn.date=outfn.date, 
                                add_layer=TRUE))
                  rm(grmx)
                  # gc()  
                } 
              }
			  if (datsource == "datamart") {
                if (exists("TREE_GRM_COMPONENT")) rm(TREE_GRM_COMPONENT)
                # gc()
              }
		
		      ## TREE_GRM_BEGIN
	          #######################################################################

              ## Create query for TREE_GRM_BEGIN
              grmbfromqry <- paste0(pfromqry, " JOIN ", SCHEMA.,
				grmbnm, " grmb ON (grmb.PLT_CN = p.", puniqueid, ")")

              grmb.qry <- paste("select distinct grmb.* \nfrom", grmbfromqry, 
		                "\nwhere", paste0(evalFilter.grm, stateFilters))

	          ## Query SQLite database or R object
              if (datsource == "sqlite") {
                grmbx <- tryCatch( DBI::dbGetQuery(dbconn, grmb.qry),
			      error=function(e) {
                    message("TREE_GRM_BEGIN query is invalid")
                    return(NULL) })

              } else {
                grmbx <- tryCatch( sqldf::sqldf(grmb.qry, stringsAsFactors=FALSE),
			      error=function(e) {
                    message("TREE_GRM_BEGIN query is invalid")
                    return(NULL) })
              }
              if (!is.null(grmbx) && nrow(grmbx) != 0) {
	  	        if (!"tree_grm_begin" %in% names(dbqueries[[state]])) {
                  dbqueries[[state]]$tree_grm_begin <- grmb.qry
	            }

                grmbx <- setDT(grmbx)
                grmbx[, PLT_CN := as.character(PLT_CN)]
                grmbx[, TRE_CN := as.character(TRE_CN)]
                setkey(grmbx, TRE_CN)

                ## Subset overall filters from condx
                grmbx <- grmbx[treeux$CN,]

                if (returndata) {
		  	      if ("tree_grm_begin" %in% names(tabs)) {
                    tabs$tree_grm_begin <- rbind(tabs$tree_grm_begin, 
						data.frame(grmbx))
	              } else {
	                tabs$tree_grm_begin <- data.frame(grmbx)
	              }
 	              if (!"tree_grm_begin" %in% names(tabIDs)) {
                    tabIDs$tree_grm_begin <- "PLT_CN"
	              }
                }
                if (savedata) {
                  index.unique.grmx <- NULL
                  if (!append_layer) index.unique.grmx <- c("TRE_CN")
                  datExportData(grmbx, 
                      index.unique = index.unique.grmx,
                      savedata_opts = list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="tree_grm_begin",
                                outfn.pre=outfn.pre, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                outfn.date=outfn.date, 
                                add_layer=TRUE))
                  rm(grmbx)
                  # gc()  
                }
                if (datsource == "datamart") {
                  if (exists("TREE_GRM_BEGIN")) rm(TREE_GRM_BEGIN)
                  # gc()
                }				
              }
            
		      ## TREE_GRM_MIDPT
	          #######################################################################

              ## Create query for TREE_GRM_MIDPT
              grmmfromqry <- paste0(pfromqry, " JOIN ", SCHEMA.,
				grmmnm, " grmm ON (grmm.PLT_CN = p.", puniqueid, ")")

              grmm.qry <- paste("select distinct grmm.* \nfrom", grmmfromqry, 
		                "\nwhere", paste0(evalFilter.grm, stateFilters))

	          ## Query SQLite database or R object
              if (datsource == "sqlite") {
                grmmx <- tryCatch( DBI::dbGetQuery(dbconn, grmm.qry),
			      error=function(e) {
                    message("TREE_GRM_MIDPT query is invalid")
                    return(NULL) })

              } else {
                grmmx <- tryCatch( sqldf::sqldf(grmm.qry, stringsAsFactors=FALSE),
			      error=function(e) {
                    message("TREE_GRM_MIDPT query is invalid")
                    return(NULL) })
              }
              if (!is.null(grmmx) && nrow(grmmx) != 0) {
	  	        if (!"tree_grm_midpt" %in% names(dbqueries[[state]])) {
                  dbqueries[[state]]$tree_grm_midpt <- grmm.qry
	            }

                grmmx <- setDT(grmmx)
                grmmx[, PLT_CN := as.character(PLT_CN)]
                grmmx[, TRE_CN := as.character(TRE_CN)]
                setkey(grmmx, TRE_CN)

                ## Subset overall filters from condx
                grmmx <- grmmx[treeux$CN,]

                if (returndata) {
		  	      if ("tree_grm_midpt" %in% names(tabs)) {
                    tabs$tree_grm_midpt <- rbind(tabs$tree_grm_midpt, 
						data.frame(grmmx))
	              } else {
	                tabs$tree_grm_midpt <- data.frame(grmmx)
	              }
 	              if (!"tree_grm_midpt" %in% names(tabIDs)) {
                    tabIDs$tree_grm_midpt <- "PLT_CN"
	              }
                }
                if (savedata) {
                  index.unique.grmx <- NULL
                  if (!append_layer) index.unique.grmx <- c("TRE_CN")
                  datExportData(grmmx, 
                      index.unique = index.unique.grmx,
                      savedata_opts = list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="tree_grm_midpt",
                                outfn.pre=outfn.pre, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                outfn.date=outfn.date, 
                                add_layer=TRUE))
                  rm(grmmx)
                  # gc()  
                } 
              }
			  if (datsource == "datamart") {
                if (exists("TREE_GRM_MIDPT")) rm(TREE_GRM_MIDPT)
                # gc()
              }
            }
          }
          if (datsource == "datamart") {
            rm(TREE)
            gc()
          }
        }
	  }
	}
	
    ##############################################################
    ## Plot counts and spatial data
    ##############################################################
    if (!is.null(pltx)) {
      ##  GET PLOT AND CONDITION COUNTS  
      ######################################################################
      plotcnt.vars <- names(pltx)[names(pltx) %in% c("CN", "STATECD", "INVYR", "FORNONSAMP")]
      pltcnt <- rbind(pltcnt, 
		 datPlotcnt(plt=unique(pltx[, plotcnt.vars, with=FALSE]), savedata=FALSE))

      ##############################################################
      ## spconddata
      ##############################################################
      if (spcond) {
        ## Get condition data for spatial plot 
        spconddatx <- getspconddat(cond=condx, condid1=spcondid1, ACI=ACI)

        ## Append data
        spconddat <- rbind(spconddat, spconddatx)
      }

      ##############################################################
      ## xydata
      ##############################################################
      #xyx <- pltx[, c("CN", getcoords(coordType), "PLOT_ID"), with=FALSE]
      if (getxy) {

        if (is.null(pjoinid)) pjoinid <- puniqueid
        if (is.null(xy_datsource)) {
          xy_datsource <- datsource
          xy_dsn <- data_dsn
        }
		xysource <- ifelse ((exists(xy) && is.data.frame(xy)), "obj", xy_datsource)
        #xycoords <- getcoords(coordType)
		xycoords <- c(xvar, yvar)

        if (!is.null(plotnm) && exists(plotnm) && !is.function(get(plotnm))) {
          dbTabs$plot_layer <- get(plotnm)
        } else {
          dbTabs$plot_layer <- plotnm
        }
	
        if (xymeasCur) {
          xydat <- DBgetXY(states = state,
                           xy_datsource = xysource,
                           xy_dsn = xy_dsn,
                           xy = xy,
                           xy_opts = xy_opts,
                           datsource = datsource,
                           data_dsn = data_dsn,
                           dbTabs = dbTabs,
                           eval = eval,
                           eval_opts = eval_options(Cur = TRUE, varCur=varCur),
                           pjoinid = pjoinid,
                           intensity1 = intensity1,
                           pvars2keep = c("INVYR", "PLOT_STATUS_CD"),
						   evalInfo = evalInfo,						   
                           SURVEY = SURVEY,
                           POP_PLOT_STRATUM_ASSGN = POP_PLOT_STRATUM_ASSGN)
          assign(paste0("xyCurx_", coordType), 
			xydat[[1]][xydat[[1]][[xydat$xy_opts$xyjoinid]] %in% pltx[[xydat$pjoinid]], ])
          if (returndata) { 
            assign(paste0("xyCur_", coordType), 
				  rbind(get(paste0("xyCur_", coordType)), get(paste0("xyCurx_", coordType))))
          } 
        } else {
          xydat <- DBgetXY(states = state,
                           xy_datsource = xysource,
                           xy_dsn = xy_dsn,
                           xy = xy,
                           xy_opts = xy_opts,
                           datsource = datsource,
                           data_dsn = data_dsn,
                           dbTabs = dbTabs,
                           eval = eval,
                           eval_opts = eval_opts,
                           pjoinid = pjoinid,
                           intensity1 = intensity1,
                           pvars2keep = c("INVYR", "PLOT_STATUS_CD"),
						   evalInfo = evalInfo,
                           SURVEY = SURVEY,
                           POP_PLOT_STRATUM_ASSGN = POP_PLOT_STRATUM_ASSGN)
          assign(paste0("xyx_", coordType), 
			xydat[[1]][xydat[[1]][[xydat$xy_opts$xyjoinid]] %in% pltx[[xydat$pjoinid]], ])
          if (returndata) { 
            assign(paste0("xy_", coordType), 
				  rbind(get(paste0("xy_", coordType)), get(paste0("xyx_", coordType))))
          } 
        }
		xynames <- names(xydat[[1]])
		if (xvar %in% xynames) {
	      xvarnm <- xvar
	    } else if (xycoords[1] %in% xynames) {
		  xvarnm <- xycoords[1]
		}
	    if (yvar %in% xynames) {
	      yvarnm <- yvar
		} else if (xycoords[2] %in% xynames) {
		  yvarnm <- xycoords[2]
		}
 
		if (!"xy" %in% names(dbqueries[[state]])) {
          dbqueries[[state]]$xy <- xydat$xyqry
	    }
      }
    }

    ##############################################################
    ## Seedling data (SEEDLING)
    ##############################################################
    if (isseed && !is.null(pltx)) {
      message("\n",
      	"## STATUS: Getting seed data from SEEDLING (", stabbr, ") ...", "\n")

      if (datsource == "sqlite") {
        seednm <- chkdbtab(dbtablst, seed_layer)
        if (is.null(seednm)) {
          stest <- dbtablst[grepl("seed", dbtablst)]
          if (length(stest) == 1) {
            seednm <- stest

            ## Get seedling fields
            seedflds <- DBI::dbListFields(dbconn, seednm)
          } else {
            message("there is no seedling table in database")
            isseed <- FALSE 
            seednm <- NULL
          }
        } else {
          seedflds <- DBI::dbListFields(dbconn, seednm)
        }
      } else if (datsource == "datamart") {
        SEEDLING <- DBgetCSV("SEEDLING", stabbr, returnDT=TRUE, 
		      stopifnull=FALSE)
        if (is.null(SEEDLING)) {
          message("there is no SEEDLING table in datamart")
        } else {
          seednm <- "SEEDLING"
          seedflds <- names(SEEDLING)
        }
      } else if (datsource %in% c("csv", "obj")) {
        SEEDLING <- pcheck.table(seed_layer, stopifnull=TRUE, stopifinvalid=TRUE)
        if (is.null(SEEDLING)) {
          message("the SEEDLING table is invalid")
        } else {
          seednm <- "SEEDLING"
          names(SEEDLING) <- toupper(names(SEEDLING))
          seedflds <- names(SEEDLING)
        }
      }

      if (is.null(seednm)) {
        seedx <- NULL
        isseed <- NULL
      } else {
        if (defaultVars) {
          scols <- unique(c(seedvarlst, ssumvarlst))
          scols <- scols[scols %in% seedflds]

          ## Add commas
          ssvars <- toString(paste0("s.", scols))
        } else {
          ssvars <- "s.*"
        }

        ## Create query for SEEDLING
        sfromqry <- paste0(pfromqry, " \nJOIN ", SCHEMA., 
				seednm, " s ON (s.PLT_CN = p.", puniqueid, ")")

        seed.qry <- paste("select distinct", ssvars, 
		                 "\nfrom", sfromqry, 
						 "\nwhere", xfilter)
						 
	    ## Query SQLite database or R object
        if (datsource == "sqlite") {
          seedx <- tryCatch( DBI::dbGetQuery(dbconn, seed.qry),
			error=function(e) {
                    message("seed query is invalid\n")
                    message(seed.qry)
                    return(NULL) })
        } else {
          seedx <- tryCatch( sqldf::sqldf(seed.qry, stringsAsFactors=FALSE),
			error=function(e) {
                    message("seed query is invalid\n")
                    message(seed.qry)
                    return(NULL) })
        }
        if (!is.null(seedx) && nrow(seedx) != 0 && length(ssvars) > 0) {
	      if (!"seed" %in% names(dbqueries[[state]])) {
            dbqueries[[state]]$seed <- seed.qry
	      }

          seedx <- setDT(seedx)
          seedx[, PLT_CN := as.character(PLT_CN)]
          setkey(seedx, PLT_CN, CONDID)

          ## Subset overall filters from pltx
          seedx <- seedx[seedx$PLT_CN %in% unique(pltx$CN),]

          ## Subset overall filters from condx
          seedx <- seedx[paste(seedx$PLT_CN, seedx$CONDID) %in% pcondID,]

          ## Check ACI
          if (!ACI) {
            ACIplts <- condx[COND_STATUS_CD == 1, paste(PLT_CN, CONDID)]
            seedx <- seedx[paste(seedx$PLT_CN, seedx$CONDID) %in% ACIplts,]
          } 

          ## Write query to outfolder
#          if (saveqry) {
#            seedqryfnbase <- DBgetfn("seed", invtype, outfn.pre, stabbr, 
#			evalid=evalid, qry=TRUE, outfn.date=outfn.date)
#            seedqryfn <- fileexistsnm(outfolder, seedqryfnbase, "txt")
#            outfile <- file(paste0(outfolder, "/", seedqryfn, ".txt"), "w")
#              cat(  paste0(seedqry, xfilter), "\n", file=outfile)
#            close(outfile)
#          }

          ## Change NA values to 0 values
#          if (any(names(seedx) %in% seednavars)) 
#            seedx <- DT_NAto0(seedx, seednavars)
     
          if (defaultVars && "TREECOUNT_CALC" %in% names(seedx)) {
            ## Create variable, SEEDCNT6, where a value of 6 means 6 or more seeds (per SUBP) 
            seedx[, SEEDCNT6 := TREECOUNT_CALC][TREECOUNT_CALC >= 6, SEEDCNT6 := 6]

            ## Create variable, SEEDSUBP6, indicating a species has 6 or more seedlings on a SUBP
            seedx[, SEEDSUBP6 := 0][TREECOUNT_CALC >= 6, SEEDSUBP6 := 1]
          }

          if (returndata) {
            ## Append data
			if ("seed" %in% names(tabs)) {
              tabs$seed <- rbind(tabs$seed, data.frame(seedx))
	        } else {
	          tabs$seed <- data.frame(seedx)
	        }
 	        if (!"seed" %in% names(tabIDs)) {
              tabIDs$seed <- "PLT_CN"
	        }
          }
          if (savedata) {
            message("saving seedling table...")
            index.unique.seedx <- NULL
            if (!append_layer) {
              index.unique.seedx <- c("PLT_CN", "CONDID", "SUBP")
            }
            datExportData(seedx, 
                index.unique = index.unique.seedx,
                savedata_opts = list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="seedling",
                                outfn.pre=outfn.pre, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                outfn.date=outfn.date, 
                                add_layer=TRUE)) 
            rm(seedx)
            # gc()
          }              
        }
      }
      if (datsource == "datamart") {
        rm(SEEDLING)
        # gc()
      }
    }

    ##############################################################
    ## Understory vegetation data (P2VEG_SUBPLOT_SPP)
    ##############################################################
    if (isveg && !is.null(pltx)) {
      message("\n",
      "## STATUS: Getting veg data from P2VEG_SUBPLOT_SPP/P2VEG_SUBP_STRUCTURE (", 
		stabbr, ") ...", "\n")

      ## Understory vegetation
      if (datsource == "sqlite") {
        vsubpsppnm <- chkdbtab(dbtablst, vsubpspp_layer)
        if (is.null(vsubpsppnm)) {
          message("there is no P2VEG_SUBPLOT_SPP table in database")
        } else {
          ## Get P2VEG_SUBPLOT_SPP fields
          vsppflds <- DBI::dbListFields(dbconn, vsubpsppnm)
        }
        vsubpstrnm <- chkdbtab(dbtablst, vsubpstr_layer)
        if (is.null(vsubpstrnm)) {
          message("there is no P2VEG_SUBP_STRUCTURE table in database")
          isveg <- FALSE
        } else {
          ## Get P2VEG_SUBP_STRUCTURE fields
          vstrflds <- DBI::dbListFields(dbconn, vsubpstrnm)
        }

      } else if (datsource == "datamart") {
        P2VEG_SUBPLOT_SPP <- 
		      DBgetCSV("P2VEG_SUBPLOT_SPP", stabbr, returnDT=TRUE, 
		      stopifnull=FALSE)
        if (!is.null(P2VEG_SUBPLOT_SPP)) {
          vsubpsppnm <- "P2VEG_SUBPLOT_SPP"
          names(P2VEG_SUBPLOT_SPP) <- toupper(names(P2VEG_SUBPLOT_SPP))
          vsppflds <- names(P2VEG_SUBPLOT_SPP)
        }

        P2VEG_SUBP_STRUCTURE <- 
		      DBgetCSV("P2VEG_SUBP_STRUCTURE", stabbr, returnDT=TRUE, 
		      stopifnull=FALSE)
        if (!is.null(P2VEG_SUBP_STRUCTURE)) {
          vsubpstrnm <- "P2VEG_SUBP_STRUCTURE"
          names(P2VEG_SUBP_STRUCTURE) <- toupper(names(P2VEG_SUBP_STRUCTURE))
          vstrflds <- names(P2VEG_SUBP_STRUCTURE)
        }
      } else if (datsource %in% c("csv", "obj")) {
        P2VEG_SUBPLOT_SPP <- pcheck.table(vsubpspp_layer, 
					stopifnull=TRUE, stopifinvalid=TRUE)
        if (!is.null(P2VEG_SUBPLOT_SPP)) {
          vsubpsppnm <- "P2VEG_SUBPLOT_SPP"
          names(P2VEG_SUBPLOT_SPP) <- toupper(names(P2VEG_SUBPLOT_SPP))
          vsppflds <- names(P2VEG_SUBPLOT_SPP)
        }

        P2VEG_SUBP_STRUCTURE <- pcheck.table(vsubpstr_layer, 
					stopifnull=TRUE, stopifinvalid=TRUE)
        if (is.null(P2VEG_SUBP_STRUCTURE)) {
          message("the P2VEG_SUBP_STRUCTURE is invalid")
        } else {
          vsubpstrnm <- "P2VEG_SUBP_STRUCTURE"
          names(P2VEG_SUBP_STRUCTURE) <- toupper(names(P2VEG_SUBP_STRUCTURE))
          vstrflds <- names(P2VEG_SUBP_STRUCTURE)
        }
      }
      
      if (!is.null(vsubpsppnm)) {
        if (defaultVars) {
          vsubpsppvarlst <- vsubpsppvarlst[vsubpsppvarlst %in% vsppflds]

          ## Add commas
          vsubpsppvars <- toString(paste0("v.", vsubpsppvarlst))
        } else {
          vsubpsppvars <- "v.*"
        }

        ## Create query for P2VEG_SUBPLOT_SPP
        vsppfromqry <- paste0(pfromqry, " \nJOIN ", SCHEMA., 
				vsubpsppnm, " v ON v.PLT_CN = p.", puniqueid)

        vsubpspp.qry <- paste("select distinct", vsubpsppvars, 
		                     "\nfrom", vsppfromqry,
							 "\nwhere", paste0(evalFilter.veg, stateFilters))

	    ## Query SQLite database or R object
        if (datsource == "sqlite") {
          p2veg_subplot_sppx <- tryCatch( DBI::dbGetQuery(dbconn, vsubpspp.qry),
			error=function(e) {
                    message("P2VEG_SUBPLOT_SPP query is invalid\n")
                    return(NULL) })
        } else {
          p2veg_subplot_sppx <- tryCatch( sqldf::sqldf(vsubpspp.qry, stringsAsFactors=FALSE),
			error=function(e) {
                    message("P2VEG_SUBPLOT_SPP query is invalid\n")
                    return(NULL) })
        }
 
        if (!is.null(p2veg_subplot_sppx) && nrow(p2veg_subplot_sppx) != 0) {
	  	  if (!"p2veg_subplot_spp" %in% names(dbqueries[[state]])) {
            dbqueries[[state]]$p2veg_subplot_spp <- vsubpspp.qry
	      }

          p2veg_subplot_sppx <- setDT(p2veg_subplot_sppx)
          p2veg_subplot_sppx[, PLT_CN := as.character(PLT_CN)]
          setkey(p2veg_subplot_sppx, PLT_CN)

          ## Subset overall filters from condx
          p2veg_subplot_sppx <- p2veg_subplot_sppx[paste(PLT_CN, CONDID) %in% pcondID,]

          if (returndata) {
		  	if ("p2veg_subplot_spp" %in% names(tabs)) {
              tabs$p2veg_subplot_spp <- rbind(tabs$p2veg_subplot_spp, 
							data.frame(p2veg_subplot_sppx))
	        } else {
	          tabs$p2veg_subplot_spp <- data.frame(p2veg_subplot_sppx)
	        }
 	        if (!"p2veg_subplot_spp" %in% names(tabIDs)) {
              tabIDs$p2veg_subplot_spp <- "PLT_CN"
	        }
          }
          if (savedata) {
            index.unique.vsubpsppx <- NULL
            if (!append_layer) index.unique.vsubpsppx <- c("PLT_CN", "CONDID")
            datExportData(p2veg_subplot_sppx, 
                 index.unique = index.unique.vsubpsppx,
                 savedata_opts = list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="p2veg_subplot_spp",
                                outfn.pre=outfn.pre, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                outfn.date=outfn.date, 
                                add_layer=TRUE)) 
            rm(p2veg_subplot_sppx)
            # gc()
          }
        }
      }
	  
	  ##############################################################
      ## Understory vegetation data (P2VEG_SUBP_STRUCTURE)
      ##############################################################
      if (!is.null(vsubpstrnm)) {
        if (defaultVars) {
          vsubpstrvarlst <- vsubpstrvarlst[vsubpstrvarlst %in% vstrflds]

          ## Add commas
          vsubpstrvars <- toString(paste0("v.", vsubpstrvarlst))
        } else {
          vsubpstrvars <- "v.*"
        }

        ## Create query for P2VEG_SUBP_STRUCTURE
        vstrfromqry <- paste0(pfromqry, " \nJOIN ", SCHEMA., 
				vsubpstrnm, " v ON v.PLT_CN = p.", puniqueid)

        vsubpstr.qry <- paste("select distinct", vsubpstrvars, 
		                     "\nfrom", vstrfromqry,
							 "\nwhere", paste0(evalFilter.veg, stateFilters))
							 
	    ## Query SQLite database or R object
        if (datsource == "sqlite") {
          p2veg_subp_structurex <- tryCatch( DBI::dbGetQuery(dbconn, vsubpstr.qry),
			error=function(e) {
                    message("P2VEG_SUBP_STRUCTURE query is invalid\n")
                    return(NULL) })
        } else {
          p2veg_subp_structurex <- tryCatch( sqldf::sqldf(vsubpstr.qry, stringsAsFactors=FALSE),
			error=function(e) {
                    message("P2VEG_SUBP_STRUCTURE query is invalid\n")
                    return(NULL) })
        }

        if (!is.null(p2veg_subp_structurex) && nrow(p2veg_subp_structurex) != 0) {
	  	  if (!"p2veg_subp_structure" %in% names(dbqueries[[state]])) {
            dbqueries[[state]]$p2veg_subp_structure <- vsubpstr.qry
	      }

          p2veg_subp_structurex <- setDT(p2veg_subp_structurex)
          p2veg_subp_structurex[, PLT_CN := as.character(PLT_CN)]
          setkey(p2veg_subp_structurex, PLT_CN)

          ## Subset overall filters from condx
          p2veg_subp_structurex <- p2veg_subp_structurex[paste(PLT_CN, CONDID) %in% pcondID,]

          if (returndata) {
		  	if ("p2veg_subp_structure" %in% names(tabs)) {
              tabs$p2veg_subp_structure <- 
					rbind(tabs$p2veg_subp_structure, data.frame(p2veg_subp_structurex))
	        } else {
	          tabs$p2veg_subp_structure <- data.frame(p2veg_subp_structurex)
	        }
 	        if (!"p2veg_subp_structure" %in% names(tabIDs)) {
              tabIDs$p2veg_subp_structure <- "PLT_CN"
	        }
          }
          if (savedata) {
            index.unique.vsubpstrx <- NULL
            if (!append_layer) index.unique.vsubpstrx <- c("PLT_CN", "CONDID")
            datExportData(p2veg_subp_structurex, 
                index.unique = index.unique.vsubpstrx,
                savedata_opts = list(outfolder = outfolder, 
                                out_fmt = out_fmt, 
                                out_dsn = out_dsn, 
                                out_layer = "p2veg_subp_structure",
                                outfn.pre = outfn.pre, 
                                overwrite_layer = overwrite_layer,
                                append_layer = append_layer,
                                outfn.date = outfn.date, 
                                add_layer = TRUE))
            rm(p2veg_subp_structurex)
            # gc() 
          }
        }
      }

      if (datsource == "datamart") {
        if (exists("P2VEG_SUBPLOT_SPP")) rm(P2VEG_SUBPLOT_SPP)
        if (exists("P2VEG_SUBP_STRUCTURE")) rm(P2VEG_SUBP_STRUCTURE)
        # gc()
      }
    }

    ##############################################################
    ## Invasive species (INVASIVE_SUBPLOT_SPP)
    ##############################################################
    if (isinv && !is.null(pltx)) {
      message("\n",
      "## STATUS: Getting invasive data from INVASIVE_SUBPLOT_SPP (", 
		stabbr, ") ...", "\n")

      ## Invasive species
      if (datsource == "sqlite") {
        invsubpnm <- chkdbtab(dbtablst, invsubp_layer)
        if (is.null(invsubpnm)) {
          message("there is no INVASIVE_SUBPLOT_SPP table in database")
        } else {
          ## Get INVASIVE_SUBPLOT_SPP fields
          invflds <- DBI::dbListFields(dbconn, invsubpnm)
        }

      } else if (datsource == "datamart") {

        INVASIVE_SUBPLOT_SPP <- 
		      DBgetCSV("INVASIVE_SUBPLOT_SPP", stabbr, returnDT=TRUE, 
		      stopifnull=FALSE)
        if (!is.null(P2VEG_SUBPLOT_SPP)) {
          invsubpnm <- "INVASIVE_SUBPLOT_SPP"
          names(INVASIVE_SUBPLOT_SPP) <- toupper(names(INVASIVE_SUBPLOT_SPP))
          invflds <- names(INVASIVE_SUBPLOT_SPP)
        }

      } else if (datsource %in% c("csv", "obj")) {

        INVASIVE_SUBPLOT_SPP <- pcheck.table(invsubp_layer, 
					stopifnull=TRUE, stopifinvalid=TRUE)
        if (!is.null(INVASIVE_SUBPLOT_SPP)) {
          invsubpnm <- "INVASIVE_SUBPLOT_SPP"
          names(INVASIVE_SUBPLOT_SPP) <- toupper(names(INVASIVE_SUBPLOT_SPP))
          invflds <- names(INVASIVE_SUBPLOT_SPP)
        }
      }
      
      if (!is.null(invsubpnm)) {
        ## Check variables in database
        if (defaultVars) {
          invsubpvarlst <- invsubpvarlst[invsubpvarlst %in% invflds]

          ## Add commas
          invsubpvars <- toString(paste0("v.", invsubpvarlst))
        } else {
          invsubpvars <- "v.*"
        }

        ## Create query for INVASIVE_SUBPLOT_SPP
        invfromqry <- paste0(pfromqry, " \nJOIN ", SCHEMA., 
				invsubpnm, " v ON v.PLT_CN = p.", puniqueid)

        invsubp.qry <- paste("select distinct", invsubpvars, 
		                    "\nfrom", invfromqry,
							"\nwhere", paste0(evalFilter.inv, stateFilters))

	    ## Query SQLite database or R object
        if (datsource == "sqlite") {
          invasive_subplot_sppx <- tryCatch( DBI::dbGetQuery(dbconn, invsubp.qry),
			error=function(e) {
                    message("INVASIVE_SUBPLOT_SPP query is invalid")
                    return(NULL) })
        } else {
          invasive_subplot_sppx <- tryCatch( sqldf::sqldf(invsubp.qry, stringsAsFactors=FALSE),
			error=function(e) {
                    message("INVASIVE_SUBPLOT_SPP query is invalid")
                    return(NULL) })
        }
        if (!is.null(invasive_subplot_sppx) && nrow(invasive_subplot_sppx) != 0) {
	  	  if (!"invasive_subplot_spp" %in% names(dbqueries[[state]])) {
            dbqueries[[state]]$invasive_subplot_spp <- vsubpstr.qry
	      }

          invasive_subplot_sppx <- setDT(invasive_subplot_sppx)
          invasive_subplot_sppx[, PLT_CN := as.character(PLT_CN)]
          setkey(invasive_subplot_sppx, PLT_CN)

          ## Subset overall filters from condx
          invasive_subplot_sppx <- invasive_subplot_sppx[paste(PLT_CN, CONDID) %in% pcondID,]

          if (returndata) {
		  	if ("invasive_subplot_spp" %in% names(tabs)) {
              tabs$invasive_subplot_spp <- rbind(tabs$invasive_subplot_spp, 
						data.frame(invasive_subplot_sppx))
	        } else {
	          tabs$invasive_subplot_spp <- data.frame(invasive_subplot_sppx)
	        }
 	        if (!"invasive_subplot_spp" %in% names(tabIDs)) {
              tabIDs$invasive_subplot_spp <- "PLT_CN"
	        }
          }
          if (savedata) {
            index.unique.invsubpx <- NULL
            if (!append_layer) index.unique.invsubpx <- c("PLT_CN", "CONDID")
            datExportData(invasive_subplot_sppx, 
                index.unique = index.unique.invsubpx,
                savedata_opts = list(outfolder = outfolder, 
                                out_fmt = out_fmt, 
                                out_dsn = out_dsn, 
                                out_layer = "invasive_subplot_spp",
                                outfn.pre = outfn.pre, 
                                overwrite_layer = overwrite_layer,
                                append_layer = append_layer,
                                outfn.date = outfn.date, 
                                add_layer = TRUE)) 
            rm(invasive_subplot_sppx)
            # gc() 
          }
        }
      }

      if (datsource == "datamart") {
        if (exists("INVASIVE_SUBPLOT_SPP")) rm(INVASIVE_SUBPLOT_SPP)
        # gc()
      }
    }

    ##############################################################
    ## Subplot data (SUBPLOT/SUBP_COND)
    ##############################################################
    if (issubp && !is.null(pltx)) {
      message("\n",
      "## STATUS: Getting subplot data from SUBPLOT/SUBP_COND (", stabbr, ") ...", "\n")


      if (datsource == "sqlite") {
        subplotnm <- chkdbtab(dbtablst, subplot_layer)
        if (is.null(subplotnm)) {
          message("there is no SUBPLOT table in database")
          issubp <- FALSE
        } else {
          ## Get SUBPLOT fields
          subpflds <- DBI::dbListFields(dbconn, subplotnm)
        }
        subpcondnm <- chkdbtab(dbtablst, subpcond_layer)
        if (is.null(subpcondnm)) {
          message("there is no SUBP_COND table in database")
          issubp <- FALSE
        } else {
          ## Get SUBP_COND fields
          subpcflds <- DBI::dbListFields(dbconn, subpcondnm)
        }
      } else if (datsource == "datamart") {
        SUBPLOT <- DBgetCSV("SUBPLOT", stabbr, returnDT=TRUE, stopifnull=FALSE)
        if (is.null(SUBPLOT)) {
          message("there is no SUBPLOT table in datamart")
        } else {
          subplotnm <- "SUBPLOT"
          names(SUBPLOT) <- toupper(names(SUBPLOT))
          subpflds <- names(SUBPLOT)
        }
        SUBP_COND <- DBgetCSV("SUBP_COND", stabbr, returnDT=TRUE, stopifnull=FALSE)
        if (!is.null(SUBP_COND)) {
          subpcondnm <- "SUBP_COND"
          subpcflds <- names(SUBP_COND)
        }
      } else if (datsource %in% c("csv", "obj")) {
        SUBPLOT <- pcheck.table(subplot_layer, 
					stopifnull=TRUE, stopifinvalid=TRUE)
        if (!is.null(SUBPLOT)) {
          subplotnm <- "SUBPLOT"
          names(SUBPLOT) <- toupper(names(SUBPLOT))
          subpflds <- names(SUBPLOT)
        }
        SUBP_COND <- pcheck.table(subpcond_layer, 
					stopifnull=TRUE, stopifinvalid=TRUE)
        if (is.null(SUBP_COND)) {
          message("the SUBP_COND table is invalid")
        } else {
          subpcondnm <- "SUBP_COND"
          names(SUBP_COND) <- toupper(names(SUBP_COND))
          subpcflds <- names(SUBP_COND)
        }
      }

      if (!is.null(subplotnm)) {
        ## Check variables in database
        if (defaultVars) {
          subpvarlst <- subpvarlst[subpvarlst %in% subpflds]

          ## Add commas
          subpvars <- toString(paste0("subp.", subpvarlst))
        } else {
          subpvars <- "subp.*"
        }

        ## Create query for SUBPLOT
        subpfromqry <- paste0(pfromqry, " \nJOIN ", SCHEMA., 
				subplotnm, " subp ON subp.PLT_CN = p.", puniqueid)

        subp.qry <- paste("select distinct", subpvars, 
		                 "\nfrom", subpfromqry,
						 "\nwhere", paste0(evalFilter, stateFilters))

	    ## Query SQLite database or R object
        if (datsource == "sqlite") {
          subpx <- tryCatch( DBI::dbGetQuery(dbconn, subp.qry),
			error=function(e) {
                    message("SUBPLOT query is invalid\n")
                    return(NULL) })
        } else {
          subpx <- tryCatch( sqldf::sqldf(subp.qry, stringsAsFactors=FALSE),
			error=function(e) {
                    message("SUBPLOT query is invalid\n")
                    return(NULL) })
        }
        if (!is.null(subpx) && nrow(subpx) != 0) {
	  	  if (!"subplot" %in% names(dbqueries[[state]])) {
            dbqueries[[state]]$subplot <- subp.qry
	      }

          subpx <- setDT(subpx)
          subpx[, PLT_CN := as.character(PLT_CN)]
          setkey(subpx, PLT_CN)

          ## Subset overall filters from condx
          subpx <- subpx[subpx$PLT_CN %in% pltx$CN,]

          if (returndata) {
		  	if ("subplot" %in% names(tabs)) {
              tabs$subplot <- rbind(tabs$subplot, data.frame(subpx))
	        } else {
	          tabs$subplot <- data.frame(subpx)
	        }
 	        if (!"subplot" %in% names(tabIDs)) {
              tabIDs$subplot <- "PLT_CN"
	        }
          }
          if (savedata) {
            index.unique.subpx <- NULL
            if (!append_layer) index.unique.subpx <- "PLT_CN"
            datExportData(subpx, 
                index.unique = index.unique.subpx,
                savedata_opts = list(outfolder = outfolder, 
                                out_fmt = out_fmt, 
                                out_dsn = out_dsn, 
                                out_layer = "subplot",
                                outfn.pre = outfn.pre, 
                                overwrite_layer = overwrite_layer,
                                append_layer = append_layer,
                                outfn.date = outfn.date, 
                                add_layer = TRUE)) 
            index.unique.subpcx <- NULL
            rm(subpx)
            # gc()
          }
        }
      }
      if (!is.null(subpcondnm)) {
        ## Check variables in database
        if (defaultVars) {
          subpcvarlst <- subpcvarlst[subpcvarlst %in% subpcflds]

          ## Add commas
          subpcvars <- toString(paste0("subpc.", subpcflds))
        } else {
          subpcvars <- "subpc.*"
        }

        ## Create query for SUBP_COND
        subpcfromqry <- paste0(pfromqry, " \nJOIN ", SCHEMA., 
				subpcondnm, " subpc ON subpc.PLT_CN = p.", puniqueid)

        subpc.qry <- paste("select", subpcvars, 
		                  "\nfrom", subpcfromqry,
						  "\nwhere", paste0(evalFilter, stateFilters))

	    ## Query SQLite database or R object
        if (datsource == "sqlite") {
          subpcx <- tryCatch( DBI::dbGetQuery(dbconn, subpc.qry),
			error=function(e) {
                    message("SUBP_COND query is invalid")
                    return(NULL) })
        } else {
          subpcx <- tryCatch( sqldf::sqldf(subpc.qry, stringsAsFactors=FALSE),
			error=function(e) {
                    message("SUBP_COND query is invalid")
                    return(NULL) })
        }
        if(!is.null(subpcx) && nrow(subpcx) != 0){
	  	  if (!"subp_cond" %in% names(dbqueries[[state]])) {
            dbqueries[[state]]$subp_cond <- subpc.qry
	      }

          subpcx <- setDT(subpcx)
          subpcx[, PLT_CN := as.character(PLT_CN)]
          setkey(subpcx, PLT_CN)

          ## Subset overall filters from condx
          subpcx <- subpcx[paste(subpcx$PLT_CN, subpcx$CONDID) %in% pcondID,]

          if (returndata) {
		  	if ("subp_cond" %in% names(tabs)) {
              tabs$subp_cond <- rbind(tabs$subp_cond, data.frame(subpcx))
	        } else {
	          tabs$subp_cond <- data.frame(subpcx)
	        }
 	        if (!"subp_cond" %in% names(tabIDs)) {
              tabIDs$subp_cond <- "PLT_CN"
	        }
          }
          if (savedata) {
            if (!append_layer) index.unique.subpcx <- c("PLT_CN", "CONDID")
            datExportData(subpcx, 
                index.unique = index.unique.subpcx,
                savedata_opts = list(outfolder = outfolder, 
                                out_fmt = out_fmt, 
                                out_dsn = out_dsn, 
                                out_layer = "subp_cond",
                                outfn.pre = outfn.pre, 
                                overwrite_layer = overwrite_layer,
                                append_layer = append_layer,
                                outfn.date = outfn.date, 
                                add_layer = TRUE)) 
            rm(subpcx)
            # gc() 
          } 
        }
      }

      if (datsource == "datamart") {
        if (exists("SUBPLOT")) rm(SUBPLOT)
        if (exists("SUBP_COND")) rm(SUBP_COND)
        # gc()
      }
    }

    ##############################################################
    ## Down woody data (COND_DWM_CALC)
    ##############################################################
    if (isdwm && !is.null(pltx)) {
      message("\n",
      "## STATUS: Getting DWM data from COND_DWM_CALC (", stabbr, ") ...", "\n")
    

      if (datsource == "sqlite") {
        dwmnm <- chkdbtab(dbtablst, dwm_layer)
        if (is.null(dwmnm)) {
          message("there is no COND_DWM_CALC table in database")
          isdwm <- FALSE
        } else {
          ## Get COND_DWM_CALC fields
          dwmflds <- DBI::dbListFields(dbconn, dwmnm)
        }
      } else if (datsource == "datamart") {
        COND_DWM_CALC <- DBgetCSV("COND_DWM_CALC", stabbr, returnDT=TRUE, 
		      stopifnull=FALSE)
        if (!is.null(COND_DWM_CALC)) {
          dwmnm <- "COND_DWM_CALC"
          dwmflds <- names(COND_DWM_CALC)
        }
      } else if (datsource %in% c("csv", "obj")) {
        COND_DWM_CALC <- pcheck.table(dwm_layer, 
					stopifnull=TRUE, stopifinvalid=TRUE)
        if (!is.null(COND_DWM_CALC)) {
          dwmnm <- "COND_DWM_CALC"
          names(COND_DWM_CALC) <- toupper(names(COND_DWM_CALC))
          dwmflds <- names(COND_DWM_CALC)
        }
      }

      if (is.null(dwmnm)) {
        dwmx <- NULL
        isdwm <- FALSE

      } else {

        ## Check variables in database
        if (defaultVars) {
          dwmvarlst <- dwmvarlst[dwmvarlst %in% dwmflds]

          ## Add commas
          dwmvars <- toString(paste0("d.", dwmvarlst))
        } else {
          dwmvars <- "d.*"
        }
        
        ## Create query for COND_DWM_CALC
        dfromqry <- paste0(pfromqry, " \nJOIN ", SCHEMA., 
				dwmnm, " d ON (d.PLT_CN = p.", puniqueid, ")")

        dwm.qry <- paste("select distinct", dwmvars, 
		                "\nfrom", dfromqry,
						"\nwhere", paste0(evalFilter.dwm, stateFilters))

	    ## Query SQLite database or R object
        if (datsource == "sqlite") {
          cond_dwm_calcx <- tryCatch( DBI::dbGetQuery(dbconn, dwm.qry),
			error=function(e) {
                    message("COND_DWM_CALC query is invalid")
                    return(NULL) })
        } else {
          cond_dwm_calcx <- tryCatch( sqldf::sqldf(dwm.qry, stringsAsFactors=FALSE),
			error=function(e) {
                    message("COND_DWM_CALC query is invalid")
                    return(NULL) })
        }
        if (!is.null(cond_dwm_calcx) && nrow(cond_dwm_calcx) != 0) {		
	  	  if (!"cond_dwm_calc" %in% names(dbqueries[[state]])) {
            dbqueries[[state]]$cond_dwm_calc <- dwm.qry
	      }

          cond_dwm_calcx <- setDT(cond_dwm_calcx)
          cond_dwm_calcx[, PLT_CN := as.character(PLT_CN)]
          setkey(cond_dwm_calcx, PLT_CN, CONDID)

          ## Subset overall filters from condx
          cond_dwm_calcx <- cond_dwm_calcx[paste(PLT_CN, CONDID) %in% pcondID,]
          if (returndata) {
		  	if ("cond_dwm_calc" %in% names(tabs)) {
              tabs$cond_dwm_calc <- rbind(tabs$cond_dwm_calc, 
							data.frame(cond_dwm_calcx))
	        } else {
	          tabs$cond_dwm_calc <- data.frame(cond_dwm_calcx)
	        }
 	        if (!"cond_dwm_calc" %in% names(tabIDs)) {
              tabIDs$cond_dwm_calc <- "PLT_CN"
	        }
          }
          if (savedata) {
            index.unique.dwmx <- NULL
            if (!append_layer) index.unique.dwmx <- c("PLT_CN", "CONDID")
            datExportData(cond_dwm_calcx, 
                  index.unique = index.unique.dwmx,
                  savedata_opts = list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="cond_dwm_calc",
                                outfn.pre=outfn.pre, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                outfn.date=outfn.date, 
                                add_layer=TRUE))
            rm(cond_dwm_calcx)
            # gc()
          } 
        }
      }
      if (datsource == "datamart") {
        if (exists("COND_DWM_CALC")) rm(COND_DWM_CALC)
        # gc()
      }
    }

    ##############################################################
    ## Other tables
    ##############################################################
    if (!is.null(othertables) && length(othertables2) > 0 && !is.null(pltx)) {

      ## Other tables
      if (!is.null(othertables) && length(othertables) > 0) {
        for (othertable in othertables) {
          if (othertable == "REF_SPECIES" && exists("REF_SPECIES") && !is.null(REF_SPECIES)) {           
            assign(othertable, REF_SPECIES)
          }
          if (datsource == "sqlite") {
            othertabnm <- chkdbtab(dbtablst, othertable)
            if (is.null(othertabnm)) {
              othertables <- othertables[othertables != othertable]
            }
          } else if (datsource == "datamart") {
            assign(othertable, 
 	       	DBgetCSV(othertable, stabbr, returnDT=TRUE, stopifnull=FALSE))

          } else if (datsource %in% c("csv", "obj")) {
            assign(othertable, pcheck.table(othertable, 
					stopifnull=TRUE, stopifinvalid=TRUE))
          }
        }
      }
      ## Create xfrom queries
      xfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				"SUBX x ON (x.PLT_CN = p.", puniqueid, ")")
      xfromqry_plotgeom <- paste0(pfromqry, " JOIN ", SCHEMA., 
				"SUBX x ON (x.CN = p.", puniqueid, ")")
      xfromqry2 <- paste0(pfromqry, " JOIN ", SCHEMA., 
				"SUBX x ON (x.STATECD = p.STATECD
						and x.UNITCD = p.UNITCD
						and x.COUNTYCD = p.COUNTYCD
						and x.PLOT = p.PLOT)")
      for (j in 1:length(othertables2)) {
        isref <- FALSE
        othertable <- othertables[j]
        othertablexnm <- paste0("otherx", j)

        if (!is.null(pcheck.varchar(othertable, checklst=pop_tables, stopifinvalid=FALSE))) {
          xfromqryx <- paste0(SCHEMA., othertable, " x")
          if (!iseval) {
            xfilterpop <- stFilter
            xfilterpop <- sub("p.", "x.", xfilterpop)
          } else {
            xfilterpop <- paste0("x.EVALID IN(", toString(evalid), ")")
          }
          xqry <- paste("select x.* \nfrom", 
		                  sub("SUBX", othertable, xfromqryx),
						  "\nwhere", xfilterpop)
        } else if (othertable == "PLOTGEOM") {
          joinid <- "CN"
          xqry <- paste("select x.* \nfrom", 
		                 sub("SUBX", othertable, xfromqry_plotgeom),
						 "\nwhere", xfilter)
        } else if (startsWith(othertable, "REF_") || startsWith(othertable, "ref_")) {
          xqry <- paste("select * from", othertable)
          isref <- TRUE
        } else {
          joinid <- "PLT_CN"
          xqry <- paste("select x.* \nfrom", 
		                 sub("SUBX", othertable, xfromqry),
						 "\nwhere", xfilter)
        }
		
        if (isref) {
          message(paste0("\n",
          "## STATUS: GETTING ", othertable, "...", "\n"))
        } else {
          message(paste0("\n",
          "## STATUS: GETTING ", othertable, " (", stabbr, ")...", "\n"))
        }

        if (datsource == "sqlite") {
          dbtabs <- DBI::dbListTables(dbconn)
          if (!othertable %in% dbtabs) {
            stop(othertable, " not in database")
          } 
          otab <- tryCatch( DBI::dbGetQuery(dbconn, xqry),
			error=function(e) return(NULL))
        } else {
          otab <- tryCatch( sqldf::sqldf(xqry, stringsAsFactors=FALSE), 
			error=function(e) return(NULL))
        }
        if (is.null(otab)) {
          xqry <- paste("select * \nfrom", othertable, 
		                "\nwhere", stFilter)

          if (datsource == "sqlite") {
            otab <- tryCatch( DBI::dbGetQuery(dbconn, xqry),
			error=function(e) return(NULL))
          } else {
            otab <- tryCatch( sqldf::sqldf(xqry, stringsAsFactors=FALSE), 
			error=function(e) return(NULL))
          }
        }

        if (isref) {
          othertables2 <- othertables2[othertables2 != othertable]
        }
        if (!isref) {
          if (is.null(pcheck.varchar(othertable, checklst=pop_tables, stopifinvalid=FALSE))) {
            ## Subset overall filters from condx
            if ("CONDID" %in% names(otab)) {
              otab <- otab[paste(otab$PLT_CN, otab$CONDID) %in% pcondID,]
            } else {
              otab <- otab[otab[[joinid]] %in% unique(pltx$CN),]
            }
          }
          if (nrow(otab) == 0) {
            message("othertable must include PLT_CN")
            otab <- NULL
          }
        }

        if (!is.null(otab)) {
          assign(othertablexnm, setDT(otab))
 
          if ("PLT_CN" %in% names(get(othertablexnm))) {
            get(othertablexnm)[, PLT_CN := as.character(PLT_CN)]
            setkey(get(othertablexnm), "PLT_CN")

            ## Subset overall filters from pltx
            assign(othertablexnm, 
			 get(othertablexnm)[get(othertablexnm)[[joinid]] %in% unique(pltx$CN),])
          }
          
          if (returndata) {
		  	if (tolower(othertable) %in% names(tabs)) {
              tabs[[tolower(othertable)]] <- 
						rbind(tabs[[tolower(othertable)]], get(othertablexnm))
	        } else {
	          tabs[[tolower(othertable)]] <- get(othertablexnm)
	        }
 	        if (!tolower(othertable) %in% names(tabIDs)) {
              tabIDs[[tolower(othertable)]] <- "PLT_CN"
	        }
          }

          if (savedata) {
            message("saving ", tolower(othertable), " table...")
            index.unique.other <- NULL
            datExportData(get(othertablexnm),
                index.unique = index.unique.other,
                savedata_opts = list(outfolder = outfolder, 
                                    out_fmt = out_fmt, 
                                    out_dsn = out_dsn, 
                                    out_layer = tolower(othertable),
                                    outfn.pre = outfn.pre, 
                                    overwrite_layer = overwrite_layer,
                                    append_layer = append_layer,
                                    outfn.date = outfn.date, 
                                    add_layer = TRUE))
          }
        }
      }
    }

    ##############################################################
    ## If savePOP or more than one evalType
    ##############################################################
    if ((iseval || savePOP) && !is.null(pltx)) {
      message(paste("\n",
      "## STATUS: GETTING POP_PLOT_STRATUM_ASSGN DATA (", stabbr, ")...", "\n"))
    
      ppsavars <- toString(c("PLT_CN", "STRATUM_CN", "EVALID", "STATECD", "ESTN_UNIT", "STRATUMCD"))
      ppsaqry <- paste("select distinct", ppsavars, 
	                   "\nfrom", ppsafromqry, 
					   "\nwhere statecd =", stcd)
      #ppsanm <- "ppsa"

      if (iseval) {
        if (subsetPOP) {
          ppsaqry <- paste(ppsaqry, "and evalid in(", toString(evalid), ")")
        } else {
          evalstyr <- substr(evalid, 1, nchar(evalid)-2)
          ppsaqry <- paste(ppsaqry, "and evalid like", paste0("'", evalstyr, "%'"))
        }
      }

      if (datsource == "sqlite") {
        ppsax <- tryCatch( DBI::dbGetQuery(dbconn, ppsaqry),
			error=function(e) return(NULL))
      } else {
        ppsax <- tryCatch( sqldf::sqldf(ppsaqry, stringsAsFactors=FALSE), 
			error=function(e) return(NULL))
      }
      if(!is.null(ppsax) && nrow(ppsax) != 0){
        ppsax <- setDT(ppsax)
        ppsax[, PLT_CN := as.character(PLT_CN)]
        setkey(ppsax, PLT_CN)

        ## Subset overall filters from pltx
        ppsax <- ppsax[ppsax$PLT_CN %in% unique(pltx$CN),]

        ## Write query to outfolder
#        if (saveqry) {
#          ppsaqryfnbase <- DBgetfn("ppsa", invtype, outfn.pre, stabbr, 
#			evalid=evalid, qry=TRUE, outfn.date=outfn.date)
#          ppsaqryfn <- fileexistsnm(outfolder, ppsaqryfnbase, "txt")
#          outfile <- file(paste0(outfolder, "/", ppsaqryfn, ".txt"), "w")
#          cat(  paste0(ppsaqry, xfilter), "\n", file=outfile)
#          close(outfile)
#        }
      }
      if (returndata) {
        ppsa <- rbind(ppsa, ppsax)
      }
    }
    if (returndata) {
	  if ("plt" %in% names(tabs)) {
        tabs$plt <- rbind(tabs$plt, data.frame(pltx))
	  } else {
	    tabs$plt <- data.frame(pltx)
	  }
	  if (!"plt" %in% names(tabIDs)) {
        tabIDs$plt <- "CN"
	  }
	  if ("cond" %in% names(tabs)) {
        tabs$cond <- rbind(tabs$cond, data.frame(condx))
	  } else {
	    tabs$cond <- data.frame(condx)
	  }
 	  if (!"cond" %in% names(tabIDs)) {
        tabIDs$cond <- "PLT_CN"
	  }
    }

    ###############################################################################
    ###############################################################################
    ## SAVE data
    ###############################################################################
    ###############################################################################
    if ((savedata || !treeReturn) && !is.null(pltx)) {
      message("saving data...")
      col.names <- ifelse (i == 1, TRUE, FALSE)

      if (savedata && getxy && issp) {
        message("saving spatial xy data...")
        #xycoords <- getcoords(coordType)
		#xycoords <- c(xvarnm, yvarnm)

        if (xymeasCur) {
          #spxynm <- paste0("xyCur_", coordType)
          spxynm <- paste0("xy_", coordType)
          xynm <- paste0("xyCurx_", coordType)
          xyplt <- get(xynm)
        } else {
          xynm <- paste0("xy_", coordType)
          xynm <- paste0("xyx_", coordType)
          xyplt <- get(xynm)
        }
        if (!is.null(xyplt)) {
          out_fmt_sp <- ifelse(out_fmt == "csv", "shp", out_fmt)
		  savedata_opts = list(out_dsn = out_dsn, 
                             out_fmt = out_fmt_sp, 
                             outfolder = outfolder, 
                             out_layer = spxynm, 
                             outfn.date = outfn.date, 
                             overwrite_layer = overwrite_layer, 
                             append_layer = append_layer, 
                             outfn.pre = outfn.pre, 
                             overwrite_dsn = overwrite_dsn)
		  if ("sf" %in% class(xyplt)) {
		    assign(spxynm, xyplt)
		    if (savedata) {
		      spExportSpatial(xyplt, savedata_opts = savedata_opts)
			} 			
		  } else {
            if (!is.null(pltx) && length(unique(xyplt$PLT_CN)) != nrow(pltx))
              warning("number of plots in ", spxynm, " does not match plot table")            
            
            ## Generate spatial output
            assign(spxynm, spMakeSpatialPoints(xyplt = xyplt, 
                              xvar = xvarnm, 
                              yvar = yvarnm, 
                              xy.uniqueid = "PLT_CN", 
                              xy.crs = 4269, 
                              addxy = TRUE, 
                              exportsp = savedata, 
                              savedata_opts = savedata_opts))
		  }
        }
      }

      if (savedata && getxy && !issp) {
        #xycoords <- getcoords(coordType)
		#xycoords <- c(xvarnm, yvarnm)

        if (xymeasCur) {
          #xynm <- paste0("xyCur_", coordType)
          xynm <- paste0("xy_", coordType)
          xyplt <- get(paste0("xyCurx_", coordType))
        } else {
          xynm <- paste0("xy_", coordType)
          xyplt <- get(paste0("xyx_", coordType))
        }

        if (!is.null(xyplt)) {
          message("saving xy data...")
          index.unique.xyplt = index.xyplt <- NULL
          if (!append_layer) {
            index.unique.xyplt <- list("PLT_CN")
            if (all(c("STATECD","UNITCD","COUNTYCD","PLOT","INVYR") %in% names(xyplt))) {
              index.unique.xyplt <- append(index.unique.xyplt, 
                   list(c("STATECD","UNITCD","COUNTYCD","PLOT","INVYR")))
            } else if (all(c("STATECD","UNITCD","COUNTYCD","PLOT") %in% names(xyplt))) {
              index.xyplt <- append(index.xyplt, 
                   c("STATECD","UNITCD","COUNTYCD","PLOT"))
            }
          } 
          datExportData(xyplt, 
              index.unique = index.unique.xyplt,
              index = index.xyplt,
              savedata_opts = list(outfolder = outfolder, 
                                   out_fmt = out_fmt, 
                                   out_dsn = out_dsn, 
                                   out_layer = xynm,
                                   outfn.pre = outfn.pre, 
                                   outfn.date = outfn.date, 
                                   overwrite_layer = overwrite_layer,
                                   append_layer = append_layer,
                                   add_layer = TRUE)) 
        }
      }  
 
      if (savedata && !is.null(spconddatx)) {
        message("saving spatial condition data...")
        index.unique.spconddat <- NULL
        if (!append_layer) index.unique.spconddatx <- "PLT_CN"
        datExportData(spconddatx, 
              index.unique = index.unique.spconddatx,
              savedata_opts = list(outfolder = outfolder, 
                                   out_fmt = out_fmt, 
                                   out_dsn = out_dsn, 
                                   out_layer = "spconddat",
                                   outfn.pre = outfn.pre, 
                                   overwrite_layer = overwrite_layer,
                                   append_layer = append_layer,
                                   outfn.date = outfn.date, 
                                   add_layer = TRUE)) 
      }
      if (savedata && !is.null(pltx)) {
        message("saving plot table...")

        index.unique.pltx = index.pltx <- NULL
        if (!append_layer) {
          index.unique.pltx <- list("CN")
          if (all(c("STATECD","UNITCD","COUNTYCD","PLOT","INVYR") %in% names(pltx))) {
            index.unique.pltx <- append(index.unique.pltx, 
                        list(c("STATECD","UNITCD", "COUNTYCD","PLOT","INVYR")))
          }
        } 
        datExportData(pltx, 
              index.unique = index.unique.pltx,
              index = index.pltx,
              savedata_opts = list(outfolder = outfolder, 
                                   out_fmt = out_fmt, 
                                   out_dsn = out_dsn, 
                                   out_layer = "plot",
                                   outfn.pre = outfn.pre, 
                                   overwrite_layer = overwrite_layer,
                                   append_layer = append_layer,
                                   outfn.date = outfn.date, 
                                   add_layer = TRUE))
        #rm(pltx)
        # gc() 
      }
      if (savedata && !is.null(condx)) {
        message("saving cond table...")

        index.unique.condx <- NULL
        if (!append_layer) index.unique.condx <- c("PLT_CN", "CONDID")
        datExportData(condx, 
              index.unique = index.unique.condx,
              savedata_opts = list(outfolder = outfolder, 
                                   out_fmt = out_fmt, 
                                   out_dsn = out_dsn, 
                                   out_layer = "cond",
                                   outfn.pre = outfn.pre, 
                                   overwrite_layer = overwrite_layer,
                                   append_layer = append_layer,
                                   outfn.date = outfn.date, 
                                   add_layer = TRUE)) 
        #rm(condx)
        # gc()
      }  
      if (savedata && savePOP && !is.null(ppsax)) {
        message("saving pop_plot_stratum_assgn table...")

        #index.unique.ppsax <- NULL
        #if (i == 1) index.unique.ppsax <- "PLT_CN"
        datExportData(ppsax, 
            savedata_opts=list(outfolder = outfolder, 
                               out_fmt = out_fmt, 
                               out_dsn = out_dsn, 
                               out_layer = "pop_plot_stratum_assgn",
                               outfn.pre = outfn.pre, 
                               overwrite_layer = overwrite_layer,
                               append_layer = append_layer,
                               outfn.date = outfn.date, 
                               add_layer = TRUE)) 
        rm(ppsax)
        # gc()
      }
    }
    rm(pltcondx)
    gc()

    if (datsource == "datamart" && datamartType == "SQLITE") {
      DBI::dbDisconnect(dbconn)
    }
  } ## end loop for states
  close(pb)

  if (savedata) {
    message("saving ref_species...")

    datExportData(ref_species, 
        savedata_opts=list(outfolder = outfolder, 
                           out_fmt = out_fmt, 
                           out_dsn = out_dsn, 
                           out_layer = "ref_species",
                           outfn.pre = outfn.pre, 
                           overwrite_layer = overwrite_layer,
                           append_layer = append_layer,
                           outfn.date = outfn.date, 
                           add_layer = TRUE)) 
  }
 
  if (savedata && saveSURVEY) {
    message("saving survey table...")

    datExportData(SURVEY, 
        savedata_opts=list(outfolder = outfolder, 
                           out_fmt = out_fmt, 
                           out_dsn = out_dsn, 
                           out_layer = "survey",
                           outfn.pre = outfn.pre, 
                           overwrite_layer = overwrite_layer,
                           append_layer = append_layer,
                           outfn.date = outfn.date, 
                           add_layer = TRUE)) 
  }
 
  ## Write out plot/condition counts to comma-delimited file.
  if (savedata && !is.null(pltcnt)) {
    message("saving pltcnt table...")

    datExportData(pltcnt, 
        savedata_opts=list(outfolder = outfolder, 
                            out_fmt = out_fmt, 
                            out_dsn = out_dsn, 
                            out_layer = "pltcnt",
                            outfn.pre = outfn.pre, 
                            overwrite_layer = overwrite_layer,
                            append_layer = append_layer,
                            outfn.date = outfn.date, 
                            add_layer = TRUE)) 
  }

  ## GENERATE RETURN LIST
  if (returndata) {
    returnlst <- list(states=states)
    returnlst$tabs <- tabs
    returnlst$tabIDs <- tabIDs
    returnlst$dbqueries <- dbqueries
    returnlst$puniqueid <- puniqueid

    if (getxy) {
      if (xymeasCur) {
        xynm <- paste0("xyCur_", coordType)
        #xynm <- paste0("xyCur_", coordType)
        #assign(xynm, get(paste0("xyCur_", coordType))) 
        returnlst[[paste0("xy_", coordType)]] <- get(xynm)
      } else {
        xynm <- paste0("xy_", coordType)
        #assign(xynm, get(paste0("xy_", coordType))) 
        returnlst[[paste0("xy_", coordType)]] <- get(xynm)
      } 
 
      if (issp) {
	    addxy <- TRUE
	    if (all(c(xvar, yvar) %in% names(get(xynm)))) {
		  addxy <- FALSE
		}
	    spxynm <- paste0("xy_", coordType)
        assign(spxynm, 
		     spMakeSpatialPoints(xyplt=get(xynm), 
		              xvar=xvarnm, yvar=yvarnm, 
		              xy.uniqueid="PLT_CN", xy.crs=4269, addxy=addxy))
        returnlst[[spxynm]] <- get(spxynm)
      }
    }
    if (!is.null(spconddat)) {
      returnlst$spconddat <- data.frame(spconddatx)
    }
#    if (savePOP && exists(ppsanm) && is.data.frame(get(ppsanm))) {
#      returnlst$pop_plot_stratum_assgn <- data.frame(get(ppsanm))
#    }
    if (savePOP) {
      returnlst$pop_plot_stratum_assgn <- ppsa
    }
     if (saveSURVEY && !is.null(SURVEY)) {
      returnlst$SURVEY <- data.frame(SURVEY)
    }
 
    if (length(evalidlist) > 0) {
      returnlst$evalid <- evalidlist
    }
    returnlst$pltcnt <- pltcnt
    returnlst$invyrs <- invyrs

    if (!is.null(evalInfo)) {
      returnlst$evalInfo <- evalInfo
    }
  }

  if (returndata && !is.null(evalidlist)) {
    evaliddf <- data.frame(do.call(rbind, evalidlist))
    stcds <- pcheck.states(row.names(evaliddf), "VALUE")
    evaliddf <- data.frame(stcds, row.names(evaliddf), evaliddf, row.names=NULL)
    names(evaliddf) <- c("STATECD", "STATE", "EVALID")
    evaliddf <- evaliddf[order(evaliddf$STATECD), ]
    returnlst$evalid <- evalidlist
	returnlst$ref_species <- ref_species

#    if (savedata) {
#      append_layer2 <- ifelse(overwrite, FALSE, append_layer)
#      datExportData(evaliddf, outfolder=outfolder, 
#			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="evalid", 
#			outfn.date=outfn.date, overwrite_layer=overwrite,
#			append_layer=append_layer2, outfn.pre=outfn.pre)
#    }
  }
  
  #if (saveqry) cat("\n", paste("Saved queries to:", outfolder), "\n") 


  ## Return data list
  if (returndata) {
    returnlst$args <- args
    return(returnlst)
  }
}

