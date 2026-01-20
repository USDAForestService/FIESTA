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
#' \tab PLOT_ID - Unique Identifier for a plot ('PID' + STATECD(2) + UNITCD(2) +
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
#' @param datsource String. Source of data ('datamart', 'sqlite', 'postgres').
#' @param data_dsn String. If datsource='sqlite', the name of SQLite database
#' (*.sqlite).
#' @param dbTabs List. Source of tables needed for estimation based on what
#' is defined in eval_opts(Type). The source can be a layer in data_dsn or
#' a comma delimited file. For example, if Type='P2VEG', vsubpspp_layer
#' and/or vsubpstr_layer must be defined. Defaults are 'P2VEG_SUBPLOT_SPP'
#' and 'P2VEG_SUBP_STRUCTURE', respectively.
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
#' @param greenwt Logical. If TRUE, green weight biomass is calculated.
#' @param addplotgeom Logical. If TRUE, variables from the PLOTGEOM table are
#' appended to the plot table.
#' @param addfvsid Logical. If TRUE, append the stand_id variable from the
#' FVS_STANDINIT_PLOT table to the plot table and the stand_id variable from the
#' FVS_STANDINIT_COND table to the cond table.
#' @param addexpns Logical. If TRUE, the EXPNS variable from the pop_stratum table
#' is appended to the plot table.
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
#' @param lowernames Logical. If TRUE, output data with lowercase variables.
#' @param returndata Logical. If TRUE, returns data objects.
#' @param savedata Logical. If TRUE, saves data to outfolder as comma-delimited
#' file (*.csv).  No objects are returned. If FALSE, the data are saved as R
#' objects and returned to user.  See details for caveats.
#' @param exportsp Logical. If TRUE, and issp=TRUE, exports spatial plots.
#' @param saveqry Logical. If TRUE, saves queries to outfolder (by state).
#' @param savePOP Logical. If TRUE, save and return the POP_PLOT_STRATUM_ASSGN
#' table.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. If out_layer = NULL,
#' @param dbconn Open database connection.
#' @param dbconnopen Logical. If TRUE, keep database connection open.
#' @param database_opts List. See help(database_options()) for a list
#' of options. Only used when datsource = 'postgres'.  
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
                        greenwt = FALSE,
                        addplotgeom = FALSE,
                        addfvsid = FALSE,
                        addexpns = FALSE,
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
                        lowernames = FALSE,
                        returndata = TRUE,
                        savedata = FALSE,
                        exportsp = FALSE,
                        saveqry = FALSE,
                        savePOP = FALSE,
                        savedata_opts = NULL,
                        dbconn = NULL,
                        dbconnopen = TRUE,
                        database_opts = database_options(),
                        evalInfo = NULL,
                        ...
                        ) {

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  #gui <- ifelse(nargs() == 0, TRUE, FALSE)
  gui <- FALSE
  saveSURVEY=isveg=ischng=isdwm=isgrm=islulc=isinv=issccm=subcycle99=biojenk=savePOP2 <- FALSE
  datamartType <- "CSV"

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
  vsubpsppnm=vsubpstrnm=invsubpnm=fvsstandplotnm=fvsstandcondnm=
	subplotnm=subpcondnm=sccmnm=grmnm=dwmnm=surveynm=evalidnm=condcnnm=
  pltcondx=GREENBIO_AG=DRYBIO_AG=DRYWT_TO_GREENWT_CONVERSION=pltflds=condflds <- NULL


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
  pcheck.params(input.params,
                savedata_opts = savedata_opts,
                eval_opts = eval_opts,
                xy_opts = xy_opts)

  ## Check eval_opts
  if (length(eval_opts) == 0) {
    message("no evaluation timeframe specified...")
    message("see eval and eval_opts parameters (e.g., eval='custom', eval_opts=eval_options(Cur=TRUE))\n")
    stop()
  }

  ## Check parameter option lists
  optslst <- pcheck.opts(optionlst = list(
                         savedata_opts = savedata_opts,
                         eval_opts = eval_opts,
                         xy_opts = xy_opts))
  savedata_opts <- optslst$savedata_opts
  eval_opts <- optslst$eval_opts
  xy_opts <- optslst$xy_opts

  for (i in 1:length(eval_opts)) {
    assign(names(eval_opts)[[i]], eval_opts[[i]])
  }
  for (i in 1:length(xy_opts)) {
    assign(names(xy_opts)[[i]], xy_opts[[i]])
  }

  ## dbTables
  ###################################################################
  ## Set user-supplied dbTabs options
  dbTables_defaults_list <- formals(dbTables)[-length(formals(dbTables))]
  dbTabs2 <- dbTables_defaults_list
  if (length(dbTabs) > 0) {
    for (i in 1:length(dbTabs)) {
      if (names(dbTabs)[[i]] %in% names(dbTables_defaults_list)) {
        if (!is.null(dbTabs[[i]])) {
          dbTabs2[[names(dbTabs)[[i]]]] <- dbTabs[[i]]
        }
      }
    }
  }
  for (i in 1:length(dbTabs2)) {
    assign(names(dbTabs2)[[i]], dbTabs2[[i]])
  }

  ## Check isveg
  if ("isveg" %in% names(args)) {
    message("the parameter isveg is deprecated... use eval_options(Type='P2VEG'))\n")
    isveg <- args$isveg
  }

  ## Define variables
  actual=getinvyr <- FALSE
  SCHEMA. <- ""
  isRMRS <- FALSE
  xycoords = c("LON_PUBLIC", "LAT_PUBLIC")
  #coordType <- "PUBLIC"
  parameters=savePOPall=indb <- FALSE


  ########################################################################
  ### GET PARAMETER INPUTS
  ########################################################################
  iseval <- FALSE
  subsetPOP <- TRUE

  ## Check invtype
  invtypelst <- c('ANNUAL', 'PERIODIC', 'BOTH')
  invtype <- pcheck.varchar(invtype, varnm="invtype", checklst=invtypelst,
		caption="Inventory Type", gui=gui)


  ## GETS DATA TABLES (OTHER THAN PLOT/CONDITION) IF NULL
  ###########################################################
  #Typelst <- c("ALL", "CURR", "VOL", "P2VEG", "DWM", "INV", "CHNG",
  #             "GROW", "MORT", "REMV", "GRM")
  #endTypelst <- c("00", "01", "01", "10", "07", "09", "06", "03", "04", "05", "03")
  #endTypelst <- c("00", "01", "01", "10", "07", "09", "03", "03", "03", "03", "03")

  if (!is.null(evalid)) {
    endType <- sort(unique(sapply(evalid, function(x) substr(x, nchar(x)-1, nchar(x)))))
    Typechk <- ifelse(endType == "00", "ALL",
                   ifelse(endType == "01", "VOL",
                          ifelse(endType == "10", "P2VEG",
                                 ifelse(endType == "07", "DWM",
                                        ifelse(endType == "09", "INV",
                                               ifelse(endType %in% c("04","05"), "GRM",
                                              "CHNG"))))))
    if (Typechk == "CHNG") {
      if (Type != "GRM") {
        Type <- Typechk
      }
    }

  } else {
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

#  biojenk <- pcheck.logical(biojenk, varnm="biojenk",
#		title="Jenkins biomass?", first="NO", gui=gui)

  greenwt <- pcheck.logical(greenwt, varnm="greenwt",
		title="Green weight?", first="NO", gui=gui)

  if ((biojenk || greenwt) && !istree) {
    istree <- TRUE
  }
  if (all(Type %in% c("CHNG", "P2VEG", "INV", "DWM"))) {
    istree <- FALSE
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

  ## Check savePOP
  savePOP <- pcheck.logical(savePOP, varnm="savePOP",
		title="Return POP table", first="NO", gui=gui)

  if (savePOP || addexpns) {
    savePOPall <- TRUE
  }

  ## Check othertables
  if (!is.null(othertables)) {
    if ("POP_PLOT_STRATUM_ASSGN" %in% othertables) {
      savePOP <- TRUE
      othertables <- othertables[othertables != "POP_PLOT_STRATUM_ASSGN"]
    }
#    if (savePOPall) {
#      othertables <- othertables[othertables %in% c("POP_STRATUM", "POP_ESTN_UNIT")]
#    }
  }

  ## Check eval_opts - Endyr.filter
  if (!is.null(Endyr.filter)) {
    message("use spGetPlots with Endyr.filter")
	  stop()
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
  returnPOP <- ifelse (datsource == "datamart", TRUE, FALSE)

  if (!is.null(evalInfo)) {
    list.items <- c("states", "invtype")
    evalInfo <- pcheck.object(evalInfo, "evalInfo", list.items=list.items)

  } else {

    if (invtype == "BOTH") {
      message("getting evalution info for annual data...")
      evalInfoA <- tryCatch( 
        DBgetEvalid(states = states,
                    RS = RS,
                    datsource = datsource,
                    data_dsn = data_dsn,
                    dbconn = dbconn,
                    invtype = "ANNUAL",
                    evalid = evalid,
                    evalCur = evalCur,
                    evalEndyr = evalEndyr,
                    evalAll = evalAll,
                    evalType = Type,
                    dbTabs = dbTabs,
                    database_opts = database_opts,
                    returnPOP = returnPOP),
			             error = function(e) {
                      message(e,"\n")
                      return(NULL) })
      if (is.null(evalInfoA)) {
        iseval <- FALSE
      } else {

        message("getting evalution info for periodic data...")
        evalInfoP <- tryCatch( 
          DBgetEvalid(states = states,
                      RS = RS,
                      datsource = datsource,
                      data_dsn = data_dsn,
                      dbconn = dbconn,
                      invtype = "PERIODIC",
                      evalid = evalid,
                      evalCur = evalCur,
                      evalEndyr = evalEndyr,
                      evalAll = evalAll,
                      evalType = Type,
                      dbTabs = dbTabs,
                      database_opts = database_opts,
                      returnPOP = returnPOP),
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
      evalInfo <- tryCatch( 
        DBgetEvalid(states = states,
                    RS = RS,
                    datsource = datsource,
                    data_dsn = data_dsn,
                    dbconn = dbconn,
                    invtype = invtype,
                    evalid = evalid,
                    evalCur = evalCur,
                    evalEndyr = evalEndyr,
                    evalAll = evalAll,
                    evalType = Type,
                    dbTabs = dbTabs,
                    database_opts = database_opts,
                    returnPOP = returnPOP),
                  error = function(e) {
                    message(e,"\n")
                    return(NULL) })
    }
    if (is.null(evalInfo)) {
	    message("returning NULL")
      return(NULL)
    }
  }

  if (is.null(evalInfo)) stop("no data to return")
  states <- evalInfo$states
  stcdlst <- evalInfo$stcdlst
  evalidlist <- evalInfo$evalidlist
  evalEndyrlist <- evalInfo$evalEndyrlist
  evalTypelist <- evalInfo$evalTypelist
  invtype <- evalInfo$invtype
  invyrtab <- evalInfo$invyrtab
  if (length(evalidlist) > 0) {
    invyrs <- evalInfo$invyrs
    iseval=savePOP <- TRUE
  }
  datsource <- evalInfo$datsource
  dbconn <- evalInfo$dbconn
  dbtablst <- evalInfo$dbtablst
  schema <- evalInfo$schema
  surveynm <- evalInfo$surveynm
  SURVEY <- evalInfo$SURVEY
  POP_PLOT_STRATUM_ASSGNe <- evalInfo$POP_PLOT_STRATUM_ASSGN
  PLOTe <- evalInfo$PLOT
  if (!is.null(schema)) SCHEMA. <- paste0(schema, ".")

  if (savePOP) {
    if (!is.null(POP_PLOT_STRATUM_ASSGNe) && is.data.frame(POP_PLOT_STRATUM_ASSGNe)) {
      ppsaflds <- names(POP_PLOT_STRATUM_ASSGNe)
      statenm <- findnm("STATECD", ppsaflds)
      if (is.null(statenm)) {
        stop("POP_PLOT_STRATUM_ASSGN must include STATECD")
      }
#      if (length(unique(POP_PLOT_STRATUM_ASSGNe[[statenm]])) != length(states)) {
#        stop("invalid POP_PLOT_STRATUM_ASSGN from DBgetEvalid")
#      }
    }
  }

  if (!is.null(PLOTe) && is.data.frame(PLOTe)) {
    pltflds <- names(PLOTe)
    statenm <- findnm("STATECD", pltflds)
    if (is.null(statenm)) {
      PLOTe <- NULL
    } else {
      #if (length(unique(PLOTe[[statenm]])) != length(states)) {
      if (!all(stcdlst %in% unique(PLOTe[[statenm]]))) {
        message("invalid PLOT from DBgetEvalid")
        PLOTe <- NULL
      }
    }
  }

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
  ## Check issp
  issp <- pcheck.logical(issp, varnm="issp",
               title="SpatialPoints of plot vars?", first="NO", gui=gui)
  if (issp && !getxy) {
    message("issp=TRUE, but getxy = FALSE... no xy extracted")
  }

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

  if (spcond) {
    ## Check spcondid1
    ###########################################################
    spcondid1 <- pcheck.logical(spcondid1, varnm="spcondid1",
		                 title="Use cond1 for spatial?", first="YES", gui=gui)
  }
  

  ########################################################################
  ### Saving data
  ########################################################################

  ## check addplotgeom
  addplotgeom <- pcheck.logical(addplotgeom, varnm="addplotgeom",
                               title="Add plotgeom?", first="NO", gui=gui)

  ## check addfvsid
  addfvsid <- pcheck.logical(addfvsid, varnm="addfvsid",
                               title="Add FVS Id?", first="NO", gui=gui)

  ## check addexpns
  addexpns <- pcheck.logical(addexpns, varnm="addexpns",
                               title="Add strata expansions?", first="NO", gui=gui)

  ## check lowernames
  lowernames <- pcheck.logical(lowernames, varnm="lowernames",
                               title="Lowercase names?", first="NO", gui=gui)
  ## check returndata
  returndata <- pcheck.logical(returndata, varnm="returndata",
		    title="Return data?", first="YES", gui=gui)
  
  ## check savedata
  savedata <- pcheck.logical(savedata, varnm="savedata",
		              title="Save data to outfolder?", first="YES", gui=gui)
  
  ## check exportsp
  exportsp <- pcheck.logical(exportsp, varnm="exportsp",
                  title="Export spatial", first="YES", gui=gui)
  if (!returndata && !savedata) {
    message("both returndata and savedata are FALSE... how would you like output")
    return(NULL)
  }

  if (!returndata && savedata && !exportsp) {
    exportsp <- TRUE
  }

  ## check dbconnopen
  dbconnopen <- pcheck.logical(dbconnopen, varnm="dbconnopen",
                               title="Keep db open?", first="YES", gui=gui)
  
  ## Check saveqry
  saveqry <- pcheck.logical(saveqry, varnm="saveqry",
		             title="Save queries to outfolder?", first="YES", gui=gui)


  ##  Check whether to return tree data
  ###########################################################
  treeReturn <- TRUE
  if (istree && (nbrstates > maxstates.tree) && returndata) {
    warning("tree data object is too big.. writing to folder, no returned object")
    #savedata <- TRUE
    treeReturn <- FALSE
  }

  ## Check outfolder, outfn.date, overwrite_dsn
  ###########################################################
  ## Note: if getxy = TRUE and out_spfmt = "sqlite", the spatial data must 
  ## be saved to the sqlite database first to make it a SpatiaLite database.
  if (savedata | saveqry | parameters | !treeReturn | !returndata) {
    outlst <- pcheck.output(savedata_opts = savedata_opts,
                            createSQLite = FALSE)
    outfolder <- outlst$outfolder
    out_dsn <- outlst$out_dsn
    out_fmt <- outlst$out_fmt
    outlst$add_layer <- TRUE
    append_layer <- outlst$append_layer
    overwrite_layer <- outlst$overwrite_layer
    if (!is.null(outlst$out_conn) && is.null(outlst$outconn)) {
      outlst$outconn <- outlst$outconn
    }
    outlst$out_conn <- NULL
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

    if (addfvsid) {
      condvarlst <- c("CN", condvarlst)
    }
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
    if (iseval) {
      if(savePOP || iseval) {
        ppsa <- {}
      }
      if (savePOPall) {
        popstratum=popestnunit <- {}
      }
    }
    ## Create empty object for each spcoords
    if (getxy) {
      for (coordType in spcoordslst) {
        if (xymeasCur) {
          assign(paste0("xyCur_", coordType), {})
        } else {
          assign(paste0("xy_", coordType), {})
        }
      }
    }
  }

  ## Check data tables
  ##########################################################
  if (datsource %in% c("sqlite", "postgres") && !is.null(dbconn)) {
    indb <- TRUE

    ## Check if plot table is in database
    if (is.null(plotnm)) {
      plotnm <- chkdbtab(dbtablst, plot_layer, stopifnull=FALSE)
    }
    if (is.null(plotnm)) {
      message("there is no PLOT table in database")
    } else {
      pltflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = plotnm, upper = TRUE)
      if (is.null(pltflds)) {
        stop()
      }
    }
    ## Check if cond is in database
    condnm <- chkdbtab(dbtablst, cond_layer, stopifnull=TRUE)
    if (is.null(condnm)) {
      message("there is no COND table in database")
    }
    condflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = condnm, upper = TRUE)
    if (is.null(condflds)) {
      stop()
    }
    
	  if (addplotgeom) {
      plotgeomnm <- chkdbtab(dbtablst, plotgeom_layer)
      if (is.null(plotgeomnm)) {
        message("there is no plotgeom table in database")
        addplotgeom <- FALSE
      } else {
        plotgeomflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = plotgeomnm, upper = TRUE)
		    pgeomvarlst <- pgeomvarlst[pgeomvarlst %in% plotgeomflds]
        pltflds <- unique(c(pltflds, pgeomvarlst))
      }
	  }
    if (addfvsid) {
      fvsstandplotnm <- chkdbtab(dbtablst, fvsstandplot_layer)
      if (is.null(fvsstandplotnm)) {
        message("there is no fvs_standinit_plot table in database")
        addfvsid <- FALSE
      } else {
        fvsstandpvar <- "STAND_ID AS FVS_STAND_ID_PLOT"  ## joins with PLOT$CN
        pltflds <- unique(c(pltflds, fvsstandpvar))
      }
      fvsstandcondnm <- chkdbtab(dbtablst, fvsstandcond_layer)
      if (is.null(fvsstandcondnm)) {
        message("there is no fvs_standinit_cond table in database")
        addfvsid <- FALSE
      } else {
        fvsstandcvar <- "STAND_ID AS FVS_STAND_ID_COND"  ## joins with COND$CN
        cnnm <- findnm("CN", condflds,  returnNULL = TRUE)
        if (!is.null(cnnm)) {
          fvsstandcvar <- toString(c(fvsstandcvar, paste0("c.", cnnm, " AS COND_CN")))
          condcnnm <- "COND_CN"
        }
        condflds <- unique(c(condflds, fvsstandcvar))
      }
    }

    if (is.null(condnm)) {
      pltcondflds <- pltflds
    } else {
      pltcondflds <- unique(c(pltflds, condflds))
    }

    ## Check if pop_plot_stratum_assgn is in database
    if (savePOP) {
	    if (is.null(ppsanm)) {
        ppsanm <- chkdbtab(dbtablst, ppsa_layer)
        if (is.null(ppsanm)) {
          ppsanm <- chkdbtab(dbtablst, "ppsa", stopifnull=TRUE)
		    }
	    }
      if (is.null(ppsanm)) {
        stop("POP_PLOT_STRATUM_ASSGN not in database")
      }
	    if (!is.null(ppsanm)) {
	      ppsaflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = ppsanm, upper = TRUE)
	      if (is.null(ppsaflds)) {
	        stop()
	      }
      }

      ## Check other pop tables in database
      if (savePOPall) {
        popstratumnm <- chkdbtab(dbtablst, popstratum_layer)
        popestnunitnm <- chkdbtab(dbtablst, "pop_estn_unit", stopifnull=TRUE)
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
      chk <- checkidx(dbconn, plotnm, c("STATECD", "UNITCD", "COUNTYCD", "PLOT"), 
                      datsource = datsource, schema = schema)
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
        append_layer <- outlst$append_layer <- TRUE
        overwrite_layer <- outlst$overwrite_layer <- FALSE
      } else {
        if (outlst$overwrite_layer) {
          append_layer <- outlst$append_layer <- FALSE
        } else {
          append_layer <- outlst$append_layer 
        }
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
		spconddatx=cond_chngx <- NULL
    nochngdata <- FALSE

    if (savePOP) {
      ppsax <- NULL
      if (savePOPall) {
        popstratumx=popestnunitx <- NULL
      }
    }
    if (!is.null(othertables)) {
      for (j in 1:length(othertables)) {
        assign(paste0("otherx", j), NULL)
      }
    }

	  dbqueries[[state]] <- list()

    ## If POP_PLOT_STRATUM_ASSGNe from DBgetEvalid is a data.frame, subset to state
    if (savePOP && !is.null(ppsanm) && is.data.frame(POP_PLOT_STRATUM_ASSGNe)) {
      statenm <- findnm("STATECD", ppsaflds)
      POP_PLOT_STRATUM_ASSGN <- POP_PLOT_STRATUM_ASSGNe[POP_PLOT_STRATUM_ASSGNe[[statenm]] == stcd, ]
    }
    ## If PLOTe from DBgetEvalid is a data.frame, subset to state
    if (!is.null(plotnm) && is.data.frame(PLOTe)) {
      statenm <- findnm("STATECD", pltflds)
      PLOT <- PLOTe[PLOTe[[statenm]] == stcd, ]
    }

    ## Get PLOT/COND data
    ###################################################
    if (datsource == "datamart") {
      ## PLOT table
	    if (is.null(plotnm)) {
        PLOT <- DBgetCSV("PLOT", stabbr, returnDT=TRUE, stopifnull=FALSE)
      }
      if (is.null(PLOT)) {
        message("there is no PLOT table in datamart")
      } else {
        plotnm <- "PLOT"
        pltflds <- names(PLOT)
        setkey(PLOT, "STATECD", "UNITCD", "COUNTYCD", "PLOT", "INVYR")
      }
      ## COND table
      COND <- DBgetCSV("COND", stabbr, returnDT=TRUE, stopifnull=FALSE)
      condnm <- "COND"
      condflds <- names(COND)
      setkey(COND, "PLT_CN", "CONDID")

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
      ## FVS tables
      if (addfvsid) {
        FVS_STANDINIT_PLOT <- DBgetCSV("FVS_STANDINIT_PLOT", stabbr, returnDT=TRUE, stopifnull=FALSE)
        if (!is.null(FVS_STANDINIT_PLOT)) {
          fvsstandplotnm <- "FVS_STANDINIT_PLOT"
          fvsstandpvar <- "STAND_ID AS FVS_STAND_ID_PLOT"  ## joins with PLOT$CN
          pltflds <- unique(pltflds, fvsstandpvar)
        } else {
          message("there is no fvs_standinit_plot in datamart")
        }
        FVS_STANDINIT_COND <- DBgetCSV("FVS_STANDINIT_COND", stabbr, returnDT=TRUE, stopifnull=FALSE)
        if (!is.null(FVS_STANDINIT_COND)) {
          fvsstandcondnm <- "FVS_STANDINIT_COND"
          fvsstandcvar <- "STAND_ID AS FVS_STAND_ID_COND"  ## joins with COND$CN

          cnnm <- findnm("CN", condflds,  returnNULL = TRUE)
          if (!is.null(cnnm)) {
            fvsstandcvar <- toString(c(fvsstandcvar, paste0("c.", cnnm, " AS COND_CN")))
            condcnnm <- "COND_CN"
          }
          condflds <- unique(condflds, fvsstandcvar)

        } else {
          message("there is no fvs_standinit_cond in datamart")
        }
      }

      ## Get pltcondflds
      if (!is.null(plotnm)) {
        pltcondflds <- unique(c(pltflds, condflds))
      } else {
        pltcondflds <- condflds
      }

      if (savePOP) {
        ## POP_PLOT_STRATUM_ASSGN table (ZIP FILE) -
        if (is.null(ppsanm)) {
          POP_PLOT_STRATUM_ASSGN <- DBgetCSV("POP_PLOT_STRATUM_ASSGN", stabbr,
		                                         returnDT=TRUE, stopifnull=TRUE)
        }
		    if (!is.null(POP_PLOT_STRATUM_ASSGN)) {
          ppsanm <- "POP_PLOT_STRATUM_ASSGN"
          ppsaflds <- names(POP_PLOT_STRATUM_ASSGN)
		    }
        if (savePOPall) {
          ## POP_STRATUM table (ZIP FILE) -
          POP_STRATUM <- DBgetCSV("POP_STRATUM", stabbr, returnDT=TRUE, stopifnull=FALSE)
          if (!is.null(POP_STRATUM)) {
            popstratumnm <- "POP_STRATUM"
          }
          ## POP_ESTN_UNIT table (ZIP FILE) -
          POP_ESTN_UNIT <- DBgetCSV("POP_ESTN_UNIT", stabbr, returnDT=TRUE, stopifnull=FALSE)
          if (!is.null(POP_ESTN_UNIT)) {
            popestnunitnm <- "POP_ESTN_UNIT"
          }
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
	    if (is.null(plotnm)) {
        PLOT <- pcheck.table(plot_layer, stopifnull=FALSE, stopifinvalid=FALSE)
	    }
      if (is.null(PLOT)) {
        message("the PLOT table does not exist")
      } else {
        plotnm <- "PLOT"
        pltflds <- names(PLOT)
      }

      ## COND table
      COND <- pcheck.table(cond_layer, stopifnull=TRUE, stopifinvalid=TRUE)
      if (is.null(COND)) {
        message("the COND table does not exist")
      }
      condnm <- "COND"
      condflds <- names(COND)


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

      ## FVS tables
      if (addfvsid) {
        FVS_STANDINIT_PLOT <- pcheck.table(fvsstandplot_layer, stopifnull=FALSE, stopifinvalid=FALSE)
        if (!is.null(FVS_STANDINIT_PLOT)) {
          fvsstandplotnm <- "FVS_STANDINIT_PLOT"
          fvsstandpvar <- "STAND_ID AS FVS_STAND_ID_PLOT"  ## joins with PLOT$CN
          pltflds <- unique(pltflds, fvsstandpvar)
        } else {
          message("there is no fvs_standinit_plot table")
        }
        FVS_STANDINIT_COND <- pcheck.table(fvsstandcond_layer, stopifnull=FALSE, stopifinvalid=FALSE)
        if (!is.null(FVS_STANDINIT_COND)) {
          fvsstandcondnm <- "FVS_STANDINIT_COND"
          fvsstandcvar <- "STAND_ID AS FVS_STAND_ID_COND"  ## joins with COND$CN
          condflds <- unique(condflds, fvsstandcvar)
        } else {
          message("there is no fvs_standinit_cond table")
        }
      }

      ## Get pltcondflds
      if (!is.null(plotnm)) {
        pltcondflds <- unique(c(pltflds, condflds))
      } else {
        pltcondflds <- condflds
      }

      ## POP_PLOT_STRATUM_ASSGN table (ZIP FILE) -
      if (savePOP) {
        if (is.null(ppsanm)) {
          POP_PLOT_STRATUM_ASSGN <- pcheck.table(ppsa_layer, stopifnull=TRUE,
                                                 stopifinvalid=TRUE)
		    }
		    if (!is.null(POP_PLOT_STRATUM_ASSGN)) {
          ppsanm <- "POP_PLOT_STRATUM_ASSGN"
          names(POP_PLOT_STRATUM_ASSGN) <- toupper(names(POP_PLOT_STRATUM_ASSGN))
          ppsaflds <- names(POP_PLOT_STRATUM_ASSGN)
		    }

        if (savePOPall) {
          if (!is.null(POP_STRATUM)) {
            popstratumnm <- "POP_STRATUM"
            names(POP_STRATUM) <- toupper(names(POP_STRATUM))
            popstratumflds <- names(POP_STRATUM)
          }
          if (!is.null(POP_ESTN_UNIT)) {
            popestnunitnm <- "POP_ESTN_UNIT"
            names(POP_ESTN_UNIT) <- toupper(names(POP_ESTN_UNIT))
            popestnunitflds <- names(POP_ESTN_UNIT)
          }
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
    if (savePOP) {
      if (!is.null(ppsanm)) {
        ppsafromqry <- paste0(SCHEMA., ppsanm, " ppsa")
      }
      if (savePOPall) {
        if (!is.null(popstratumnm)) {
          pstratumfromqry <- paste0(SCHEMA., popstratumnm, " ppsa")
        }
        if (!is.null(popestnunitnm)) {
          pestnunitfromqry <- paste0(SCHEMA., popestnunitnm, " ppsa")
        }
      }
    }

    ## PLOT from/join query
    ################################################
    if (iseval) {
      pfrom_layer <- ifelse (is.null(plotnm), condnm, plotnm)
      pfromqry <- paste0(ppsafromqry,
          "\nJOIN ", SCHEMA., pfrom_layer, " p ON (p.", puniqueid, " = ppsa.PLT_CN)")

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
      pcplusfromqry <- paste0(pcfromqry, " \nJOIN ", SCHEMA.,
			                plotgeomnm, " pg ON (pg.CN = p.", puniqueid, ")")
    } else {
      pcplusfromqry <- pcfromqry
    }
    if (addfvsid) {
      if (!is.null(fvsstandplotnm)) {
        pcplusfromqry <- paste0(pcplusfromqry, " \nJOIN ", SCHEMA.,
                            fvsstandplotnm, " fvsp ON (fvsp.STAND_CN = p.", puniqueid, ")")
      }
      if (!is.null(fvsstandcondnm)) {
        pcplusfromqry <- paste0(pcplusfromqry, " \nJOIN ", SCHEMA.,
                              fvsstandcondnm, " fvsc ON (fvsc.STAND_CN = ", condcnnm, ")")
      }
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
      Types <- gsub("EXP", "", Types)
      evalFilter <- paste0("ppsa.EVALID IN(", toString(evalid), ")")

      if ("P2VEG" %in% Types) {
        evalid.veg <- evalid[endsWith(as.character(evalid), "10")]
        evalFilter.veg <- paste0("ppsa.EVALID IN (", toString(evalid.veg), ")")
      } else {
        evalFilter.veg <- evalFilter
      }

      if ("INV" %in% Types) {
	      evalid.inv <- evalid[endsWith(as.character(evalid), "09")]
        evalFilter.inv <- paste0("ppsa.EVALID IN (", toString(evalid.inv), ")")
      } else {
        evalFilter.inv <- evalFilter
      }

      if ("DWM" %in% Types) {
	  	  evalid.dwm <- evalid[endsWith(as.character(evalid), "07")]
        evalFilter.dwm <- paste0("ppsa.EVALID IN (", toString(evalid.dwm), ")")
      } else {
        evalFilter.dwm <- evalFilter
      }
      if (any(c("GROW", "MORT", "REMV", "GRM", "CHNG") %in% Types)) {
	  	  evalid.grm <- evalid[endsWith(as.character(evalid), "03")]
        evalFilter.grm <- paste0("ppsa.EVALID IN (", toString(evalid.grm), ")")
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
        filterdb <- sort(na.omit(sqldf::sqldf(filterdbqry, connection  = NULL)[[1]]))

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
      if (is.null(fvsstandplotnm) && is.null(fvsstandcondnm)) {
        addfvsid <- FALSE
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
		    if (!is.null(lonfld) && !"LON_PUBLIC" %in% pltvarlst && "LON" %in% pltvarlst) {
		      pltvarlst <- sub("LON", "LON_PUBLIC", pltvarlst)
		    }
	    }
	    if (!"LAT" %in% pltcondflds) {
	      latfld <- findnm("LAT_PUBLIC", pltcondflds, returnNULL=TRUE)
		    if (!is.null(latfld) && !"LAT_PUBLIC" %in% pltvarlst && "LAT" %in% pltvarlst) {
		      pltvarlst <- sub("LAT", "LAT_PUBLIC", pltvarlst)
		    }
	    }
      pltvarlst <- pltvarlst[pltvarlst %in% pltcondflds]
      condvarlst <- condvarlst[condvarlst %in% pltcondflds]

      ## Add commas
      pcvars <- NULL
      if (length(pltvarlst) > 0) {
        pcvars <- toString(paste0("p.", pltvarlst))
        if (addplotgeom) {
          #pgeomvarlst <- pgeomvarlst[pgeomvarlst %in% plotgeomflds]
          pcvars <- toString(c(pcvars, paste0("pg.", pgeomvarlst)))
        }
        if (addfvsid && !is.null(fvsstandplotnm)) {
          pcvars <- toString(c(pcvars, paste0("fvsp.", fvsstandpvar)))
        }
      }
      ## Add condition level variables
      pcvars <- toString(c(pcvars, paste0("c.", condvarlst)))
      if (addfvsid && !is.null(fvsstandcondnm)) {
        pcvars <- toString(c(pcvars, paste0("fvsc.", fvsstandcvar)))
      }

      ## Create pltcond query
      if (addplotgeom || addfvsid) {
        pltcond.qry <- paste0("SELECT ", pcvars,
		                         "\nFROM ", pcplusfromqry,
                             "\nWHERE ", xfilter)
      } else {
        pltcond.qry <- paste0("SELECT ", pcvars,
		                         "\nFROM ", pcfromqry,
                             "\nWHERE ", xfilter)
      }
	    if (!"pltcond" %in% names(dbqueries[[state]])) {
        dbqueries[[state]]$pltcond <- pltcond.qry
	    }

      ## Run pltcond query
      if (indb) {
        pltcondx <- tryCatch(
          DBI::dbGetQuery(dbconn, pltcond.qry),
			                 error=function(e)
			                   message(e, "\n"))
      } else {
        pltcondx <- tryCatch(
          sqldf::sqldf(pltcond.qry, stringsAsFactors=FALSE, connection=NULL),
                       error=function(e)
                         message(e, "\n"))
      }
      if (is.null(pltcondx)) {
        message("invalid query for plot/cond tables")
        message(pltcond.qry)
      } else {
        names(pltcondx) <- toupper(names(pltcondx))
        
        pltcondx <- data.table::setDT(pltcondx)
        names(pltcondx) <- toupper(names(pltcondx))
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
      if (addfvsid) {
        pltvarlst2 <- unique(c(pltvarlst2, "FVS_STAND_ID_PLOT"))
      }
      #if (iseval) pltvarlst2 <- c(pltvarlst2, "EVALID")
      condvarlst2 <- condvarlst
      if (addfvsid) {
        condvarlst2 <- unique(c(condvarlst2, "FVS_STAND_ID_COND"))
      }

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
        pltx[[puniqueid]] <- as.character(pltx[[puniqueid]])
        setkeyv(pltx, puniqueid)

        prev_pltcnnm <- findnm("PREV_PLTCN", names(pltx), returnNULL = TRUE)
        if (!is.null(prev_pltcnnm)) {
          pltx[[prev_pltcnnm]] <- as.character(pltx[[prev_pltcnnm]])
        }
      }
      if (!is.null(condvarlst) && "CONDID" %in% names(pltcondx)) {
        condx <- unique(pltcondx[, condvarlst2, with=FALSE])
        condx[, PLT_CN := as.character(PLT_CN)]
        setkey(condx, PLT_CN, CONDID)

        if ("MACRPROP_UNADJ" %in% names(condx) && !is.numeric(condx$MACRPROP_UNADJ)) {
          condx$MACRPROP_UNADJ <- as.numeric(condx$MACRPROP_UNADJ)
        }
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
        pltx[, PLOT_ID := paste0("PID",
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
      
      ## Check for xy coordinates
      if (getxy) {
        xvarchk <- findnm(xvar, pltflds, returnNULL = TRUE)
        if (is.null(xvarchk) && toupper(xvar) == "LON") {
          xvar <- findnm("LON_PUBLIC", pltflds, returnNULL = TRUE)
          if (!is.null(xvar)) {
            xy_opts$xvar <- xvar
          }
        }
        yvarchk <- findnm(yvar, pltflds, returnNULL = TRUE)
        if (is.null(yvarchk) && toupper(yvar) == "LAT") {
          yvar <- findnm("LAT_PUBLIC", pltflds, returnNULL = TRUE)
          if (!is.null(yvar)) {
            xy_opts$yvar <- yvar
          }
        }
        if (any(is.null(xvar), is.null(yvar))) {
          getxy <- FALSE
        }
      }
      
      if (getxy) {
        if (is.null(pjoinid)) pjoinid <- puniqueid
        if (is.null(xy_datsource)) {
          xy_datsource <- datsource
          xy_dsn <- data_dsn
        }

        if (is.data.frame(xy)) {
          xysource <- "obj"
        } else if (is.data.frame(get(xy))) {
          if (exists(xy)) {
            xy <- get(xy)
          }
          xysource <- "obj"
        } else {
          xysource <- xy_datsource
        }
        xycoords <- c(xvar, yvar)

        if (!is.null(plotnm) && exists(plotnm) && !is.function(get(plotnm))) {
          dbTabs$plot_layer <- get(plotnm)
        } else {
          dbTabs$plot_layer <- plotnm
        }
		    if (!is.null(POP_PLOT_STRATUM_ASSGN)) {
		      dbTabs$ppsa_layer <- POP_PLOT_STRATUM_ASSGN
		    }
		    if (!is.null(SURVEY)) {
		      dbTabs$survey_layer <- SURVEY
		    }

        if (!stcd %in% c(64,78) && xymeasCur) {
          xydat <- suppressMessages(
            DBgetXY(states = state,
                           xy_datsource = xysource,
                           xy_dsn = xy_dsn,
                           xy = xy,
                           xy_opts = xy_opts,
                           dbconn = dbconn,
                           datsource = datsource,
                           data_dsn = data_dsn,
                           dbTabs = dbTabs,
                           eval = "custom",
                           eval_opts = eval_options(Cur = TRUE, varCur=varCur, Type="ALL"),
                           database_opts = database_opts,
                           pjoinid = pjoinid,
                           intensity1 = intensity1,
                           pvars2keep = c("INVYR", "PLOT_STATUS_CD", "INTENSITY")))
		      xyxnm <- paste0("xyCurx_", coordType)
		      xynm <- paste0("xyCur_", coordType)
		      xytable <- xydat[[1]][xydat[[1]][[xydat$xy_opts$xyjoinid]] %in% pltx[[xydat$pjoinid]], ]
		      if (lowernames) {
		        names(xytable) <- tolower(names(xytable))
		      }
		      assign(xyxnm, xytable)
          if (returndata) {
            assign(xynm, rbind(get(xynm), get(xyxnm)))
          }
        } else {
          xydat <- DBgetXY(states = state,
                           xy_datsource = xysource,
                           xy_dsn = xy_dsn,
                           xy = xy,
                           xy_opts = xy_opts,
                           dbconn = dbconn,
                           datsource = datsource,
                           data_dsn = data_dsn,
                           dbTabs = dbTabs,
                           eval = eval,
                           eval_opts = eval_opts,
                           database_opts = database_opts,
                           pjoinid = pjoinid,
                           intensity1 = intensity1,
                           pvars2keep = c("INVYR", "PLOT_STATUS_CD", "INTENSITY"),
						               evalInfo = evalInfo,
                           SURVEY = SURVEY,
                           POP_PLOT_STRATUM_ASSGN = POP_PLOT_STRATUM_ASSGN)
		      xyxnm <- paste0("xyx_", coordType)
		      xynm <- paste0("xy_", coordType)
		      xytable <- xydat[[1]][xydat[[1]][[xydat$xy_opts$xyjoinid]] %in% pltx[[xydat$pjoinid]], ]
		      if (lowernames) {
		        names(xytable) <- tolower(names(xytable))
		      }
		      assign(xyxnm, xytable)
          if (returndata) {
            assign(xynm, rbind(get(xynm), get(xyxnm)))
          }
        }
		    xynames <- names(xydat[[1]])
		    if (xvar %in% xynames) {
	        xvarnm <- xvar
	      } else if (xycoords[1] %in% xynames) {
		      xvarnm <- xycoords[1]
		    } else if (xvar == "LON" && "LON_PUBLIC" %in% xynames) {
		      xvarnm <- "LON_PUBLIC"
		    } else {
		      message(xvar, " not in data")
		      return(NULL)
		    }
	      if (yvar %in% xynames) {
	        yvarnm <- yvar
		    } else if (xycoords[2] %in% xynames) {
		      yvarnm <- xycoords[2]
		    } else if (yvar == "LAT" && "LAT_PUBLIC" %in% xynames) {
		      yvarnm <- "LAT_PUBLIC"
		    } else {
		      message(xvar, " not in data")
		      return(NULL)
		    }

		    if (!"xy" %in% names(dbqueries[[state]])) {
          dbqueries[[state]]$xy <- xydat$xyqry
	      }

	      if (savedata) {
	        xyplt <- get(xyxnm)

	        if (issp && exportsp) {
	          outlst$outsp_fmt <- ifelse(out_fmt == "csv", "shp", out_fmt)
	          outlst$out_layer <- xynm

		        if ("sf" %in% class(xyplt)) {
		          spExportSpatial(xyplt,
		                          savedata_opts = outlst)
		        } else {
              if (!is.null(pltx) && length(unique(xyplt$PLT_CN)) != nrow(pltx))
                warning("number of plots in ", xynm, " does not match plot table")

              ## Generate spatial output
		          message("saving spatial xy data...")
		          xyoutlst <- outlst
		          xyoutlst$out_layer <- paste0("sp", xynm)
              assign(xyxnm, spMakeSpatialPoints(xyplt = xyplt,
                              xvar = xvarnm,
                              yvar = yvarnm,
                              xy.uniqueid = "PLT_CN",
                              xy.crs = 4269,
                              addxy = TRUE,
                              exportsp = savedata,
                              savedata_opts = xyoutlst))
              
              if (outlst$outsp_fmt == "sqlite" && out_fmt == "sqlite") {
                outlst$outconn <- DBI::dbConnect(RSQLite::SQLite(), 
                                                 file.path(outlst$outfolder, outlst$out_dsn))
              }
		        }
	        } else {
	          
	          if (outlst$outsp_fmt == "sqlite" && out_fmt == "sqlite") {
	            outlst$outconn <- DBI::dbConnect(RSQLite::SQLite(), 
	                                             file.path(outlst$outfolder, outlst$out_dsn))
	          }
	          
	          if (!is.null(xyplt) && nrow(xyplt) > 0) {
	            message("saving xy data...")
	            index.unique.xyplt = index.xyplt <- NULL

	            if (!append_layer) {
	              index.unique.xyplt <- list("PLT_CN")
	              if (all(c("STATECD","UNITCD","COUNTYCD","PLOT","INVYR") %in% names(xyplt))) {
	                index.unique.xyplt <- append(index.unique.xyplt,
	                                             list(c("STATECD","UNITCD","COUNTYCD","PLOT","INVYR")))
	              } else if (all(c("STATECD","UNITCD","COUNTYCD","PLOT") %in% names(xyplt))) {
	                index.xyplt <- append(index.xyplt,
	                                      c("STATECD", "UNITCD","COUNTYCD","PLOT"))
	              }
	            }
	            outlst$out_layer <- xynm
	            datExportData(xyplt,
	                          index.unique = index.unique.xyplt,
	                          index = index.xyplt,
	                          savedata_opts = outlst)
	          }
	        }
	        if (!is.null(spconddatx)) {
            message("saving spatial condition data...")
            if (!append_layer) {
              index.unique.spconddat <- "PLT_CN"
            }
	          outlst$out_layer <- "spconddat"
            datExportData(spconddatx,
                          index.unique = index.unique.spconddat,
                          savedata_opts = outlst)

	        }  ## End getxy
		    }
      }
    }
    
    ## if getxy = FALSE... and out_fmt = "sqlite", open database connection
    if (savedata && out_fmt == "sqlite" && !is.null(outlst$outconn)) {
      outlst$outconn <- DBI::dbConnect(RSQLite::SQLite(), 
                                       file.path(outlst$outfolder, outlst$out_dsn))
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

 	    pcvarsb <- gsub("p\\.", "pplot\\.", pcvars)
 	    pcvarsb <- gsub("c\\.", "pcond\\.", pcvarsb)

      if (addplotgeom || addfvsid) {
        chgfromqry <- paste0(pcplusfromqry,
           " \nJOIN ", SCHEMA., plot_layer, " pplot ON (pplot.CN = p.PREV_PLT_CN)",
		       " \nJOIN ", SCHEMA., cond_layer, " pcond ON (pcond.PLT_CN = p.PREV_PLT_CN)")
      } else {
        chgfromqry <- paste0(pcfromqry,
           " \nJOIN ", SCHEMA., plot_layer, " pplot ON (pplot.CN = p.PREV_PLT_CN)",
		       " \nJOIN ", SCHEMA., cond_layer, " pcond ON (pcond.PLT_CN = p.PREV_PLT_CN)")
      }

      ## Unioned condition table
      pltcondu.qrya <- paste0("SELECT DISTINCT ", pcvars,
							"\nFROM ", chgfromqry,
							"\nWHERE ", paste0(evalFilter.grm, stateFilters))
      pltcondu.qryb <- paste0("SELECT DISTINCT ", pcvarsb,
	            "\nFROM ", chgfromqry,
							"\nWHERE ", paste0(evalFilter.grm, stateFilters))
      pltcondu.qry <- paste(pltcondu.qrya, "\nUNION\n", pltcondu.qryb)

	    if (!"pltcondu" %in% names(dbqueries[[state]])) {
        dbqueries[[state]]$pltcondu <- pltcondu.qry
	    }

      ## Run pltcondu query
      #####################################################################################
      stat <- paste("## STATUS: GETTING PLOT/COND CHANGE DATA (", stabbr, ") ...")
      cat("\n", stat, "\n")

      if (indb) {
        pltcondux <- tryCatch(
          DBI::dbGetQuery(dbconn, pltcondu.qry),
			           error=function(e)
			             return(NULL))
      } else {
        pltcondux <- tryCatch(
          sqldf::sqldf(pltcondu.qry, stringsAsFactors=FALSE, connection=NULL),
		 	          error=function(e)
		 	            return(NULL))
      }
	    if (is.null(pltcondux)) {
	      message("pltcondu query is invalid")
	      message("\n", pltcondu.qry)
	      pltcondux <- NULL
	    } else if (nrow(pltcondux) == 0) {
	      message("no change data exists for ", state)
	      pltcondux <- NULL
	    } else {
	      pltcondux <- data.table::setDT(pltcondux)


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
          condux <- pltcondux[, condvarlst2, with=FALSE]
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
          pltux[, PLOT_ID := paste0("PID",
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
        if (lowernames) {
          names(pltux) <- tolower(names(pltux))
          names(condux) <- tolower(names(condux))
        }

        if (returndata) {
	  	    if ("pltu" %in% names(tabs)) {
            tabs$pltu <- rbind(tabs$pltu, data.frame(pltux))
	        } else {
	          tabs$pltu <- data.frame(pltux)
	        }
 	        if (!"pltu" %in% names(tabIDs)) {
            tabIDs$pltu <- "CN"
	        }
		      if ("condu" %in% names(tabs)) {
		        tabs$condu <- rbind(tabs$condu, data.frame(condux))
		      } else {
		        tabs$condu <- data.frame(condux)
		      }
 	        if (!"condu" %in% names(tabIDs)) {
            tabIDs$condu <- "PLT_CN"
	        }
        }

        if (savedata) {
          message("saving pltu and condu tables...")

          index.unique.pltux = index.pltux <- NULL
          if (!append_layer) {
            index.unique.pltux <- list("CN", c("CN", "PREV_PLT_CN"))
            if (all(c("STATECD", "UNITCD", "COUNTYCD", "PLOT") %in% names(pltux))) {
              index.pltux <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT")
            }
          }
          outlst$out_layer <- "plotu"
          datExportData(pltux,
                        index.unique = index.unique.pltux,
                        index = index.pltux,
                        savedata_opts = outlst)
          rm(pltux)

          index.unique.condux <- NULL
          if (!append_layer) index.unique.condux <- c("PLT_CN", "CONDID")
          outlst$out_layer <- "condu"
          datExportData(condux,
                        index.unique = index.unique.condux,
                        savedata_opts = outlst)
          rm(condux)
          # gc()
        }
	    }

	    ##############################################################
      ## Area Change Matrix (SUBP_COND_CHNG_MTRX)
      ##############################################################
      message("\n",
        "## STATUS: Getting change data from SUBP_COND_CHNG_MTRX (", stabbr, ") ...", "\n")

      ## ssmx data
      ############################################
      if (indb) {
        sccmnm <- chkdbtab(dbtablst, sccm_layer)
        if (is.null(sccmnm)) {
          message("there is no subp_cond_chng_mtrx table in database")
          islulc=isgrm <- FALSE
        } else {
          ## Get SUBP_COND_CHNG_MTRX fields
          sccmflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = sccmnm, upper = TRUE)
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
        sccmfromqry <- paste0(chgfromqry,
               " \nJOIN ", SCHEMA., "SUBP_COND_CHNG_MTRX sccm ON (sccm.PLT_CN = c.PLT_CN",
							 "\n                                     AND sccm.PREV_PLT_CN = pcond.PLT_CN",
							 "\n                                     AND sccm.CONDID = c.CONDID",
							 "\n                                     AND sccm.PREVCOND = pcond.CONDID)")

        sccm.qry <- paste0("SELECT ", sccmvars,
		                       "\nFROM ", sccmfromqry,
                           "\nWHERE ", paste0(evalFilter.grm, stateFilters))
 	      ## Query SQLite database or R object
        if (indb) {
          sccmx <- tryCatch(
            DBI::dbGetQuery(dbconn, sccm.qry),
			              error=function(e) {
                    return(NULL) })
        } else {
          sccmx <- tryCatch(
            sqldf::sqldf(sccm.qry, stringsAsFactors=FALSE, connection=NULL),
			              error=function(e) {
                    return(NULL) })
        }
        if (is.null(sccmx) || nrow(sccmx) == 0) {
          message("SUBP_COND_CHNG_MTRX query is invalid")
		      message(sccm.qry)
        } else {
          names(sccmx) <- toupper(names(sccmx))

	  	    if (!"subp_cond_chng_mtrx" %in% names(dbqueries[[state]])) {
            dbqueries[[state]]$subp_cond_chng_mtrx <- sccm.qry
	        }

          sccmx <- data.table::setDT(sccmx)
          sccmx[, PLT_CN := as.character(PLT_CN)]
          setkey(sccmx, PLT_CN, CONDID)

          ## Subset overall filters from condx
          sccmx <- sccmx[paste(sccmx$PLT_CN, sccmx$CONDID) %in% pcondID,]

          if (lowernames) {
            names(sccmx) <- tolower(names(sccmx))
          }

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
            index.unique.sccmx = index.sccmx <- NULL
            if (!append_layer) {
              index.unique.sccmx <- c("PLT_CN","PREV_PLT_CN","SUBP","SUBPTYP","CONDID","PREVCOND")
              index.sccmx <- list("PLT_CN", "PREV_PLT_CN")
            }
            outlst$out_layer <- "subp_cond_chng_mtrx"
            datExportData(sccmx,
                          index.unique = index.unique.sccmx,
                          index = index.sccmx,
                          savedata_opts = outlst)
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

      if (indb) {
        treenm <- chkdbtab(dbtablst, tree_layer)
        if (is.null(treenm)) {
          message("there is no tree table in database")
        }
        ## Get TREE fields
        treeflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = treenm, upper = TRUE)

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
            refspp <- sqldf::sqldf(refspp.qry, connection = NULL)
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
        tfromqry <- paste0(pfromqry,
               "\nJOIN ", SCHEMA., treenm, " t ON (t.PLT_CN = p.", puniqueid, ")")

        tree.qry <- paste0("SELECT DISTINCT ", ttvars,
		                      "\nFROM ", tfromqry,
						              "\nWHERE ", xfilter)

	      ## Query SQLite database or R object
        if (indb) {
          treex <- tryCatch(
            DBI::dbGetQuery(dbconn, tree.qry),
			              error=function(e) {
                    return(NULL) })
        } else {
          treex <- tryCatch(
            sqldf::sqldf(tree.qry, stringsAsFactors=FALSE, connection = NULL),
			              error=function(e) {
                    return(NULL) })
        }
        if (is.null(treex) || nrow(treex) == 0) {
          message("TREE query is invalid")
          message(tree.qry)
        } else {
          names(treex) <- toupper(names(treex))

		      if (!"tree" %in% names(dbqueries[[state]])) {
            dbqueries[[state]]$tree <- tree.qry
	        }

          treex <- data.table::setDT(treex)
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
          if (any(nbrvars %in% names(treex))) {
             nbrvars <- nbrvars[which(nbrvars %in% names(treex))]
          }
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
            ref_spp <- tryCatch( sqldf::sqldf(sppsql, connection = NULL),
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

          if (lowernames) {
            names(treex) <- tolower(names(treex))
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
            index.treex <- index.unique.treex <- NULL
            if (!append_layer) {
              index.unique.treex <- c("PLT_CN", "CONDID", "SUBP", "TREE")
              index.treex <- "PREV_TRE_CN"
            }
            outlst$out_layer <- "tree"
            datExportData(treex,
                          index.unique = index.unique.treex,
                          index = index.treex,
                          savedata_opts = outlst)
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
				                 "TREE t ON (t.CONDID = c.CONDID AND t.PLT_CN = p.CN AND t.PREVCOND = pcond.CONDID)",
				            " \nLEFT OUTER JOIN ", SCHEMA.,
				                 "TREE ptree ON (t.PREV_TRE_CN = ptree.CN)")


            treea.qry <- paste0("SELECT DISTINCT ", ttvarsa,
		                            "\nFROM ", tchgfromqry,
						                    "\nWHERE ", xfilter)
            treeb.qry <- paste0("SELECT DISTINCT ", ttvarsb,
		                            "\nFROM ", tchgfromqry,
						                    "\nWHERE ", xfilter)
            treeu.qry <- paste(treea.qry, "UNION", treeb.qry)
            cat("\n", "## STATUS: Getting tree change data (", stabbr, ") ...", "\n")


	        ## Query SQLite database or R object
            if (indb) {
              treeux <- tryCatch(
                DBI::dbGetQuery(dbconn, treeu.qry),
			              error=function(e) {
                    return(NULL) })
            } else {
              treeux <- tryCatch(
                sqldf::sqldf(treeu.qry, stringsAsFactors=FALSE, connection = NULL),
			              error=function(e) {
                    return(NULL) })
            }
            if (is.null(treeux) || nrow(treeux) == 0) {
              message("TREEU query is invalid")
              message(treeu.qry)
            } else {
              names(treeux) <- toupper(names(treeux))

		          if (!"treeu" %in% names(dbqueries[[state]])) {
                dbqueries[[state]]$treeu <- treeu.qry
	            }

              treeux <- data.table::setDT(treeux)
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

              if (lowernames) {
                names(treeux) <- tolower(names(treeux))
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
				                                      "TREE_CN", c("PLT_CN", "PREV_TRE_CN"))
                }
                outlst$out_layer <- "treeu"
                datExportData(treeux,
                              index.unique = index.unique.treeux,
                              savedata_opts = outlst)
                rm(treex)
                # gc()
			        }
            }

            ##############################################################
            ## Tree Change, Growth, and Mortality
		        ## (TREE_GRM_COMPONENT, TREE_GRM_BEGIN, TREE_GRM_MIDPT)
            ##############################################################
            message("\n",
             "## STATUS: Getting GRM data from TREE_GRM_COMPONENT (", stabbr, ") ...", "\n")

            ## GRM data
            if (indb) {
              grmnm <- chkdbtab(dbtablst, grm_layer)
              if (!is.null(grmnm)) {
                ## Get TREE_GRM_COMPONENT fields
                grmflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = grmnm, upper = TRUE)
              }
			        grmbnm <- chkdbtab(dbtablst, grmb_layer)
              if (!is.null(grmbnm)) {
                ## Get TREE_GRM_BEGIN fields
                grmbflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = grmbnm, upper = TRUE)
              }
			        grmmnm <- chkdbtab(dbtablst, grmm_layer)
              if (!is.null(grmmnm)) {
                ## Get TREE_GRM_MIDPT fields
                grmmflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = grmmnm, upper = TRUE)
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
              grmfromqry <- paste0(pfromqry,
                    "\nJOIN ", SCHEMA., grmnm, " grm ON (grm.PLT_CN = p.", puniqueid, ")")

              grm.qry <- paste0("SELECT DISTINCT grm.* ",
                               "\nFROM ", grmfromqry,
		                           "\nWHERE ", paste0(evalFilter.grm, stateFilters))

	            ## Query SQLite database or R object
              if (indb) {
                grmx <- tryCatch(
                  DBI::dbGetQuery(dbconn, grm.qry),
			              error=function(e) {
                    return(NULL) })

              } else {
                grmx <- tryCatch(
                  sqldf::sqldf(grm.qry, stringsAsFactors=FALSE, connection = NULL),
			              error=function(e) {
                    return(NULL) })
              }
              if (is.null(grmx) || nrow(grmx) == 0) {
                message("TREE_GRM_COMPONENT query is invalid")
                message(grm.qry)
              } else {
                names(grmx) <- toupper(names(grmx))
                
	  	          if (!"tree_grm_component" %in% names(dbqueries[[state]])) {
                  dbqueries[[state]]$tree_grm_component <- grm.qry
	              }

                grmx <- data.table::setDT(grmx)
                grmx[, PLT_CN := as.character(PLT_CN)]
                grmx[, TRE_CN := as.character(TRE_CN)]
                setkey(grmx, TRE_CN)

                ## Subset overall filters from condx
                grmx <- grmx[treeux$CN,]

                if (lowernames) {
                  names(grmx) <- tolower(names(grmx))
                }

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
                  outlst$out_layer <- "tree_grm_component"
                  datExportData(grmx,
                                index.unique = index.unique.grmx,
                                savedata_opts = outlst)
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

              grmb.qry <- paste0("SELECT DISTINCT grmb.*",
                                "\nFROM ", grmbfromqry,
		                            "\nWHERE ", paste0(evalFilter.grm, stateFilters))

	          ## Query SQLite database or R object
              if (indb) {
                grmbx <- tryCatch(
                  DBI::dbGetQuery(dbconn, grmb.qry),
			              error=function(e) {
                    return(NULL) })

              } else {
                grmbx <- tryCatch(
                  sqldf::sqldf(grmb.qry, stringsAsFactors=FALSE, connection = NULL),
			              error=function(e) {
                    return(NULL) })
              }
              if (is.null(grmbx) || nrow(grmbx) == 0) {
                message("TREE_GRM_BEGIN query is invalid")
                message(grmb.qry)
              } else {
                names(grmbx) <- toupper(names(grmbx))
                
	  	          if (!"tree_grm_begin" %in% names(dbqueries[[state]])) {
                  dbqueries[[state]]$tree_grm_begin <- grmb.qry
	              }

                grmbx <- data.table::setDT(grmbx)
                grmbx[, PLT_CN := as.character(PLT_CN)]
                grmbx[, TRE_CN := as.character(TRE_CN)]
                setkey(grmbx, TRE_CN)

                ## Subset overall filters from condx
                grmbx <- grmbx[treeux$CN,]

                if (lowernames) {
                  names(grmbx) <- tolower(names(grmbx))
                }

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
                  index.unique.grmbx <- NULL
                  if (!append_layer) index.unique.grmbx <- c("TRE_CN")
                  outlst$out_layer <- "tree_grm_begin"
                  datExportData(grmbx,
                                index.unique = index.unique.grmbx,
                                savedata_opts = outlst)
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

              grmm.qry <- paste0("SELECT DISTINCT grmm.*",
                                "\nFROM ", grmmfromqry,
		                            "\nWHERE ", paste0(evalFilter.grm, stateFilters))

	            ## Query SQLite database or R object
              if (indb) {
                grmmx <- tryCatch(
                  DBI::dbGetQuery(dbconn, grmm.qry),
			              error=function(e) {
                    return(NULL) })

              } else {
                grmmx <- tryCatch(
                  sqldf::sqldf(grmm.qry, stringsAsFactors=FALSE, connection = NULL),
			              error=function(e) {
                    return(NULL) })
              }
              if (is.null(grmmx) || nrow(grmmx) == 0) {
                message("TREE_GRM_MIDPT query is invalid")
                message(grmm.qry)
              } else {
                names(grmmx) <- toupper(names(grmmx))
                
	  	          if (!"tree_grm_midpt" %in% names(dbqueries[[state]])) {
                  dbqueries[[state]]$tree_grm_midpt <- grmm.qry
	              }

                grmmx <- data.table::setDT(grmmx)
                grmmx[, PLT_CN := as.character(PLT_CN)]
                grmmx[, TRE_CN := as.character(TRE_CN)]
                setkey(grmmx, TRE_CN)

                ## Subset overall filters from condx
                grmmx <- grmmx[treeux$CN,]

                if (lowernames) {
                  names(grmmx) <- tolower(names(grmmx))
                }

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
                  index.unique.grmmx <- NULL
                  if (!append_layer) index.unique.grmmx <- c("TRE_CN")
                  outlst$out_layer <- "tree_grm_midpt"
                  datExportData(grmmx,
                                index.unique = index.unique.grmmx,
                                savedata_opts = outlst)
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
    ## Seedling data (SEEDLING)
    ##############################################################
    if (isseed && !is.null(pltx)) {
      message("\n",
      	"## STATUS: Getting seedling data from SEEDLING (", stabbr, ") ...", "\n")

      if (indb) {
        seednm <- findnm(seed_layer, dbtablst, returnNULL = TRUE)
        if (is.null(seednm)) {
          stest <- findnm("seed", dbtablst, returnNULL = TRUE)
          if (length(stest) == 1) {
            seednm <- stest
            seedflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = seednm, upper = TRUE)

          } else {
            if (length(stest) == 1) {
              seednm <- stest
              seedflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = seednm, upper = TRUE)

            } else {
              stest <- findnm("seedling", dbtablst, returnNULL = TRUE)
              if (length(stest) == 1) {
                seednm <- stest
                seedflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = seednm, upper = TRUE)
              } else {
                message("there is no seedling table in database")
                isseed <- FALSE
                seednm <- NULL
              }
            }
          }
        } else {
          seedflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = seednm, upper = TRUE)
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

        seed.qry <- paste0("SELECT DISTINCT ", ssvars,
		                      "\nFROM ", sfromqry,
						              "\nWHERE ", xfilter)

	      ## Query SQLite database or R object
        if (indb) {
          seedx <- tryCatch(
            DBI::dbGetQuery(dbconn, seed.qry),
			             error=function(e) {
                    return(NULL) })
        } else {
          seedx <- tryCatch(
            sqldf::sqldf(seed.qry, stringsAsFactors=FALSE, connection = NULL),
			              error=function(e) {
                    return(NULL) })
        }
        if (is.null(seedx) || nrow(seedx) == 0) {
          message("no seedling data for ", stabbr)
          message(seed.qry)
        } else if (length(ssvars) > 0) {
          names(seedx) <- toupper(names(seedx))
          
	        if (!"seed" %in% names(dbqueries[[state]])) {
            dbqueries[[state]]$seed <- seed.qry
	        }

          seedx <- data.table::setDT(seedx)
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
#			       evalid=evalid, qry=TRUE, outfn.date=outfn.date)
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

          if (lowernames) {
            names(seedx) <- tolower(names(seedx))
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
              index.unique.seedx <- c("PLT_CN", "CONDID", "SUBP", "SPCD")
            }
            outlst$out_layer <- "seedling"
            datExportData(seedx,
                          index.unique = index.unique.seedx,
                          savedata_opts = outlst)
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

      ## Understory vegetation data (P2VEG_SUBPLOT_SPP)
      defaultvarlst <- vsubpsppvarlst
      vsppinfo <- 
        dbgettable(vsubpspp_layer, 
                   indb = indb,
                   datsource = datsource,
                   dbtablst = dbtablst,
                   dbconn = dbconn, schema = schema,
                   stabbr = stabbr,
                   pltx = pltx,
                   pfromqry = pfromqry, puniqueid = puniqueid, 
                   defaultVars = defaultVars,
                   defaultvarlst = defaultvarlst,
                   evalFilter = evalFilter,
                   stateFilters = stateFilters,
                   lowernames = lowernames)
     
      if (!is.null(vsppinfo)) {
        p2veg_subplot_sppx <- vsppinfo$dbtabx
        vsubpspp.qry <- vsppinfo$dbtab.qry
        
        if (!"p2veg_subplot_spp" %in% names(dbqueries[[state]])) {
          dbqueries[[state]]$p2veg_subplot_spp <- vsubpspp.qry
        }
        
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
          if (!append_layer) index.vsubpsppx <- c("PLT_CN", "SUBP")
          outlst$out_layer <- "p2veg_subplot_spp"
          datExportData(p2veg_subplot_sppx,
                        index = index.vsubpsppx,
                        savedata_opts = outlst)
          rm(p2veg_subplot_sppx)
          # gc()
        }
      }

      ## Understory vegetation data (P2VEG_SUBP_STRUCTURE)
      defaultvarlst <- vsubpstrvarlst
      vstrinfo <- 
        dbgettable(vsubpstr_layer, 
                   indb = indb,
                   datsource = datsource,
                   dbtablst = dbtablst,
                   dbconn = dbconn, schema = schema,
                   pltx = pltx,
                   pfromqry = pfromqry, puniqueid = puniqueid, 
                   defaultVars = defaultVars,
                   defaultvarlst = defaultvarlst,
                   evalFilter = evalFilter,
                   stateFilters = stateFilters,
                   lowernames = lowernames)
      
      if (!is.null(vsppinfo)) {
        p2veg_subp_structurex <- vstrinfo$dbtabx
        vsubpstr.qry <- vstrinfo$dbtab.qry
        
        if (!"p2veg_subp_structure" %in% names(dbqueries[[state]])) {
          dbqueries[[state]]$p2veg_subp_structure <- vsubpstr.qry
        }
        
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
          if (!append_layer) index.vsubpstrx <- c("PLT_CN", "SUBP")
          outlst$out_layer <- "p2veg_subp_structure"
          datExportData(p2veg_subp_structurex,
                        index = index.vsubpstrx,
                        savedata_opts = outlst)
          rm(p2veg_subp_structurex)
          # gc()
        }
      }
    }
    
    
    ##############################################################
    ## Invasive species (INVASIVE_SUBPLOT_SPP)
    ##############################################################
    if (isinv && !is.null(pltx)) {
      message("\n",
      "## STATUS: Getting invasive data from INVASIVE_SUBPLOT_SPP (",
		         stabbr, ") ...", "\n")
      
      ## get invasive species
      defaultvarlst <- invsubpvarlst
      invinfo <- 
        dbgettable(invsubp_layer, 
                   indb = indb,
                   datsource = datsource,
                   dbtablst = dbtablst,
                   dbconn = dbconn, schema = schema,
                   pltx = pltx,
                   pfromqry = pfromqry, puniqueid = puniqueid, 
                   defaultVars = defaultVars,
                   defaultvarlst = defaultvarlst,
                   evalFilter = evalFilter,
                   stateFilters = stateFilters,
                   lowernames = lowernames)
      
      if (!is.null(invinfo)) {
        invasive_subplot_sppx <- invinfo$dbtabx
        invsubp.qry <- invinfo$dbtab.qry
        
        if (!"invasive_subplot_spp" %in% names(dbqueries[[state]])) {
          dbqueries[[state]]$invasive_subplot_spp <- invsubp.qry
        }
        
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
          if (!append_layer) index.invsubpx <- c("PLT_CN", "SUBP")
          outlst$out_layer <- "invasive_subplot_spp"
          datExportData(invasive_subplot_sppx,
                        index = index.invsubpx,
                        savedata_opts = outlst)
          rm(invasive_subplot_sppx)
          # gc()
        }
      }  
    }

    ##############################################################
    ## Subplot data (SUBPLOT/SUBP_COND)
    ##############################################################
    if (issubp && !is.null(pltx)) {
      message("\n",
      "## STATUS: Getting subplot data from SUBPLOT/SUBP_COND (", stabbr, ") ...", "\n")
      
      ## get subplot
      defaultvarlst <- subpvarlst
      subpinfo <- 
        dbgettable(subplot_layer, 
                   indb = indb,
                   datsource = datsource,
                   dbtablst = dbtablst,
                   dbconn = dbconn, schema = schema,
                   pltx = pltx,
                   pfromqry = pfromqry, puniqueid = puniqueid, 
                   defaultVars = defaultVars,
                   defaultvarlst = defaultvarlst,
                   evalFilter = evalFilter,
                   stateFilters = stateFilters,
                   lowernames = lowernames)
      
      if (!is.null(subpinfo)) {
        subpx <- subpinfo$dbtabx
        subp.qry <- subpinfo$dbtab.qry
        
        if (!"subplot" %in% names(dbqueries[[state]])) {
          dbqueries[[state]]$subplot <- subp.qry
        }
        
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
          if (!append_layer) index.unique.subpx <- c("PLT_CN", "SUBP")
          outlst$out_layer <- "subplot"
          datExportData(subpx,
                        index.unique = index.unique.subpx,
                        savedata_opts = outlst)
          rm(subpx)
          # gc()
        }
      }
      
      ## get supb_cond
      defaultvarlst <- subpcvarlst
      subpcinfo <- 
        dbgettable(subpcond_layer, 
                   indb = indb,
                   datsource = datsource,
                   dbtablst = dbtablst,
                   dbconn = dbconn, schema = schema,
                   pltx = pltx,
                   pfromqry = pfromqry, puniqueid = puniqueid, 
                   defaultVars = defaultVars,
                   defaultvarlst = defaultvarlst,
                   evalFilter = evalFilter,
                   stateFilters = stateFilters,
                   lowernames = lowernames)
      
      if (!is.null(subpcinfo)) {
        subpcx <- subpcinfo$dbtabx
        subpc.qry <- subpcinfo$dbtab.qry

        if (!"subp_cond" %in% names(dbqueries[[state]])) {
          dbqueries[[state]]$subp_cond <- subpc.qry
        }
        
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
          index.unique.subpcx <- NULL
          if (!append_layer) index.unique.subpcx <- c("PLT_CN", "SUBP", "CONDID")
          outlst$out_layer <- "subp_cond"
          datExportData(subpcx,
                        index.unique = index.unique.subpcx,
                        savedata_opts = outlst)
          rm(subpcx)
          # gc()
        }
      }
    }

    ##############################################################
    ## Down woody data (COND_DWM_CALC)
    ##############################################################
    if (isdwm && !is.null(pltx)) {
      message("\n",
      "## STATUS: Getting DWM data from COND_DWM_CALC (", stabbr, ") ...", "\n")
      
      ## get subplot
      defaultvarlst <- dwmvarlst
      dwminfo <- 
        dbgettable(dwm_layer, 
                   indb = indb,
                   datsource = datsource,
                   dbtablst = dbtablst,
                   dbconn = dbconn, schema = schema,
                   pltx = pltx,
                   pfromqry = pfromqry, puniqueid = puniqueid, 
                   defaultVars = defaultVars,
                   defaultvarlst = defaultvarlst,
                   evalFilter = evalFilter.dwm,
                   stateFilters = stateFilters,
                   lowernames = lowernames)
      
      if (!is.null(dwminfo)) {
        cond_dwm_calcx <- dwminfo$dbtabx
        dwm.qry <- dwminfo$dbtab.qry
        
        if (!"cond_dwm_calc" %in% names(dbqueries[[state]])) {
          dbqueries[[state]]$cond_dwm_calc <- dwm.qry
        }
        
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
          outlst$out_layer <- "cond_dwm_calc"
          datExportData(cond_dwm_calcx,
                        index.unique = index.unique.dwmx,
                        savedata_opts = outlst)
          rm(cond_dwm_calcx)
          # gc()
        }
      }
    }

    ##############################################################
    ## Other tables
    ##############################################################
    if (!is.null(othertables) && length(othertables2) > 0 && !is.null(pltx)) {
      ouniqueid <- NULL

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
        isref=isfvs <- FALSE
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
          xqry <- paste0("SELECT x.*",
                        "\nFROM ", sub("SUBX", othertable, xfromqryx),
						            "\nWHERE ", xfilterpop)
        } else if (startsWith(toupper(othertable), "REF_")) {
          xqry <- paste("SELECT * FROM", othertable)
          isref <- TRUE
        } else if (startsWith(toupper(othertable), "FVS_")) {
          xqry <- paste("SELECT * FROM", othertable)
          isfvs <- TRUE
        } else {
          xqry <- paste0("SELECT x.*",
                        "\nFROM ", sub("SUBX", othertable, xfromqry),
						            "\nWHERE ", xfilter)
        }

        if (isref) {
          message(paste0("\n",
          "## STATUS: GETTING ", othertable, "...", "\n"))
        } else {
          message(paste0("\n",
          "## STATUS: GETTING ", othertable, " (", stabbr, ")...", "\n"))
        }

        if (indb) {
          dbtabs <- DBI::dbListTables(dbconn)
          othertable <- chkdbtab(dbtabs, othertable, stopifnull=FALSE)
          if (is.null(othertable)) {
            stop(othertable, " not in database")
          }
          otab <- tryCatch( DBI::dbGetQuery(dbconn, xqry),
			            error=function(e) return(NULL))
        } else {
          otab <- tryCatch( sqldf::sqldf(xqry,
                        stringsAsFactors=FALSE, connection = NULL),
			            error=function(e) return(NULL))
        }
        if (is.null(otab)) {
          xqry <- paste0("SELECT *",
                         "\nFROM ", othertable,
		                     "\nWHERE ", stFilter)

          if (indb) {
            otab <- tryCatch(
              DBI::dbGetQuery(dbconn, xqry),
			              error=function(e) return(NULL))
          } else {
            otab <- tryCatch(
              sqldf::sqldf(xqry, stringsAsFactors=FALSE, connection = NULL),
			              error=function(e) return(NULL))
          }
        }
        if (is.null(otab)) {
          xqry <- paste0("SELECT *",
                         "\nFROM ", othertable)
          if (indb) {
            otab <- tryCatch(
              DBI::dbGetQuery(dbconn, xqry),
              error=function(e) return(NULL))
          } else {
            otab <- tryCatch(
              sqldf::sqldf(xqry, stringsAsFactors=FALSE, connection = NULL),
              error=function(e) return(NULL))
          }
        }

        if (is.null(otab)) {
          message("invalid query for ", othertable)
        } else {
          names(otab) <- toupper(names(otab))
          
          otab <- setDT(otab)
          otabnames <- names(otab)

          if (isref) {
            othertables2 <- othertables2[othertables2 != othertable]
          } else if (isfvs) {
            if (toupper(othertable) %in% c("FVS_PLOTINIT_PLOT", "FVS_STANDINIT_PLOT",
                                           "FVS_TREEINIT_PLOT", "FVS_TREEINIT_COND")) {
              stand_cn <- findnm("STAND_CN", otabnames, returnNULL = TRUE)
              otab <- otab[otab[[stand_cn]] %in% unique(pltx[puniqueid]),]
              otab[[stand_cn]] <- as.character(otab[[stand_cn]])
              setkeyv(otab, stand_cn)
              ouniqueid <- stand_cn

            } else if (toupper(othertable) %in% c("FVS_STANDINIT_COND") && !is.null(condcnnm)) {
              stand_cn <- findnm("STAND_CN", otabnames, returnNULL = TRUE)
              otab <- otab[otab[[stand_cn]] %in% unique(condx[condcnnm]),]
              otab[[stand_cn]] <- as.character(otab[[stand_cn]])
              setkeyv(otab, stand_cn)
              ouniqueid <- stand_cn

            } else {
              message("no id in FVS table to subset table... ")
            }

          } else {

            ## Subset overall filters from condx
            cn <- findnm("CN", otabnames, returnNULL = TRUE)
            plt_cn <- findnm("PLT_CN", otabnames, returnNULL = TRUE)
            condid <- findnm("CONDID", otabnames, returnNULL = TRUE)

            if (is.null(pcheck.varchar(othertable, checklst=pop_tables, stopifinvalid=FALSE))) {

              if (!is.null(plt_cn) && !is.null(condid)) {
                otab <- otab[paste(otab[[plt_cn]], otab[[condid]]) %in% pcondID,]
                otab[[plt_cn]] <- as.character(otab[[plt_cn]])
                setkeyv(otab, c(plt_cn, condid))
                ouniqueid <- c(plt_cn, condid)

              } else if (!is.null(plt_cn)) {
                otab <- otab[otab[[plt_cn]] %in% unique(pltx[puniqueid]),]
                otab[[plt_cn]] <- as.character(otab[[plt_cn]])
                setkeyv(otab, plt_cn)
                ouniqueid <- plt_cn
              }
            }
            if (nrow(otab) == 0) {
              message(othertable, " does not include PLT_CN... returning entire table")
              #otab <- NULL
            }
          }
          ## set names to lowercase
          if (lowernames) {
            names(otab) <- tolower(names(otab))
          }

          ## assign names to table
          assign(othertablexnm, data.table::setDT(otab))

          if (returndata) {
		  	    if (tolower(othertable) %in% names(tabs)) {
              tabs[[tolower(othertable)]] <-
						       rbind(tabs[[tolower(othertable)]], otab)
	          } else {
	            tabs[[tolower(othertable)]] <- otab
	          }
 	          if (!tolower(othertable) %in% names(tabIDs) && "PLT_CN" %in% names(otab)) {
              tabIDs[[tolower(othertable)]] <- "PLT_CN"
	          }
          }
          if (savedata) {
            message("saving ", tolower(othertable), " table...")
            index.unique.other <- NULL
            if (!is.null(ouniqueid)) {
              index.unique.other <- ouniqueid
            }
            outlst$out_layer <- tolower(othertable)
            datExportData(otab,
                          index.unique = index.unique.other,
                          savedata_opts = outlst)
          }
        }
      }  ## end j loop
    }

    ##############################################################
    ## If savePOP or more than one evalType
    ##############################################################
    if (savePOP && !is.null(pltx)) {
      message(paste("\n",
      "## STATUS: GETTING POP_PLOT_STRATUM_ASSGN DATA (", stabbr, ")...", "\n"))

      ppsavars <- c("PLT_CN", "STRATUM_CN", "EVALID",
                             "STATECD", "ESTN_UNIT", "STRATUMCD")
      ppsavars <- ppsavars[ppsavars %in% ppsaflds]
      ppsaqry <- paste0("SELECT * ",
	                     "\nFROM ", ppsafromqry,
					             "\nWHERE statecd = ", stcd)
      if (iseval) {
        if (subsetPOP) {
          ppsaqry <- paste(ppsaqry, "AND evalid IN (", toString(evalid), ")")
        } else {
          evalstyr <- substr(evalid, 1, nchar(evalid)-2)
          ppsaqry <- paste(ppsaqry, "AND evalid LIKE", paste0("'", evalstyr, "%'"))
        }
      }
      if (indb) {
        ppsax <- tryCatch(
          DBI::dbGetQuery(dbconn, ppsaqry),
                             error=function(e) return(NULL))
      } else {
        ppsax <- tryCatch(
          sqldf::sqldf(ppsaqry, stringsAsFactors=FALSE, connection = NULL),
                             error=function(e) return(NULL))
      }
      if (is.null(ppsax) || nrow(ppsax) == 0) {
        message("invalid query for POP_PLOT_STRATUM_ASSGN:")
        message(ppsaqry)
        stop()
      } else {
        names(ppsax) <- toupper(names(ppsax))
        
        ppsax <- data.table::setDT(ppsax)
        ppsax[, PLT_CN := as.character(PLT_CN)]
        setkey(ppsax, PLT_CN)

        ## Subset overall filters from pltx
        ppsax <- ppsax[ppsax$PLT_CN %in% unique(pltx$CN),]
      }

      if (lowernames) {
        names(ppsax) <- tolower(names(ppsax))
      }

      if (returndata) {
        ppsa <- rbind(ppsa, ppsax)
      }

      if (savePOPall) {
        message(paste("\n",
        "## STATUS: GETTING POP_STRATUM DATA (", stabbr, ")...", "\n"))
        pstratumqry <- paste0("SELECT *",
                        "\nFROM ", pstratumfromqry,
                        "\nWHERE statecd =", stcd)
        message(paste("\n",
        "## STATUS: GETTING POP_ESTN_UNIT DATA (", stabbr, ")...", "\n"))
        pestnunitqry <- paste0("SELECT *",
                            "\nFROM ", pestnunitfromqry,
                            "\nWHERE statecd =", stcd)
        #ppsanm <- "ppsa"

        if (iseval) {
          if (subsetPOP) {
            pstratumqry <- paste(pstratumqry, "AND evalid IN (", toString(evalid), ")")
            pestnunitqry <- paste(pestnunitqry, "AND evalid IN (", toString(evalid), ")")
          } else {
            evalstyr <- substr(evalid, 1, nchar(evalid)-2)
            pstratumqry <- paste(pstratumqry, "AND evalid LIKE", paste0("'", evalstyr, "%'"))
            pestnunitqry <- paste(pestnunitqry, "AND evalid LIKE", paste0("'", evalstyr, "%'"))
          }
        }

        if (indb) {
          popstratumx <- tryCatch( DBI::dbGetQuery(dbconn, pstratumqry),
                             error=function(e) return(NULL))
          popestnunitx <- tryCatch( DBI::dbGetQuery(dbconn, pestnunitqry),
                             error=function(e) return(NULL))
        } else {
          popstratumx <- tryCatch( sqldf::sqldf(pstratumqry,
                     stringsAsFactors=FALSE, connection = NULL),
                             error=function(e) return(NULL))
          popestnunitx <- tryCatch( sqldf::sqldf(pestnunitqry,
                     stringsAsFactors=FALSE, connection = NULL),
                             error=function(e) return(NULL))
        }
        names(popstratumx) <- toupper(names(popstratumx))
        names(popestnunitx) <- toupper(names(popestnunitx))
        
        popstratumx <- popstratumx[order(popstratumx$STATECD, popstratumx$ESTN_UNIT, popstratumx$STRATUMCD),]
        popestnunitx <- popestnunitx[order(popestnunitx$STATECD, popestnunitx$ESTN_UNIT),]
        
        if (lowernames) {
          names(popstratumx) <- tolower(names(popstratumx))
          names(popestnunitx) <- tolower(names(popestnunitx))
        }
        
        if (returndata) {
          popstratum <- rbind(popstratum, popstratumx)
          popestnunit <- rbind(popestnunit, popestnunitx)
        }

        ## add EXPNS to pltx from pop_stratum table
        if (addexpns) {
          expnsqry <- paste0("SELECT pltx.*, EXPNS
                          FROM pltx
                          JOIN ppsax ON (ppsax.PLT_CN = pltx.CN)
                          JOIN popstratumx ON (popstratumx.CN = ppsax.STRATUM_CN)")
          pltexpns <- tryCatch(sqldf::sqldf(expnsqry),
                               error = function(e) {
                                 message(e,"\n")
                                 return(NULL) })
          if (is.null(pltexpns) || nrow(pltexpns) == 0) {
            message("EXPNS query is invalid")
            message(expnsqry)
          } else {
            pltx <- pltexpns
          }
        }
      }
    }
    if (lowernames) {
      names(pltx) <- tolower(names(pltx))
      names(condx) <- tolower(names(condx))
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
        outlst$out_layer <- "plot"
        datExportData(pltx,
                      index.unique = index.unique.pltx,
                      index = index.pltx,
                      savedata_opts = outlst)
        #rm(pltx)
        # gc()
      }
      if (savedata && !is.null(condx)) {
        message("saving cond table...")

        index.unique.condx <- NULL
        if (!append_layer) {
          index.unique.condx <- c("PLT_CN", "CONDID")
        }

        outlst$out_layer <- "cond"
        datExportData(condx,
                      index.unique = index.unique.condx,
                      savedata_opts = outlst)
        #rm(condx)
        # gc()
      }

      if (savedata && savePOP && !is.null(ppsax)) {
        message("saving pop_plot_stratum_assgn table...")

        index.unique.ppsax=index.ppsax <- NULL
        if (!append_layer) {
          index.unique.ppsax <- "PLT_CN"
          if (all(c("STATECD","UNITCD","COUNTYCD","PLOT") %in% names(ppsax))) {
            index.ppsax <- c("STATECD","UNITCD", "COUNTYCD","PLOT")
          }
          if (all(c("EVALID", "ESTN_UNIT", "STRATUMCD") %in% names(ppsax))) {
            if (!is.null(index.ppsax)) {
              index.ppsax <- list(index.ppsax, c("EVALID", "ESTN_UNIT", "STRATUMCD"))
            } else {
              index.ppsax <- c("EVALID", "ESTN_UNIT", "STRATUMCD")
            }
          }
        }
        outlst$out_layer <- "pop_plot_stratum_assgn"
        datExportData(ppsax,
                      index.unique = index.unique.ppsax,
                      index = index.ppsax,
                      savedata_opts = outlst)
        rm(ppsax)
      }
      if (savePOPall && !"pop_stratum" %in% othertables) {
        message("saving pop_stratum table...")

        index.unique.popstratumx <- NULL
        if (!append_layer) {
          index.unique.popstratumx <- c("RSCD","EVALID","ESTN_UNIT","STRATUMCD")
        }
        outlst$out_layer <- "pop_stratum"
        datExportData(popstratumx,
                      index.unique = index.unique.popstratumx,
                      savedata_opts = outlst)
        rm(popstratumx)
      }
      if (savePOPall && !"pop_estn_unit" %in% othertables) {
        message("saving pop_estn_unit table...")

        index.unique.popestnunitx <- NULL
        if (!append_layer) {
          index.unique.popestnunitx <- c("RSCD","EVALID","ESTN_UNIT")
        }
        outlst$out_layer <- "pop_estn_unit"
        datExportData(popestnunitx,
                      index.unique = index.unique.popestnunitx,
                      savedata_opts = outlst)
          rm(popestnunitx)
      }
      # gc()
    } ## end - savedata
    rm(pltcondx)
    gc()

    if (datsource == "datamart" && datamartType == "SQLITE") {
      DBI::dbDisconnect(dbconn)
    }
  } ## end loop for states
  close(pb)

  if (savedata) {
    message("saving ref_species...")

    outlst$out_layer <- "ref_species"
    datExportData(ref_species,
                  #index.unique = list("SPCD", "SPECIES_SYMBOL"),
                  savedata_opts = outlst)
  }

  if (savedata && saveSURVEY) {
    message("saving survey table...")

    outlst$out_layer <- "survey"
    datExportData(SURVEY,
                  index.unique = "CN",
                  savedata_opts = outlst)
  }

  ## Write out plot/condition counts to comma-delimited file.
  if (savedata && !is.null(pltcnt)) {
    message("saving pltcnt table...")

    outlst$out_layer <- "pltcnt"
    datExportData(pltcnt,
                  savedata_opts = outlst)
  }

  ## GENERATE RETURN LIST
  if (returndata) {
    returnlst <- list(states=states)
    returnlst$tabs <- tabs
    returnlst$tabIDs <- tabIDs
    returnlst$dbqueries <- dbqueries
    returnlst$puniqueid <- puniqueid

    if (getxy) {
#      if (xymeasCur) {
#        xynm <- paste0("xyCur_", coordType)
#      } else {
#        xynm <- paste0("xy_", coordType)
#      }
      if (issp) {
	      addxy <- TRUE
	      if (all(c(xvar, yvar) %in% names(get(xynm)))) {
		      addxy <- FALSE
		    }
        assign(xynm,
		      spMakeSpatialPoints(xyplt = get(xynm),
		              xvar = xvarnm, yvar = yvarnm,
		              xy.uniqueid = "PLT_CN", xy.crs = 4269, addxy = addxy))
        returnlst[[xynm]] <- get(xynm)
      } else {
        returnlst[[xynm]] <- get(xynm)
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
    if (savePOPall) {
      returnlst$pop_stratum <- popstratum
      returnlst$pop_estn_unit <- popestnunit
    }
    if (saveSURVEY && !is.null(SURVEY)) {
      returnlst$SURVEY <- data.frame(SURVEY)
    }

    if (length(evalidlist) > 0) {
      returnlst$evalid <- evalInfo$evalidlist
      returnlst$evalEndyr <- evalInfo$evalEndyrlist
    }
    returnlst$pltcnt <- pltcnt
    returnlst$invyrs <- invyrs

    if (!is.null(evalInfo)) {
      returnlst$evalInfo <- evalInfo
    }

    ## Disconnect database
    if (!is.null(dbconn)) {
      if (!is.null(dbconn) && !dbconnopen && DBI::dbIsValid(dbconn)) {
        DBI::dbDisconnect(dbconn)
      } else {
        returnlst$dbconn <- dbconn
      }
    }
  }

  if (returndata && !is.null(evalidlist)) {
    #evaliddf <- data.frame(do.call(rbind, evalidlist))
    #stcds <- pcheck.states(row.names(evaliddf), "VALUE")
    #evaliddf <- data.frame(stcds, row.names(evaliddf), evaliddf, row.names=NULL)
    #names(evaliddf) <- c("STATECD", "STATE", "EVALID")
    #evaliddf <- evaliddf[order(evaliddf$STATECD), ]
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
  } else {
    returnlst <- list(dbqueries = dbqueries, pltcnt = pltcnt)
    return(returnlst)
  }
}

