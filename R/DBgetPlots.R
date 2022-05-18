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
#' conditions that have been visited in the field (NF_SAMPLING_STATUS_CD = 1),
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
#' *FIADB Table Extraction*\cr \tabular{lll}{ 
#' \tab \bold{Argument} \tab \bold{Table Name(s)}\cr 
#' \tab istree \tab TREE\cr 
#' \tab isveg \tab P2VEG_SUBPLOT_SPP, P2VEG_SUBP_STRUCTURE, INVASIVE_SUBPLOT_SPP\cr 
#' \tab issubp \tab SUBPLOT, SUBP_COND\cr 
#' \tab isdwm \tab COND_DWM_CALC\cr 
#' \tab isgrm \tab TREE_GRM_COMPONENT\cr 
#' \tab issccm \tab SUBP_COND_CHNG_MTRX\cr }
#' 
#' \bold{FIA Evaluations}
#' 
#' An evaluation is a group of plots within the FIA database that is used for
#' generating population estimates, representing different inventory spans of
#' data with different stratification or area adjustments. Each evaluation is
#' determined by the type of estimation (evalType) including: area and tree
#' estimates; growth, removal, and mortality estimates; and area change
#' estimates (EVAL_TYPE). These plots are identified by an evalid, which is a
#' unique identifier in the format of a 2-digit State code, a 2-digit year
#' code, and a 2-digit evaluation type code. For example, EVALID '491601'
#' represents the Utah 2016 evaluation for current area estimates.
#' 
#' \bold{FIA Evaluation Types}
#' 
#' Define one or more Evaluation Type for evalCur=TRUE or evalEndyr=YYYY. An
#' Evaluation type is used to identify a specific set of plots for a particular
#' response that can be used to a make a statistically valid sample-based
#' estimate. If evalType="ALL", the evaluation includes all sampled and
#' nonsampled plots or plots that were missed in an inventory year.
#' 
#' Regional differences may occur on how missed plots are represented in a FIA
#' Evaluation.  For example, RMRS Evaluations are static; missed plots are
#' included in an Evaluation as nonsampled, and when measured, are included in
#' a following Evaluation.  Therefore, the number of nonsampled plots in
#' previous Evaluations may change, depending on when missed plot are measured.
#' In the PNW Research Station, plots are brought forward to replace missed
#' plots in an evaluation, depending on the evalType.
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
#' *FIA Evaluation*\cr 
#' \tabular{lll}{ \tab \bold{Argument} \tab \bold{Description}\cr 
#' \tab EVALID \tab Specified FIA EVALID (e.g., 491801)\cr 
#' \tab evalCur \tab Most current FIA Evaluation\cr 
#' \tab evalEndyr \tab End year of an FIA Evaluation (e.g., 2018)\cr 
#' \tab evalAll \tab All evaluations in database\cr 
#' \tab evalType \tab Type of FIA Evaluation (response)\cr }
#' 
#' *Custom evaluation*\cr 
#' \tabular{lll}{ \tab \bold{Argument} \tab \bold{Description}\cr 
#' \tab measCur \tab Most current measurement of plot in database\cr 
#' \tab measEndyr \tab Most current measurement of plot in database in or 
#' before year\cr 
#' \tab allyrs \tab All years for invtype (ANNUAL/PERIODIC)\cr 
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
#' @param datsource String. Source of data ('datamart', 'sqlite').
#' @param data_dsn String. If datsource='sqlite', the name of SQLite database
#' (*.sqlite).
#' @param RS String vector. Name of research station(s) to get public XY
#' coordinates for ('RMRS','SRS','NCRS','NERS','PNWRS'). Do not use if states 
#' is populated. See FIESTA::ref_statecd for reference to RS and states.
#' @param invtype String. Type of FIA inventory to extract ('PERIODIC',
#' 'ANNUAL').  Only one inventory type (PERIODIC/ANNUAL) at a time.
#' @param evalid Integer. Inventory span defining variable. Extract data for a
#' specific FIA Evaluation (See details for more information about FIA
#' Evaluations).
#' @param evalCur Logical. Inventory span defining variable. If TRUE, extract
#' data for the most current FIA Evaluation for each state.
#' @param evalEndyr YYYY. Inventory span defining variable. Extract data for
#' the FIA Evaluation(s) ending in the specified evalEndyr(s). If more than one
#' state and different Evaluations by state are desired, input a named list
#' @param evalAll Logical. Inventory span defining variable. If TRUE, extract
#' data for all FIA Evaluations for each state.
#' @param evalType String vector. The type(s) of FIA Evaluation of interest 
#' ('ALL', 'CURR', 'VOL', 'GRM', 'P2VEG', 'DWM", 'INV', 'REGEN', 'CRWN'). 
#' The evalType 'ALL' includes nonsampled plots; 'CURR' includes plots used 
#' for area estimates; 'VOL' includes plots used for area and/or tree estimates;
#' 'GRM' includes plots used for growth, removals, mortality, and
#' change estimates (evalType %in% c(GROW, MORT, REMV, CHNG)).  Multiple types
#' are accepted. See details below and FIA database manual for regional
#' availability and/or differences. Note: do not use if EVALID is specified.
#' @param measCur Logical. Inventory span defining variable. If TRUE, extract
#' plots with most current sampled measurement for state(s).
#' @param measEndyr Logical. Inventory span defining variable. If TRUE, extract
#' plots with most current sampled measurement for state(s) for years measured 
#' in or before measEndyr.
#' @param allyrs Logical. Inventory span defining variable. If TRUE, extract
#' all annual inventory years in database for each state.
#' @param invyrs YYYY vector. Inventory span defining variable. Extract data by
#' state for the specified inventory year(s) (e.g., c(2000, 2001, 2002)). If
#' more than one state and different inventory years are desired, input a named
#' list object with years labeled by state (e.g., list(Utah=2000:2009,
#' Colorado=c(2002,2003,2005)).
#' @param measyrs YYYY vector. Measurement year span defining variable. Extract
#' data by state for the specified measurement year(s) (e.g., c(2000, 2001,
#' 2002)). If more than one state and different measurement years are desired,
#' input a named list object with years labeled by state (e.g.,
#' list(Utah=2000:2009, Colorado=c(2002,2003,2005)).
#' @param getxy Logical. If TRUE, gets separate XY table.
#' @param xymeasCur Logical. If TRUE, include XY coordinates from the most 
#' current sampled measurement of each plot.
#' @param istree Logical. If TRUE, tree data are extracted from TREE table in
#' database.
#' @param isseed Logical. If TRUE, seedling data are extracted from SEEDLING
#' table in database.
#' @param isveg Logical. If TRUE, understory vegetation tables are extracted
#' from FIA database (P2VEG_SUBPLOT_SPP, P2VEG_SUBP_STRUCTURE, INVASIVE_SUBPLOT_SPP).
#' @param issubp Logical. If TRUE, subplot tables are extracted from FIA
#' database (SUBPLOT, SUBP_COND).
#' @param islulc Logical. If TRUE, condition-level land use/land cover data are
#' for current and previous years are extracted from FIA database.
#' @param isdwm Logical. If TRUE, summarized condition-level down woody debris
#' data are extracted from FIA database (COND_DWM_CALC).
#' @param plotgeom Logical. If TRUE, variables from the PLOTGEOM table are
#' appended to the plot table.
#' @param othertables String Vector. Name of other table(s) in FIADB to include
#' in output. The table must have PLT_CN as unique identifier of a plot.
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
#' @param subcycle99 Logical. If TRUE, excludes plots with SUBCYCLE = 99. These
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
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. If out_layer = NULL,
#' @param savePOP Logical. If TRUE, save and return the POP_PLOT_STRATUM_ASSGN
#' table.
#' 
#' @return if returndata=TRUE, a list of the following objects: 
#' \item{states}{ Vector. Input state(s) (full state names: Arizona). } 
#' \item{tabs}{ List. A list of data frames from FIA database, including 
#' plt and cond; and tree (if istree=TRUE); seed (if isseed=TRUE), vsubpspp,
#' vsubpstr, and invsubp (if isveg=TRUE), lulc (if islulc=TRUE). See below 
#' 'Output Tables - FIA Table Names' for reference to FIA database tables. 
#' See FIESTA:ref_* for variable descriptions (e.g., FIESTA::ref_tree). 
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
#' table or, if more than one evalType and savePOP=FALSE. If more than one
#' evalType, only the records for the evalTypes are returned, otherwise all
#' evalTypes for the state evaluation are returned. }
#' 
#' *Output Tables - FIA Table Names*\cr \tabular{lll}{ 
#' \tab \bold{tab} \tab \bold{FIA Table}\cr 
#' \tab plt \tab plot\cr
#' \tab cond \tab cond\cr
#' \tab tree \tab tree\cr 
#' \tab vsubpspp \tab P2VEG_SUBPLOT_SPP\cr 
#' \tab vsubpstr \tab P2VEG_SUBP_STRUCTURE\cr 
#' \tab invsubp \tab INVASIVE_SUBPLOT_SPP\cr 
#' \tab subplot \tab SUBPLOT\cr 
#' \tab subp_cond \tab SUBP_COND\cr 
#' \tab dwm \tab COND_DWM_CALC\cr 
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
#'                     evalCur = TRUE)
#' names(UTdat)
#' head(UTdat$plt)
#' UTdat$pltcnt
#' 
#' # Look at number of plots by inventory year
#' table(UTdat$plt$INVYR)
#' 
#' # Note: see FIESTA::ref_plt and FIESTA::ref_cond for variable descriptions
#' # Or consult FIA Database documentation
#' # \link{https://www.fia.fs.fed.us/library/database-documentation/index.php}
#' 
#' # Extract specified inventory years 2012:2014 and spatial information
#' UTdat2 <- DBgetPlots(states = "Utah",
#'                      invyrs = 2012:2014, 
#'                      issp = TRUE)
#' names(UTdat2)
#' UTdat2$pltcnt
#' UTdat2$spxy_PUBLIC
#'
#' # Extract and display plots with aspen forest type
#' UTdat3 <- DBgetPlots(states = "Utah",
#'                      invyrs = 2012:2014,
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
                        datsource = "datamart", 
                        data_dsn = NULL, 
                        RS = NULL, 
                        invtype = "ANNUAL", 
                        evalid = NULL, 
                        evalCur = FALSE, 
                        evalEndyr = NULL, 
                        evalAll = FALSE, 
                        evalType = "VOL", 
                        measCur = FALSE, 
                        measEndyr = NULL, 
                        allyrs = FALSE, 
                        invyrs = NULL, 
                        measyrs = NULL, 
                        getxy = TRUE,
                        xymeasCur = FALSE, 
                        istree = FALSE, 
                        isseed = FALSE, 
                        isveg = FALSE, 
                        issubp = FALSE, 
                        islulc = FALSE, 
                        isdwm = FALSE, 
                        plotgeom = FALSE, 
                        othertables = NULL, 
                        issp = FALSE, 
                        spcond = FALSE, 
                        spcondid1 = FALSE, 
                        defaultVars = TRUE, 
                        regionVars = FALSE, 
                        regionVarsRS = "RMRS", 
                        ACI = FALSE, 
                        subcycle99 = FALSE, 
                        intensity1 = FALSE, 
                        stateFilter = NULL, 
                        allFilter = NULL, 
                        alltFilter = NULL,
                        returndata = TRUE,
                        savedata = FALSE, 
                        saveqry = FALSE, 
                        savePOP = FALSE,
                        savedata_opts = NULL) {

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

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
    invtype=evalCur=evalAll=evalType=measCur=allyrs=istree=isseed=issubp=
	isveg=isdwm=isgrm=issccm=issp=spcondid1=defaultVars=regionVars=ACI=
	subcycle99=intensity1=allFilter=savedata=saveqry=parameters=out_fmt=
	overwrite=BIOJENK_kg=BIOJENK_lb=PREV_PLTCN=savePOP=xymeasCur <- NULL
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
	PREV_PLT_CN <- NULL



  ## Define functions
  ###########################################################
  getcoords <- function(coords){
    switch(coords,
      ACTUAL = c("LON_ACTUAL", "LAT_ACTUAL"),
      PUBLIC = c("LON_PUBLIC", "LAT_PUBLIC"))
  }  
  
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  input.params <- names(as.list(match.call()))[-1]
  if (!all(input.params %in% names(formals(DBgetPlots)))) {
    miss <- input.params[!input.params %in% formals(DBgetPlots)]
    stop("invalid parameter: ", toString(miss))
  }
  
  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts)
  
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
  coords <- "PUBLIC"
  parameters <- FALSE
  biojenk <- FALSE 
  greenwt <- TRUE
  isgrm <- FALSE
  issccm=FALSE

  ## Define layers in SQLite
  plot_layer <- "PLOT"
  cond_layer <- "COND"
  tree_layer <- "TREE" 
  ppsa_layer <- "POP_PLOT_STRATUM_ASSGN"


  ########################################################################
  ### GET PARAMETER INPUTS
  ########################################################################
  iseval <- FALSE
  subsetPOP <- FALSE

  ## Check invtype
  invtypelst <- c('ANNUAL', 'PERIODIC')
  invtype <- pcheck.varchar(invtype, varnm="invtype", checklst=invtypelst, 
		caption="Inventory Type", gui=gui)

  #############################################################################
  ## Set datsource
  ########################################################
  datsourcelst <- c("datamart", "sqlite")
  datsource <- pcheck.varchar(var2check=datsource, varnm="datsource", 
		checklst=datsourcelst, gui=gui, caption="Data source?") 
  if (datsource %in% c("sqlite", "gdb")) {
    data_dsn <- DBtestSQLite(data_dsn)
  }
  if (!is.null(data_dsn)) {
    if (getext(data_dsn) %in% c("sqlite", "db", "db3")) {
      dbconn <- DBtestSQLite(data_dsn, dbconnopen=TRUE, showlist=FALSE)
      dbtablst <- DBI::dbListTables(dbconn)
    } else {
      stop("only sqlite databases available currently")
    }     
  }

  ## GETS DATA TABLES (OTHER THAN PLOT/CONDITION) IF NULL
  ###########################################################
  if (gui) {
    datatablst <- c("tree", "seed", "veg", "subp", "dwm")
    datatabs <- select.list(c("NONE", datatablst), title="Other tables??", 
		preselect="NONE", multiple=TRUE)
    if (length(datatabs)==0) datatabs <- "NONE"
    istree <- ifelse(any(datatabs == "tree"), TRUE, FALSE)
    isseed <- ifelse(any(datatabs == "seed"), TRUE, FALSE)
    isveg <- ifelse(any(datatabs == "veg"), TRUE, FALSE)
    if (isveg) {
      issubp <- TRUE
    } else {
      issubp <- ifelse(any(datatabs == "subp"), TRUE, FALSE)
    }
    isdwm <- ifelse(any(datatabs == "dwm"), TRUE, FALSE)
    isgrm <- ifelse(any(datatabs == "grm"), TRUE, FALSE)
    issccm <- ifelse(any(datatabs == "sccm"), TRUE, FALSE)
  } else {
    istree <- pcheck.logical(istree, varnm="istree", 
		title="Tree variables?", first="YES", gui=gui)
    isseed <- pcheck.logical(isseed, varnm="isseed", 
		title="Seedling variables?", first="YES", gui=gui)
    isveg <- pcheck.logical(isveg, varnm="isveg", 
		title="Understory veg variables?", first="YES", gui=gui)
    if (isveg && invtype == "PERIODIC") {
      message("understory vegetation data only available for annual data\n")
      isveg <- FALSE
    }
    #if (all(!rslst %in% c("RMRS", "PNWRS"))) isveg <- FALSE
    if (isveg) {
      issubp <- TRUE
    } else {
      issubp <- pcheck.logical(issubp, varnm="issubp", 
		title="Subplot tables?", first="YES", gui=gui)
    }
    isdwm <- pcheck.logical(isdwm, varnm="isdwm", 
		title="DWM variables?", first="YES", gui=gui)
    isgrm <- pcheck.logical(isgrm, varnm="isgrm", 
		title="GRM variables?", first="YES", gui=gui)
    issccm <- pcheck.logical(issccm, varnm="issccm", 
		title="Subplot Change variables?", first="YES", gui=gui)
  }


  ########################################################################
  ### DBgetEvalid()
  ########################################################################

  ## Data warnings
  ## Note: Periodic data in database includes forested plots >= 5% cover 
  ## Note: Annual data in database includes forested plots >=10% cover

  if (isdwm) {
    evalType <- c(evalType, "DWM")
  }
#  if (isveg) {
#    evalType <- c(evalType, "P2VEG")
#  }
  if (isgrm || issccm) {
    evalType <- c(evalType, "CHNG")
  }

  ## Get states, Evalid and/or invyrs info
  evalInfo <- DBgetEvalid(states=states, RS=RS, 
                          datsource=datsource, 
                          data_dsn=data_dsn, 
                          invtype=invtype, 
                          evalid=evalid, 
                          evalCur=evalCur, 
                          evalEndyr=evalEndyr, 
                          evalAll=evalAll, 
                          evalType=evalType, 
                          gui=gui)
  if (is.null(evalInfo)) return(NULL)
  states <- evalInfo$states
  rslst <- evalInfo$rslst
  evalidlist <- evalInfo$evalidlist
  invtype <- evalInfo$invtype
  invyrtab <- evalInfo$invyrtab
  SURVEY <- evalInfo$SURVEY
  if (length(evalidlist) > 0) {
    invyrs <- evalInfo$invyrs
    iseval <- TRUE
    if (!savePOP && (any(lapply(evalInfo$evalTypelist, length) > 1) || 
		any(lapply(evalInfo$evalidlist, length) > 1))) {
      savePOP <- TRUE
    }
  }

  ### GET RS & rscd
  ###########################################################
  isRMRS <- ifelse(length(rslst) == 1 && rslst == "RMRS", TRUE, FALSE) 
     
  ## Get state abbreviations and codes 
  ###########################################################
  stabbrlst <- pcheck.states(states, statereturn="ABBR")
  stcdlst <- pcheck.states(states, statereturn="VALUE")

  ## Get number of states 
  nbrstates <- length(states)  ##  Check whether to return tree data

  ## If using EVALID, you don't need to get INVYRS, intensity, or subcycle
  if (!iseval) {  
    ### Check measCur
    ###########################################################
    measCur <- pcheck.logical(measCur, varnm="measCur", 
		title="Current measyear?", first="YES", gui=gui)

    ### Check measEndyr
    ###########################################################
    measEndyr.filter <- NULL
    if (!is.null(measEndyr)) {
      if (!is.null(invyrtab)) {
        minyr <- min(invyrtab$INVYR)
        if (!is.numeric(measEndyr) || measEndyr < minyr)
          stop("measEndyr must be yyyy format and greater than minimum inventory year: ", 
			minyr)
        measCur <- TRUE
        measEndyr.filter <- paste0(" and MEASYEAR < ", measEndyr)
      }
    }
    if (measCur) {
      xymeasCur <- TRUE
      allyrs <- FALSE
    }

    ## Check allyrs
    ###########################################################
    allyrs <- pcheck.logical(allyrs, varnm="allyrs", title="All years?", 
		first="YES", gui=gui)
    if (allyrs) {
      ## xymeasCur
      xymeasCur <- pcheck.logical(xymeasCur, varnm="xymeasCur", 
		      title="Most current XY?", first="YES", gui=gui)
      measCur <- FALSE
      measEndyr=measEndyr.filter <- NULL
    }

    ## Check INVYR(S) of MEASYR(S)
    ###########################################################
    if (!measCur) {
      if ((is.null(invyrs) || length(invyrs) == 0) && 
		(is.null(measyrs) || length(measyrs) == 0)) {
        if (is.null(invyrtab)) {
          stop("must include INVYR in plot")
        } 
        invyrs <- sapply(states, function(x) NULL)
        for (state in states) { 
          stabbr <- pcheck.states(state, "ABBR")
          stinvyrlst <- sort(invyrtab[invyrtab$STATENM == state, "INVYR"])

          if (allyrs) {
            invyr <- stinvyrlst
          } else {
            if (!gui) stop("need to specify a timeframe for plot data")

            ## GET INVENTORY YEAR(S) FROM USER
            invyr <- select.list(as.character(stinvyrlst), 
                                 title=paste("Inventory year(s) -", stabbr), 
                                 multiple=TRUE)
            if (length(invyr) == 0) stop("")
          }
          invyrs[[state]] <- as.numeric(invyr)
        }
      } else if (!is.null(invyrs)) {
        if (!is(invyrs, "list")) {
          if (is.vector(invyrs) && is.numeric(invyrs)) {
            invyrs <- list(invyrs)
            if (length(states) == 1) {
              names(invyrs) <- states
            } else {
              message("using specified invyrs for all states")
              yrs <- invyrs
              invyrs <- sapply(states, function(x) NULL)
              for (st in states) invyrs[st] <- yrs
            } 
          }
        } else if (length(invyrs) != length(states)) {
          stop("check invyrs list.. does not match number of states")
        }
        ## Check inventory years
        for (state in states) {
          stcd <- pcheck.states(state, "VALUE")
          if ("STATENM" %in% names(invyrtab)) {
            stinvyrlst <- sort(invyrtab[invyrtab$STATENM == state, "INVYR"])
          } else if ("STATECD" %in% names(invyrtab)) {
            stinvyrlst <- sort(invyrtab[invyrtab$STATECD == stcd, "INVYR"])
          } else {
            stop("invyrtab is invalid")
          }
          if (!all(invyrs[[state]] %in% stinvyrlst)) {
            invyrs[[state]] <- invyrs[[state]][invyrs[[state]] %in% stinvyrlst]
            missyr <- invyrs[[state]][!invyrs[[state]] %in% stinvyrlst]
            message(state, " missing following inventory years: ", toString(missyr))
          }
        }
      } else if (!is.null(measyrs)) {
        if (!is(measyrs, "list")) {
          if (is.vector(measyrs) && is.numeric(measyrs)) {
            measyrs <- list(measyrs)
            if (length(states) == 1) {
              names(measyrs) <- states
            } else {
              message("using specified measurement year for all states")
              yrs <- measyrs
              measyrs <- sapply(states, function(x) NULL)
              for (st in states) measyrs[st] <- yrs
            } 
          }
        } else if (length(measyrs) != length(states)) {
          stop("check measyrs list.. does not match number of states")
        }
        ## Check inventory years
        for (state in states) {
          stcd <- pcheck.states(state, "VALUE")
          if ("STATENM" %in% names(invyrtab)) {
            stinvyrlst <- sort(invyrtab[invyrtab$STATENM == state, "INVYR"])
          } else if ("STATECD" %in% names(invyrtab)) {
            stinvyrlst <- sort(invyrtab[invyrtab$STATECD == stcd, "INVYR"])
          } else {
            stop("invyrtab is invalid")
          }
          if (!all(measyrs[[state]] %in% stinvyrlst)) {
            measyrs[[state]] <- measyrs[[state]][measyrs[[state]] %in% stinvyrlst]
            missyr <- measyrs[[state]][!measyrs[[state]] %in% stinvyrlst]
            message(state, " missing following inventory years: ", toString(missyr))
          }
        }
      }
    }

    ## Check subcycle99
    subcycle99 <- pcheck.logical(subcycle99, varnm="subcycle99", 
		title="Keep SUBCYCLE 99?", first="NO", gui=gui)

    ## For periodic data, the INTENSITY variable does not equal 1
    if (!invtype == "ANNUAL") {
      message("note: periodic data includes forested plots >= 5% cover")
      #intensity1 <- FALSE
    }

    ## Check ACI
    ACI <- pcheck.logical(ACI, varnm="ACI", 
		title="ACI conditions?", first="NO", gui=gui)

  } else {
    subsetPOP <- TRUE

    if (!is.null(subcycle99) && subcycle99) 
      message("subcycle99 plots are not included in FIA evaluations")  
    subcycle99 <- FALSE
    
    if (!is.null(ACI) && ACI) 
      message("ACI plots are not included in FIA evaluations")  
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
  
  
  ## Check intensity1
  intensity1 <- pcheck.logical(intensity1, varnm="intensity1", 
                               title="Intensity = 1?", 
                               first="YES", gui=gui)
  
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

  if (spcond) {
    ## Check spcondid1
    ###########################################################
    spcondid1 <- pcheck.logical(spcondid1, varnm="spcondid1", 
		title="Use cond1 for spatial?", first="YES", gui=gui)
  }


  ########################################################################
  ### Saving data
  ########################################################################

  ## Check savedata
  savedata <- pcheck.logical(savedata, varnm="savedata", 
		title="Save data to outfolder?", first="YES", gui=gui)

  ## Check saveqry
  saveqry <- pcheck.logical(saveqry, varnm="saveqry", 
		title="Save queries to outfolder?", first="YES", gui=gui)

  ## Check parameters
  parameters <- pcheck.logical(parameters, varnm="parameters", 
		title="Save parameters", first="NO", gui=gui)

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
    outlst <- pcheck.output(out_dsn=out_dsn, 
                            out_fmt=out_fmt, 
                            outfolder=outfolder, 
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_dsn=overwrite_dsn, 
                            append_layer=append_layer, 
                            gui=gui)
    outfolder <- outlst$outfolder
    out_dsn <- outlst$out_dsn
    out_fmt <- outlst$out_fmt
  }
 
  ###########################################################################
  #########################      BUILD QUERIES     ##########################
  ###########################################################################
  if (defaultVars) {
    istree2 <- ifelse(istree || !is.null(alltFilter), TRUE, FALSE)
    DBvars <- DBvars.default(istree=istree2, isseed=isseed, isveg=isveg, 
		issubp=issubp, isdwm=isdwm, plotgeom=plotgeom, regionVars=regionVars)
    for (nm in names(DBvars)) assign(nm, DBvars[[nm]])
    for (nm in names(filtervarlst)) assign(nm, filtervarlst[[nm]])

    if (datsource == "sqlite") {
      pltfldlst <- DBI::dbListFields(dbconn, plot_layer)
      if (is.null(chkdbtab(pltfldlst, "LON")) && !is.null(chkdbtab(pltfldlst, "LON_PUBLIC"))) {
        pltvarlst <- sub("LON", "LON_PUBLIC", pltvarlst)
        pltvarlst <- sub("LAT", "LAT_PUBLIC", pltvarlst)
      }
      if (is.null(chkdbtab(pltfldlst, "ELEV")) && !is.null(chkdbtab(pltfldlst, "ELEV_PUBLIC"))) {
        pltvarlst <- sub("ELEV", "ELEV_PUBLIC", pltvarlst)
      }
      pltvarlst <- pltvarlst[pltvarlst %in% pltfldlst]
    }
 
    ## add commas
    vars <- toString(c(paste0("p.", pltvarlst), paste0("c.", condvarlst)))
    pcgvars <- toString(c(paste0("p.", pltvarlst), paste0("pg.", pgeomvarlst), 
		paste0("c.", condvarlst)))
    if (iseval) {
      vars <- paste0(vars, ", ppsa.EVALID")
    }
  } else {
    vars <- "p.*"
  }
  sppvars <- {}
  if (biojenk) sppvars <- c(sppvars, "JENKINS_TOTAL_B1", "JENKINS_TOTAL_B2")
  if (greenwt) sppvars <- c(sppvars, "DRYWT_TO_GREENWT_CONVERSION")


  ###########################################################################
  ############################      From query       ########################
  ###########################################################################
  if (datsource == "sqlite") {
    plot_layer <- chkdbtab(dbtablst, plot_layer, stopifnull=TRUE)
    cond_layer <- chkdbtab(dbtablst, cond_layer, stopifnull=TRUE)
  } 

  ## PPSA query
  ################################################
  if (savePOP || iseval) {
    if (datsource == "sqlite") {
      ppsa_layer <- chkdbtab(dbtablst, "POP_PLOT_STRATUM_ASSGN")
      if (is.null(ppsa_layer)) {
        ppsa_layer <- chkdbtab(dbtablst, "ppsa", stopifnull=TRUE)
      }
    }
    ppsafromqry <- paste0(SCHEMA., ppsa_layer, " ppsa")
  }
 
  ## PLOT from/join query
  ################################################
  if (iseval) {
    pfromqry <- paste0(ppsafromqry, " JOIN ", SCHEMA., 
			plot_layer, " p ON (p.CN = ppsa.PLT_CN)")
  } else if (measCur) {
    popSURVEY <- TRUE
    survey_layer <- "SURVEY"
    if (datsource == "sqlite") {
      survey_layer <- chkdbtab(dbtablst, "SURVEY")
      popSURVEY <- ifelse(is.null(survey_layer), FALSE, TRUE)
    }
    pfromqry <- getpfromqry(Endyr=measEndyr, SCHEMA.=SCHEMA., allyrs=allyrs,
			subcycle99=subcycle99, intensity1=intensity1, popSURVEY=popSURVEY,
			plotnm=plot_layer, surveynm=survey_layer)
    pfromqry <- gsub("survey", survey_layer, pfromqry)
  } else {
    pfromqry <- paste0(SCHEMA., "PLOT p")
  }

  ## PLOT/COND from/join query
  ################################################
  pcfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				cond_layer, " c ON (c.PLT_CN = p.CN)")
  if (plotgeom) {
    plotgeom_layer <- "PLOTGEOM"
    if (datsource == "sqlite") {
      plotgeom_layer <- chkdbtab(dbtablst, "PLOTGEOM")
      if (is.null(plotgeom_layer)) {
        plotgeom <- FALSE
      }
    }
    pcgeomfromqry <- paste0(pcfromqry, " JOIN ", SCHEMA., 
				plotgeom_layer, " pg ON (pg.CN = p.CN)")
  }

  ## TREE query
  ################################################
  if (istree || !is.null(alltFilter)) {
    if (datsource == "sqlite") {
      tree_layer <- chkdbtab(dbtablst, "TREE")
      if (is.null(tree_layer)) {
        istree <- FALSE
      }
    }
    tfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				tree_layer, " t ON (t.PLT_CN = p.CN)")
  }
  ## SEED query
  ################################################
  if (isseed) {
    seed_layer <- "SEEDLING"
    if (datsource == "sqlite") {
      seed_layer <- chkdbtab(dbtablst, "SEEDLING")
      if (is.null(seed_layer)) {
        isseed <- FALSE
      }
    }
    sfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				seed_layer, " seed ON (seed.PLT_CN = p.CN)")
  } 
  ## VEG query
  ################################################
  if (isveg) {
    vsub_layer <- "P2VEG_SUBPLOT_SPP"
    vstr_layer <- "P2VEG_SUBP_STRUCTURE"
    inv_layer <- "INVASIVE_SUBPLOT_SPP"
    if (datsource == "sqlite") {
      vsub_layer <- chkdbtab(dbtablst, vsub_layer)
      vstr_layer <- chkdbtab(dbtablst, vstr_layer)
      if (is.null(vstr_layer)) {
        isveg <- FALSE
      }
      inv_layer <- chkdbtab(dbtablst, inv_layer)
    }
    vfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				vsub_layer, " v ON v.PLT_CN = p.CN")
    vstrfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				vstr_layer, " v ON v.PLT_CN = p.CN")
    invfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				inv_layer, " v ON v.PLT_CN = p.CN")
  }
  ## SUBP query
  ################################################
  if (issubp) {
    subp_layer <- "SUBPLOT"
    subpcond_layer <- "SUBP_COND"
    if (datsource == "sqlite") {
      subp_layer <- chkdbtab(dbtablst, "SUBPLOT")
      if (is.null(subp_layer)) {
        issubp <- FALSE
      }
      subpcond_layer <- chkdbtab(dbtablst, "SUBP_COND")
      if (is.null(subpcond_layer)) {
        issubp <- FALSE
      }
    }
    subpfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				subp_layer, " subp ON subp.PLT_CN = p.CN")
    subpcfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				subpcond_layer, " subpc ON subpc.PLT_CN = p.CN")
  }
  ## DWM query
  ################################################
  if (isdwm) {
    dwm_layer <- "COND_DWM_CALC"
    if (datsource == "sqlite") {
      dwm_layer <- chkdbtab(dbtablst, "COND_DWM_CALC")
      if (is.null(dwm_layer)) {
        isdwm <- FALSE
      }
    }
    dfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				dwm_layer, " d ON (d.PLT_CN = p.CN)")
  }

  ## lulc query
  ################################################
  if (islulc) {
    lulcfromqry <- paste0(pcfromqry, 
		" JOIN ", SCHEMA., "COND pcond ON (pcond.PLT_CN = p.PREV_PLT_CN)",
		" JOIN ", SCHEMA., "SUBP_COND_CHNG_MTRX sccm ON (sccm.PLT_CN = c.PLT_CN 
		AND sccm.PREV_PLT_CN = pcond.PLT_CN 
		AND sccm.CONDID = c.CONDID 
		AND sccm.PREVCOND = pcond.CONDID)")
  }

  ## GRM query
  ################################################
  if (isgrm) {
    grm_layer <- "TREE_GRM_COMPONENT"
    if (datsource == "sqlite") {
      grm_layer <- chkdbtab(dbtablst, "TREE_GRM_COMPONENT")
      if (is.null(grm_layer)) {
        isgrm <- FALSE
      }
    }
    grmfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				grm_layer, " grm ON (grm.PLT_CN = p.CN)")
  }

  ## Other tables
  ################################################
  if (!is.null(othertables)) {
    if (datsource == "sqlite") {
      for (othertab in othertables) {
        othertab_layer <- chkdbtab(dbtablst, othertab)
        if (is.null(othertab_layer)) {
          othertables <- othertables[othertables != othertab]
        }
      }
    }
    xfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				"SUBX x ON (x.PLT_CN = p.CN)")
    xfromqry_plotgeom <- paste0(pfromqry, " JOIN ", SCHEMA., 
				"SUBX x ON (x.CN = p.CN)")
    xfromqry2 <- paste0(pfromqry, " JOIN ", SCHEMA., 
				"SUBX x ON (x.STATECD = p.STATECD
						and x.UNITCD = p.UNITCD
						and x.COUNTYCD = p.COUNTYCD
						and x.PLOT = p.PLOT)")
  }
  


##############################################################################
##############################################################################
##############################################################################
  nbrcnds <- {}
  stcds <- {}
  stabbrfn <- ""
  pltcnt <- {}
  stateFilters <- {}
  filtervarlst <- c(pltvarlst, condvarlst)
  spcoords <- "PUBLIC"
  spcoordslst <- "PUBLIC"

  if (returndata) {
    plt=cond=pltcond=tree=seed=spconddat <- {}
    if(isveg) { vsubpspp=vsubpstr=invsubp <- {} }
    if(issubp) { subp=subpc <- {} }
    if(isdwm) { dwm <- {} }
    if(issccm) { sccm <- {} }
    if(isgrm) { grm <- {} }
    if (islulc) {lulc <- {} }
    if(savePOP || iseval) ppsa <- {}  

    if (!is.null(othertables)) {
      for (i in 1:length(othertables)) 
        assign(paste0("other", i), {})
    }    

    ## Create empty object for each spcoords
    for (coords in spcoordslst) {
      if (xymeasCur) {
        assign(paste0("xyCur_", coords), {})
      } else {
        assign(paste0("xy_", coords), {})
      }

      if (issp) {
        if (xymeasCur) {
          assign(paste0("spxyCur_", coords), {})
        } else {
          assign(paste0("spxy_", coords), {})
        }
      } 
    }
  }

  ## REF_SPECIES table 
  if (istree && !is.null(sppvars)) {
    REF_SPECIES <- DBgetCSV("REF_SPECIES", returnDT=TRUE, stopifnull=FALSE)
  }

  ###################################################################################
  ## Loop through states
  ###################################################################################
  for (i in 1:length(states)) {
    evalid <- NULL
    state <- states[i]
    message("getting data from ", state)
    stcd <- pcheck.states(state, "VALUE")
    stabbr <- pcheck.states(state, "ABBR")
    pltx=condx=treex=seedx=vsubpsppx=vsubpstrx=invsubpx=subpx=subpcx=dwmx=sccmx=
		ppsax=spconddatx=lulcx <- NULL   

    if (!is.null(othertables)) {
      for (j in 1:length(othertables)) 
        assign(paste0("otherx", j), NULL)
    }    
   
    ## Create filter for state
    stFilter <- paste0("p.STATECD IN(", stcd, ")") 

    ## If FIA evaluation, get all plot from all evaluations.
    if (iseval) {
      evalid <- evalidlist[[state]]
      evalFilter <- paste0("ppsa.EVALID IN(", toString(evalid), ")")

      if (any(evalType == "P2VEG")) {
        evalid.veg <- evalid[endsWith(as.character(evalid), "10")]
        if (length(evalid.veg) == 0) stop("must include evaluation ending in 10")
        evalFilter.veg <- paste("ppsa.EVALID =", evalid.veg)
      } else {
        evalFilter.veg <- evalFilter
      }
      if (isdwm) {
        evalid.dwm <- evalid[endsWith(as.character(evalid), "07")]
        if (length(evalid.dwm) == 0) stop("must include evaluation ending in 07")
        evalFilter.dwm <- paste("ppsa.EVALID =", evalid.dwm)
      } 
      if (isgrm) {
        evalid.grm <- evalid[endsWith(as.character(evalid), "03")]
        if (length(evalid.grm) == 0) stop("must include evaluation ending in 03")
        evalFilter.grm <- paste("ppsa.EVALID =", evalid.grm)
      } 
    } else {
      evalFilter <- stFilter 
      if (length(invyrs) > 0){
        invyr <- invyrs[[state]]
        evalFilter <- paste0(stFilter, " and p.INVYR IN(", toString(invyr), ")")

      } else if (length(measyrs) > 0) {
        measyr <- measyrs[[state]]
        evalFilter <- paste0(stFilter, " and p.MEASYEAR IN(", toString(measyr), ")")
      }
      if (!subcycle99) {
        evalFilter <- paste(evalFilter, "and p.SUBCYCLE <> 99")
      }
      if (isveg) {
        evalFilter.veg <- evalFilter
      }
      if (isdwm) {
        evalFilter.dwm <- evalFilter 
      }      
      if (isgrm) {
        evalFilter.grm <- evalFilter
      }       
    } 
    if (intensity1) {
      evalFilter <- paste(evalFilter, "and p.INTENSITY = '1'")
    }

    if (datsource == "datamart") {

      ## Get CSV files
      #################################################

      ## PLOT table  
      PLOT <- DBgetCSV("PLOT", stabbr, returnDT=TRUE, stopifnull=FALSE)
 
      ## PLOTGEOM table  
      if (plotgeom) {
        PLOTGEOM <- DBgetCSV("PLOTGEOM", stabbr, returnDT=TRUE, stopifnull=FALSE)
      }

      ## COND table 
      COND <- DBgetCSV("COND", stabbr, returnDT=TRUE, stopifnull=FALSE)
 
      if (iseval || savePOP) {
        ## POP_PLOT_STRATUM_ASSGN table (ZIP FILE) - 
        ## To get estimation unit & stratum assignment for each plot. 
        POP_PLOT_STRATUM_ASSGN <- DBgetCSV("POP_PLOT_STRATUM_ASSGN", stabbr, 
		      returnDT=TRUE, stopifnull=FALSE) 
      }   
      ## Seedling table
      if (isseed) {
        SEEDLING <- DBgetCSV("SEEDLING", stabbr, returnDT=TRUE, 
		      stopifnull=FALSE)
      }
      ## Understory vegetation
      if (isveg) {
        P2VEG_SUBPLOT_SPP <- 
		      DBgetCSV("P2VEG_SUBPLOT_SPP", stabbr, returnDT=TRUE, 
		      stopifnull=FALSE)
        P2VEG_SUBP_STRUCTURE <- 
		      DBgetCSV("P2VEG_SUBP_STRUCTURE", stabbr, returnDT=TRUE, 
		      stopifnull=FALSE)
        INVASIVE_SUBPLOT_SPP <- 
		      DBgetCSV("INVASIVE_SUBPLOT_SPP", stabbr, returnDT=TRUE, 
		      stopifnull=FALSE)
      }
      ## Subplot data
      if (issubp) {
        SUBPLOT <- 
		      DBgetCSV("SUBPLOT", stabbr, returnDT=TRUE, stopifnull=FALSE)
        SUBP_COND <- 
		      DBgetCSV("SUBP_COND", stabbr, returnDT=TRUE, stopifnull=FALSE)
      }
      ## DWM calc table
      if (isdwm) {
        COND_DWM_CALC <- DBgetCSV("COND_DWM_CALC", stabbr, returnDT=TRUE, 
		      stopifnull=FALSE)
      }
      ## Area change matrix table
      if (issccm || islulc) {
        SUBP_COND_CHNG_MTRX <- DBgetCSV("SUBP_COND_CHNG_MTRX", stabbr, 
		      returnDT=TRUE, stopifnull=FALSE)
      }
      ## GRM calc table
      if (isgrm) {
        TREE_GRM_COMPONENT <- DBgetCSV("TREE_GRM_COMPONENT", stabbr, 
		      returnDT=TRUE, stopifnull=FALSE)
      }

      ## Other tables
      if (!is.null(othertables)) {
        for (othertable in othertables) {
          assign(othertable, 
 		        DBgetCSV(othertable, stabbr, returnDT=TRUE, stopifnull=FALSE))
        }
      }
    } 
############ End CSV only        

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

        filterdbqry <- paste0("select distinct ", filtervarx, " from ", filterfromqry, 
			" where ", evalFilter)
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
      #if (iseval) 
      #  vars <- paste0(vars, ", ppsa.EVALID")
      if (plotgeom) {
        pltcondqry <- paste("select distinct", pcgvars, "from", pcgeomfromqry, "where", xfilter)
      } else {      
        pltcondqry <- paste("select distinct", vars, "from", pcfromqry, "where", xfilter)
      }

      if (datsource == "sqlite") {
        pltcondx <- DBI::dbGetQuery(dbconn, pltcondqry)
      } else {
        pltcondx <- setDT(sqldf::sqldf(pltcondqry, stringsAsFactors=FALSE))
      }
   
      ## Write query to outfolder
      if (saveqry) {
        pltcondqryfn <- DBgetfn("pltcond", invtype, outfn.pre, stabbr, 
		      evalid=evalid, qry=TRUE, outfolder=outfolder, overwrite=overwrite_layer, 
		      outfn.date=outfn.date, ext="txt")
        outfile <- file(pltcondqryfn, "w")
        cat(  pltcondqry, "\n", file=outfile)
        close(outfile)
        message("saved pltcond query to:\n", pltcondqryfn)
      }
    }

    if (is.null(pltcondx) || nrow(pltcondx) == 0) {
      message("no plots in database for ", state)
    } else {
      pltvarlst2 <- pltvarlst
      if (plotgeom) {
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
        if ("PREV_PLTCN" %in% names(pltx))
          pltx[, PREV_PLTCN := as.character(PREV_PLTCN)]             
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
      }
      if ("LAT" %in% names(pltx)) {
        setnames(pltx, "LAT", "LAT_PUBLIC")
        pltvarlst2[pltvarlst2 == "LAT"] <- "LAT_PUBLIC"
      }
      if ("ELEV" %in% names(pltx)) {
        setnames(pltx, "ELEV", "ELEV_PUBLIC")
        pltvarlst2[pltvarlst2 == "ELEV"] <- "ELEV_PUBLIC"
      }


      ## Create plot-level, number of condtion variables
      ###########################################################
      if (defaultVars) {

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

        ## Merge to plt table
        pltx <- nbrcnd[pltx]

        nbrcndlst <- c("NBRCND", "NBRCNDSAMP", "NBRCNDFOR", "NBRCNDFTYP")
        pltvarlst2 <- c(pltvarlst2, nbrcndlst)      

        ## CCLIVEPLT:
        ## A plot level canopy cover variable based on LIVE_CANOPY_CVR_PCT
        if (all(c("LIVE_CANOPY_CVR_PCT", "CONDPROP_UNADJ") %in% names(condx))) {
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
          if (all(c("CRCOVPCT_RMRS", "CONDPROP_UNADJ") %in% names(condx))) {
            ccRMRSplt <- condx[, list(round(sum(CRCOVPCT_RMRS * CONDPROP_UNADJ, 
			        na.rm=TRUE), 2)), by="PLT_CN"]
            setnames(ccRMRSplt, c("PLT_CN", "CCRMRSPLT"))
            pltx <- ccRMRSplt[pltx]

            pltvarlst2 <- c(pltvarlst2, "CCRMRSPLT")
          }
          ## CCPLT: plot level canopy cover variable based on CRCOV
          if (all(c("CRCOV", "CONDPROP_UNADJ") %in% names(condx))) {
            ccplt <- condx[, list(round(sum(CRCOVPCT_RMRS * CONDPROP_UNADJ, 
			na.rm=TRUE), 2)), by="PLT_CN"]
            setnames(ccplt, c("PLT_CN", "CCPLT"))
            pltx <- ccplt[pltx]

            pltvarlst2 <- c(pltvarlst2, "CCRMRSPLT")
          }
        }  

        ## FORNONSAMP: 
        ## Plot-level variable based on PLOT_STATUS_CD and PLOT_NONSAMPLE_REASN_CD
        if ("PLOT_NONSAMPLE_REASN_CD" %in% names(pltx)) {
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
        pltx[, PLOT_ID := paste0("ID", 
		        formatC(pltx$STATECD, width=2, digits=2, flag=0), 
          	formatC(pltx$UNITCD, width=2, digits=2, flag=0),
          	formatC(pltx$COUNTYCD, width=3, digits=3, flag=0),
          	formatC(pltx$PLOT, width=5, digits=5, flag=0))] 
        pltvarlst2 <- c(pltvarlst2, "PLOT_ID")

        ## Additional condition variables
        ######################################################################
        ref_fortypgrp <- FIESTAutils::ref_codes[FIESTAutils::ref_codes$VARIABLE == "FORTYPCD",]

        ## FORTYPGRP: condition level variable grouping FORTYPCD
        cndnames <- names(condx)
        if ("FORTYPCD" %in% names(condx)) {
          condx <- merge(condx, ref_fortypgrp[,c("VALUE", "GROUPCD")],
        		by.x="FORTYPCD", by.y="VALUE", all.x=TRUE)
          setnames(condx, "GROUPCD", "FORTYPGRPCD")
          setcolorder(condx, c(cndnames, "FORTYPGRPCD"))
        
          condvarlst2 <- c(condvarlst2, "FORTYPGRPCD")
        }
        ## FLDTYPGRP: condition level variable grouping FLDTYPGRP
        if ("FLDTYPCD" %in% names(condx)) {
          condx <- merge(condx, ref_fortypgrp[,c("VALUE", "GROUPCD")], 
               by.x="FLDTYPCD", by.y="VALUE", all.x=TRUE)
          setnames(condx, "GROUPCD", "FLDTYPGRPCD")
          setcolorder(condx, c(cndnames, "FLDTYPGRPCD"))

          condvarlst2 <- c(condvarlst2, "FLDTYPGRPCD")
        }
        setkey(condx, PLT_CN, CONDID)

        ## TIMBERCD condition level variable defining TIMBERLAND conditions
        if ("SITECLCD" %in% names(condx)) {
          condx[COND_STATUS_CD == 1, TIMBERCD := 2]
          condx[SITECLCD %in% 1:6, TIMBERCD := 1]

          condvarlst2 <- c(condvarlst2, "TIMBERCD")
        }

        ## LANDUSECD
        ## A combination of PRESNFCD and COND_STATUS_CD
        if (all(c("PRESNFCD", "COND_STATUS_CD") %in% names(condx))) {
          condx$LANDUSECD <- with(condx, ifelse(is.na(PRESNFCD), COND_STATUS_CD, PRESNFCD))
        }
      }   ##  End (defaultVars)
      
      setnames(pltx, "PLT_CN", "CN")
      setkeyv(pltx, "CN")
    }
    pltx <- pltx[, pltvarlst2, with=FALSE]
 
    ## Create combined unique identifier to subset other tables
    pcondID <- condx[, paste(PLT_CN, CONDID)]


    ##############################################################
    ## lulc data
    ##############################################################
    if (islulc && !is.null(pltx)) {
      message("\n",
      	"## STATUS: Getting Land Use/Land Cover data (", stabbr, ") ...", "\n")
      lulcqry <- paste("select c.PLT_CN, p.PREV_PLT_CN, p.STATECD, p.UNITCD, p.COUNTYCD, p.PLOT,
 		pcond.CONDID PREV_CONDID, c.CONDID, 
    		pcond.INVYR PREV_INVYR, c.INVYR,
		pcond.CONDPROP_UNADJ PREV_CONDPROP_UNADJ, c.CONDPROP_UNADJ, 
 		pcond.COND_STATUS_CD PREV_COND_STATUS_CD, c.COND_STATUS_CD,
 		pcond.LAND_COVER_CLASS_CD PREV_LAND_COVER_CLASS_CD, c.LAND_COVER_CLASS_CD, 
		pcond.PRESNFCD PREV_PRESNFCD, c.PRESNFCD,
		case when pcond.PRESNFCD is null 
			then pcond.COND_STATUS_CD 
				else pcond.PRESNFCD end as PREV_LANDUSECD,
		case when c.PRESNFCD is null 
			then c.COND_STATUS_CD 
				else c.PRESNFCD end as LANDUSECD,
		sum(sccm.SUBPTYP_PROP_CHNG) / 4 PROP_CHNG",  
		"from", lulcfromqry, 
   		"where c.CONDPROP_UNADJ IS NOT NULL 
			AND ((sccm.SUBPTYP = 3 AND c.PROP_BASIS = 'MACR') 
				OR (sccm.SUBPTYP = 1 AND c.PROP_BASIS = 'SUBP')) 
			AND COALESCE(c.COND_NONSAMPLE_REASN_CD, 0) = 0 
			AND COALESCE(pcond.COND_NONSAMPLE_REASN_CD, 0) = 0",
		"and", xfilter, 
		"group by c.PLT_CN, p.PREV_PLT_CN, 
  			p.STATECD, p.UNITCD, p.COUNTYCD, p.PLOT,
    			pcond.CONDID, c.CONDID,
    			pcond.INVYR, c.INVYR, pcond.CONDPROP_UNADJ, c.CONDPROP_UNADJ,
    			pcond.COND_STATUS_CD, c.COND_STATUS_CD,
    			pcond.LAND_COVER_CLASS_CD, c.LAND_COVER_CLASS_CD,
    			pcond.PRESNFCD, c.PRESNFCD")
      if (datsource == "sqlite") {
        lulcx <- DBI::dbGetQuery(dbconn, lulcqry)
      } else {
        lulcx <- sqldf::sqldf(lulcqry, stringsAsFactors=FALSE)
      }
      if (nrow(lulcx) != 0) {
        lulcx <- setDT(lulcx)
        lulcx[, PLT_CN := as.character(PLT_CN)]
        lulcx[, PREV_PLT_CN := as.character(PREV_PLT_CN)]
        setkey(lulcx, PLT_CN)

        ## Subset overall filters from pltx
        lulcx <- lulcx[lulcx$PLT_CN %in% pltx$CN,]

        ## Merge to pltx
        #pltx <- merge(pltx, lulcx, all.x=TRUE, by.x="CN", by.y="PLT_CN")  
      } else {
        lulcx <- NULL
      }

      ## Write query to outfolder
      if (saveqry) {
        lulcqryfn <- DBgetfn("lulc", invtype, outfn.pre, stabbr, 
		evalid=evalid, qry=TRUE, outfolder=outfolder, 
		overwrite=overwrite_layer, outfn.date=outfn.date, ext="txt")
        outfile <- file(lulcqryfn, "w")
              cat(  lulcqryfn, "\n", file=outfile)
              close(outfile)
      }

      if (returndata) {
        ## Append data
        lulc <- rbind(lulc, lulcx)
      }
    }

    ##############################################################
    ## Tree data
    ##############################################################
    if ((istree || !is.null(alltFilter)) && !is.null(pltx)) {
      ## TREE table
      if (istree || !is.null(alltFilter)) {
        TREE <- DBgetCSV("TREE", stabbr, returnDT=TRUE, stopifnull=FALSE)
      }

      message("\n",
      	"## STATUS: Getting tree data from TREE (", stabbr, ") ...", "\n")
      if (is.null(treevarlst) & is.null(tsumvarlst)) {
        treex <- NULL
        istree <- FALSE
      } else {
        ttvars <- toString(paste0("t.", c(treevarlst, tsumvarlst)))
        treeqry <- paste("select distinct", ttvars, "from", tfromqry, "where", xfilter)

        if (datsource == "sqlite") {
          treex <- DBI::dbGetQuery(dbconn, treeqry)
        } else {
          treex <- sqldf::sqldf(treeqry, stringsAsFactors=FALSE)
        }
        if (nrow(treex) != 0) {
          treex <- setDT(treex)
          treex[, PLT_CN := as.character(PLT_CN)]
          setkey(treex, PLT_CN, CONDID)

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

          if (istree && !is.null(treex)) {
            ## Check ACI
            if (!ACI) {
              ACIpltID <- condx[COND_STATUS_CD == 1, paste(PLT_CN, CONDID)]
              treex <- treex[paste(treex$PLT_CN, treex$CONDID) %in% ACIpltID,]
            } 

            ## Write query to outfolder
            if (saveqry) {
              treeqryfn <- DBgetfn("tree", invtype, outfn.pre, stabbr, 
			evalid=evalid, qry=TRUE, outfolder=outfolder, 
			overwrite=overwrite_layer, outfn.date=outfn.date, ext="txt")
              outfile <- file(treeqryfn, "w")
              cat(  treeqryfn, "\n", file=outfile)
              close(outfile)
            }

            ## Make sure these variables are numeric
            nbrvars <- c("DIA", "DRYBIO_BOLE", "DRYBIO_STUMP", "DRYBIO_TOP", 
			"DRYBIO_SAPLING", "DRYBIO_WDLD_SPP", "BHAGE")
            if (any(nbrvars %in% names(treex)))
              nbrvars <- nbrvars[which(nbrvars %in% names(treex))]
            treex[, (nbrvars) := lapply(.SD, check.numeric), .SDcols=nbrvars]

            ## Change NA values to 0 values
            #if (any(names(treex) %in% treenavars)) 
            #  treex <- DT_NAto0(treex, treenavars)

            if (defaultVars)
              ## Create new tree variables - basal area
              treex[, BA := DIA * DIA * 0.005454]

            ## Create new biomass variables
            if (!is.null(sppvars)) {
              sppsql <- paste("select SPCD,", paste(sppvars, collapse=","), 
				          "from REF_SPECIES")
              ref_spp <- sqldf::sqldf(sppsql)

              treenames <- names(treex)
              treex <- merge(treex, ref_spp, by="SPCD")
              if (biojenk) {
                treex[, BIOJENK_kg := exp(JENKINS_TOTAL_B1 + JENKINS_TOTAL_B2 * log(DIA * 2.54))]
                treex[, BIOJENK_lb := BIOJENK_kg * 2.2046]		## Converts back to tons
                treex[, JENKINS_TOTAL_B1 := NULL][, JENKINS_TOTAL_B2 := NULL]
                sppvarsnew <- c(sppvars, "BIOJENK_kg", "BIOJENK_lb")
              }
              setcolorder(treex, c(treenames, sppvarsnew)) 
            }  

            ## Append data
            if (treeReturn && returndata) {
              tree <- rbind(tree, treex)
            }
          }
          rm(TREE)
          gc()
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
      #xyx <- pltx[, c("CN", getcoords(coords), "PLOT_ID"), with=FALSE]
      if (getxy) {
        xyx <- copy(pltx)
        setnames(xyx, "CN", "PLT_CN")

        ## Get xy for the most current sampled plot
        if (xymeasCur) {
          xyfromqry <- getpfromqry(Endyr=measEndyr, SCHEMA.=SCHEMA.,
		        subcycle99=subcycle99, intensity1=intensity1, plotnm="pltx")

          xvars <- c("p.CN", "p.STATECD", "p.UNITCD", "p.COUNTYCD", "p.PLOT", 
		          "p.PLOT_ID", paste0("p.", getcoords(coords)))
          xyx.qry <- paste("select distinct", toString(xvars), "from", xyfromqry)
          xyCurx <- sqldf::sqldf(xyx.qry)
          names(xyCurx)[names(xyCurx) == "CN"] <- "PLT_CN"
          xyCurx$COUNTYFIPS <- paste0(formatC(xyCurx$STATECD, width=2, digits=2, flag=0), 
          	formatC(xyCurx$COUNTYCD, width=3, digits=3, flag=0))
          assign(paste0("xyCurx_", coords), xyCurx) 
          if (returndata) {
            assign(paste0("xyCur_", coords), 
				  rbind(get(paste0("xyCur_", coords)), xyCurx)) 
          }
        } else {
          xyx <- xyx[, c("PLT_CN", "STATECD", "UNITCD", "COUNTYCD", "PLOT", 
		          "LON_PUBLIC", "LAT_PUBLIC", "PLOT_ID"), with=FALSE]
          xyx$COUNTYFIPS <- paste0(formatC(xyx$STATECD, width=2, digits=2, flag=0), 
          	formatC(xyx$COUNTYCD, width=3, digits=3, flag=0))
          assign(paste0("xyx_", coords), xyx)
          if (returndata) {
            assign(paste0("xy_", coords), 
				  rbind(get(paste0("xy_", coords)), xyx))
          }
        } 
      }
    }
 
    ##############################################################
    ## Seedling data (SEEDLING)
    ##############################################################
    if (isseed && !is.null(pltx)) {

      if (is.null(seedvarlst)) {
        seedx <- NULL
        isseed <- NULL
      } else {
        message("\n",
      	"## STATUS: Getting seed data from SEEDLING (", stabbr, ") ...", "\n")

        ssvars <- toString(paste0("seed.", c(seedvarlst, ssumvarlst)))
        seedqry <- paste("select distinct", ssvars, "from", sfromqry, "where", xfilter)
        if (datsource == "sqlite") {
          seedx <- DBI::dbGetQuery(dbconn, seedqry)
        } else {
          seedx <- sqldf::sqldf(seedqry, stringsAsFactors=FALSE)
        }
        if (nrow(seedx) != 0) {
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
        } else {
          seedx <- NULL
        }
      }

      if (returndata) {
        ## Append data
        seed <- rbind(seed, seedx)
      }
    }

    ##############################################################
    ## Understory vegetation data (P2VEG_SUBPLOT_SPP/P2VEG_SUBP_STRUCTURE
    ##############################################################
    if (isveg && !is.null(pltx)) {
      message("\n",
      "## STATUS: Getting veg data from P2VEG_SUBPLOT_SPP/P2VEG_SUBP_STRUCTURE (", 
		stabbr, ") ...", "\n")

      ## Get data for P2VEG_SUBPLOT_SPP
      vsubpsppvars <- toString(paste0("v.", vsubpsppvarlst))
      vsubpsppqry <- paste("select distinct", vsubpsppvars, "from", vfromqry, 
		                       "where", paste0(evalFilter.veg, stateFilters))

      if (datsource == "sqlite") {
        vsubpsppx <- DBI::dbGetQuery(dbconn, vsubpsppqry)
      } else {
        vsubpsppx <- sqldf::sqldf(vsubpsppqry, stringsAsFactors=FALSE)
      }
      if (nrow(vsubpsppx) != 0) {
        vsubpsppx <- setDT(vsubpsppx)
        vsubpsppx[, PLT_CN := as.character(PLT_CN)]
        setkey(vsubpsppx, PLT_CN)

        ## Subset overall filters from condx
        vsubpsppx <- vsubpsppx[paste(vsubpsppx$PLT_CN, vsubpsppx$CONDID) %in% pcondID,]
      } else {
        vsubpsppx <- NULL
      }

      ## Get data for P2VEG_SUBP_STRUCTURE
      vsubpstrvars <- toString(paste0("v.", vsubpstrvarlst))
      vsubpstrqry <- paste("select distinct", vsubpstrvars, "from", vstrfromqry, 
		                       "where", paste0(evalFilter.veg, stateFilters))
      vsubpstrx <- sqldf::sqldf(vsubpstrqry, stringsAsFactors=FALSE)

      if(nrow(vsubpstrx) != 0){
        vsubpstrx <- setDT(vsubpstrx)
        vsubpstrx[, PLT_CN := as.character(PLT_CN)]
        setkey(vsubpstrx, PLT_CN)

        ## Subset overall filters from condx
        vsubpstrx <- vsubpstrx[paste(vsubpstrx$PLT_CN, vsubpstrx$CONDID) %in% pcondID,]
      } else {
        vsubpstrx <- NULL
      }

      ## Get data for INVASIVE_SUBPLOT_SPP
      invsubpvars <- toString(paste0("v.", invsubpvarlst))
      invsubpqry <- paste("select distinct", invsubpvars, "from", invfromqry, 
		                      "where", paste0(evalFilter.veg, stateFilters))
      invsubpx <- sqldf::sqldf(invsubpqry, stringsAsFactors=FALSE)

      if(nrow(invsubpx) != 0){
        invsubpx <- setDT(invsubpx)
        invsubpx[, PLT_CN := as.character(PLT_CN)]
        setkey(invsubpx, PLT_CN)

        ## Subset overall filters from condx
        invsubpx <- invsubpx[paste(invsubpx$PLT_CN, invsubpx$CONDID) %in% pcondID,]
      } else {
        invsubpx <- NULL
      }

      if (returndata) {
        vsubpspp <- rbind(vsubpspp, vsubpsppx)
        vsubpstr <- rbind(vsubpstr, vsubpstrx)
        invsubp <- rbind(invsubp, invsubpx)
      }
    }

    ##############################################################
    ## Subplot data (SUBPLOT/SUBP_COND)
    ##############################################################
    if (issubp && !is.null(pltx)) {
      message("\n",
      "## STATUS: Getting subplot data from SUBPLOT/SUBP_COND (", stabbr, ") ...", "\n")

      ## Get data for SUBPLOT
      subpvars <- toString(paste0("subp.", subpvarlst))
      subpqry <- paste("select distinct", subpvars, " from", subpfromqry, 
		"where", paste0(evalFilter, stateFilters))

      if (datsource == "sqlite") {
        subpx <- DBI::dbGetQuery(dbconn, subpqry)
      } else {
        subpx <- sqldf::sqldf(subpqry, stringsAsFactors=FALSE)
      }
      if (nrow(subpx) != 0) {
        subpx <- setDT(subpx)
        subpx[, PLT_CN := as.character(PLT_CN)]
        setkey(subpx, PLT_CN)

        ## Subset overall filters from condx
        subpx <- subpx[subpx$PLT_CN %in% pltx$CN,]
      } else {
        subpx <- NULL
      }

      ## Get data for SUBP_COND
      subpcvars <- toString(paste0("subpc.", subpcvarlst))
      subpcqry <- paste("select distinct", subpcvars, "from", subpcfromqry, 
		"where", paste0(evalFilter, stateFilters))
      subpcx <- sqldf::sqldf(subpcqry, stringsAsFactors=FALSE)

      if(nrow(subpcx) != 0){
        subpcx <- setDT(subpcx)
        subpcx[, PLT_CN := as.character(PLT_CN)]
        setkey(subpcx, PLT_CN)

        ## Subset overall filters from condx
        subpcx <- subpcx[paste(subpcx$PLT_CN, subpcx$CONDID) %in% pcondID,]
      } else {
        subpcx <- NULL
      }

      if (returndata) {
        subp <- rbind(subp, subpx)
        subpc <- rbind(subpc, subpcx)
      }
    }

    ##############################################################
    ## Down woody data (COND_DWM_CALC)
    ##############################################################
    if (isdwm && !is.null(pltx)) {
      message("\n",
      "## STATUS: Getting DWM data from COND_DWM_CALC (", stabbr, ") ...", "\n")
    
      if (is.null(dwmvarlst)) {
        dwmx <- NULL
        isdwm <- FALSE
      } else {

        dvars <- toString(paste0("d.", dwmvarlst))
        dwmqry <- paste("select distinct", dvars, "from", dfromqry, 
		"where", paste0(evalFilter.dwm, stateFilters))

        if (datsource == "sqlite") {
          dwmx <- DBI::dbGetQuery(dbconn, dwmqry)
        } else {
          dwmx <- sqldf::sqldf(dwmqry, stringsAsFactors=FALSE)
        }
        if (nrow(dwmx) != 0) {
          dwmx <- setDT(dwmx)
          dwmx[, PLT_CN := as.character(PLT_CN)]
          setkey(dwmx, PLT_CN, CONDID)

          ## Subset overall filters from condx
          dwmx <- dwmx[paste(dwmx$PLT_CN, dwmx$CONDID) %in% pcondID,]
        } else {
          dwmx <- NULL
        }
      }
      if (returndata) {
        dwm <- rbind(dwm, dwmx)
      }
    }


    ##############################################################
    ## Tree Change, Growth, and Mortality (TREE_GRM_COMPONENT)
    ##############################################################
    if (isgrm && !is.null(pltx)) {
      message("\n",
      "## STATUS: Getting GRM data from TREE_GRM_COMPONENT (", stabbr, ") ...", "\n")
    
      if (is.null(grmvarlst)) {
        grmx <- NULL
        isgrm <- NULL
      } else {

        #grmvars <- toString(paste0("grm.", grmvarlst))
        grmqry <- paste("select grm.* from", dfromqry, 
		"where", paste0(evalFilter.dwm, stateFilters))

        if (datsource == "sqlite") {
          grmx <- DBI::dbGetQuery(dbconn, grmqry)
        } else {
          grmx <- sqldf::sqldf(grmqry, stringsAsFactors=FALSE)
        }
        if (nrow(grmx) != 0) {
          grmx <- setDT(grmx)
          grmx[, PLT_CN := as.character(PLT_CN)]
          setkey(grmx, PLT_CN, CONDID)

          ## Subset overall filters from condx
          #grmx <- grmx[paste(grmx$PLT_CN, grmx$CONDID) %in% pcondID,]
          grmx <- grmx[grmx$PLT_CN %in% pltx$CN,]
        } else {
          grmx <- NULL
        }
      }
      if (returndata) {
        grm <- rbind(grm, grmx)
      }
    }

    ##############################################################
    ## Other tables
    ##############################################################
    if (!is.null(othertables) && !is.null(pltx)) {
      for (j in 1:length(othertables)) {
        othertable <- othertables[j]
        othertablexnm <- paste0("otherx", j)

        message(paste0("\n",
        "## STATUS: GETTING", othertable, " (", stabbr, ")...", "\n"))
    
        if (!is.null(pcheck.varchar(othertable, checklst=pop_tables, stopifinvalid=FALSE))) {
          xfromqry <- paste0(SCHEMA., othertable, " x")
          if (!iseval) {
            xfilterpop <- stFilter
            xfilterpop <- sub("p.", "x.", xfilterpop)
          } else {
            xfilterpop <- paste0("x.EVALID IN(", toString(evalid), ")")
          }
          xqry <- paste("select distinct x.* from", sub("SUBX", othertable, xfromqry), 
			"where", xfilterpop)

        } else if (othertable == "PLOTGEOM") {
          joinid <- "CN"
          xqry <- paste("select distinct x.* from", sub("SUBX", othertable, xfromqry_plotgeom), 
			"where", xfilter)
        } else {
          joinid <- "PLT_CN"
          xqry <- paste("select distinct x.* from", sub("SUBX", othertable, xfromqry), 
			"where", xfilter)
        }
        if (datsource == "sqlite") {
          tab <- tryCatch( DBI::dbGetQuery(dbconn, xqry),
			error=function(e) return(NULL))
        } else {
          tab <- tryCatch( sqldf::sqldf(xqry, stringsAsFactors=FALSE), 
			error=function(e) return(NULL))
        }
        if (is.null(tab)) {
          xqry <- paste("select * from", othertable, "where", stFilter)

          if (datsource == "sqlite") {
            tab <- tryCatch( DBI::dbGetQuery(dbconn, xqry),
			error=function(e) return(NULL))
          } else {
            tab <- tryCatch( sqldf::sqldf(xqry, stringsAsFactors=FALSE), 
			error=function(e) return(NULL))
          }
        }
 
        if (is.null(pcheck.varchar(othertable, checklst=pop_tables, stopifinvalid=FALSE))) {
          ## Subset overall filters from condx
          if ("CONDID" %in% names(tab)) {
            tab <- tab[paste(tab$PLT_CN, tab$CONDID) %in% pcondID,]
          } else {
            tab <- tab[tab[[joinid]] %in% unique(pltx$CN),]
          }
        }
        if (nrow(tab) == 0) {
          message("othertable must include PLT_CN")
          tab <- NULL
        }

        if (!is.null(tab)) {
          assign(othertablexnm, setDT(tab))

          if ("PLT_CN" %in% names(get(othertablexnm))) {
            get(othertablexnm)[, PLT_CN := as.character(PLT_CN)]
            setkey(get(othertablexnm), "PLT_CN")

            ## Subset overall filters from pltx
            assign(othertablexnm, 
			get(othertablexnm)[get(othertablexnm)[[joinid]] %in% unique(pltx$CN),])
          }
          if (returndata) 
            assign(paste0("other", j), rbind(get(paste0("other", j)), get(othertablexnm)))
        }
      }
    }

    ##############################################################
    ## If savePOP or more than one evalType
    ##############################################################
    if ((iseval || savePOP) && !is.null(pltx)) {
      message(paste("\n",
      "## STATUS: GETTING POP_PLOT_STRATUM_ASSGN DATA (", stabbr, ")...", "\n"))
    
      ppsavars <- toString(c("PLT_CN", "EVALID", "STATECD", "ESTN_UNIT", "STRATUMCD"))
      ppsaqry <- paste("select", ppsavars, "from", ppsafromqry, "where statecd =", stcd)

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

      if(nrow(ppsax) != 0){
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
 
    ###############################################################################
    ###############################################################################
    ## SAVE data
    ###############################################################################
    ###############################################################################
    if ((savedata || !treeReturn) && !is.null(pltx)) {
      message("saving data...")
      col.names <- ifelse (i == 1, TRUE, FALSE)
      if (i > 1) { 
        append_layer <- TRUE
      }
      if (append_layer && overwrite_layer) {
        overwrite_layer <- FALSE
      }

      if (savedata && getxy && issp) {
        message("saving spatial xy data...")
        xycoords <- getcoords(coords)

        if (xymeasCur) {
          #spxynm <- paste0("spxyCur_", coords)
          xyplt <- get(paste0("xyCurx_", coords))
          spxynm <- xyplt
        } else {
          #spxynm <- paste0("spxy_", coords)
          xyplt <- get(paste0("xyx_", coords))
          spxynm <- xyplt
        }
        if (!is.null(xyplt)) {
          if (!is.null(pltx) && length(unique(xyplt$PLT_CN)) != nrow(pltx))
            warning("number of plots in ", spxynm, " does not match plt table")            

          ## Generate spatial output
          out_fmt_sp <- ifelse(out_fmt == "csv", "shp", out_fmt)
          assign(spxynm, spMakeSpatialPoints(xyplt=xyplt, 
                              xvar=xycoords[1], yvar=xycoords[2], 
                              xy.uniqueid="PLT_CN", xy.crs=4269, 
                              addxy=TRUE, exportsp=savedata, 
                              savedata_opts=list(
                                out_dsn=out_dsn, out_fmt=out_fmt_sp, 
                                outfolder=outfolder, out_layer=spxynm, 
                                outfn.date=outfn.date, overwrite_layer=overwrite_layer, 
                                append_layer=append_layer, outfn.pre=outfn.pre, 
                                overwrite_dsn=overwrite_dsn)))
        }
      # } else {
      #   ## output parameters
      #   ###########################################################
      #   if (savedata | saveqry | parameters | !treeReturn | !returndata) {
      #     outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
      #                     outfolder=outfolder, outfn.pre=outfn.pre, 
      #                     outfn.date=outfn.date, overwrite_dsn=overwrite_dsn, 
      #                     append_layer=append_layer, gui=gui)
      #     out_dsn <- outlst$out_dsn
      #     out_fmt <- outlst$out_fmt
      #   } 
      }

      if (savedata && getxy && !issp) {
        xycoords <- getcoords(coords)

        if (xymeasCur) {
          xynm <- paste0("xyCur_", coords)
          xyplt <- get(paste0("xyCurx_", coords))
        } else {
          xynm <- paste0("xy_", coords)
          xyplt <- get(paste0("xyx_", coords))
        }
        if (!is.null(xyplt)) {
          index.unique.xyplt <- NULL
          if (!append_layer) index.unique.xyplt <- "PLT_CN"
          datExportData(xyplt, 
              index.unique = index.unique.xyplt,
              savedata_opts = list(outfolder=outfolder, 
                                  out_fmt=out_fmt, 
                                  out_dsn=out_dsn, 
                                  out_layer=xynm,
                                  outfn.pre=outfn.pre, 
                                  outfn.date=outfn.date, 
                                  overwrite_layer=overwrite_layer,
                                  append_layer=append_layer,
                                  add_layer=TRUE)) 
        }
      }  
 
      if (savedata && !is.null(spconddatx)) {
        index.unique.spconddat <- NULL
        #if (!append_layer) index.unique.spconddatx <- "PLT_CN"
        index.unique.spconddatx <- "PLT_CN"
        datExportData(spconddatx, 
              index.unique = index.unique.spconddatx,
              savedata_opts = list(outfolder=outfolder, 
                                  out_fmt=out_fmt, 
                                  out_dsn=out_dsn, 
                                  out_layer="spconddat",
                                  outfn.pre=outfn.pre, 
                                  overwrite_layer=overwrite_layer,
                                  append_layer=append_layer,
                                  outfn.date=outfn.date, 
                                  add_layer=TRUE)) 
      }
      if (savedata && !is.null(pltx)) {
        index.unique.pltx <- NULL
        if (i == 1) index.unique.pltx <- "CN"
        datExportData(pltx, 
              index.unique = index.unique.pltx,
              savedata_opts = list(outfolder=outfolder, 
                                    out_fmt=out_fmt, 
                                    out_dsn=out_dsn, 
                                    out_layer="plot",
                                    outfn.pre=outfn.pre, 
                                    overwrite_layer=overwrite_layer,
                                    append_layer=append_layer,
                                    outfn.date=outfn.date, 
                                    add_layer=TRUE)) 
      }
      if (savedata && !is.null(condx)) {
        index.unique.condx <- NULL
        if (!append_layer) index.unique.condx <- c("PLT_CN", "CONDID")
        datExportData(condx, 
              index.unique = index.unique.condx,
              savedata_opts = list(outfolder=outfolder, 
                                  out_fmt=out_fmt, 
                                  out_dsn=out_dsn, 
                                  out_layer="cond",
                                  outfn.pre=outfn.pre, 
                                  overwrite_layer=overwrite_layer,
                                  append_layer=append_layer,
                                  outfn.date=outfn.date, 
                                  add_layer=TRUE)) 
      }
      if (savedata && !is.null(lulcx)) {
        index.unique.lulcx <- NULL
        if (!append_layer) index.unique.lulcx <- c("PLT_CN", "CONDID")
        datExportData(lulcx, 
            index.unique = index.unique.lulcx,
            savedata_opts = list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="lulc",
                                outfn.pre=outfn.pre, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                outfn.date=outfn.date, 
                                add_layer=TRUE)) 
      } 
 
      if ((savedata || !treeReturn) && !is.null(treex)) {
        index.unique.treex <- NULL
        if (!append_layer) index.unique.treex <- c("PLT_CN", "CONDID", "SUBP", "TREE")
        datExportData(treex, 
            index.unique = index.unique.treex,
            savedata_opts = list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="tree",
                                outfn.pre=outfn.pre, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                outfn.date=outfn.date, 
                                add_layer=TRUE)) 
      }
      if (savedata && !is.null(seedx)) {
        index.unique.seedx <- NULL
        if (!append_layer) index.unique.seedx <- c("PLT_CN", "CONDID", "SUBP")
        datExportData(seedx, 
            index.unique = index.unique.seedx,
            savedata_opts = list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="seed",
                                outfn.pre=outfn.pre, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                outfn.date=outfn.date, 
                                add_layer=TRUE)) 
      } 

      if (savedata && !is.null(vsubpsppx)) {
        index.unique.vsubpsppx <- NULL
        if (!append_layer) index.unique.vsubpsppx <- c("PLT_CN", "CONDID")
        datExportData(vsubpsppx, 
            index.unique = index.unique.vsubpsppx,
            savedata_opts = list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="vsubpspp",
                                outfn.pre=outfn.pre, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                outfn.date=outfn.date, 
                                add_layer=TRUE)) 
      }

      if (savedata && !is.null(vsubpstrx)) {
        index.unique.vsubpstrx <- NULL
        if (!append_layer) index.unique.vsubpstrx <- c("PLT_CN", "CONDID")
        datExportData(vsubpstrx, 
            index.unique = index.unique.vsubpstrx,
            savedata_opts = list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="vsubpstr",
                                outfn.pre=outfn.pre, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                outfn.date=outfn.date, 
                                add_layer=TRUE)) 
      }

      if (savedata && !is.null(invsubpx)) {
        index.unique.invsubpx <- NULL
        if (!append_layer) index.unique.invsubpx <- c("PLT_CN", "CONDID")
        datExportData(invsubpx, 
            index.unique = index.unique.invsubpx,
            savedata_opts = list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="invsubp",
                                outfn.pre=outfn.pre, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                outfn.date=outfn.date, 
                                add_layer=TRUE)) 
      }

      if (savedata && !is.null(subpx)) {
        index.unique.subpx <- NULL
        if (!append_layer) index.unique.subpx <- "PLT_CN"
        datExportData(subpx, 
            index.unique = index.unique.subpx,
            savedata_opts = list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="subplot",
                                outfn.pre=outfn.pre, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                outfn.date=outfn.date, 
                                add_layer=TRUE)) 
        index.unique.subpcx <- NULL
        
        if (!append_layer) index.unique.subpcx <- c("PLT_CN", "CONDID")
        datExportData(subpcx, 
            index.unique = index.unique.subpcx,
            savedata_opts = list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="subp_cond",
                                outfn.pre=outfn.pre, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                outfn.date=outfn.date, 
                                add_layer=TRUE)) 
      } 
 
      if (savedata && !is.null(dwmx)) {
        index.unique.dwmx <- NULL
        if (!append_layer) index.unique.dwmx <- c("PLT_CN", "CONDID")
        datExportData(dwmx, 
            index.unique = index.unique.dwmx,
            savedata_opts = list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="dwm_calc",
                                outfn.pre=outfn.pre, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                outfn.date=outfn.date, 
                                add_layer=TRUE)) 
      } 

      if (savedata && !is.null(othertables)) {
        for (j in 1:length(othertables)) {
          othertable <- othertables[j]
          othertablexnm <- paste0("otherx", j)
          othernm <- paste0("other", j)

          if (!is.null(get(othertablexnm))) {
            index.unique.other <- NULL
            if (othertable == "SUBPLOT") {
              index.unique.other <- c("PLT_CN", "SUBP")
            }
            datExportData(get(othertablexnm),
                index.unique = index.unique.other,
                savedata_opts = list(outfolder=outfolder, 
                                    out_fmt=out_fmt, 
                                    out_dsn=out_dsn, 
                                    out_layer=tolower(othertable),
                                    outfn.pre=outfn.pre, 
                                    overwrite_layer=overwrite_layer,
                                    append_layer=append_layer,
                                    outfn.date=outfn.date, 
                                    add_layer=TRUE))
          }
        }
      }  
      if (savedata && savePOP && !is.null(ppsax)) {
        #index.unique.ppsax <- NULL
        #if (i == 1) index.unique.ppsax <- "PLT_CN"
        datExportData(ppsax, 
            savedata_opts=list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="pop_plot_stratum_assgn",
                                outfn.pre=outfn.pre, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                outfn.date=outfn.date, 
                                add_layer=TRUE)) 
      }
    }
    if (returndata) {
      plt <- rbind(plt, pltx)
      cond <- rbind(cond, condx)
    }
    rm(nbrcnd)
    rm(pltcondx)
    rm(treex)
    gc()
  }

  if (parameters) {
    ## OUTPUTS A TEXTFILE OF INPUT PARAMETERS TO OUTFOLDER
    ###########################################################

    params <- formals(DBgetPlots)
    params <- mget(names(params),ifnotfound="NULL",envir=as.environment(-1))

    outparamfn <- paste0("DBgetPlots_parameters_", stabbrfn, "_", 
		format(Sys.time(), "%Y%m%d"))
    if (!overwrite)
      outparamfn <- fileexistsnm(outfolder, outparamfn, "txt")  
    statesout <- toString(paste0("'", params$states, "'"))
    rsout <- toString(paste0("'", params$RS, "'"))
    stateFilter <- ifelse(is.null(params$stateFilter), FALSE, TRUE)

    outfile <- file(paste0(outfolder, "/", outparamfn, ".txt"), "w")
    cat(  "states <- c(", statesout, ")", "\n", 
      "RS <- c(", rsout, ")", "\n", 
      "invtype <- \"", params$invtype, "\"", "\n",
      "evalid <- ", getlistparam(params$evalid), "\n",  
      "evalCur <- ", params$evalCur, "\n",
      "evalEndyr <- ", getlistparam(params$evalEndyr), "\n",
      "evalAll <- ", params$evalAll, "\n",    
      "evalType <- \"", getlistparam(params$evalType), "\"", "\n",
      "measCur <- ", params$measCur, "\n",
      "measEndyr <- ", getlistparam(params$measEndyr), "\n",
      "allyrs <- ", params$allyrs, "\n",
      "invyrs <- ", getlistparam(params$invyrs), "\n",  
      "istree <- ", params$istree, "\n",
      "isseed <- ", params$isseed, "\n",
      "isveg <- ", params$isveg, "\n",
      "issubp <- ", params$issubp, "\n",
      "isdwm <- ", params$isdwm, "\n",
      "issp <- ", params$issp, "\n",
      "spcond <- ", params$spcond, "\n", 
      "spcondid1 <- ", params$spcondid1, "\n",
      "defaultVars <- ", params$defaultVars, "\n",
      "regionVars <- ", params$regionVars, "\n",
      "ACI <- ", params$ACI, "\n",
      "subcycle99 <- ", params$subcycle99, "\n",
      "intensity1 <- ", params$intensity1, "\n",
      "allFilter <- \"", params$xfilters, "\"", "\n",
      "savedata <- ", params$savedata, "\n",
      "saveqry <- ", params$saveqry, "\n",
      "outfolder <- \"", params$outfolder, "\"", "\n",
      "out_dsn <- \"", params$out_dsn, "\"", "\n",
      "gpkg <- ", params$gpkg, "\n",
      "outfn.pre <- \"", params$outfn.pre, "\"", "\n",
      "outfn.date <- ", params$outfn.date, "\n",
      "overwrite <- ", params$overwrite, "\n",
      "savePOP <- ", params$savePOP, "\n",
     "\n",
    file = outfile, sep="")

    cat(  "fiadat <- DBgetPlots(states=states, RS=RS, invtype=invtype, evalid=evalid, 
	evalCur=evalCur, evalEndyr=evalEndyr, evalAll=evalAll, evalType=evalType, 
	measCur=measCur, measEndyr=measEndyr, allyrs=allyrs, invyrs=invyrs, istree=istree, 
	isseed=isseed, isveg=isveg, issubp=issubp, isdwm=isdwm, 
	issp=issp, spcondid1=spcondid1, defaultVars=defaultVars, regionVars=regionVars, 
	ACI=ACI, subcycle99=FALSE, intensity1=TRUE, allFilter=allFilter, 
	savedata=savedata, saveqry=saveqry, parameters=parameters, outfolder=outfolder, 
	out_dsn=out_dsn, gpkg=gkpg, outfn.pre=outfn.pre, outfn.date=outfn.date,
	overwrite=overwrite, savePOP=savePOP)",
    file = outfile, sep="")

    close(outfile)
  }
 
  ## Write out plot/condition counts to comma-delimited file.
  if (savedata) {
    datExportData(pltcnt, 
        savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer="pltcnt",
                            outfn.pre=outfn.pre, 
                            overwrite_layer=overwrite_layer,
                            append_layer=append_layer,
                            outfn.date=outfn.date, 
                            add_layer=TRUE)) 
  }


  ## GENERATE RETURN LIST
  fiadatlst <- list(states=states)

  tabs <- list()
  tabIDs <- list()
  if (returndata) {
    if (!is.null(plt)) {
      nbrplots <- length(unique(plt$CN))
      if (nrow(plt) != nbrplots) warning("plt records are not unique")
      tabs$plt <- setDF(plt) 
      tabIDs$plt <- "CN"
    }
    if (!is.null(cond)) {
      if (!is.null(plt)) {
        if (length(unique(cond$PLT_CN)) != nbrplots)
          warning("number of plots in cond table does not match plt table")
      }
      tabs$cond <- setDF(cond)
      tabIDs$cond <- "PLT_CN"
    }
    notsame <- FALSE
    if (istree && !is.null(tree)) {
      tabs$tree <- setDF(tree)
      tabIDs$tree <- "PLT_CN"
    }
    if (isseed && !is.null(seed)) {
      tabs$seed <- setDF(seed)
      tabIDs$seed <- "PLT_CN"
    }
    if (issccm && !is.null(sccm)) {
      tabs$sccm <- setDF(sccm)
      tabIDs$sccm <- "PLT_CN"
    }
    if (isveg) {
      if (!is.null(vsubpspp)) {
        tabs$vsubpspp <- setDF(vsubpspp)
        tabIDs$vsubpspp <- "PLT_CN"
      }
      if (!is.null(vsubpstr)) {
        tabs$vsubpstr <- setDF(vsubpstr)
        tabIDs$vsubpstr <- "PLT_CN"
      }
      if (!is.null(invsubp)) {
        tabs$invsubp <- setDF(invsubp)
        tabIDs$invsubp <- "PLT_CN"
      }
    }
    if (issubp) {
      if (!is.null(subpx)) {
        tabs$subplot <- setDF(subpx)
        tabIDs$subplot <- "PLT_CN"
      }
      if (!is.null(subpcx)) {
        tabs$subp_cond <- setDF(subpcx)
        tabIDs$subp_cond <- "PLT_CN"
      }
    }
    if (islulc) {
      if (!is.null(lulc)) {
        tabs$lulc <- setDF(lulc)
        tabIDs$lulc <- "PLT_CN"
      }
    }
    if (isdwm) {
      if (!is.null(dwm)) {
        tabs$dwm <- setDF(dwm)
        tabIDs$dwm <- "PLT_CN"
      }     
    }
    if (!is.null(othertables)) {
      for (i in 1:length(othertables)) {
        tabs[[othertables[i]]] <- get(paste0("other", i))
        if ("PLT_CN" %in% names(get(paste0("other", i)))) {
          tabIDs[[othertables[i]]] <- "PLT_CN"
        } else if ("CN" %in% names(get(paste0("other", i)))) {
          tabIDs[[othertables[i]]] <- "CN"
        } else {
          tabIDs[[othertables[i]]] <- NA
        }  
      }   
    }
    fiadatlst$tabs <- tabs
    fiadatlst$tabIDs <- tabIDs

    if (getxy && issp) {
      xycoords <- getcoords(coords)
      if (xymeasCur) {
        #spxyCurnm <- paste0("spxyCur_", coords)
        spxyCurnm <- paste0("xyCur_", coords)
        
        assign(spxyCurnm, 
		        spMakeSpatialPoints(xyplt=get(paste0("xyCur_", coords)), 
		              xvar=xycoords[1], yvar=xycoords[2], 
		              xy.uniqueid="PLT_CN", xy.crs=4269, addxy=TRUE))
        fiadatlst[[spxyCurnm]] <- get(spxyCurnm)
      } else {  
        #spxynm <- paste0("spxy_", coords)
        spxynm <- paste0("xy_", coords)
        assign(spxynm, 
		        spMakeSpatialPoints(xyplt=get(paste0("xy_", coords)), 
		              xvar=xycoords[1], yvar=xycoords[2], 
		              xy.uniqueid="PLT_CN", xy.crs=4269, addxy=TRUE))
        fiadatlst[[spxynm]] <- get(spxynm)
      }
    } else {
      xycoords <- getcoords(coords)
      if (xymeasCur) {
        xyCurnm <- paste0("xyCur_", coords)
        #assign(xyCurnm, get(paste0("xyCur_", coords))) 
        fiadatlst[[xyCurnm]] <- get(paste0("xyCur_", coords))
      } else {
        xynm <- paste0("xy_", coords)
        #assign(xynm, get(paste0("xy_", coords))) 
        fiadatlst[[xynm]] <- get(paste0("xy_", coords))
      }
    }

    if (!is.null(spconddat)) {
      fiadatlst$spconddat <- setDF(spconddatx)
    }
    if (savePOP || (iseval && length(evalidlist) > 1) && !is.null(ppsa)) {
      fiadatlst$pop_plot_stratum_assgn <- setDF(ppsa)
    }
  }
 
  if (length(evalidlist) > 0) fiadatlst$evalid <- evalidlist
  fiadatlst$pltcnt <- pltcnt

  if (!is.null(evalidlist)) {
    evaliddf <- data.frame(do.call(rbind, evalidlist))
    stcds <- pcheck.states(row.names(evaliddf), "VALUE")
    evaliddf <- data.frame(stcds, row.names(evaliddf), evaliddf, row.names=NULL)
    names(evaliddf) <- c("STATECD", "STATE", "EVALID")
    evaliddf <- evaliddf[order(evaliddf$STATECD), ]
    fiadatlst$evalid <- evalidlist

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
  return(fiadat=fiadatlst)
}

