\name{DBgetPlots}
\alias{DBgetPlots}
\title{
	Database - Extracts inventory plot data from FIA database.
}
\description{
	Extracts field, calculated, and derived plot data from FIA's internal, permission-
	controlled, Oracle database, or from FIA's publically-accessible online 
	DataMart (https://apps.fs.usda.gov/fia/datamart/datamart.html), in the form 
	of comma-separated values (CSV) files or compresssed comma-delimited files (*.zip).
}
\usage{
DBgetPlots(datsource = "ORACLE", ZIP = TRUE, FS_FIADB = TRUE, states = NULL, 
	rs = NULL, invtype = "ANNUAL", evalid = NULL, evalCur = FALSE, 
	evalEndyr = NULL, evalType = "ALL", allyrs = FALSE, invyrs = NULL, 
	actual = FALSE, istree = FALSE, isseed = FALSE, isveg = FALSE, isdwm = FALSE, 
	issp = FALSE, spcoords = NULL, spcond = TRUE, spcondid1 = FALSE, 
	defaultVars = TRUE, regionVars = FALSE, ACI = FALSE, subcycle99 = FALSE, 
	intensity1 = TRUE, allFilter = NULL, savedata = FALSE, saveqry = FALSE, 
	parameters = FALSE, outfolder = NULL, outfn.pre = NULL, outfn.date = FALSE, 
	overwrite = FALSE, dbconn = NULL, dbconnopen = FALSE, returnPOP = FALSE, 
	gui = FALSE)
}
\arguments{
  \item{datsource}{ String. Source of FIA data ('ORACLE', 'CSV'). 
	If datsource="ORACLE", select permission of FIA Oracle tables must be granted to 
	user and an ODBC connection set up on computer. Use DBtestORACLE function to 
	test connection to ORACLE. If datsource="CSV", *.csv or *.zip (if ZIP=TRUE) files 
	are downloaded and extracted from FIA DataMart 
	(http://apps.fs.usda.gov/fia/datamart/datamart.html). If datsource is equal to 
	CSV, the actual coordinates are not available. }
  \item{ZIP}{ Logical. If datsource="CSV". If TRUE, compressed comma-separated values (CSV) 
	files (*.zip) are downloaded from FIA DataMart; saved to a temporary space;
	extracted; and deleted from temporary space. If FALSE, CSV files are directly
	downloaded. }
  \item{FS_FIADB}{ Logical. If datsource="ORACLE". If TRUE, data are extracted from FIA's 
	national public database (Oracle - FS_FIADB schema). If FALSE, data are extracted 
	from the regional FIADB (Oracle FS_NIMS_FIADB_'NIMS_UNIT' schema). }
  \item{states}{ String or numeric vector. Name(s) (e.g., 'Arizona','New Mexico') or 
	code(s) (e.g., 4, 35) of states. }
  \item{rs}{ String vector. Name of research station(s) ('RMRS','SRS','NCRS','NERS','PNWRS'). }
  \item{invtype}{ String. The type of data to extract ('PERIODIC', 'ANNUAL').  
	See further details below. }
  \item{evalid}{ Integer. To extract data for a specific evaluation period. See notes 
	for more information about FIA Evaluations. }
  \item{evalCur}{ Logical. If TRUE, the most current FIA Evalidation for state(s). }
  \item{evalEndyr}{ Integer. The last year of the evaluation period of interest. Selects
	only sampled plots and conditions for the evalidation period ending in the specified 
	year. If more than one state, create a named list object with years labeled
	by state (e.g., list(Utah=2014, Colorado=2013). }
  \item{evalType}{ String vector. The type(s) of evaluation of interest 
	('ALL', 'AREAVOL', 'GRM', 'P2VEG', 'DWM', 'INV', 'REGEN', 'CRWN'). 
	The evalType 'ALL' includes nonsampled plots; 'AREAVOL' includes plots used
	for area or tree estimates (eval_typ %in% c(CURR, VOL)); The evalType 'GRM'
	includes plots used for growth, removals, mortality, and change estimates
	(eval_typ %in% c(GROW, MORT, REMV, CHNG)). Multiple types are accepted. 
	See FIA database manual for regional availability and/or differences. }
  \item{allyrs}{ Logical. If TRUE, all annual inventory years in database are returned for 
	each state. }
  \item{invyrs}{ Integer vector. Inventory year(s) (ex. c(2000, 2001, 2002)). If more than 
	one state, a named list object with years labeled by state 
	(ex. list(Utah=2000:2009, Colorado=c(2002,2003,2005)). }
  \item{actual}{ Logical. If TRUE, actual coordinates and actual variables are output to two 
	separate dataframe objects: actualp (plot-level variables) and actualc (condition-level
	variables). }
  \item{istree}{ Logical. If TRUE, tree data are extracted from TREE table in database. }
  \item{isseed}{ Logical. If TRUE, seedling data are extracted from SEEDLING table in 
	database. }
  \item{isveg}{ Logical. If TRUE, understory vegetation data are extracted from NIMS 
	database (3 tables: P2VEG_SUBPLOT_SPP, P2VEG_SUBP_STRUCTURE). }
  \item{isdwm}{ Logical. If TRUE, summarized down woody debris data are extracted from 
	FIA database (COND_DWM_CALC). }
  \item{issp}{ Logical. If TRUE, a SpatialPointsDataFrame object is generated from the 
	plot-level table. A few condition-level attributes (i.e. FORTYPCD) are included for 
	visual purposes only (See Notes for more info on how condition attributes were added).
	IF savedata=TRUE, an ArcGIS shapefile is output to folder with variable names with 
	variable names 10 characters or less. }
  \item{spcond}{ Logical. If TRUE, add condition information to spatial file. }
  \item{spcoords}{ String vector. If issp=TRUE, the type(s) of coordinates to use for 
	generating SpatialPoints ("ACTUAL", "PUBLIC"). If actual=FALSE, spcoords 
	automatically set to "PUBLIC". Note: If ACTUAL you must set actual=TRUE. More than 
	one option is allowed. }
  \item{spcondid1}{ Logical. If TRUE and issp=TRUE, condition variables
	are determined by condition 1 attributes. If FALSE, an algorithm is used to favor 
	plots with a forest condition (see details). }
  \item{defaultVars}{ Logical. If TRUE, the default variables are selected in query. 
	See notes for variable descriptions. }
  \item{regionVars}{ Logical. If TRUE, regional variables are included in query 
	(e.g., SDI_RMRS, SDIPCT_RMRS, SDIMAX_RMRS, QMD_RMRS). }
  \item{ACI}{ Logical. If TRUE, the data from All Condition Inventories (ACI) 
	are included in dataset (NF_SAMPLING_STATUS_CD = 1). See below for more details. }
  \item{subcycle99}{ Logical. If TRUE, excludes plots with SUBCYCLE = 99. These plots 
	are plots that are measured more than once and are not included in the estimation 
	process. }
  \item{intensity1}{ Logical. If TRUE, includes only plots where INTENSITY = 1. }
  \item{allFilter}{ String. An overall filter for data set (i.e. all states). The expression 
	must be R syntax. If no filter, allFilter="NONE". }
  \item{savedata}{ Logical. If TRUE, saves data to outfolder as comma-delimited file (*.csv). 
	No objects are returned. If FALSE, the data are saved as R objects and returned to user. 
	See details for caveats. }
  \item{saveqry}{ Logical. If TRUE, saves queries to outfolder as text files (*.txt). 
	Default name is: *qry_ 'invtypenm' _ 'stabbr' _ 'date' .txt }
  \item{parameters}{ Logical. If TRUE, saves file of input parameters to outfolder. }
  \item{outfolder}{ String. The output folder path. If NULL and savedata=TRUE or parameters=TRUE 
	or isshp=TRUE, outfolder is the working directory. }
  \item{outfn.pre}{ String. The name used for prefix of outfiles (e.g., outfn.pre'_plt*'.) }
  \item{outfn.date}{ Logical. If TRUE, add date to end of outfile (e.g., outfn_'date'.csv). }
  \item{overwrite}{ Logical. If TRUE and exportshp=TRUE, overwrite files in outfolder. }
  \item{dbconn}{ Database connection. The connection information to the FIA Oracle database. }
  \item{dbconnopen}{ Logical. If TRUE, the dbconn connection is not closed. }
  \item{returnPOP}{ Logical. If TRUE, and using evaluations and datsource='CSV', return
	the POP_PLOT_STRATUM_ASSGN table. }
  \item{gui}{ Logical. If TRUE, gui windows pop up for parameter selection. }
}
\details{

	If no parameters are included, the user is prompted for input. If partial parameters, 
	the default parameter values are used for those not specified. This function will 
	not function without a working ODBC connection to FIA's Oracle database or


	\bold{Data Access}

	Access to Oracle is only available through FIA's security policies and select 
	permission must be granted to query Oracle's tables. Contact your local Oracle 
	database manager to grant access. See details for more information.

	Because of FIA's confidentiality agreement to protect the privacy of landowners 
	as well as protecting the scientific integrity of FIA's sample design, the exact 
	coordinates of the sample plot locations are not included in the publically-
	available databases. The LON/LAT coordinates for download are perturbed up to a mile
	from the original location (https://www.fia.fs.fed.us/tools-data/spatial/Policy/index.php). 
	If the exact location of the plots are necessary for your analysis, contact FIA's 
	Spatial Data Services (https://www.fia.fs.fed.us/tools-data/spatial/index.php).

 	Querying from Oracle is currently only functional from the Cran R, 32-bit version, 
	using the DBI package with ODBC (Open DataBase Connectivity) compliancy. ODBC 
	refers to a database driver on the client computer which translates queries from 
	client applications into commands the database understands. Check you local IT 
	service for compliancy and setup. See details for more information.	

	For datsource = "ORACLE", you must have access to the FIA Oracle database and select 
	permission for the required tables listed below. 

	Required FIA tables:\cr
	\tabular{ll}{
		\tab PLOT, COND, SURVEY, POP_EVAL \cr
		\tab POP_PLOT_STRATUM_ASSGN (if evalCur or evalEndyr != NULL) \cr
		\tab TREE (if istree=TRUE) \cr 
		\tab SEEDLING (if isseed=TRUE) \cr 
		\tab P2VEG_SUBP_STRUCTURE, P2VEG_SUBPLOT_SPP  (if isveg=TRUE) \cr
		\tab SDS_PLOT, SDS_COND  (if actual=TRUE) \cr
		\tab NIMS_BASE_PLOT  (if shpcoords="DIGITIZED") \cr
		\tab NIMS_PLOT_'unit', NIMS_COND_'unit', NIMS_COND_'unit' 
			(if regionVars=TRUE and/or OWL=TRUE) \cr
	}


	ODBC driver
	For datsource = "ORACLE" you must have the required ODBC driver 
	(ex. Oracle in OraClient11g_home1) configured with the correct Transparent Network 
	Substrate (TNS) Service Name (ex. FIADBA) and the correct Oracle Data Source Name (DSN) 
	(ex. IDBFIA.MCI). Use DBtestOracle() to test your connection to the Oracle database. 
	See your local Oracle administrator if connection is unsuccessful. 


	\bold{Inventory type (invtype)}
	If more than one inventory type (PERIODIC/ANNUAL) is desired, extract data one at a time.


	\bold{TPA}
	If TPA=TRUE and istree=TRUE or isseed=TRUE, the following tree/seedling variables are 
		multiplied by trees-per-acre (TPA_UNADJ). TPA_UNADJ is set to a constant derived 
		from the plot size and equals 6.018046 for trees sampled on subplots, 
		74.965282 for trees sampled on microplots, and 0.999188 for trees sampled on 
		macroplots. Variable-radius plots were often used in earlier inventories, 
		so the value in TPA_UNADJ decreases as the tree diameter increases (FIADB User Guide) 

	Variables:
  	VOLCFNET, VOLCFGRS, GROWCFGS, GROWCFAL, FGROWCFGS, FGROWCFAL, MORTCFGS, MORTCFAL, 
	FMORTCFGS, FMORTCFAL, REMVCFGS, REMVCFAL, FREMVCFGS, FREMVCFAL, DRYBIO_BOLE, 
	DRYBIO_STUMP, DRYBIO_TOP, DRYBIO_SAPLING, DRYBIO_WDLD_SPP, DRYBIO_BG, CARBON_BG, 
	CARBON_AG


	\bold{Saving data}
	If savedata=FALSE, data are returned as R objects from function. Some caveats follow 
	to deal with current limitations of R object size and memory issues within 32-bit R 
	(ex. requests involving multiple states): \cr 

	If istree=TRUE, and more than 1 state is requested, the tree data are saved 
	to outfolder, with one file per state, and no tree data are returned. 

	If greater than 6 states are requested at one time, all tables and shapefiles
	are saved to outfolder, with one file per state, and no tables or shapefiles 
	are returned.
			
}
\value{
  fiadat - a list of the following objects:
  \item{states}{ Vector. The states of interest (full state names: Arizona). }
  \item{plt}{ Data frame. Plot variables from PLOT table. See ref_plt for variable 
	definitions. }
  \item{cond}{ Data frame. Cond variables from COND table. Condition table. 
	See ref_cond for variable definitions. }
  \item{actualc}{ Data frame. If actual=TRUE, actual condition-level variables. 
		See ref_actualc for variable definitions. }
  \item{actualp}{ Data frame. If actual=TRUE, actual plot-variables.
		See ref_actualp for variable definitions. }
  \item{tree}{ Data frame. If istree=TRUE, tree variables from TREE table. 
		See ref_tree for variable definitions. }
  \item{seed}{ Data frame. If isseed=TRUE, seedling variables from SEEDLING table. }
  \item{spplt_*}{ S4 SpatialPointsDataFrame object. If isshp=TRUE, a SpatialPoints object 
		with plot-level data and some condition attributes represented at plot-level. 
		(See Notes for more info on SpatialPoints attributes). } 
  \item{vspspp}{ Data frame. If isveg=TRUE, Subplot-level understory vegetation 
		variables from P2VEG_SUBPLOT_SPP table. }
  \item{vspstr}{ Data frame. If isveg=TRUE, Subplot-level cover and layer understory 
		vegetation variables from P2VEG_SUBP_STRUCTURE table. }
  \item{evalid}{ Number. If evalCur=TRUE or evalEndyr is not NULL, the Evalidation 
		ID is output. }
  \item{pltcnt}{ Data frame. Number of plots (NBRPLOTS) by state, cycle, inventory year,
		and plot status. }
  \item{dbconn}{ ODBC database connection. If datsource=ORACLE and dbconnopen=TRUE, 
		the open database connection. }
  \item{POP_PLOT_STRATUM_ASSGN}{ Data frame. If returnPOP=TRUE, and Evaluations are
		used to extract data from database, return the POP_PLOT_STRATUM_ASSGN table. }

  
  Outputs to outfolder (if savedata=TRUE):
	\tabular{ll}{
	\tab	- If saveqry=TRUE, text file(s) of SQL queries used to extract data from database (*.txt).
			Note: the plot and cond table is 1 query (pltcondqry*.txt). \cr
	\tab	- If parameters=TRUE, text file of parameters used. This file can be used 
			to run program again (DBgetPlots_parameters_'date'.txt).\cr
	\tab	- Text file of plot and condition counts (pltcnt_*'date'.txt).\cr
	\tab	- CSV files of data frame objects (*'date'.csv).\cr
	\tab	- If isshp=TRUE, an ESRI shapefile of plot-level and some condition-level 
			attributes (shp_'shpcooords'_'date'.shp). Variable names are truncated to 10
			characters or less. \cr
	\tab	- If isshp=TRUE, a CSV file of the SpatialPoints data frame (shpdat_'shpcooords'_'date'.csv).\cr
	\tab	- If isshp=TRUE, a CSV file of truncated names for conversion to
			shapefile (shp_'shpcooords'_'date'_newnames.shp). See notes for more info.\cr
	}
}
\author{
	Tracey S. Frescino
}
\note{
	\bold{FIA forest land definition:}

	\bold{Current:} 
	Forested plots include plots with >= 10 percent cover (or equivalent stocking) by 
	live trees of any size, including land that formerly had such tree cover and that will 
	be naturally or artificially regenerated. To qualify, the area must be >= 1.0 acre in 
	size and 120.0 feet wide (See Woudenberg et al. 2011).

	\bold{OWL (Other Wooded Land):}
	RMRS only. Reflects the past definition of forest land. For the periodic inventory and 
	annual inventories up to 2012, the forest land definition included plots >= 5 percent 
	cover (or equivalent stocking) by live trees of any size, including.... 
	(DeBlander et al. 2010). Starting in 2013 (MANUAL 6.0), no OWL data collected.

	\bold{ACI (All Condition Inventory):}
	RMRS National Forest plots. For nonforest conditions that have been visited in the 
	field (NF_SAMPLING_STATUS_CD = 1), if trees exist on the condition, the data exist 
	in the tree table. If you do not want these trees included, ACI=FALSE.



	\bold{FIA ORACLE SCHEMAS}

	This function extracts data from five different Oracle schemas: 
	\tabular{llll}{
		\tab \bold{Database}	\tab \bold{Schema Name}	\tab \bold{Type of Data}\cr
	 	\tab FIADB national	\tab FS_FIADB/FS_FIADB	\tab	Public data (FIA DataMart)\cr
	 	\tab FIADB regional	\tab FS_FIADB/FS_NIMS_FIADB_RMRS	\tab	Public data\cr
	 	\tab NIMS			\tab FS_NIMS_RMRS		\tab	Annual regional and OWL data\cr
		\tab FIADB1.0		\tab FS_RMRS_FIA_FIADB	\tab	Periodic regional data\cr
		\tab FIADB1.0 Prefield	\tab FS_RMRS_FIA_FIELD	\tab	Prefield ownership data\cr
		\tab FIADB1.0 MPC		\tab FS_RMRS_FIA_MPC	\tab	Prefield ownership levels\cr	
	}

	FIADB public schema or data available at FIA DataMart has all data accessible to the public. 
	This includes the following details:
	\tabular{ll}{
		\tab	- the PLOT variable is renumbered.\cr
		\tab	- the LON/LAT coordinates are fuzzed & swapped.\cr
		\tab	- the OWNERCD variable is based on fuzzed & swapped locations.\cr
		\tab	- ECOSUBCD, CONGCD, ELEV, and EMAP_HEX are GIS-extracted values based on 
				fuzzed & swapped locations.\cr
		\tab	- For annual data, forested plots represent the current definition 
				of >= 10 percent cover...\cr
		\tab	- For periodic data, forested plots are defined by the past, OWL definition, 
				of >= 5 percent cover...\cr
	}

	NIMS schema is FIA's working environment. It contains all data in the FIADB public 
		schema plus regional and OWL data. 
	\tabular{ll}{
		\tab	- the PLOT variable is the actual PLOT number (=to PLOT_PERIODIC).\cr
		\tab	- the LON/LAT coordinates are fuzzed & swapped.\cr
		\tab	- the OWNERCD variable is the actual OWNERCD.\cr
		\tab	- For annual data, forested plots are defined by the past, OWL definition, 
				of >= 5 percent cover...\cr
		\tab	- For periodic data, forested plots are defined by the past, OWL definition, 
				of >= 5 percent cover...\cr
	}

	FIADB1.0 schema is the old FIADB database. This database includes the periodic regional data.
	\tabular{ll}{
		\tab	- the PLOT variable is the actual PLOT number.\cr
		\tab	- the LON/LAT coordinates are actual LON/LAT values.\cr
		\tab	- the OWNERCD variable is the actual OWNERCD.\cr
		\tab	- For periodic data, forested plots are defined by the past, OWL definition, 
			of >= 5 percent cover...\cr
	}

	FIADB1.0 Prefield and MPC schema is the old FIADB database that contains ownership 
	information for prefield. This table is used temporarily until new ownership table 
	is developed in NIMS. The prefield ownership calls may be different then the NIMS 
	database ownership calls because of changes from field, therefore this table should 
	be used with caution.


	\bold{FIA Evaluations}

	An evaluation is a group of plots within the FIA database that is used for generating 
	population estimates, representing different inventory spans of data with different 
	stratification or area adjustments. Each evaluation is determined by the type of 
	estimation (evalType) including: area and tree estimates; growth, removal, and 
	mortality estimates; and area change estimates (EVAL_TYPE). These plots are identified 
	by an evalid, which is a unique identifier in the format of a 2-digit State code, 
	a 2-digit year code, and a 2-digit evaluation type code. For example, EVALID '491601' 
	represents the Utah 2016 evaluation for current area estimates. 


	\bold{FIA Evaluation Types}

	If evaltype="ALL", the evaluation includes all sampled and nonsampled plots or 
	plots that were missed in an inventory year. Regional differences may occur on how
	missed plots are represented in an evaluation. For example, in the RMRS Research 
	Station, evaluations in the database are static; missed plots are added as 
	nonsampled plots to past evaluations, but are included in new evaluations; therefore, 
	the number of nonsampled plots may change for old evaluations, depending on when a 
	missed plot is measured. In the PNW Research Station, plots are brought forward
	to replace missed plots in an evaluation, depending on the evalType.


	\bold{SHAPEFILE}

	If isshp=TRUE, a SpatialPointsDataFrame object of plot-level attributes with some 
	condition-level attributes is generated and, if savedata=TRUE, is exported to the 
	outfolder using the ESRI Shapefile driver. The driver truncates variable names to 10 
	characters or less. Variable names are changed using an internal function. The 
	name changes are defined in a file written to the outfolder, shpfile_newnames.csv.

	The shapefile will have a defined projection of Geographic, NAD83 datum.

	Because there may be more than one condition per plot, for spatial purposes, 
	condition-level attributes are selected based on CONDID=1, if CONDID1=TRUE, or the 
	following selection criteria (if the variable exists in the dataset). A column named 
	CONDMETHOD is added to the shapefile's attribute table to show the steps that were used, 
	based on the following abbreviation in parentheses. 

	\tabular{ll}{
		\tab	(1) minimum COND_STATUS_CD (_ST)\cr
		\tab	(2) maximum condition proportion (_CP)\cr
		\tab	(3) maximum live_canopy_cvr_pct (_CC)\cr
		\tab	(4) minimum STDSZCD (_SZ)\cr
		\tab	(5) minimum CONDID (_C1)\cr 
	}

	

	\bold{DERIVED VARIABLES}
	
	These variables are not in the NIMS database and are calculated after the query. 
	You can filter the plot/condition tables based on the plot/cond-level variables, 
	but they are not included in the output query text file. These variables are only
	calculated when defaultVars=TRUE.


	Plot-level variables:\cr	
	\tabular{ll}{
	\tab NBRCND - The number of conditions on plot, including nonsampled conditions 
			(COND_STATUS_CD = 5)\cr 
	\tab NBRCNDSAMP - The number of sampled conditions on plot, excluding nonresponse.\cr
	\tab NBRCNDFOR - The number of sampled forested conditions on plot.\cr
	\tab NBRCNDFTYP - The number of sampled forested conditions with different forest types on plot.\cr
	\tab NBRCNDFGRP - The number of sampled forested conditions with different forest type groups on plot.\cr
	\tab CCLIVEPLT - Percent live canopy cover of condition aggregated to plot-level 
			(LIVE_CANOPY_CVR_PCT * CONDPROP_UNADJ).\cr
	\tab UNIQUEID - Unique Identifier for plot location 
			('Z' + STATECD(2) + UNITCD(2) + COUNTYCD(3) + PLOT(5)).
			This variable can be used to identify multiple plots per location.\cr
	}

	Condition-level variables:\cr
	\tabular{ll}{
	\tab FORTYPGRP - TYPGRPCD merged to FORTYPCD\cr
	\tab FLDTYPGRP - TYPGRPCD merged to FLDTYPCD\cr
	\tab FORNONSAMP - Combination of PLOT_STATUS_CD and PLOT_NONSAMPLE_REASN_CD\cr
	\tab QMD - Quadratic Mean Diameter\cr
	}

	Tree-level variables:\cr
	\tabular{ll}{
	\tab BA - the basal area of a tree (BA = DIA * DIA * 0.005454).\cr
  	}
	\tabular{ll}{
 	\tab TREEAGE - the average age of live trees. (TREEAGE = BHAGE, for timber species, 
		and TOTAGE, for woodland species or timber species < 5.0" DIA).\cr
	}    

 
	\tabular{ll}{
	\tab TREEAGE Notes:\cr
	\tab - Available for live timber and woodland trees in the following states: AZ,CO,ID,MT,NV,UT,OR,WA.\cr
	\tab - BHAGE - Breast height age (4.5' above ground) of timber trees.\cr
	\tab - PNW - one tree is sampled for each species, within each crown class, and for each 
			condition class present on plot. Age of saplings (<5.0" DIA) may be aged by 
			counting branch whorls above 4.5ft. No timber hardwood species other than 
			red alder are bored for age.\cr
	\tab - RMRS - one tree is sampled for each species and broad diameter class present on plot.\cr
	\tab - TOTAGE - The total age of a live tree derived either from increment core at DRC, 
			or for small samplings (1.0-2.9"), by counting all branch whorls, or by adding 
			a species-dependent number of years to breast height age.\cr 
	}

	\tabular{ll}{
	\tab DRYBIO_AG - Aboveground oven-dry biomass, in pounds 
		(DRYBIO_AG = (DRYBIO_BOLE + DRYBIO_STUMP + DRYBIO_TOP + DRYBIO_SAPLING + DRYBIO_WDLD_SPP).\cr
	}
		
	\tabular{ll}{
	\tab DRYBIO_AG Notes:\cr
	\tab - Available for both timber and woodland species, live trees >= 1.0" DIA and dead 
		trees >= 5.0" DIA. Summed dry biomass of the top, bole, and stump of a tree,
 		excluding foliage based on component ratio method (Heath and others, 2009).\cr
	\tab - DRYBIO_BOLE - dry biomass of sound wood in live and dead trees, including bark, 
		from a 1-foot stump to a min 4-inch top DIA of central stem (Calculated for 
		timber trees >= 5.0" DIA).\cr
	\tab - DRYBIO_STUMP - dry biomass in the tree stump, including the portion of the tree 
		from the ground to the bottom of merchantable bole, 1-foot (Calculated for 
		live and dead trees >= 5.0" DIA).\cr
	\tab - DRYBIO_TOP - dry biomass in the top of the tree, including the portion of the 
		tree above merchantable bole, 4-inch top, and all branches, excludes foliage 
		(Calculated for live and dead trees >= 5.0" DIA).\cr
	\tab - DRYBIO_SAPLING - dry biomass of saplings, including aboveground portion, 
		excluding foliage, of live timber trees >=1.0" and <5.0" DIA.\cr
	\tab - DRYBIO_WDLD_SPP - dry biomass of woodland trees, live or dead, including the 
		aboveground portion, excluding foliage, the top of the tree above 1.5" DIA, 
		and a portion of the stump from ground to DRC (Calculated for woodland 
		trees >= 1.0" DIA.\cr
	}

	ABOVEGROUND CARBON ESTIMATES (IN POUNDS)\cr
		Available for both timber and woodland species, live trees >= 1.0" DIA and 
			dead trees >= 5.0" DIA.
		Calculated as 1/2 of the aboveground esimates of biomass: \cr
		CARBON_AG = 0.5 * (DRYBIO_BOLE + DRYBIO_STUMP + DRYBIO_TOP + DRYBIO_SAPLING + DRYBIO_WDLD_SPP)


	TREEAGE DATA ONLY IN FOR ("AZ", "CO", "ID", "MT", "NV", "UT") \cr
	FMORTCFAL includes trees >= 5.0" DIA and greater and is not populated for 
		states("CA", "OR", "WA", "OK") \cr
	Mortality variables only available in:  AZ, CO, ID, MT, NV, NM, UT, WY, ND, SD, NE, KS, OK.


	\bold{MISC}

	For regions outside RMRS, there is no OWNCD attached to nonforest lands. 


	Example: \cr
	dat <- DBgetPlots(datsource="CSV", states="Utah", invyrs=c(2002:2006), 
		istree=TRUE, isshp=TRUE, allFilter="COUNTYCD == 41")
	


}

\examples{

  \dontrun{

  # Most current evaluation and shapefile with public coordinates
  WYdat <- DBgetPlots(datsource="CSV", states="Wyoming", evalCur=TRUE)
  names(WYdat)
  WYplt <- WYdat$plt
  WYcond <- WYdat$cond
  WYdat$evalid
  WYdat$pltcnt

  head(WYcond)
  table(WYplt$INVYR)

  # Inventory years 2012:2014 and shapefile with public coordinates
  WYdat2 <- DBgetPlots(datsource="CSV", states="Wyoming", invyrs=2012:2014)
  names(WYdat2)
  WYplt2 <- WYdat2$plt
  WYcond2 <- WYdat2$cond
  WYdat2$evalid
  WYdat2$pltcnt

  head(WYcond2)
  table(WYplt2$INVYR)
  }

}

\references{
	DeBlander, Larry T.; Shaw, John D.; Witt, Chris; Menlove, Jim; Thompson, Michael T.; 
	Morgan, Todd A.; DeRose, R. Justin; Amacher, Michael, C. 2010. Utah's forest resources, 
	2000-2005. Resour. Bull. RMRS-RB-10. Fort Collins, CO; U.S. Department of Agriculture, 
	Forest Service, Rocky Mountain Research Station. 144 p.

	Heath, L.S.; Hansen, M. H.; Smith, J.E. [and others]. 2009. Investigation into calculating 
	tree biomass and carbon in the FIADB using a biomass expansion factor approach. In: Forest 
	Inventory and Analysis (FIA) Symposium 2008. RMRS-P-56CD. Fort Collins, CO: U.S. Department 
	of Agriculture, Forest Service, Rocky Mountain Research Station. 1 CD.

	O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer, D.; Wilson, A.M.; 
	Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The Forest Inventory and Analysis 
	Database: Database Description and Users Manual Version 5.1.2 for Phase 2. U.S. Department 
	of Agriculture. 
	(http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user manual_5-1-2_p2_07_2012.pdf)
}


\keyword{data}
