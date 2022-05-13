## External data documentation


##### extdata/PB_data/...
Large-scale areal photography interpretation data from the Image-Based Change Estimation (ICE) Project
chg_ag_LUT.csv - ICE look-up table for change agent classes
cover_LUT.csv - ICE look-up table for change agent classes
icepctcover_utco1135.csv - ICE plot-level percentages of land cover, Davis and Salt Lake Counties, Utah
icepltassgn_utco1135.csv - ICE plot-level data, including estimation unit and strata variables, Davis and Salt Lake Counties, Utah
icepnt_utco1135.csv - ICE point-level data (see ref_icepnt R data frame for variable descriptions), Davis and Salt Lake Counties, Utah

strlut_utco1135.csv - Pixel counts by strata (STRATUMCD) and estimation unit (ESTN_UNIT), Davis and Salt Lake Counties, Utah
unitarea_utco1135.csv - Area, in acres, by county estimation unit (ESTN_UNIT), Davis and Salt Lake Counties, Utah

##### extdata/ref_data/...
ref_domain.csv - Condition domain reference table
ref_estvar.csv - Tree estimation variable reference table


##### extdata/sp_data/...
WYbighorn_adminbnd.shp - Polygon shapefile of WY Bighorn National Forest Administrative boundary (USDA 2018a)
WYbighorn_districtbnd.shp - Polygon shapefile of WY Bighorn National Forest District boundaries (USDA 2018b)
                          
WYbighorn_forest_nonforest_250m.tif - Wyoming Bighorn National Forest RasterLayer of defining forest and nonforest land. Based on MODIS-based classified map resampled from 250m to 500m resolution and reclassified from 3 to 2 classes: 1:forest; 2:nonforest. Projected in Albers Conical Equal Area, Datum NAD27 (Ruefenacht et al. 2008). Clipped to extent of WYbighorn_adminbnd.shp
WYbighorn_dem_250m.img" - Erdas Imagine raster of elevation change, in meters (USGS 2017)


##### References:
Ruefenacht, B.; Finco, M.V.; Nelson, M.D.; Czaplewski, R.; Helmer, E.H.; 
Blackard, J. A.; Holden, G.R.; Lister, A.J.; Salajanu, D.; Weyermann, D.; 
Winterberger, K. 2008. Conterminous U.S. and Alaska Forest Type Mapping Using 
Forest Inventory and Analysis Data.  Photogrammetric Engineering &amp; Remote 
Sensing Vol. 74, No. 11, November 2008, pp. 1379-1388.

USDA Forest Service, Automated Lands Program (ALP). 2018a. S_USA.AdministrativeForest (http://data.fs.usda.gov/geodata/edw)

USDA Forest Service, Automated Lands Program (ALP). 2018b. S_USA.RangerDistrict 
(http://data.fs.usda.gov/geodata/edw)

USGS National Elevation Dataset (NED). 2017. Resampled from 30m resolution to 250m. 
Projected in Albers Conical Equal Area, Datum NAD27 (U.S. Geological Survey 2017). 
Clipped to boundary of WYbighorn_adminbnd.shp


