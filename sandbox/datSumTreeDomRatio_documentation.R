#' Data - Generates ratio of tree domain variables.
#' 
#' Generates ratio of tree domain summaries from FIESTA::datSumTreedom().
#' 
#' 
#' @param ndat Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Table from FIESTA::datSumTreeDomRatio() with numerator tree domain
#' variables.
#' @param ddat Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Table from FIESTA::datSumTreeDomRatio() with numerator tree domain
#' variables.
#' @param uniqueid String. The unique identifier of both tables (default =
#' "PLT_CN").
#' @param nprefix String. The prefix variable identifier of numerator domain
#' variables in ndat.
#' @param dprefix String. The prefix variable identifier of denominator domain
#' variables in ddat.
#' @param rprefix String. The prefix variable identifier of new ratio variables
#' (default="r").
#' @param datround Integer. Number of digits to round ratio values to.
#' @param savedata Logical. If TRUE, saves data to outfolder.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. If out_layer = NULL,
#' default = 'tsumrat'. 
#' @param gui Logical. If gui, user is prompted for parameters.
#'
#' @return \item{rdat}{ Data frame. Table with ratio values (ndat / ddat). }
#' \item{rvars}{ String vector. Variable names in rdat. }
#' 
#' If savedata=TRUE, the data table will be saved to the outfolder: \cr
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' # Ratio of live and dead basal area
#' # Live basal area
#' live <- datSumTreeDom(tree = FIESTA::WYtree, 
#'                       tuniqueid = "PLT_CN", 
#'                       tsumvar = "BA", 
#'                       tfilter = "STATUSCD==1")
#'                       
#' # Dead basal area                       
#' dead <- datSumTreeDom(tree = FIESTA::WYtree, 
#'                       tuniqueid = "PLT_CN", 
#'                       tsumvar = "BA", 
#'                       tfilter = "STATUSCD == 2 & STANDING_DEAD_CD == 1")
#' 
#' # Ratio of dead and live basal area                        
#' ratio <- FIESTA:::datSumTreeDomRatio(ndat = dead$tdomdat,
#'                                      ddat = live$tdomdat)    
#' 
#' str(ratio, max.level = 1)
#' 
#' head(ratio$rdat)     