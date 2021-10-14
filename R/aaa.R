#' @import Rcpp data.table sf
#' @useDynLib FIESTA
#' @importFrom grDevices colors dev.new dev.off jpeg pdf
#' @importFrom grDevices png postscript rainbow terrain.colors
#' @importFrom grDevices is.raster cm.colors hcl.colors heat.colors topo.colors
#' @importFrom graphics arrows legend mtext par pie plot barplot segments title 
#' @importFrom graphics lines text hist split.screen screen axis abline box close.screen
#' @importFrom stats as.formula na.omit rnorm sd xtabs aggregate 
#' @importFrom stats median var complete.cases ave model.matrix
#' @importFrom utils download.file select.list unzip write.table head 
#' @importFrom utils tail setTxtProgressBar txtProgressBar installed.packages capture.output
#' @importFrom utils choose.files choose.dir 
#' @importFrom methods as new slot canCoerce
#' @importFrom rgdal readOGR GDALinfo GDAL.open GDAL.close getRasterBand 
#' @importFrom rgdal saveDataset getRasterData getDriverName putRasterData getProjectionRef
#' @export RasterizePolygon
NULL