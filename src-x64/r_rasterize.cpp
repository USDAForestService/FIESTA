// Rasterize one polygon
// Chris Toney, christoney at fs.fed.us

#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::plugins(unwindProtect)]]

#include <cstdlib>
#include <algorithm>

// [[Rcpp::export]]
int RasterizePolygon(int nRasterXSize, int nRasterYSize,
					const Rcpp::IntegerVector& ivPartSizes,
					const Rcpp::NumericVector& dvX, 
					const Rcpp::NumericVector& dvY,
					Rcpp::Function fnRasterIO,
					double dBurnValue, Rcpp::String sAttrValue = NA_STRING) {

	if (dvX.size() != dvY.size()) return 1;
	int nCoords = dvX.size();
	int nParts = ivPartSizes.size();

    int *panNodeX = (int *)(std::malloc( sizeof(int) * nCoords ));

	double dminY = dvY[0];
	double dmaxY = dvY[0];
    for (int i = 1; i < nCoords; i++) {
        if (dvY[i] < dminY) dminY = dvY[i];
        if (dvY[i] > dmaxY) dmaxY = dvY[i];
    }
    int minY = (int)(dminY);
    int maxY = (int)(dmaxY);

    if (minY < 0) minY = 0;
    if (maxY >= nRasterYSize) maxY = nRasterYSize - 1;

    const int minX = 0;
    const int maxX = nRasterXSize - 1;
	
    for (int y = minY; y <= maxY; y++) {
		const double dY = y + 0.5;
		std::memset(panNodeX, -1, sizeof(int) * nCoords);
		int nodes = 0;
		int partOffset = 0;
		for (int part = 0; part < nParts; part++) {
			int j = partOffset + ivPartSizes[part] - 1;
			for (int i=partOffset; i<(partOffset + ivPartSizes[part]); i++) {
				if ((dvY[i]<dY && dvY[j]>=dY) || (dvY[j]<dY && dvY[i]>=dY)) {
			  		const double intersectX = (dvX[i]+(dY-dvY[i])/(dvY[j]-dvY[i])*(dvX[j]-dvX[i]));
					panNodeX[nodes++] = std::floor(intersectX + 0.5);
				}
				j=i;
			}
			partOffset += ivPartSizes[part];
		}
		
		std::sort(panNodeX, panNodeX+nodes);

		for (int i=0; i<nodes; i+=2) {
			if (panNodeX[i] >= maxX) break;
			if (panNodeX[i+1] > minX) {
				if (panNodeX[i] < minX) panNodeX[i] = minX;
				if (panNodeX[i+1] > maxX) panNodeX[i+1] = maxX;
			}
			else continue;
			if (panNodeX[i+1] > panNodeX[i]) {
				fnRasterIO(y, panNodeX[i], panNodeX[i+1]-1, dBurnValue, sAttrValue);
			}
		}
	}

	std::free(panNodeX);
	return 0;
}

