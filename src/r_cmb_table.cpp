// Hash table class for counting unique integer combinations
// Mainly for raster combine
// Chris Toney, christoney at fs.fed.us

#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

#include <string>
#include <unordered_map>

struct cmbKey {
	Rcpp::IntegerVector cmb;

	bool operator==(const cmbKey &other) const {
		for (int i = 0; i < cmb.size(); ++i) {
			if (cmb[i] != other.cmb[i]) return false;
		}
		return true;
	}
};

struct cmbData {
	unsigned long ID;
	unsigned long long count = 0;
};

struct cmbHasher {
	// Boost hash_combine method
	std::size_t operator()(cmbKey const& vec) const {
		std::size_t seed = 0;
		for (int i=0; i < vec.cmb.size(); ++i) {
			seed ^= vec.cmb[i] + 0x9e3779b9 + (seed << 6) + (seed >> 2);
		}
		return seed;
	}
};

class CmbTable {
	unsigned int nKeyLen;
	Rcpp::CharacterVector cvVarNames;
	unsigned long nLastCmbID;

	std::unordered_map<cmbKey, cmbData, cmbHasher> cmb_map;

	public:
	CmbTable(unsigned int keyLen, Rcpp::CharacterVector varNames): 
		nKeyLen(keyLen), cvVarNames(varNames), nLastCmbID(0)  {}

	unsigned long update(Rcpp::IntegerVector ivKey, unsigned long nIncr) {
		// Increment count for existing key
		// or insert new key with count = nIncr

		if (nIncr == 0) return 0;
		cmbKey key;
		key.cmb = ivKey;
		cmbData& cmbdat = cmb_map[key];
		if (cmbdat.count != 0) {
			cmbdat.count += nIncr;
		}
		else {
			cmbdat.count = nIncr;
			cmbdat.ID = ++nLastCmbID;
		}
		
		return cmbdat.ID;
	}

	Rcpp::IntegerVector updateFromMatrix(const Rcpp::IntegerMatrix& imKeys, int nIncr) {
		// int combinations (keys) are in columns of a matrix (nKeyLen = nrow)
		// This is much faster than using apply update on the matrix from R

		// Increment count for existing key,
		// else insert new key with count = nIncr
		// Return a vector of cmb IDs for the columns of the input matrix

		R_xlen_t ncol = imKeys.ncol();
		Rcpp::IntegerVector out(ncol);

		for (R_xlen_t k=0; k!=ncol; ++k) {
			out[k] = update(imKeys.column(k), nIncr);
		}
		return out;
	}

	Rcpp::DataFrame asDataFrame() {
		// Return the table as a data frame.
		// Set up the data in a named list, then create the dataframe from
		// the list.

		Rcpp::IntegerVector ivCmbID(cmb_map.size());
		Rcpp::NumericVector dvCmbCount(cmb_map.size());
		Rcpp::IntegerVector aVec[nKeyLen]; //array of data vectors
		cmbKey key;
		cmbData cmbdat;

		// set table data in vectors
		for(int n=0; n < nKeyLen; ++n) {
			aVec[n] = Rcpp::IntegerVector(cmb_map.size());
		}
		unsigned long this_idx = 0;
		for(auto iter = cmb_map.begin(); iter != cmb_map.end(); ++iter) {
			key = iter->first;
			cmbdat = iter->second;
			ivCmbID[this_idx] = cmbdat.ID;
			// numeric vector to preserve count as long long int
			dvCmbCount[this_idx] = cmbdat.count;
			for(int var=0; var < nKeyLen; ++var) {
				aVec[var][this_idx] = key.cmb[var];
			}
			++this_idx;
		}

		// make the data frame
		Rcpp::List lOut = Rcpp::List::create(Rcpp::Named("cmbid") = ivCmbID,
								 			Rcpp::Named("count") = dvCmbCount);
		for(int n=0; n < nKeyLen; ++n) {
			lOut.push_back(aVec[n], Rcpp::String(cvVarNames[n]));
		}
		
		Rcpp::DataFrame dfOut(lOut);
		return dfOut;
	}

};

RCPP_EXPOSED_CLASS(CmbTable)
RCPP_MODULE(mod_cmb_table) {

    Rcpp::class_<CmbTable>("CmbTable")

    .constructor<unsigned int, Rcpp::CharacterVector>("Sets length of the combination vector and variable names")

    .method("update", &CmbTable::update, "Increment by nIncr if key exists, else insert with count = nIncr")
	.method("updateFromMatrix", &CmbTable::updateFromMatrix, "Increment by nIncr if key exists, else insert with count = nIncr")
    .method("asDataFrame", &CmbTable::asDataFrame, "Returns a dataframe containing the cmb table")
    ;
}

