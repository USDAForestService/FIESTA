/* Get mean and variance in one pass.
Intended mainly for raster zonal statistics.
Uses Welford's online algorithm (see 
https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance)
Also tracks the min, max and sum.
na.rm and NA return values consistent with the mean() function in R.
Chris Toney, christoney at fs.fed.us */

#include <Rcpp.h> 
// [[Rcpp::plugins(cpp11)]]

class RunningStats {
	bool na_rm;
	unsigned long long count;
	double mean, min, max, sum;
	double M2;

	public:
	RunningStats(bool na_rm_in) : na_rm(na_rm_in), count(0) {}

	void update(const Rcpp::NumericVector& newvalues) {
		R_xlen_t n = newvalues.size();
		for (R_xlen_t i=0; i!=n; ++i) {
			if (na_rm) {
				if (Rcpp::NumericVector::is_na(newvalues[i])) continue;
			}
			++count;
			if (count == 1) {
				mean = min = max = sum = newvalues[i];
				M2 = 0;
			}
			else {
				const double delta = newvalues[i] - mean;
				mean += (delta / count);
				const double delta2 = newvalues[i] - mean;
				M2 += (delta * delta2);
				if (newvalues[i] < min) min = newvalues[i];
				if (newvalues[i] > max) max = newvalues[i];
				sum += newvalues[i];
			}
		}
	}
	
	void reset() {
		count = 0;
	}
	
	unsigned long long get_count() {
		return count;
	}

	double get_mean() {
		if (count > 0) return mean;
		else return NA_REAL;
	}

	// From R help for min/max:
	// "The minimum and maximum of a numeric empty set are ‘+Inf’ and
	// ‘-Inf’ (in this order!) which ensures _transitivity_, e.g.,
	// ‘min(x1, min(x2)) == min(x1, x2)’."
	double get_min() {
		if (Rcpp::NumericVector::is_na(sum)) return NA_REAL;
		if (count > 0) return min;
		else return R_PosInf;
	}
	
	double get_max() {
		if (Rcpp::NumericVector::is_na(sum)) return NA_REAL;
		if (count > 0) return max;
		else return R_NegInf;
	}
	
	double get_sum() {
		if (count > 0) return sum;
		else return 0;
	}
	
	double get_popVar() {
		if (count < 2) return NA_REAL;
		else return (M2 / count);
	}

	double get_sampVar() {
		if (count < 2) return NA_REAL;
		else return (M2 / (count-1));
	}

	double get_popSD() {
		if (count < 2) return NA_REAL;
		else return sqrt(M2 / count);
	}

	double get_sampSD() {
		if (count < 2) return NA_REAL;
		else return sqrt(M2 / (count-1));
	}
};

RCPP_EXPOSED_CLASS(RunningStats)
RCPP_MODULE(mod_running_stats) {

    Rcpp::class_<RunningStats>("RunningStats")

    .constructor<bool>("Calculates mean and variance on a data stream. Initialize with na_rm_in = TRUE or FALSE")

	.method("update", &RunningStats::update, "Add new values from a numeric vector")
    .method("reset", &RunningStats::reset, "Reset the data stream to count = 0")
	.method("get_count", &RunningStats::get_count, "Return the count of values currently in the stream")
	.method("get_mean", &RunningStats::get_mean, "Return the mean of the values currently in the stream")
	.method("get_min", &RunningStats::get_min, "Return the minimum value currently in the stream")
	.method("get_max", &RunningStats::get_max, "Return the maximum value currently in the stream")
	.method("get_sum", &RunningStats::get_sum, "Return the sum of values currently in the stream")
	.method("get_popVar", &RunningStats::get_popVar, "Return the population variance of the values currently in the stream")
	.method("get_sampVar", &RunningStats::get_sampVar, "Return the sample variance of the values currently in the stream")
	.method("get_popSD", &RunningStats::get_popSD, "Return the population standard deviation of the values currently in the stream")
	.method("get_sampSD", &RunningStats::get_sampSD, "Return the sample standard deviation of the values currently in the stream")
    ;
}

