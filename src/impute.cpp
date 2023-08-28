#include <Rcpp.h>
using namespace Rcpp;
using namespace std;
using namespace sugar;

static bool less_than(double u, double v) {  
    if (NumericVector::is_na(u) || traits::is_nan<REALSXP>(u)) {
        return true;
    }
    if (NumericVector::is_na(v) || traits::is_nan<REALSXP>(v)) {
        return false;
    }
    return u < v;
}

// [[Rcpp::export]]
double meanTest(NumericVector x) {
    return mean(x);
}

// [[Rcpp::export]]
double medianC(NumericVector x) {
    return median(x);
}

// [[Rcpp::export]]
double meanC(NumericVector x, bool b = false) {
  int n = x.size();
  int counts = 0;
  double total = 0;

  for(int i = 0; i < n; ++i) {
    if (b == true) {
        if (!NumericVector::is_na(x[i]) && !traits::is_nan<REALSXP>(x[i])) {
            total += x[i];
            counts += 1;
        } 
    } else {
        total += x[i];
        counts += 1;
    }
  }
  return total / counts;
}

// [[Rcpp::export]]
NumericVector pdistC(NumericVector x, NumericVector y) {
  int n = x.size();
  NumericVector out(n);
  for(int i = 0; i < n; ++i) {
    out[i] = sqrt(pow(x[i] - y[i], 2.0));
  }
  return out;
}

// [[Rcpp::export]]
double normC(NumericVector v) {
  int n = v.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += sqrt(pow(v[i], 2.0));
  }
  return sqrt(total);
}