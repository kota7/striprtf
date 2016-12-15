#include <Rcpp.h>
#include <string>


using namespace Rcpp;



// [[Rcpp::export]]
std::string to_hexstr(int x, int pad = 4);
