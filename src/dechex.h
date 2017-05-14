#ifndef DECHEXHEADERDEF
#define DECHEXHEADERDEF

#include <Rcpp.h>
#include <string>


using namespace Rcpp;



// [[Rcpp::export]]
std::string to_hexstr(int x, int pad = 4);


// [[Rcpp::export]]
IntegerVector hex_to_int(std::string h, char sep = 'x');


#endif
