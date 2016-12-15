#include <Rcpp.h>
#include <string>
#include "dechex.h"

using namespace Rcpp;



std::string to_hexstr(int x, int pad)
{
  // returns hex string represenging decimal number x
  std::string out = "";
  char p0 = '0';
  char pA = 'A';
  while (x > 0)
  {
    int r = x % 16;
    if (r < 10) {
      out = (char)(p0 + r) + out;
    } else {
      out = (char)(pA + r - 10) + out;
    }
    x /= 16;
  }

  // pad zeros
  while (out.size() < 4) out = '0' + out;

  return out;
}



/*** R
striprtf:::to_hexstr(50)
*/
