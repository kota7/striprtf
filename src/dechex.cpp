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


IntegerVector hex_to_int(std::string h, char sep)
{
  // convert hex string into integer vector
  //
  // args:
  //   h:   hex string in the form e.g., x000AxCA01 ...
  //   sep: char separating codes in h, if x001Ax0408, ..., then 'x'
  //
  // returns:
  //   integer vector

  h += sep; // make sure the last hex is read

  IntegerVector out;
  bool started = false;
  for (int i = 0; i < h.size(); i++)
  {
    int start;
    if (h[i] == sep) {
      if (!started) {
        start = i + 1;
        started = true;
      } else {
        int end = i;

        int tmp = 0;
        int base = 1;
        for (int k = end-1; k >= start; k--)
        {
          char c = h[k];
          int n;
          if (c >= '0' && c <= '9')      n = c - '0';
          else if (c >= 'A' && c <= 'F') n = c - 'A' + 10;
          else if (c >= 'a' && c <= 'f') n = c - 'a' + 10;
          else stop("invalid hex");

          tmp += base*n;
          base *= 16;
        }
        out.push_back(tmp);
        start = i + 1;
      }
    }
  }
  return out;
}


/*** R
striprtf:::to_hexstr(50)

h <- "xffffx0101x00a0"
striprtf:::hex_to_int(h)
as.integer(as.hexmode(strsplit(h, 'x')[[1]]))[-1]  # should produce same result
*/
