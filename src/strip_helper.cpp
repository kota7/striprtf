#include <Rcpp.h>
#include <string>
#include <set>
#include <vector>
#include <stack>
#include <cstdio>
#include <ostream>

#include "dict.h"
#include "dechex.h"


using namespace Rcpp;


struct State
{
  int ucskip;
  bool ignorable;
  State()
  {
    ucskip = 1;
    ignorable = false;
  }

  State(int uc, bool ig)
  {
    ucskip = uc;
    ignorable = ig;
  }
};


struct Section
{
  std::string strcode;
  bool toconv;
  Section()
  {
    strcode = "";
    toconv = false;
  }
  Section(std::string s, bool t)
  {
    strcode = s;
    toconv = t;
  }
};





void set_parameters(
    std::set<std::string> &destinations,
    SimpleOrderedDict<std::string> &specialchars,
    CharacterVector dest_names,
    CharacterVector special_keys,
    CharacterVector special_hex)
{
  // destinations
  for (int i = 0; i < dest_names.size(); i++)
    destinations.insert(as<std::string>(dest_names[i]));

  // special chars
  for (int i = 0; i < special_keys.size(); i++)
    specialchars.insert(std::pair<std::string, std::string>(
        as<std::string>(special_keys[i]), as<std::string>(special_hex[i])));

}




void append_out(std::vector<Section> &doc, std::string value, bool toconv)
{
  // debug
  // if (value == "xFEFF") {
  //   stop("It's me!\n");
  // }


  // if vec size is zero, just append
  int N = doc.size();
  if (N == 0) {
    doc.push_back(Section(value, toconv));
    return;
  }

  // if the last element in the toconv_vec is same as toconv,
  // append the value to the last element
  if (doc.back().toconv == toconv) {
    doc.back().strcode += value;
    return;
  }

  // if the toconv is different from the last element, start a new section
  doc.push_back(Section(value, toconv));

}


// [[Rcpp::export]]
List strip_helper(CharacterMatrix match_mat,
                  CharacterVector dest_names,
                  CharacterVector special_keys,
                  CharacterVector special_hex,
                  bool verbose) {
  // helps rtf2text function by handling loop part
  //
  // match_mat:
  //   result of regex. character matrix of size (N, 7).
  //   each row represents a match
  //     col 0: entire match string
  //     col 1: word
  //     col 2: arg
  //     col 3: hex
  //     col 4: char
  //     col 5: brace
  //     col 6: tchar
  // dest_names:
  //   character vector of destination words
  //
  // special_keys, special_hex:
  //   characte vVector of same size, which match the
  //   special words to the hex string to replace
  //
  // returns a list of two vectors of the same length
  //   - strcode : character vector of hex codes, in the form
  //               e.g, x0010x3010...
  //   - toconv  : logocal vector indicating
  //               whether the codes should be converted using the cp tables.
  //

  // make sure that the special_keys and special_hex have the same size
  if (special_keys.size() != special_hex.size())
    stop("special keys and values have different length");

  std::set<std::string> destinations;
  SimpleOrderedDict<std::string> specialchars;
  set_parameters(destinations, specialchars,
                 dest_names, special_keys, special_hex);


  // debug
  // Rcout << "* special chars *\n";
  // for (unsigned int i = 0; i < specialchars.size(); i++)
  //   Rcout << "  " << specialchars.keys[i] << " = " <<
  //     specialchars.values[i] << "\n";
  // Rcout << "* destinations *\n";
  // for (std::set<std::string>::iterator it = destinations.begin();
  //      it != destinations.end(); ++it)
  //   Rcout << "  " << *it << "\n";

  // for (int i = 0; i < special_keys.size(); i++)
  //   Rcout << special_keys[i] << " " <<
  //     specialchars.haskey(as<std::string>(special_keys[i])) << "\n";
  // for (int i = 0; i < specialchars.size(); i++)
  //   Rcout << specialchars.keys[i] << "=" <<
  //     specialchars.values[i] << "\n";



  std::vector<Section> doc;
  std::stack<State> state_stack;
  bool ignorable = false;
  int ucskip = 1;
  int curskip = 0;
  std::string curhex = "";

  int N = match_mat.nrow();
  //Rcout << N << "rows\n";
  if (match_mat.ncol() < 7) stop("match_mat must have 7 columns");

  // initialize paramters for verbose mode
  int max_bars = 50;
  int cur_bars = 0;
  std::string bar = "";
  std::string emp(max_bars, ' ');
  for (int i = 0; i < N; i++)
  {
    // progress report
    if (verbose) {
      if (max_bars * (i+1) / N > cur_bars) {
        cur_bars = max_bars * (i+1) / N;
        bar.push_back('=');
        emp.erase(0,1);
      }
      int pct = (i+1)*100/N;
      Rcout << "\r[" + bar + emp + "]" << pct << "%";// << std::flush;
    }



    std::string word  = as<std::string>(match_mat(i,1));
    std::string arg   = as<std::string>(match_mat(i,2));
    std::string hex   = as<std::string>(match_mat(i,3));
    std::string cha   = as<std::string>(match_mat(i,4));
    std::string brace = as<std::string>(match_mat(i,5));
    std::string tchar = as<std::string>(match_mat(i,6));

    // Rcout << i << ": " << word << " " << arg << " " << hex << " " << cha << " " <<
    //   brace << " " << tchar << "\n";

    if (curhex.size() > 0 && (hex == "" || curhex.size() == 4)) {
      // make sure the length is 4
      while (curhex.size() < 4) curhex = '0' + curhex;
      append_out(doc, 'x' + curhex, true);
      curhex = "";
    }


    if (brace != "") {
      curskip = 0;
      if (brace == "{") {
        state_stack.push(State(ucskip, ignorable));
      } else if (brace == "}") {
        ucskip = state_stack.top().ucskip;
        ignorable = state_stack.top().ignorable;
        state_stack.pop();
      }
    } else if (cha != "") {
      curskip = 0;
      if (cha == "~") {
        if (!ignorable) append_out(doc, "x00A0", false);
      } else if (cha == "{") {
        if (!ignorable) append_out(doc, "x007b", false);
      } else if (cha == "}") {
        if (!ignorable) append_out(doc, "x007d", false);
      } else if (cha == "\\") {
        if (!ignorable) append_out(doc, "x005c", false);
      } else if (cha == "*") {
        ignorable = true;
      }
    } else if (word != "") {
      //Rcout << word << "...\n";
      curskip = 0;
      if (destinations.find(word) != destinations.end()) {
        ignorable = true;
      } else if (ignorable) {
        continue;
      } else if (specialchars.haskey(word)) {
        append_out(doc, specialchars.getvalue(word), false);
      } else if (word == "uc") {
        int n;
        std::istringstream(arg) >> n;
        ucskip = n;
      } else if (word == "u") {
        int n;
        std::istringstream(arg) >> n;
        if (n < 0) n += 0x10000;
        append_out(doc, 'x' + to_hexstr(n), false);
        curskip = ucskip;
      }
    } else if (hex != "") {
      if (curskip > 0) {
        curskip--;
      } else if (!ignorable) {
        curhex += hex;
      }
    } else if (tchar != "") {
      if (curskip > 0) {
        curskip--;
      } else if (!ignorable) {
        for (std::string::iterator it = tchar.begin(); it != tchar.end(); ++it)
        {
          int n = *it;
          append_out(doc, 'x' + to_hexstr(n), false);
        }
      }
    }
  }
  if (verbose) Rcout << "\n";

  // compile output
  CharacterVector str_vec;
  LogicalVector   toconv_vec;
  for (unsigned int i = 0; i < doc.size(); i++)
  {
    str_vec.push_back(doc[i].strcode);
    toconv_vec.push_back(doc[i].toconv);
  }
  List out = List::create(Named("strcode") = str_vec, Named("toconv") = toconv_vec);
  return out;

}



/*** R

*/
