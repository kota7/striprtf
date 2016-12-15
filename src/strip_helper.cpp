#include <Rcpp.h>
#include <string>
#include <set>
#include <vector>
#include <stack>
#include <stdio.h>

#include "dict.h"
#include "dechex.h"
#include "UnicodeToUTF8.h"


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
    SimpleOrderedDict<std::string> &specialchars)
{
  // destinations
  destinations.insert("aftncn");
  destinations.insert("aftnsep");
  destinations.insert("aftnsepc");
  destinations.insert("annotation");
  destinations.insert("atnauthor");
  destinations.insert("atndate");
  destinations.insert("atnicn");
  destinations.insert("atnid");
  destinations.insert("atnparent");
  destinations.insert("atnref");
  destinations.insert("atntime");
  destinations.insert("atrfend");
  destinations.insert("atrfstart");
  destinations.insert("author");
  destinations.insert("background");
  destinations.insert("bkmkend");
  destinations.insert("bkmkstart");
  destinations.insert("blipuid");
  destinations.insert("buptim");
  destinations.insert("category");
  destinations.insert("colorschememapping");
  destinations.insert("colortbl");
  destinations.insert("comment");
  destinations.insert("company");
  destinations.insert("creatim");
  destinations.insert("datafield");
  destinations.insert("datastore");
  destinations.insert("defchp");
  destinations.insert("defpap");
  destinations.insert("do");
  destinations.insert("doccomm");
  destinations.insert("docvar");
  destinations.insert("dptxbxtext");
  destinations.insert("ebcend");
  destinations.insert("ebcstart");
  destinations.insert("factoidname");
  destinations.insert("falt");
  destinations.insert("fchars");
  destinations.insert("ffdeftext");
  destinations.insert("ffentrymcr");
  destinations.insert("ffexitmcr");
  destinations.insert("ffformat");
  destinations.insert("ffhelptext");
  destinations.insert("ffl");
  destinations.insert("ffname");
  destinations.insert("ffstattext");
  destinations.insert("field");
  destinations.insert("file");
  destinations.insert("filetbl");
  destinations.insert("fldinst");
  destinations.insert("fldrslt");
  destinations.insert("fldtype");
  destinations.insert("fname");
  destinations.insert("fontemb");
  destinations.insert("fontfile");
  destinations.insert("fonttbl");
  destinations.insert("footer");
  destinations.insert("footerf");
  destinations.insert("footerl");
  destinations.insert("footerr");
  destinations.insert("footnote");
  destinations.insert("formfield");
  destinations.insert("ftncn");
  destinations.insert("ftnsep");
  destinations.insert("ftnsepc");
  destinations.insert("g");
  destinations.insert("generator");
  destinations.insert("gridtbl");
  destinations.insert("header");
  destinations.insert("headerf");
  destinations.insert("headerl");
  destinations.insert("headerr");
  destinations.insert("hl");
  destinations.insert("hlfr");
  destinations.insert("hlinkbase");
  destinations.insert("hlloc");
  destinations.insert("hlsrc");
  destinations.insert("hsv");
  destinations.insert("htmltag");
  destinations.insert("info");
  destinations.insert("keycode");
  destinations.insert("keywords");
  destinations.insert("latentstyles");
  destinations.insert("lchars");
  destinations.insert("levelnumbers");
  destinations.insert("leveltext");
  destinations.insert("lfolevel");
  destinations.insert("linkval");
  destinations.insert("list");
  destinations.insert("listlevel");
  destinations.insert("listname");
  destinations.insert("listoverride");
  destinations.insert("listoverridetable");
  destinations.insert("listpicture");
  destinations.insert("liststylename");
  destinations.insert("listtable");
  destinations.insert("listtext");
  destinations.insert("lsdlockedexcept");
  destinations.insert("macc");
  destinations.insert("maccPr");
  destinations.insert("mailmerge");
  destinations.insert("maln");
  destinations.insert("malnScr");
  destinations.insert("manager");
  destinations.insert("margPr");
  destinations.insert("mbar");
  destinations.insert("mbarPr");
  destinations.insert("mbaseJc");
  destinations.insert("mbegChr");
  destinations.insert("mborderBox");
  destinations.insert("mborderBoxPr");
  destinations.insert("mbox");
  destinations.insert("mboxPr");
  destinations.insert("mchr");
  destinations.insert("mcount");
  destinations.insert("mctrlPr");
  destinations.insert("md");
  destinations.insert("mdeg");
  destinations.insert("mdegHide");
  destinations.insert("mden");
  destinations.insert("mdiff");
  destinations.insert("mdPr");
  destinations.insert("me");
  destinations.insert("mendChr");
  destinations.insert("meqArr");
  destinations.insert("meqArrPr");
  destinations.insert("mf");
  destinations.insert("mfName");
  destinations.insert("mfPr");
  destinations.insert("mfunc");
  destinations.insert("mfuncPr");
  destinations.insert("mgroupChr");
  destinations.insert("mgroupChrPr");
  destinations.insert("mgrow");
  destinations.insert("mhideBot");
  destinations.insert("mhideLeft");
  destinations.insert("mhideRight");
  destinations.insert("mhideTop");
  destinations.insert("mhtmltag");
  destinations.insert("mlim");
  destinations.insert("mlimloc");
  destinations.insert("mlimlow");
  destinations.insert("mlimlowPr");
  destinations.insert("mlimupp");
  destinations.insert("mlimuppPr");
  destinations.insert("mm");
  destinations.insert("mmaddfieldname");
  destinations.insert("mmath");
  destinations.insert("mmathPict");
  destinations.insert("mmathPr");
  destinations.insert("mmaxdist");
  destinations.insert("mmc");
  destinations.insert("mmcJc");
  destinations.insert("mmconnectstr");
  destinations.insert("mmconnectstrdata");
  destinations.insert("mmcPr");
  destinations.insert("mmcs");
  destinations.insert("mmdatasource");
  destinations.insert("mmheadersource");
  destinations.insert("mmmailsubject");
  destinations.insert("mmodso");
  destinations.insert("mmodsofilter");
  destinations.insert("mmodsofldmpdata");
  destinations.insert("mmodsomappedname");
  destinations.insert("mmodsoname");
  destinations.insert("mmodsorecipdata");
  destinations.insert("mmodsosort");
  destinations.insert("mmodsosrc");
  destinations.insert("mmodsotable");
  destinations.insert("mmodsoudl");
  destinations.insert("mmodsoudldata");
  destinations.insert("mmodsouniquetag");
  destinations.insert("mmPr");
  destinations.insert("mmquery");
  destinations.insert("mmr");
  destinations.insert("mnary");
  destinations.insert("mnaryPr");
  destinations.insert("mnoBreak");
  destinations.insert("mnum");
  destinations.insert("mobjDist");
  destinations.insert("moMath");
  destinations.insert("moMathPara");
  destinations.insert("moMathParaPr");
  destinations.insert("mopEmu");
  destinations.insert("mphant");
  destinations.insert("mphantPr");
  destinations.insert("mplcHide");
  destinations.insert("mpos");
  destinations.insert("mr");
  destinations.insert("mrad");
  destinations.insert("mradPr");
  destinations.insert("mrPr");
  destinations.insert("msepChr");
  destinations.insert("mshow");
  destinations.insert("mshp");
  destinations.insert("msPre");
  destinations.insert("msPrePr");
  destinations.insert("msSub");
  destinations.insert("msSubPr");
  destinations.insert("msSubSup");
  destinations.insert("msSubSupPr");
  destinations.insert("msSup");
  destinations.insert("msSupPr");
  destinations.insert("mstrikeBLTR");
  destinations.insert("mstrikeH");
  destinations.insert("mstrikeTLBR");
  destinations.insert("mstrikeV");
  destinations.insert("msub");
  destinations.insert("msubHide");
  destinations.insert("msup");
  destinations.insert("msupHide");
  destinations.insert("mtransp");
  destinations.insert("mtype");
  destinations.insert("mvertJc");
  destinations.insert("mvfmf");
  destinations.insert("mvfml");
  destinations.insert("mvtof");
  destinations.insert("mvtol");
  destinations.insert("mzeroAsc");
  destinations.insert("mzeroDesc");
  destinations.insert("mzeroWid");
  destinations.insert("nesttableprops");
  destinations.insert("nextfile");
  destinations.insert("nonesttables");
  destinations.insert("objalias");
  destinations.insert("objclass");
  destinations.insert("objdata");
  destinations.insert("object");
  destinations.insert("objname");
  destinations.insert("objsect");
  destinations.insert("objtime");
  destinations.insert("oldcprops");
  destinations.insert("oldpprops");
  destinations.insert("oldsprops");
  destinations.insert("oldtprops");
  destinations.insert("oleclsid");
  destinations.insert("operator");
  destinations.insert("panose");
  destinations.insert("password");
  destinations.insert("passwordhash");
  destinations.insert("pgp");
  destinations.insert("pgptbl");
  destinations.insert("picprop");
  destinations.insert("pict");
  destinations.insert("pn");
  destinations.insert("pnseclvl");
  destinations.insert("pntext");
  destinations.insert("pntxta");
  destinations.insert("pntxtb");
  destinations.insert("printim");
  destinations.insert("private");
  destinations.insert("propname");
  destinations.insert("protend");
  destinations.insert("protstart");
  destinations.insert("protusertbl");
  destinations.insert("pxe");
  destinations.insert("result");
  destinations.insert("revtbl");
  destinations.insert("revtim");
  destinations.insert("rsidtbl");
  destinations.insert("rxe");
  destinations.insert("shp");
  destinations.insert("shpgrp");
  destinations.insert("shpinst");
  destinations.insert("shppict");
  destinations.insert("shprslt");
  destinations.insert("shptxt");
  destinations.insert("sn");
  destinations.insert("sp");
  destinations.insert("staticval");
  destinations.insert("stylesheet");
  destinations.insert("subject");
  destinations.insert("sv");
  destinations.insert("svb");
  destinations.insert("tc");
  destinations.insert("template");
  destinations.insert("themedata");
  destinations.insert("title");
  destinations.insert("txe");
  destinations.insert("ud");
  destinations.insert("upr");
  destinations.insert("userprops");
  destinations.insert("wgrffmtfilter");
  destinations.insert("windowcaption");
  destinations.insert("writereservation");
  destinations.insert("writereservhash");
  destinations.insert("xe");
  destinations.insert("xform");
  destinations.insert("xmlattrname");
  destinations.insert("xmlattrvalue");
  destinations.insert("xmlclose");
  destinations.insert("xmlname");
  destinations.insert("xmlnstbl");
  destinations.insert("xmlopen");

  // special chars
  // ordered by key just in case it is helpful...
  specialchars.insert(std::pair<std::string, std::string>("bullet", "x2022"));
  specialchars.insert(std::pair<std::string, std::string>("emdash", "x2014"));
  specialchars.insert(std::pair<std::string, std::string>("emspace", "x2003"));
  specialchars.insert(std::pair<std::string, std::string>("endash", "x2013"));
  specialchars.insert(std::pair<std::string, std::string>("enspace", "x2002"));
  specialchars.insert(std::pair<std::string, std::string>("ldblquote", "x201C"));
  specialchars.insert(std::pair<std::string, std::string>("line", "x000A"));
  specialchars.insert(std::pair<std::string, std::string>("lquote", "xu2018"));
  specialchars.insert(std::pair<std::string, std::string>("page", "x000Ax000A"));
  specialchars.insert(std::pair<std::string, std::string>("par", "x000A"));
  specialchars.insert(std::pair<std::string, std::string>("qmspace", "x2005"));
  specialchars.insert(std::pair<std::string, std::string>("rdblquote", "x201D"));
  specialchars.insert(std::pair<std::string, std::string>("rquote", "x2019"));
  specialchars.insert(std::pair<std::string, std::string>("sect", "x000Ax000A"));
  specialchars.insert(std::pair<std::string, std::string>("tab", "x0009"));
}




void append_out(std::vector<Section> &doc, std::string value, bool toconv)
{
  // if vec size is zero, just append
  long long N = doc.size();
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
List strip_helper(CharacterMatrix match_mat) {
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
  //
  // returns a list of two vectors of the same length
  //   - str  : character vector of hex codes, in the form, e.g, x0010x3010...
  //   - conv : logocal vector indicating
  //            whether the codes should be converted.
  //

  std::set<std::string> destinations;
  SimpleOrderedDict<std::string> specialchars;
  set_parameters(destinations, specialchars);

  // debug
  // Rcout << "* special chars *\n";
  // for (unsigned int i = 0; i < specialchars.size(); i++)
  //   Rcout << "  " << specialchars.keys[i] << " = " <<
  //     specialchars.values[i] << "\n";
  // Rcout << "* destinations *\n";
  // for (std::set<std::string>::iterator it = destinations.begin();
  //      it != destinations.end(); ++it)
  //   Rcout << "  " << *it << "\n";



  std::vector<Section> doc;
  std::stack<State> state_stack;
  bool ignorable = false;
  int ucskip = 1;
  int curskip = 0;
  std::string curhex = "";

  long long N = match_mat.nrow();
  if (match_mat.ncol() < 7) stop("match_mat must have 7 columns");
  for (long long i = 0; i < N; i++)
  {

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


  // compile output
  CharacterVector str_vec;
  LogicalVector   toconv_vec;
  for (unsigned long long i = 0; i < doc.size(); i++)
  {
    str_vec.push_back(doc[i].strcode);
    toconv_vec.push_back(doc[i].toconv);
  }
  List out = List::create(Named("strcode") = str_vec, Named("toconv") = toconv_vec);
  return out;

}



/*** R

library(magrittr)
text <- readLines("tests/testthat/cp932.rtf", warn = FALSE) %>%
  paste0(collapse = "\n")
pattern <- stringr::regex("\\\\([a-z]{1,32})(-?\\d{1,10})?[ ]?|\\\\'([0-9a-f]{2})|\\\\([^a-z])|([{}])|[\r\n]+|(.)",
                          ignore_case = TRUE)
match_mat <- stringr::str_match_all(text, pattern)[[1]]
o <- striprtf:::strip_helper(match_mat)
strsplit(o$strcode, "x") %>% unlist() %>% as.hexmode() %>% as.integer() %>%
  intToUtf8()

*/
