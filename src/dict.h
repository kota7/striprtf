#ifndef DICTHEADERDEF
#define DICTHEADERDEF

#include <string>
#include <Rcpp.h>

using namespace Rcpp;


// very simple dictionary class
// this requires that the keys are added in
// not usable for other occasions


template <class S, class T>
  struct SimpleOrderedDict
{
  std::vector<S> keys;
  std::vector<T> values;

  SimpleOrderedDict()
  {
    std::vector<S> keys(0);
    std::vector<T> values(0);
  }
  unsigned int size()
  {
    return keys.size();
  }

  void insert(S key, T value)
  {
    // if empty, just add them
    if (size() == 0) {
      keys.push_back(key);
      values.push_back(value);
      return;
    }
    // make sure the key is ordered
    if (key < keys.back()) stop("key must be increasing");
    // make sure this is a new key
    if (haskey(key)) stop(key + " already exists");

    keys.push_back(key);
    values.push_back(value);
  }

  int locate(const S &key)
  {
    // returns the location of key
    // if not exists, returns negative integer
    //
    // conduct binary search
    int left = 0;
    int right = size()-1;
    while (left <= right)
    {
      int mid = (left+right)/2;
      if (key == keys[mid]) return mid;
      if (key < keys[mid]) {
        right = mid-1;
      } else {
        left = mid + 1;
      }
    }
    return -1;
  }

  bool haskey(const S &key)
  {
    // returns true if and only if the key exists
    return (locate(key) >= 0);
  }

  T getvalue(const S & key)
  {
    int i = locate(key);
    if (i >= 0) return values[i];
    Rcpp::stop("key does not exist");
  }
};


#endif
