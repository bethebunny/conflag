#include "conflag.h"
#include <iostream>
#include <string>
#include <sstream>
#include <iterator>
#include <algorithm>

using namespace std;

int main(int argc, char **argv) {
  if (argc < 2) {
    cout << "Usage: " << argv[0] << " config [queries...]" << endl;
    return 1;
  }
  conflag::bref out = conflag::load(argv[1]);
  if (!out) {
    cerr << "Failed to parse file!" << endl;
    return 1;
  }
  if (argc < 3) { cout << out << endl; return 0; }
  for (int i = 2; i < argc; ++i) {
    istringstream q(argv[i]);
    vector<string> query{istream_iterator<string>(q), istream_iterator<string>()};
    conflag::bref result = out;
    for (auto s : query) {
      auto o = dynamic_pointer_cast<const conflag::Object>(result);
      if (!o) {
        cout << "Attempted to query non-object with " << s << endl;
        return 1;
      }
      result = (*o)[s];
    }
    cout << result << endl;
  }
}
