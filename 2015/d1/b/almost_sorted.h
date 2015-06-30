// Sample input 2, in CPP.

#include <cassert>
#include <algorithm>
#include <vector>

long long QQQ = 100LL;

long long NumberOfFiles() {
  return QQQ;
}

long long MaxDistance() {
  return 2LL;
}

long long Identifier(long long j) {
  std::vector<long long> ids(1000);
  for(int i = 0; i < QQQ; i++) {
    ids[i] = i;
  }

  for(int i = 3; i < QQQ-1; i += 7) {
    long long tmp = ids[i];
    ids[i] = ids[i+1];
    ids[i+1] = tmp;
  }

  return ids[j];
}
