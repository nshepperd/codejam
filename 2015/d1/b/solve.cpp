#include <message.h>
#include <cassert>
#include <cstdio>

#include "almost_sorted.h"

#include <algorithm>
#include <vector>

using namespace std;

typedef long long int64;

static const int64 MOD = 1048576;

void trivial();

int main() {
  int64 sum = 0LL;

  const int64 n = NumberOfFiles();
  const int64 d = MaxDistance();

  if(n <= NumberOfNodes()) {
    trivial();
  } else {
    const int64 sub_nodes = NumberOfNodes(); //min((int64) NumberOfNodes(), n / d);
    const int64 myid = MyNodeId();

    if(myid >= sub_nodes) {
      return 0;
    }

    const int64 start = myid * (n / sub_nodes);
    int64 end = (myid + 1) * (n / sub_nodes);
    if (myid == sub_nodes - 1) {
      end = n;
    }

    fprintf(stderr, "[%i]: (%lld, %lld)\n", myid, start, end);

    const int64 ex_start = max(0LL, start - d);
    const int64 ex_end = min(n, end + d);

    fprintf(stderr, "[%i]: ex = (%lld, %lld)\n", myid, ex_start, ex_end);

    vector<int64> ids(ex_end - ex_start);
    for(int64 i = ex_start; i < ex_end; i++) {
      ids[i - ex_start] = Identifier(i);
    }
    sort(ids.begin(), ids.end());
    for(int64 i = start; i < end; i++) {
      int64 v = ids[i - ex_start];
      sum = (sum + (i * v) % MOD) % MOD;
    }

    if (MyNodeId() > 0) {
      Receive(MyNodeId() - 1);
      sum = (sum + GetLL(MyNodeId() - 1)) % MOD;
    }
    if (MyNodeId() < sub_nodes - 1) {
      PutLL(MyNodeId() + 1, sum);
      Send(MyNodeId() + 1);
    } else {
      printf("%lld\n", sum);
    }
  }
  return 0;
}

void trivial() {
  if (MyNodeId() > 0) {
    return;
  }

  int64 sum = 0LL;
  int64 n = NumberOfFiles();
  vector<int64> ids(n);
  for(int64 i = 0; i < n; i++) {
    ids[i] = Identifier(i);
  }
  sort(ids.begin(), ids.end());

  for(int64 i = 0; i < n; i++) {
    int64 v = ids[i];
    sum = (sum + (i * v) % MOD) % MOD;
  }

  printf("%lld\n", sum);
}
