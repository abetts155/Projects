#include "stdio.h"

int bar (int x) {
  if (x > 0)
    return x + 10;
  else
    return x + 9;
}

int foo (int x) {
  return bar(x) + 10;
}


int main () {
  int x = 10;
  int y = foo(x);
  int z = bar(y);

  return 0;
}
