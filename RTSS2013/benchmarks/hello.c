#include "stdio.h"

int baz (int x){
  int i;
  int y = 0;
  for (i = 0; i<x; ++i)
  {
    y *= y + 2;
  }
  return y;
}

int bar (int x) {
  if (x > 0)
    return x + 10;
  else
    return x + baz(9);
}

int foo (int x) {
  return bar(x) + baz(2);
}


int main (int argc, char *argv[]) {
  const int ARRAY_SIZE = argc - 1;
  int TV[ARRAY_SIZE];
  int i;

  /*
   * At least one integer value must be supplied
   */
  if (argc == 1)
  {
    return 1;
  }

  for (i = 0; i < argc - 1; ++i)
  {
    TV[i] = atoi (argv[i + 1]);
  }

  int y = foo(TV[0]);
  int z = bar(y);

  return 0;
}
