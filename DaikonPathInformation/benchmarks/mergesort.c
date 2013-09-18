/*
 * Merge sort which consumes a
 * test vector supplied on the command line.
 *
 * For this program, the test vector is a list of integers to be sorted.
 * Note that the size of the array is determined by the number of arguments
 * given to the program, i.e. argc - 1.
 */

int
merge (int ARRAY_SIZE, int a[], int b[], int l, int r, int u)
{
  #ifdef CBMC
  int __count_13_15 = 0;
  int __count_14_15 = 0;
  int __count_16_20 = 0;
  int __count_18_20 = 0;
  int __count_20_19 = 0;
  int __count_23_22 = 0;
  int __count_26_25 = 0;
  int __count_L26 = 0;
  int __count_L23 = 0;
  int __count_L20 = 0;
  int __count_L16 = 0;
  #endif

  int i = l;
  int j = r;
  int k = l;

  while (
    #ifdef CBMC
    __count_L16++,
    #endif
    i < r && j < u)
  {
    if (a[i] <= a[j])
    {
      b[k] = a[i];
      i++;
      #ifdef CBMC
      __count_13_15++;
      #endif
    }
    else
    {
      b[k] = a[j];
      j++;
      #ifdef CBMC
      __count_14_15++;
      #endif
    }
    k++;
  }

  // TODO: check
  #ifdef CBMC
  if(i < r) {
    __count_18_20++;
  }
  else {
    __count_16_20++;
  }
  #endif

  while (
    #ifdef CBMC
    __count_L20++,
    #endif
    i < r)
  {
    #ifdef CBMC
    __count_20_19++;
    #endif
    b[k] = a[i];
    i++;
    k++;
  }
  while (
    #ifdef CBMC
    __count_L23++,
    #endif
    j < u)
  {
    #ifdef CBMC
    __count_23_22++;
    #endif
    b[k] = a[j];
    j++;
    k++;
  }
  for (k = l; 
    #ifdef CBMC
    __count_L26++,
    #endif
    k < u; k++)
  {
    #ifdef CBMC
    __count_26_25++;
    #endif
    a[k] = b[k];
  }
}

int
mergesort (int ARRAY_SIZE, int a[])
{
  #ifdef CBMC
  int __count_3_5 = 0;
  int __count_4_5 = 0;
  int __count_L9 = 0;
  int __count_L7 = 0;
  #endif

  int k = 1;
  int u;
  int i;
  int b[ARRAY_SIZE];

  while (
    #ifdef CBMC
    __count_L9++,
    #endif
    k < ARRAY_SIZE)
  {
    i = 1;
    while (
      #ifdef CBMC
      __count_L7++,
      #endif
      i + k <= ARRAY_SIZE)
    {
      u = i + k * 2;
      if (u > ARRAY_SIZE)
      {
        u = ARRAY_SIZE + 1;
        #ifdef CBMC
        __count_4_5++;
        #endif
      }
      #ifdef CBMC
      else __count_3_5++;
      #endif
      merge (ARRAY_SIZE, a, b, i, i + k, u);
      i = i + k * 2;
    }
    k = k * 2;
  }
  
  return a[0];
}

int
main (int argc, char *argv[])
{
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

  int val = mergesort (ARRAY_SIZE, TV);
  
  printf("%d", val);

  return 0;
}
