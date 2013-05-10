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
  int i = l;
  int j = r;
  int k = l;

  while (i < r && j < u)
  {
    if (a[i] <= a[j])
    {
      b[k] = a[i];
      i++;
    }
    else
    {
      b[k] = a[j];
      j++;
    }
    k++;
  }
  while (i < r)
  {
    b[k] = a[i];
    i++;
    k++;
  }
  while (j < u)
  {
    b[k] = a[j];
    j++;
    k++;
  }
  for (k = l; k < u; k++)
  {
    a[k] = b[k];
  }
}

int
mergesort (int ARRAY_SIZE, int a[])
{
  int k = 1;
  int u;
  int i;
  int b[ARRAY_SIZE];

  while (k < ARRAY_SIZE)
  {
    i = 1;
    while (i + k <= ARRAY_SIZE)
    {
      u = i + k * 2;
      if (u > ARRAY_SIZE)
      {
        u = ARRAY_SIZE + 1;
      }
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
