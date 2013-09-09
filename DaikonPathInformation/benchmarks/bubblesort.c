/*
 * Bubble sort taken from MDH suite and modified by Adam Betts to consume a
 * test vector supplied on the command line.
 *
 * For this program, the test vector is a list of integers to be sorted.
 * Note that the size of the array is determined by the number of arguments
 * given to the program, i.e. argc - 1.
 */

/*
 * Swaps the values if the value pointed to by a is greater than the
 * value pointed to by b and returns 1 if a swap is performed, 0 otherwise
 */
int
swapIfLarger (int *a, int *b)
{
  int tmp;
  if (*a > *b)
  {	
    tmp = *a;
    *a = *b;
    *b = tmp;
    return 1;
  }
  return 0;
}

void
bubblesort (int ARRAY_SIZE, int a[])
{
  int i, j, tmp;
  int swapped = 0;
  #ifdef CBMC
  int __counti = 0;
  int __countj = 0;
  #endif
  for (i = 0; i < ARRAY_SIZE - 1; i++)
  {
    #ifdef CBMC
    __counti++;
    #endif
    swapped = 0;
    for (j = 0; j < ARRAY_SIZE - 1 - i; j++)
    {
      #ifdef CBMC
      __countj++;
      #endif
      if (swapIfLarger(&a[j], &a[j+1]))
      {
        swapped = 1;
      }
    }
    if (swapped == 0)
    {
      break;
    }
  }
  #ifdef CBMC
  assert(__counti<=10);
  assert(__countj<=10);
  #endif
}

int
main (int argc, char *argv[])
{
  const int ARRAY_SIZE = argc - 1;
  int TV[ARRAY_SIZE];
  int i;
  #ifdef CBMC
  __CPROVER_assume(ARRAY_SIZE == 10);
  #endif

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

  bubblesort (ARRAY_SIZE, TV);

  return 0;
}
