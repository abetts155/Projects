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
  #ifdef CBMC
  int __count_14_16 = 0;
  int __count_15_17 = 0;
  #endif

  int tmp;
  if (*a > *b)
  {	
    #ifdef CBMC
    __count_15_17++;
    #endif
    tmp = *a;
    *a = *b;
    *b = tmp;
    return 1;
  }
  #ifdef CBMC
  else __count_14_16++;
  #endif
  return 0;
}

void
bubblesort (int ARRAY_SIZE, int a[])
{
  #ifdef CBMC
  int __count_L10 = 0;
  int __count_L7 = 0;
  int __count_5_6 = 0;
  int __count_8_9 = 0;
  int __count_12_13 = 0;
  int __count_4_6 = 0;
  int __count_11_13 = 0;
  #endif

  int i, j, tmp;
  int swapped = 0;
  for (i = 0;
       #ifdef CBMC
       __count_L10++,
       #endif
       i < ARRAY_SIZE - 1;
       i++)
  {
    swapped = 0;
    for (j = 0; 
         #ifdef CBMC
         __count_L7++,
         #endif
         j < ARRAY_SIZE - 1 - i; 
         j++)
    {
      if (swapIfLarger(&a[j], &a[j+1]))
      {        
        #ifdef CBMC
        __count_5_6++;
        #endif
        swapped = 1;
      }
      #ifdef CBMC
      else __count_4_6++;
      #endif
    }
    if (swapped == 0)
    {
      #ifdef CBMC
      __count_12_13++;
      #endif
      break;
    }
    #ifdef CBMC
    else __count_8_9++;
    #endif
  }
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
