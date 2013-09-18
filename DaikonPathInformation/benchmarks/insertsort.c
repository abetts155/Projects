/*
 * Insert sort taken from MDH suite and modified by Adam Betts to consume a
 * test vector supplied on the command line.
 *
 * For this program, the test vector is a list of integers to be sorted.
 * Note that the size of the array is determined by the number of arguments
 * given to the program, i.e. argc - 1.
 */

int
insertsort (int ARRAY_SIZE, int a[])
{
  #ifdef CBMC
  int __count_4_6 = 0;
  int __count_5_3 = 0;
  int __count_5_6 = 0;
  int __count_L7 = 0;
  int __count_L4 = 0;
  #endif
  int i, j, key;

  for (j = 1; 
    #ifdef CBMC
    __count_L7++,
    #endif
  j < ARRAY_SIZE; j++) // 7
  {
    key = a[j];
    i = j - 1;

    while (
      #ifdef CBMC
      __count_L4++,
      #endif
    a[i] > key && i >= 0) // 4, 5
    {
      #ifdef CBMC
      __count_5_3++;
      #endif
      a[i+1] = a[i];
      i--;
    }
    #ifdef CBMC
    if (a[i] <= key) __count_4_6++;
    else __count_5_6++;
    #endif

    a[i+1] = key;
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

  for (i = 0; i < argc - 1; i++)
  {
    TV[i] = atoi (argv[i + 1]);
  }

  int val = insertsort (ARRAY_SIZE, TV);
  printf("%d", val);

  return 0;
}

