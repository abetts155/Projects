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
//==========> insertsort : header 4
int __count_4_5 = 0; //Loop counter
//==========> insertsort : header 7
int __count_4_6 = 0;
int __count_5_6 = 0;
int __count_7_2 = 0; //Loop counter
//==========> insertsort : header 1
int __count_8 = 0;
int __count_7_8 = 0;
#endif
  int i, j, key;

  #ifdef CBMC
  __count_7_2 = 0;
  #endif
  for (j = 1; j < ARRAY_SIZE; j++) // 7
  {
    #ifdef CBMC
    __count_7_2++;
    #endif
    key = a[j];
    i = j - 1;

    #ifdef CBMC
    __count_4_5 = 0;
    #endif
    while (a[i] > key && 
     (
     #ifdef CBMC
     __count_4_5++,
     #endif
     i >= 0
     )
    ) // 4, 5
    {
      a[i+1] = a[i];
      i--;
    }
    #ifdef CBMC
    if (a[i] <= key) __count_4_6++;
    else __count_5_6++;
    #endif

    a[i+1] = key;
  }

  #ifdef CBMC
  __count_7_8++;
  __count_8++;
  #endif
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

