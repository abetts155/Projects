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
int __count_4_5 = 0;
int __count_4_5_L = 0; //Loop counter
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
    __count_4_5_L = 0;
    #endif
    while (a[i] > key && 
     (
     #ifdef CBMC
     (
      __count_4_5_L++,
      __count_4_5++
     ),
     #endif
     i >= 0
     )
    ) // 4, 5
    {
      a[i+1] = a[i];
      i--;
    }
    #ifdef CBMC
    assert(__count_4_5_L  <= 98); // Loop counter property
    #endif

    #ifdef CBMC
    if (a[i] <= key) __count_4_6++;
    else __count_5_6++;
    #endif

    a[i+1] = key;
  }
  #ifdef CBMC
  assert(__count_7_2  <= 100); // Loop counter property
  #endif

  #ifdef CBMC
  __count_7_8++;
  __count_8++;
  #endif

#ifdef CBMC
assert(__count_8 >= 1); // Lower capacity constraint
assert(__count_8 <= 1); // Upper capacity constraint
assert(__count_7_8 >= 1); // Lower capacity constraint
assert(__count_7_8 <= 1); // Upper capacity constraint
//assert(__count_4_5 >= 1979); // Lower capacity constraint
assert(__count_4_5 <= 3385); // Upper capacity constraint
//assert(__count_4_6 >= 99); // Lower capacity constraint
assert(__count_4_6 <= 99); // Upper capacity constraint
//assert(__count_5_6 == 0); // Dead code
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
  
  #ifdef CBMC
  __CPROVER_assume(ARRAY_SIZE >= 1 && ARRAY_SIZE <= 100);
  #endif

  int val = insertsort (ARRAY_SIZE, TV);

  return 0;
}

