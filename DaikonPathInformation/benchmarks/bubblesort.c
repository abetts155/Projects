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
//==========> swapiflarger : header 14
int __count_17 = 0;
int __count_14_15 = 0;
int __count_16_17 = 0;
  #endif

  int tmp;
  if (*a > *b)
  {	
    #ifdef CBMC
    __count_14_15++;
    #endif
    tmp = *a;
    *a = *b;
    *b = tmp;
    #ifdef CBMC
    __count_17++;
    #endif

#ifdef CBMC
assert(__count_16_17 <= 1); // Upper capacity constraint
assert(__count_14_15 <= 1); // Upper capacity constraint
assert(__count_17 >= 1); // Lower capacity constraint
assert(__count_17 <= 1); // Upper capacity constraint
assert(__count_16_17 > 0 ==> __count_17 > 0); // Execution dependence
assert(__count_14_15 > 0 ==> __count_17 > 0); // Execution dependence
#endif

    return 1;
  }
  #ifdef CBMC
  else __count_16_17++;
  #endif

  #ifdef CBMC
  __count_17++;
  #endif

#ifdef CBMC
assert(__count_16_17 <= 1); // Upper capacity constraint
assert(__count_14_15 <= 1); // Upper capacity constraint
assert(__count_17 >= 1); // Lower capacity constraint
assert(__count_17 <= 1); // Upper capacity constraint
assert(__count_16_17 > 0 ==> __count_17 > 0); // Execution dependence
assert(__count_14_15 > 0 ==> __count_17 > 0); // Execution dependence
#endif

  return 0;
}

void
bubblesort (int ARRAY_SIZE, int a[])
{

  #ifdef CBMC
//==========> bubblesort : header 7
int __count_4_6 = 0;
int __count_5_6 = 0;
int __count_7_3 = 0; //Loop counter
//==========> bubblesort : header 10
int __count_7_8 = 0;
int __count_10_2 = 0; //Loop counter
//==========> bubblesort : header 1
int __count_13 = 0;
int __count_8_12 = 0;
int __count_10_11 = 0;
  #endif

  int i, j, tmp;
  int swapped = 0;
  // header 10
  for (i = 0;
       i < ARRAY_SIZE - 1;
       i++)
  {
    #ifdef CBMC
    __count_10_2++;
    __count_7_3 = 0;
    #endif
    swapped = 0;
    // header 7
    for (j = 0; 
         j < ARRAY_SIZE - 1 - i; 
         j++)
    {
      #ifdef CBMC
      __count_7_3++;
      #endif
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
    #ifdef CBMC
    //assert(__count_7_3 <= 10); // Loop counter property

    // asserts for header 7
    
    __count_7_8++;
    #endif
    if (swapped == 0)
    {
      #ifdef CBMC
      __count_8_12++;
      #endif
      break;
    }
  }
  #ifdef CBMC
  assert(__count_10_2 <= 10); // Loop counter property
  __count_10_11++;
  __count_13++;
  #endif

#ifdef CBMC
assert(__count_13 >= 1); // Lower capacity constraint
assert(__count_13 <= 1); // Upper capacity constraint
//assert(__count_4_6 >= 3); // Lower capacity constraint
assert(__count_4_6 <= 24); // Upper capacity constraint
//assert(__count_5_6 >= 12); // Lower capacity constraint
assert(__count_5_6 <= 42); // Upper capacity constraint
//assert(__count_7_8 >= 5); // Lower capacity constraint
assert(__count_7_8 <= 9); // Upper capacity constraint
assert(__count_8_12 <= 1); // Upper capacity constraint
assert(__count_10_11 <= 1); // Upper capacity constraint
assert(__count_8_12 > 0 ==> __count_13 > 0); // Execution dependence
assert(__count_8_12 > 0 ==> __count_4_6 > 0); // Execution dependence
//assert(__count_8_12 > 0 ==> __count_5_6 > 0); // Execution dependence
assert(__count_8_12 > 0 ==> __count_7_8 > 0); // Execution dependence
assert(__count_10_11 > 0 ==> __count_13 > 0); // Execution dependence
//assert(__count_10_11 > 0 ==> __count_4_6 > 0); // Execution dependence
//assert(__count_10_11 > 0 ==> __count_5_6 > 0); // Execution dependence
//assert(__count_10_11 > 0 ==> __count_7_8 > 0); // Execution dependence
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
