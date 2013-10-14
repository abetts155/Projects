/*
 * Quick sort taken from http://jeffreystedfast.blogspot.com/2007/03/quick-sort.html
 * and modified by Adam Betts to consume a
 * test vector supplied on the command line.
 *
 * For this program, the test vector is a list of integers to be sorted.
 * Note that the size of the array is determined by the number of arguments
 * given to the program, i.e. argc - 1.
 */

typedef struct qstack
{
  int lo;
  int hi;
} qstack_t;

void
quicksort (int ARRAY_SIZE, int a[])
{
#ifdef CBMC
//==========> quicksort : header 10
int __count_11_9 = 0;
int __count_10_11 = 0; //Loop counter
//==========> quicksort : header 6
int __count_6_10 = 0;
int __count_7_5 = 0;
int __count_7_8 = 0;
int __count_10_12 = 0;
int __count_11_12 = 0;
int __count_12_13 = 0;
int __count_16_18 = 0;
int __count_17_18 = 0;
int __count_6_10 = 0; //Loop counter
int __count_6_7 = 0; //Loop counter
//==========> quicksort : header 20
int __count_14_19 = 0;
int __count_20_3 = 0; //Loop counter
//==========> quicksort : header 1
int __count_23 = 0;
int __count_20_21 = 0;
int __count_22_23 = 0;
#endif

  qstack_t stack[32];
  qstack_t *sp;
  int lo;
  int hi;
  int low;
  int high;
  int pivot;
  int tmp;

  if (ARRAY_SIZE < 2)
  {
   #ifdef CBMC
   __count_22_23++;
   #endif
    return;
  }

  /* push our initial values onto the stack */
  sp = stack;
  sp->lo = 0;
  sp->hi = ARRAY_SIZE;
  sp++;

  while (sp > stack)
  {
  #ifdef CBMC
  __count_20_3++;
  #endif
  
    /* pop lo and hi off the stack */
    sp--;
    high = sp->hi;
    low = sp->lo;

    hi = high - 1;
    lo = low;

    pivot = a[lo];

    while (1)
    {
      while (
      #ifdef CBMC 
      __count_6_7++,
      __count_6_10++,  
      #endif
      lo < high && 
      a[lo] < pivot)
      {
        #ifdef CBMC
        __count_6_10--;
        __count_7_5++;
        #endif
        lo++;
      }

      while (
      #ifdef CBMC 
      __count_10_11++, 
      __count_10_12++, 
      #endif 
      hi > low && 
      a[hi] >= pivot)
      {
        #ifdef CBMC
        __count_10_12--; 
        __count_11_9++;
        #endif
        hi--;
      }

      if (lo < hi)
      {
        #ifdef CBMC
        __count_12_13++;
        #endif
      
        /* swap */
        tmp = a[lo];
        a[lo] = a[hi];
        a[hi] = tmp;
        hi--;
      }
      else
      {
        hi++;

        if (hi == high)
        {
        #ifdef CBMC
        __count_14_19++;
        #endif
          /* done with this segment */
          break;
        }

        /* push the larger segment onto the
         * stack and continue sorting the
         * smaller segment. */
        if ((hi - low) > (high - hi))
        {
        #ifdef CBMC
        __count_17_18++;
        #endif
        
          sp->lo = low;
          sp->hi = hi;
          sp++;

          hi = high;
          low = lo;
        }
        else
        {
        #ifdef CBMC
        __count_16_18++;
        #endif
          sp->hi = high;
          sp->lo = hi;
          sp++;

          high = hi;
          lo = low;
        }

        pivot = a[lo];
        hi--;
      }
    }
  }
  
  #ifdef CBMC
  __count_23++;
  __count_20_21++;
  #endif
  
  #ifdef CBMC
assert(__count_10_11  <= 60); // Loop counter property
assert(__count_6_10  + __count_6_7  <= 138); // Loop counter property
assert(__count_20_3  <= 101); // Loop counter property
assert(__count_7_8 >= 327); // Lower capacity constraint
assert(__count_7_8 <= 353); // Upper capacity constraint
assert(__count_7_5 >= 241); // Lower capacity constraint
assert(__count_7_5 <= 516); // Upper capacity constraint
assert(__count_10_12 >= 156); // Lower capacity constraint
assert(__count_10_12 <= 169); // Upper capacity constraint
assert(__count_11_9 >= 227); // Lower capacity constraint
assert(__count_11_9 <= 500); // Upper capacity constraint
assert(__count_11_12 >= 164); // Lower capacity constraint
assert(__count_11_12 <= 197); // Upper capacity constraint
assert(__count_12_13 >= 128); // Lower capacity constraint
assert(__count_12_13 <= 154); // Upper capacity constraint
assert(__count_14_19 >= 100); // Lower capacity constraint
assert(__count_14_19 <= 100); // Upper capacity constraint
assert(__count_16_18 >= 12); // Lower capacity constraint
assert(__count_16_18 <= 31); // Upper capacity constraint
assert(__count_17_18 >= 68); // Lower capacity constraint
assert(__count_17_18 <= 87); // Upper capacity constraint
assert(__count_20_21 >= 1); // Lower capacity constraint
assert(__count_20_21 <= 1); // Upper capacity constraint
assert(__count_22_23 == 0); // Dead code
assert(__count_23 >= 1); // Lower capacity constraint
assert(__count_23 <= 1); // Upper capacity constraint
assert(__count_6_10 == 0); // Dead code
#endif
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
  
  #ifdef CBMC
  __CPROVER_assume(ARRAY_SIZE >= 1 && ARRAY_SIZE <= 100);
  #endif

  for (i = 0; i < argc - 1; ++i)
  {
    TV[i] = atoi (argv[i + 1]);
  }

  quicksort (ARRAY_SIZE, TV);

  return 0;
}
