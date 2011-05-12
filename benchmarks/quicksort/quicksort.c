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
    return;
  }

  /* push our initial values onto the stack */
  sp = stack;
  sp->lo = 0;
  sp->hi = ARRAY_SIZE;
  sp++;

  while (sp > stack)
  {
    /* pop lo and hi off the stack */
    sp--;
    high = sp->hi;
    low = sp->lo;

    hi = high - 1;
    lo = low;

    pivot = a[lo];

    while (1)
    {
      while (lo < high && a[lo] < pivot)
      {
        lo++;
      }

      while (hi > low && a[hi] >= pivot)
      {
        hi--;
      }

      if (lo < hi)
      {
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
          /* done with this segment */
          break;
        }

        /* push the larger segment onto the
         * stack and continue sorting the
         * smaller segment. */
        if ((hi - low) > (high - hi))
        {
          sp->lo = low;
          sp->hi = hi;
          sp++;

          hi = high;
          low = lo;
        }
        else
        {
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

  quicksort (ARRAY_SIZE, TV);

  for (i = 0; i < argc - 1; ++i)
  {
    printf("%d ", TV[i]);
  }

  return 0;
}
