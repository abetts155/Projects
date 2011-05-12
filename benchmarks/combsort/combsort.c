/*
 * Comb sort which consumes a
 * test vector supplied on the command line.
 *
 * For this program, the test vector is a list of integers to be sorted.
 * Note that the size of the array is determined by the number of arguments
 * given to the program, i.e. argc - 1.
 */

#define FALSE 0
#define TRUE 1

void
combsort (int ARRAY_SIZE, int a[])
{
  float shrink_factor = 1.247330950103979;
  int gap = ARRAY_SIZE;
  int swapped = TRUE;
  int i;
  int tmp;

  while ((gap > 1) || swapped)
  {
    if (gap > 1)
    {
      gap = gap / shrink_factor;
    }

    swapped = FALSE;
    i = 0;

    while ((gap + i) < ARRAY_SIZE)
    {
      if (a[i] - a[i + gap] > 0)
      {
        tmp = a[i];
        a[i] = a[i + gap];
        a[i + gap] = tmp;
        swapped = TRUE;
      }
      ++i;
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

  combsort (ARRAY_SIZE, TV);

  return 0;
}
