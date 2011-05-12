/*
 * Cocktail sort (also known as bidirectional bubble sort) which consumes a
 * test vector supplied on the command line.
 *
 * For this program, the test vector is a list of integers to be sorted.
 * Note that the size of the array is determined by the number of arguments
 * given to the program, i.e. argc - 1.
 */

#define FALSE 0
#define TRUE 1

void
cocktailsort (int ARRAY_SIZE, int a[])
{
  int begin = -1;
  int end = ARRAY_SIZE;
  int swapped;
  int i;
  int tmp;

  while (begin < end)
  {
    swapped = FALSE;
    begin = begin + 1;
    end = end - 1;

    for (i = begin; i < end; ++i)
    {
      if (a[i + 1] < a[i])
      {
        tmp = a[i];
        a[i] = a[i + 1];
        a[i + 1] = tmp;
        swapped = TRUE;
      }
    }

    if (swapped == FALSE)
    {
      break;
    }

    swapped = FALSE;

    for (i = end; --i >= begin;)
    {
      if (a[i + 1] < a[i])
      {
        tmp = a[i];
        a[i] = a[i + 1];
        a[i + 1] = tmp;
        swapped = TRUE;
      }
    }

    if (swapped == FALSE)
    {
      break;
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

  cocktailsort (ARRAY_SIZE, TV);

  return 0;
}
