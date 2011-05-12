/*
 * Insert sort taken from MDH suite and modified by Adam Betts to consume a
 * test vector supplied on the command line.
 *
 * For this program, the test vector is a list of integers to be sorted.
 * Note that the size of the array is determined by the number of arguments
 * given to the program, i.e. argc - 1.
 */

void
insertsort (int ARRAY_SIZE, int a[])
{
  int i, j, key;

  for (j = 1; j < ARRAY_SIZE; j++)
  {
    key = a[j];
    i = j - 1;

    while (a[i] > key && i >= 0)
    {
      a[i+1] = a[i];
      i--;
    }

    a[i+1] = key;
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

  for (i = 0; i < argc - 1; i++)
  {
    TV[i] = atoi (argv[i + 1]);
  }

  insertsort (ARRAY_SIZE, TV);

  return 0;
}

