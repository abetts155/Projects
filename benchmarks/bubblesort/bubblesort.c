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
 * value pointed to by b
 */
void
swapIfLarger (int *a, int *b)
{
	int tmp;
	if (*a > *b)
	{	
		tmp = *a;
		*a = *b;
		*b = tmp;
	}
}

void
bubblesort (int ARRAY_SIZE, int a[])
{
  int i, j, tmp;
  for (i = 0; i < ARRAY_SIZE - 1; i++)
  {
    for (j = 0; j < ARRAY_SIZE - 1 - i; j++)
    {
      swapIfLarger(&a[j], &a[j+1]);
/*
      if (a[j + 1] < a[j])
      {
        tmp = a[j];
        a[j] = a[j + 1];
        a[j + 1] = tmp;
      }
*/
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

  bubblesort (ARRAY_SIZE, TV);

  for (i = 0; i < argc - 1; ++i)
  {
    printf("%i ", TV[i]);
  }
  printf("\n");

  return 0;
}
