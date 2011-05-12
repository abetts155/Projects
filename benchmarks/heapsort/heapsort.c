/*
 * Heap sort which consumes a
 * test vector supplied on the command line.
 *
 * For this program, the test vector is a list of integers to be sorted.
 * Note that the size of the array is determined by the number of arguments
 * given to the program, i.e. argc - 1.
 */

void
heapbubble (int ARRAY_SIZE, int a[], int pos)
{
  int z = 0;
  int max = 0;
  int tmp = 0;
  int left = 0;
  int right = 0;

  z = pos;
  for (;;)
  {
    left = 2 * z + 1;
    right = left + 1;

    if (left >= ARRAY_SIZE)
    {
      return;
    }
    else if (right >= ARRAY_SIZE)
    {
      max = left;
    }
    else if (a[left] > a[right])
    {
      max = left;
    }
    else
    {
      max = right;
    }
    if (a[z] > a[max])
    {
      return;
    }

    tmp = a[z];
    a[z] = a[max];
    a[max] = tmp;
    z = max;
  }
}

void
heapsort (int ARRAY_SIZE, int a[])
{
  int i;
  int tmp;
  int z;
  int max;
  int tmp2;
  int left;
  int right;

  for (i = ARRAY_SIZE / 2; i >= 0; --i)
  {
    max   = 0;
    tmp2  = 0;
    left  = 0;
    right = 0;
    z     = i;
    
    for (;;)
    {
      left = 2 * z + 1;
      right = left + 1;

      if (left >= ARRAY_SIZE)
      {
        break;
      }
      else if (right >= ARRAY_SIZE)
      {
        max = left;
      }
      else if (a[left] > a[right])
      {
        max = left;
      }
      else
      {
        max = right;
      }
      if (a[z] > a[max])
      {
        break;
      }

      tmp2 = a[z];
      a[z] = a[max];
      a[max] = tmp2;
      z = max;
    }
  } 

  for (i = ARRAY_SIZE - 1; i > 0; i--)
  {
    tmp   = a[0];
    a[0]  = a[i];
    a[i]  = tmp;
    max   = 0;
    tmp2  = 0;
    left  = 0;
    right = 0;
    z     = 0;
    
    for (;;)
    {
      left = 2 * z + 1;
      right = left + 1;

      if (left >= ARRAY_SIZE)
      {
        break;
      }
      else if (right >= ARRAY_SIZE)
      {
        max = left;
      }
      else if (a[left] > a[right])
      {
        max = left;
      }
      else
      {
        max = right;
      }
      if (a[z] > a[max])
      {
        break;
      }

      tmp2 = a[z];
      a[z] = a[max];
      a[max] = tmp2;
      z = max;
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

  heapsort (ARRAY_SIZE, TV);

  return 0;
}
