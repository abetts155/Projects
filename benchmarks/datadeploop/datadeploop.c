/*
 * Takes a vector of integers as arguments and outputs a result
 * 
 * Contains a loop with number of iterations dependent on the caluclated average of the elements
 * in the input vector
 */

int result = 0;

int sumArray (int ARRAY_SIZE, int a[])
{
  int i, sum;

  sum = 0;
  for (i = 0; i < ARRAY_SIZE; i++)
  {
    sum += a[i];
  }

  return sum;
}

int calculateResult (int ARRAY_SIZE, int a[], int avg)
{
  int i, j, bSum, bAvg;
  const int bSize = avg;
  int b[bSize];

  j = 0;
  for(i = 0; i < bSize; i++)
  {
    b[i] = a[j];
    j++;
    j = j % ARRAY_SIZE;
  }

  bSum = sumArray (bSize, b);
  bAvg = bSum / bSize;

  return avg - bAvg;
}

void datadeploop (int ARRAY_SIZE, int a[])
{
  int i, sum, avg;

  sum = sumArray (ARRAY_SIZE, a);
  avg = sum / ARRAY_SIZE;

  result = calculateResult (ARRAY_SIZE, a, avg);
}

int main (int argc, char *argv[])
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

  datadeploop (ARRAY_SIZE, TV);

  return 0;
}
