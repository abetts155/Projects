/*
 * Matrix multiplication taken from MDH suite and modified by Adam Betts to
 * consume a test vector supplied on the command line.
 *
 * For this program, the test vector consists of 25 integers since there are
 * two 5*5 matrices to be initialised before multiplication.
 */

#define UPPERLIMIT 5
typedef int matrix[UPPERLIMIT][UPPERLIMIT];

int
matrixmultiply (matrix A, matrix B, matrix C)
{
  int i, j, k;

  for (i = 0; i < UPPERLIMIT; ++i)
  {
    for (j = 0; j < UPPERLIMIT; ++j)
    {
      C[i][j] = 0;
      for (k = 0; k < UPPERLIMIT; ++k)
      {
        C[i][j] += A[j][k] * B[k][j];
      }
    }
  }
  
  return C[0][0];
}

int
main (int argc, char *argv[])
{
  int i, j, k;
  matrix A, B, C;

  /*
   * There are 2 matrices with UPPERLIMIT*UPPERLIMIT dimensions that need to be filled up.
   * 2*UPPERLIMIT*UPPERLIMIT values need to be passed on the command-line as a consequence.
   */
  if (argc != 2*UPPERLIMIT*UPPERLIMIT+1)
  {
    return 1;
  }

  /*
   * Initialise matrix A.
   */
  k = 0;
  for (i = 0; i < UPPERLIMIT; ++i)
  {
    for (j = 0; j < UPPERLIMIT; ++j)
    {
      A[i][j] = atoi (argv[k + 1]);
      k++;
    }
  }

  /*
   * Initialise matrix B but do NOT reset the value of k as it now refers
   * to the 400th element of the command-line.
   */
  for (i = 0; i < UPPERLIMIT; ++i)
  {
    for (j = 0; j < UPPERLIMIT; ++j)
    {
      B[i][j] = atoi (argv[k + 1]);
      k++;
    }
  }

  int val = matrixmultiply (A, B, C);
    
  printf("%d", val);

  return 0;
}

