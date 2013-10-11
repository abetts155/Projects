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
#ifdef CBMC
//==========> matrixmultiply : header 5
int __count_5_4 = 0;
int __count_5_4_L = 0; //Loop counter
//==========> matrixmultiply : header 7
int __count_5_6 = 0;
int __count_7_3 = 0; //Loop counter
//==========> matrixmultiply : header 9
int __count_7_8 = 0;
int __count_9_2 = 0; //Loop counter
//==========> matrixmultiply : header 1
int __count_10 = 0;
int __count_9_10 = 0;
#endif

  int i, j, k;

  #ifdef CBMC
  __count_9_2 = 0;
  #endif
  for (i = 0; i < UPPERLIMIT; ++i) // 9
  {
    #ifdef CBMC
    __count_9_2++;
    #endif

    #ifdef CBMC
    __count_7_3 = 0;
    #endif
    for (j = 0; j < UPPERLIMIT; ++j) // 7
    {
      #ifdef CBMC
      __count_7_3++;
      #endif
      C[i][j] = 0;
      #ifdef CBMC
      __count_5_4_L = 0;
      #endif
      for (k = 0; k < UPPERLIMIT; ++k) // 5
      {
        #ifdef CBMC
        __count_5_4++;
        __count_5_4_L++;
        #endif
        C[i][j] += A[j][k] * B[k][j];
      }
      #ifdef CBMC
      assert(__count_5_4_L  <= 6); // Loop counter property
      __count_5_6++;
      #endif
    }
    #ifdef CBMC
    assert(__count_7_3  <= 6); // Loop counter property
    __count_7_8++;
    #endif
  }
  #ifdef CBMC
  assert(__count_9_2  <= 6); // Loop counter property
  __count_9_10++;
  __count_10++;
  #endif

#ifdef CBMC
assert(__count_5_6 >= 25); // Lower capacity constraint
assert(__count_5_6 <= 25); // Upper capacity constraint
assert(__count_7_8 >= 5); // Lower capacity constraint
assert(__count_7_8 <= 5); // Upper capacity constraint
assert(__count_10 >= 1); // Lower capacity constraint
assert(__count_10 <= 1); // Upper capacity constraint
assert(__count_9_10 >= 1); // Lower capacity constraint
assert(__count_9_10 <= 1); // Upper capacity constraint
assert(__count_5_4 >= 125); // Lower capacity constraint
assert(__count_5_4 <= 125); // Upper capacity constraint
#endif

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

