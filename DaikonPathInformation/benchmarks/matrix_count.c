/*
 * Matrix count taken from MDH suite and modified
 * by Adam Betts to consume a test vector supplied on the command line.
 *
 * For this program, the test vector consists of UPPERLIMIT * UPPERLIMIT integers 
 * since there is a matrix if that size to be initialised. 
 * The program does the following:
 * 1) It counts the number of positive numbers in the matrix.
 * 2) It sums the positive numbers.
 * 3) It counts the number of negative numbers in the matrix.
 * 4) It sums the negative numbers.
 */

#define UPPERLIMIT 5
typedef int matrix[UPPERLIMIT][UPPERLIMIT];

int Postotal;
int Negtotal;
int Poscnt;
int Negcnt;

int
matrix_count (matrix A)
{
  #ifdef CBMC
  int __count_3_5 = 0;
  int __count_4_6 = 0;
  int __count_L9 = 0;
  int __count_L7 = 0;
  #endif
  int i, j;
  int Ptotal = 0;
  int Ntotal = 0;
  int Pcnt = 0;
  int Ncnt = 0;

  for (i = 0; 
  #ifdef CBMC
  __count_L9++,
  #endif
  i < UPPERLIMIT; ++i) // 9
  {
    for (j = 0; 
    #ifdef CBMC
    __count_L7++,
    #endif
    j < UPPERLIMIT; ++j) // 7
    {
      if (A[i][j] < 0)
      {
        // 4
        Ntotal += A[i][j];
        Ncnt++;
        #ifdef CBMC
        __count_4_6++;
        #endif
      }
      else
      {
        #ifdef CBMC
        __count_3_5++;
        #endif
        Ptotal += A[i][j];
        Pcnt++;
      }
    }
  }

  Postotal = Ptotal;
  Poscnt = Pcnt;
  Negtotal = Ntotal;
  Negcnt = Ncnt;
  
  return Postotal + Poscnt + Negtotal + Negcnt;
}

int
main (int argc, char *argv[])
{
  int i, j, k;
  matrix A;

  /*
   * There is a matrix with UPPERLIMIT*UPPERLIMIT dimensions that needs to be filled up.
   * This many values need to be passed on the command-line as a consequence.
   */
  if (argc != UPPERLIMIT * UPPERLIMIT + 1)
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

  int val = matrix_count (A);
  
  printf("%d", val);

  return 0;
}
