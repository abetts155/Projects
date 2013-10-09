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
//==========> matrix_count : header 7
int __count_4_6 = 0;
int __count_5_6 = 0;
int __count_7_3 = 0; //Loop counter
//==========> matrix_count : header 9
int __count_7_8 = 0;
int __count_9_2 = 0; //Loop counter
//==========> matrix_count : header 1
int __count_10 = 0;
int __count_9_10 = 0;
  #endif
  int i, j;
  int Ptotal = 0;
  int Ntotal = 0;
  int Pcnt = 0;
  int Ncnt = 0;

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
      if (A[i][j] < 0) // 3
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
        __count_5_6++;
        #endif
        Ptotal += A[i][j];
        Pcnt++;
      }
    }
    #ifdef CBMC
    __count_7_8++;
    #endif
  }

  Postotal = Ptotal;
  Poscnt = Pcnt;
  Negtotal = Ntotal;
  Negcnt = Ncnt;
  
  #ifdef CBMC
  __count_9_10++;
  __count_10++;
  #endif
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
