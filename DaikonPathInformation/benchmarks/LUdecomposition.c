/* MDH WCET BENCHMARK SUITE. File version $Id: ludcmp.c,v 1.2 2006/01/27 13:15:28 jgn Exp $ */

/*************************************************************************/
/*                                                                       */
/*   SNU-RT Benchmark Suite for Worst Case Timing Analysis               */
/*   =====================================================               */
/*                              Collected and Modified by S.-S. Lim      */
/*                                           sslim@archi.snu.ac.kr       */
/*                                         Real-Time Research Group      */
/*                                        Seoul National University      */
/*                                                                       */
/*                                                                       */
/*        < Features > - restrictions for our experimental environment   */
/*                                                                       */
/*          1. Completely structured.                                    */
/*               - There are no unconditional jumps.                     */
/*               - There are no exit from loop bodies.                   */
/*                 (There are no 'break' or 'return' in loop bodies)     */
/*          2. No 'switch' statements.                                   */
/*          3. No 'do..while' statements.                                */
/*          4. Expressions are restricted.                               */
/*               - There are no multiple expressions joined by 'or',     */
/*                'and' operations.                                      */
/*          5. No library calls.                                         */
/*               - All the functions needed are implemented in the       */
/*                 source file.                                          */
/*                                                                       */
/*                                                                       */
/*************************************************************************/
/*                                                                       */
/*  FILE: ludcmp.c                                                       */
/*  SOURCE : Turbo C Programming for Engineering                         */
/*                                                                       */
/*  DESCRIPTION :                                                        */
/*                                                                       */
/*     Simultaneous linear equations by LU decomposition.                */
/*     The arrays a[][] and b[] are input and the array x[] is output    */
/*     row vector.                                                       */
/*     The variable n is the number of equations.                        */
/*     The input arrays are initialized in function main.                */
/*                                                                       */
/*                                                                       */
/*  REMARK :                                                             */
/*                                                                       */
/*  EXECUTION TIME :                                                     */
/*                                                                       */
/*                                                                       */
/*************************************************************************/


/* Changes:
 * JG 2005/12/12: Indented program. Removed unused variable nmax.
 */

/*
** Benchmark Suite for Real-Time Applications, by Sung-Soo Lim
**
**    III-4. ludcmp.c : Simultaneous Linear Equations by LU Decomposition
**                 (from the book C Programming for EEs by Hyun Soon Ahn)
*/

#define ARRAY_DIMENSION 6
double a[ARRAY_DIMENSION][ARRAY_DIMENSION], b[ARRAY_DIMENSION], x[ARRAY_DIMENSION];

static double 
fabs (double n)
{
  #ifdef CBMC
  int __count_38_40 = 0;
  int __count_39_40 = 0;
  #endif

  if (n >= 0)
  {
    #ifdef CBMC
    __count_38_40++;
    #endif
    return n;
  }
  else
  {
    #ifdef CBMC
    __count_39_40++; // TODO: check
    #endif
    return -n; // 39
  }
}

int
LUdecomposition (int n, double eps)
{
  #ifdef CBMC
  int __count_1_3 = 0;
  int __count_2_3 = 0;
  int __count_7_36 = 0;
  int __count_9_13 = 0;
  int __count_12_11 = 0;
  int __count_18_17 = 0;
  int __count_26_25 = 0;
  int __count_31_32 = 0;
  int __count_L34 = 0;
  int __count_L32 = 0;
  int __count_L28 = 0;
  int __count_L26 = 0;
  int __count_L22 = 0;
  int __count_L20 = 0;
  int __count_L18 = 0;
  int __count_L14 = 0;
  int __count_L12 = 0;
  #endif

  int i, j, k;
  double w, y[100];

  if (n > 99 || eps <= 0.0) // 1,2
  {
    #ifdef CBMC
    if (n>99) __count_1_3++;
    else if (eps <= 0.0) __count_2_3++;
    #endif
    return 999;
  }

  for (i = 0; 
    #ifdef CBMC
    __count_L22++,
    #endif
  i < n; i++) // 22
  {
    if (fabs(a[i][i]) <= eps) // 6
    {
      // 7
      #ifdef CBMC
      __count_7_36++;
      #endif
      return 1;
    }
  
    for (j = i + 1; 
      #ifdef CBMC
      __count_L14++,
      #endif
    j <= n; j++) // 14
    {
      w = a[j][i];
      if (i != 0) // 9
      {  
        for (k = 0; 
          #ifdef CBMC
          __count_L12++,
          #endif
        k < i; k++) // 12
        {
          #ifdef CBMC
          __count_12_11++;
          #endif
          w -= a[j][k] * a[k][i];
        }
      }
      #ifdef CBMC
      else __count_9_13++;
      #endif
      a[j][i] = w / a[i][i];
    }
    for (j = i + 1; 
      #ifdef CBMC
      __count_L20++,
      #endif
    j <= n; j++)  // 20
    {
      w = a[i + 1][j];
      for (k = 0; 
        #ifdef CBMC
        __count_L18++,
        #endif
      k <= i; k++) // 18
      {
        #ifdef CBMC
        __count_18_17++;
        #endif
        w -= a[i + 1][k] * a[k][j];
      }
      a[i + 1][j] = w;
    }
  }

  y[0] = b[0];
  
  for (i = 1; 
    #ifdef CBMC
    __count_L28++,
    #endif
  i <= n; i++) // 28
  {
    w = b[i];
    for (j = 0; 
      #ifdef CBMC
      __count_L26++,
      #endif
    j < i; j++) // 26
    {
      #ifdef CBMC
      __count_26_25++;
      #endif
      w -= a[i][j] * y[j];
    }
    y[i] = w;
  }
  
  x[n] = y[n] / a[n][n];
  for (i = n - 1; 
    #ifdef CBMC
    __count_L34++,
    #endif
  i >= 0; i--) // 34
  {
    w = y[i];
    for (j = i + 1; 
      #ifdef CBMC
      __count_L32++,
      #endif
    j <= n; j++) // 32
    {
      w -= a[i][j] * x[j];
      #ifdef CBMC
      __count_31_32++;
      #endif
    }
    x[i] = w / a[i][i];
  }
  
  return 0;
}

int 
main (int argc, char* argv[])
{
  int i, j, k, chkerr;
  double w;
  double eps = 1.0e-6;

  /*
   * There is 1 matrix of size ARRAY_DIMENSION * ARRAY_DIMENSION that need to be filled up.
   */
  if (argc != ARRAY_DIMENSION * ARRAY_DIMENSION +1)
  {
    return 1;
  }
  
  k = 0;
  for (i = 0; i <= ARRAY_DIMENSION - 1; i++) 
  {
    w = 0.0;
    for (j = 0; j <= ARRAY_DIMENSION - 1; j++) 
    {
      a[i][j] = atoi (argv[k + 1]);

      k++;
      if (i == j)
        a[i][j] *= 10.0;
      w += a[i][j];
    }
    b[i] = w;
  }

  chkerr = LUdecomposition (ARRAY_DIMENSION - 1, eps);
  
  printf("%d", chkerr);

  return 0;
}

