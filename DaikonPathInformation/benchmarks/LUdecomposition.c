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
//==========> fabs : header 37
int __count_40 = 0;
int __count_37_38 = 0;
int __count_37_39 = 0;
#endif

  if (n >= 0)
  {
    #ifdef CBMC
    __count_37_38++;
    __count_40++;
    #endif
    return n;
  }
  else
  {
    #ifdef CBMC
    __count_37_39++;
    __count_40++;
    #endif
    return -n; // 39
  }
}

int
LUdecomposition (int n, double eps)
{
#ifdef CBMC
//==========> ludecomposition : header 18
int __count_18_17 = 0; //Loop counter
//==========> ludecomposition : header 12
int __count_12_11 = 0; //Loop counter
//==========> ludecomposition : header 32
int __count_32_31 = 0; //Loop counter
//==========> ludecomposition : header 26
int __count_26_25 = 0; //Loop counter
//==========> ludecomposition : header 20
int __count_18_19 = 0;
int __count_20_16 = 0; //Loop counter
//==========> ludecomposition : header 14
int __count_9_13 = 0;
int __count_12_13 = 0;
int __count_14_9 = 0; //Loop counter
//==========> ludecomposition : header 34
int __count_32_33 = 0;
int __count_34_30 = 0; //Loop counter
//==========> ludecomposition : header 28
int __count_26_27 = 0;
int __count_28_24 = 0; //Loop counter
//==========> ludecomposition : header 22
int __count_14_15 = 0;
int __count_22_5 = 0; //Loop counter
//==========> ludecomposition : header 1
int __count_36 = 0;
int __count_1_3 = 0;
int __count_2_3 = 0;
int __count_6_7 = 0;
int __count_34_35 = 0;
#endif

  int i, j, k;
  double w, y[100];

  if (n > 99 || eps <= 0.0) // 1,2
  {
    #ifdef CBMC
    if (n>99) __count_1_3++;
    else if (eps <= 0.0) __count_2_3++;

    __count_36++;
    #endif
    return 999;
  }

  #ifdef CBMC
  __count_22_5 = 0;
  #endif
  for (i = 0; i < n; i++) // 22
  {
    #ifdef CBMC
    __count_22_5++;
    #endif
    if (fabs(a[i][i]) <= eps) // 6
    {
      #ifdef CBMC
      __count_6_7++;
      #endif
      // 7
      #ifdef CBMC
      __count_36++;
      #endif
      return 1;
    }
  
    #ifdef CBMC
    __count_14_9 = 0;
    #endif
    for (j = i + 1; j <= n; j++) // 14
    {
      #ifdef CBMC
      __count_14_9++;
      #endif
      w = a[j][i];
      if (i != 0) // 9
      {  
        #ifdef CBMC
        __count_12_11 = 0;
        #endif
        for (k = 0; k < i; k++) // 12
        {
          #ifdef CBMC
          __count_12_11++;
          #endif
          w -= a[j][k] * a[k][i];
        }
        #ifdef CBMC
        __count_12_13++;
        #endif
      }
      #ifdef CBMC
      else __count_9_13++;
      #endif
      a[j][i] = w / a[i][i];
    }
    #ifdef CBMC
    __count_14_15++;
    #endif

    #ifdef CBMC
    __count_20_16 = 0;
    #endif
    for (j = i + 1; j <= n; j++)  // 20
    {
      #ifdef CBMC
      __count_20_16++;
      #endif
      w = a[i + 1][j];
      #ifdef CBMC
      __count_18_17 = 0;
      #endif
      for (k = 0; k <= i; k++) // 18
      {
        #ifdef CBMC
        __count_18_17++;
        #endif
        w -= a[i + 1][k] * a[k][j];
      }
      #ifdef CBMC
      __count_18_19++;
      #endif
      a[i + 1][j] = w;
    }
  }

  y[0] = b[0];
  
  #ifdef CBMC
  __count_28_24 = 0;
  #endif
  for (i = 1; i <= n; i++) // 28
  {
    #ifdef CBMC
    __count_28_24++;
    #endif
    w = b[i];
    #ifdef CBMC
    __count_26_25 = 0;
    #endif
    for (j = 0; j < i; j++) // 26
    {
      #ifdef CBMC
      __count_26_25++;
      #endif
      w -= a[i][j] * y[j];
    }
    #ifdef CBMC
    __count_26_27++;
    #endif
    y[i] = w;
  }
  
  x[n] = y[n] / a[n][n];
  #ifdef CBMC
  __count_34_30 = 0;
  #endif
  for (i = n - 1; i >= 0; i--) // 34
  {
    #ifdef CBMC
    __count_34_30++;
    #endif
    w = y[i];
    #ifdef CBMC
    __count_32_31 = 0;
    #endif
    for (j = i + 1; j <= n; j++) // 32
    {
      #ifdef CBMC
      __count_32_31++;
      #endif
      w -= a[i][j] * x[j];
    }
    #ifdef CBMC
    __count_32_33++;
    #endif
    x[i] = w / a[i][i];
  }
  #ifdef CBMC
  __count_34_35++;
  #endif
  
  #ifdef CBMC
  __count_36++;
  #endif
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

