/*
 * Matrix inversion taken from MDH suite and modified by Adam Betts to
 * consume a test vector supplied on the command line.
 *
 * For this program, the test vector consists of 9 integers since there is
 * a single UPPERLIMIT*UPPERLIMIT matrix to be initialised before inversion.
 */

#define UPPERLIMIT 5

double
fabs (double n)
{
  if (n >= 0)
    return n;
  else
    return -n;
}

int
matrix_inverse (double a[][UPPERLIMIT], int row, int col, double eps)
{
  #ifdef CBMC
  int __count_1_43 = 0;
  int __count_2_43 = 0;
  int __count_3_43 = 0;
  int __count_5_6 = 0;
  int __count_9_10 = 0;
  int __count_9_11 = 0;
  int __count_14_18 = 0;
  int __count_17_16 = 0;
  int __count_20_19 = 0;
  int __count_22_30 = 0;
  int __count_23_30 = 0;
  int __count_25_26 = 0;
  int __count_25_27 = 0;
  int __count_38_37 = 0;
  int __count_44_45 = 0;

  int __count_L41 = 0;
  int __count_L35 = 0;
  int __count_L38 = 0;
  int __count_L33 = 0;
  int __count_L17 = 0;
  int __count_L31 = 0;
  int __count_L28 = 0;
  int __count_L20 = 0;
  int __count_L12 = 0;
  int __count_L6 = 0;

  #endif

  int work[500];
  int i;
  int j;
  int k;
  int r;
  int iw;
  int s;
  int t;
  int u;
  int v;
  double w;
  double wmax;
  double pivot;
  double api;
  double w1;
  double b[UPPERLIMIT][UPPERLIMIT];
  double c[UPPERLIMIT][UPPERLIMIT];
  double e[UPPERLIMIT][UPPERLIMIT];

  if (row < 2 || row > 500 || eps <= 0.0)
  {
    #ifdef CBMC
    // TODO: check
    if (row < 2)
      __count_1_43++;
    else if (row > 500)
      __count_2_43++;
    else if (eps <= 0.0)
      __count_3_43++;
    #endif
    return;
  }

  w1 = 1.0;
  for (i = 0; 
  #ifdef CBMC
  __count_L6++,
  #endif
  i < row; i++) // 6
  {
    work[i] = i;
    #ifdef CBMC
    __count_5_6++;
    #endif
  }

  for (k = 0; 
  #ifdef CBMC
  __count_L33++,
  #endif
  k < row; k++) // 33
  {
    wmax = 0.0;
    for (i = k; 
    #ifdef CBMC
    __count_L12++,
    #endif
    i < row; i++) // 12
    {
      w = fabs (a[i][k]);
      if (w > wmax)
      {
        #ifdef CBMC
        __count_9_10++;
        #endif
        wmax = w;
        r = i;
      }
      #ifdef CBMC
      else __count_9_11++;
      #endif
    }
    pivot = a[r][k];
    api = fabs (pivot);

    if (api <= eps) // 13 
    {
      // 44
      #ifdef CBMC
      __count_44_45++;
      #endif
      return;
    }

    w1 *= pivot;
    u = k * col;
    v = r * col;

    if (r != k) // 14
    {
      w1 = -w;
      iw = work[k];
      work[k] = work[r];
      work[r] = iw;
      for (j = 0; 
      #ifdef CBMC
      __count_L17++,
      #endif
      j < row; j++) // 17
      {
        #ifdef CBMC
        __count_17_16++;
        #endif
        s = u + j;
        t = v + j;
        w = a[k][j];
        a[k][j] = a[r][j];
        a[r][j] = w;
      }
    }
    #ifdef CBMC
    else __count_14_18++;
    #endif

    for (i = 0; 
    #ifdef CBMC
    __count_L20++,
    #endif
    i < row; i++) // 20
    {
      #ifdef CBMC
      __count_20_19++;
      #endif
      a[k][i] /= pivot;
    }

    for (i = 0; 
    #ifdef CBMC
    __count_L31++,
    #endif
    i < row; i++) // 31
    {
      if (i != k) // 22
      {
        v = i * col;
        s = v + k;
        w = a[i][k];
        if (w != 0.0) // 23
        {
          for (j = 0; 
          #ifdef CBMC
          __count_L28++,
          #endif
          j < row; j++) // 28
          {
            if (j != k) // 25
            {
              #ifdef CBMC
              __count_25_26++;
              #endif
              a[i][j] -= w * a[k][j];
            }
            #ifdef CBMC
            else __count_25_27++;
            #endif
          }
          a[i][k] = -w / pivot;
        }
        #ifdef CBMC
        else __count_23_30++;
        #endif
      }
      #ifdef CBMC
      else __count_22_30++;
      #endif
    }
    a[k][k] = 1.0 / pivot;
  }

  for (i = 0; 
  #ifdef CBMC
  __count_L41++,
  #endif
  i < row; i++) // 41
  {
    while (
    #ifdef CBMC
    __count_L35++,
    #endif
    1) // 35
    {
      k = work[i];

      if (k == i)
      {
        break;
      }

      iw = work[k];
      work[k] = work[i];
      work[i] = iw;
      for (j = 0; 
      #ifdef CBMC
      __count_L38++,
      #endif
      j < row; j++) // 38
      {
        #ifdef CBMC
        __count_38_37++;
        #endif
        u = j * col;
        s = u + i;
        t = u + k;
        w = a[k][i];
        a[k][i] = a[k][k];
        a[k][k] = w;
      }
    }
  }
}

int
main (int argc, char **argv)
{
  int i;
  int j;
  int k;
  double a[UPPERLIMIT][UPPERLIMIT];
  double eps = 1.0e-6;

  /*
   * Need UPPERLIMIT*UPPERLIMIT values to fill up the matrix.
   */
  if (argc != UPPERLIMIT*UPPERLIMIT+1)
  {
    return 1;
  }

  k = 0;
  for (i = 0; i < UPPERLIMIT; ++i)
  {
    for (j = 0; j < UPPERLIMIT; ++j)
    {
      a[i][j] = atoi (argv[k + 1]);
      k++;
    }
  }

  int val = matrix_inverse (a, UPPERLIMIT, UPPERLIMIT, eps);
    
  printf("%d", val);

  return 0;
}
