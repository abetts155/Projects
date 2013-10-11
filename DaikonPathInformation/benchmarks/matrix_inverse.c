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
//==========> matrix_inverse : header 38
int __count_38_37 = 0
int __count_38_37_L = 0; //Loop counter
//==========> matrix_inverse : header 28
int __count_25_26 = 0;
int __count_25_27 = 0;
int __count_28_25 = 0; //Loop counter
//==========> matrix_inverse : header 35
int __count_38_39 = 0;
int __count_35_36 = 0; //Loop counter
//==========> matrix_inverse : header 17
int __count_17_16 = 0;
int __count_17_16_L = 0; //Loop counter
//==========> matrix_inverse : header 31
int __count_22_30 = 0;
int __count_23_30 = 0;
int __count_28_29 = 0;
int __count_31_22 = 0; //Loop counter
//==========> matrix_inverse : header 20
int __count_20_19 = 0;
int __count_20_19_L = 0; //Loop counter
//==========> matrix_inverse : header 12
int __count_9_10 = 0;
int __count_9_11 = 0;
int __count_12_9 = 0; //Loop counter
//==========> matrix_inverse : header 41
int __count_35_40 = 0;
int __count_41_35 = 0; //Loop counter
//==========> matrix_inverse : header 33
int __count_14_18 = 0;
int __count_17_18 = 0;
int __count_33_8 = 0; //Loop counter
//==========> matrix_inverse : header 6
int __count_6_5 = 0;
int __count_6_5_L = 0; //Loop counter
//==========> matrix_inverse : header 1
int __count_45 = 0;
int __count_1_43 = 0;
int __count_2_43 = 0;
int __count_3_43 = 0;
int __count_13_44 = 0;
int __count_33_34 = 0;

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
    #ifdef CBMC
    __count_45++;
    #endif
    return;
  }

  w1 = 1.0;
  #ifdef CBMC
  __count_6_5_L = 0;
  #endif
  for (i = 0; i < row; i++) // 6
  {
    #ifdef CBMC
    __count_6_5++;
    __count_6_5_L++;
    #endif
    work[i] = i;
  }
  #ifdef CBMC
  assert(__count_6_5_L  <= 6); // Loop counter property
  #endif

  #ifdef CBMC
  __count_33_8 = 0;
  #endif
  for (k = 0; k < row; k++) // 33
  {
    #ifdef CBMC
    __count_33_8++;
    #endif
    wmax = 0.0;
    #ifdef CBMC
    __count_12_9 = 0;
    #endif
    for (i = k; i < row; i++) // 12
    {
      #ifdef CBMC
      __count_12_9++;
      #endif
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
    #ifdef CBMC
    assert(__count_12_9  <= 6); // Loop counter property
    #endif
    pivot = a[r][k];
    api = fabs (pivot);

    if (api <= eps) // 13 
    {
      // 44
      #ifdef CBMC
      __count_13_44++;
      __count_45++;
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
      #ifdef CBMC
      __count_17_16_L = 0;
      #endif
      for (j = 0; j < row; j++) // 17
      {
        #ifdef CBMC
        __count_17_16_L++;
        __count_17_16++;
        #endif
        s = u + j;
        t = v + j;
        w = a[k][j];
        a[k][j] = a[r][j];
        a[r][j] = w;
      }
      #ifdef CBMC
      assert(__count_17_16_L  <= 6); // Loop counter property
      __count_17_18++;
      #endif
    }
    #ifdef CBMC
    else __count_14_18++;
    #endif

    #ifdef CBMC
    __count_20_19_L = 0;
    #endif
    for (i = 0; i < row; i++) // 20
    {
      #ifdef CBMC
      __count_20_19_L++;
      __count_20_19++;
      #endif
      a[k][i] /= pivot;
    }
    #ifdef CBMC
    assert(__count_20_19_L  <= 6); // Loop counter property
    #endif

    #ifdef CBMC
    __count_31_22 = 0;
    #endif
    for (i = 0; i < row; i++) // 31
    {
      #ifdef CBMC
      __count_31_22++;
      #endif
      if (i != k) // 22
      {
        v = i * col;
        s = v + k;
        w = a[i][k];
        if (w != 0.0) // 23
        {
          #ifdef CBMC
          __count_28_25 = 0;
          #endif
          for (j = 0; j < row; j++) // 28
          {
            #ifdef CBMC
            __count_28_25++;
            #endif
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
          #ifdef CBMC
          assert(__count_28_25  <= 6); // Loop counter property
          __count_28_29++;
          #endif
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
    #ifdef CBMC
    assert(__count_31_22  <= 6); // Loop counter property
    #endif
    a[k][k] = 1.0 / pivot;
  }
  #ifdef CBMC
  assert(__count_33_8  <= 6); // Loop counter property
  __count_33_34++;
  #endif

  #ifdef CBMC
  __count_41_35 = 0;
  #endif
  for (i = 0; i < row; i++) // 41
  {
    #ifdef CBMC
    __count_41_35++;
    #endif
    #ifdef CBMC
    __count_35_36 = 0;
    #endif
    while (1) // 35
    {
      #ifdef CBMC
      __count_35_36++;
      #endif
      k = work[i];

      if (k == i)
      {
        break;
      }

      iw = work[k];
      work[k] = work[i];
      work[i] = iw;
      #ifdef CBMC
      __count_38_37_L = 0;
      #endif
      for (j = 0; j < row; j++) // 38
      {
        #ifdef CBMC
        __count_38_37_L++;
        __count_38_37++;
        #endif
        u = j * col;
        s = u + i;
        t = u + k;
        w = a[k][i];
        a[k][i] = a[k][k];
        a[k][k] = w;
      }
      #ifdef CBMC
      assert(__count_38_37_L  <= 6); // Loop counter property
      __count_38_39++;
      #endif
    }
    #ifdef CBMC
    assert(__count_35_36  <= 5); // Loop counter property
    __count_35_40++;
    #endif
  }
  #ifdef CBMC
  assert(__count_41_35  <= 6); // Loop counter property
  __count_45++;
  #endif

#ifdef CBMC


assert(__count_13_44 == 0); // Dead code
assert(__count_35_40 >= 5); // Lower capacity constraint
assert(__count_35_40 <= 5); // Upper capacity constraint
assert(__count_14_18 >= 1); // Lower capacity constraint
assert(__count_14_18 <= 4); // Upper capacity constraint
assert(__count_38_37 >= 5); // Lower capacity constraint
assert(__count_38_37 <= 20); // Upper capacity constraint
assert(__count_38_39 >= 1); // Lower capacity constraint
assert(__count_38_39 <= 4); // Upper capacity constraint
assert(__count_17_16 >= 5); // Lower capacity constraint
assert(__count_17_16 <= 20); // Upper capacity constraint
assert(__count_17_18 >= 1); // Lower capacity constraint
assert(__count_17_18 <= 4); // Upper capacity constraint
assert(__count_23_30 == 0); // Dead code
assert(__count_20_19 >= 25); // Lower capacity constraint
assert(__count_20_19 <= 25); // Upper capacity constraint
assert(__count_45 >= 1); // Lower capacity constraint
assert(__count_45 <= 1); // Upper capacity constraint
assert(__count_22_30 >= 5); // Lower capacity constraint
assert(__count_22_30 <= 5); // Upper capacity constraint
assert(__count_1_43 == 0); // Dead code
assert(__count_2_43 == 0); // Dead code
assert(__count_3_43 == 0); // Dead code
assert(__count_25_27 >= 20); // Lower capacity constraint
assert(__count_25_27 <= 20); // Upper capacity constraint
assert(__count_25_26 >= 80); // Lower capacity constraint
assert(__count_25_26 <= 80); // Upper capacity constraint
assert(__count_6_5 >= 5); // Lower capacity constraint
assert(__count_6_5 <= 5); // Upper capacity constraint
assert(__count_28_29 >= 20); // Lower capacity constraint
assert(__count_28_29 <= 20); // Upper capacity constraint
assert(__count_9_10 >= 6); // Lower capacity constraint
assert(__count_9_10 <= 13); // Upper capacity constraint
assert(__count_9_11 >= 2); // Lower capacity constraint
assert(__count_9_11 <= 9); // Upper capacity constraint
assert(__count_33_34 >= 1); // Lower capacity constraint
assert(__count_33_34 <= 1); // Upper capacity constraint
#endif
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
