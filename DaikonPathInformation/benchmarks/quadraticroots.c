/*
 * Root computation of a quadratic equation (qurt) taken from MDH suite and
 * modified by Adam Betts to consume a test vector supplied on the command line.
 *
 * For this program, the test vector is the 3 integer coefficients of a
 * polynomial ax^2 + bx + c where a /= 0
 */

double
fabs (double n)
{
  if (n >= 0)
  {
    return n;
  }
  else
  {
    return -n;
  }
}

double
sqrt (double val)
{
#ifdef CBMC
//==========> sqrt : header 8
int __count_4_7 = 0;
int __count_5_6 = 0;
int __count_5_7 = 0;
int __count_8_4 = 0; //Loop counter
//==========> sqrt : header 1
int __count_9 = 0;
int __count_2_9 = 0;
int __count_8_9 = 0;
#endif
  double x = val / 10;
  double dx;
  double diff;
  double min_tol = 0.00001;
  int i;
  int flag = 0;

  if (val == 0)
  {
    x = 0;
    #ifdef CBMC
    __count_2_9++;
    #endif
  }
  else
  {
    #ifdef CBMC
    __count_8_4 = 0;
    #endif
    for (i = 1; i < 20; i++) // 8
    {
      #ifdef CBMC
      __count_8_4++;
      #endif
      if (!flag)
      {
        dx = (val - (x * x)) / (2.0 * x);
        x = x + dx;
        diff = val - (x * x);
        if (fabs (diff) <= min_tol)
        {
          #ifdef CBMC
          __count_5_6++;
          #endif
          flag = 1;
        }
        #ifdef CBMC
        else __count_5_7++;
        #endif
      }
      else
      {
        x = x;
        #ifdef CBMC
        __count_4_7++;
        #endif
      }
    }
    #ifdef CBMC
    __count_8_9++;
    #endif
  }
  #ifdef CBMC
  __count_9++;
  #endif
  return x;
}

void
quadraticroots (double a[])
{
#ifdef CBMC
//==========> quadraticroots : header 10
int __count_18 = 0;
int __count_10_17 = 0;
int __count_12_13 = 0;
int __count_14_15 = 0;
int __count_16_18 = 0;
#endif
  double x1[2];
  double x2[2];
  double d;
  double w1;
  double w2;

  if (a[0] == 0.0) // 10
  {
    #ifdef CBMC
    __count_10_17++;
    __count_18++;
    #endif
    return 999;
  }

  d = a[1] * a[1] - 4 * a[0] * a[2];
  w1 = 2.0 * a[0];
  w2 = sqrt (fabs (d));

  if (d > 0.0) // 12
  {

    x1[0] = (-a[1] + w2) / w1;
    x1[1] = 0.0;
    x2[0] = (-a[1] - w2) / w1;
    x2[1] = 0.0;
    #ifdef CBMC
    __count_12_13++;
    __count_18++;
    #endif
    return 0;
  }
  else if (d == 0.0)
  {
    x1[0] = -a[1] / w1;
    x1[1] = 0.0;
    x2[0] = x1[0];
    x2[1] = 0.0;
    #ifdef CBMC
    __count_14_15++;
    __count_18++;
    #endif
    return 0;
  }
  else
  {
    #ifdef CBMC
    __count_16_18++;
    #endif
    w2 /= w1;
    x1[0] = -a[1] / w1;
    x1[1] = w2;
    x2[0] = x1[0];
    x2[1] = -w2;
    #ifdef CBMC
    __count_18++;
    #endif
    return 0;
  }
}

int
main (int argc, char *argv[])
{
  double a[3];
  /*
   * Three integers must be supplied
   */
  if (argc != 4)
  {
    return 1;
  }
  if (*argv[1] == '0')
  {
    return 1;
  }

  a[0] = atoi (argv[1]);
  a[1] = atoi (argv[2]);
  a[2] = atoi (argv[3]);

  quadraticroots(a);
  printf("%lf", a[0]);

  return 0;
}
