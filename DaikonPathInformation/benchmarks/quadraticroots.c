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
  double x = val / 10;
  double dx;
  double diff;
  double min_tol = 0.00001;
  int i;
  int flag = 0;

  if (val == 0)
  {
    x = 0;
  }
  else
  {
    for (i = 1; i < 20; i++)
    {
      if (!flag)
      {
        dx = (val - (x * x)) / (2.0 * x);
        x = x + dx;
        diff = val - (x * x);
        if (fabs (diff) <= min_tol)
        {
          flag = 1;
        }
      }
      else
      {
        x = x;
      }
    }
  }
  return x;
}

void
quadraticroots (double a[])
{
  double x1[2];
  double x2[2];
  double d;
  double w1;
  double w2;

  if (a[0] == 0.0)
  {
    return 999;
  }

  d = a[1] * a[1] - 4 * a[0] * a[2];
  w1 = 2.0 * a[0];
  w2 = sqrt (fabs (d));

  if (d > 0.0)
  {
    x1[0] = (-a[1] + w2) / w1;
    x1[1] = 0.0;
    x2[0] = (-a[1] - w2) / w1;
    x2[1] = 0.0;
    return 0;
  }
  else if (d == 0.0)
  {
    x1[0] = -a[1] / w1;
    x1[1] = 0.0;
    x2[0] = x1[0];
    x2[1] = 0.0;
    return 0;
  }
  else
  {
    w2 /= w1;
    x1[0] = -a[1] / w1;
    x1[1] = w2;
    x2[0] = x1[0];
    x2[1] = -w2;
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
