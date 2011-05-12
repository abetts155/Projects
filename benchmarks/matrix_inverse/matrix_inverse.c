/*
 * Matrix inversion taken from MDH suite and modified by Adam Betts to
 * consume a test vector supplied on the command line.
 *
 * For this program, the test vector consists of 9 integers since there is
 * a single 3*3 matrix to be initialised before inversion.
 */

#define UPPERLIMIT 3

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

void
minver (double a[][UPPERLIMIT], int row, int col, double eps)
{
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
    return;
  }

  w1 = 1.0;
  for (i = 0; i < row; i++)
  {
    work[i] = i;
  }

  for (k = 0; k < row; k++)
  {
    wmax = 0.0;
    for (i = k; i < row; i++)
    {
      w = fabs (a[i][k]);
      if (w > wmax)
      {
        wmax = w;
        r = i;
      }
    }
    pivot = a[r][k];
    api = fabs (pivot);

    if (api <= eps)
    {
      return;
    }

    w1 *= pivot;
    u = k * col;
    v = r * col;

    if (r != k)
    {
      w1 = -w;
      iw = work[k];
      work[k] = work[r];
      work[r] = iw;
      for (j = 0; j < row; j++)
      {
        s = u + j;
        t = v + j;
        w = a[k][j];
        a[k][j] = a[r][j];
        a[r][j] = w;
      }
    }

    for (i = 0; i < row; i++)
    {
      a[k][i] /= pivot;
    }

    for (i = 0; i < row; i++)
    {
      if (i != k)
      {
        v = i * col;
        s = v + k;
        w = a[i][k];
        if (w != 0.0)
        {
          for (j = 0; j < row; j++)
          {
            if (j != k)
            {
              a[i][j] -= w * a[k][j];
            }
          }
          a[i][k] = -w / pivot;
        }
      }
    }
    a[k][k] = 1.0 / pivot;
  }

  for (i = 0; i < row; i++)
  {
    while (1)
    {
      k = work[i];

      if (k == i)
      {
        break;
      }

      iw = work[k];
      work[k] = work[i];
      work[i] = iw;
      for (j = 0; j < row; j++)
      {
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
   * Need 9 values to fill up the matrix.
   */
  if (argc != 10)
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

  minver (a, UPPERLIMIT, UPPERLIMIT, eps);

  return 0;
}
