/*
 * Square root (Sqrt) program taken from MDH suite and modified by Adam Betts
 * to consume a test vector supplied on the command line.
 *
 * For this program, the test vector is a single element for which the square root
 * is computed.
 */

float
squareroot (float val)
{
  float x = val / 10;
  float dx;
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

int
main (int argc, char *argv[])
{
  float val;
  unsigned int input;

  /*
   * One integer must be supplied
   */
  if (argc != 2)
  {
    return 1;
  }

  input = atoi(argv[1]);
  val = squareroot (input);

  return 0;
}
