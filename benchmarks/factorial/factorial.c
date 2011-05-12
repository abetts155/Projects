/*
 * Factorial taken from MDH suite and modified by Adam Betts to consume a
 * test vector supplied on the command line.
 *
 * For this program, a one-element test vector is expected, the
 * factorial of which will be computed.
 */

int
factorial (int n)
{
  if (n == 0)
  {
    return 1;
  }
  else
  {
    return (n * factorial (n - 1));
  }
}

int
main (int argc, char *argv[])
{
  int fac;

  /*
   * One integer value must be supplied
   */
  if (argc != 2)
  {
    return 1;
  }

  fac = factorial (atoi (argv[1]));

  return 0;
}

