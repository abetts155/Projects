/*
 * Janne Complex taken from MDH suite and modified by Adam Betts to consume a
 * test vector supplied on the command line.
 *
 * For this program, a two-element test vector is expected.
 */

int
janne_complex (int a, int b)
{
  while (a < 30)
  {
    while (b < a)
    {
      if (b > 5)
      {
        b = b * 3;
      }
      else
      {
        b = b + 2;
      }
      if (b >= 10 && b <= 12)
      {
        a = a + 10;
      }
      else
      {
        a = a + 1;
      }
    }
    a = a + 2;
    b = b - 10;
  }
  return 1;
}

int
main (int argc, char *argv[])
{
  int a, b, answer;

  /*
   * Two integer values must be supplied
   */
  if (argc != 3)
  {
    return 1;
  }

  a = atoi (argv[1]);
  b = atoi (argv[2]);
  answer = janne_complex (a, b);

  return 0;
}

