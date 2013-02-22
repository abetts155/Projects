/*
 * Janne Complex taken from MDH suite and modified by Adam Betts to consume a
 * test vector supplied on the command line.
 *
 * For this program, a two-element test vector is expected.
 */

void
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
}

int
main (int argc, char *argv[])
{
  /*
   * Two integer values must be supplied
   */
  if (argc != 3)
  {
    return 1;
  }

  janne_complex (atoi(argv[1]), atoi(argv[2]));

  return 0;
}

