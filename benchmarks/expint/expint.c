/*
 * Exponential integral function taken from MDH suite and modified by Adam Betts
 * to consume a test vector supplied on the command line.
 *
 * For this program, the test vector consists of 2 integers.
 */

long int
expint (int n, int x)
{
  /*
   * Counters added to record loop iterations. Used by GA to evaluate fitness
   */
  int counter1 = 0;
  int counter2 = 0;
  int counter3 = 0;

  int i, ii, nm1;
  long int a, b, c, d, del, fact, h, psi, ans;
  
  /* arg = 50 => 49 */
  nm1 = n - 1;

  if (x > 1)
  {
    b = x + n;
    c = 2e6;
    d = 3e7;
    h = d;

    /* MAXIT is 100 */
    for (i = 1; i <= 100; i++)
    {
      counter1++;
      a = -i * (nm1 + i);
      b += 2;
      d = 10* (a *d+b);
      c=b+a/c;
      del=c*d;
      h *= del;
      if (del < 10000)
      {
        ans=h*-x;
        return ans;
      }
    }
  }
  else
  {
    /*
     * For the current argument, will always take '2' path here
     */
    ans = nm1 != 0 ? 2 : 1000;
    fact=1;
    for (i=1;i<=100;i++)
    {
      counter2++;
      fact *= -x/i;
      if (i != nm1)
      {
        /*
         * Depends on parameter n
         */
        del = -fact/(i-nm1);
      }
      else
      {
        /*
         * This fat piece only runs ONCE on iteration 49
         */
        psi = 0x00FF;
        for (ii=1; ii <= nm1; ii++)
        {
          counter3++;         
          psi += ii + nm1;
        }
        del = psi + fact * x * x + (8* x ) << 4 - x;
      }
      ans += del;
    }

  }
  return ans;
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

  expint (atoi (argv[1]), atoi (argv[2]));

  return 0;
}
