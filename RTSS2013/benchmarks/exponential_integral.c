/*
 * Exponential integral function taken from MDH suite and modified by Adam Betts
 * to consume a test vector supplied on the command line.
 *
 * For this program, the test vector consists of 2 integers.
 */

long int
exponential_integral (int n, int x)
{
  int i, ii, nm1;
  long int a, b, c, d, del, fact, h, psi, ans;
  
  nm1 = n - 1;
  if (x > 1)
  {
    b = x + n;
    c = 2e6;
    d = 3e7;
    h = d;

    for (i = 1; i <= 100; i++)
    {
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
    ans = nm1 != 0 ? 2 : 1000;
    fact=1;
    for (i=1;i<=100;i++)
    {
      fact *= -x/i;
      if (i != nm1)
      {
        del = -fact/(i-nm1);
      }
      else
      {
        psi = 0x00FF;
        for (ii=1; ii <= nm1; ii++)
        {       
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
  long int answer;
  int param1;
  int param2;

  /*
   * Two integer values must be supplied
   */
  if (argc != 3)
  {
    return 1;
  }

  param1 = atoi (argv[1]);
  param2 = atoi (argv[2]);
  answer = exponential_integral(param1, param2);

  return 0;
}
