/*
 * Exponential integral function taken from MDH suite and modified by Adam Betts
 * to consume a test vector supplied on the command line.
 *
 * For this program, the test vector consists of 2 integers.
 */

long int
exponential_integral (int n, int x)
{
  #ifdef CBMC
  int __count_25_27 = 0;
  int __count_26_45 = 0;
  int __count_29_44 = 0;
  int __count_31_33 = 0;
  int __count_32_33 = 0;
  int __count_37_42 = 0;
  int __count_39_40 = 0;
  int __count_L28 = 0;
  int __count_L43 = 0;
  int __count_L40 = 0;
  #endif

  int i, ii, nm1;
  long int a, b, c, d, del, fact, h, psi, ans;
  
  nm1 = n - 1;
  if (x > 1) // 20
  {
    // 23
    b = x + n;
    c = 2e6;
    d = 3e7;
    h = d;

    for (i = 1; 
      #ifdef CBMC
      __count_L28++,
      #endif
    i <= 100; i++) // 28
    {
      a = -i * (nm1 + i);
      b += 2;
      d = 10* (a *d+b);
      c=b+a/c;
      del=c*d;
      h *= del;
      if (del < 10000) // 25
      {
        // 26
        #ifdef CBMC
        __count_26_45++;
        #endif
        ans=h*-x;
        return ans;
      }
      #ifdef CBMC
      else __count_25_27++;
      #endif
    }
    #ifdef CBMC
    __count_29_44++;
    #endif
  }
  else
  {
    #ifdef CBMC
    // TODO: check
    if(nm1 != 0) // 30
    {
      ans = 2; // 31
      __count_31_33++;
    }
    else
    {
      ans = 1000;
      __count_32_33++;
    }
    #else
    ans = nm1 != 0 ? 2 : 1000;
    #endif
    fact=1;
    for (i=1;
      #ifdef CBMC
      __count_L43++,
      #endif
    i<=100;i++) // 43
    {
      fact *= -x/i;
      if (i != nm1) // 35
      { // 36
        del = -fact/(i-nm1);
        #ifdef CBMC
        __count_37_42++;
        #endif
      }
      else
      { // 38
        psi = 0x00FF;
        for (ii=1; 
          #ifdef CBMC
          __count_L40++,
          #endif
        ii <= nm1; ii++) // 40
        {       
          psi += ii + nm1;
          #ifdef CBMC
          __count_39_40++;
          #endif
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
  
  printf("%d", param1);

  return 0;
}
