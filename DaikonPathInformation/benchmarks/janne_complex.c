/*
 * Janne Complex taken from MDH suite and modified by Adam Betts to consume a
 * test vector supplied on the command line.
 *
 * For this program, a two-element test vector is expected.
 */

int
janne_complex (int a, int b)
{
  #ifdef CBMC
  int __count_2_3 = 0;
  int __count_2_4 = 0;
  int __count_5_8 = 0;
  int __count_6_8 = 0;
  int __count_7_9 = 0;
  int __count_L9 = 0;
  int __count_L11 = 0;
  #endif
  while (
    #ifdef CBMC
    __count_L11++,
    #endif
  a < 30) // 11
  {
    while (
      #ifdef CBMC
      __count_L9++,
      #endif
    b < a) // 9
    {
      if (b > 5) // 2
      {
        #ifdef CBMC
        __count_2_3++;
        #endif
        b = b * 3; // 3
      }
      else
      {
        #ifdef CBMC
        __count_2_4++;
        #endif
        b = b + 2; // 4
      }
 
      if (b >= 10 && b <= 12) // 5, 6
      {
        a = a + 10; // 7
        #ifdef CBMC
        __count_7_9++;
        #endif
      }
      else
      {
        #ifdef CBMC
        if (b >= 10) __count_6_8++;
        else __count_5_8++;
        #endif
        a = a + 1; // 8
      }
    }
    a = a + 2;
    b = b - 10;
  }
  
  return a;
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

  int val = janne_complex (atoi(argv[1]), atoi(argv[2]));
  printf("%d", val);

  return 0;
}

