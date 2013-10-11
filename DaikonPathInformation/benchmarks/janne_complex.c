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
//==========> janne_complex : header 9
int __count_2_3 = 0;
int __count_2_4 = 0;
int __count_5_8 = 0;
int __count_6_7 = 0;
int __count_6_8 = 0;
int __count_9_2 = 0; //Loop counter
//==========> janne_complex : header 11
int __count_9_10 = 0;
int __count_11_9 = 0; //Loop counter
//==========> janne_complex : header 1
int __count_12 = 0;
int __count_11_12 = 0;
#endif
  #ifdef CBMC
  __count_11_9 = 0;
  #endif
  while (a < 30) // 11
  {
    #ifdef CBMC
    __count_11_9++;
    #endif

    #ifdef CBMC
    __count_9_2 = 0;
    #endif
    while (b < a) // 9
    {
      #ifdef CBMC
      __count_9_2++;
      #endif
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
        #ifdef CBMC
        __count_6_7++;
        #endif
        a = a + 10; // 7
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
    #ifdef CBMC
    assert(__count_9_2  <= 7); // Loop counter property
    __count_9_10++;
    #endif
    a = a + 2;
    b = b - 10;
  }
  #ifdef CBMC
  assert(__count_11_9  <= 11); // Loop counter property
  __count_11_12++;
  __count_12++;
  #endif

#ifdef CBMC
assert(__count_12 >= 1); // Lower capacity constraint
assert(__count_12 <= 1); // Upper capacity constraint
assert(__count_2_3 <= 5); // Upper capacity constraint
assert(__count_2_4 <= 7); // Upper capacity constraint
assert(__count_5_8 <= 7); // Upper capacity constraint
assert(__count_6_8 <= 5); // Upper capacity constraint
assert(__count_6_7 == 0); // Dead code
assert(__count_9_10 <= 10); // Upper capacity constraint
assert(__count_11_12 >= 1); // Lower capacity constraint
assert(__count_11_12 <= 1); // Upper capacity constraint
assert(__count_2_3 > 0 ==> __count_6_8 > 0); // Mutual inclusion
assert(__count_6_8 > 0 ==> __count_2_3 > 0); // Mutual inclusion
assert(__count_2_3 > 0 ==> __count_9_10 > 0); // Mutual inclusion
assert(__count_9_10 > 0 ==> __count_2_3 > 0); // Mutual inclusion
assert(__count_2_4 > 0 ==> __count_5_8 > 0); // Mutual inclusion
assert(__count_5_8 > 0 ==> __count_2_4 > 0); // Mutual inclusion
assert(__count_6_8 > 0 ==> __count_9_10 > 0); // Mutual inclusion
assert(__count_9_10 > 0 ==> __count_6_8 > 0); // Mutual inclusion
assert(__count_2_3 > 0 ==> __count_12 > 0); // Execution dependence
assert(__count_2_3 > 0 ==> __count_11_12 > 0); // Execution dependence
assert(__count_2_4 > 0 ==> __count_12 > 0); // Execution dependence
assert(__count_2_4 > 0 ==> __count_2_3 > 0); // Execution dependence
assert(__count_2_4 > 0 ==> __count_6_8 > 0); // Execution dependence
assert(__count_2_4 > 0 ==> __count_9_10 > 0); // Execution dependence
assert(__count_2_4 > 0 ==> __count_11_12 > 0); // Execution dependence
assert(__count_5_8 > 0 ==> __count_12 > 0); // Execution dependence
assert(__count_5_8 > 0 ==> __count_2_3 > 0); // Execution dependence
assert(__count_5_8 > 0 ==> __count_6_8 > 0); // Execution dependence
assert(__count_5_8 > 0 ==> __count_9_10 > 0); // Execution dependence
assert(__count_5_8 > 0 ==> __count_11_12 > 0); // Execution dependence
assert(__count_6_8 > 0 ==> __count_12 > 0); // Execution dependence
assert(__count_6_8 > 0 ==> __count_11_12 > 0); // Execution dependence
assert(__count_9_10 > 0 ==> __count_12 > 0); // Execution dependence
assert(__count_9_10 > 0 ==> __count_11_12 > 0); // Execution dependence
#endif

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

