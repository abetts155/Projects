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
#ifdef CBMC
//==========> squareroot : header 8
int __count_4_7 = 0;
int __count_5_6 = 0;
int __count_5_7 = 0;
int __count_8_4 = 0; //Loop counter
//==========> squareroot : header 1
int __count_9 = 0;
int __count_2_9 = 0;
int __count_8_9 = 0;
#endif

  float x = val / 10;
  float dx;
  double diff;
  double min_tol = 0.00001;
  int i;
  int flag = 0;

  if (val == 0)
  {
    x = 0;
    #ifdef CBMC
    __count_2_9++;
    #endif
  }
  else
  {
    #ifdef CBMC
    __count_8_4 = 0;
    #endif
    for (i = 1; i < 20; i++)
    {
      #ifdef CBMC
      __count_8_4++;
      #endif
      if (!flag)
      {
        dx = (val - (x * x)) / (2.0 * x);
        x = x + dx;
        diff = val - (x * x);
        if (fabs (diff) <= min_tol) // 5
        {
          #ifdef CBMC
          __count_5_6++;
          #endif
          flag = 1;
        }
        #ifdef CBMC
        else __count_5_7++;
        #endif
      }
      else
      {
        #ifdef CBMC
        __count_4_7++;
        #endif
        x = x;
      }
    }
    #ifdef CBMC
    assert(__count_8_4  <= 20); // Loop counter property
    __count_8_9++;
    #endif
  }
  #ifdef CBMC
  __count_9++;
  #endif

#ifdef CBMC

assert(__count_9 >= 1); // Lower capacity constraint
assert(__count_9 <= 1); // Upper capacity constraint
//assert(__count_2_9 == 0); // Dead code
//assert(__count_4_7 <= 7); // Upper capacity constraint
assert(__count_5_6 <= 1); // Upper capacity constraint
//assert(__count_5_7 >= 11); // Lower capacity constraint
assert(__count_5_7 <= 19); // Upper capacity constraint
//assert(__count_8_9 >= 1); // Lower capacity constraint
assert(__count_8_9 <= 1); // Upper capacity constraint
assert(__count_4_7 > 0 ==> __count_5_6 > 0); // Mutual inclusion
assert(__count_5_6 > 0 ==> __count_4_7 > 0); // Mutual inclusion
assert(__count_4_7 > 0 ==> __count_9 > 0); // Execution dependence
//assert(__count_4_7 > 0 ==> __count_5_7 > 0); // Execution dependence
assert(__count_4_7 > 0 ==> __count_8_9 > 0); // Execution dependence
assert(__count_5_6 > 0 ==> __count_9 > 0); // Execution dependence
//assert(__count_5_6 > 0 ==> __count_5_7 > 0); // Execution dependence
assert(__count_5_6 > 0 ==> __count_8_9 > 0); // Execution dependence
#endif

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
  
  printf("%f", val);

  return 0;
}
