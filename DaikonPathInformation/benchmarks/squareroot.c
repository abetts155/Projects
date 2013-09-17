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
  int __count_4_7 = 0;
  int __count_5_7 = 0;
  int __count_6_7 = 0;
  int __count_2_9 = 0;
  int __count_L10 = 0;
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
  }
  else
  {
    for (i = 1; 
      #ifdef CBMC
      __count_L10++,
      #endif
      i < 20; i++)
    {
      if (!flag)
      {
        dx = (val - (x * x)) / (2.0 * x);
        x = x + dx;
        diff = val - (x * x);
        if (fabs (diff) <= min_tol)
        {
          #ifdef CBMC
          __count_6_7++;
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
  
  printf("%f", val);

  return 0;
}
