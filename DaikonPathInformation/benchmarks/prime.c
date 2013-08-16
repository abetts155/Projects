/*
 * Prime program taken from MDH suite and modified by Adam Betts
 * to consume a test vector supplied on the command line.
 *
 * For this program, the test vector is a single element. The program
 * determines if it is prime or not.
 */

unsigned char
divides (unsigned int n, unsigned int m)
{
  return m % n == 0;
}

unsigned char
even (unsigned int n)
{
  return divides (2, n);
}

unsigned char
prime (unsigned int n)
{
  int i;

  if (even (n))
  {
    return n == 2;
  }

  for (i = 3; i * i <= n; i += 2)
  {
    if (divides (i, n))
    {
      return 0;
    }
  }
  return n > 1;
}

int
main (int argc, char *argv[])
{
  unsigned char answer;
  unsigned int number;
 
  /*
   * One integer must be supplied
   */
  if (argc != 2)
  {
    return 1;
  }
  
  number = atoi(argv[1]);
  if (number < 2)
  {
    return 1;
  }

  answer = prime (number);
  printf("%c", answer); 

  return 0;
}
