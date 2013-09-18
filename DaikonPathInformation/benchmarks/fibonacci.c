
int 
fibonacci (int n)
{
  #ifdef CBMC
  int __count_2_9 = 0;
  int __count_4_9 = 0;
  int __count_7_6 = 0;
  int __count_L7 = 0;
  #endif

  if (n == 0) // 1 
  {
    #ifdef CBMC
    __count_2_9++;
    #endif
    return 0; // 2
  }
  if (n == 1) // 3
  {
    #ifdef CBMC
    __count_4_9++;
    #endif
    return 1; // 4
  }

// 5
  int prevPrev = 0;
  int prev     = 1;
  int result   = 0;
  int i;

  for (i = 2; 
    #ifdef CBMC
    __count_L7++,
    #endif
  i <= n; i++) // 7
  {
    #ifdef CBMC
    __count_7_6++;
    #endif
    result   = prev + prevPrev;
    prevPrev = prev;
    prev     = result;
  }
  return result;
}

int 
main (int argc, char *argv[])
{
  int num;

  if (argc != 2)
  {
    return 1;
  }
  
  num = atoi(argv[1]);
  int val = fibonacci (num);
  printf("%d", val);
  
  return 0;
}
