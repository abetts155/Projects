/*************************************************************************/
/*                                                                       */
/*   SNU-RT Benchmark Suite for Worst Case Timing Analysis               */
/*   =====================================================               */
/*                              Collected and Modified by S.-S. Lim      */
/*                                           sslim@archi.snu.ac.kr       */
/*                                         Real-Time Research Group      */
/*                                        Seoul National University      */
/*                                                                       */
/*                                                                       */
/*        < Features > - restrictions for our experimental environment   */
/*                                                                       */
/*          1. Completely structured.                                    */
/*               - There are no unconditional jumps.                     */
/*               - There are no exit from loop bodies.                   */
/*                 (There are no 'break' or 'return' in loop bodies)     */
/*          2. No 'switch' statements.                                   */
/*          3. No 'do..while' statements.                                */
/*          4. Expressions are restricted.                               */
/*               - There are no multiple expressions joined by 'or',     */
/*                'and' operations.                                      */
/*          5. No library calls.                                         */
/*               - All the functions needed are implemented in the       */
/*                 source file.                                          */
/*                                                                       */
/*                                                                       */
/*************************************************************************/
/*                                                                       */
/*  FILE: fft1.c                                                         */
/*  SOURCE : Turbo C Programming for Engineering by Hyun Soon Ahn        */
/*                                                                       */
/*  DESCRIPTION :                                                        */
/*                                                                       */
/*     FFT using Cooly-Turkey algorithm.                                 */
/*     There are two inputs, ar[] and ai[]. ar[] is real number parts    */
/*     of input array and the ai[] is imaginary number parts of input.   */
/*     The function fft1 process FFT or inverse FFT according to the    .*/
/*     parameter flag. (FFT with flag=0, inverse FFT with flag=1).       */
/*                                                                       */
/*                                                                       */
/*  REMARK :                                                             */
/*                                                                       */
/*  EXECUTION TIME :                                                     */
/*                                                                       */
/*                                                                       */
/*************************************************************************/


#define PI 3.14159
#define ARRAY_SIZE 8

double realArray[ARRAY_SIZE];
double imaginaryArray[ARRAY_SIZE] = {0.,  };

static double 
fabs (double n)
{
  if (n >= 0)
  {
    return n;
  }
  else
  {
     return -n;
  }
}

static double 
log(double n)
{
  return 4.5;
}

static double 
sin (double rad)
{
  #ifdef CBMC
  int __count_42_41 = 0;
  int __count_45_44 = 0;
  int __count_49_47 = 0;
  int __count_L42 = 0;
  int __count_L45 = 0;
  int __count_L48 = 0;
  #endif
  double app;

  double diff;
  int inc = 1;
  int counter =0;

  while (
    #ifdef CBMC
    __count_L42++,
    #endif
  rad > 2*PI) // 42
	{
    #ifdef CBMC
    __count_42_41++;
    #endif
    rad -= 2*PI;
  }
  while (
    #ifdef CBMC
    __count_L45++,
    #endif
  rad < -2*PI) // 45
  {
    #ifdef CBMC
    __count_45_44++;
    #endif
    rad += 2*PI;
  }
  app = diff = rad;
   diff = (diff * (-(rad*rad))) /
      ((2.0 * inc) * (2.0 * inc + 1.0));
    app = app + diff;
    inc++;
  while(
    #ifdef CBMC
    __count_L48++,
    #endif
  fabs(diff) >= 0.00001) { // 48, 49
    #ifdef CBMC
    __count_49_47++;
    #endif
    counter++;
    diff = (diff * (-(rad*rad))) /
      ((2.0 * inc) * (2.0 * inc + 1.0));
    app = app + diff;
    inc++;
  }

  return(app);
}

static double 
cos (double rad)
{
  double sin();

  return (sin (PI / 2.0 - rad));
}

int 
fft (int n, int flag)
{
  #ifdef CBMC
  int __count_4_39 = 0;
  int __count_9_8 = 0;
  int __count_10_11 = 0;
  int __count_10_12 = 0;
  int __count_18_19 = 0;
  int __count_25_27 = 0;
  int __count_26_27 = 0;
  int __count_29_30 = 0;
  int __count_34_39 = 0;
  int __count_36_37 = 0;
  int __count_L32 = 0;
  int __count_L30 = 0;
  int __count_L23 = 0;
  int __count_L21 = 0;
  int __count_L19 = 0;
  int __count_L9 = 0;
  int __count_L37 = 0;
  #endif
  int i, j, k, it, xp, xp2, j1, j2, iter;
  double sign, w, wr, wi, dr1, dr2, di1, di2, tr, ti, arg;

  if (n < 2) // 3
  {
    #ifdef CBMC
    __count_4_39++;
    #endif
    return 999; // 4
  }

  iter = log((double)n)/log(2.0);
  j = 1;
  for(i = 0; 
  #ifdef CBMC
  __count_L9++,
  #endif
  i < iter; i++) // 9
  {
    #ifdef CBMC
    __count_9_8++;
    #endif
    j *= 2;
  }

  /*  Main FFT Loops  */
  #ifdef CBMC
  // TODO: check
  if (flag == 1) // 10
  {
    __count_10_12++;
    sign = 1.0; // 12?
  }
  else
  {
    __count_10_11++;
    sign = -1.0; // 11?
  }
  #else
  sign = ((flag == 1) ? 1.0 : -1.0); // 10, 11, 12
  #endif
  xp2 = n;
  for(it = 0;
    #ifdef CBMC
    __count_L23++,
    #endif
  it < iter; it++) // 23
  {
    // 14
    xp = xp2;
    xp2 /= 2;
    w = PI / xp2;
    for(k = 0; 
      #ifdef CBMC
      __count_L21++,
      #endif
    k < xp2; k++) // 21
    {
      arg = k * w;
      wr  = cos(arg);
      wi  = sign * sin(arg);
      i   = k - xp;
      for(j = xp; 
        #ifdef CBMC
        __count_L19++,
        #endif
      j <= n; j += xp) // 19
      {
        #ifdef CBMC
        __count_18_19++;
        #endif
      	j1 = j + i;
      	j2 = j1 + xp2;
      	dr1 = realArray[j1];
      	dr2 = realArray[j2];
      	di1 = imaginaryArray[j1];
      	di2 = imaginaryArray[j2];
      	tr = dr1 - dr2;
      	ti = di1 - di2;
      	realArray[j1] = dr1 + dr2;
      	imaginaryArray[j1] = di1 + di2;
      	realArray[j2] = tr * wr - ti * wi;
      	imaginaryArray[j2] = ti * wr + tr * wi;
       }
     }
  }
// 24
  /*  Digit Reverse Counter  */
  j1 = n / 2;
  j2 = n - 1;
  j = 1;
  for(i = 1;
    #ifdef CBMC
    __count_L32++,
    #endif
  i <= j2; i++) // 32
  {
    if(i < j) // 25
    {
      // 26
      #ifdef CBMC
      __count_26_27++;
      #endif
      tr = realArray[j-1];
      ti = imaginaryArray[j-1];
      realArray[j-1] = realArray[i-1];
      imaginaryArray[j-1] = imaginaryArray[i-1];
      realArray[i-1] = tr;
      imaginaryArray[i-1] = ti;
     }
     #ifdef CBMC
     else __count_25_27++;
     #endif
     
     k = j1;
     while(
      #ifdef CBMC
      __count_L30++,
      #endif
     k < j) // 30
     {
       j -= k;
       k /= 2;
       #ifdef CBMC
       __count_29_30++;
       #endif
     }
     j += k;
  }
   
  if(flag == 0) // 33
  {
    #ifdef CBMC
    __count_34_39++;
    #endif
    return 0; // 34
  }

  w = n;
  for(i = 0; 
    #ifdef CBMC
    __count_L37++,
    #endif
  i < n; i++) // 37
  {
    realArray[i] /= w;
    imaginaryArray[i] /= w;
    #ifdef CBMC
    __count_36_37++;
    #endif
  }

  return 0;
}

int 
main (int argc, char *argv[])
{
  int  i;
  int n = ARRAY_SIZE;
  int flag;
  int chkerr;

  /*
   * At least eight values must be supplied
   */
  if (argc != 9)
  {
    return 1;
  }

  for (i = 0; i < argc - 1; ++i)
  {
    sscanf(argv[i+1], "%lf", realArray+i);
  }

  /* forward fft */
  flag = 0;
  chkerr = fft(ARRAY_SIZE, flag);

  /* inverse fft */
  flag = 1;
  chkerr = fft(ARRAY_SIZE, flag);
  
  printf("%d", chkerr);

  return 0;
}


