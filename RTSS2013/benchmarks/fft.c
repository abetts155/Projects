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
  double app;

  double diff;
  int inc = 1;
  int counter =0;

  while (rad > 2*PI)
	rad -= 2*PI;
  while (rad < -2*PI)
    rad += 2*PI;
  app = diff = rad;
   diff = (diff * (-(rad*rad))) /
      ((2.0 * inc) * (2.0 * inc + 1.0));
    app = app + diff;
    inc++;
  while(fabs(diff) >= 0.00001) {
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
  int i, j, k, it, xp, xp2, j1, j2, iter;
  double sign, w, wr, wi, dr1, dr2, di1, di2, tr, ti, arg;

  if (n < 2)
  {
    return 999;
  }

  iter = log((double)n)/log(2.0);
  j = 1;
  for(i = 0; i < iter; i++)
  {
    j *= 2;
  }

  /*  Main FFT Loops  */
  sign = ((flag == 1) ? 1.0 : -1.0);
  xp2 = n;
  for(it = 0; it < iter; it++)
  {
    xp = xp2;
    xp2 /= 2;
    w = PI / xp2;
    for(k = 0; k < xp2; k++)
    {
      arg = k * w;
      wr  = cos(arg);
      wi  = sign * sin(arg);
      i   = k - xp;
      for(j = xp; j <= n; j += xp)
      {
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

  /*  Digit Reverse Counter  */
  j1 = n / 2;
  j2 = n - 1;
  j = 1;
  for(i = 1; i <= j2; i++)
  {
    if(i < j)
    {
      tr = realArray[j-1];
      ti = imaginaryArray[j-1];
      realArray[j-1] = realArray[i-1];
      imaginaryArray[j-1] = imaginaryArray[i-1];
      realArray[i-1] = tr;
      imaginaryArray[i-1] = ti;
     }
     
     k = j1;
     while(k < j)
     {
       j -= k;
       k /= 2;
     }
     j += k;
  }
   
  if(flag == 0)
  { 
    return 0;
  }

  w = n;
  for(i = 0; i < n; i++)
  {
    realArray[i] /= w;
    imaginaryArray[i] /= w;
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

  return 0;
}


