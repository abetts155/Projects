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
#ifdef CBMC
//==========> fabs : header 51
int __count_54 = 0;
int __count_51_52 = 0;
int __count_51_53 = 0;
#endif
  if (n >= 0)
  {
    #ifdef CBMC
    __count_51_52++;
    #endif
    // 52
    #ifdef CBMC
    __count_54++;
    #endif

#ifdef CBMC
assert(__count_51_52 <= 1); // Upper capacity constraint
assert(__count_51_53 <= 1); // Upper capacity constraint
assert(__count_54 >= 1); // Lower capacity constraint
assert(__count_54 <= 1); // Upper capacity constraint
assert(__count_51_52 > 0 ==> __count_54 > 0); // Execution dependence
assert(__count_51_53 > 0 ==> __count_54 > 0); // Execution dependence
#endif

    return n;
  }
  else
  {
    #ifdef CBMC
    __count_51_53++;
    #endif
    // 53
     #ifdef CBMC
     __count_54++;
     #endif

#ifdef CBMC
assert(__count_51_52 <= 1); // Upper capacity constraint
assert(__count_51_53 <= 1); // Upper capacity constraint
assert(__count_54 >= 1); // Lower capacity constraint
assert(__count_54 <= 1); // Upper capacity constraint
assert(__count_51_52 > 0 ==> __count_54 > 0); // Execution dependence
assert(__count_51_53 > 0 ==> __count_54 > 0); // Execution dependence
#endif

     return -n;
  }
}

static double 
log(double n)
{
#ifdef CBMC
//==========> log : header 55
int __count_55 = 0;
#endif
  #ifdef CBMC
  __count_55++;
  #endif

#ifdef CBMC
assert(__count_55 >= 1); // Lower capacity constraint
assert(__count_55 <= 1); // Upper capacity constraint
#endif

  return 4.5;
}

static double 
sin (double rad)
{
#ifdef CBMC
//==========> sin : header 48
int __count_48_49 = 0;
int __count_48_49_L = 0; //Loop counter
//==========> sin : header 45
int __count_45_44 = 0;
int __count_45_44_L = 0; //Loop counter
//==========> sin : header 42
int __count_42_41 = 0;
int __count_42_41_L = 0; //Loop counter
//==========> sin : header 40
int __count_50 = 0;
int __count_42_43 = 0;
#endif
  double app;

  double diff;
  int inc = 1;
  int counter =0;

  #ifdef CBMC
  __count_42_41_L = 0;
  #endif
  while (rad > 2*PI) // 42
	{
    #ifdef CBMC
    __count_42_41++;
    __count_42_41_L++;
    #endif
    rad -= 2*PI;
  }
  #ifdef CBMC
  assert(__count_42_41_L  <= 1); // Loop counter property
  __count_42_43++;
  #endif

  while (rad < -2*PI) // 45
  {
    #ifdef CBMC
    __count_45_44++;
    __count_45_44_L++;
    #endif
    rad += 2*PI;
  }
  app = diff = rad;
   diff = (diff * (-(rad*rad))) /
      ((2.0 * inc) * (2.0 * inc + 1.0));
    app = app + diff;
    inc++;

  #ifdef CBMC
  __count_48_49_L = 0;
  #endif
  while(
  (
#ifdef CBMC
  (__count_48_49_L++,
  __count_48_49++),
#endif
  fabs(diff)
  ) >= 0.00001) { // 48, 49
    counter++;
    diff = (diff * (-(rad*rad))) /
      ((2.0 * inc) * (2.0 * inc + 1.0));
    app = app + diff;
    inc++;
  }
  #ifdef CBMC
  assert(__count_48_49_L  <= 8); // Loop counter property
  __count_50++;
  #endif

#ifdef CBMC
assert(__count_42_41 == 0); // Dead code
assert(__count_50 >= 1); // Lower capacity constraint
assert(__count_50 <= 1); // Upper capacity constraint
assert(__count_48_49 >= 1); // Lower capacity constraint
assert(__count_48_49 <= 7); // Upper capacity constraint
assert(__count_42_43 >= 1); // Lower capacity constraint
assert(__count_42_43 <= 1); // Upper capacity constraint
assert(__count_45_44 == 0); // Dead code
#endif

  return(app);
}

static double 
cos (double rad)
{
#ifdef CBMC
//==========> cos : header 1
int __count_2 = 0;
int __count_1_2 = 0;
#endif
  double sin();

  #ifdef CBMC
  __count_1_2++;
  __count_2++;
  #endif

#ifdef CBMC
assert(__count_2 >= 1); // Lower capacity constraint
assert(__count_2 <= 1); // Upper capacity constraint
assert(__count_1_2 >= 1); // Lower capacity constraint
assert(__count_1_2 <= 1); // Upper capacity constraint
#endif

  return (sin (PI / 2.0 - rad));
}

int 
fft (int n, int flag)
{
#ifdef CBMC
//==========> fft : header 19
int __count_19_18 = 0;
int __count_19_18_L = 0; //Loop counter
//==========> fft : header 30
int __count_30_29 = 0;
int __count_30_29_L = 0; //Loop counter
//==========> fft : header 21
int __count_19_20 = 0;
int __count_21_15 = 0; //Loop counter
//==========> fft : header 32
int __count_25_26 = 0;
int __count_25_27 = 0;
int __count_32_25 = 0; //Loop counter
//==========> fft : header 23
int __count_21_22 = 0;
int __count_23_14 = 0; //Loop counter
//==========> fft : header 9
int __count_9_8 = 0;
int __count_9_8_L = 0; //Loop counter
//==========> fft : header 37
int __count_37_36 = 0;
int __count_37_36_L = 0; //Loop counter
//==========> fft : header 3
int __count_39 = 0;
int __count_4_39 = 0;
int __count_11_13 = 0;
int __count_12_13 = 0;
int __count_33_34 = 0;
int __count_37_38 = 0;
#endif
  int i, j, k, it, xp, xp2, j1, j2, iter;
  double sign, w, wr, wi, dr1, dr2, di1, di2, tr, ti, arg;

  if (n < 2) // 3
  {
    #ifdef CBMC
    __count_4_39++;
    __count_39++;
    #endif

#ifdef CBMC
assert(__count_25_26 >= 2); // Lower capacity constraint
assert(__count_25_26 <= 2); // Upper capacity constraint
assert(__count_25_27 >= 5); // Lower capacity constraint
assert(__count_25_27 <= 5); // Upper capacity constraint
assert(__count_30_29 >= 4); // Lower capacity constraint
assert(__count_30_29 <= 4); // Upper capacity constraint
assert(__count_37_38 <= 1); // Upper capacity constraint
assert(__count_39 >= 1); // Lower capacity constraint
assert(__count_39 <= 1); // Upper capacity constraint
assert(__count_37_36 <= 8); // Upper capacity constraint
assert(__count_33_34 <= 1); // Upper capacity constraint
assert(__count_9_8 >= 1); // Lower capacity constraint
assert(__count_9_8 <= 1); // Upper capacity constraint
assert(__count_11_13 <= 1); // Upper capacity constraint
assert(__count_12_13 <= 1); // Upper capacity constraint
assert(__count_19_18 >= 4); // Lower capacity constraint
assert(__count_19_18 <= 4); // Upper capacity constraint
assert(__count_19_20 >= 4); // Lower capacity constraint
assert(__count_19_20 <= 4); // Upper capacity constraint
assert(__count_21_22 >= 1); // Lower capacity constraint
assert(__count_21_22 <= 1); // Upper capacity constraint
assert(__count_4_39 == 0); // Dead code
assert(__count_33_34 > 0 ==> __count_12_13 > 0); // Mutual inclusion
assert(__count_12_13 > 0 ==> __count_33_34 > 0); // Mutual inclusion
assert(__count_37_36 > 0 ==> __count_11_13 > 0); // Mutual inclusion
assert(__count_11_13 > 0 ==> __count_37_36 > 0); // Mutual inclusion
assert(__count_37_38 > 0 ==> __count_11_13 > 0); // Mutual inclusion
assert(__count_11_13 > 0 ==> __count_37_38 > 0); // Mutual inclusion
assert(__count_37_38 > 0 ==> __count_37_36 > 0); // Mutual inclusion
assert(__count_37_36 > 0 ==> __count_37_38 > 0); // Mutual inclusion
assert(__count_33_34 > 0 ==> __count_11_13 == 0); // Mutual exclusion
assert(__count_11_13 > 0 ==> __count_33_34 == 0); // Mutual exclusion
assert(__count_37_36 > 0 ==> __count_12_13 == 0); // Mutual exclusion
assert(__count_12_13 > 0 ==> __count_37_36 == 0); // Mutual exclusion
assert(__count_37_38 > 0 ==> __count_12_13 == 0); // Mutual exclusion
assert(__count_12_13 > 0 ==> __count_37_38 == 0); // Mutual exclusion
assert(__count_11_13 > 0 ==> __count_39 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_9_8 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_19_18 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_19_20 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_21_22 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_25_26 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_25_27 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_30_29 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_39 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_9_8 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_19_18 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_19_20 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_21_22 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_25_26 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_25_27 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_30_29 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_39 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_9_8 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_19_18 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_19_20 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_21_22 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_25_26 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_25_27 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_30_29 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_39 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_9_8 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_19_18 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_19_20 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_21_22 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_25_26 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_25_27 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_30_29 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_39 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_9_8 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_19_18 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_19_20 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_21_22 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_25_26 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_25_27 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_30_29 > 0); // Execution dependence
#endif

    return 999; // 4
  }

  iter = log((double)n)/log(2.0);
  j = 1;
  #ifdef CBMC
  __count_9_8_L = 0;
  #endif
  for(i = 0; i < iter; i++) // 9
  {
    #ifdef CBMC
    __count_9_8_L++;
    __count_9_8++;
    #endif
    j *= 2;
  }
  #ifdef CBMC
  assert(__count_9_8_L  <= 2); // Loop counter property
  #endif

  /*  Main FFT Loops  */
  #ifdef CBMC
  // TODO: check
  if (flag == 1) // 10
  {
    sign = 1.0; // 12?
    __count_12_13++;
  }
  else
  {
    sign = -1.0; // 11?
    __count_11_13++;
  }
  #else
  sign = ((flag == 1) ? 1.0 : -1.0); // 10, 11, 12
  #endif
  xp2 = n;

  #ifdef CBMC
  __count_23_14 = 0;
  #endif
  for(it = 0; it < iter; it++) // 23
  {
    #ifdef CBMC
    __count_23_14++;
    #endif

    // 14
    xp = xp2;
    xp2 /= 2;
    w = PI / xp2;
    #ifdef CBMC
    __count_21_15 = 0;
    #endif
    for(k = 0; k < xp2; k++) // 21
    {
      #ifdef CBMC
      __count_21_15++;
      #endif
      arg = k * w;
      wr  = cos(arg);
      wi  = sign * sin(arg);
      i   = k - xp;
      #ifdef CBMC
      __count_19_18_L = 0;
      #endif
      for(j = xp; j <= n; j += xp) // 19
      {
        #ifdef CBMC
        __count_19_18_L++;
        __count_19_18++;
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
       #ifdef CBMC
       assert(__count_19_18_L  <= 2); // Loop counter property
       __count_19_20++;
       #endif
     }
     #ifdef CBMC
     assert(__count_21_15  <= 5); // Loop counter property
     __count_21_22++;
     #endif
  }
  #ifdef CBMC
  assert(__count_23_14  <= 2); // Loop counter property
  #endif
// 24
  /*  Digit Reverse Counter  */
  j1 = n / 2;
  j2 = n - 1;
  j = 1;

  #ifdef CBMC
  __count_32_25 = 0;
  #endif
  for(i = 1;i <= j2; i++) // 32
  {
    #ifdef CBMC
    __count_32_25++;
    #endif

    if(i < j) // 25
    {
      #ifdef CBMC
      __count_25_26++;
      #endif
      // 26
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
     #ifdef CBMC
     __count_30_29_L = 0;
     #endif
     while(k < j) // 30
     {
       #ifdef CBMC
       __count_30_29_L++;
       __count_30_29++;
       #endif
       j -= k;
       k /= 2;
     }
     #ifdef CBMC
     assert(__count_30_29_L  <= 3); // Loop counter property
     #endif
     j += k;
  }
  #ifdef CBMC
  assert(__count_32_25  <= 8); // Loop counter property
  #endif
   
  if(flag == 0) // 33
  {
    #ifdef CBMC
    __count_33_34++;
    __count_39++;
    #endif

#ifdef CBMC
assert(__count_25_26 >= 2); // Lower capacity constraint
assert(__count_25_26 <= 2); // Upper capacity constraint
assert(__count_25_27 >= 5); // Lower capacity constraint
assert(__count_25_27 <= 5); // Upper capacity constraint
assert(__count_30_29 >= 4); // Lower capacity constraint
assert(__count_30_29 <= 4); // Upper capacity constraint
assert(__count_37_38 <= 1); // Upper capacity constraint
assert(__count_39 >= 1); // Lower capacity constraint
assert(__count_39 <= 1); // Upper capacity constraint
assert(__count_37_36 <= 8); // Upper capacity constraint
assert(__count_33_34 <= 1); // Upper capacity constraint
assert(__count_9_8 >= 1); // Lower capacity constraint
assert(__count_9_8 <= 1); // Upper capacity constraint
assert(__count_11_13 <= 1); // Upper capacity constraint
assert(__count_12_13 <= 1); // Upper capacity constraint
assert(__count_19_18 >= 4); // Lower capacity constraint
assert(__count_19_18 <= 4); // Upper capacity constraint
assert(__count_19_20 >= 4); // Lower capacity constraint
assert(__count_19_20 <= 4); // Upper capacity constraint
assert(__count_21_22 >= 1); // Lower capacity constraint
assert(__count_21_22 <= 1); // Upper capacity constraint
assert(__count_4_39 == 0); // Dead code
assert(__count_33_34 > 0 ==> __count_12_13 > 0); // Mutual inclusion
assert(__count_12_13 > 0 ==> __count_33_34 > 0); // Mutual inclusion
assert(__count_37_36 > 0 ==> __count_11_13 > 0); // Mutual inclusion
assert(__count_11_13 > 0 ==> __count_37_36 > 0); // Mutual inclusion
assert(__count_37_38 > 0 ==> __count_11_13 > 0); // Mutual inclusion
assert(__count_11_13 > 0 ==> __count_37_38 > 0); // Mutual inclusion
assert(__count_37_38 > 0 ==> __count_37_36 > 0); // Mutual inclusion
assert(__count_37_36 > 0 ==> __count_37_38 > 0); // Mutual inclusion
assert(__count_33_34 > 0 ==> __count_11_13 == 0); // Mutual exclusion
assert(__count_11_13 > 0 ==> __count_33_34 == 0); // Mutual exclusion
assert(__count_37_36 > 0 ==> __count_12_13 == 0); // Mutual exclusion
assert(__count_12_13 > 0 ==> __count_37_36 == 0); // Mutual exclusion
assert(__count_37_38 > 0 ==> __count_12_13 == 0); // Mutual exclusion
assert(__count_12_13 > 0 ==> __count_37_38 == 0); // Mutual exclusion
assert(__count_11_13 > 0 ==> __count_39 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_9_8 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_19_18 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_19_20 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_21_22 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_25_26 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_25_27 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_30_29 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_39 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_9_8 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_19_18 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_19_20 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_21_22 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_25_26 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_25_27 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_30_29 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_39 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_9_8 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_19_18 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_19_20 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_21_22 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_25_26 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_25_27 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_30_29 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_39 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_9_8 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_19_18 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_19_20 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_21_22 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_25_26 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_25_27 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_30_29 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_39 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_9_8 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_19_18 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_19_20 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_21_22 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_25_26 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_25_27 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_30_29 > 0); // Execution dependence
#endif

    return 0; // 34
  }

  w = n;
  #ifdef CBMC
  __count_37_36_L = 0;
  #endif
  for(i = 0; i < n; i++) // 37
  {
    #ifdef CBMC
    __count_37_36_L++;
    __count_37_36++;
    #endif
    realArray[i] /= w;
    imaginaryArray[i] /= w;
  }
  #ifdef CBMC
  assert(__count_37_36_L  <= 9); // Loop counter property
  __count_37_38++;
  #endif

  #ifdef CBMC
  __count_39++;
  #endif

#ifdef CBMC
assert(__count_25_26 >= 2); // Lower capacity constraint
assert(__count_25_26 <= 2); // Upper capacity constraint
assert(__count_25_27 >= 5); // Lower capacity constraint
assert(__count_25_27 <= 5); // Upper capacity constraint
assert(__count_30_29 >= 4); // Lower capacity constraint
assert(__count_30_29 <= 4); // Upper capacity constraint
assert(__count_37_38 <= 1); // Upper capacity constraint
assert(__count_39 >= 1); // Lower capacity constraint
assert(__count_39 <= 1); // Upper capacity constraint
assert(__count_37_36 <= 8); // Upper capacity constraint
assert(__count_33_34 <= 1); // Upper capacity constraint
assert(__count_9_8 >= 1); // Lower capacity constraint
assert(__count_9_8 <= 1); // Upper capacity constraint
assert(__count_11_13 <= 1); // Upper capacity constraint
assert(__count_12_13 <= 1); // Upper capacity constraint
assert(__count_19_18 >= 4); // Lower capacity constraint
assert(__count_19_18 <= 4); // Upper capacity constraint
assert(__count_19_20 >= 4); // Lower capacity constraint
assert(__count_19_20 <= 4); // Upper capacity constraint
assert(__count_21_22 >= 1); // Lower capacity constraint
assert(__count_21_22 <= 1); // Upper capacity constraint
assert(__count_4_39 == 0); // Dead code
assert(__count_33_34 > 0 ==> __count_12_13 > 0); // Mutual inclusion
assert(__count_12_13 > 0 ==> __count_33_34 > 0); // Mutual inclusion
assert(__count_37_36 > 0 ==> __count_11_13 > 0); // Mutual inclusion
assert(__count_11_13 > 0 ==> __count_37_36 > 0); // Mutual inclusion
assert(__count_37_38 > 0 ==> __count_11_13 > 0); // Mutual inclusion
assert(__count_11_13 > 0 ==> __count_37_38 > 0); // Mutual inclusion
assert(__count_37_38 > 0 ==> __count_37_36 > 0); // Mutual inclusion
assert(__count_37_36 > 0 ==> __count_37_38 > 0); // Mutual inclusion
assert(__count_33_34 > 0 ==> __count_11_13 == 0); // Mutual exclusion
assert(__count_11_13 > 0 ==> __count_33_34 == 0); // Mutual exclusion
assert(__count_37_36 > 0 ==> __count_12_13 == 0); // Mutual exclusion
assert(__count_12_13 > 0 ==> __count_37_36 == 0); // Mutual exclusion
assert(__count_37_38 > 0 ==> __count_12_13 == 0); // Mutual exclusion
assert(__count_12_13 > 0 ==> __count_37_38 == 0); // Mutual exclusion
assert(__count_11_13 > 0 ==> __count_39 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_9_8 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_19_18 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_19_20 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_21_22 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_25_26 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_25_27 > 0); // Execution dependence
assert(__count_11_13 > 0 ==> __count_30_29 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_39 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_9_8 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_19_18 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_19_20 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_21_22 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_25_26 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_25_27 > 0); // Execution dependence
assert(__count_12_13 > 0 ==> __count_30_29 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_39 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_9_8 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_19_18 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_19_20 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_21_22 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_25_26 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_25_27 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_30_29 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_39 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_9_8 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_19_18 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_19_20 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_21_22 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_25_26 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_25_27 > 0); // Execution dependence
assert(__count_37_36 > 0 ==> __count_30_29 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_39 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_9_8 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_19_18 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_19_20 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_21_22 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_25_26 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_25_27 > 0); // Execution dependence
assert(__count_37_38 > 0 ==> __count_30_29 > 0); // Execution dependence
#endif

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


