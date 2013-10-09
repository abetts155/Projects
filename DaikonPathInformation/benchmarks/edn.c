
#define ARRAY_SIZE 200
#define N 100
#define ORDER 50

void
vec_mpy1 (short y[], const short x[], short scaler)
{
#ifdef CBMC
//==========> vec_mpy1 : header 10
int __count_10_9 = 0; //Loop counter
//==========> vec_mpy1 : header 8
int __count_11 = 0;
int __count_10_11 = 0;
#endif

  long int i;

  #ifdef CBMC
  __count_10_9 = 0;
  #endif
  for (i = 0; i < 150; i++) // 10
  {
    #ifdef CBMC
    __count_10_9++;
    #endif
    y[i] += ((scaler * x[i]) >> 15);
  }
  #ifdef CBMC
  __count_10_11++;
  __count_11++;
  #endif
}

/*****************************************************
*			Dot Product	      *
*****************************************************/
long int
mac (const short *a, const short *b, long int sqr, long int *sum)
{
#ifdef CBMC
//==========> mac : header 31
int __count_31_30 = 0; //Loop counter
//==========> mac : header 29
int __count_32 = 0;
int __count_31_32 = 0;
#endif

  long int i;
  long int dotp = *sum;

  #ifdef CBMC
  __count_31_30 = 0;
  #endif
  for (i = 0; i < 150; i++) // 31
  {
    #ifdef CBMC
    __count_31_30++;
    #endif
    dotp += b[i] * a[i];
    sqr += b[i] * b[i];
  }

  *sum = dotp;
  #ifdef CBMC
  __count_31_32++;
  __count_32++;
  #endif
  return sqr;
}


/*****************************************************
*		FIR Filter		     *
*****************************************************/
void
fir (const short array1[], const short coeff[], long int output[])
{
  #ifdef CBMC
//==========> fir : header 4
int __count_4_3 = 0; //Loop counter
//==========> fir : header 6
int __count_4_5 = 0;
int __count_6_2 = 0; //Loop counter
//==========> fir : header 1
int __count_7 = 0;
int __count_6_7 = 0;
  #endif
  long int i, j, sum;

  #ifdef CBMC
  __count_6_2 = 0;
  #endif
  for (i = 0; i < N - ORDER; i++) // 6
  {
    #ifdef CBMC
    __count_6_2++;
    #endif
    sum = 0;
    #ifdef CBMC
    __count_4_3 = 0;
    #endif
    for (j = 0; j < ORDER; j++) // 4
    {
      #ifdef CBMC
      __count_4_3++;
      #endif
      sum += array1[i + j] * coeff[j];
    }
    #ifdef CBMC
    __count_4_5++;
    #endif
    output[i] = sum >> 15;
  }
  #ifdef CBMC
  __count_6_7++;
  __count_7++;
  #endif
}

/****************************************************
*	FIR Filter with Redundant Load Elimination
						
By doing two outer loops simultaneously, you can potentially  reuse data (depending on the DSP architecture).
x and h  only  need to be loaded once, therefore reducing redundant loads.
This reduces memory bandwidth and power.
*****************************************************/
void
fir_no_red_ld (const short x[], const short h[], long int y[])
{
#ifdef CBMC
//==========> fir_no_red_ld : header 36
int __count_36_35 = 0; //Loop counter
//==========> fir_no_red_ld : header 38
int __count_36_37 = 0;
int __count_38_34 = 0; //Loop counter
//==========> fir_no_red_ld : header 33
int __count_39 = 0;
int __count_38_39 = 0;
#endif

  long int i, j;
  long int sum0, sum1;
  short x0, x1, h0, h1;

  #ifdef CBMC
  __count_38_34 = 0;
  #endif
  for (j = 0; j < 100; j += 2) // 38
  {
    #ifdef CBMC
    __count_38_34++;
    #endif
    sum0 = 0;
    sum1 = 0;
    x0 = x[j];
    #ifdef CBMC
    __count_36_35 = 0;
    #endif
    for (i = 0; i < 32; i += 2) // 36
    {
      #ifdef CBMC
      __count_36_35++;
      #endif
      x1 = x[j + i + 1];
      h0 = h[i];
      sum0 += x0 * h0;
      sum1 += x1 * h0;
      x0 = x[j + i + 2];
      h1 = h[i + 1];
      sum0 += x1 * h1;
      sum1 += x0 * h1;
    }
    #ifdef CBMC
    __count_36_37++;
    #endif
    y[j] = sum0 >> 15;
    y[j + 1] = sum1 >> 15;
  }
  #ifdef CBMC
  __count_38_39++;
  __count_39++;
  #endif
}

/*******************************************************
* Lattice Synthesis	         
* This function doesn't follow the typical DSP multiply two  vector operation, 
* but it will point out the compiler's flexibility   
********************************************************/
long int
latsynth (short b[], const short k[], long int n, long int f)
{
#ifdef CBMC
//==========> latsynth : header 14
int __count_14_13 = 0; //Loop counter
//==========> latsynth : header 12
int __count_15 = 0;
int __count_14_15 = 0;
#endif

  long int i;

  f -= b[n - 1] * k[n - 1];
  #ifdef CBMC
  __count_14_13 = 0;
  #endif
  for (i = n - 2; i >= 0; i--)  // 14
  {
    #ifdef CBMC
    __count_14_13++;
    #endif
    f -= b[i] * k[i];
    b[i + 1] = b[i] + ((k[i] * (f >> 16)) >> 16);
  }
  
  b[0] = f >> 16;
  #ifdef CBMC
  __count_14_15++;
  __count_15++;
  #endif
  return f;
}

/*****************************************************
*			IIR Filter		     *
*****************************************************/
void
iir1 (const short *coefs, const short *input, long int *optr, long int *state)
{
#ifdef CBMC
//==========> iir1 : header 52
int __count_52_51 = 0; //Loop counter
//==========> iir1 : header 50
int __count_53 = 0;
int __count_52_53 = 0;
#endif
  long int x;
  long int t;
  long int n;

  x = input[0];
  #ifdef CBMC
  __count_52_51 = 0;
  #endif
  for (n = 0; n < 50; n++) // 52
  {
    #ifdef CBMC
    __count_52_51++;
    #endif
    t = x + ((coefs[2] * state[0] + coefs[3] * state[1]) >> 15);
    x = t + ((coefs[0] * state[0] + coefs[1] * state[1]) >> 15);
    state[1] = state[0];
    state[0] = t;
    coefs += 4;	/* point to next filter coefs  */
    state += 2;	/* point to next filter states */
  }

  *optr++ = x;
  #ifdef CBMC
  __count_52_53++;
  __count_53++;
  #endif
}

/*****************************************************
*	Vocoder Codebook Search 	     *
*****************************************************/
long int
codebook (long int mask, long int bitchanged, long int numbasis, long int codeword, long int g, const short *d, short ddim, short theta)
{
#ifdef CBMC
//==========> codebook : header 27
int __count_27_26 = 0; //Loop counter
//==========> codebook : header 25
int __count_28 = 0;
int __count_27_28 = 0;
#endif

  long int j;
  long int tmpMask;

  tmpMask = mask << 1;
  #ifdef CBMC
  __count_27_26 = 0;
  #endif
  for (j = bitchanged + 1; j <= numbasis; j++) // 27
  {
    #ifdef CBMC
    __count_27_26++;
    #endif
/*		
 * The following code is removed since it gave a memory access exception.
 * It is OK since the return value does not control the flow.
 * The loop always iterates a fixed number of times independent of the loop body.
 
    if (theta == !(!(codeword & tmpMask)))
			g += *(d + bitchanged * ddim + j);
		else
			g -= *(d + bitchanged * ddim + j);
		tmpMask <<= 1;
*/
  }
  #ifdef CBMC
  __count_27_28++;
  __count_28++;
  #endif
  return g;
}


/*****************************************************
*		JPEG Discrete Cosine Transform 
*****************************************************/
void
jpegdct(short *d, short *r)
{
#ifdef CBMC
//==========> jpegdct : header 44
int __count_44_43 = 0; //Loop counter
//==========> jpegdct : header 46
int __count_44_45 = 0;
int __count_46_42 = 0; //Loop counter
//==========> jpegdct : header 48
int __count_46_47 = 0;
int __count_48_41 = 0; //Loop counter
//==========> jpegdct : header 40
int __count_49 = 0;
int __count_48_49 = 0;
#endif
  long int t[12];
  short i, j, k, m, n, p;
  
  #ifdef CBMC
  __count_48_41 = 0;
  #endif
  for (k = 1, m = 0, n = 13, p = 8;
       k <= 8; 
       k += 7, m += 3, n += 3, p -= 7, d -= 64) // 48
  {
    #ifdef CBMC
    __count_48_41++;
    #endif

    #ifdef CBMC
    __count_46_42 = 0;
    #endif
    for (i = 0; i < 8; i++, d += p) // 46
    {
      #ifdef CBMC
      __count_46_42++;
      #endif

      #ifdef CBMC
      __count_44_43 = 0;
      #endif
      for (j = 0; j < 4; j++) // 44
      {
        #ifdef CBMC
        __count_44_43++;
        #endif
        t[j] = d[k * j] + d[k * (7 - j)];
	t[7 - j] = d[k * j] - d[k * (7 - j)];
      }
      #ifdef CBMC
      __count_44_45++;
      #endif
    
      t[8] = t[0] + t[3];
      t[9] = t[0] - t[3];
      t[10] = t[1] + t[2];
      t[11] = t[1] - t[2];
      d[0] = (t[8] + t[10]) >> m;
      d[4 * k] = (t[8] - t[10]) >> m;
      t[8] = (short) (t[11] + t[9]) * r[10];
      d[2 * k] = t[8] + (short) ((t[9] * r[9]) >> n);
      d[6 * k] = t[8] + (short) ((t[11] * r[11]) >> n);
      t[0] = (short) (t[4] + t[7]) * r[2];
      t[1] = (short) (t[5] + t[6]) * r[0];
      t[2] = t[4] + t[6];
      t[3] = t[5] + t[7];
      t[8] = (short) (t[2] + t[3]) * r[8];
      t[2] = (short) t[2] * r[1] + t[8];
      t[3] = (short) t[3] * r[3] + t[8];
      d[7 * k] = (short) (t[4] * r[4] + t[0] + t[2]) >> n;
      d[5 * k] = (short) (t[5] * r[6] + t[1] + t[3]) >> n;
      d[3 * k] = (short) (t[6] * r[5] + t[1] + t[2]) >> n;
      d[1 * k] = (short) (t[7] * r[7] + t[0] + t[3]) >> n;
    }
    #ifdef CBMC
    __count_46_47++;
    #endif
  }
  #ifdef CBMC
  __count_48_49++;
  __count_49++;
  #endif
}

int
edn (short a[], short b[])
{
#ifdef CBMC
//==========> edn : header 16
int __count_24 = 0;
int __count_16_17 = 0;
#endif

  short c = 0x3;
  long int output[200];
  long int d = 0xAAAA;
  int e[1] = {0xEEEE};

  vec_mpy1(a, b, c);
  c = mac(a, b, (long int) c, (long int *) output);
  fir(a, b, output);
  fir_no_red_ld(a, b, output);
  d = latsynth(a, b, N, d);
  iir1(a, b, &output[100], output);
  e[0] = codebook(d, 1, 17, e[0], d, a, c, 1);
  jpegdct(a, b);
  
  #ifdef CBMC
  __count_16_17++;
  __count_24++;
  #endif
  return e[0];
}

int
main (int argc, char *argv[])
{ 
  short a[ARRAY_SIZE];
  short b[ARRAY_SIZE];
  int i;
  int k;
 
 /*
  * Need two arrays of size ARRAY_SIZE
  */
  if (argc != 2*ARRAY_SIZE+1)
  {
    return 1;
  }

  k = 0;
  for (i = 0; i < ARRAY_SIZE; ++i)
  {
    a[i] = (short) atoi (argv[k + 1]);
    k++;
  }

  for (i = 0; i < ARRAY_SIZE; ++i)
  {
    b[i] = (short) atoi (argv[k + 1]);
    k++;
  }

  int val = edn (a, b);
  printf("%d", val);

  return 0;
}
