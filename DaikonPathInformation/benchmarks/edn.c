
#define ARRAY_SIZE 200
#define N 100
#define ORDER 50

void
vec_mpy1 (short y[], const short x[], short scaler)
{
  #ifdef CBMC
  int __count_10_9 = 0;
  int __count_L10 = 0;
  #endif
  long int i;

  for (i = 0; 
    #ifdef CBMC
    __count_L10++,
    #endif
  i < 150; i++) // 10
  {
    #ifdef CBMC
    __count_10_9++;
    #endif
    y[i] += ((scaler * x[i]) >> 15);
  }
}

/*****************************************************
*			Dot Product	      *
*****************************************************/
long int
mac (const short *a, const short *b, long int sqr, long int *sum)
{
  #ifdef CBMC
  int __count_31_30 = 0;
  int __count_L31 = 0;
  #endif
  long int i;
  long int dotp = *sum;

  for (i = 0; 
    #ifdef CBMC
    __count_L31++,
    #endif
  i < 150; i++) // 31
  {
    #ifdef CBMC
    __count_31_30++;
    #endif
    dotp += b[i] * a[i];
    sqr += b[i] * b[i];
  }

  *sum = dotp;
  return sqr;
}


/*****************************************************
*		FIR Filter		     *
*****************************************************/
void
fir (const short array1[], const short coeff[], long int output[])
{
  #ifdef CBMC
  int __count_4_3 = 0;
  int __count_L6 = 0;
  int __count_L4 = 0;
  #endif
  long int i, j, sum;

  for (i = 0; 
    #ifdef CBMC
    __count_L6++,
    #endif
  i < N - ORDER; i++) // 6
  {
    sum = 0;
    for (j = 0; 
      #ifdef CBMC
      __count_L4++,
      #endif
    j < ORDER; j++) // 4
    {
      #ifdef CBMC
      __count_4_3++;
      #endif
      sum += array1[i + j] * coeff[j];
    }
    output[i] = sum >> 15;
  }
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
  int __count_36_35 = 0;
  int __count_L38 = 0;
  int __count_L36 = 0;
  #endif
  long int i, j;
  long int sum0, sum1;
  short x0, x1, h0, h1;

  for (j = 0; 
    #ifdef CBMC
    __count_L38++,
    #endif
  j < 100; j += 2) // 38
  {
    sum0 = 0;
    sum1 = 0;
    x0 = x[j];
    for (i = 0; 
      #ifdef CBMC
      __count_L36++,
      #endif
    i < 32; i += 2) // 36
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
    y[j] = sum0 >> 15;
    y[j + 1] = sum1 >> 15;
  }
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
  int __count_14_13 = 0;
  int __count_L14 = 0;
  #endif
  long int i;

  f -= b[n - 1] * k[n - 1];
  for (i = n - 2; 
    #ifdef CBMC
    __count_L14++,
    #endif
  i >= 0; i--)  // 14
  {
    #ifdef CBMC
    __count_14_13++;
    #endif
    f -= b[i] * k[i];
    b[i + 1] = b[i] + ((k[i] * (f >> 16)) >> 16);
  }
  
  b[0] = f >> 16;
  return f;
}

/*****************************************************
*			IIR Filter		     *
*****************************************************/
void
iir1 (const short *coefs, const short *input, long int *optr, long int *state)
{
  #ifdef CBMC
  int __count_52_51 = 0;
  int __count_L52 = 0;
  #endif
  long int x;
  long int t;
  long int n;

  x = input[0];
  for (n = 0; 
    #ifdef CBMC
    __count_L52++,
    #endif
  n < 50; n++) // 52
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
}

/*****************************************************
*	Vocoder Codebook Search 	     *
*****************************************************/
long int
codebook (long int mask, long int bitchanged, long int numbasis, long int codeword, long int g, const short *d, short ddim, short theta)
{
  #ifdef CBMC
  int __count_26_27 = 0;
  int __count_L27 = 0;
  #endif

  long int j;
  long int tmpMask;

  tmpMask = mask << 1;
  for (j = bitchanged + 1; 
    #ifdef CBMC
    __count_L27++,
    #endif
  j <= numbasis; j++) // 27
  {
    #ifdef CBMC
    __count_26_27++;
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
  return g;
}


/*****************************************************
*		JPEG Discrete Cosine Transform 
*****************************************************/
void
jpegdct(short *d, short *r)
{
  #ifdef CBMC
  int __count_44_43 = 0;
  int __count_L48 = 0;
  int __count_L46 = 0;
  int __count_L44 = 0;
  #endif
  long int t[12];
  short i, j, k, m, n, p;
  
  for (k = 1, m = 0, n = 13, p = 8;
        #ifdef CBMC
         __count_L48++,
         #endif 
       k <= 8; 
       k += 7, m += 3, n += 3, p -= 7, d -= 64) // 48
  {
    for (i = 0; 
      #ifdef CBMC
      __count_L46++,
      #endif
         i < 8; 
         i++, d += p) // 46
    {
      for (j = 0; 
        #ifdef CBMC
        __count_L44++,
        #endif
      j < 4; j++) // 44
      {
        #ifdef CBMC
        __count_44_43++;
        #endif
        t[j] = d[k * j] + d[k * (7 - j)];
	t[7 - j] = d[k * j] - d[k * (7 - j)];
      }
    
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
  }
}

int
edn (short a[], short b[])
{
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
