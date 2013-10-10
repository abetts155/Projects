/*
 * Statistics program taken from MDH suite and modified by Adam Betts to consume a
 * test vector supplied on the command line.
 *
 * For this program, the test vector is a list of doubles.
 * There must be an even number of doubles on the command line because there are
 * two arrays to fill, each of the same dimension.
 *
 * Unfortunately the 'math' header file is needed for the 'sqrt' function.
 * To compile and link correctly do:
 * 'gcc -lm -o <outfile> st.c'
 */

float sqrt(const double x)  
{
#ifdef CBMC
//==========> sqrt : header 15
int __count_15 = 0;
#endif
  union
  {
    int i;
    double x;
  } u;

  u.x = x;
  u.i = (1<<29) + (u.i >> 1) - (1<<22); 
  #ifdef CBMC
  __count_15++;
  #endif
  return u.x;
} 

double
Square (double x)
{
#ifdef CBMC
//==========> square : header 16
int __count_16 = 0;
#endif

  #ifdef CBMC
  __count_16++;
  #endif
  return x * x;
}

void
Calc_Sum_Mean (double Array[], double * Sum, double * Mean, int ARRAY_SIZE)
{
#ifdef CBMC
//==========> calc_sum_mean : header 19
int __count_19_18 = 0; //Loop counter
//==========> calc_sum_mean : header 17
int __count_20 = 0;
int __count_19_20 = 0;
#endif

  int i;

  *Sum = 0;
  #ifdef CBMC
  __count_19_18 = 0;
  #endif
  for (i = 0; i < ARRAY_SIZE; i++)
  {
    #ifdef CBMC
    __count_19_18++;
    #endif
    *Sum += Array[i];
  }
  #ifdef CBMC
  __count_19_20++;
  #endif

  *Mean = *Sum / ARRAY_SIZE;
  
  #ifdef CBMC
  __count_20++;
  #endif
}

void
Calc_Var_Stddev (double Array[], double Mean, double * Var, double * Stddev,
    int ARRAY_SIZE)
{
#ifdef CBMC
//==========> calc_var_stddev : header 24
int __count_24_22 = 0; //Loop counter
//==========> calc_var_stddev : header 21
int __count_26 = 0;
int __count_24_25 = 0;
#endif

  int i;
  double diffs = 0.0;

  #ifdef CBMC
  __count_24_22 = 0;
  #endif
  for (i = 0; i < ARRAY_SIZE; i++)
  {
    #ifdef CBMC
    __count_24_22++;
    #endif
    diffs += Square (Array[i] - Mean);
  }
  #ifdef CBMC
  __count_24_25++;
  #endif

  *Var = diffs / ARRAY_SIZE;
  *Stddev = sqrt (*Var);
  
  #ifdef CBMC
  __count_26++;
  #endif
}

void
Calc_LinCorrCoef (double ArrayA[], double ArrayB[], double MeanA, double MeanB,
    int ARRAY_SIZE)
{
#ifdef CBMC
//==========> calc_lincorrcoef : header 11
int __count_8_9 = 0;
int __count_11_8 = 0; //Loop counter
//==========> calc_lincorrcoef : header 7
int __count_14 = 0;
int __count_11_12 = 0;
#endif

  int i;
  double numerator = 0.0;
  double Aterm = 0.0;
  double Bterm = 0.0;
  double Coef;

  #ifdef CBMC
  __count_11_8 = 0;
  #endif
  for (i = 0; i < ARRAY_SIZE; i++)
  {
    #ifdef CBMC
    __count_11_8++;
    __count_8_9++;
    #endif
    numerator += (ArrayA[i] - MeanA) * (ArrayB[i] - MeanB);
    Aterm += Square (ArrayA[i] - MeanA);
    Bterm += Square (ArrayB[i] - MeanB);
  }
  #ifdef CBMC
  __count_11_12++;
  #endif
  Coef = numerator / (sqrt (Aterm) * sqrt (Bterm));
  #ifdef CBMC
  __count_14++;
  #endif
}

int
statistics (double ArrayA[], double ArrayB[], int ARRAY_SIZE) 
{
#ifdef CBMC
//==========> statistics : header 1
int __count_6 = 0;
int __count_1_2 = 0;
#endif

  #ifdef CBMC
  __count_1_2++;
  #endif

  double SumA;
  double SumB;
  double VarA;
  double VarB;
  double MeanA;
  double MeanB;
  double StddevA;
  double StddevB; 
 
  Calc_Sum_Mean (ArrayA, &SumA, &MeanA, ARRAY_SIZE);
  Calc_Var_Stddev (ArrayA, MeanA, &VarA, &StddevA, ARRAY_SIZE);
  Calc_Sum_Mean (ArrayB, &SumB, &MeanB, ARRAY_SIZE);
  Calc_Var_Stddev (ArrayB, MeanB, &VarB, &StddevB, ARRAY_SIZE);
  Calc_LinCorrCoef (ArrayA, ArrayB, MeanA, MeanB, ARRAY_SIZE);
  
  #ifdef CBMC
  __count_6++;
  #endif
  return ArrayA[0];
}

int
main (int argc, char *argv[])
{
  const int ELEMENTS = argc - 1;
  int i;
  double ArrayA[ELEMENTS/2];
  double ArrayB[ELEMENTS/2];
  
  if (argc == 1 || ELEMENTS % 2 == 1)
  {
    return 1;
  }

  for (i = 0; i < ELEMENTS/2; i++)
  {
    ArrayA[i] = atoi (argv[i + 1]);
  }
  for (i = 0; i < ELEMENTS/2; i++)
  {
    ArrayB[i] = atoi (argv[i + 1]);
  }

  int val = statistics (ArrayA, ArrayB, ELEMENTS/2);
  
  printf("%d", val);

  return 0;
}
