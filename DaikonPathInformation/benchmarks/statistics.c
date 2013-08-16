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
  union
  {
    int i;
    double x;
  } u;

  u.x = x;
  u.i = (1<<29) + (u.i >> 1) - (1<<22); 
  return u.x;
} 

double
Square (double x)
{
  return x * x;
}

void
Calc_Sum_Mean (double Array[], double * Sum, double * Mean, int ARRAY_SIZE)
{
  int i;

  *Sum = 0;
  for (i = 0; i < ARRAY_SIZE; i++)
  {
    *Sum += Array[i];
  }
  *Mean = *Sum / ARRAY_SIZE;
}

void
Calc_Var_Stddev (double Array[], double Mean, double * Var, double * Stddev,
    int ARRAY_SIZE)
{
  int i;
  double diffs = 0.0;

  for (i = 0; i < ARRAY_SIZE; i++)
  {
    diffs += Square (Array[i] - Mean);
  }
  *Var = diffs / ARRAY_SIZE;
  *Stddev = sqrt (*Var);
}

void
Calc_LinCorrCoef (double ArrayA[], double ArrayB[], double MeanA, double MeanB,
    int ARRAY_SIZE)
{
  int i;
  double numerator = 0.0;
  double Aterm = 0.0;
  double Bterm = 0.0;
  double Coef;

  for (i = 0; i < ARRAY_SIZE; i++)
  {
    numerator += (ArrayA[i] - MeanA) * (ArrayB[i] - MeanB);
    Aterm += Square (ArrayA[i] - MeanA);
    Bterm += Square (ArrayB[i] - MeanB);
  }
  Coef = numerator / (sqrt (Aterm) * sqrt (Bterm));
}

int
statistics (double ArrayA[], double ArrayB[], int ARRAY_SIZE) 
{
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
