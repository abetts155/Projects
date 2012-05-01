/*
 * 
 * 
 * The algorithm is an adapted version of the 'Globally Convergent Newton Method'
 * algorithm found on pages 480-483 in the book "Numerical Recipes The Art Of Scientific Computing"
 * Third Edition by William H. Press et al
 */

#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define XSIZE 3

typedef struct {
	int n;
	double lu[XSIZE][XSIZE];
	int indx[XSIZE];
	double d;
} LUdcmp;

double maxDoub(double a, double b)
{
	if(a > b)
	{
		return a;
	}
	return b;
}

LUdcmp initialiseLUdcmp(double a[XSIZE][XSIZE]) {
	LUdcmp newLUdcmp;
	newLUdcmp.n = XSIZE;
	
	int i, j;
	for (i = 0; i < XSIZE; i++) {
		for (j = 0; j < XSIZE; j++) {
			newLUdcmp.lu[i][j] = a[i][j];
		}
	}

	const double TINY = 1.0e-40;
	int imax, k;
	double big, temp;
	double vv[XSIZE];
	newLUdcmp.d = 1.0;

	for (i = 0; i < newLUdcmp.n; i++) {
		big = 0.0;
		for (j = 0; j < newLUdcmp.n; j++) {
			if ((temp = fabs(newLUdcmp.lu[i][j])) > big) {big = temp;}
		}
		if (big == 0.0) { exit(1);}
		vv[i] = 1.0 / big;
	}
	
	for (k = 0; k < newLUdcmp.n; k++) {
		big = 0.0;
		for (i = k; i < newLUdcmp.n; i++) {
			temp = vv[i] * fabs(newLUdcmp.lu[i][k]);
			if (temp > big) {
				big = temp;
				imax = i;
			}
		}

		if (k != imax) {
			for (j = 0; j < newLUdcmp.n; j++) {
				temp = newLUdcmp.lu[imax][j];
				newLUdcmp.lu[imax][j] = newLUdcmp.lu[k][j];
				newLUdcmp.lu[k][j] = temp;
			}
			newLUdcmp.d = -newLUdcmp.d;
			vv[imax] = vv[k];
		}
		newLUdcmp.indx[k] = imax;
		if (newLUdcmp.lu[k][k] == 0.0) {newLUdcmp.lu[k][k] = TINY;}

		for (i = k + 1; i < newLUdcmp.n; i++) {
			temp = newLUdcmp.lu[i][k] /= newLUdcmp.lu[k][k];
			for (j = k + 1; j < newLUdcmp.n; j++) {
				newLUdcmp.lu[i][j] -= temp * newLUdcmp.lu[k][j];
			}
		}
	}
	return newLUdcmp;
}

void solveLUdcmp(LUdcmp l, double b[], double x[]) {
	int i, ii = 0, ip, j;
	double sum;

	//if (b.size() != l.n || x.size() != l.n)
	//	exit(1);

	for (i = 0; i < l.n; i++) {
		x[i] = b[i];
	}

	for (i = 0; i < l.n; i++) {
		ip = l.indx[i];
		sum = x[ip];
		x[ip] = x[i];
		if (ii != 0) {
			for (j = ii-1; j < i; j++) {
				sum -= l.lu[i][j] * x[j];
			}
		} else if (sum != 0.0) {
			ii = i + 1;
		}
		x[i] = sum;
	}

	for (i = l.n - 1; i >= 0 ; i--) {
		sum = x[i];
		for (j = i + 1; j < l.n; j++) {
			sum -= l.lu[i][j] * x[j];
		}
		x[i] = sum / l.lu[i][i];
	}
}

/*
	The routine requires a user-supplied function or functor that computes
	the vector of functions to be zeroed. Its declaration as a function is:
	VecDoub vecfunc(VecDoub_I x);
	(The name vecfunc is arbitrary.) The declaration as a functor is similar

	The vector of functions to be zeroed, called fvec[0..n-1] in the routine below,
	is returned by the user-supplied function or functor vecfunc.
 */
double funcValues[XSIZE];
double* vecfunc(double x[]) {
	int i;
	
	funcValues[0] = (x[0] * 3) - 18;
	funcValues[1] = pow(x[1], 3) + (-5 * pow(x[1], 2)) + (2 * x[1]) + 8;
	funcValues[2] = pow(x[2], 2) + (-5 * x[2]) + 6;
	/*for (i = 0; i < XSIZE; i++) {
		funcValues[i] = (x[i] * (i + 1)) - (i + 1);
	}*/

	return funcValues;
}

double fvec[XSIZE];
// Returns f = 0.5F.F at x. ALso stores value of F in fvec
double NRfmin(double x[]) {
	int n = XSIZE;
	double sum = 0;
	int i;
	double *fvectmp;

	fvectmp = vecfunc(x);
	for (i = 0; i < n; i++) {
		fvec[i] = fvectmp[i];
	}
	for (i = 0; i < n; i++) {
		//sum += SQR(fvec[i]);
		sum += (fvec[i] * fvec[i]);
	}
	return 0.5 * sum;
}

void NRfdjac(double x[], double fvecIn[], double df[XSIZE][XSIZE]) {
	const double EPS = 1.0e-8;
	int n = XSIZE;
	int i, j;

	double xh[XSIZE];
	for(i = 0; i < XSIZE; i++) {
		xh[i] = x[i];
	}

	for(j = 0; j < n; j++) {
		double temp = xh[j];
		double h = EPS * fabs(temp);
		if (h == 0.0) h = EPS;
		xh[j] = temp + h;
		h = xh[j] - temp;
		double *f = vecfunc(xh);
		xh[j] = temp;
		for (i = 0; i < n; i++) {
			df[i][j] = (f[i] - fvecIn[i]) / h;
		}
	}
}

void lnsearch(double xold[], const double fold, double g[], double p[],
			double x[], double *f, const double stpmax, int *check) {
	const double ALF = 1.0e-4;
	// Library use DBL_EPSILON
	const double TOLX = DBL_EPSILON;

	double a, alam, alam2 = 2.0, alamin, b, disc, f2 = 0.0;
	double rhs1, rhs2, slope = 0.0, sum = 0.0, temp, test, tmplam;
	int i, n = XSIZE;

	*check = 0;
	for (i = 0; i < n; i++) {
		sum += p[i] * p[i];
	}
	// Library use: sqrt
	sum = sqrt(sum);
	if (sum > stpmax) {
		for (i = 0; i < n; i++) {
			p[i] *= stpmax / sum;
		}
	}

	for (i = 0; i < n; i++) {
		slope += g[i] * p[i];
	}
	if (slope >= 0.0) { exit (1); }

	test = 0.0;
	for (i = 0; i < n; i++) {
		temp = fabs(p[i]) / maxDoub(fabs(xold[i]), 1.0);
		if (temp > test) {test = temp;}
	}

	alamin = TOLX / test;
	alam = 1.0;
	for(;;) {
		for (i = 0; i < n; i++) x[i] = xold[i] + alam * p[i];
		*f = NRfmin(x);
		
		if (alam < alamin) {
			for (i = 0; i < n; i++) x[i] = xold[i];
			*check = 1;
			return;
		} else if (*f <= fold + ALF * alam * slope) {
			return;
		} else {
			if (alam == 1.0) {
				tmplam = -slope / (2.0 * (*f - fold - slope));
			} else {
				rhs1 = *f - fold - alam * slope;
				rhs2 = f2 - fold - alam2 * slope;
				a = (rhs1 / (alam * alam) - rhs2 / (alam2 * alam2)) / (alam - alam2);
				b = (-alam2 * rhs1 / (alam * alam) + alam * rhs2 / (alam2 * alam2)) / (alam - alam2);
				if (a == 0.0) {
					tmplam = -slope / (2.0 * b);
				} else {
					disc = b * b - 3.0 * a * slope;
					if (disc < 0.0) tmplam = 0.5 * alam;
					else if (b <= 0.0) tmplam = (-b + sqrt(disc)) / (3.0 * a);
					else tmplam = -slope / (b + sqrt(disc));
				}
				if (tmplam > 0.5 * alam) {
					tmplam = 0.5 * alam;
				}
			}
		}
		alam2 = alam;
		f2 = *f;
		alam = maxDoub(tmplam, 0.1 * alam);
	}
}

void newtonMethod(double x[], int *check) {
	const int MAXITS = 200;
	const double TOLF = 1.0e-8, TOLMIN = 1.0e-12, STPMX = 100.0;
	// Library use DBL_EPSILON
	const double TOLX = DBL_EPSILON;

	int i, j, its, n = XSIZE;
	double den, f, fold, stpmax, sum, temp, test;
	double g[XSIZE], p[XSIZE], xold[XSIZE];
	double fjac[XSIZE][XSIZE];

	f = NRfmin(x);
	test = 0.0;

	for (i = 0; i < n; i++) {
		if (fabs(fvec[i]) > test) test = fabs(fvec[i]);
	}
	if (test < 0.01 * TOLF) {
		*check = 0;
		return;
	}

	sum = 0.0;
	for (i = 0; i < n; i++) sum += x[i] * x[i];
	stpmax = STPMX * maxDoub(sqrt(sum), (double)n);
	
	for (its = 0; its < MAXITS; its++) {
		NRfdjac(x, fvec, fjac);
		for (i = 0; i < n; i++) {
			sum = 0.0;
			for (j = 0; j < n; j++) {
				sum += fjac[j][i] * fvec[j];
			}
			g[i] = sum;
		}
		
		for (i = 0; i < n; i++) xold[i] = x[i];
		fold = f;
		for (i = 0; i < n; i++) p[i] = -fvec[i];
		
		LUdcmp alu = initialiseLUdcmp(fjac);
		solveLUdcmp(alu, p, p);

		lnsearch(xold, fold, g, p, x, &f, stpmax, check);
		test = 0.0;
		for (i = 0; i < n; i++) {
			if (fabs(fvec[i]) > test) test = fabs(fvec[i]);
		}
		if (test < TOLF) {			
			*check = 0;
			return;
		}
		if (*check) {
			test = 0.0;
			den = maxDoub(f, 0.5 * n);
			for (i = 0; i < n; i++) {
				temp = fabs(g[i]) * maxDoub(fabs(x[i]), 1.0) / den;
				if (temp > test) test = temp;
			}
			if (test < TOLMIN)
			{
				*check = 1;
			}
			else
			{
				*check = 0;
			}
			return;
		}
		
		test = 0.0;
		for (i = 0; i < n; i++) {
			temp = (fabs(x[i] - xold[i])) / maxDoub(fabs(x[i]), 1.0);
			if (temp > test) test = temp;
		}
		if (test < TOLX) {	
			return;
		}
	}
	exit(1);
}

int
main (int argc, char *argv[])
{
  const int ARRAY_SIZE = argc - 1;
  double TV[ARRAY_SIZE];
  int i;
  int check = 0;

  /*
   * Expecting XSIZE arguments
   */
  if (argc != (XSIZE + 1))
  {
    return 1;
  }

  for (i = 0; i < argc - 1; ++i)
  {
    TV[i] = atof (argv[i + 1]);
  }

  newtonMethod(TV, &check);

  printf("Check value = %i\n", check);

  for (i = 0; i < argc - 1; ++i)
  {
    printf("%lf ", TV[i]);
  }
  printf("\n");


  return 0;
}

