/*
 * 
 * 
 * The algorithm is an adapted version of the 'Globally Convergent Newton Method'
 * algorithm found on pages 480-483 in the book "Numerical Recipes The Art Of Scientific Computing"
 * Third Edition by William H. Press et al
 */

#define XSIZE 10

typedef struct {
	int n;
	double lu[XSIZE][XSIZE];
	int indx[XSIZE];
	double d;
} LUdcmp;

LUdcmp initialiseLUdcmp(double a[][]) {
	LUdcmp newLUcmp;
	newLUcmp.n = XSIZE;
	
	int i, j;
	for (int i = 0; i < XSIZE; i++) {
		for (int j = 0; j < XSIZE; j++) {
			newLUcmp.lu[i][j] = a[i][j];
		}
	}

	const double TINY = 1.0e-40;
	int imax, k;
	double big, temp;
	double vv[XSIZE];
	newLUcmp.d = 1.0;

	for (i = 0; i < newLUcmp.n; i++) {
		big = 0.0;
		for (j = 0; j < newLUcmp.n; j++) {
			if ((temp = abs(newLUcmp.lu[i][j])) > big) {big = temp;}
		}
		//if (big == 0.0) throw("Singular matrix in LUdcmp");
		vv[i] = 1.0 / big;
	}
	
	for (k = 0; k < newLUcmp.n; k++) {
		big = 0.0;
		for (i = k; i < newLUcmp.n; i++) {
			temp = vv[i] * abs(newLUcmp.lu[i][k]);
			if (temp > big) {
				big = temp;
				imax = i;
			}
		}

		if (k != imax) {
			for (j = 0; j < newLUcmp.n; j++) {
				temp = newLUcmp.lu[imax][j];
				newLUcmp.lu[imax][j] = newLUcmp.lu[k][j];
				newLUcmp.lu[k][j] = temp;
			}
			newLUcmp.d = -newLUcmp.d;
			vv[imax] = vv[k];
		}
		newLUcmp.indx[k] = imax;
		if (newLUcmp.lu[k][k] == 0.0) {newLUcmp.lu[k][k] = TINY;}

		for (i = k + 1; i < newLUcmp.n; i++) {
			temp = newLUcmp.lu[i][k] /= newLUcmp.lu[k][k];
			for (j = k + 1; j < newLUcmp.n; j++) {
				newLUcmp.lu[i][j] -= temp * newLUcmp.lu[k][j];
			}
		}
	}
	return newLUcmp;
}

void solveLUdcmp(LUdcmp l, double b[], double x[]) {
	int i, ii = 0, ip, j;
	double sum;

	//if (b.size() != n || x.size() != n)
	//	throw("solveLUdcmp bad sizes");

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
	The routine requires a user-supplied function of functor that computes
	the vector of functions to be zeroed. Its declaration as a function is:
	VecDoub vecfunc(VecDoub_I x);
	(The name vecfunc is arbitrary.) The declaration as a functor is similar

	The vector of functions to be zeroed, called fvec[0..n-1] in the routine below,
	is returned by the user-supplied function or functor vecfunc.
 */
double[] vecfunc(double x[]) {
	
}

double fvec[];
// Returns f = 0.5F.F at x. ALso stores value of F in fvec
double NRfmin(double[] x) {
	int n = XSIZE;
	double sum = 0;
	fvec = vecfunc(x);
	for (int i = 0; i < n; i++) {
		//sum += SQR(fvec[i]);
		sum += (fvec[i] * fvec[i]);
	}
	return 0.5 * sum;
}

double[][] NRfdjac(double x[], double fvecIn[]) {
	const double EPS = 1.0e-8;
	int n = XSIZE;
	double df[XSIZE][XSIZE];

	double xh[XSIZE];
	for(int i = 0; i < XSIZE; i++) {
		xh[i] = x[i];
	}

	for(int j = 0; j < n; j++) {
		double temp = xh[j];
		double h = EPS * abs(temp);
		if (h == 0.0) h = EPS;
		xh[j] = temp + h;
		h = xh[j] - temp;
		double f[] = vecfunc(xh);
		xh[j] = temp;
		for (int i = 0; i < n; i++) {
			df[i][j] = (f[i] - fvecIn[i]) / h;
		}
	}
	return df;
}

void lnsearch(double xold[], const double fold, double g[], double p[],
			double x[], double &f, const double stpmax, bool &check) {
	const double ALF = 1.0e-4;
	// Library use DBL_EPSILON
	const double TOLX = DBL_EPSILON;

	double a, alam, alam2 = 2.0, alamin, b, disc, f2 = 0.0;
	double rhs1, rhs2, slope = 0.0, sum = 0.0, temp, test, tmplam;
	int i, n = XSIZE;

	check = false;
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
	//if (slope >= 0.0) throw ("Roundoff problem in lnsrch");

	test = 0.0;
	for (i = 0; i < n; i++) {
		temp = abs(p[i]) / MAX(abs(xold[i]), 1.0);
		if (temp > test) {test = temp;}
	}

	alamin = TOLX / test;
	alam = 1.0;
	for(;;) {
		for (i = 0; i < n; i++) x[i] = xold[i] + alam * p[i];
		f = NRfmin(x);
		
		if (alam < alamin) {
			for (i = 0; i < n; i++) x[i] = xold[i];
			check = true;
			return;
		} else if (f <= fold + ALF * alam * slope) {
			return;
		} else {
			if (alam == 1.0) {
				tmplam = -slope / (2.0 * (f - fold - slope));
			} else {
				rhs1 = f - fold - alam * slope;
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
		f2 = f;
		alam = MAX(tmplam, 0.1 * alam);
	}
}

void newt(double []x, bool &check) {
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
		if (abs(fvec[i]) > test) test = abs(fvec[i]);
	}
	if (test < 0.01 * TOLF) {
		check = false;
		return;
	}

	sum = 0.0;
	//for (i = 0; i < n; i++) sum += SQR(x[i]);
	for (i = 0; i < n; i++) sum += x[i] * x[i];
	stpmax = STPMX * MAX(sqrt(sum), (double)n);
	
	for (its = 0; its < MAXITS; its++) {
		fjac = NRfdjac(x, fvec);
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

		lnsearch(xold, fold, g, p, x, f, stpmax, check);
		test = 0.0;
		for (i = 0; i < n; i++) {
			if (abs(fvec[i]) > test) test = abs(fvec[i]);
		}
		if (test < TOLF) {
			check = false;
			return;
		}
		if (check) {
			test = 0.0;
			den = MAX(f, 0.5 * n);
			for (i = 0; i < n; i++) {
				temp = abs(g[i]) * MAX(abs(x[i]), 1.0) / den;
				if (temp > test) test = temp;
			}
			check = (test < TOLMIN);
			return;
		}
		
		test = 0.0;
		for (i = 0; i < n; i++) {
			temp = (abs(x[i] - xold[i])) / MAX(abs(x[i]), 1.0);
			if (temp > test) test = temp;
		}
		if (test < TOLX)
			return;
	}
	//throw("MAXITS exceeded in newt");
}



