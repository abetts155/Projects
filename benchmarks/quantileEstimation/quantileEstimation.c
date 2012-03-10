/*
 * Takes an input vector of doubles and calculates an estimate of the 0.5 quantile (i.e. the median)
 * 
 * The algorithm is an adapted version of the 'Single-Pass Estimation of Arbitrary Quantiles'
 * algorithm found on pages 435-438 in the book "Numerical Recipes The Art Of Scientific Computing"
 * Third Edition by William H. Press et al
 */

#define nbuf 1000
#define nq 251

int nt = 0;
int nd = 0;

double pval[nq];
double dbuf[nbuf];
double qile[nq];

double q0 = 1.0e99;
double qm = -1.0e99;

double minDoub(double a, double b) {
	if(a < b) {return a;}
	else {return b;}
}

double maxDoub(double a, double b) {
	if(a > b) {return a;}
	else {return b;}
}

int merge (int ARRAY_SIZE, double a[], double b[], int l, int r, int u)
{
  int i = l;
  int j = r;
  int k = l;

  while (i < r && j < u)
  {
    if (a[i] <= a[j])
    {
      b[k] = a[i];
      i++;
    }
    else
    {
      b[k] = a[j];
      j++;
    }
    k++;
  }
  while (i < r)
  {
    b[k] = a[i];
    i++;
    k++;
  }
  while (j < u)
  {
    b[k] = a[j];
    j++;
    k++;
  }
  for (k = l; k < u; k++)
  {
    a[k] = b[k];
  }
}

void mergesort (int ARRAY_SIZE, double a[])
{
  int k = 1;
  int u;
  int i;
  double b[ARRAY_SIZE];

  while (k < ARRAY_SIZE)
  {
    i = 1;
    while (i + k <= ARRAY_SIZE)
    {
      u = i + k * 2;
      if (u > ARRAY_SIZE)
      {
        u = ARRAY_SIZE + 1;
      }
      merge (ARRAY_SIZE, a, b, i, i + k, u);
      i = i + k * 2;
    }
    k = k * 2;
  }
}

void initialise() {
	int i;
	for (i = 0; i < nq; i++) {
		qile[i] = 0.0;
	}

	for (i = 85; i <= 165; i++) {
		pval[i] = (i - 75.0) / 100.0;
	}

	for (i = 84; i >= 0; i--) {
		pval[i] = 0.87191909 * pval[i+1];
		pval[250 - i] = 1.0 - pval[i];
	}
}

void update() {
	int jd = 0, jq = 0, iq;
	double target, told = 0.0, tnew = 0.0;
	double qold, qnew;
	double newqile[nq];
	
	mergesort(nd, dbuf);
	qold = qnew = qile[0] = newqile[0] = q0;
	qile[nq-1] = newqile[nq-1] = qm;

	pval[0] = minDoub(0.5 / (nt + nd), 0.5 * pval[1]);
	pval[nq-1] = maxDoub(1.0 - 0.5 / (nt + nd), 0.5 * (1.0 + pval[nq - 2]));

	for(iq = 1; iq < nq - 1; iq++) {
		target = (nt + nd) * pval[iq];
		if (tnew < target) {
			for (;;) {
				if (jq < nq && (jd >= nd || qile[jq] < dbuf[jd])) {
					qnew = qile[jq];
					tnew = jd + nt * pval[jq++];
					if (tnew >= target) break;
				} else {
					qnew = dbuf[jd];
					tnew = told;
					if (qile[jq] > qile[jq-1]) {
						tnew += nt * (pval[jq] - pval[jq-1]) * (qnew - qold) / (qile[jq] - qile[jq-1]);
					}
					jd++;
					if (tnew >= target) break;
					told = tnew++;
					qold = qnew;
					if (tnew >= target) break;
				}
				told = tnew;
				qold = qnew;
			}			
		}
		if (tnew == told) {
			newqile[iq] = 0.5 * (qold + qnew);
		} else {
			newqile[iq] = qold + (qnew - qold) * (target - told) / (tnew - told);
		}
		told = tnew;
		qold = qnew;
	}
	int i;
	for(i = 0; i < nq; i++) {
		qile[i] = newqile[i];
	}
	nt += nd;
	nd = 0;
}

double report(double p) {
	double q;
	if (nd > 0) update();

	int jl = 0;
	int jh = nq - 1;
	int j;

	while (jh - jl > 1) {
		j = (jh + jl) >> 1;
		if (p > pval[j]) {jl = j;}
		else {jh = j;}
	}
	j = jl;
	q = qile[j] + (qile[j+1] - qile[j]) * (p - pval[j]) / (pval[j+1] - pval[j]);
	return maxDoub(qile[0], minDoub(qile[nq-1], q));
}

void add(double datum) {
	dbuf[nd++] = datum;
	if (datum < q0) {q0 = datum;}
	if (datum > qm) {qm = datum;}
	if (nd == nbuf) update();
}

double estimateQuantile(double a[], int ARRAY_SIZE, double quantile) {
	int i;	

	initialise();
	for (i = 0; i < ARRAY_SIZE; i++) {
		add(a[i]);
	}

	return report(quantile);
}

int main (int argc, char *argv[])
{
	const int ARRAY_SIZE = argc - 1;
	double TV[ARRAY_SIZE];
	int i;

	/*
	 * At least one integer value must be supplied
	 */
	if (argc == 1)
	{
		return 1;
	}

	for (i = 0; i < argc - 1; i++)
	{
		TV[i] = atoi (argv[i + 1]);
	}

	double quantileEstimate = estimateQuantile(TV, ARRAY_SIZE, 0.5);

	printf("%lf\n", quantileEstimate);

	return 0;
}
