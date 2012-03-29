/*
 * Takes a vector of poistive integers and returns a calculated value
 * the value is dependent on the first integer as a different function is selected
 * based upon the value of this integer
 */

int max(int ARRAY_SIZE, int a[])
{
	int i;
	int max = -1;
	for (i = 0; i < ARRAY_SIZE; i++)
	{
		if(a[i] > max)
		{
			max = a[i];
		}
	}
	return max;
}

int min(int ARRAY_SIZE, int a[])
{
	int i;
	int min = -1;
	for (i = 0; i < ARRAY_SIZE; i++)
	{
		if(a[i] < min || min == -1)
		{
			min = a[i];
		}
	}
	return min;
}

int sum(int ARRAY_SIZE, int a[])
{
	int i;
	int sum = 0;
	for (i = 0; i < ARRAY_SIZE; i++)
	{
		sum += a[i];
	}
	return sum;
}

int product(int ARRAY_SIZE, int a[])
{
	int i;
	int product = 1;
	for (i = 0; i < ARRAY_SIZE; i++)
	{
		product *= a[i];
	}
	return product;
}

int mean(int ARRAY_SIZE, int a[])
{
	int i;
	int mean = 0;
	for (i = 0; i < ARRAY_SIZE; i++)
	{
		mean += a[i];
	}
	mean /= ARRAY_SIZE;
	return mean;
}

int median(int ARRAY_SIZE, int a[])
{
	int pos = 1;
	int temp;
	
	while (pos < ARRAY_SIZE)
	{
		if (a[pos] >= a[pos - 1])
		{
			pos++;
		}
		else
		{
			// Swap a[pos] and a[pos - 1]
			temp = a[pos];
			a[pos] = a[pos - 1];
			a[pos - 1] = temp;

			if (pos > 1)
			{
				pos--;
			}
			else
			{
				pos++;
			}
		}
	}

	if (ARRAY_SIZE % 2)
	{
		return a[ARRAY_SIZE / 2];
	}
	else
	{
		return (a[ARRAY_SIZE / 2] + a[(ARRAY_SIZE / 2) - 1]) / 2;
	}
}

int listOps(int ARRAY_SIZE, int a[], int (*listOp)(int, int*))
{
	return listOp(ARRAY_SIZE, a);
}

int main (int argc, char *argv[])
{
	const int ARRAY_SIZE = argc - 1;
	int TV[ARRAY_SIZE];
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

	int (*func)(int, int*) = 0;
	int n = TV[0] % 6;

	switch (n)
	{
		case 0 : func = &max; break;
		case 1 : func = &min; break;
		case 2 : func = &sum; break;
		case 3 : func = &product; break;
		case 4 : func = &mean; break;
		default : func = &median; break;
	}

	int result = listOps (ARRAY_SIZE, TV, func);

	printf("%i\n", result);

	return 0;
}
