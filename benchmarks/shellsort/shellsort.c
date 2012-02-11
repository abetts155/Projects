/*
 * Takes a vector of integers as arguments and sorts the vector into ascending order
 * using the shell sort algorihtm
 */

const int numGaps = 2;
const int gaps[2] = {2, 1};

void shellsort (int ARRAY_SIZE, int a[])
{
	int g, gap;
	int i, j;
	int temp;

	for (g = 0; g < numGaps; g++)
	{
		gap = gaps[g];

		for (i = gap; i < ARRAY_SIZE; i++)
		{
			temp = a[i];
			for (j = i; j >= gap && a[j - gap] > temp; j -= gap)
			{
				a[j] = a[j - gap];
			}
			a[j] = temp;
		}
	}
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

	for (i = 0; i < argc - 1; ++i)
	{
		TV[i] = atoi (argv[i + 1]);
	}

	shellsort (ARRAY_SIZE, TV);

	for (i = 0; i < ARRAY_SIZE; i++)
	{
		printf("%i\n", TV[i]);
	}

	return 0;
}
