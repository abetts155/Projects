/*
 * Takes a vector of integers as arguments and sorts the vector into ascending order
 * using the gnome sort algorihtm
 */

void gnomesort (int ARRAY_SIZE, int a[])
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

	gnomesort (ARRAY_SIZE, TV);

	return 0;
}
