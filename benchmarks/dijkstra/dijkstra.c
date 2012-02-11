/*
 * Takes a vector of 60 positive integers as arguments and uses them as edge weights to
 * calculate the longest shortest path from the root node
 * in a predefined graph using dijkstra's shortest path algorithm
 */

const unsigned int neighbours[30][4] =
{
	{1, 5, 6, 24},
	{0, 2, 7, 25},
	{1, 3, 8, 26},
	{2, 4, 9, 27},
	{3, 5, 10, 28},
	{0, 4, 11, 29},
	{0, 7, 11, 12},
	{1, 6, 8, 13},
	{2, 7, 9, 14},
	{3, 8, 10, 15},
	{4, 9, 11, 16},
	{5, 6, 10, 17},
	{6, 13, 17, 18},
	{7, 12, 14, 19},
	{8, 13, 15, 20},
	{9, 14, 16, 21},
	{10, 15, 17, 22},
	{11, 12, 16, 23},
	{12, 19, 23, 24},
	{13, 18, 20, 25},
	{14, 19, 21, 26},
	{15, 20, 22, 27},
	{16, 21, 23, 28},
	{17, 18, 22, 29},
	{0, 18, 25, 29},
	{1, 19, 24, 26},
	{2, 20, 25, 27},
	{3, 21, 26, 28},
	{4, 22, 27, 29},
	{5, 23, 24, 28},
};

int removeFromUnvisited(unsigned int node, int numUnvisited, unsigned int unvisited[])
{
	int i, removed;

	removed = 0;
	for (i = 0; i < numUnvisited; i++)
	{
		if (removed)
		{
			unvisited[i - 1] = unvisited[i];
		}
		else if (unvisited[i] == node)
		{
			removed = 1;
		}
	}

	return numUnvisited - removed;
}

int isUnvisited(int node, unsigned int unvisited[], int numUnvisited)
{
	int i;
	for (i = 0; i < numUnvisited; i++)
	{
		if (unvisited[i] == node)
		{
			return 1;
		}
	}
	return 0;
}

int smallestUnvisited(int distance[], unsigned int unvisited[], int numUnvisited)
{
	int smallestDistance = -1;
	int smallestIndex = -1;
	int i;
	for (i = 0; i < 30; i++)
	{
		if ( ((distance[i] < smallestDistance && distance[i] >= 0) || smallestDistance < 0)
				&& (isUnvisited(i, unvisited, numUnvisited) == 1))
		{
			smallestDistance = distance[i];
			smallestIndex = i;
		}
	}

	return smallestIndex;
}

int dijkstra(int edgeWeights[])
{
	int distance[30];
	unsigned int unvisited[30];
	unsigned int currentNode;
	int numUnvisited;
	int newDistance, maxSmallestPath;
	int i, edgeIndex, neighbourIndex;

	for (i = 0; i < 30; i++)
	{
		distance[i] = -1;
		unvisited[i] = i;
	}
	numUnvisited = 30;

	currentNode = 0;
	distance[currentNode] = 0;

	maxSmallestPath = 0;	
	edgeIndex = 0;

	while (numUnvisited > 0)
	{
		currentNode = smallestUnvisited(distance, unvisited, numUnvisited);
		
		for (i = 0; i < 4; i++)
		{
			neighbourIndex = neighbours[currentNode][i];
			if (isUnvisited(neighbourIndex, unvisited, numUnvisited) == 1)
			{
				newDistance = distance[currentNode] + edgeWeights[edgeIndex];
				edgeIndex++;

				if (newDistance < distance[neighbourIndex]
					|| distance[neighbourIndex] == -1)
				{
					distance[neighbourIndex] = newDistance;
				}
			}
		}

		numUnvisited = removeFromUnvisited(currentNode, numUnvisited, unvisited);
		if (maxSmallestPath < distance[currentNode])
		{
			maxSmallestPath = distance[currentNode];
		}
	}

	return maxSmallestPath;
}

int main(int argc, char *argv[])
{
	int edgeWeights[60];
	int i, result;

	/*
	 * 60 positive integer values must be supplied
	 */
	if (argc != (61))
	{
		return 1;
	}

	for (i = 0; i < 60; i++)
	{
		edgeWeights[i] = atoi(argv[i + 1]);
		if (edgeWeights[i] < 0)
		{
			return 1;
		}
	}

	result = dijkstra (edgeWeights);

	return 0;
}
