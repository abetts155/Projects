/*
 * Takes a vector of 36 positive integers as arguments and uses them as edge weights to
 * calculate the longest shortest path from the root node
 * in a predefined graph using dijkstra's shortest path algorithm
 */

#define numverts 15
const unsigned int neighbours[numverts][4] =
{
	{1, 4, 5, 10},
	{0, 2, 6, 11},
	{1, 3, 7, 12},
	{2, 4, 8, 13},
	{0, 3, 9, 14},
	{0, 6, 9, 10},
	{1, 5, 7, 11},
	{2, 6, 8, 12},
	{3, 7, 9, 13},
	{4, 5, 8, 14},
	{0, 5, 11, 14},
	{1, 6, 10, 12},
	{2, 7, 11, 13},
	{3, 8, 12, 14},
	{4, 9, 10, 13},
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
	for (i = 0; i < numverts; i++)
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
	int distance[numverts];
	unsigned int unvisited[numverts];
	unsigned int currentNode;
	int numUnvisited;
	int newDistance, maxSmallestPath;
	int i, edgeIndex, neighbourIndex;

	for (i = 0; i < numverts; i++)
	{
		distance[i] = -1;
		unvisited[i] = i;
	}
	numUnvisited = numverts;

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
	int edgeWeights[numverts * 2];
	int i, result;

	/*
	 * 60 positive integer values must be supplied
	 */
	if (argc != ((numverts * 2) + 1))
	{
		return 1;
	}

	for (i = 0; i < numverts * 2; i++)
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
