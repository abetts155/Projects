/*
 * Select taken from MDH suite and modified by Adam Betts to consume a
 * test vector supplied on the command line.
 *
 * For this program, a one-element test vector k is expected to select
 * the kth largest number out of n numbers. Currently, n is fixed to 1000.
 */

#define SWAP(a,b) temp=(a);(a)=(b);(b)=temp;

#define ARRAY_SIZE 1000

float arr[ARRAY_SIZE] =
  {18.65, 14.9, 86.85, 48.38, 94.78, 66.62, 18.25, 55.73, 49.43, 39.94, 52.63,
      59.9, 67.81, 82.52, 48.56, 56.75, 66.37, 11.49, 1.24, 15.08, 24.62,
      89.62, 52.47, 1.39, 1.98, 64.35, 98.29, 79.36, 94.22, 11.94, 91.29,
      54.24, 8.11, 37.32, 83.4, 6.09, 91.1, 4.93, 23.75, 70.26, 69.86, 2.48,
      76.54, 69.85, 40.5, 50.2, 37.95, 28.28, 61.1, 13.21, 19.12, 93.39, 28.86,
      1.36, 7.35, 12.59, 94.1, 18.04, 51.11, 78.85, 20.49, 71.34, 44.81, 38.45,
      40.23, 88.52, 94.63, 55.39, 61.67, 18.98, 33.47, 40.28, 53.54, 9.62,
      71.04, 11.33, 77.19, 29.77, 69.55, 60.18, 34.76, 49.67, 59.91, 32.87,
      70.41, 77.4, 19.04, 6.26, 47.68, 44.72, 41.03, 22.86, 70.14, 5.92, 55.71,
      36.5, 63.4, 36.44, 65.33, 87.12, 58.09, 93.27, 84.92, 78.13, 67.98,
      28.92, 87.38, 8.62, 77.78, 4.32, 2.61, 39.2, 78.14, 48.8, 12.69, 8.02,
      63.74, 54.35, 44.82, 95.41, 15.33, 47.9, 62.38, 53.51, 14.52, 35.04,
      8.71, 50.18, 29.48, 1.64, 48.28, 52.2, 41.95, 79.41, 32.73, 94.45, 37.44,
      61.41, 91.5, 18.74, 69.88, 34.22, 94.96, 20.59, 49.98, 89.8, 57.06, 7.42,
      50.56, 23.67, 1.37, 15.51, 11.57, 33.94, 69.42, 52.68, 2.67, 70.34,
      21.28, 52.81, 35.93, 54.67, 64.32, 74.17, 73.93, 38.81, 76.44, 29.21,
      93.43, 46.56, 30.59, 97.57, 33.1, 22.6, 92.81, 11.35, 48.5, 89.21, 40.2,
      42.77, 46.82, 14.79, 31.13, 60.48, 39.13, 92.34, 83.96, 22.59, 78.99,
      65.23, 92.81, 15.8, 21.61, 80.85, 22.29, 48.78, 92.63, 55.33, 21.47,
      26.49, 35.76, 86.42, 63.96, 88.71, 49.55, 76.38, 92.46, 30.48, 2.37,
      60.64, 13.39, 86.24, 17.21, 77.87, 50.96, 75.04, 33.64, 6.34, 82.96,
      77.71, 51.39, 43.96, 61.53, 13.65, 69.24, 18.14, 84.3, 11.12, 58.42,
      4.81, 89.77, 14.86, 83.48, 33.47, 38.14, 2.33, 67.79, 22.69, 49.45,
      39.93, 70.87, 36.06, 16.56, 59.51, 43.48, 18.76, 12.54, 32.81, 2.24,
      94.32, 91.25, 59.29, 45.74, 41.59, 95.8, 38.63, 42.78, 99.97, 26.47,
      34.96, 87.11, 31.5, 73.16, 29.48, 30.61, 42.77, 36.68, 19.82, 4.75, 5.07,
      25.27, 80.96, 24.8, 42.5, 32.76, 46.67, 40.4, 28.59, 28.93, 42.53, 67.76,
      5.58, 80.86, 86.07, 7.99, 63.04, 83.05, 28.14, 85.44, 55.41, 71.86, 74.4,
      16.72, 24.56, 95.06, 63.68, 85.14, 64.64, 92.6, 51.18, 78.9, 28.89,
      94.94, 91.2, 34.12, 4.88, 78.59, 40.18, 95.01, 99.64, 1.1, 55.14, 40.28,
      47.05, 77.89, 32.68, 6.85, 9.43, 31.37, 21.36, 73.42, 42.05, 29.05,
      61.54, 92.47, 86.44, 41.14, 64.7, 82.01, 92.48, 46.01, 32.5, 97.33, 17.6,
      66.92, 46.31, 36.16, 79.39, 51.1, 43.41, 24.47, 72.88, 26.37, 72.65, 8.2,
      12.17, 43.26, 52.24, 13.88, 22.17, 8.66, 61.88, 43.15, 49.5, 29.39,
      15.31, 77.22, 76.19, 5.58, 97.31, 88.49, 55.78, 57.75, 29.3, 39.14,
      61.12, 93.4, 66.77, 49.56, 29.47, 19.27, 85.08, 94.52, 92.92, 63.89,
      55.61, 33.07, 46.51, 9.78, 11.26, 73.94, 98.64, 81.75, 27.73, 88.36,
      96.39, 44.04, 82.89, 87.3, 61.03, 98.81, 2.25, 17.06, 30.43, 62.4, 83.75,
      93.81, 17.74, 43.61, 61.36, 2.29, 74.22, 8.61, 99.54, 48.41, 79.25, 4.65,
      74.17, 7.17, 75.85, 87.58, 69.77, 10.09, 42.7, 7.22, 76.51, 22.86, 23.99,
      97.85, 78.18, 68.21, 31.74, 61.14, 43.21, 38.5, 99.32, 69.59, 94.56,
      58.77, 91.61, 36.32, 30.47, 27.37, 84.32, 45.37, 51.27, 61.39, 39.95,
      31.53, 19.25, 32.62, 82.31, 99.84, 4.6, 77.17, 75.28, 64.63, 5.54, 49.5,
      58.42, 52.46, 28.55, 48.42, 90.95, 68.74, 33.61, 10.01, 96.95, 5.31,
      83.01, 21.56, 73.37, 83.72, 40.07, 65.27, 43.08, 11.93, 33.42, 98.94,
      88.13, 31.54, 87.67, 46.51, 86.82, 75.28, 54.94, 1.59, 31.68, 81.88,
      26.27, 83.93, 78.52, 72.11, 64.76, 59.65, 31.43, 7.89, 34.21, 47.33,
      17.35, 16.11, 30.24, 99.07, 83.78, 86.84, 47.1, 54.75, 80.38, 31.38,
      25.49, 77.22, 17.04, 59.53, 2.18, 54.1, 56.48, 71.84, 46.53, 42.99,
      45.52, 18.87, 67.0, 12.01, 39.04, 95.68, 95.49, 56.57, 9.38, 19.14,
      23.18, 87.96, 60.91, 13.45, 9.53, 25.7, 13.92, 24.22, 25.62, 1.84, 11.93,
      86.78, 7.33, 92.3, 75.98, 73.2, 71.27, 81.17, 87.68, 13.83, 7.37, 90.35,
      3.78, 43.9, 42.78, 97.7, 9.82, 29.92, 17.11, 4.66, 46.31, 82.62, 99.57,
      44.61, 14.01, 7.84, 23.46, 22.87, 92.47, 92.45, 41.09, 19.61, 40.93,
      2.44, 50.72, 87.4, 77.47, 65.06, 55.06, 14.39, 89.34, 33.91, 57.91,
      11.98, 65.49, 24.56, 15.92, 44.1, 85.54, 23.1, 22.14, 32.16, 1.21, 48.63,
      20.31, 58.5, 55.68, 97.84, 42.43, 64.94, 4.47, 4.6, 43.76, 14.01, 39.26,
      66.27, 43.17, 13.82, 59.86, 60.4, 28.77, 56.7, 13.86, 96.47, 14.54,
      42.51, 36.06, 60.24, 57.48, 99.63, 25.83, 25.14, 81.8, 78.35, 18.4,
      37.64, 82.82, 94.17, 48.96, 1.41, 75.98, 16.59, 97.72, 49.98, 35.04,
      84.19, 13.88, 1.52, 47.37, 81.02, 91.18, 73.04, 23.73, 49.5, 19.35,
      57.85, 34.82, 46.07, 23.9, 20.28, 23.79, 91.91, 54.24, 53.03, 42.56,
      9.68, 89.27, 20.53, 68.21, 25.63, 6.25, 82.62, 32.83, 14.75, 98.63, 12.2,
      42.94, 85.79, 20.1, 87.51, 67.62, 62.22, 38.49, 26.88, 55.65, 6.76, 4.41,
      66.16, 52.14, 47.45, 21.85, 69.14, 34.3, 24.75, 44.59, 87.86, 73.04,
      13.66, 95.17, 56.08, 20.91, 56.4, 20.47, 93.48, 97.69, 32.45, 21.99,
      67.72, 44.04, 96.04, 34.39, 34.09, 92.22, 78.21, 91.66, 73.26, 89.66,
      91.58, 47.75, 5.11, 70.2, 54.95, 70.92, 79.15, 71.34, 44.12, 53.52, 1.33,
      68.47, 32.48, 31.76, 69.5, 93.3, 84.13, 30.79, 37.05, 72.48, 15.53,
      26.83, 65.36, 56.97, 38.42, 54.24, 92.32, 24.83, 19.81, 11.24, 45.3,
      87.08, 99.31, 33.05, 66.06, 81.41, 88.22, 19.4, 71.87, 50.94, 97.11,
      19.5, 85.95, 34.83, 42.09, 63.91, 4.91, 15.83, 77.81, 53.01, 60.21,
      94.12, 92.13, 24.55, 24.97, 25.45, 91.28, 70.91, 47.34, 91.79, 65.71,
      53.47, 12.05, 26.58, 6.49, 72.9, 50.91, 90.99, 72.68, 69.98, 10.66,
      37.27, 93.52, 63.06, 12.28, 96.16, 91.41, 75.63, 85.35, 73.44, 79.29,
      85.18, 64.66, 82.81, 52.18, 33.15, 74.51, 78.33, 23.1, 63.02, 10.82,
      79.52, 73.82, 3.01, 67.44, 74.21, 25.16, 55.9, 41.74, 3.67, 72.26, 34.76,
      55.84, 49.33, 16.41, 69.86, 2.94, 66.86, 27.71, 61.97, 58.91, 13.17,
      21.2, 94.42, 54.39, 35.47, 23.37, 38.71, 23.66, 6.02, 17.66, 39.47,
      51.32, 17.88, 27.85, 45.62, 20.02, 24.77, 78.2, 26.6, 73.01, 64.25,
      55.86, 81.34, 9.8, 44.48, 10.07, 33.06, 67.98, 87.62, 4.58, 24.7, 81.24,
      58.85, 72.63, 98.25, 74.59, 18.73, 6.3, 6.45, 31.71, 51.26, 9.6, 44.27,
      18.32, 15.55, 94.33, 32.09, 93.39, 34.38, 81.7, 5.23, 4.55, 3.81, 18.85,
      18.74, 55.22, 31.02, 13.77, 10.6, 25.38, 99.2, 89.43, 17.39, 99.08,
      88.15, 69.08, 13.57, 82.28, 75.95, 20.23, 42.6, 61.14, 86.52, 41.46,
      15.56, 88.75, 8.58, 68.75, 22.77, 2.88, 11.37, 72.32, 75.79, 65.82,
      15.88, 3.47, 28.7, 3.42, 49.34, 24.66, 95.44, 29.69, 6.98, 97.94, 62.11,
      3.15, 34.91, 21.45, 34.64, 95.93, 60.91, 16.95, 71.36, 17.87, 13.9, 50.6,
      12.44, 75.55, 35.92, 2.35, 94.27, 21.28, 76.51, 29.11, 82.16, 58.77,
      62.87, 93.35, 72.62, 90.12, 97.02, 92.2, 83.19, 9.45, 84.39, 3.54, 17.89,
      46.84, 76.5, 80.7, 16.0, 32.88, 80.72, 15.11, 6.68, 4.84, 80.32, 34.45,
      21.95, 93.94, 76.65, 3.84, 13.42, 71.23, 70.47, 85.27, 15.14, 80.9, 8.57,
      6.78, 11.03, 9.96, 43.87, 3.25, 37.0, 28.5, 8.16, 63.55, 34.73, 7.73,
      7.78, 63.8, 21.98, 19.64, 53.31, 61.51, 83.33, 13.76, 22.94, 99.56,
      54.55, 56.46, 40.08, 54.16, 73.0, 59.4, 94.31, 54.64, 95.9, 40.46, 32.79,
      67.68, 80.42, 42.06, 17.39, 63.06, 31.33, 7.65, 14.13, 67.25, 43.05,
      1.86, 36.99};

float
select (unsigned long k)
{
  unsigned long i, ir, j, l, mid;
  float a, temp;
  int flag, flag2;

  l = 1;
  ir = ARRAY_SIZE - 1;
  flag = flag2 = 0;

  while (!flag)
  {
    if (ir <= l + 1)
    {
      if (ir == l + 1)
      {
        if (arr[ir] < arr[l])
        {
          SWAP(arr[l], arr[ir])
        }
      }
      flag = 1;
    }
    else if (!flag)
    {
      mid = (l + ir) >> 1;
      SWAP(arr[mid], arr[l + 1])
      if (arr[l + 1] > arr[ir])
      {
        SWAP(arr[l + 1], arr[ir])
      }
      if (arr[l] > arr[ir])
      {
        SWAP(arr[l], arr[ir])
      }
      if (arr[l + 1] > arr[l])
      {
        SWAP(arr[l + 1], arr[l])
      }
      i = l + 1;
      j = ir;
      a = arr[l];
      while (!flag2)
      {
        i++;
        while (arr[i] < a)
        {
          i++;
        }
        j--;
        while (arr[j] > a)
        {
          j--;
        }
        if (j < i)
        {
          flag2 = 1;
        }
        if (!flag2)
        {
          SWAP(arr[i], arr[j])
        }
      }
      arr[l] = arr[j];
      arr[j] = a;
      if (j >= k)
      {
        ir = j - 1;
      }
      if (j <= k)
      {
        l = i;
      }
    }
  }
  return arr[k];
}

int
main (int argc, char *argv[])
{
  int element;

  /*
   * One integer must be supplied
   */
  if (argc != 2)
  {
    return 1;
  }

  element = select (atoi (argv[1]));
  
  printf("%d", element);

  return 0;
}
