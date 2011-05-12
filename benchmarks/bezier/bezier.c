/*
 * Bezier curve algorithm taken from http://www-users.cs.york.ac.uk/~bernat/pwcet
 * and modified by Adam Betts to consume a test vector supplied on the command line.
 *
 * For this program, an 80-element test vector is expected.
 */

#define STEPWIDTH 0.01 /* draws 1/STEPWIDTH +1 points between SP and EP */
#define XSIZE 800
#define YSIZE 600
#define CURVENO 20

void
bezier (int data[20][4][2])
{
  char screen[YSIZE][XSIZE];
  int xco[4], yco[4];
  int i;
  int y;
  int x;
  float k;

  for (i = 0; i < CURVENO; i++)
  { /* PAN_VARPATH */
    /*calculate polynomial coefficients*/
    xco[3] = data[i][0][0];
    yco[3] = data[i][0][1];
    xco[2] = 3 * (data[i][2][0] - data[i][0][0]);
    yco[2] = 3 * (data[i][2][1] - data[i][0][1]);
    xco[1] = 3 * (data[i][3][0] - data[i][2][0]) - xco[2];
    yco[1] = 3 * (data[i][3][1] - data[i][2][1]) - yco[2];
    xco[0] = data[i][1][0] - data[i][0][0] - xco[2] - xco[1];
    yco[0] = data[i][1][1] - data[i][0][1] - yco[2] - yco[1];

    /*scan curve for t = 0 to t = 1 with STEPWIDTH*/
    for (k = 0; k <= 1; k += STEPWIDTH)
    { /* PAN_FIXED_LOOP PAN_VARPATH */
      x = (int) (((float) xco[0] * k * k * k) + ((float) xco[1] * k * k)
          + ((float) xco[2] * k) + (float) xco[3]);
      y = (int) (((float) yco[0] * k * k * k) + ((float) yco[1] * k * k)
          + ((float) yco[2] * k) + (float) yco[3]);
      if ((x < XSIZE) && (x > 0) && (y < YSIZE) && (y > 0))
      {
        /*write dot to screen*/
        screen[y][x] = 0; /*black*/
      }
    }
  }
}

int
main (int argc, char *argv[])
{
  int TV[20][4][2];
  int x;
  int i;
  int j;
  int k;

  if (argc != 81)
  {
    return 1;
  }

  k = 0;
  for (i = 0; i < 20; i++)
  {
    for (j = 0; j < 4; j++)
    {
      x = atoi (argv[(k + 1)]);
      TV[i][j][0] = x % 800;
      TV[i][j][1] = x % 800;
      k++;
    }
  }

  bezier (TV);

  return 0;
}

