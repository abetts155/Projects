/*
 * Fast Discrete Cosine Transform (FDCT) taken from MDH suite and modified by
 * Adam Betts to consume a test vector supplied on the command line.
 *
 * For this program, the test vector is a list of 64 integers.
 */

#define W1 2841                 /* 2048*sqrt(2)*cos(1*pi/16) */
#define W2 2676                 /* 2048*sqrt(2)*cos(2*pi/16) */
#define W3 2408                 /* 2048*sqrt(2)*cos(3*pi/16) */
#define W5 1609                 /* 2048*sqrt(2)*cos(5*pi/16) */
#define W6 1108                 /* 2048*sqrt(2)*cos(6*pi/16) */
#define W7 565                  /* 2048*sqrt(2)*cos(7*pi/16) */

#define CONST_BITS  13
#define PASS1_BITS  2

void
fdct (int *blk)
{
  const int lx = 8;
  int tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7;
  int tmp10, tmp11, tmp12, tmp13;
  int z1, z2, z3, z4, z5;
  int i;
  short int *block;

  int constant;

  block = blk;

  for (i = 0; i < 8; i++)
  {
    tmp0 = block[0] + block[7];
    tmp7 = block[0] - block[7];
    tmp1 = block[1] + block[6];
    tmp6 = block[1] - block[6];
    tmp2 = block[2] + block[5];
    tmp5 = block[2] - block[5];
    tmp3 = block[3] + block[4];
    tmp4 = block[3] - block[4];

    tmp10 = tmp0 + tmp3;
    tmp13 = tmp0 - tmp3;
    tmp11 = tmp1 + tmp2;
    tmp12 = tmp1 - tmp2;

    block[0] = ((tmp10 + tmp11) << PASS1_BITS);
    block[4] = ((tmp10 - tmp11) << PASS1_BITS);

    constant = 4433;
    z1 = (tmp12 + tmp13) * constant;
    constant = 6270;
    block[2] = (z1 + (tmp13 * constant)) >> (CONST_BITS - PASS1_BITS);
    constant = -15137;
    block[6] = (z1 + (tmp12 * constant)) >> (CONST_BITS - PASS1_BITS);

    z1 = tmp4 + tmp7;
    z2 = tmp5 + tmp6;
    z3 = tmp4 + tmp6;
    z4 = tmp5 + tmp7;
    constant = 9633;
    z5 = ((z3 + z4) * constant); /* sqrt(2) * c3 */

    constant = 2446;
    tmp4 = (tmp4 * constant); /* sqrt(2) * (-c1+c3+c5-c7) */
    constant = 16819;
    tmp5 = (tmp5 * constant); /* sqrt(2) * ( c1+c3-c5+c7) */
    constant = 25172;
    tmp6 = (tmp6 * constant); /* sqrt(2) * ( c1+c3+c5-c7) */
    constant = 12299;
    tmp7 = (tmp7 * constant); /* sqrt(2) * ( c1+c3-c5-c7) */
    constant = -7373;
    z1 = (z1 * constant); /* sqrt(2) * (c7-c3) */
    constant = -20995;
    z2 = (z2 * constant); /* sqrt(2) * (-c1-c3) */
    constant = -16069;
    z3 = (z3 * constant); /* sqrt(2) * (-c3-c5) */
    constant = -3196;
    z4 = (z4 * constant); /* sqrt(2) * (c5-c3) */

    z3 += z5;
    z4 += z5;

    block[7] = (tmp4 + z1 + z3) >> (CONST_BITS - PASS1_BITS);
    block[5] = (tmp5 + z2 + z4) >> (CONST_BITS - PASS1_BITS);
    block[3] = (tmp6 + z2 + z3) >> (CONST_BITS - PASS1_BITS);
    block[1] = (tmp7 + z1 + z4) >> (CONST_BITS - PASS1_BITS);

    block += lx;
  }

  block = blk;

  for (i = 0; i < 8; i++)
  {
    tmp0 = block[0] + block[7* lx ];
    tmp7 = block[0] - block[7* lx ];
    tmp1 = block[lx] + block[6* lx ];
    tmp6 = block[lx] - block[6* lx ];
    tmp2 = block[2* lx ] + block[5* lx ];
    tmp5 = block[2* lx ] - block[5* lx ];
    tmp3 = block[3* lx ] + block[4* lx ];
    tmp4 = block[3* lx ] - block[4* lx ];

    tmp10 = tmp0 + tmp3;
    tmp13 = tmp0 - tmp3;
    tmp11 = tmp1 + tmp2;
    tmp12 = tmp1 - tmp2;

    block[0] = (tmp10 + tmp11) >> (PASS1_BITS + 3);
    block[4* lx ] = (tmp10 - tmp11) >> (PASS1_BITS + 3);

    constant = 4433;
    z1 = ((tmp12 + tmp13) * constant);
    constant = 6270;
    block[2* lx ] = (z1 + (tmp13 * constant)) >> (CONST_BITS + PASS1_BITS + 3);
    constant = -15137;
    block[6* lx ] = (z1 + (tmp12 * constant)) >> (CONST_BITS + PASS1_BITS + 3);

    z1 = tmp4 + tmp7;
    z2 = tmp5 + tmp6;
    z3 = tmp4 + tmp6;
    z4 = tmp5 + tmp7;
    constant = 9633;
    z5 = ((z3 + z4) * constant); /* sqrt(2) * c3 */

    constant = 2446;
    tmp4 = (tmp4 * constant); /* sqrt(2) * (-c1+c3+c5-c7) */
    constant = 16819;
    tmp5 = (tmp5 * constant); /* sqrt(2) * ( c1+c3-c5+c7) */
    constant = 25172;
    tmp6 = (tmp6 * constant); /* sqrt(2) * ( c1+c3+c5-c7) */
    constant = 12299;
    tmp7 = (tmp7 * constant); /* sqrt(2) * ( c1+c3-c5-c7) */
    constant = -7373;
    z1 = (z1 * constant); /* sqrt(2) * (c7-c3) */
    constant = -20995;
    z2 = (z2 * constant); /* sqrt(2) * (-c1-c3) */
    constant = -16069;
    z3 = (z3 * constant); /* sqrt(2) * (-c3-c5) */
    constant = -3196;
    z4 = (z4 * constant); /* sqrt(2) * (c5-c3) */

    z3 += z5;
    z4 += z5;

    block[7* lx ] = (tmp4 + z1 + z3) >> (CONST_BITS + PASS1_BITS + 3);
    block[5* lx ] = (tmp5 + z2 + z4) >> (CONST_BITS + PASS1_BITS + 3);
    block[3* lx ] = (tmp6 + z2 + z3) >> (CONST_BITS + PASS1_BITS + 3);
    block[lx] = (tmp7 + z1 + z4) >> (CONST_BITS + PASS1_BITS + 3);

    block++;
  }
}

int
main (int argc, char *argv[])
{
  const int ARRAY_SIZE = 64;
  int TV[ARRAY_SIZE];
  int i;

  /*
   * 64 integers must be supplied
   */
  if (argc != ARRAY_SIZE + 1)
  {
    return 1;
  }

  for (i = 0; i < ARRAY_SIZE; ++i)
  {
    TV[i] = atoi (argv[i + 1]);
  }

  fdct (TV);

  return 0;
}
