/*
 * Cyclic Redundancy Check (CRC) taken from MDH suite and modified by Adam
 * Betts to consume a test vector supplied on the command line.
 *
 * For this program, a one-element test vector is expected.
 */

typedef unsigned char uchar;
#define LOBYTE(x) ((uchar)((x) & 0xFF))
#define HIBYTE(x) ((uchar)((x) >> 8))

unsigned short
crc1 (unsigned char * lin, unsigned short the_crc, unsigned char onech)
{
  int i;
  unsigned short ans = (the_crc ^ onech << 8);

  for (i = 0; i < 8; i++)
  {
    if (ans & 0x8000)
    {
      ans = (ans <<= 1) ^ 4129;
    }
    else
    {
      ans <<= 1;
    }
  }
  return ans;
}

unsigned short
crc (unsigned char * lin, unsigned short the_crc, unsigned long len,
    short jinit, int jrev)
{
  static unsigned short icrctb[256];
  static unsigned short init = 0;
  static uchar rchr[256];
  static uchar it[16] =
    {0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15};

  unsigned short tmp1;
  unsigned short tmp2;
  unsigned short j;
  unsigned short cword = the_crc;

  if (!init)
  {
    init = 1;
    for (j = 0; j <= 255; j++)
    {
      icrctb[j] = crc1 (lin, j << 8, (uchar) 0);
      rchr[j] = (uchar) (it[j & 0xF] << 4 | it[j >> 4]);
    }
  }

  if (jinit >= 0)
  {
    cword = ((uchar) jinit) | (((uchar) jinit) << 8);
  }
  else if (jrev < 0)
  {
    cword = rchr[HIBYTE(cword)] | rchr[LOBYTE(cword)] << 8;
  }

  for (j = 1; j <= len; j++)
  {
    if (jrev < 0)
    {
      tmp1 = rchr[lin[j]] ^ HIBYTE(cword);
    }
    else
    {
      tmp1 = lin[j] ^ HIBYTE(cword);
    }
    cword = icrctb[tmp1] ^ LOBYTE(cword) << 8;
  }

  if (jrev >= 0)
  {
    tmp2 = cword;
  }
  else
  {
    tmp2 = rchr[HIBYTE(cword)] | rchr[LOBYTE(cword)] << 8;
  }
  return (tmp2);
}

int
main (int argc, char *argv[])
{
  unsigned short i1;
  unsigned short i2;
  unsigned long n = 40;
  unsigned char lin[256] = "asdffeagewaHAFEFaeDsFEawFdsFaefaeerdjgp";

  lin[n + 1] = 0;
  i1 = crc (lin, 0, n, (short) 0, 1);
  lin[n + 1] = HIBYTE(i1);
  lin[n + 2] = LOBYTE(i1);
  i2 = crc (lin, i1, n + 2, (short) 0, 1);

  printf ("%d %d %s\n", i1, i2, lin);

  return 0;
}
