#define TEST_VECTOR_SIZE 39
#define LOBYTE(x) ((unsigned char)((x) & 0xFF))
#define HIBYTE(x) ((unsigned char)((x) >> 8))

unsigned short 
icrc1 (unsigned short crc, unsigned char onech)
{
  int i;
  unsigned short ans = (crc^onech << 8);
  for (i=0;i<8;i++) 
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
crc (unsigned short crc, unsigned long len, short jinit, int jrev, unsigned char* lin)
{
  static unsigned short icrctb[256],init=0;
  static unsigned char rchr[256];
  unsigned short tmp1, tmp2, j,cword=crc;
  static unsigned char it[16]={0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15};

  if (!init) 
  {
    init=1;
    for (j=0; j<=255; j++) 
    {
      icrctb[j] = icrc1 (j << 8,(unsigned char)0);
      rchr[j]   = (unsigned char)(it[j & 0xF] << 4 | it[j >> 4]);
    }
  }
  
  if (jinit >= 0) 
  {
    cword=((unsigned char) jinit) | (((unsigned char) jinit) << 8);
  }
  else if (jrev < 0)
  {  
    cword=rchr[HIBYTE(cword)] | rchr[LOBYTE(cword)] << 8;
  }
  
  for (j=1; j<=len; j++) 
  {
    if (jrev < 0) 
    {
      tmp1 = rchr[lin[j]]^ HIBYTE(cword);
    }
    else 
    {
      tmp1 = lin[j]^ HIBYTE(cword);
    }
    cword = icrctb[tmp1] ^ LOBYTE(cword) << 8;
  }
  
  if (jrev >= 0)
  {
    return cword;
  }
  else 
  {
    return rchr[HIBYTE(cword)] | rchr[LOBYTE(cword)] << 8;
  }
}

int
main (int argc, char *argv[])
{
  /*
   * At least as many integers as the TV size
   */
  if (argc != TEST_VECTOR_SIZE + 1)
  {
    return 1;
  }
  
  unsigned char lin[256];
  int i;
  for (i = 0; i < argc-1; ++i)
  {
    lin[i] = argv[i + 1][0];
  }

  unsigned short i1,i2;
  unsigned long n = TEST_VECTOR_SIZE + 1;
  lin[n+1] = 0;
  i1       = crc(0,n,(short)0,1, lin);
  lin[n+1] = HIBYTE(i1);
  lin[n+2] = LOBYTE(i1);
  i2       = crc(i1,n+2,(short)0,1, lin);
  
  printf("%d", i2);
  
  return 0;
}


