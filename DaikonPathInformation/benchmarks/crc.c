#define TEST_VECTOR_SIZE 39
#define LOBYTE(x) ((unsigned char)((x) & 0xFF))
#define HIBYTE(x) ((unsigned char)((x) >> 8))

unsigned short 
icrc1 (unsigned short crc, unsigned char onech)
{
  #ifdef CBMC
  int __count_22_24 = 0;
  int __count_23_24 = 0;
  int __count_L25 = 0;
  #endif

  int i;
  unsigned short ans = (crc^onech << 8);
  for (i=0;
    #ifdef CBMC
    __count_L25++,
    #endif
  i<8;i++)  // 25
  {
    if (ans & 0x8000) // 21
    {
      // 22
      ans = (ans <<= 1) ^ 4129;
      #ifdef CBMC
      __count_22_24++;
      #endif
    }
    else
    {
      // 23
      ans <<= 1;
      #ifdef CBMC
      __count_23_24++;
      #endif
    }
  }
  return ans;
}

unsigned short 
crc (unsigned short crc, unsigned long len, short jinit, int jrev, unsigned char* lin)
{
  #ifdef CBMC
  int __count_1_6 = 0;
  int __count_3_4 = 0;
  int __count_7_10 = 0;
  int __count_8_9 = 0;
  int __count_8_10 = 0;
  int __count_12_14 = 0;
  int __count_13_14 = 0;
  int __count_16_17 = 0;
  int __count_16_18 = 0;
  int __count_L15 = 0;
  int __count_L5 = 0;
  #endif
  static unsigned short icrctb[256],init=0;
  static unsigned char rchr[256];
  unsigned short tmp1, tmp2, j,cword=crc;
  static unsigned char it[16]={0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15};

  if (!init) // 1
  {
    init=1;
    for (j=0; 
      #ifdef CBMC
      __count_L5++,
      #endif
    j<=255; j++) // 5
    {
      icrctb[j] = icrc1 (j << 8,(unsigned char)0);
      rchr[j]   = (unsigned char)(it[j & 0xF] << 4 | it[j >> 4]);
      #ifdef CBMC
      __count_3_4++;
      #endif
    }
  }
  #ifdef CBMC
  else __count_1_6++;
  #endif
  
  if (jinit >= 0) // 6
  {
    // 7
    #ifdef CBMC
    __count_7_10++;
    #endif
    cword=((unsigned char) jinit) | (((unsigned char) jinit) << 8);
  }
  else if (jrev < 0) // 8
  {  
    #ifdef CBMC
    __count_8_9++;
    #endif
    // 9
    cword=rchr[HIBYTE(cword)] | rchr[LOBYTE(cword)] << 8;
  }
  #ifdef CBMC
  __count_8_10++;
  #endif
  
  for (j=1; 
    #ifdef CBMC
    __count_L15++,
    #endif
  j<=len; j++) // 15
  {
    if (jrev < 0) // 11
    {
      // 12
      tmp1 = rchr[lin[j]]^ HIBYTE(cword);
      #ifdef CBMC
      __count_12_14++;
      #endif
    }
    else 
    {
      // 13
      tmp1 = lin[j]^ HIBYTE(cword);
      #ifdef CBMC
      __count_13_14++;
      #endif
    }
    cword = icrctb[tmp1] ^ LOBYTE(cword) << 8;
  }
  
  if (jrev >= 0) // 16
  {
    #ifdef CBMC
    __count_16_17++;
    #endif
    // 17
    return cword;
  }
  else 
  {
    #ifdef CBMC
    __count_16_18++;
    #endif
    // 18
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


