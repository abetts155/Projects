#define TEST_VECTOR_SIZE 39
#define LOBYTE(x) ((unsigned char)((x) & 0xFF))
#define HIBYTE(x) ((unsigned char)((x) >> 8))

unsigned short 
icrc1 (unsigned short crc, unsigned char onech)
{
  #ifdef CBMC
//==========> icrc1 : header 25
int __count_21_22 = 0;
int __count_21_23 = 0;
int __count_25_21 = 0; //Loop counter
//==========> icrc1 : header 20
int __count_26 = 0;
int __count_25_26 = 0;
  #endif

  int i;
  unsigned short ans = (crc^onech << 8);
  #ifdef CBMC
  __count_25_21 = 0;
  #endif
  for (i=0;i<8;i++)  // 25
  {
    #ifdef CBMC
    __count_25_21++;
    #endif
    if (ans & 0x8000) // 21
    {
      #ifdef CBMC
      __count_21_22++;
      #endif
      // 22
      ans = (ans <<= 1) ^ 4129;
    }
    else
    {
      #ifdef CBMC
      __count_21_23++;
      #endif
      // 23
      ans <<= 1;
    }
  }
  #ifdef CBMC
  assert(__count_25_21  <= 9); // Loop counter property
  __count_25_26++;
  __count_26++;
  #endif

#ifdef CBMC
assert(__count_21_22 <= 8); // Upper capacity constraint
assert(__count_21_23 <= 8); // Upper capacity constraint
assert(__count_26 >= 1); // Lower capacity constraint
assert(__count_26 <= 1); // Upper capacity constraint
assert(__count_25_26 >= 1); // Lower capacity constraint
assert(__count_25_26 <= 1); // Upper capacity constraint
assert(__count_21_22 > 0 ==> __count_25_26 > 0); // Execution dependence
assert(__count_21_22 > 0 ==> __count_26 > 0); // Execution dependence
assert(__count_21_23 > 0 ==> __count_25_26 > 0); // Execution dependence
assert(__count_21_23 > 0 ==> __count_26 > 0); // Execution dependence
#endif

  return ans;
}

unsigned short 
crc (unsigned short crc, unsigned long len, short jinit, int jrev, unsigned char* lin)
{
#ifdef CBMC
//==========> crc : header 15
int __count_11_12 = 0;
int __count_11_13 = 0;
int __count_15_11 = 0; //Loop counter
//==========> crc : header 5
int __count_3_4 = 0;
int __count_5_3 = 0; //Loop counter
//==========> crc : header 1
int __count_19 = 0;
int __count_1_6 = 0;
int __count_5_6 = 0;
int __count_7_10 = 0;
int __count_8_9 = 0;
int __count_8_10 = 0;
int __count_16_17 = 0;
int __count_16_18 = 0;
#endif
  static unsigned short icrctb[256],init=0;
  static unsigned char rchr[256];
  unsigned short tmp1, tmp2, j,cword=crc;
  static unsigned char it[16]={0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15};

  if (!init) // 1
  {
    init=1;
    #ifdef CBMC
    __count_5_3 = 0;
    #endif
    for (j=0; j<=255; j++) // 5
    {
      #ifdef CBMC
      __count_5_3++;
      #endif
      icrctb[j] = icrc1 (j << 8,(unsigned char)0);
      rchr[j]   = (unsigned char)(it[j & 0xF] << 4 | it[j >> 4]);
      #ifdef CBMC
      __count_3_4++;
      #endif
    }
    #ifdef CBMC
    assert(__count_5_3  <= 257); // Loop counter property
    __count_5_6++;
    #endif
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
  
  #ifdef CBMC
  __count_15_11 = 0;
  #endif
  for (j=1; j<=len; j++) // 15
  {
    #ifdef CBMC
    __count_15_11++;
    #endif
    if (jrev < 0) // 11
    {
      #ifdef CBMC
      __count_11_12++;
      #endif
      // 12
      tmp1 = rchr[lin[j]]^ HIBYTE(cword);
    }
    else 
    {
      #ifdef CBMC
      __count_11_13++;
      #endif
      // 13
      tmp1 = lin[j]^ HIBYTE(cword);
    }
    cword = icrctb[tmp1] ^ LOBYTE(cword) << 8;
  }
  #ifdef CBMC
  assert(__count_15_11  <= 43); // Loop counter property
  #endif
  
  if (jrev >= 0) // 16
  {
    #ifdef CBMC
    __count_16_17++;
    #endif
    // 17
    #ifdef CBMC
    __count_19++;
    #endif

#ifdef CBMC
assert(__count_11_12 == 0); // Dead code
assert(__count_11_13 >= 40); // Lower capacity constraint
assert(__count_11_13 <= 42); // Upper capacity constraint
assert(__count_16_17 >= 1); // Lower capacity constraint
assert(__count_16_17 <= 1); // Upper capacity constraint
assert(__count_16_18 == 0); // Dead code
assert(__count_19 >= 1); // Lower capacity constraint
assert(__count_19 <= 1); // Upper capacity constraint
assert(__count_1_6 <= 1); // Upper capacity constraint
assert(__count_3_4 <= 256); // Upper capacity constraint
assert(__count_5_6 <= 1); // Upper capacity constraint
assert(__count_7_10 >= 1); // Lower capacity constraint
assert(__count_7_10 <= 1); // Upper capacity constraint
assert(__count_8_9 == 0); // Dead code
assert(__count_8_10 == 0); // Dead code
assert(__count_3_4 > 0 ==> __count_5_6 > 0); // Mutual inclusion
assert(__count_5_6 > 0 ==> __count_3_4 > 0); // Mutual inclusion
assert(__count_1_6 > 0 ==> __count_19 > 0); // Execution dependence
assert(__count_1_6 > 0 ==> __count_7_10 > 0); // Execution dependence
assert(__count_1_6 > 0 ==> __count_11_13 > 0); // Execution dependence
assert(__count_1_6 > 0 ==> __count_16_17 > 0); // Execution dependence
assert(__count_3_4 > 0 ==> __count_19 > 0); // Execution dependence
assert(__count_3_4 > 0 ==> __count_7_10 > 0); // Execution dependence
assert(__count_3_4 > 0 ==> __count_11_13 > 0); // Execution dependence
assert(__count_3_4 > 0 ==> __count_16_17 > 0); // Execution dependence
assert(__count_5_6 > 0 ==> __count_19 > 0); // Execution dependence
assert(__count_5_6 > 0 ==> __count_7_10 > 0); // Execution dependence
assert(__count_5_6 > 0 ==> __count_11_13 > 0); // Execution dependence
assert(__count_5_6 > 0 ==> __count_16_17 > 0); // Execution dependence
#endif

    return cword;
  }
  else 
  {
    #ifdef CBMC
    __count_16_18++;
    #endif
    // 18
    #ifdef CBMC
    __count_19++;
    #endif


#ifdef CBMC
assert(__count_11_12 == 0); // Dead code
assert(__count_11_13 >= 40); // Lower capacity constraint
assert(__count_11_13 <= 42); // Upper capacity constraint
assert(__count_16_17 >= 1); // Lower capacity constraint
assert(__count_16_17 <= 1); // Upper capacity constraint
assert(__count_16_18 == 0); // Dead code
assert(__count_19 >= 1); // Lower capacity constraint
assert(__count_19 <= 1); // Upper capacity constraint
assert(__count_1_6 <= 1); // Upper capacity constraint
assert(__count_3_4 <= 256); // Upper capacity constraint
assert(__count_5_6 <= 1); // Upper capacity constraint
assert(__count_7_10 >= 1); // Lower capacity constraint
assert(__count_7_10 <= 1); // Upper capacity constraint
assert(__count_8_9 == 0); // Dead code
assert(__count_8_10 == 0); // Dead code
assert(__count_3_4 > 0 ==> __count_5_6 > 0); // Mutual inclusion
assert(__count_5_6 > 0 ==> __count_3_4 > 0); // Mutual inclusion
assert(__count_1_6 > 0 ==> __count_19 > 0); // Execution dependence
assert(__count_1_6 > 0 ==> __count_7_10 > 0); // Execution dependence
assert(__count_1_6 > 0 ==> __count_11_13 > 0); // Execution dependence
assert(__count_1_6 > 0 ==> __count_16_17 > 0); // Execution dependence
assert(__count_3_4 > 0 ==> __count_19 > 0); // Execution dependence
assert(__count_3_4 > 0 ==> __count_7_10 > 0); // Execution dependence
assert(__count_3_4 > 0 ==> __count_11_13 > 0); // Execution dependence
assert(__count_3_4 > 0 ==> __count_16_17 > 0); // Execution dependence
assert(__count_5_6 > 0 ==> __count_19 > 0); // Execution dependence
assert(__count_5_6 > 0 ==> __count_7_10 > 0); // Execution dependence
assert(__count_5_6 > 0 ==> __count_11_13 > 0); // Execution dependence
assert(__count_5_6 > 0 ==> __count_16_17 > 0); // Execution dependence
#endif


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


