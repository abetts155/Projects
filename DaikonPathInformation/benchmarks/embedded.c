typedef struct IMMENSE { unsigned long l, r; } immense;
typedef struct GREAT { unsigned long l, c, r; } great;

unsigned long bit[33];

static immense icd;
static char ipc1[57]={0,57,49,41,33,25,17,9,1,58,50,
   42,34,26,18,10,2,59,51,43,35,27,19,11,3,60,
   52,44,36,63,55,47,39,31,23,15,7,62,54,46,38,
   30,22,14,6,61,53,45,37,29,21,13,5,28,20,12,4};
static char ipc2[49]={0,14,17,11,24,1,5,3,28,15,6,21,
   10,23,19,12,4,26,8,16,7,27,20,13,2,41,52,31,
   37,47,55,30,40,51,45,33,48,44,49,39,56,34,
   53,46,42,50,36,29,32};

unsigned long 
getbit (immense source, int bitno, int nbits) {
#ifdef CBMC
//==========> getbit : header 16
int __count_24 = 0;
int __count_17_18 = 0;
int __count_17_19 = 0;
int __count_21_22 = 0;
int __count_21_23 = 0;
#endif

  if (bitno <= nbits) // 16
  {
    // 17
    #ifdef CBMC
    // TODO: check 
    if(bit[bitno] & source.r)
    {
      __count_17_18++;
      __count_24++;
assert(__count_21_22 <= 1); // Upper capacity constraint
assert(__count_21_23 <= 1); // Upper capacity constraint
assert(__count_17_18 <= 1); // Upper capacity constraint
assert(__count_17_19 <= 1); // Upper capacity constraint
assert(__count_24 >= 1); // Lower capacity constraint
assert(__count_24 <= 1); // Upper capacity constraint
assert(__count_17_18 > 0 ==> __count_24 > 0); // Execution dependence
assert(__count_17_19 > 0 ==> __count_24 > 0); // Execution dependence
assert(__count_21_22 > 0 ==> __count_24 > 0); // Execution dependence
assert(__count_21_23 > 0 ==> __count_24 > 0); // Execution dependence
      return 1L;
    }
    else
    {
      __count_17_19++;
      __count_24++;
assert(__count_21_22 <= 1); // Upper capacity constraint
assert(__count_21_23 <= 1); // Upper capacity constraint
assert(__count_17_18 <= 1); // Upper capacity constraint
assert(__count_17_19 <= 1); // Upper capacity constraint
assert(__count_24 >= 1); // Lower capacity constraint
assert(__count_24 <= 1); // Upper capacity constraint
assert(__count_17_18 > 0 ==> __count_24 > 0); // Execution dependence
assert(__count_17_19 > 0 ==> __count_24 > 0); // Execution dependence
assert(__count_21_22 > 0 ==> __count_24 > 0); // Execution dependence
assert(__count_21_23 > 0 ==> __count_24 > 0); // Execution dependence
      return 0L;
    }
    #else
    return bit[bitno] & source.r ? 1L : 0L;
    #endif
  }
  else
  {
    // 21
    #ifdef CBMC
    // TODO: check
    if (bit[bitno-nbits] & source.l)
    {
      __count_21_22++;
      __count_24++;
assert(__count_21_22 <= 1); // Upper capacity constraint
assert(__count_21_23 <= 1); // Upper capacity constraint
assert(__count_17_18 <= 1); // Upper capacity constraint
assert(__count_17_19 <= 1); // Upper capacity constraint
assert(__count_24 >= 1); // Lower capacity constraint
assert(__count_24 <= 1); // Upper capacity constraint
assert(__count_17_18 > 0 ==> __count_24 > 0); // Execution dependence
assert(__count_17_19 > 0 ==> __count_24 > 0); // Execution dependence
assert(__count_21_22 > 0 ==> __count_24 > 0); // Execution dependence
assert(__count_21_23 > 0 ==> __count_24 > 0); // Execution dependence
      return 1L;
    }
    else
    {
      __count_21_23++;
      __count_24++;
assert(__count_21_22 <= 1); // Upper capacity constraint
assert(__count_21_23 <= 1); // Upper capacity constraint
assert(__count_17_18 <= 1); // Upper capacity constraint
assert(__count_17_19 <= 1); // Upper capacity constraint
assert(__count_24 >= 1); // Lower capacity constraint
assert(__count_24 <= 1); // Upper capacity constraint
assert(__count_17_18 > 0 ==> __count_24 > 0); // Execution dependence
assert(__count_17_19 > 0 ==> __count_24 > 0); // Execution dependence
assert(__count_21_22 > 0 ==> __count_24 > 0); // Execution dependence
assert(__count_21_23 > 0 ==> __count_24 > 0); // Execution dependence
      return 0L;
    }
    #else
    return bit[bitno-nbits] & source.l ? 1L : 0L;
    #endif
  }
}
   
void 
ks (int n, great * kn) 
{
#ifdef CBMC
//==========> ks : header 8
int __count_8_7 = 0;
int __count_8_7_L = 0; //Loop counter
//==========> ks : header 14
int __count_10_11 = 0;
int __count_14_10 = 0; //Loop counter
//==========> ks : header 1
int __count_15 = 0;
int __count_1_5 = 0;
int __count_2_5 = 0;
int __count_3_5 = 0;
int __count_4_5 = 0;
int __count_8_9 = 0;
#endif

  int i,j,k,l;

  if (n == 1 || n == 2 || n == 9 || n == 16) 
  {
    #ifdef CBMC
    // TODO: check
    if      (n == 1)  __count_1_5++;
    else if (n == 2)  __count_2_5++;
    else if (n == 9)  __count_3_5++;
    else if (n == 16) __count_4_5++;
    #endif
    icd.r = (icd.r | ((icd.r & 1L) << 28)) >> 1;
    icd.l = (icd.l | ((icd.l & 1L) << 28)) >> 1;
  }
  else
  {
    #ifdef CBMC
    __count_8_7_L = 0;
    #endif
    for (i=1;i<=2;i++) // 8
    {
      #ifdef CBMC
      __count_8_7_L++;
      __count_8_7++;
      #endif
      icd.r = (icd.r | ((icd.r & 1L) << 28)) >> 1;
      icd.l = (icd.l | ((icd.l & 1L) << 28)) >> 1;
    }
    #ifdef CBMC
    assert(__count_8_7_L  <= 3); // Loop counter property
    __count_8_9++;
    #endif
  } 
  
  (*kn).r=(*kn).c=(*kn).l=0;
  #ifdef CBMC
  __count_14_10 = 0;
  #endif
  for (j=16,k=32,l=48; j>=1; j--,k--,l--) // 14
  {
    #ifdef CBMC
    __count_14_10++;
    #endif
    (*kn).r=((*kn).r <<= 1) | (unsigned short) getbit(icd,ipc2[j],28);
    (*kn).c=((*kn).c <<= 1) | (unsigned short) getbit(icd,ipc2[k],28);
    (*kn).l=((*kn).l <<= 1) | (unsigned short) getbit(icd,ipc2[l],28);
    #ifdef CBMC
    __count_10_11++;
    #endif
  }
  #ifdef CBMC
  assert(__count_14_10  <= 17); // Loop counter property
  __count_15++;
  #endif


#ifdef CBMC
assert(__count_15 >= 1); // Lower capacity constraint
assert(__count_15 <= 1); // Upper capacity constraint
assert(__count_1_5 <= 1); // Upper capacity constraint
assert(__count_2_5 <= 1); // Upper capacity constraint
assert(__count_3_5 <= 1); // Upper capacity constraint
assert(__count_4_5 <= 1); // Upper capacity constraint
assert(__count_8_9 <= 1); // Upper capacity constraint
assert(__count_8_7 <= 2); // Upper capacity constraint
assert(__count_10_11 >= 16); // Lower capacity constraint
assert(__count_10_11 <= 16); // Upper capacity constraint
assert(__count_8_9 > 0 ==> __count_8_7 > 0); // Mutual inclusion
assert(__count_8_7 > 0 ==> __count_8_9 > 0); // Mutual inclusion
assert(__count_1_5 > 0 ==> __count_15 > 0); // Execution dependence
assert(__count_1_5 > 0 ==> __count_10_11 > 0); // Execution dependence
assert(__count_2_5 > 0 ==> __count_15 > 0); // Execution dependence
assert(__count_2_5 > 0 ==> __count_10_11 > 0); // Execution dependence
assert(__count_3_5 > 0 ==> __count_15 > 0); // Execution dependence
assert(__count_3_5 > 0 ==> __count_10_11 > 0); // Execution dependence
assert(__count_4_5 > 0 ==> __count_15 > 0); // Execution dependence
assert(__count_4_5 > 0 ==> __count_10_11 > 0); // Execution dependence
assert(__count_8_9 > 0 ==> __count_15 > 0); // Execution dependence
assert(__count_8_9 > 0 ==> __count_10_11 > 0); // Execution dependence
assert(__count_8_7 > 0 ==> __count_15 > 0); // Execution dependence
assert(__count_8_7 > 0 ==> __count_10_11 > 0); // Execution dependence
#endif
}

void 
cyfun (unsigned long ir, great k, unsigned long * iout) 
{
#ifdef CBMC
//==========> cyfun : header 48
int __count_44_45 = 0;
int __count_44_46 = 0;
int __count_48_44 = 0; //Loop counter
//==========> cyfun : header 42
int __count_42_41 = 0;
int __count_42_41_L = 0; //Loop counter
//==========> cyfun : header 39
int __count_39_38 = 0;
int __count_39_38_L = 0; //Loop counter
//==========> cyfun : header 36
int __count_27_29 = 0;
int __count_28_29 = 0;
int __count_29_30 = 0;
int __count_29_31 = 0;
int __count_32_33 = 0;
int __count_32_34 = 0;
int __count_36_26 = 0; //Loop counter
//==========> cyfun : header 25
int __count_49 = 0;
int __count_36_37 = 0;
#endif

  static char iet[49]={0,32,1,2,3,4,5,4,5,6,7,8,9,8,9,
      10,11,12,13,12,13,14,15,16,17,16,17,18,19,
      20,21,20,21,22,23,24,25,24,25,26,27,28,29,
      28,29,30,31,32,1};
  static char ipp[33]={0,16,7,20,21,29,12,28,17,1,15,
      23,26,5,18,31,10,2,8,24,14,32,27,3,9,19,13,
      30,6,22,11,4,25};
  static char is[16][4][9]={
      0,14,15,10,7,2,12,4,13,0,0,3,13,13,14,10,13,1,
      0,4,0,13,10,4,9,1,7,0,15,13,1,3,11,4,6,2,
      0,4,1,0,13,12,1,11,2,0,15,13,7,8,11,15,0,15,
      0,1,14,6,6,2,14,4,11,0,12,8,10,15,8,3,11,1,
      0,13,8,9,14,4,10,2,8,0,7,4,0,11,2,4,11,13,
      0,14,7,4,9,1,15,11,4,0,8,10,13,0,12,2,13,14,
      0,1,14,14,3,1,15,14,4,0,4,7,9,5,12,2,7,8,
      0,8,11,9,0,11,5,13,1,0,2,1,0,6,7,12,8,7,
      0,2,6,6,0,7,9,15,6,0,14,15,3,6,4,7,4,10,
      0,13,10,8,12,10,2,12,9,0,4,3,6,10,1,9,1,4,
      0,15,11,3,6,10,2,0,15,0,2,2,4,15,7,12,9,3,
      0,6,4,15,11,13,8,3,12,0,9,15,9,1,14,5,4,10,
      0,11,3,15,9,11,6,8,11,0,13,8,6,0,13,9,1,7,
      0,2,13,3,7,7,12,7,14,0,1,4,8,13,2,15,10,8,
      0,8,4,5,10,6,8,13,1,0,1,14,10,3,1,5,10,4,
      0,11,1,0,13,8,3,14,2,0,7,2,7,8,13,10,7,13,
      0,3,9,1,1,8,0,3,10,0,10,12,2,4,5,6,14,12,
      0,15,5,11,15,15,7,10,0,0,5,11,4,9,6,11,9,15,
      0,10,7,13,2,5,13,12,9,0,6,0,8,7,0,1,3,5,
      0,12,8,1,1,9,0,15,6,0,11,6,15,4,15,14,5,12,
      0,6,2,12,8,3,3,9,3,0,12,1,5,2,15,13,5,6,
      0,9,12,2,3,12,4,6,10,0,3,7,14,5,0,1,0,9,
      0,12,13,7,5,15,4,7,14,0,11,10,14,12,10,14,12,11,
      0,7,6,12,14,5,10,8,13,0,14,12,3,11,9,7,15,0,
      0,5,12,11,11,13,14,5,5,0,9,6,12,1,3,0,2,0,
      0,3,9,5,5,6,1,0,15,0,10,0,11,12,10,6,14,3,
      0,9,0,4,12,0,7,10,0,0,5,9,11,10,9,11,15,14,
      0,10,3,10,2,3,13,5,3,0,0,5,5,7,4,0,2,5,
      0,0,5,2,4,14,5,6,12,0,3,11,15,14,8,3,8,9,
      0,5,2,14,8,0,11,9,5,0,6,14,2,2,5,8,3,6,
      0,7,10,8,15,9,11,1,7,0,8,5,1,9,6,8,6,2,
      0,0,15,7,4,14,6,2,8,0,13,9,12,14,3,13,12,11};
  static char ibin[16]={0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15};
  great ie;
  unsigned long itmp,ietmp1,ietmp2;
  char iec[9];
  int jj,irow,icol,iss,j,l,m;
  unsigned long *p;

  p = bit;
  ie.r=ie.c=ie.l=0;
  #ifdef CBMC
  __count_36_26 = 0;
  #endif
  for (j=16,l=32,m=48;j>=1;j--,l--,m--) // 36
  {
    #ifdef CBMC
    __count_36_26++;
    #endif

    #ifdef CBMC
    // TODO: check
    if (p[iet[j]] & ir) // 26
    {
      ie.r = (ie.r <<=1) | 1; // 27
      __count_27_29++;
    }
    else
    {
      ie.r = (ie.r <<=1) | 0; // 28
      __count_28_29++;
    }
    #else
    ie.r = (ie.r <<=1) | (p[iet[j]] & ir ? 1 : 0);
    #endif

    #ifdef CBMC
    if (p[iet[l]] & ir) // 29
    {
      __count_29_30++;
      ie.c = (ie.c <<=1) | 1; //30
    }
    else
    {
      __count_29_31++;
      ie.c = (ie.c <<=1) | 0; // 31
    }
    #else
    ie.c = (ie.c <<=1) | (p[iet[l]] & ir ? 1 : 0);
    #endif

    #ifdef CBMC
    if (p[iet[m]] & ir) // 32
    {
      ie.l = (ie.l <<=1) | 1; // 33
      __count_32_33++;
    }
    else
    {
      ie.l = (ie.l <<=1) | 0; // 34
      __count_32_34++;
    }
    #else
    ie.l = (ie.l <<=1) | (p[iet[m]] & ir ? 1 : 0);
    #endif
  }
  #ifdef CBMC
  assert(__count_36_26  <= 17); // Loop counter property
  __count_36_37++;
  #endif
  
  ie.r ^= k.r;
  ie.c ^= k.c;
  ie.l ^= k.l;
  ietmp1=((unsigned long) ie.c << 16)+(unsigned long) ie.r;
  ietmp2=((unsigned long) ie.l << 8)+((unsigned long) ie.c >> 8);
  
  #ifdef CBMC
  __count_39_38_L = 0;
  #endif
  for (j=1,m=5;j<=4;j++,m++) // 39
  {
    #ifdef CBMC
    __count_39_38_L++;
    __count_39_38++;
    #endif
    iec[j]=ietmp1 & 0x3fL;
    iec[m]=ietmp2 & 0x3fL;
    ietmp1 >>= 6;
    ietmp2 >>= 6;
  }
  #ifdef CBMC
  assert(__count_39_38_L  <= 5); // Loop counter property
  #endif
  
  itmp=0L;
  #ifdef CBMC
  __count_42_41_L = 0;
  #endif
  for (jj=8;jj>=1;jj--) // 42
  {
    #ifdef CBMC
    __count_42_41_L++;
    __count_42_41++;
    #endif
    j =iec[jj];
    irow=((j & 0x1) << 1)+((j & 0x20) >> 5);
    icol=((j & 0x2) << 2)+(j & 0x4) +((j & 0x8) >> 2)+((j & 0x10) >> 4);
    iss=is[icol][irow][jj];
    itmp = (itmp <<= 4) | ibin[iss];
  }
  #ifdef CBMC
  assert(__count_42_41_L  <= 9); // Loop counter property
  #endif
  
  *iout=0L;
  p = bit;
  #ifdef CBMC
  __count_48_44 = 0;
  #endif
  for (j=32; j>=1;j--) // 48
  {
    #ifdef CBMC
    __count_48_44++;
    #endif
    #ifdef CBMC
    // TODO: check
    if(p[ipp[j]] & itmp) // 44
    {
      __count_44_45++;
      *iout = (*iout <<= 1) | 1; // 45
    }
    else
    {
      __count_44_46++;
      *iout = (*iout <<= 1) | 0; // 46
    }
    #else
    *iout = (*iout <<= 1) | (p[ipp[j]] & itmp ? 1 : 0);
    #endif
  }
  #ifdef CBMC
  assert(__count_48_44  <= 33); // Loop counter property
  __count_49++;
  #endif

#ifdef CBMC
assert(__count_27_29 <= 16); // Upper capacity constraint
assert(__count_28_29 <= 16); // Upper capacity constraint
assert(__count_29_30 <= 16); // Upper capacity constraint
assert(__count_29_31 <= 16); // Upper capacity constraint
//assert(__count_32_33 <= 15); // Upper capacity constraint
assert(__count_32_34 >= 1); // Lower capacity constraint
assert(__count_32_34 <= 16); // Upper capacity constraint
assert(__count_36_37 >= 1); // Lower capacity constraint
assert(__count_36_37 <= 1); // Upper capacity constraint
assert(__count_49 >= 1); // Lower capacity constraint
assert(__count_49 <= 1); // Upper capacity constraint
//assert(__count_44_45 >= 8); // Lower capacity constraint
//assert(__count_44_45 <= 23); // Upper capacity constraint
//assert(__count_44_46 >= 9); // Lower capacity constraint
//assert(__count_44_46 <= 24); // Upper capacity constraint
assert(__count_42_41 >= 8); // Lower capacity constraint
assert(__count_42_41 <= 8); // Upper capacity constraint
assert(__count_39_38 >= 4); // Lower capacity constraint
assert(__count_39_38 <= 4); // Upper capacity constraint
assert(__count_27_29 > 0 ==> __count_32_34 > 0); // Execution dependence
assert(__count_27_29 > 0 ==> __count_36_37 > 0); // Execution dependence
assert(__count_27_29 > 0 ==> __count_39_38 > 0); // Execution dependence
assert(__count_27_29 > 0 ==> __count_42_41 > 0); // Execution dependence
assert(__count_27_29 > 0 ==> __count_49 > 0); // Execution dependence
assert(__count_27_29 > 0 ==> __count_44_45 > 0); // Execution dependence
assert(__count_27_29 > 0 ==> __count_44_46 > 0); // Execution dependence
assert(__count_28_29 > 0 ==> __count_32_34 > 0); // Execution dependence
assert(__count_28_29 > 0 ==> __count_36_37 > 0); // Execution dependence
assert(__count_28_29 > 0 ==> __count_39_38 > 0); // Execution dependence
assert(__count_28_29 > 0 ==> __count_42_41 > 0); // Execution dependence
assert(__count_28_29 > 0 ==> __count_49 > 0); // Execution dependence
assert(__count_28_29 > 0 ==> __count_44_45 > 0); // Execution dependence
assert(__count_28_29 > 0 ==> __count_44_46 > 0); // Execution dependence
assert(__count_29_30 > 0 ==> __count_32_34 > 0); // Execution dependence
assert(__count_29_30 > 0 ==> __count_36_37 > 0); // Execution dependence
assert(__count_29_30 > 0 ==> __count_39_38 > 0); // Execution dependence
assert(__count_29_30 > 0 ==> __count_42_41 > 0); // Execution dependence
assert(__count_29_30 > 0 ==> __count_49 > 0); // Execution dependence
assert(__count_29_30 > 0 ==> __count_44_45 > 0); // Execution dependence
assert(__count_29_30 > 0 ==> __count_44_46 > 0); // Execution dependence
assert(__count_29_31 > 0 ==> __count_32_34 > 0); // Execution dependence
assert(__count_29_31 > 0 ==> __count_36_37 > 0); // Execution dependence
assert(__count_29_31 > 0 ==> __count_39_38 > 0); // Execution dependence
assert(__count_29_31 > 0 ==> __count_42_41 > 0); // Execution dependence
assert(__count_29_31 > 0 ==> __count_49 > 0); // Execution dependence
assert(__count_29_31 > 0 ==> __count_44_45 > 0); // Execution dependence
assert(__count_29_31 > 0 ==> __count_44_46 > 0); // Execution dependence
assert(__count_32_33 > 0 ==> __count_32_34 > 0); // Execution dependence
assert(__count_32_33 > 0 ==> __count_36_37 > 0); // Execution dependence
assert(__count_32_33 > 0 ==> __count_39_38 > 0); // Execution dependence
assert(__count_32_33 > 0 ==> __count_42_41 > 0); // Execution dependence
assert(__count_32_33 > 0 ==> __count_49 > 0); // Execution dependence
assert(__count_32_33 > 0 ==> __count_44_45 > 0); // Execution dependence
assert(__count_32_33 > 0 ==> __count_44_46 > 0); // Execution dependence
#endif

}
  
int 
embedded (immense inp, immense key, int * newkey, int isw, immense * out) 
{
#ifdef CBMC
//==========> embedded : header 63
int __count_61_62 = 0;
int __count_63_61 = 0; //Loop counter
//==========> embedded : header 59
int __count_56_57 = 0;
int __count_59_56 = 0; //Loop counter
//==========> embedded : header 80
int __count_80_77 = 0;
int __count_80_77_L = 0; //Loop counter
//==========> embedded : header 75
int __count_70_71 = 0;
int __count_70_72 = 0;
int __count_75_70 = 0; //Loop counter
//==========> embedded : header 53
int __count_53_52 = 0;
int __count_53_52_L = 0; //Loop counter
//==========> embedded : header 68
int __count_68_65 = 0;
int __count_68_65_L = 0; //Loop counter
//==========> embedded : header 50
int __count_81 = 0;
int __count_50_54 = 0;
int __count_53_54 = 0;
int __count_54_64 = 0;
int __count_59_60 = 0;
#endif
  static char ip[65] =
      {0,58,50,42,34,26,18,10,2,60,52,44,36,
      28,20,12,4,62,54,46,38,30,22,14,6,64,56,48,40,
      32,24,16,8,57,49,41,33,25,17,9,1,59,51,43,35,
      27,19,11,3,61,53,45,37,29,21,13,5,63,55,47,39,
      31,23,15,7};
  static char ipm[65]=
      {0,40,8,48,16,56,24,64,32,39,7,47,15,
      55,23,63,31,38,6,46,14,54,22,62,30,37,5,45,13,
      53,21,61,29,36,4,44,12,52,20,60,28,35,3,43,11,
      51,19,59,27,34,2,42,10,50,18,58,26,33,1,41,9,
      49,17,57,25};
  static great kns[17];
  static int initflag=1;
  int ii,i,j,k;
  unsigned long ic,shifter,getbit();
  immense itmp;
  great pg;

  if (initflag) // 50
  {
    initflag=0;
    bit[1]=shifter=1L;
    #ifdef CBMC
    __count_53_52_L = 0;
    #endif
    for(j=2;j<=32;j++) // 53
    {
      #ifdef CBMC
      __count_53_52_L++;
      __count_53_52++;
      #endif
      bit[j] = (shifter <<= 1);
    }
    #ifdef CBMC
    assert(__count_53_52_L  <= 32); // Loop counter property
    __count_53_54++;
    #endif
  }
  #ifdef CBMC
  else __count_50_54++;
  #endif
  
  if (*newkey) // 54
  {
    *newkey=0;
    icd.r=icd.l=0L;
    #ifdef CBMC
    __count_59_56 = 0;
    #endif
    for (j=28,k=56;j>=1;j--,k--) // 59
    {
      #ifdef CBMC
      __count_59_56++;
      __count_56_57++;
      #endif
      icd.r = (icd.r <<= 1) | getbit(key,ipc1[j],32);
      icd.l = (icd.l <<= 1) | getbit(key,ipc1[k],32);
    }
    #ifdef CBMC
    assert(__count_59_56  <= 29); // Loop counter property
    __count_59_60++;
    #endif

    #ifdef CBMC
    __count_63_61 = 0;
    #endif
    for (i=1; i<=16;i++) // 63
    { 
      #ifdef CBMC
      __count_63_61++;
      __count_61_62++;
      #endif
      //62
      pg = kns[i]; ks(/* key,*/ i, &pg); kns[i] = pg;
    }
    #ifdef CBMC
    assert(__count_63_61  <= 17); // Loop counter property
    #endif
  }
  #ifdef CBMC
  __count_54_64++;
  #endif
  
  itmp.r=itmp.l=0L;
  
  #ifdef CBMC
  __count_68_65_L = 0;
  #endif
  for (j=32,k=64;j>=1;j--,k--) // 68
  {
    #ifdef CBMC
    __count_68_65_L++;
    __count_68_65++;
    #endif
    itmp.r = (itmp.r <<= 1) | getbit(inp,ip[j],32);
    itmp.l = (itmp.l <<= 1) | getbit(inp,ip[k],32);
  }
  #ifdef CBMC
  assert(__count_68_65_L  <= 33); // Loop counter property
  #endif
  
  #ifdef CBMC
  __count_75_70 = 0;
  #endif
  for (i=1;i<=16;i++) // 75
  {
    #ifdef CBMC
    __count_75_70++;
    // TODO: check 
    if(isw == 1)
    {
      ii = 17-i; // 71
      __count_70_71++;
    }
    else
    {
      ii = i; // 72
      __count_70_72++;
    }
    #else
    ii = (isw == 1 ? 17-i : i);
    #endif
    cyfun(itmp.l, kns[ii], &ic);
    ic ^= itmp.r;
    itmp.r=itmp.l;
    itmp.l=ic;
  }
  #ifdef CBMC
  assert(__count_75_70  <= 17); // Loop counter property
  #endif
  
  ic=itmp.r;
  itmp.r=itmp.l;
  itmp.l=ic;
  (*out).r=(*out).l=0L;
  
  #ifdef CBMC
  __count_80_77_L = 0;
  #endif
  for (j=32,k=64; j >= 1; j--, k--) // 80
  {
    #ifdef CBMC
    __count_80_77_L++;
    __count_80_77++;
    #endif
    (*out).r = ((*out).r <<= 1) | getbit(itmp,ipm[j],32);
    (*out).l = ((*out).l <<= 1) | getbit(itmp,ipm[k],32);
  }
  #ifdef CBMC
  assert(__count_80_77_L  <= 33); // Loop counter property
  #endif
  
  #ifdef CBMC
  __count_81++;
  #endif

#ifdef CBMC
assert(__count_50_54 == 0); // Dead code
assert(__count_53_52 >= 31); // Lower capacity constraint
assert(__count_53_52 <= 31); // Upper capacity constraint
assert(__count_53_54 >= 1); // Lower capacity constraint
assert(__count_53_54 <= 1); // Upper capacity constraint
//assert(__count_54_64 == 0); // Dead code
assert(__count_80_77 >= 32); // Lower capacity constraint
assert(__count_80_77 <= 32); // Upper capacity constraint
assert(__count_56_57 >= 28); // Lower capacity constraint
assert(__count_56_57 <= 28); // Upper capacity constraint
assert(__count_59_60 >= 1); // Lower capacity constraint
assert(__count_59_60 <= 1); // Upper capacity constraint
assert(__count_61_62 >= 16); // Lower capacity constraint
assert(__count_61_62 <= 16); // Upper capacity constraint
assert(__count_81 >= 1); // Lower capacity constraint
assert(__count_81 <= 1); // Upper capacity constraint
assert(__count_68_65 >= 32); // Lower capacity constraint
assert(__count_68_65 <= 32); // Upper capacity constraint
//assert(__count_70_72 >= 16); // Lower capacity constraint
assert(__count_70_72 <= 16); // Upper capacity constraint
//assert(__count_70_71 == 0); // Dead code
#endif

  return *newkey;
}

int 
main (int argc, char *argv[])
{  
  /*
   * Six values must be supplied: unsigned long, unsigned long, unsigned long, unsigned long, int, int
   */
  if (argc != 7)
  {
    return 1;
  }
  
  int newkey, isw;
  immense inp, key, out;
  
  inp.l  = atol(argv[1]);
  inp.r  = atol(argv[2]);
  key.l  = atol(argv[3]);
  key.r  = atol(argv[4]);
  newkey = atoi(argv[5]);
  isw    = atoi(argv[6]);  
  // Adam Betts: 'newkey' needs to be set to 1 to trigger a conditional in 'embedded'
  // Similarly for 'isw' 

  int val = embedded(inp, key, &newkey, isw, &out);
  printf("%d", val);
  
  return 0;
}

