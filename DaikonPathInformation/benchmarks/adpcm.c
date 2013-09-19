#define PI 3141
#define TEST_VECTOR_LENGTH 4

int xl,xh;
int xout1,xout2;
int xs,xd;
int il,szl,spl,sl,el;
int nbl;                
int al1,al2;
int plt,plt1,plt2;
int rs;
int dlt;
int detl;
int rlt,rlt1,rlt2;
int deth;
int sh;
int eh;
int dh,ih;
int nbh,szh;
int sph,ph,yh,rh;
int ah1,ah2;
int ph1,ph2;
int rh1,rh2;
int ilr,yl,rl;
int dec_deth,dec_detl,dec_dlt;
int dec_plt,dec_plt1,dec_plt2;
int dec_szl,dec_spl,dec_sl;
int dec_rlt1,dec_rlt2,dec_rlt;
int dec_al1,dec_al2;
int dl;
int dec_nbl,dec_yh,dec_dh,dec_nbh;
int dec_szh;
int dec_rh1,dec_rh2;
int dec_ah1,dec_ah2;
int dec_ph,dec_sph;
int dec_sh,dec_rh;
int dec_ph1,dec_ph2;

int tqmf[24];
int accumc[11];
int accumd[11];
int delay_bpl[6];
int delay_dltx[6];
int dec_del_bpl[6];
int dec_del_dltx[6];
int delay_dhx[6];
int delay_bph[6];
int dec_del_bph[6];
int dec_del_dhx[6];

int h[24] = {
    12,   -44,   -44,   212,    48,  -624,   128,  1448,
  -840, -3220,  3804, 15504, 15504,  3804, -3220,  -840,
  1448,   128,  -624,    48,   212,   -44,   -44,    12
};

int qq4_code4_table[16] = {
     0,  -20456,  -12896,   -8968,   -6288,   -4240,   -2584,   -1200,
 20456,   12896,    8968,    6288,    4240,    2584,    1200,       0
};

int qq5_code5_table[32] = {
  -280,    -280,  -23352,  -17560,  -14120,  -11664,   -9752,   -8184,
 -6864,   -5712,   -4696,   -3784,   -2960,   -2208,   -1520,    -880,
 23352,   17560,   14120,   11664,    9752,    8184,    6864,    5712,
  4696,    3784,    2960,    2208,    1520,     880,     280,    -280
};

int qq6_code6_table[64] = {
  -136,    -136,    -136,    -136,  -24808,  -21904,  -19008,  -16704,
-14984,  -13512,  -12280,  -11192,  -10232,   -9360,   -8576,   -7856,
 -7192,   -6576,   -6000,   -5456,   -4944,   -4464,   -4008,   -3576,
 -3168,   -2776,   -2400,   -2032,   -1688,   -1360,   -1040,    -728,
 24808,   21904,   19008,   16704,   14984,   13512,   12280,   11192,
 10232,    9360,    8576,    7856,    7192,    6576,    6000,    5456,
  4944,    4464,    4008,    3576,    3168,    2776,    2400,    2032,
  1688,    1360,    1040,     728,     432,     136,    -432,    -136
};

int wl_code_table[16] = {
   -60,  3042,  1198,   538,   334,   172,    58,   -30,
  3042,  1198,   538,   334,   172,    58,   -30,   -60
};

int wl_table[8] = {
   -60,   -30,    58,   172,   334,   538,  1198,  3042
};

int ilb_table[32] = {
  2048,  2093,  2139,  2186,  2233,  2282,  2332,  2383,
  2435,  2489,  2543,  2599,  2656,  2714,  2774,  2834,
  2896,  2960,  3025,  3091,  3158,  3228,  3298,  3371,
  3444,  3520,  3597,  3676,  3756,  3838,  3922,  4008
};

int decis_levl[30] = {
   280,   576,   880,  1200,  1520,  1864,  2208,  2584,
  2960,  3376,  3784,  4240,  4696,  5200,  5712,  6288,
  6864,  7520,  8184,  8968,  9752, 10712, 11664, 12896,
 14120, 15840, 17560, 20456, 23352, 32767
};

int quant26bt_pos[31] = {
    61,    60,    59,    58,    57,    56,    55,    54,
    53,    52,    51,    50,    49,    48,    47,    46,
    45,    44,    43,    42,    41,    40,    39,    38,
    37,    36,    35,    34,    33,    32,    32
};

int quant26bt_neg[31] = {
    63,    62,    31,    30,    29,    28,    27,    26,
    25,    24,    23,    22,    21,    20,    19,    18,
    17,    16,    15,    14,    13,    12,    11,    10,
     9,     8,     7,     6,     5,     4,     4
};

int qq2_code2_table[4] = {
  -7408,   -1616,   7408,  1616
};

int wh_code_table[4] = {
   798,   -214,    798,   -214
};

typedef struct 
{
  int real;
  int imag;
} COMPLEX;

int 
my_abs (int n)
{
  #ifdef CBMC
  int __count_38_40 = 0;
  int __count_39_41 = 0;
  #endif
  if (n >= 0)  // 38
  {
    // 39
    #ifdef CBMC
    __count_39_41++;
    #endif
    return n;
  }
  else
  {
    #ifdef CBMC
    __count_38_40++;
    #endif
    // 40
    return-n; 
  }
}

int 
my_sin (int rad)
{
  int diff;
  int app=0;
  int inc = 1;

  while (rad > 2*PI)
  {
    rad -= 2*PI;
  }
  
  while (rad < -2*PI)
  {   
    rad += 2*PI;
  }
  
  diff = rad;
  app  = diff;
  diff = (diff * (-(rad*rad))) / ((2 * inc) * (2 * inc + 1));
  app  = app + diff;
  inc++;
  
  while (my_abs(diff) >= 1) 
  {
    diff = (diff * (-(rad*rad))) / ((2 * inc) * (2 * inc + 1));
    app = app + diff;
    inc++;
  }

  return app;
}

int 
my_cos (int rad)
{
  return (my_sin (PI / 2 - rad));
}

void 
upzero (int dlt,int *dlti,int *bli)
{
  #ifdef CBMC
  int __count_125_124 = 0;
  int __count_128_129 = 0;
  int __count_128_130 = 0;
  int __count_L132 = 0;
  int __count_L125 = 0;
  #endif

  int i,wd2,wd3;
  if(dlt == 0) // 122
  {
    for(i = 0 ; 
      #ifdef CBMC
      __count_L125++,
      #endif
    i < 6 ; i++) // 125
    {
      #ifdef CBMC
      __count_125_124++;
      #endif
      bli[i] = (int)((255L*bli[i]) >> 8L); 
    }
  }
  else 
  {
    for(i = 0 ; 
      #ifdef CBMC
      __count_L132++,
      #endif
    i < 6 ; i++) // 132
    {
      if ((long)dlt*dlti[i] >= 0) // 128
      { 
        #ifdef CBMC
        __count_128_129++;
        #endif
        // 129
        wd2 = 128;
      }
      else
      { 
        #ifdef CBMC
        __count_128_130++;
        #endif
        // 130
        wd2 = -128;
      }
      wd3 = (int)((255L*bli[i]) >> 8L);  
      bli[i] = wd2 + wd3;
     }
  }
  
  dlti[5] = dlti[4];
  dlti[4] = dlti[3];
  dlti[3] = dlti[2];
  dlti[1] = dlti[0];
  dlti[0] = dlt;
}


int 
encode (int xin1,int xin2)
{
  #ifdef CBMC
  int __count_66_67 = 0;
  int __count_70_69 = 0;
  int __count_82_84 = 0;
  int __count_83_84 = 0;
  int __count_85_87 = 0;
  int __count_86_87 = 0;
  int __count_L70 = 0;
  int __count_L67 = 0;
  #endif
  int i;
  int *h_ptr,*tqmf_ptr,*tqmf_ptr1;
  long int xa,xb;
  int decis;

  h_ptr = h;
  tqmf_ptr = tqmf;
  xa = (long)(*tqmf_ptr++) * (*h_ptr++);
  xb = (long)(*tqmf_ptr++) * (*h_ptr++);
  
  for(i = 0 ; 
    #ifdef CBMC
    __count_L67++,
    #endif
  i < 10 ; i++) // 67
  {
    xa += (long)(*tqmf_ptr++) * (*h_ptr++);
    xb += (long)(*tqmf_ptr++) * (*h_ptr++);
    #ifdef CBMC
    __count_66_67++;
    #endif
  }
  
  xa += (long)(*tqmf_ptr++) * (*h_ptr++);
  xb += (long)(*tqmf_ptr) * (*h_ptr++);
  tqmf_ptr1 = tqmf_ptr - 2;
  
  for(i = 0 ; 
    #ifdef CBMC
    __count_L70++,
    #endif
  i < 22 ; i++) // 70
  {
    #ifdef CBMC
    __count_70_69++;
    #endif
    *tqmf_ptr-- = *tqmf_ptr1--;
  }
  
  *tqmf_ptr-- = xin1;
  *tqmf_ptr = xin2;
  xl = (xa + xb) >> 15;
  xh = (xa - xb) >> 15;
  szl = filtez(delay_bpl,delay_dltx);
  spl = filtep(rlt1,al1,rlt2,al2);
  sl = szl + spl;
  el = xl - sl;
  il = quantl(el,detl);
  dlt = ((long)detl*qq4_code4_table[il >> 2]) >> 15;
  nbl = logscl(il,nbl);
  detl = scalel(nbl,8);
  plt = dlt + szl;
  upzero(dlt,delay_dltx,delay_bpl);
  
  al2 = uppol2(al1,al2,plt,plt1,plt2);
  al1 = uppol1(al1,al2,plt,plt1);
  rlt = sl + dlt;
  rlt2 = rlt1;
  rlt1 = rlt;
  plt2 = plt1;
  plt1 = plt;
  
  szh = filtez(delay_bph,delay_dhx);
  sph = filtep(rh1,ah1,rh2,ah2);
  sh = sph + szh;
  eh = xh - sh;

  if(eh >= 0) // 81
  {
    // 82
    ih = 3;
    #ifdef CBMC
    __count_82_84++;
    #endif
  }
  else
  {
    // 83
    ih = 1;
    #ifdef CBMC
    __count_83_84++;
    #endif
  }
  
  decis = (564L*(long)deth) >> 12L;
  
  if(my_abs(eh) > decis) // 85
  { 
    // 86
    ih--;
    #ifdef CBMC
    __count_86_87++;
    #endif
  }
  #ifdef CBMC
  else __count_85_87++;
  #endif
  
  dh = ((long)deth*qq2_code2_table[ih]) >> 15L ;
  nbh = logsch(ih,nbh);
  deth = scalel(nbh,10);

  ph = dh + szh;

  upzero(dh,delay_dhx,delay_bph);
    
  ah2 = uppol2(ah1,ah2,ph,ph1,ph2);
  ah1 = uppol1(ah1,ah2,ph,ph1);
  yh = sh + dh;
  rh2 = rh1;
  rh1 = yh;
  ph2 = ph1;
  ph1 = ph;

  return il | (ih << 6);
}

void 
decode (int input)
{
  #ifdef CBMC
  int __count_58_57 = 0;
  int __count_61_60 = 0;
  int __count_L58 = 0;
  int __count_L61 = 0;
  #endif
  int i;
  long int xa1,xa2;  
  int *h_ptr,*ac_ptr,*ac_ptr1,*ad_ptr,*ad_ptr1;

  ilr = input & 0x3f;
  ih = input >> 6;
  dec_szl = filtez(dec_del_bpl,dec_del_dltx);
  dec_spl = filtep(dec_rlt1,dec_al1,dec_rlt2,dec_al2);
  dec_sl = dec_spl + dec_szl;
  dec_dlt = ((long)dec_detl*qq4_code4_table[ilr >> 2]) >> 15;
  dl = ((long)dec_detl*qq6_code6_table[il]) >> 15;
  rl = dl + dec_sl;
  dec_nbl = logscl(ilr,dec_nbl);
  dec_detl = scalel(dec_nbl,8);
  dec_plt = dec_dlt + dec_szl;
  upzero(dec_dlt,dec_del_dltx,dec_del_bpl);
  dec_al2 = uppol2(dec_al1,dec_al2,dec_plt,dec_plt1,dec_plt2);
  dec_al1 = uppol1(dec_al1,dec_al2,dec_plt,dec_plt1);
  dec_rlt = dec_sl + dec_dlt;
  dec_rlt2 = dec_rlt1;
  dec_rlt1 = dec_rlt;
  dec_plt2 = dec_plt1;
  dec_plt1 = dec_plt;
  dec_szh = filtez(dec_del_bph,dec_del_dhx);
  dec_sph = filtep(dec_rh1,dec_ah1,dec_rh2,dec_ah2);
  dec_sh = dec_sph + dec_szh;
  dec_dh = ((long)dec_deth*qq2_code2_table[ih]) >> 15L ;
  dec_nbh = logsch(ih,dec_nbh);
  dec_deth = scalel(dec_nbh,10);
  dec_ph = dec_dh + dec_szh;
  upzero(dec_dh,dec_del_dhx,dec_del_bph);
  dec_ah2 = uppol2(dec_ah1,dec_ah2,dec_ph,dec_ph1,dec_ph2);
  dec_ah1 = uppol1(dec_ah1,dec_ah2,dec_ph,dec_ph1);
  rh = dec_sh + dec_dh;
  dec_rh2 = dec_rh1;
  dec_rh1 = rh;
  dec_ph2 = dec_ph1;
  dec_ph1 = dec_ph;
  xd = rl - rh;
  xs = rl + rh;

  h_ptr = h;
  ac_ptr = accumc;
  ad_ptr = accumd;
  xa1 = (long)xd * (*h_ptr++);
  xa2 = (long)xs * (*h_ptr++);

  for(i = 0 ; 
    #ifdef CBMC
    __count_L58++,
    #endif
  i < 10 ; i++) // 58
  {
    #ifdef CBMC
    __count_58_57++;
    #endif
    xa1 += (long)(*ac_ptr++) * (*h_ptr++);
    xa2 += (long)(*ad_ptr++) * (*h_ptr++);
  }

  xa1 += (long)(*ac_ptr) * (*h_ptr++);
  xa2 += (long)(*ad_ptr) * (*h_ptr++);

  xout1 = xa1 >> 14;
  xout2 = xa2 >> 14;

  ac_ptr1 = ac_ptr - 1;
  ad_ptr1 = ad_ptr - 1;
  
  for(i = 0 ; 
    #ifdef CBMC
    __count_L61++,
    #endif
  i < 10 ; i++) // 61
  {
    #ifdef CBMC
    __count_61_60++;
    #endif
    *ac_ptr-- = *ac_ptr1--;
    *ad_ptr-- = *ad_ptr1--;
  }
  
  *ac_ptr = xd;
  *ad_ptr = xs;
}

void 
reset ()
{
  #ifdef CBMC
  int __count_2_3 = 0;
  int __count_6_5 = 0;
  int __count_8_9 = 0;
  int __count_12_11 = 0;
  int __count_L3 = 0;
  int __count_L6 = 0;
  int __count_L9 = 0;
  int __count_L12 = 0;
  #endif

  int i;

  detl = dec_detl = 32; 
  deth = dec_deth = 8;
  nbl = al1 = al2 = plt1 = plt2 = rlt1 = rlt2 = 0;
  nbh = ah1 = ah2 = ph1 = ph2 = rh1 = rh2 = 0;
  dec_nbl = dec_al1 = dec_al2 = dec_plt1 = dec_plt2 = dec_rlt1 = dec_rlt2 = 0;
  dec_nbh = dec_ah1 = dec_ah2 = dec_ph1 = dec_ph2 = dec_rh1 = dec_rh2 = 0;

  for(i = 0;
    #ifdef CBMC
    __count_L3++,
    #endif
      i < 6; i++) // 3
  {
    delay_dltx[i] = 0;
    delay_dhx[i] = 0;
    dec_del_dltx[i] = 0;
    dec_del_dhx[i] = 0;
    #ifdef CBMC
    __count_2_3++;
    #endif
  }

  for(i = 0; 
    #ifdef CBMC
    __count_L6++,
    #endif
      i < 6 ; i++) // 6
  {
    #ifdef CBMC
    __count_6_5++;
    #endif
    delay_bpl[i] = 0;
    delay_bph[i] = 0;
    dec_del_bpl[i] = 0;
    dec_del_bph[i] = 0;
  }

  for(i = 0;
    #ifdef CBMC
    __count_L9++,
    #endif
      i < 23 ; i++) // 9
  {
    tqmf[i] = 0;
    #ifdef CBMC
    __count_8_9++;
    #endif
  }
  
  for(i = 0;
    #ifdef CBMC
    __count_L12++,
    #endif
      i < 11 ; i++) // 12
  {
    #ifdef CBMC
    __count_12_11++;
    #endif
    accumc[i] = 0;
    accumd[i] = 0;
  }
  
}

int 
filtez (int *bpl,int *dlt)
{
  #ifdef CBMC
  int __count_31_30 = 0;
  int __count_L31 = 0;
  #endif
  int i;
  long int zl;
  zl = (long)(*bpl++) * (*dlt++);
  for(i = 1 ; 
    #ifdef CBMC
    __count_L31++,
    #endif
  i < 6 ; i++) // 31
  {
    #ifdef CBMC
    __count_31_30++;
    #endif
    zl += (long)(*bpl++) * (*dlt++);
  }
  return (int)(zl >> 14);
}

int 
filtep (int rlt1,int al1,int rlt2,int al2)
{
  long int pl,pl2;
  pl = 2*rlt1;
  pl = (long)al1*pl;
  pl2 = 2*rlt2;
  pl += (long)al2*pl2;
  return (int)(pl >> 15);
}

int 
quantl (int el,int detl)
{
  #ifdef CBMC
  int __count_105_109 = 0;
  int __count_106_107 = 0;
  int __count_107_108 = 0;
  int __count_111_113 = 0;
  int __count_112_113 = 0;

  int __count_L107 = 0;

  // TODO: check
  int did_break = 0;
  #endif
  int ril,mil;
  long int wd,decis;
  wd = my_abs(el);
  for(mil = 0 ; 
    #ifdef CBMC
    __count_L107++,
    #endif
  mil < 30 ; mil++) // 107
  {
    decis = (decis_levl[mil]*(long)detl) >> 15L;
    if(wd <= decis) // 105
    { 
      #ifdef CBMC
      __count_105_109++;
      did_break = 1;
      #endif
      // 109
      break;
    }
    #ifdef CBMC
    __count_106_107++;
    #endif
  }

  #ifdef CBMC
  // exit without break
  if(!did_break) {
    __count_107_108++;
  }
  #endif

  if (el >= 0) // 110
  {
    // 111
    ril = quant26bt_pos[mil];
    #ifdef CBMC
    __count_111_113++;
    #endif
  }
  else
  { 
    // 112
    ril = quant26bt_neg[mil];
    #ifdef CBMC
    __count_112_113++;
    #endif
  }
  return ril;
}

int 
logscl (int il,int nbl)
{
  #ifdef CBMC
  int __count_14_15 = 0;
  int __count_14_16 = 0;
  int __count_16_18 = 0;
  int __count_17_18 = 0;
  #endif
  long int wd;  
  wd = ((long)nbl * 127L) >> 7L; 
  nbl = (int)wd + wl_code_table[il >> 2];
  if(nbl < 0) // 14
  {
    #ifdef CBMC
    __count_14_15++;
    #endif
    // 15
    nbl = 0;
  }
  #ifdef CBMC
  else __count_14_16++;
  #endif
  
  if (nbl > 18432) // 16
  {
    // 17
    nbl = 18432;
    #ifdef CBMC
    __count_17_18++;
    #endif
  }
  #ifdef CBMC
  else __count_16_18++;
  #endif
  
  
  return nbl;
}

int 
scalel (int nbl,int shift_constant)
{
  int wd1,wd2,wd3;
  wd1 = (nbl >> 6) & 31;
  wd2 = nbl >> 11;
  wd3 = ilb_table[wd1] >> (shift_constant + 1 - wd2);
  return wd3 << 3;
}

int 
uppol2 (int al1,int al2,int plt,int plt1,int plt2)
{
  #ifdef CBMC
  int __count_93_95 = 0;
  int __count_94_95 = 0;
  int __count_95_96 = 0;
  int __count_95_97 = 0;
  int __count_98_100 = 0;
  int __count_99_100 = 0;
  int __count_100_101 = 0;
  int __count_100_102 = 0;
  #endif

  long int wd2,wd4;
  int apl2;
  wd2 = 4L*(long)al1;
  if ((long)plt*plt1 >= 0L) // 93
  {
    wd2 = -wd2;
    #ifdef CBMC
    __count_94_95++;
    #endif
  }   
  #ifdef CBMC
  else __count_93_95++;
  #endif

  wd2 = wd2 >> 7;                  
  if((long)plt*plt2 >= 0L) // 95
  {
    #ifdef CBMC
    __count_95_96++;
    #endif
    // 96
    wd4 = wd2 + 128;    
  }
  else 
  {
    #ifdef CBMC
    __count_95_97++;
    #endif
    // 97
    wd4 = wd2 - 128;
  }
  apl2 = wd4 + (127L*(long)al2 >> 7L);

  if (apl2 > 12288) // 98
  {
   apl2 = 12288;
   #ifdef CBMC
   __count_99_100++;
   #endif
  }
  #ifdef CBMC
  else __count_98_100++;
  #endif

  if (apl2 < -12288) // 100
  {
    #ifdef CBMC
    __count_100_101++;
    #endif
   apl2 = -12288;
  }
  #ifdef CBMC
  else __count_100_102++;
  #endif
  return apl2;
}

int 
uppol1 (int al1,int apl2,int plt,int plt1)
{
  #ifdef CBMC
  int __count_114_115 = 0;
  int __count_114_116 = 0;
  int __count_117_119 = 0;
  int __count_118_119 = 0;
  int __count_119_121 = 0;
  int __count_120_121 = 0;
  #endif

  long int wd2;
  int wd3,apl1;
  
  wd2 = ((long)al1*255L) >> 8L;  
  if((long)plt*plt1 >= 0L) // 114
  {
    #ifdef CBMC
    __count_114_115++;
    #endif
    // 115
    apl1 = (int)wd2 + 192;   
  }
  else 
  {
    #ifdef CBMC
    __count_114_116++;
    #endif
    // 116
    apl1 = (int)wd2 - 192;
  }
    
  wd3 = 15360 - apl2;          
  if(apl1 > wd3) // 117
  {
    apl1 = wd3;
    #ifdef CBMC
    __count_118_119++;
    #endif
  }
  #ifdef CBMC
  else __count_117_119++;
  #endif

  if(apl1 < -wd3) // 119
  { 
    apl1 = -wd3;
    #ifdef CBMC
    __count_120_121++;
    #endif
  }
  #ifdef CBMC
  else __count_119_121++;
  #endif
  return apl1;
}

int 
logsch (int ih,int nbh)
{
  #ifdef CBMC
  int __count_33_35 = 0;
  int __count_34_35 = 0;
  int __count_35_37 = 0;
  int __count_36_37 = 0;
  #endif

  int wd;
  wd = ((long)nbh * 127L) >> 7L;       
  nbh = wd + wh_code_table[ih];
  
  if (nbh < 0) // 33
  {
    // 34
    nbh = 0;
    #ifdef CBMC
    __count_34_35++;
    #endif
  }
  #ifdef CBMC
  else __count_33_35++;
  #endif
  if (nbh > 22528) // 35
  {
    // 36
    nbh = 22528;
    #ifdef CBMC
    __count_36_37++;
    #endif
  }  
  #ifdef CBMC
  else __count_35_37++;
  #endif
  
  return nbh;
}

void
adpcm (int test_data[])
{
  #ifdef CBMC
  int __count_23_21 = 0;
  int __count_26_27 = 0;
  int __count_L27 = 0;
  int __count_L23 = 0;
  #endif
  int compressed[2];
  int result[4];

  reset();
  
  int i;
  for(i = 0 ; 
    #ifdef CBMC
    __count_L23++,
    #endif
  i < TEST_VECTOR_LENGTH; i += 2) // 23
  {
    #ifdef CBMC
    __count_23_21++;
    #endif
    compressed[i/2] = encode(test_data[i],test_data[i+1]);
  }
  
  for(i = 0 ; 
    #ifdef CBMC
    __count_L27++,
    #endif
  i < TEST_VECTOR_LENGTH; i += 2) // 27
  {
    decode(compressed[i/2]);
    result[i] = xout1;
    result[i+1] = xout2;
    #ifdef CBMC
    __count_26_27++;
    #endif
  }
}

int
main (int argc, char *argv[])
{
  /*
   * At least as many integers as the test vector length
   */
  if (argc != TEST_VECTOR_LENGTH + 1)
  {
    return 1;
  }
  
  int test_data[4];
  
  int i;
  for (i = 0 ; i < TEST_VECTOR_LENGTH; i++) 
  {
    test_data[i] = my_cos(atoi(argv[i + 1])*PI*i); 
  }

  adpcm(test_data);

  return 0;
}


