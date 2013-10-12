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
//==========> my_abs : header 38
int __count_41 = 0;
int __count_38_39 = 0;
int __count_40_41 = 0;
  #endif
  if (n >= 0)  // 38
  {
    // 39
    #ifdef CBMC
    __count_38_39++;
    __count_41++;
    #endif

#ifdef CBMC
assert(__count_40_41 <= 1); // Upper capacity constraint
assert(__count_38_39 <= 1); // Upper capacity constraint
assert(__count_41 >= 1); // Lower capacity constraint
assert(__count_41 <= 1); // Upper capacity constraint
assert(__count_40_41 > 0 ==> __count_41 > 0); // Execution dependence
assert(__count_38_39 > 0 ==> __count_41 > 0); // Execution dependence
#endif

    return n;
  }
  else
  {
    #ifdef CBMC
    __count_40_41++;
    __count_41++;
    #endif
    // 40

#ifdef CBMC
assert(__count_40_41 <= 1); // Upper capacity constraint
assert(__count_38_39 <= 1); // Upper capacity constraint
assert(__count_41 >= 1); // Lower capacity constraint
assert(__count_41 <= 1); // Upper capacity constraint
assert(__count_40_41 > 0 ==> __count_41 > 0); // Execution dependence
assert(__count_38_39 > 0 ==> __count_41 > 0); // Execution dependence
#endif

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
//==========> upzero : header 132
int __count_129_131 = 0;
int __count_130_131 = 0;
int __count_132_128 = 0; //Loop counter
//==========> upzero : header 125
int __count_125_124 = 0;
int __count_125_124_L = 0; //Loop counter
//==========> upzero : header 122
int __count_133 = 0;
int __count_125_126 = 0;
int __count_132_133 = 0;

  #endif

  int i,wd2,wd3;
  if(dlt == 0) // 122
  {
    #ifdef CBMC
    __count_125_124_L = 0;
    #endif
    for(i = 0 ; i < 6 ; i++) // 125
    {
      #ifdef CBMC
      __count_125_124++;
      __count_125_124_L++;
      #endif
      bli[i] = (int)((255L*bli[i]) >> 8L); 
    }
    #ifdef CBMC
    assert(__count_125_124_L  <= 7); // Loop counter property
    __count_125_126++;
    #endif
  }
  else 
  {
    #ifdef CBMC
    __count_132_128 = 0;
    #endif
    for(i = 0 ; i < 6 ; i++) // 132
    {
      #ifdef CBMC
      __count_132_128++;
      #endif
      if ((long)dlt*dlti[i] >= 0) // 128
      { 
        // 129
        wd2 = 128;
        #ifdef CBMC
        __count_129_131++;
        #endif
      }
      else
      { 
        // 130
        wd2 = -128;
        #ifdef CBMC
        __count_130_131++;
        #endif
      }
      wd3 = (int)((255L*bli[i]) >> 8L);  
      bli[i] = wd2 + wd3;
     }
     #ifdef CBMC
     assert(__count_132_128  <= 7); // Loop counter property
     __count_132_133++;
     #endif
  }
  
  dlti[5] = dlti[4];
  dlti[4] = dlti[3];
  dlti[3] = dlti[2];
  dlti[1] = dlti[0];
  dlti[0] = dlt;

#ifdef CBMC
__count_133++;
#endif

#ifdef CBMC
assert(__count_129_131 <= 6); // Upper capacity constraint
assert(__count_130_131 == 0); // Dead code
assert(__count_133 >= 1); // Lower capacity constraint
assert(__count_133 <= 1); // Upper capacity constraint
assert(__count_132_133 <= 1); // Upper capacity constraint
assert(__count_125_124 <= 6); // Upper capacity constraint
assert(__count_125_126 <= 1); // Upper capacity constraint
assert(__count_129_131 > 0 ==> __count_132_133 > 0); // Mutual inclusion
assert(__count_132_133 > 0 ==> __count_129_131 > 0); // Mutual inclusion
assert(__count_125_124 > 0 ==> __count_125_126 > 0); // Mutual inclusion
assert(__count_125_126 > 0 ==> __count_125_124 > 0); // Mutual inclusion
assert(__count_129_131 > 0 ==> __count_133 > 0); // Execution dependence
assert(__count_132_133 > 0 ==> __count_133 > 0); // Execution dependence
assert(__count_125_124 > 0 ==> __count_133 > 0); // Execution dependence
assert(__count_125_126 > 0 ==> __count_133 > 0); // Execution dependence
#endif

}


int 
encode (int xin1,int xin2)
{
  #ifdef CBMC
//==========> encode : header 70
int __count_70_69 = 0;
int __count_70_69_L = 0; //Loop counter
//==========> encode : header 67
int __count_67_66 = 0;
int __count_67_66_L = 0; //Loop counter
//==========> encode : header 65
int __count_92 = 0;
int __count_81_83 = 0;
int __count_82_84 = 0;
int __count_85_86 = 0;
int __count_85_87 = 0;

  #endif
  int i;
  int *h_ptr,*tqmf_ptr,*tqmf_ptr1;
  long int xa,xb;
  int decis;

  h_ptr = h;
  tqmf_ptr = tqmf;
  xa = (long)(*tqmf_ptr++) * (*h_ptr++);
  xb = (long)(*tqmf_ptr++) * (*h_ptr++);
  
  #ifdef CBMC
  __count_67_66_L = 0;
  #endif
  for(i = 0 ; i < 10 ; i++) // 67
  {
    #ifdef CBMC
    __count_67_66++;
    __count_67_66_L++;
    #endif
    xa += (long)(*tqmf_ptr++) * (*h_ptr++);
    xb += (long)(*tqmf_ptr++) * (*h_ptr++);
  }
  #ifdef CBMC
  assert(__count_67_66_L  <= 11); // Loop counter property
  #endif
  
  xa += (long)(*tqmf_ptr++) * (*h_ptr++);
  xb += (long)(*tqmf_ptr) * (*h_ptr++);
  tqmf_ptr1 = tqmf_ptr - 2;
  
  #ifdef CBMC
  __count_70_69_L = 0;
  #endif
  for(i = 0 ; i < 22 ; i++) // 70
  {
    #ifdef CBMC
    __count_70_69++;
    __count_70_69_L++;
    #endif
    *tqmf_ptr-- = *tqmf_ptr1--;
  }
  #ifdef CBMC
  assert(__count_70_69_L  <= 23); // Loop counter property
  #endif
  
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
    #ifdef CBMC
    __count_81_83++;
    #endif
    // 83
    ih = 1;
  }
  
  decis = (564L*(long)deth) >> 12L;
  
  if(my_abs(eh) > decis) // 85
  { 
    #ifdef CBMC
    __count_85_86++;
    #endif
    // 86
    ih--;
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

#ifdef CBMC
__count_92++;
#endif

#ifdef CBMC
assert(__count_67_66 >= 10); // Lower capacity constraint
assert(__count_67_66 <= 10); // Upper capacity constraint
assert(__count_70_69 >= 22); // Lower capacity constraint
assert(__count_70_69 <= 22); // Upper capacity constraint
assert(__count_81_83 <= 1); // Upper capacity constraint
assert(__count_82_84 <= 1); // Upper capacity constraint
assert(__count_85_86 <= 1); // Upper capacity constraint
assert(__count_85_87 <= 1); // Upper capacity constraint
assert(__count_92 >= 1); // Lower capacity constraint
assert(__count_92 <= 1); // Upper capacity constraint
//assert(__count_81_83 > 0 ==> __count_85_87 == 0); // Mutual exclusion
//assert(__count_85_87 > 0 ==> __count_81_83 == 0); // Mutual exclusion
assert(__count_81_83 > 0 ==> __count_67_66 > 0); // Execution dependence
assert(__count_81_83 > 0 ==> __count_70_69 > 0); // Execution dependence
//assert(__count_81_83 > 0 ==> __count_85_86 > 0); // Execution dependence
assert(__count_81_83 > 0 ==> __count_92 > 0); // Execution dependence
assert(__count_82_84 > 0 ==> __count_67_66 > 0); // Execution dependence
assert(__count_82_84 > 0 ==> __count_70_69 > 0); // Execution dependence
assert(__count_82_84 > 0 ==> __count_92 > 0); // Execution dependence
assert(__count_85_86 > 0 ==> __count_67_66 > 0); // Execution dependence
assert(__count_85_86 > 0 ==> __count_70_69 > 0); // Execution dependence
assert(__count_85_86 > 0 ==> __count_92 > 0); // Execution dependence
assert(__count_85_87 > 0 ==> __count_67_66 > 0); // Execution dependence
assert(__count_85_87 > 0 ==> __count_70_69 > 0); // Execution dependence
//assert(__count_85_87 > 0 ==> __count_82_84 > 0); // Execution dependence
assert(__count_85_87 > 0 ==> __count_92 > 0); // Execution dependence
#endif

  return il | (ih << 6);
}

void 
decode (int input)
{
  #ifdef CBMC
//==========> decode : header 61
int __count_61_60 = 0;
int __count_61_60_L = 0; //Loop counter
//==========> decode : header 58
int __count_58_57 = 0;
int __count_58_57_L = 0; //Loop counter
//==========> decode : header 42
int __count_62 = 0;
int __count_58_59 = 0;

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

  #ifdef CBMC
  __count_58_57_L = 0;
  #endif
  for(i = 0 ; i < 10 ; i++) // 58
  {
    #ifdef CBMC
    __count_58_57++;
    __count_58_57_L++;
    #endif
    xa1 += (long)(*ac_ptr++) * (*h_ptr++);
    xa2 += (long)(*ad_ptr++) * (*h_ptr++);
  }
  #ifdef CBMC
  assert(__count_58_57_L  <= 11); // Loop counter property
  __count_58_59++;
  #endif

  xa1 += (long)(*ac_ptr) * (*h_ptr++);
  xa2 += (long)(*ad_ptr) * (*h_ptr++);

  xout1 = xa1 >> 14;
  xout2 = xa2 >> 14;

  ac_ptr1 = ac_ptr - 1;
  ad_ptr1 = ad_ptr - 1;
  
  #ifdef CBMC
  __count_61_60_L = 0;
  #endif
  for(i = 0 ; i < 10 ; i++) // 61
  {
    #ifdef CBMC
    __count_61_60++;
    __count_61_60_L++;
    #endif
    *ac_ptr-- = *ac_ptr1--;
    *ad_ptr-- = *ad_ptr1--;
  }
  #ifdef CBMC
  assert(__count_61_60_L  <= 11); // Loop counter property
  #endif
  
  *ac_ptr = xd;
  *ad_ptr = xs;

  #ifdef CBMC
  __count_62++;
  #endif

#ifdef CBMC
assert(__count_58_57 >= 10); // Lower capacity constraint
assert(__count_58_57 <= 10); // Upper capacity constraint
assert(__count_58_59 >= 1); // Lower capacity constraint
assert(__count_58_59 <= 1); // Upper capacity constraint
assert(__count_61_60 >= 10); // Lower capacity constraint
assert(__count_61_60 <= 10); // Upper capacity constraint
assert(__count_62 >= 1); // Lower capacity constraint
assert(__count_62 <= 1); // Upper capacity constraint
#endif

}

void 
reset ()
{
  #ifdef CBMC
//==========> reset : header 12
int __count_12_11 = 0;
int __count_12_11_L = 0; //Loop counter
//==========> reset : header 9
int __count_9_8 = 0;
int __count_9_8_L = 0; //Loop counter
//==========> reset : header 6
int __count_6_5 = 0;
int __count_6_5_L = 0; //Loop counter
//==========> reset : header 3
int __count_3_2 = 0;
int __count_3_2_L = 0; //Loop counter
//==========> reset : header 1
int __count_13 = 0;
int __count_3_4 = 0;

  #endif

  int i;

  detl = dec_detl = 32; 
  deth = dec_deth = 8;
  nbl = al1 = al2 = plt1 = plt2 = rlt1 = rlt2 = 0;
  nbh = ah1 = ah2 = ph1 = ph2 = rh1 = rh2 = 0;
  dec_nbl = dec_al1 = dec_al2 = dec_plt1 = dec_plt2 = dec_rlt1 = dec_rlt2 = 0;
  dec_nbh = dec_ah1 = dec_ah2 = dec_ph1 = dec_ph2 = dec_rh1 = dec_rh2 = 0;

  #ifdef CBMC
  __count_3_2_L = 0;
  #endif
  for(i = 0; i < 6; i++) // 3
  {
    #ifdef CBMC
    __count_3_2++;
    __count_3_2_L++;
    #endif
    delay_dltx[i] = 0;
    delay_dhx[i] = 0;
    dec_del_dltx[i] = 0;
    dec_del_dhx[i] = 0;
  }
  #ifdef CBMC
  assert(__count_3_2_L  <= 7); // Loop counter property
  __count_3_4++;
  #endif

  #ifdef CBMC
  __count_6_5_L = 0;
  #endif
  for(i = 0; i < 6 ; i++) // 6
  {
    #ifdef CBMC
    __count_6_5++;
    __count_6_5_L++;
    #endif
    delay_bpl[i] = 0;
    delay_bph[i] = 0;
    dec_del_bpl[i] = 0;
    dec_del_bph[i] = 0;
  }
  #ifdef CBMC
  assert(__count_6_5_L  <= 7); // Loop counter property
  #endif

  #ifdef CBMC
  __count_9_8_L = 0;
  #endif
  for(i = 0; i < 23 ; i++) // 9
  {
    #ifdef CBMC
    __count_9_8++;
    __count_9_8_L++;
    #endif
    tqmf[i] = 0;
  }
  #ifdef CBMC
  assert(__count_9_8_L  <= 24); // Loop counter property
  #endif
  
  #ifdef CBMC
  __count_12_11_L = 0;
  #endif
  for(i = 0; i < 11 ; i++) // 12
  {
    #ifdef CBMC
    __count_12_11_L++;
    __count_12_11++;
    #endif
    accumc[i] = 0;
    accumd[i] = 0;
  }
  #ifdef CBMC
  assert(__count_12_11_L  <= 12); // Loop counter property
  __count_13++;
  #endif

#ifdef CBMC
assert(__count_13 >= 1); // Lower capacity constraint
assert(__count_13 <= 1); // Upper capacity constraint
assert(__count_3_2 >= 6); // Lower capacity constraint
assert(__count_3_2 <= 6); // Upper capacity constraint
assert(__count_3_4 >= 1); // Lower capacity constraint
assert(__count_3_4 <= 1); // Upper capacity constraint
assert(__count_6_5 >= 6); // Lower capacity constraint
assert(__count_6_5 <= 6); // Upper capacity constraint
assert(__count_9_8 >= 23); // Lower capacity constraint
assert(__count_9_8 <= 23); // Upper capacity constraint
assert(__count_12_11 >= 11); // Lower capacity constraint
assert(__count_12_11 <= 11); // Upper capacity constraint
#endif

}

int 
filtez (int *bpl,int *dlt)
{
  #ifdef CBMC
//==========> filtez : header 31
int __count_31_30 = 0;
int __count_31_30_L = 0; //Loop counter
//==========> filtez : header 29
int __count_32 = 0;
int __count_31_32 = 0;

  #endif
  int i;
  long int zl;
  zl = (long)(*bpl++) * (*dlt++);
  #ifdef CBMC
  __count_31_30_L = 0;
  #endif
  for(i = 1 ; i < 6 ; i++) // 31
  {
    #ifdef CBMC
    __count_31_30++;
    __count_31_30_L++;
    #endif
    zl += (long)(*bpl++) * (*dlt++);
  }
  #ifdef CBMC
  assert(__count_31_30_L  <= 6); // Loop counter property
  __count_31_32++;
  #endif

  #ifdef CBMC
  __count_32++;
  #endif

#ifdef CBMC
assert(__count_32 >= 1); // Lower capacity constraint
assert(__count_32 <= 1); // Upper capacity constraint
assert(__count_31_32 >= 1); // Lower capacity constraint
assert(__count_31_32 <= 1); // Upper capacity constraint
assert(__count_31_30 >= 5); // Lower capacity constraint
assert(__count_31_30 <= 5); // Upper capacity constraint
#endif

  return (int)(zl >> 14);
}

int 
filtep (int rlt1,int al1,int rlt2,int al2)
{
#ifdef CBMC
//==========> filtep : header 64
int __count_64 = 0;
#endif
  long int pl,pl2;
  pl = 2*rlt1;
  pl = (long)al1*pl;
  pl2 = 2*rlt2;
  pl += (long)al2*pl2;
  #ifdef CBMC
  __count_64++;
  #endif

#ifdef CBMC
assert(__count_64 >= 1); // Lower capacity constraint
assert(__count_64 <= 1); // Upper capacity constraint
#endif

  return (int)(pl >> 15);
}

int 
quantl (int el,int detl)
{
  #ifdef CBMC
//==========> quantl : header 107
int __count_107_105 = 0;
int __count_107_105_L = 0; //Loop counter
//==========> quantl : header 103
int __count_113 = 0;
int __count_105_109 = 0;
int __count_107_108 = 0;
int __count_110_111 = 0;
int __count_110_112 = 0;


  // TODO: check
  int did_break = 0;
  #endif
  int ril,mil;
  long int wd,decis;
  wd = my_abs(el);
  #ifdef CBMC
  __count_107_105_L = 0;
  #endif
  for(mil = 0 ; mil < 30 ; mil++) // 107
  {
    #ifdef CBMC
    __count_107_105_L++;
    __count_107_105++;
    #endif
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
  }
  #ifdef CBMC
  assert(__count_107_105_L  <= 31); // Loop counter property
  #endif

  #ifdef CBMC
  // exit without break
  if(!did_break) {
    __count_107_108++;
  }
  #endif

  if (el >= 0) // 110
  {
    #ifdef CBMC
    __count_110_111++;
    #endif
    // 111
    ril = quant26bt_pos[mil];
  }
  else
  { 
    #ifdef CBMC
    __count_110_112++;
    #endif
    // 112
    ril = quant26bt_neg[mil];
  }

#ifdef CBMC
__count_113++;
#endif

#ifdef CBMC
assert(__count_105_109 <= 1); // Upper capacity constraint
assert(__count_107_105 >= 1); // Lower capacity constraint
assert(__count_107_105 <= 30); // Upper capacity constraint
assert(__count_107_108 <= 1); // Upper capacity constraint
assert(__count_110_112 <= 1); // Upper capacity constraint
assert(__count_110_111 <= 1); // Upper capacity constraint
assert(__count_113 >= 1); // Lower capacity constraint
assert(__count_113 <= 1); // Upper capacity constraint
//assert(__count_105_109 > 0 ==> __count_110_112 == 0); // Mutual exclusion
//assert(__count_110_112 > 0 ==> __count_105_109 == 0); // Mutual exclusion
assert(__count_105_109 > 0 ==> __count_107_105 > 0); // Execution dependence
//assert(__count_105_109 > 0 ==> __count_110_111 > 0); // Execution dependence
assert(__count_105_109 > 0 ==> __count_113 > 0); // Execution dependence
assert(__count_107_108 > 0 ==> __count_107_105 > 0); // Execution dependence
assert(__count_107_108 > 0 ==> __count_113 > 0); // Execution dependence
assert(__count_110_112 > 0 ==> __count_107_105 > 0); // Execution dependence
//assert(__count_110_112 > 0 ==> __count_107_108 > 0); // Execution dependence
assert(__count_110_112 > 0 ==> __count_113 > 0); // Execution dependence
assert(__count_110_111 > 0 ==> __count_107_105 > 0); // Execution dependence
assert(__count_110_111 > 0 ==> __count_113 > 0); // Execution dependence
#endif

  return ril;
}

int 
logscl (int il,int nbl)
{
  #ifdef CBMC
//==========> logscl : header 14
int __count_18 = 0;
int __count_14_15 = 0;
int __count_14_16 = 0;
int __count_16_17 = 0;
int __count_16_18 = 0;
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
    #ifdef CBMC
    __count_16_17++;
    #endif
    // 17
    nbl = 18432;
  }
  #ifdef CBMC
  else __count_16_18++;
  #endif
  
  #ifdef CBMC
  __count_18++;
  #endif

#ifdef CBMC
assert(__count_16_17 == 0); // Dead code
assert(__count_18 >= 1); // Lower capacity constraint
assert(__count_18 <= 1); // Upper capacity constraint
assert(__count_16_18 >= 1); // Lower capacity constraint
assert(__count_16_18 <= 1); // Upper capacity constraint
assert(__count_14_16 <= 1); // Upper capacity constraint
assert(__count_14_15 <= 1); // Upper capacity constraint
assert(__count_14_16 > 0 ==> __count_16_18 > 0); // Execution dependence
assert(__count_14_16 > 0 ==> __count_18 > 0); // Execution dependence
assert(__count_14_15 > 0 ==> __count_16_18 > 0); // Execution dependence
assert(__count_14_15 > 0 ==> __count_18 > 0); // Execution dependence
#endif

  return nbl;
}

int 
scalel (int nbl,int shift_constant)
{
#ifdef CBMC
//==========> scalel : header 63
int __count_63 = 0;
#endif
  int wd1,wd2,wd3;
  wd1 = (nbl >> 6) & 31;
  wd2 = nbl >> 11;
  wd3 = ilb_table[wd1] >> (shift_constant + 1 - wd2);
  #ifdef CBMC
  __count_63++;
  #endif

#ifdef CBMC
assert(__count_63 >= 1); // Lower capacity constraint
assert(__count_63 <= 1); // Upper capacity constraint
#endif

  return wd3 << 3;
}

int 
uppol2 (int al1,int al2,int plt,int plt1,int plt2)
{
  #ifdef CBMC
//==========> uppol2 : header 93
int __count_102 = 0;
int __count_93_94 = 0;
int __count_93_95 = 0;
int __count_96_98 = 0;
int __count_97_98 = 0;
int __count_98_99 = 0;
int __count_98_100 = 0;
int __count_100_102 = 0;
int __count_101_102 = 0;
  #endif

  long int wd2,wd4;
  int apl2;
  wd2 = 4L*(long)al1;
  if ((long)plt*plt1 >= 0L) // 93
  {
    #ifdef CBMC
    __count_93_94++;
    #endif
    wd2 = -wd2;
  }   
  #ifdef CBMC
  else __count_93_95++;
  #endif

  wd2 = wd2 >> 7;                  
  if((long)plt*plt2 >= 0L) // 95
  {
    // 96
    wd4 = wd2 + 128;    
    #ifdef CBMC
    __count_96_98++;
    #endif
  }
  else 
  {
    // 97
    wd4 = wd2 - 128;
    #ifdef CBMC
    __count_97_98++;
    #endif
  }
  apl2 = wd4 + (127L*(long)al2 >> 7L);

  if (apl2 > 12288) // 98
  {
   #ifdef CBMC
   __count_98_99++;
   #endif
   apl2 = 12288;
  }
  #ifdef CBMC
  else __count_98_100++;
  #endif

  if (apl2 < -12288) // 100
  {
   apl2 = -12288;
    #ifdef CBMC
    __count_101_102++;
    #endif
  }
  #ifdef CBMC
  else __count_100_102++;
  #endif

#ifdef CBMC
__count_102++;
#endif

#ifdef CBMC
assert(__count_96_98 >= 1); // Lower capacity constraint
assert(__count_96_98 <= 1); // Upper capacity constraint
assert(__count_97_98 == 0); // Dead code
assert(__count_98_99 == 0); // Dead code
assert(__count_98_100 >= 1); // Lower capacity constraint
assert(__count_98_100 <= 1); // Upper capacity constraint
assert(__count_102 >= 1); // Lower capacity constraint
assert(__count_102 <= 1); // Upper capacity constraint
assert(__count_100_102 >= 1); // Lower capacity constraint
assert(__count_100_102 <= 1); // Upper capacity constraint
assert(__count_101_102 == 0); // Dead code
assert(__count_93_94 >= 1); // Lower capacity constraint
assert(__count_93_94 <= 1); // Upper capacity constraint
assert(__count_93_95 == 0); // Dead code
#endif

  return apl2;
}

int 
uppol1 (int al1,int apl2,int plt,int plt1)
{
  #ifdef CBMC
//==========> uppol1 : header 114
int __count_121 = 0;
int __count_115_117 = 0;
int __count_116_117 = 0;
int __count_117_118 = 0;
int __count_117_119 = 0;
int __count_119_120 = 0;
int __count_119_121 = 0;

  #endif

  long int wd2;
  int wd3,apl1;
  
  wd2 = ((long)al1*255L) >> 8L;  
  if((long)plt*plt1 >= 0L) // 114
  {
    // 115
    apl1 = (int)wd2 + 192;   
    #ifdef CBMC
    __count_115_117++;
    #endif
  }
  else 
  {
    // 116
    apl1 = (int)wd2 - 192;
    #ifdef CBMC
    __count_116_117++;
    #endif
  }
    
  wd3 = 15360 - apl2;          
  if(apl1 > wd3) // 117
  {
    #ifdef CBMC
    __count_117_118++;
    #endif
    apl1 = wd3;
  }
  #ifdef CBMC
  else __count_117_119++;
  #endif

  if(apl1 < -wd3) // 119
  { 
    #ifdef CBMC
    __count_119_120++;
    #endif
    apl1 = -wd3;
  }
  #ifdef CBMC
  else __count_119_121++;
  #endif

#ifdef CBMC
__count_121++;
#endif

#ifdef CBMC
assert(__count_115_117 >= 1); // Lower capacity constraint
assert(__count_115_117 <= 1); // Upper capacity constraint
assert(__count_116_117 == 0); // Dead code
assert(__count_117_118 == 0); // Dead code
assert(__count_117_119 >= 1); // Lower capacity constraint
assert(__count_117_119 <= 1); // Upper capacity constraint
assert(__count_119_120 == 0); // Dead code
assert(__count_119_121 >= 1); // Lower capacity constraint
assert(__count_119_121 <= 1); // Upper capacity constraint
assert(__count_121 >= 1); // Lower capacity constraint
assert(__count_121 <= 1); // Upper capacity constraint
#endif

  return apl1;
}

int 
logsch (int ih,int nbh)
{
  #ifdef CBMC
//==========> logsch : header 33
int __count_37 = 0;
int __count_33_34 = 0;
int __count_33_35 = 0;
int __count_35_36 = 0;
int __count_35_37 = 0;

  #endif

  int wd;
  wd = ((long)nbh * 127L) >> 7L;       
  nbh = wd + wh_code_table[ih];
  
  if (nbh < 0) // 33
  {
    // 34
    nbh = 0;
    #ifdef CBMC
    __count_33_34++;
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
    __count_35_36++;
    #endif
  }  
  #ifdef CBMC
  else __count_35_37++;
  #endif
  
  #ifdef CBMC
  __count_37++;
  #endif

#ifdef CBMC
assert(__count_33_34 <= 1); // Upper capacity constraint
assert(__count_33_35 <= 1); // Upper capacity constraint
assert(__count_35_36 == 0); // Dead code
assert(__count_37 >= 1); // Lower capacity constraint
assert(__count_37 <= 1); // Upper capacity constraint
assert(__count_35_37 >= 1); // Lower capacity constraint
assert(__count_35_37 <= 1); // Upper capacity constraint
assert(__count_33_34 > 0 ==> __count_35_37 > 0); // Execution dependence
assert(__count_33_34 > 0 ==> __count_37 > 0); // Execution dependence
assert(__count_33_35 > 0 ==> __count_35_37 > 0); // Execution dependence
assert(__count_33_35 > 0 ==> __count_37 > 0); // Execution dependence
#endif

  return nbh;
}

void
adpcm (int test_data[])
{
  #ifdef CBMC
//==========> adpcm : header 27
int __count_27_25 = 0;
int __count_27_25_L = 0; //Loop counter
//==========> adpcm : header 23
int __count_21_22 = 0;
int __count_23_21 = 0; //Loop counter
//==========> adpcm : header 19
int __count_28 = 0;
int __count_23_24 = 0;

  #endif
  int compressed[2];
  int result[4];

  reset();
  
  int i;
  #ifdef CBMC
  __count_23_21 = 0;
  #endif
  for(i = 0 ; i < TEST_VECTOR_LENGTH; i += 2) // 23
  {
    #ifdef CBMC
    __count_23_21++;
    __count_21_22++;
    #endif
    compressed[i/2] = encode(test_data[i],test_data[i+1]);
  }
  #ifdef CBMC
  assert(__count_23_21  <= 3); // Loop counter property
  __count_23_24++;
  #endif
  
  #ifdef CBMC
  __count_27_25_L = 0;
  #endif
  for(i = 0 ; i < TEST_VECTOR_LENGTH; i += 2) // 27
  {
    #ifdef CBMC
    __count_27_25++;
    __count_27_25_L++;
    #endif
    decode(compressed[i/2]);
    result[i] = xout1;
    result[i+1] = xout2;
  }
  #ifdef CBMC
  assert(__count_27_25_L  <= 3); // Loop counter property
  #endif
  
  #ifdef CBMC
  __count_28++;
  #endif

#ifdef CBMC
assert(__count_27_25 >= 2); // Lower capacity constraint
assert(__count_27_25 <= 2); // Upper capacity constraint
assert(__count_21_22 >= 2); // Lower capacity constraint
assert(__count_21_22 <= 2); // Upper capacity constraint
assert(__count_28 >= 1); // Lower capacity constraint
assert(__count_28 <= 1); // Upper capacity constraint
assert(__count_23_24 >= 1); // Lower capacity constraint
assert(__count_23_24 <= 1); // Upper capacity constraint
#endif

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


