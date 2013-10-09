#define BUFFERSIZE 50
#define HSIZE 257
#define BITS 16
#define INIT_BITS 9
#define BLOCK_MASK 0x80
#define FIRST 257	/* first free entry */
#define	CLEAR 256	/* table clear output code */
#define CHECK_GAP 10000	/* ratio check interval */

#define MAXCODE(n_bits) ((1 << (n_bits)) - 1)

typedef long int code_int;
typedef unsigned long int count_int;
typedef unsigned short int count_short;
typedef	unsigned char char_type;

int exit_stat = 0;
int n_bits;                             /* number of bits/code */
int maxbits = BITS;                     /* user settable max # bits/code */
int nomagic = 1;                        /* Use a 3-byte magic number header, unless old file */
int zcat_flg = 0;                       /* Write output on stdout, suppress messages */
int quiet = 1;                          /* don't tell me about compression */
int block_compress = BLOCK_MASK;
int clear_flg = 0;
int InCnt;
int apsim_InCnt;
int force = 0;
int offset;
long int in_count = 1;			/* length of input */
long int bytes_out;			/* length of compressed output */
long int out_count = 0;			/* # of codes output (for debugging) */
long int ratio = 0;
char ofname [100];
char orig_text_buffer[BUFFERSIZE];
char comp_text_buffer[BUFFERSIZE+5];
char buf[BITS];
unsigned char *InBuff;
unsigned char *OutBuff;
unsigned short codetab [HSIZE];

count_int checkpoint = CHECK_GAP;
code_int maxcode;                       /* maximum code, given n_bits */
code_int maxmaxcode = 1L << BITS;       /* should NEVER generate this code */
code_int hsize = HSIZE;                 /* for dynamic table sizing */
code_int free_ent = 0;                  /* first unused entry */
count_int fsize;
count_int htab [HSIZE];

char_type lmask[9] = {0xff, 0xfe, 0xfc, 0xf8, 0xf0, 0xe0, 0xc0, 0x80, 0x00};
char_type rmask[9] = {0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f, 0xff};

void 
cl_hash (count_int hsize)		/* reset code table */
{
#ifdef CBMC
//==========> cl_hash : header 81
int __count_81_80 = 0; //Loop counter
//==========> cl_hash : header 78
int __count_78 = 0;
int __count_78_78 = 0; //Loop counter
//==========> cl_hash : header 77
int __count_82 = 0;
int __count_81_82 = 0;
#endif

   register count_int *htab_p = htab+hsize;
   register long i;
   register long m1 = -1;

   i = hsize - 16;

   #ifdef CBMC
   __count_78_78--;
   #endif
   do // 78
   {	
      #ifdef CBMC
      __count_78++;
      __count_78_78++;
      #endif
      /* might use Sys V memset(3) here */
      *(htab_p-16) = m1;
      *(htab_p-15) = m1;
      *(htab_p-14) = m1;
      *(htab_p-13) = m1;
      *(htab_p-12) = m1;
      *(htab_p-11) = m1;
      *(htab_p-10) = m1;
      *(htab_p-9) = m1;
      *(htab_p-8) = m1;
      *(htab_p-7) = m1;
      *(htab_p-6) = m1;
      *(htab_p-5) = m1;
      *(htab_p-4) = m1;
      *(htab_p-3) = m1;
      *(htab_p-2) = m1;
      *(htab_p-1) = m1;
      htab_p -= 16;
   } while ((i -= 16) >= 0);
   
   for (i += 16; i > 0; i--) // 81
   {
      #ifdef CBMC
      __count_81_80++;
      #endif
      *--htab_p = m1; //80
   }
   #ifdef CBMC
   __count_81_82++;
   __count_82++;
   #endif
}

void 
putbyte (char c)
{
#ifdef CBMC
//==========> putbyte : header 108
int __count_108 = 0;
#endif
   *OutBuff++ = c;       		 /* apsim_unknown comp_text_buffer */
#ifdef CBMC
__count_108++;
#endif
}

void 
writebytes (char *buf, int n)
{
#ifdef CBMC
//==========> writebytes : header 17
int __count_17_18 = 0; //Loop counter
//==========> writebytes : header 15
int __count_19 = 0;
int __count_17_19 = 0;
int __count_18_19 = 0;
#endif

  int i;
  for (i=0; (i<n) && (i < BITS); i++) // 17
  {
    #ifdef CBMC
    __count_17_18++;
    #endif
    *OutBuff++ = buf[i];       		 /* apsim_unknown comp_text_buffer */
  }
  #ifdef CBMC
  if(i<n)
  {
    __count_18_19++;
  }
  else
  {
    __count_17_19++;
  }

  __count_19++;
  #endif
}

void 
output (code_int code)
{
#ifdef CBMC
//==========> output : header 90
int __count_90_91 = 0; //Loop counter
//==========> output : header 83
int __count_107 = 0;
int __count_84_85 = 0;
int __count_84_86 = 0;
int __count_86_88 = 0;
int __count_87_88 = 0;
int __count_88_94 = 0;
int __count_91_93 = 0;
int __count_92_93 = 0;
int __count_94_96 = 0;
int __count_95_96 = 0;
int __count_95_107 = 0;
int __count_96_99 = 0;
int __count_98_99 = 0;
int __count_99_100 = 0;
int __count_101_102 = 0;
int __count_103_107 = 0;
int __count_104_105 = 0;
int __count_104_106 = 0;
#endif
  /*
  * On the VAX, it is important to have the register declarations
  * in exactly the order given, or the asm will break.
  */
  register int r_off = offset, bits= n_bits;
  register char * bp = buf;

  if (code >= 0) // 83
  {
    // 84
    /*
     * byte/bit numbering on the VAX is simulated by the following code
     */
    /*
     * Get to the first byte.
     */
    bp += (r_off >> 3);
    r_off &= 7;
    
    /*
     * Since code is always >= 8 bits, only need to mask the first
     * hunk on the left.
     */
    *bp = ((*bp & rmask[r_off]) | (code << r_off)) & lmask[r_off];  /* apsim_unknown buf */
    bp++;
    bits -= (8 - r_off);
    code >>= 8 - r_off;
    
    /* Get any 8 bit parts in the middle (<=1 for up to 16 bits). */
    // 84
    if (bits >= 8)
    {
      #ifdef CBMC
      __count_84_85++;
      #endif
      // 85
      *bp++ = code;  /* apsim_unknown buf */
      code >>= 8;
      bits -= 8;
    }
    #ifdef CBMC
    else __count_84_86++;
    #endif

    /* Last bits. */
    // 86
    if(bits) 
    {
      // 87
      *bp = code;      /* apsim_unknown buf */
      #ifdef CBMC
      __count_87_88++;
      #endif
    }
    #ifdef CBMC
    else __count_86_88++;
    #endif

    offset += n_bits;
    // 88
    if (offset == (n_bits << 3)) 
    {
      // 89
      bp = buf;
      bits = n_bits;
      bytes_out += bits;
      #ifdef CBMC
      __count_90_91=0;
      #endif
      do // 90
      {
        #ifdef CBMC
        __count_90_91++;
        #endif
        putbyte(*bp++);
      }     // 91, 92
      while(( --bits) && ((bp - buf < BITS)));
      
      #ifdef CBMC
      if(bits)
      {
        __count_92_93++;
      }
      else
      {
        __count_91_93++;
      }
      #endif

      // 93
      offset = 0;
    }
    #ifdef CBMC
    else __count_88_94++;
    #endif
    
    /*
     * If the next entry is going to be too big for the code size,
     * then increase it, if possible.
     */
     // 94, 95?
    if ( free_ent > maxcode || ((clear_flg > 0))) 
    {
      // 96
      
      #ifdef CBMC
      if(free_ent > maxcode)
      {
        __count_94_96++;
      }
      else
      {
        __count_95_96++;
      }
      #endif
      /*
       * Write the whole buffer, because the input side won't
       * discover the size increase until after it has read it.
       */
      // 96
      if (offset > 0) 
      {
        // 97
        writebytes( buf, n_bits );
        bytes_out += n_bits;
        #ifdef CBMC
        __count_98_99++;
        #endif
      }
      #ifdef CBMC
      else __count_96_99++;
      #endif
      
      offset = 0;
      // 99
      if (clear_flg) 
      {
        #ifdef CBMC
        __count_99_100++;
        #endif
        // 100
        maxcode = MAXCODE (n_bits = INIT_BITS);
        clear_flg = 0;
      } 
      else 
      {
        n_bits++;
        // 101
        if ( n_bits == maxbits )
        {
          #ifdef CBMC
          __count_101_102++;
          #endif
          // 102
          maxcode = maxmaxcode;
        }
        else
        {
          // 103
          maxcode = MAXCODE(n_bits);
          #ifdef CBMC
          __count_103_107++;
          #endif
        }
      }
    }
    #ifdef CBMC
    else __count_95_107++;
    #endif
  } 
  else 
  {
    // 104
    /*
     * At EOF, write the rest of the buffer.
     */
    // 104
    if ( offset > 0 )
    {
      #ifdef CBMC
      __count_104_105++;
      #endif
      // 105
      writebytes( buf, ((offset + 7) / 8) );
    }
    #ifdef CBMC
    else __count_104_106++;
    #endif
    // 106
    bytes_out += (offset + 7) / 8;
    offset = 0;
  }
  // 107
  #ifdef CBMC
  __count_107++;
  #endif
}

void 
cl_block ()		/* table clear for block compress */
{
  #ifdef CBMC
  //==========> cl_block : header 3
  int __count_14 = 0;
  int __count_3_8 = 0;
  int __count_4_6 = 0;
  int __count_5_10 = 0;
  int __count_11_14 = 0;
  int __count_12_13 = 0;
  #endif

  register long int rat;

  checkpoint = in_count + CHECK_GAP;

  if (in_count > 0x007fffff) // 3
  {
    /* shift will overflow */

    rat = bytes_out >> 8;
    if (rat == 0) // 4
    { 
      /* Don't divide by zero */
      rat = 0x7fffffff; // 5
      #ifdef CBMC
      __count_5_10++;
      #endif
    } 
    else 
    {
      #ifdef CBMC
      __count_4_6++;
      #endif
      rat = in_count / rat; // 6,7
    }
   } 
   else 
   {
    #ifdef CBMC
    __count_3_8++;
    #endif
     // 8
     rat = (in_count << 8) / bytes_out;	/* 8 fractional bits */
   }
   
   if (rat > ratio) // 10
   {
     // 11
     ratio = rat;
     #ifdef CBMC
     __count_11_14++;
     #endif
   } 
   else 
   {
     // 12
    #ifdef CBMC
    __count_12_13++;
    #endif
     ratio = 0;
     cl_hash ((count_int) hsize); 
     free_ent = FIRST;
     clear_flg = 1;
     output ((code_int) CLEAR);
   }
   #ifdef CBMC
   __count_14++;
   #endif
}

unsigned int 
getbyte ()
{
  #ifdef CBMC
//==========> getbyte : header 72
int __count_76 = 0;
int __count_72_75 = 0;
int __count_73_74 = 0;
int __count_73_75 = 0;
  #endif
  if (InCnt > 0 && (apsim_InCnt-- > 0)) // 72,73
  {
    // 74
    InCnt--;
    #ifdef CBMC
    __count_73_74++;
    __count_76++;
    #endif
    return (unsigned int)*InBuff++;
  } 
  else 
  {
    // 75

    #ifdef CBMC
    if(InCnt > 0)
    {
      __count_73_75++;
    }
    else
    {
      __count_72_75++;
    }
    #endif

    #ifdef CBMC
    __count_76++;
    #endif
    return -1;
  }
}

int 
compress ()
{
#ifdef CBMC
//==========> compress : header 47
int __count_47_48 = 0; //Loop counter
int __count_47_49 = 0; //Loop counter
//==========> compress : header 60
int __count_41_42 = 0;
int __count_44_45 = 0;
int __count_44_47 = 0;
int __count_49_50 = 0;
int __count_51_55 = 0;
int __count_52_53 = 0;
int __count_54_55 = 0;
int __count_55_56 = 0;
int __count_57_58 = 0;
int __count_60_40 = 0; //Loop counter
//==========> compress : header 37
int __count_37_36 = 0; //Loop counter
//==========> compress : header 31
int __count_63 = 0;
int __count_32_34 = 0;
int __count_33_34 = 0;
int __count_61_62 = 0;
int __count_61_63 = 0;
#endif

  register long fcode;
  register code_int i = 0;
  register int c;
  register code_int ent;
  register int disp;
  register code_int hsize_reg;
  register int hshift;

  offset = 0;
  bytes_out = 3;
  out_count = 0;
  clear_flg = 0;
  ratio = 0;
  in_count = 1;
  checkpoint = CHECK_GAP;
  maxcode = MAXCODE(n_bits = INIT_BITS);
  
  free_ent = ((block_compress) ? 
    (
      #ifdef CBMC
      __count_32_34++,
      #endif
      FIRST) 
    :
    (
      #ifdef CBMC
      __count_33_34++,
      #endif
      256) );
  ent = getbyte ();
  hshift = 0;
  
  // 37
  for (fcode = (long) hsize; fcode < 65536L; fcode *= 2L)
  {
    #ifdef CBMC
    __count_37_36++;
    #endif
    hshift++;
  }

  hshift = 8 - hshift;	/* set hash code range bound */
  hsize_reg = hsize;
  cl_hash( (count_int) hsize_reg); /* clear hash table */

  #ifdef CBMC
  __count_60_40=0;
  #endif
  // 60
  while (InCnt > 0)           /* apsim_loop 11 0 */
  {
    #ifdef CBMC
    __count_60_40++;
    #endif
    int apsim_bound111 = 0;
    c = getbyte();   /* decrements InCnt */

    in_count++;
    fcode = (long) (((long) c << maxbits) + ent);
    i     = ((c << hshift) ^ ent);

    if (htab[i] == fcode) // 41
    {
      #ifdef CBMC
      __count_41_42++;
      #endif
      //42
      ent = codetab[i];
      continue;
    } 
    else if ((long)htab[i] < 0 ) // 43
    {
      // 54
      #ifdef CBMC
      __count_54_55++;
      #endif
      goto nomatch;
    }

    // 44
    disp = hsize_reg - i;
    // 44
    if (i == 0) 
    {
      #ifdef CBMC
      __count_44_45++;
      #endif
      // 45
      disp = 1;
    }
    #ifdef CBMC
    else __count_44_47++;
    #endif
#ifdef CBMC
__count_47_48=0;
__count_47_49=0;
#endif
probe:
    // 47
    if ((i -= disp) < 0) 
    {  /* apsim_loop 111 11 */
      #ifdef CBMC
      __count_47_48++;
      #endif
      // 48
      i += hsize_reg;
    }
    #ifdef CBMC
    else __count_47_49++;
    #endif

    // 49
    if (htab[i] == fcode) 
    {
      #ifdef CBMC
      __count_49_50++;
      #endif
      // 50
      ent = codetab[i];
      continue;
    }

    // 51, 52 (53)
    if ((long) htab[i] > 0 && (++apsim_bound111 < in_count))
    {
      // 46
      goto probe;
    }
    #ifdef CBMC
    if((long) htab[i] > 0)
    {
      __count_52_53++;
    }
    else
    {
      __count_51_55++;
    }
    
    #endif
nomatch:
    // 55
    out_count++;
    ent = c;
    // 55
    if (free_ent < maxmaxcode)
    {
      #ifdef CBMC
      __count_55_56++;
      #endif
      // 56
      codetab[i] = free_ent++;	
      htab[i] = fcode;
    } 
    // 57, 58
    else if (((count_int)in_count >= checkpoint) 
      && (
        #ifdef CBMC
         __count_57_58++,
        #endif
        block_compress
        )
      ) 
    {
      // 59 
      cl_block ();
    }
  }
   
  if (bytes_out > in_count) // 61
  { 
    // 62
    #ifdef CBMC
    __count_61_62++;
    #endif
    exit_stat = 2;
  }
  #ifdef CBMC
  else __count_61_63++;
  #endif

  // 63
  #ifdef CBMC
  __count_63++;
  #endif
  return exit_stat;
}

int
main (int argc, char *argv[])
{
  /*
   * At least as many integers as the buffer size need to be supplied
   */
  if (argc != BUFFERSIZE + 1)
  {
    return 1;
  }
  
  int i;
  for (i = 0; i < argc - 1; ++i)
  {
    orig_text_buffer[i] = atoi (argv[i + 1]);
  }

  maxbits     = BITS;
  maxmaxcode  = 1 << maxbits;   
  InCnt       = BUFFERSIZE;
  apsim_InCnt = BUFFERSIZE + 3;
  InBuff      = (unsigned char *) orig_text_buffer;
  OutBuff     = (unsigned char *) comp_text_buffer;

  int val = compress();
  printf("%d", val);
  
  return 0;
}

