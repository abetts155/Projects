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
   register count_int *htab_p = htab+hsize;
   register long i;
   register long m1 = -1;

   i = hsize - 16;
   do 
   {	
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
   
   for (i += 16; i > 0; i--)
   {
      *--htab_p = m1;
   }
}

void 
putbyte (char c)
{
   *OutBuff++ = c;       		 /* apsim_unknown comp_text_buffer */
}

void 
writebytes (char *buf, int n)
{
  int i;
  for (i=0; (i<n) && (i < BITS); i++) 
  {
    *OutBuff++ = buf[i];       		 /* apsim_unknown comp_text_buffer */
  }
}

void 
output (code_int code)
{
  /*
  * On the VAX, it is important to have the register declarations
  * in exactly the order given, or the asm will break.
  */
  register int r_off = offset, bits= n_bits;
  register char * bp = buf;

  if (code >= 0) 
  {
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
    if (bits >= 8) 
    {
      *bp++ = code;  /* apsim_unknown buf */
      code >>= 8;
      bits -= 8;
    }

    /* Last bits. */
    if(bits) 
    {
      *bp = code;			 /* apsim_unknown buf */
    }

    offset += n_bits;
    if (offset == (n_bits << 3)) 
    {
      bp = buf;
      bits = n_bits;
      bytes_out += bits;
      do 
      {
        putbyte(*bp++);
      } 
      while(( --bits) && ((bp - buf < BITS)));
      
      offset = 0;
    }
    
    /*
     * If the next entry is going to be too big for the code size,
     * then increase it, if possible.
     */
    if ( free_ent > maxcode || ((clear_flg > 0))) 
    {
      /*
       * Write the whole buffer, because the input side won't
       * discover the size increase until after it has read it.
       */
      if (offset > 0) 
      {
        writebytes( buf, n_bits );
        bytes_out += n_bits;
      }
      
      offset = 0;
      if (clear_flg) 
      {
        maxcode = MAXCODE (n_bits = INIT_BITS);
        clear_flg = 0;
      } 
      else 
      {
        n_bits++;
        if ( n_bits == maxbits )
        {
          maxcode = maxmaxcode;
        }
        else
        {
          maxcode = MAXCODE(n_bits);
        }
      }
    }
  } 
  else 
  {
    /*
     * At EOF, write the rest of the buffer.
     */
    if ( offset > 0 )
    {
      writebytes( buf, ((offset + 7) / 8) );
    }
    bytes_out += (offset + 7) / 8;
    offset = 0;
  }
}

void 
cl_block ()		/* table clear for block compress */
{
  register long int rat;

  checkpoint = in_count + CHECK_GAP;

  if (in_count > 0x007fffff) 
  {
    /* shift will overflow */

    rat = bytes_out >> 8;
    if (rat == 0) 
    { 
      /* Don't divide by zero */
      rat = 0x7fffffff;
    } 
    else 
    {
      rat = in_count / rat;
    }
   } 
   else 
   {
     rat = (in_count << 8) / bytes_out;	/* 8 fractional bits */
   }
   
   if (rat > ratio) 
   {
     ratio = rat;
   } 
   else 
   {
     ratio = 0;
     cl_hash ((count_int) hsize); 
     free_ent = FIRST;
     clear_flg = 1;
     output ((code_int) CLEAR);
   }
}

unsigned int 
getbyte ()
{
  if (InCnt > 0 && (apsim_InCnt-- > 0)) 
  {
    InCnt--;
    return (unsigned int)*InBuff++;
  } 
  else 
  {
    return -1;
  }
}

void 
compress ()
{
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
  free_ent = ((block_compress) ? (FIRST) : (256) );
  ent = getbyte ();
  hshift = 0;
  
  for (fcode = (long) hsize; fcode < 65536L; fcode *= 2L)
  {
    hshift++;
  }

  hshift = 8 - hshift;	/* set hash code range bound */
  hsize_reg = hsize;
  cl_hash( (count_int) hsize_reg); /* clear hash table */

  while (InCnt > 0)           /* apsim_loop 11 0 */
  {
    int apsim_bound111 = 0;
    c = getbyte();   /* decrements InCnt */

    in_count++;
    fcode = (long) (((long) c << maxbits) + ent);
    i     = ((c << hshift) ^ ent);

    if (htab[i] == fcode) 
    {
      ent = codetab[i];
      continue;
    } 
    else if ((long)htab[i] < 0 ) 
    {
      goto nomatch;
    }

    disp = hsize_reg - i;
    if (i == 0) 
    {
      disp = 1;
    }

probe:
    if ((i -= disp) < 0) 
    {  /* apsim_loop 111 11 */
      i += hsize_reg;
    }

    if (htab[i] == fcode) 
    {
      ent = codetab[i];
      continue;
    }

    if ((long) htab[i] > 0 && (++apsim_bound111 < in_count))
    {
      goto probe;
    }

nomatch:
    out_count++;
    ent = c;
    if (free_ent < maxmaxcode)
    {
      codetab[i] = free_ent++;	
      htab[i] = fcode;
    } 
    else if (((count_int)in_count >= checkpoint) && (block_compress))
    {
      cl_block ();
    }
  }
   
  if (bytes_out > in_count) 
  { 
    exit_stat = 2;
  }
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

  compress();
   
  return 0;
}

