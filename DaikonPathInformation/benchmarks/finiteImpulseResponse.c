#define TEST_VECTOR_LENGTH 720

long 
finiteImpulseResponse (long* in,long* out,long in_len, long* coef,long coef_len,long scale)
{
  #ifdef CBMC
  int __count_24_25 = 0;
  int __count_28_32 = 0;
  int __count_29_30 = 0;
  int __count_29_31 = 0;
  int __count_L33 = 0;
  int __count_L25 = 0;
  #endif
  long i,j,coef_len2,acc_length;
  long acc;
  long *in_ptr,*data_ptr,*coef_start,*coef_ptr,*in_end;

  /* set up for coefficients */
  coef_start = coef;
  coef_len2 = (coef_len + 1) >> 1;

  /* set up input data pointers */
  in_end = in + in_len - 1;
  in_ptr = in + coef_len2 - 1;

  /* initial value of accumulation length for startup */
  acc_length = coef_len2;

  for(i = 0 ; 
    #ifdef CBMC
    __count_L33++,
    #endif
  i < in_len ; i++) // 33
  {
    /* set up pointer for accumulation */
    data_ptr = in_ptr;
    coef_ptr = coef_start;

    /* do accumulation and write result with scale factor */
    acc = (long)(*coef_ptr++) * (*data_ptr--);
    for(j = 1 ; 
      #ifdef CBMC
      __count_L25++,
      #endif
    j < acc_length ; j++) // 25
    {
      acc += (long)(*coef_ptr++) * (*data_ptr--);
      #ifdef CBMC
      __count_24_25++;
      #endif
    }
    *out++ = (int)(acc/scale);

    /* check for end case */
    if(in_ptr == in_end) // 27
    {
      // 28
      acc_length--;       /* one shorter each time */
      coef_start++;       /* next coefficient each time */
      #ifdef CBMC
      __count_28_32++;
      #endif
    }
    else 
    {
      /* if not at end, then check for startup, add to input pointer */
      if(acc_length < coef_len) // 29
      {
        #ifdef CBMC
        __count_29_30++;
        #endif
        // 30
        acc_length++;
      }
      #ifdef CBMC
      else __count_29_31++;
      #endif
      in_ptr++;
    }
  }
  
  return *coef;
}

int
main (int argc, char *argv[])
{ 
  if (argc != TEST_VECTOR_LENGTH + 1)
  {
    return 1;
  }
  
  long in_data[TEST_VECTOR_LENGTH+1];
  int i;
  for (i = 0; i < argc - 1; ++i)
  {
    in_data[i] = (long) atoi (argv[i + 1]);
  }
  // Sentinel value needed
  in_data[TEST_VECTOR_LENGTH] = 0;
  
  long out_data[TEST_VECTOR_LENGTH+1];
  
  long fir_int[36]={
    0xfffffffe, 0x1, 0x4, 0x3, 0xfffffffe, 0xfffffffc, 0x2, 0x7, 0x0,
    0xfffffff7, 0xfffffffc, 0xc, 0xb, 0xfffffff2, 0xffffffe6, 0xf, 0x59, 0x7f,
    0x59, 0xf, 0xffffffe6, 0xfffffff2, 0xb, 0xc, 0xfffffffc, 0xfffffff7, 0x0,
    0x7, 0x2, 0xfffffffc, 0xfffffffe, 0x3, 0x4, 0x1, 0xfffffffe, 0};

  long val = finiteImpulseResponse (in_data, out_data, 700, fir_int, 35, 285); 
  
  printf("%ld", val);

  return 0;
}


