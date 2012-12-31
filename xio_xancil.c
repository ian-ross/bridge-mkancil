#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include "xio_xancil.h"

void (*xerror_func)(char *, va_list) = NULL;

int get_type(char *file)
{
    char cbuf[256];
    char *c;
    int j, fwi;
    int64 ibuf64[2], isub64;
    INTEGER ibuf[2], isub;
    INTEGER buf[4], swapbuf[4];
    REAL rbuf1, rbuf2, rbuf3;
    FILE *fp;

#if _FLT_TYPE == _IEEE4

    /* Fix Unaligned access problems, crashes on sgi, hp, warnings on alpha */

    REAL rbuf0[7], *rbuf;
    unsigned long ladd;
    char cadd[19];

    sprintf(cadd, "%p", rbuf0);
    ladd = strtoul(cadd, (char **) NULL, 16);

    if (8*(ladd/8) == ladd)
       rbuf = rbuf0;
    else
       rbuf = rbuf0+1;
#else
    REAL rbuf[6];
#endif

    /* Check for UM files */

    if ((fp = fopen(file, "rb")) == NULL)
    {
       fprintf(stderr,"Error opening file %s \n",file);
       perror("fopen error");
       return 0;
    }

    fread(buf, 1, 16, fp);

    if (fclose(fp) != 0)
    {
       fprintf(stderr,"Error closing file %s \n",file);
       return 0;
    }

    /* Check for 32 bit ieee UM file */

#if _INT_TYPE == _CRAY8
    i4_to_c8(buf, ibuf, 2);
    isub = ibuf[1];
#elif _INT_TYPE == _IEEE8
    i4_to_i8(buf, ibuf, 2);
    isub = ibuf[1];
#else
    isub = buf[1];
#endif

#ifdef DEBUG
#if INTEGER == int
    printf("isub = %d \n",isub);
#elif INTEGER == long
    printf("isub = %ld \n",isub);
#elif INTEGER == long long
    printf("isub = %lld \n",isub);
#endif
#endif

    if (isub >= 1 && isub <= 4) 
    {
       printf("file %s is a 32 bit ieee um file \n",file);
       return UMFILE;
    }

    /* Check for byte swapped 32 bit ieee UM file */

    memcpy(swapbuf, buf, 8);
    swap_bytes(swapbuf,4,2);

#if _INT_TYPE == _CRAY8
    i4_to_c8(swapbuf, ibuf, 2);
    isub = ibuf[1];
#elif _INT_TYPE == _IEEE8
    i4_to_i8(swapbuf, ibuf, 2);
    isub = ibuf[1];
#else
    isub = swapbuf[1];
#endif

#ifdef DEBUG
#if INTEGER == int
    printf("isub = %d \n",isub);
#elif INTEGER == long
    printf("isub = %ld \n",isub);
#elif INTEGER == long long
    printf("isub = %lld \n",isub);
#endif
#endif

    if (isub >= 1 && isub <= 4) 
    {
       printf("file %s is a byte swapped 32 bit ieee um file \n",file);
       return UMFILEBS;
    }

    /* Check for 64 bit ieee or CRAY UM file */

#if _INT_TYPE == _CRAY8
    isub64 = buf[1];
#elif _INT_TYPE == _IEEE8
    isub64 = buf[1];
#else
    isub64 = (int64) *((int64 *) (buf+2));
#endif

#ifdef DEBUG
#if defined INT64
    printf("isub64 = %d \n",isub64);
#elif defined LONG64
    printf("isub64 = %ld \n",isub64);
#else
    printf("isub64 = %lld \n",isub64);
#endif
#endif

    if (isub64 >= 1 && isub64 <= 4) 
    {
       if ((fp = fopen(file, "rb")) == NULL)
       {
          fprintf(stderr,"Error opening file %s \n",file);
          perror("fopen error");
          return 0;
       }

       xfseek(fp, 832, SEEK_SET);
       fread(ibuf64, 8, 1, fp);
       xfseek(fp, 8*(ibuf64[0] + 1), SEEK_SET);
       fread(rbuf, 8, 3, fp);

       if (fclose(fp) != 0)
       {
          fprintf(stderr,"Error closing file %s \n",file);
          return 0;
       }

#ifdef BIG__ENDIAN
#if _FLT_TYPE == _CRAY8
       rbuf1 = rbuf[0];
       rbuf2 = rbuf[1];
       rbuf3 = rbuf[2];
#elif _FLT_TYPE == _IEEE8
       if (c8_to_r8(rbuf, &rbuf1, 1) != 0) rbuf1 = -999.999;
       if (c8_to_r8(rbuf+1, &rbuf2, 1) != 0) rbuf2 = -999.999;
       if (c8_to_r8(rbuf+2, &rbuf3, 1) != 0) rbuf3 = -999.999;
#else
       if (c8_to_r4(rbuf, &rbuf1, 1) != 0) rbuf1 = -999.999;
       if (c8_to_r4(rbuf+2, &rbuf2, 1) != 0) rbuf2 = -999.999;
       if (c8_to_r4(rbuf+4, &rbuf3, 1) != 0) rbuf3 = -999.999;
#endif
#ifdef DEBUG
       printf("rbuf1 = %f \n",rbuf1);
       printf("rbuf2 = %f \n",rbuf2);
       printf("rbuf3 = %f \n",rbuf3);
#endif
       if ((rbuf1 >= -180.0 && rbuf1 <= 180.0)  &&
           (rbuf2 >= -360.0 && rbuf2 <= 360.0) &&
           (rbuf3 >= -180.0 && rbuf3 <= 180.0))
       {
          printf("file %s is a Cray um file \n",file);
          return UMFILECRAY;
       }
#endif

#if _FLT_TYPE == _CRAY8
       if (r8_to_c8(rbuf, &rbuf1, 1) != 0) rbuf1 = -999.999;
       if (r8_to_c8(rbuf+1, &rbuf2, 1) != 0) rbuf2 = -999.999;
       if (r8_to_c8(rbuf+2, &rbuf3, 1) != 0) rbuf3 = -999.999;
#elif _FLT_TYPE == _IEEE8
       rbuf1 = rbuf[0];
       rbuf2 = rbuf[1];
       rbuf3 = rbuf[2];
#else
       if (r8_to_r4(rbuf, &rbuf1, 1) != 0) rbuf1 = -999.999;
       if (r8_to_r4(rbuf+2, &rbuf2, 1) != 0) rbuf2 = -999.999;
       if (r8_to_r4(rbuf+4, &rbuf3, 1) != 0) rbuf3 = -999.999;
#endif
#ifdef DEBUG
       printf("rbuf1 = %f \n",rbuf1);
       printf("rbuf2 = %f \n",rbuf2);
       printf("rbuf3 = %f \n",rbuf3);
#endif
       if ((rbuf1 >= -180.0 && rbuf1 <= 180.0)  &&
           (rbuf2 >= -360.0 && rbuf2 <= 360.0) &&
           (rbuf3 >= -180.0 && rbuf3 <= 180.0))
       {
          printf("file %s is a 64 bit ieee um file \n",file);
          return UMFILE64;
       }
    }

    /* Check for byte swapped 64 bit ieee or CRAY UM file */

    memcpy(swapbuf, buf, 16);
    swap_bytes(swapbuf,8,2);

#if _INT_TYPE == _CRAY8
    isub64 = swapbuf[1];
#elif _INT_TYPE == _IEEE8
    isub64 = swapbuf[1];
#else
    isub64 = (int64) *((int64 *) (swapbuf+2));
#endif

#ifdef DEBUG
#if defined INT64
    printf("isub64 = %d \n",isub64);
#elif defined LONG64
    printf("isub64 = %ld \n",isub64);
#else
    printf("isub64 = %lld \n",isub64);
#endif
#endif

    if (isub64 >= 1 && isub64 <= 4) 
    {
       if ((fp = fopen(file, "rb")) == NULL)
       {
          fprintf(stderr,"Error opening file %s \n",file);
          perror("fopen error");
          return 0;
       }

       xfseek(fp, 832, SEEK_SET);
       fread(ibuf64, 8, 1, fp);
       swap_bytes(ibuf64,8,1);
       xfseek(fp, 8*(ibuf64[0] + 1), SEEK_SET);
       fread(rbuf, 8, 3, fp);

       if (fclose(fp) != 0)
       {
          fprintf(stderr,"Error closing file %s \n",file);
          return 0;
       }

#ifdef LITTLE__ENDIAN
#if _FLT_TYPE == _CRAY8
       rbuf1 = rbuf[0];
       rbuf2 = rbuf[1];
       rbuf3 = rbuf[2];
#elif _FLT_TYPE == _IEEE8
       if (c8_to_r8(rbuf, &rbuf1, 1) != 0) rbuf1 = -999.999;
       if (c8_to_r8(rbuf+1, &rbuf2, 1) != 0) rbuf2 = -999.999;
       if (c8_to_r8(rbuf+2, &rbuf3, 1) != 0) rbuf3 = -999.999;
#else
       if (c8_to_r4(rbuf, &rbuf1, 1) != 0) rbuf1 = -999.999;
       if (c8_to_r4(rbuf+2, &rbuf2, 1) != 0) rbuf2 = -999.999;
       if (c8_to_r4(rbuf+4, &rbuf3, 1) != 0) rbuf3 = -999.999;
#endif
#ifdef DEBUG
       printf("rbuf1 = %f \n",rbuf1);
       printf("rbuf2 = %f \n",rbuf2);
       printf("rbuf3 = %f \n",rbuf3);
#endif
       if ((rbuf1 >= -180.0 && rbuf1 <= 180.0)  &&
           (rbuf2 >= -360.0 && rbuf2 <= 360.0) &&
           (rbuf3 >= -180.0 && rbuf3 <= 180.0))
       {
          printf("file %s is a Cray um file \n",file);
          return UMFILECRAY;
       }
#endif

       swap_bytes(rbuf,8,3);
#if _FLT_TYPE == _CRAY8
       if (r8_to_c8(rbuf, &rbuf1, 1) != 0) rbuf1 = -999.999;
       if (r8_to_c8(rbuf+1, &rbuf2, 1) != 0) rbuf2 = -999.999;
       if (r8_to_c8(rbuf+2, &rbuf3, 1) != 0) rbuf3 = -999.999;
#elif _FLT_TYPE == _IEEE8
       rbuf1 = rbuf[0];
       rbuf2 = rbuf[1];
       rbuf3 = rbuf[2];
#else
       if (r8_to_r4(rbuf, &rbuf1, 1) != 0) rbuf1 = -999.999;
       if (r8_to_r4(rbuf+2, &rbuf2, 1) != 0) rbuf2 = -999.999;
       if (r8_to_r4(rbuf+4, &rbuf3, 1) != 0) rbuf3 = -999.999;
#endif
#ifdef DEBUG
       printf("rbuf1 = %f \n",rbuf1);
       printf("rbuf2 = %f \n",rbuf2);
       printf("rbuf3 = %f \n",rbuf3);
#endif
       if ((rbuf1 >= -180.0 && rbuf1 <= 180.0)  &&
           (rbuf2 >= -360.0 && rbuf2 <= 360.0) &&
           (rbuf3 >= -180.0 && rbuf3 <= 180.0))
       {
          printf("file %s is a byte swapped 64 bit ieee um file \n",file);
          return UMFILE64BS;
       }
    }

    /* Check for PP files */

#if _INT_TYPE == _CRAY8
    i4_to_c8(buf, ibuf, 2);
#elif _INT_TYPE == _IEEE8
    i4_to_i8(buf, ibuf, 2);
#else
    ibuf[0] = buf[0];
    ibuf[1] = buf[1];
#endif

#ifdef DEBUG
#if INTEGER == int
    printf("ibuf[0] = %d \n",ibuf[0]);
    printf("ibuf[1] = %d \n",ibuf[1]);
#elif INTEGER == long
    printf("ibuf[0] = %ld \n",ibuf[0]);
    printf("ibuf[1] = %ld \n",ibuf[1]);
#elif INTEGER == long long
    printf("ibuf[0] = %lld \n",ibuf[0]);
    printf("ibuf[1] = %lld \n",ibuf[1]);
#endif
#endif

    if (ibuf[0] == 256 && ibuf[1] >= 0 && ibuf[1] <= 10000) 
    {
       printf("file %s is a 32 bit ieee pp file \n",file);
       return PPFILE;
    }

    memcpy(swapbuf, buf, 8);
    swap_bytes(swapbuf,4,2);

#if _INT_TYPE == _CRAY8
    i4_to_c8(swapbuf, ibuf, 2);
#elif _INT_TYPE == _IEEE8
    i4_to_i8(swapbuf, ibuf, 2);
#else
    ibuf[0] = swapbuf[0];
    ibuf[1] = swapbuf[1];
#endif

#ifdef DEBUG
#if INTEGER == int
    printf("ibuf[0] = %d \n",ibuf[0]);
    printf("ibuf[1] = %d \n",ibuf[1]);
#elif INTEGER == long
    printf("ibuf[0] = %ld \n",ibuf[0]);
    printf("ibuf[1] = %ld \n",ibuf[1]);
#elif INTEGER == long long
    printf("ibuf[0] = %lld \n",ibuf[0]);
    printf("ibuf[1] = %lld \n",ibuf[1]);
#endif
#endif

    if (ibuf[0] == 256 && ibuf[1] >= 0 && ibuf[1] <= 10000) 
    {
       printf("file %s is a byte swapped 32 bit ieee pp file \n",file);
       return PPFILEBS;
    }

#if _INT_TYPE == _CRAY8
    i4_to_c8(buf, ibuf, 1);
    ibuf64[0] = (buf[0] << 32) && (~(~ZERO64 << 32) && (buf[1] >> 32));
#elif _INT_TYPE == _IEEE8
    i4_to_i8(buf, ibuf, 1);
    ibuf64[0] = (buf[0] << 32) && (~(~ZERO64 << 32) && (buf[1] >> 32));
#else
    ibuf[0] = buf[0];
    /* 
       statement below doesn't work on fujitsu, so do alternative
       ibuf64[0] = (int64) *((int64 *) (buf+1)); 
    */
    ibuf64[0] = (int64) *((int64 *) (buf));
    ibuf64[1] = (int64) *((int64 *) (buf+2));
    ibuf64[0] = (ibuf64[0] << 32) && (~(~ZERO64 << 32) && (ibuf64[1] >> 32));
#endif

#ifdef DEBUG
#if INTEGER == int
    printf("ibuf[0] = %d \n",ibuf[0]);
#elif INTEGER == long
    printf("ibuf[0] = %ld \n",ibuf[0]);
#elif INTEGER == long long
    printf("ibuf[0] = %lld \n",ibuf[0]);
#endif
#if defined INT64
    printf("ibuf64[0] = %d \n",ibuf64[0]);
#elif defined LONG64
    printf("ibuf64[0] = %ld \n",ibuf64[0]);
#else
    printf("ibuf64[0] = %lld \n",ibuf64[0]);
#endif
#endif

    if (ibuf[0] == 512 && ibuf64[0] >= 0 && ibuf64[0] <= 10000) 
    {
       printf("file %s is a 64 bit ieee pp file \n",file);
       return PPFILE64;
    }

    memcpy(swapbuf, buf, 4);
    swap_bytes(swapbuf,4,1);
    swap_bytes(ibuf64,8,1);

#if _INT_TYPE == _CRAY8
    i4_to_c8(swapbuf, ibuf, 1);
#elif _INT_TYPE == _IEEE8
    i4_to_i8(swapbuf, ibuf, 1);
#else
    ibuf[0] = swapbuf[0];
#endif

#ifdef DEBUG
#if INTEGER == int
    printf("ibuf[0] = %d \n",ibuf[0]);
#elif INTEGER == long
    printf("ibuf[0] = %ld \n",ibuf[0]);
#elif INTEGER == long long
    printf("ibuf[0] = %lld \n",ibuf[0]);
#endif
#if defined INT64
    printf("ibuf64[0] = %d \n",ibuf64[0]);
#elif defined LONG64
    printf("ibuf64[0] = %ld \n",ibuf64[0]);
#else
    printf("ibuf64[0] = %lld \n",ibuf64[0]);
#endif
#endif

    if (ibuf[0] == 512 && ibuf64[0] >= 0 && ibuf64[0] <= 10000) 
    {
       printf("file %s is a byte swapped 64 bit ieee pp file \n",file);
       return PPFILE64BS;
    }

#if _INT_TYPE == _CRAY8
    ibuf64[0] = buf[0];
    ibuf64[1] = buf[1];
#elif _INT_TYPE == _IEEE8
    ibuf64[0] = buf[0];
    ibuf64[1] = buf[1];
#else
    ibuf64[0] = (int64) *((int64 *) (buf));
    ibuf64[1] = (int64) *((int64 *) (buf+2));
#endif
    fwi = ibuf64[0] & 0x1ff;

#ifdef DEBUG
    printf("fwi = %d \n",fwi);
#if defined INT64
    printf("ibuf64[0] = %d \n",ibuf64[0]);
    printf("ibuf64[1] = %d \n",ibuf64[1]);
#elif defined LONG64
    printf("ibuf64[0] = %ld \n",ibuf64[0]);
    printf("ibuf64[1] = %ld \n",ibuf64[1]);
#else
    printf("ibuf64[0] = %lld \n",ibuf64[0]);
    printf("ibuf64[1] = %lld \n",ibuf64[1]);
#endif
#endif

#ifdef BIG__ENDIAN
    if (fwi == 64 && ibuf64[1] >= 0 && ibuf64[1] <= 10000) 
    {
       if ((fp = fopen(file, "rb")) == NULL)
       {
          fprintf(stderr,"Error opening file %s \n",file);
          perror("fopen error");
          return 0;
       }

       xfseek(fp, 448, SEEK_SET);
       fread(rbuf, 8, 2, fp);

       if (fclose(fp) != 0)
       {
          fprintf(stderr,"Error closing file %s \n",file);
          return 0;
       }

#if _FLT_TYPE == _CRAY8
       rbuf1 = rbuf[0];
       rbuf2 = rbuf[1];
#elif _FLT_TYPE == _IEEE8
       if (c8_to_r8(rbuf, &rbuf1, 1) != 0) rbuf1 = -999.999;
       if (c8_to_r8(rbuf+1, &rbuf2, 1) != 0) rbuf2 = -999.999;
#else
       if (c8_to_r4(rbuf, &rbuf1, 1) != 0) rbuf1 = -999.999;
       if (c8_to_r4(rbuf+2, &rbuf2, 1) != 0) rbuf2 = -999.999;
#endif
#ifdef DEBUG
       printf("rbuf1 = %f \n",rbuf1);
       printf("rbuf2 = %f \n",rbuf2);
#endif
       if ((rbuf1 >= -180.0 && rbuf1 <= 180.0) &&
           (rbuf2 >= -360.0 && rbuf2 <= 360.0))
       {
          printf("file %s is a Cray blocked Cray pp file \n",file);
          return PPFILECRAY;
       }

#if _FLT_TYPE == _CRAY8
       if (r8_to_c8(rbuf, &rbuf1, 1) != 0) rbuf1 = -999.999;
       if (r8_to_c8(rbuf+1, &rbuf2, 1) != 0) rbuf2 = -999.999;
#elif _FLT_TYPE == _IEEE8
       rbuf1 = rbuf[0];
       rbuf2 = rbuf[1];
#else
       if (r8_to_r4(rbuf, &rbuf1, 1) != 0) rbuf1 = -999.999;
       if (r8_to_r4(rbuf+2, &rbuf2, 1) != 0) rbuf2 = -999.999;
#endif
#ifdef DEBUG
       printf("rbuf1 = %f \n",rbuf1);
       printf("rbuf2 = %f \n",rbuf2);
#endif
       if ((rbuf1 >= -180.0 && rbuf1 <= 180.0) &&
           (rbuf2 >= -360.0 && rbuf2 <= 360.0))
       {
          printf("file %s is a Cray blocked 64 bit ieee pp file \n",file);
          return PPFILECRAYIEEE;
       }
    }
#else

    swap_bytes(ibuf64,8,2);
    fwi =  ibuf64[0] & 0x1ff;

#ifdef DEBUG
    printf("fwi = %d \n",fwi);
#if defined INT64
    printf("ibuf64[0] = %d \n",ibuf64[0]);
    printf("ibuf64[1] = %d \n",ibuf64[1]);
#elif defined LONG64
    printf("ibuf64[0] = %ld \n",ibuf64[0]);
    printf("ibuf64[1] = %ld \n",ibuf64[1]);
#else
    printf("ibuf64[0] = %lld \n",ibuf64[0]);
    printf("ibuf64[1] = %lld \n",ibuf64[1]);
#endif
#endif

    if (fwi == 64 && ibuf64[1] >= 0 && ibuf64[1] <= 10000) 
    {
       if ((fp = fopen(file, "rb")) == NULL)
       {
          fprintf(stderr,"Error opening file %s \n",file);
          perror("fopen error");
          return 0;
       }

       xfseek(fp, 448, SEEK_SET);
       fread(rbuf, 8, 2, fp);

       if (fclose(fp) != 0)
       {
          fprintf(stderr,"Error closing file %s \n",file);
          return 0;
       }

#if _FLT_TYPE == _CRAY8
       rbuf1 = rbuf[0];
       rbuf2 = rbuf[1];
#elif _FLT_TYPE == _IEEE8
       if (c8_to_r8(rbuf, &rbuf1, 1) != 0) rbuf1 = -999.999;
       if (c8_to_r8(rbuf+1, &rbuf2, 1) != 0) rbuf2 = -999.999;
#else
       if (c8_to_r4(rbuf, &rbuf1, 1) != 0) rbuf1 = -999.999;
       if (c8_to_r4(rbuf+2, &rbuf2, 1) != 0) rbuf2 = -999.999;
#endif
#ifdef DEBUG
       printf("rbuf1 = %f \n",rbuf1);
       printf("rbuf2 = %f \n",rbuf2);
#endif
       if ((rbuf1 >= -180.0 && rbuf1 <= 180.0) &&
           (rbuf2 >= -360.0 && rbuf2 <= 360.0))
       {
          printf("file %s is a Cray blocked Cray pp file \n",file);
          return PPFILECRAY;
       }

       swap_bytes(rbuf,8,2);
#if _FLT_TYPE == _CRAY8
       if (r8_to_c8(rbuf, &rbuf1, 1) != 0) rbuf1 = -999.999;
       if (r8_to_c8(rbuf+1, &rbuf2, 1) != 0) rbuf2 = -999.999;
#elif _FLT_TYPE == _IEEE8
       rbuf1 = rbuf[0];
       rbuf2 = rbuf[1];
#else
       if (r8_to_r4(rbuf, &rbuf1, 1) != 0) rbuf1 = -999.999;
       if (r8_to_r4(rbuf+2, &rbuf2, 1) != 0) rbuf2 = -999.999;
#endif
#ifdef DEBUG
       printf("rbuf1 = %f \n",rbuf1);
       printf("rbuf2 = %f \n",rbuf2);
#endif
       if ((rbuf1 >= -180.0 && rbuf1 <= 180.0) &&
           (rbuf2 >= -360.0 && rbuf2 <= 360.0))
       {
          printf("file %s is a Cray blocked 64 bit ieee pp file \n",file);
          return PPFILECRAYIEEE;
       }
    }
#endif

    if (fclose(fp) != 0)
    {
       fprintf(stderr,"Error closing file %s \n",file);
       return 0;
    }

    printf("Error file %s has unknown file type \n",file);
    return 0;
}

#if _INT_SIZE == 8
int xreadint(FILEINFO *finfo, INTEGER *buf, int n)
{
    int num, iret, filetype, n1, n2;
    INTEGER *buf1;

    if (n == 0) 
       return 0;
    else if (n < 0) 
       return 1;

    filetype = finfo->ftype;

    if (filetype == UMFILE64 || filetype == UMFILE64BS || 
        filetype == UMFILECRAY)
    {
       num = fread(buf, 8, n, finfo->fid->fp);
       if (filetype == UMFILE64BS) swap_bytes(buf,8,n);
       if (num == n)
          iret = 0;
       else if (feof(finfo->fid->fp))
          iret = -1;
       else
          iret = 1;
    }
    else if (filetype == UMFILE || filetype == UMFILEBS)
    {
       n1 = (n+1)/2;
       if ( (buf1 = malloc (n1*8)) == NULL )
       {
          if (n1 != 0)
             xerror("Error unable to allocate memory for buf1 in xreadint n1 = %d \n", n1);
       }

       num = fread(buf1, 4, n, finfo->fid->fp);
       if (filetype == UMFILEBS) 
          swap_bytes(buf1,4,n);
       if (num == n)
          iret = 0;
       else if (feof(finfo->fid->fp))
          iret = -1;
       else
          iret = 1;

#if _INT_TYPE == _IEEE8
       i4_to_i8(buf1, buf, n);
#else
       i4_to_c8(buf1, buf, n);
#endif

       buf1 = free_x (buf1);
    }
    else if (filetype == PPFILECRAY || filetype == PPFILECRAYIEEE)
    {
       n1 = n*8;
       iret = cos_read(finfo->fid->cosid->cosfp, buf, n1, &n2);
       if (iret == 0 || iret == -3) 
          (finfo->fid->cosid->pos)++;
       else if (iret == -1)
          return -1;
#ifdef LITTLE__ENDIAN
       swap_bytes(buf,8,n);
#endif
    }
    else if (filetype == PPFILE || filetype == PPFILEBS || 
             filetype == PPFILEI32R64)
    {
       n1 = (n+1)/2;
       if ( (buf1 = malloc (n1*8)) == NULL )
       {
          if (n1 != 0)
             xerror("Error unable to allocate memory for buf1 in xreadint n1 = %d \n", n1);
       }

       iret = ppread(finfo->fid->ppid, buf1, n1*8, &n2);

#if _INT_TYPE == _IEEE8
       i4_to_i8(buf1, buf, n);
#else
       i4_to_c8(buf1, buf, n);
#endif

       buf1 = free_x (buf1);
    }
    else if (filetype == PPFILE64 || filetype == PPFILE64BS)
    {
       iret = ppread(finfo->fid->ppid, buf, n*8, &n2);
    }
    else
       iret = 1;

    return iret;
}
#else
int xreadint(FILEINFO *finfo, INTEGER *buf, int n)
{
    int num, iret, filetype, n1, n2;
    int64 *buf64;

    if (n == 0) 
       return 0;
    else if (n < 0) 
       return 1;

    filetype = finfo->ftype;

    if (filetype == UMFILE || filetype == UMFILEBS)
    {
       num = fread(buf, 4, n, finfo->fid->fp);
       if (filetype == UMFILEBS) 
          swap_bytes(buf,4,n);
       if (num == n)
          iret = 0;
       else if (feof(finfo->fid->fp))
          iret = -1;
       else
          iret = 1;
    }
    else if (filetype == UMFILE64 || filetype == UMFILE64BS || 
             filetype == UMFILECRAY)
    {
       if ( (buf64 = malloc (n*8)) == NULL )
       {
          if (n != 0)
             xerror("Error unable to allocate memory for buf64 in xreadint n = %d \n", n);
       }

       num = fread(buf64, 8, n, finfo->fid->fp);
       if (filetype == UMFILE64BS) swap_bytes(buf64,8,n);
       if (num == n)
          iret = 0;
       else if (feof(finfo->fid->fp))
          iret = -1;
       else
          iret = 1;

       if (filetype == UMFILE64 || filetype == UMFILE64BS)
          i8_to_i4(buf64, buf, n);
       else if (filetype == UMFILECRAY)
          c8_to_i4(buf64, buf, n);

       buf64 = free_x (buf64);
    }
    else if (filetype == PPFILE || filetype == PPFILEBS || 
             filetype == PPFILEI32R64)
    {
       iret = ppread(finfo->fid->ppid, buf, n*4, &n2);
    }
    else if (filetype == PPFILE64 || filetype == PPFILE64BS || 
             filetype == PPFILECRAY || filetype == PPFILECRAYIEEE)
    {
       if ( (buf64 = malloc (n*8)) == NULL )
       {
          if (n != 0)
             xerror("Error unable to allocate memory for buf64 in xreadint n = %d \n", n);
       }

       if (filetype == PPFILE64 || filetype == PPFILE64BS)
       {
          iret = ppread(finfo->fid->ppid, buf64, n*8, &n2);
          i8_to_i4(buf64, buf, n);
       }
       else if (filetype == PPFILECRAY)
       {
          n1 = n*8;
          iret = cos_read(finfo->fid->cosid->cosfp, buf64, n1, &n2);
          if (iret == 0 || iret == -3) 
             (finfo->fid->cosid->pos)++;
          else if (iret == -1)
             return -1;
          c8_to_i4(buf64, buf, n);
       }
       else if (filetype == PPFILECRAYIEEE)
       {
          n1 = n*8;
          iret = cos_read(finfo->fid->cosid->cosfp, buf64, n1, &n2);
          if (iret == 0 || iret == -3) 
             (finfo->fid->cosid->pos)++;
          else if (iret == -1)
             return -1;
#ifdef LITTLE__ENDIAN
          swap_bytes(buf64,8,n);
#endif
          i8_to_i4(buf64, buf, n);
       }

       buf64 = free_x (buf64);
    }
    else
       iret = 1;

    return iret;
}
#endif

#if _FLT_SIZE == 8
int xreadreal(FILEINFO *finfo, REAL *buf, int n)
{
    int num, iret, filetype, n1, n2;
    REAL *buf1;

    if (n == 0) 
       return 0;
    else if (n < 0) 
       return 1;

    filetype = finfo->ftype;

    if (filetype == UMFILECRAY)
    {
#if _FLT_TYPE == _IEEE8
       if ( (buf1 = malloc (n*8)) == NULL )
       {
          if (n != 0)
             xerror("Error unable to allocate memory for buf1 in xreadreal n = %d \n", n);
       }
       num = fread(buf1, 8, n, finfo->fid->fp);
       c8_to_r8(buf1, buf, n);
       buf1 = free_x (buf1);
#else
       num = fread(buf, 8, n, finfo->fid->fp);
#endif
       if (num == n)
          iret = 0;
       else if (feof(finfo->fid->fp))
          iret = -1;
       else
          iret = 1;
    }
    else if (filetype == UMFILE64 || filetype == UMFILE64BS)
    {
#if _FLT_TYPE == _CRAY8
       if ( (buf1 = malloc (n*8)) == NULL )
       {
          if (n != 0)
             xerror("Error unable to allocate memory for buf1 in xreadreal n = %d \n", n);
       }
       num = fread(buf1, 8, n, finfo->fid->fp);
       if (filetype == UMFILE64BS) swap_bytes(buf1,8,n);
       r8_to_c8(buf1, buf, n);
       buf1 = free_x (buf1);
#else
       num = fread(buf, 8, n, finfo->fid->fp);
       if (filetype == UMFILE64BS) swap_bytes(buf,8,n);
#endif
       if (num == n)
          iret = 0;
       else if (feof(finfo->fid->fp))
          iret = -1;
       else
          iret = 1;
    }
    else if (filetype == UMFILE || filetype == UMFILEBS)
    {
       n1 = (n+1)/2;
       if ( (buf1 = malloc (n1*8)) == NULL )
       {
          if (n1 != 0)
             xerror("Error unable to allocate memory for buf1 in xreadreal n1 = %d \n", n1);
       }
       num = fread(buf1, 4, n, finfo->fid->fp);
       if (filetype == UMFILEBS) 
          swap_bytes(buf1,4,n);
       if (num == n)
          iret = 0;
       else if (feof(finfo->fid->fp))
          iret = -1;
       else
          iret = 1;

#if _FLT_TYPE == _IEEE8
       r4_to_r8(buf1, buf, n);
#else
       r4_to_c8(buf1, buf, n);
#endif
       buf1 = free_x (buf1);
    }
    else if (filetype == PPFILECRAY)
    {
       n1 = n*8;
#if _FLT_TYPE == _IEEE8
       if ( (buf1 = malloc (n*8)) == NULL )
       {
          if (n != 0)
             xerror("Error unable to allocate memory for buf1 in xreadreal n = %d \n", n);
       }
       iret = cos_read(finfo->fid->cosid->cosfp, buf1, n1, &n2);
       if (iret == 0 || iret == -3) 
          (finfo->fid->cosid->pos)++;
       else if (iret == -1)
          return -1;
       c8_to_r8(buf1, buf, n);
       buf1 = free_x (buf1);
#else
       iret = cos_read(finfo->fid->cosid->cosfp, buf, n1, &n2);
       if (iret == 0 || iret == -3) 
          (finfo->fid->cosid->pos)++;
       else if (iret == -1)
          return -1;
#endif
    }
    else if (filetype == PPFILECRAYIEEE)
    {
       n1 = n*8;
#if _FLT_TYPE == _CRAY8
       if ( (buf1 = malloc (n*8)) == NULL )
       {
          if (n != 0)
             xerror("Error unable to allocate memory for buf1 in xreadreal n = %d \n", n);
       }
       iret = cos_read(finfo->fid->cosid->cosfp, buf1, n1, &n2);
       if (iret == 0 || iret == -3) 
          (finfo->fid->cosid->pos)++;
       else if (iret == -1)
          return -1;
       r8_to_c8(buf1, buf, n);
       buf1 = free_x (buf1);
#else
       iret = cos_read(finfo->fid->cosid->cosfp, buf, n1, &n2);
       if (iret == 0 || iret == -3) 
          (finfo->fid->cosid->pos)++;
       else if (iret == -1)
          return -1;
#ifdef LITTLE__ENDIAN
       swap_bytes(buf,8,n);
#endif
#endif
    }
    else if (filetype == PPFILE64 || filetype == PPFILE64BS || 
             filetype == PPFILEI32R64)
    {
#if _FLT_TYPE == _CRAY8
       if ( (buf1 = malloc (n*8)) == NULL )
       {
          if (n != 0)
             xerror("Error unable to allocate memory for buf1 in xreadreal n = %d \n", n);
       }
       iret = ppread(finfo->fid->ppid, buf1, n*8, &n2);
       r8_to_c8(buf1, buf, n);
       buf1 = free_x (buf1);
#else
       iret = ppread(finfo->fid->ppid, buf, n*8, &n2);
#endif
    }
    else if (filetype == PPFILE || filetype == PPFILEBS)
    {
       n1 = (n+1)/2;
       if ( (buf1 = malloc (n1*8)) == NULL )
       {
          if (n1 != 0)
             xerror("Error unable to allocate memory for buf1 in xreadreal n1 = %d \n", n1);
       }
       iret = ppread(finfo->fid->ppid, buf1, n1*8, &n2);

#if _FLT_TYPE == _IEEE8
       r4_to_r8(buf1, buf, n);
#else
       r4_to_c8(buf1, buf, n);
#endif
       buf1 = free_x (buf1);
    }
    else
       iret = 1;

    return iret;
}
#else
int xreadreal(FILEINFO *finfo, REAL *buf, int n)
{
    int num, iret, filetype, n1, n2;
    float64 *buf64;

    if (n == 0) 
       return 0;
    else if (n < 0) 
       return 1;

    filetype = finfo->ftype;

    if (filetype == UMFILE || filetype == UMFILEBS)
    {
       num = fread(buf, 4, n, finfo->fid->fp);
       if (filetype == UMFILEBS) 
          swap_bytes(buf,4,n);
       if (num == n)
          iret = 0;
       else if (feof(finfo->fid->fp))
          iret = -1;
       else
          iret = 1;
    }
    else if (filetype == UMFILE64 || filetype == UMFILE64BS || 
             filetype == UMFILECRAY)
    {
       if ( (buf64 = malloc (n*8)) == NULL )
       {
          if (n != 0)
             xerror("Error unable to allocate memory for buf64 in xreadreal n = %d \n", n);
       }

       num = fread(buf64, 8, n, finfo->fid->fp);
       if (filetype == UMFILE64BS) swap_bytes(buf64,8,n);
       if (num == n)
          iret = 0;
       else if (feof(finfo->fid->fp))
          iret = -1;
       else
          iret = 1;

       if (filetype == UMFILE64 || filetype == UMFILE64BS)
          r8_to_r4(buf64, buf, n);
       else if (filetype == UMFILECRAY)
          c8_to_r4(buf64, buf, n);

       buf64 = free_x (buf64);
    }
    else if (filetype == PPFILE || filetype == PPFILEBS)
    {
       iret = ppread(finfo->fid->ppid, buf, n*4, &n2);
    }
    else if (filetype == PPFILE64 || filetype == PPFILE64BS || 
             filetype == PPFILECRAY || filetype == PPFILECRAYIEEE || 
             filetype == PPFILEI32R64)
    {
       if ( (buf64 = malloc (n*8)) == NULL )
       {
          if (n != 0)
             xerror("Error unable to allocate memory for buf64 in xreadreal n = %d \n", n);
       }

       if (filetype == PPFILE64 || filetype == PPFILE64BS || 
           filetype == PPFILEI32R64)
       {
          iret = ppread(finfo->fid->ppid, buf64, n*8, &n2);
          r8_to_r4(buf64, buf, n);
       }
       else if (filetype == PPFILECRAY)
       {
          n1 = n*8;
          iret = cos_read(finfo->fid->cosid->cosfp, buf64, n1, &n2);
          if (iret == 0 || iret == -3) 
             (finfo->fid->cosid->pos)++;
          else if (iret == -1)
             return -1;
          c8_to_r4(buf64, buf, n);
       }
       else if (filetype == PPFILECRAYIEEE)
       {
          n1 = n*8;
          iret = cos_read(finfo->fid->cosid->cosfp, buf64, n1, &n2);
          if (iret == 0 || iret == -3) 
             (finfo->fid->cosid->pos)++;
          else if (iret == -1)
             return -1;
#ifdef LITTLE__ENDIAN
          swap_bytes(buf64,8,n);
#endif
          r8_to_r4(buf64, buf, n);
       }

       buf64 = free_x (buf64);
    }
    else
       iret = 1;

    return iret;
}
#endif

int xreadpack(FILEINFO *finfo, int *buf, int n)
{
    int num, iret, filetype, len1;
    long len;

    if (n == 0) 
       return 0;
    else if (n < 0) 
       return 1;

    filetype = finfo->ftype;

    if (filetype == UMFILE || filetype == UMFILE64 || filetype == UMFILECRAY ||
        filetype == UMFILEBS || filetype == UMFILE64BS)
    {
       num = fread(buf, sizeof(int), n, finfo->fid->fp);
       if (num == n)
          iret = 0;
       else if (feof(finfo->fid->fp))
          iret = -1;
       else
          iret = 1;
    }
    else if (filetype == PPFILE || filetype == PPFILE64 || 
             filetype == PPFILEBS || filetype == PPFILE64BS || 
             filetype == PPFILEI32R64)
    {
       iret = ppreadpack(finfo->fid->ppid, buf, n*sizeof(int), &len1);
    }
    else if (filetype == PPFILECRAY || filetype == PPFILECRAYIEEE)
    {
       len = n*sizeof(int);
       iret = cos_read(finfo->fid->cosid->cosfp, buf, len, &len1);
       if (iret == 0 || iret == -3) 
          (finfo->fid->cosid->pos)++;
       else if (iret == -1)
          return -1;
    }
    else
       iret = 1;

    return iret;
}

int xreadpphead(FILEINFO *finfo, INTEGER *buf1, int n1, REAL *buf2, int n2)
{
    int iret, filetype, n3, n4;
    int64 *buf1_64;
    float64 *buf2_64;
#if _FLT_SIZE == 8 || _INT_SIZE == 8
    int i;
#endif
#if _INT_TYPE == _IEEE8
    int n11;
#endif
#if _FLT_TYPE == _IEEE8
    int n22;
#endif

    filetype = finfo->ftype;

    if (filetype == UMFILE || filetype == UMFILE64 || filetype == UMFILECRAY ||
        filetype == UMFILEBS || filetype == UMFILE64BS)
    {
       iret = xreadint(finfo, buf1, n1);
       if (iret == 0)
          iret = xreadreal(finfo, buf2, n2);
    }
    else if (filetype == PPFILE || filetype == PPFILEBS)
    {
#if _INT_TYPE == _IEEE8
       n11 = (n1+1)/2;
       if ( (buf1_64 = malloc (n11*8)) == NULL )
       {
          if (n11 != 0)
             xerror("Error unable to allocate memory for buf1_64 in xreadpphead n = %d \n", n1);
       }
#else
       buf1_64 = (int64 *) buf1;
#endif
#if _FLT_TYPE == _IEEE8
       n22 = (n2+1)/2;
       if ( (buf2_64 = malloc (n22*8)) == NULL )
       {
          if (n22 != 0)
              xerror("Error unable to allocate memory for buf2_64 in xreadpphead n = %d \n", n2);
       }
#else
       buf2_64 = (float64 *) buf2;
#endif
       iret = ppreadhead(finfo->fid->ppid, buf1_64, n1*4, &n3, buf2_64, n2*4, &n4);

#if _INT_TYPE == _IEEE8
       i4_to_i8(buf1_64, buf1, n1);
       buf1_64 = free_x (buf1_64);
#endif
#if _FLT_TYPE == _IEEE8
       r4_to_r8(buf2_64, buf2, n2);
       buf2_64 = free_x (buf2_64);
#endif
    }
    else if (filetype == PPFILE64 || filetype == PPFILE64BS)
    {
#if _INT_TYPE == _IEEE4
       if ( (buf1_64 = malloc (n1*8)) == NULL )
       {
          if (n1 != 0)
             xerror("Error unable to allocate memory for buf1_64 in xreadpphead n = %d \n", n1);
       }
#else
       buf1_64 = (int64 *) buf1;
#endif
#if _FLT_TYPE == _IEEE4
       if ( (buf2_64 = malloc (n2*8)) == NULL )
       {
          if (n2 != 0)
              xerror("Error unable to allocate memory for buf2_64 in xreadpphead n = %d \n", n2);
       }
#else
       buf2_64 = (float64 *) buf2;
#endif

       iret = ppreadhead(finfo->fid->ppid, buf1_64, n1*8, &n3, buf2_64, n2*8, &n4);

#if _INT_TYPE == _IEEE4
       i8_to_i4(buf1_64, buf1, n1);
       buf1_64 = free_x (buf1_64);
#endif
#if _FLT_TYPE == _IEEE4
       r8_to_r4(buf2_64, buf2, n2);
       buf2_64 = free_x (buf2_64);
#endif
    }
    else if (filetype == PPFILEI32R64)
    {
#if _INT_TYPE == _IEEE8
       n11 = (n1+1)/2;
       if ( (buf1_64 = malloc (n11*8)) == NULL )
       {
          if (n11 != 0)
             xerror("Error unable to allocate memory for buf1_64 in xreadpphead n = %d \n", n1);
       }
#else
       buf1_64 = (int64 *) buf1;
#endif
#if _FLT_TYPE == _IEEE4
       if ( (buf2_64 = malloc (n2*8)) == NULL )
       {
          if (n2 != 0)
              xerror("Error unable to allocate memory for buf2_64 in xreadpphead n = %d \n", n2);
       }
#else
       buf2_64 = (float64 *) buf2;
#endif

       iret = ppreadhead(finfo->fid->ppid, buf1_64, n1*4, &n3, buf2_64, n2*8, &n4);

#if _INT_TYPE == _IEEE8
       i4_to_i8(buf1_64, buf1, n1);
       buf1_64 = free_x (buf1_64);
#endif
#if _FLT_TYPE == _IEEE4
       r8_to_r4(buf2_64, buf2, n2);
       buf2_64 = free_x (buf2_64);
#endif
    }
    else if (filetype == PPFILECRAY)
    {
       if ( (buf1_64 = malloc ((n1+n2)*8)) == NULL )
       {
          if ((n1+n2) != 0)
             xerror("Error unable to allocate memory for buf1_64 in xreadpphead n = %d \n", n1);
       }

       n3 = (n1+n2)*8;
       iret = cos_read(finfo->fid->cosid->cosfp, buf1_64, n3, &n4);
       if (iret == 0 || iret == -3) 
          (finfo->fid->cosid->pos)++;
       else if (iret == -1)
          return -1;

#if _INT_TYPE == _CRAY8
       for (i=0; i<n1; i++) buf1[i] = buf1_64[i];
#elif _INT_TYPE == _IEEE8
       c8_to_i8(buf1_64, buf1, n1);
#else
       c8_to_i4(buf1_64, buf1, n1);
#endif

#if _FLT_TYPE == _CRAY8
       for (i=0; i<n2; i++) buf2[i] = *((REAL *) buf1_64+n1+i);
#elif _FLT_TYPE == _IEEE8
       c8_to_r8(buf1_64+n1, buf2, n2);
#else
       c8_to_r4(buf1_64+n1, buf2, n2);
#endif

       buf1_64 = free_x (buf1_64);
    }
    else if (filetype == PPFILECRAYIEEE)
    {
       if ( (buf1_64 = malloc ((n1+n2)*8)) == NULL )
       {
          if ((n1+n2) != 0)
             xerror("Error unable to allocate memory for buf1_64 in xreadpphead n = %d \n", n1);
       }

       n3 = (n1+n2)*8;
       iret = cos_read(finfo->fid->cosid->cosfp, buf1_64, n3, &n4);
       if (iret == 0 || iret == -3) 
          (finfo->fid->cosid->pos)++;
       else if (iret == -1)
          return -1;

#ifdef LITTLE__ENDIAN
       swap_bytes(buf1_64,8,n1+n2);
#endif
#if _INT_TYPE == _IEEE8
       for (i=0; i<n1; i++) buf1[i] = buf1_64[i];
#elif _INT_TYPE == _CRAY8
       i8_to_c8(buf1_64, buf1, n1);
#else
       i8_to_i4(buf1_64, buf1, n1);
#endif

#if _FLT_TYPE == _IEEE8
       for (i=0; i<n2; i++) buf2[i] = *((REAL *) buf1_64+n1+i);
#elif _FLT_TYPE == _CRAY8
       r8_to_c8(buf1_64+n1, buf2, n2);
#else
       r8_to_r4(buf1_64+n1, buf2, n2);
#endif

       buf1_64 = free_x (buf1_64);
    }
    else
       iret = 1;

    return iret;
}

PPFILEID *ppopen(char *fname, char *mode, int swap)
{
   PPFILEID *file;
   FILE *fp;
   char *ppmode;
   int alloc_ppmode, j;

   /* Allocate memory for PPFILE structure */

   if ((file = (PPFILEID *) malloc(sizeof(PPFILEID))) == NULL)
   {
       fprintf(stderr,"Error unable to allocate memory in ppopen \n");
       return NULL;
   }

   /* Make sure mode=b is used */

   alloc_ppmode = TRUE;
   for (j=0; j<strlen(mode); j++)
   {
      if (mode[j] == 'b')
      {
         alloc_ppmode = FALSE;
         ppmode = mode;
         break;
      }
   }
   if (alloc_ppmode)
   {
      if ((ppmode = (char *) malloc(strlen(mode)+2)) == NULL)
         return NULL;
      strcpy(ppmode, mode);
      strcat(ppmode, "b");
   }

   /* Open file */

   if ((fp = fopen(fname, ppmode)) == NULL)
   {
       fprintf(stderr,"Error opening file %s in ppopen \n", fname);
       perror("fopen error");
       if (alloc_ppmode) ppmode = free_x(ppmode);
       return NULL;
   }

   /* Initialise PPFILEID structure */

   file->fname = fname;
   file->fp = fp;
   file->pos = 0;
   file->swap = (char) swap;
   if (alloc_ppmode) ppmode = free_x(ppmode);

   return file;
}

int ppclose (PPFILEID *file)
{
   int iret;

   /* Close file */

   iret = fclose(file->fp);

   if (iret != 0)
      fprintf(stderr,"Error closing file %s in ppclose \n", file->fname);

   /* Free memory for PPFILE structure */

   file = free_x (file);

   return iret;
}

int pprewind (PPFILEID *file)
{
   int iret;

   /* Rewind file */

   iret = xfseek(file->fp, 0, SEEK_SET);
   if (iret != 0)
   {
      fprintf(stderr,"Error rewinding file %s in pprewind \n", file->fname);
      return 1;
   }

   /* Reset byte position */

   file->pos = 0;

   return 0;
}

int ppbackspace (PPFILEID *file)
{
   int iret;
#if _INT_TYPE == _CRAY8
   int size, size1;
#else
   int32 size;
#endif

   /* read size of previous record */

   iret = xfseek(file->fp, -4, SEEK_CUR);
   if (iret != 0)
   {
      fprintf(stderr,"Error seeking in file %s in ppbackspace \n", file->fname);
      return 1;
   }
   iret = fread(&size, 4, 1, file->fp);
   if (iret != 1)
   {
      fprintf(stderr,"Error reading file %s in ppbackspace \n", file->fname);
      return 2;
   }
   if (file->swap != 0) swap_bytes(&size,4,1);
#if _INT_TYPE == _CRAY8
   i4_to_c8(size, size1, 1);
   size = size1;
#endif

   /* seek to start of previous record */

   iret = xfseek(file->fp, -(size + 4), SEEK_CUR);
   if (iret != 0)
   {
      fprintf(stderr,"Error seeking in file %s in ppbackspace \n", file->fname);
      return 3;
   }

   /* Reset byte position */

   file->pos -= size + 8;

   return 0;
}

int ppskip (PPFILEID *file)
{
   int iret;
#if _INT_TYPE == _CRAY8
   int size, size1;
#else
   int32 size;
#endif

   /* read size of record */

   iret = fread(&size, 4, 1, file->fp);
   if (iret != 1)
   {
      if (feof(file->fp) != 0 ) return -1;
      fprintf(stderr,"Error reading file %s in ppskip \n", file->fname);
      return 1;
   }
   if (file->swap != 0) swap_bytes(&size,4,1);
#if _INT_TYPE == _CRAY8
   i4_to_c8(size, size1, 1);
   size = size1;
#endif

   /* seek to end of record */

   iret = xfseek(file->fp, size + 4, SEEK_CUR);
   if (iret != 0)
   {
      fprintf(stderr,"Error seeking in file %s in ppskip \n", file->fname);
      return 2;
   }

   /* Reset byte position */

   file->pos += size + 8;

   return 0;
}

int ppseek (PPFILEID *file, XADDRESS newpos)
{
   int iret;

   iret = xfseek(file->fp, newpos, SEEK_SET);
   if (iret != 0)
   {
      fprintf(stderr,"Error seeking in file %s in ppseek \n", file->fname);
      return 1;
   }

   /* Reset byte position */

   file->pos = newpos;

   return 0;
}

XADDRESS pptell (PPFILEID *file)
{
   return xftell(file->fp);
}

int ppread (PPFILEID *file, void *data, int datasize, int *readsize)
{
   int iret, iret1;
#if _INT_TYPE == _CRAY8
   int recsize, recsize1;
#else
   int32 recsize;
#endif
   char *cdata;

#ifdef DEBUG
   printf("reading %d bytes of data from file %s at postion %lu in ppread\n",
           datasize,file->fname,file->pos);
#endif
   cdata = (char *) data;

   /* Get size of data record to be read */

   iret = fread(&recsize, 4, 1, file->fp);
   if (iret != 1)
   {
      if (feof(file->fp) != 0 ) return -1;
      fprintf(stderr,"Error reading file %s in ppread \n", file->fname);
      return 1;
   }
   if (file->swap != 0) swap_bytes(&recsize,4,1);
#if _INT_TYPE == _CRAY8
   i4_to_c8(recsize, recsize1, 1);
   recsize = recsize1;
#endif
#ifdef DEBUG
   printf("recsize = %d \n",recsize);
#endif

   if (recsize > datasize)
   {
      /* Only read partial record */

      if (datasize > 0)
      {
         iret = fread(cdata, 1, datasize, file->fp);
         if (iret != datasize)
         {
            fprintf(stderr,"Error reading file %s in ppread \n", file->fname);
            return 2;
         }
         if (file->swap != 0) swap_bytes(cdata,file->swap,datasize/file->swap);
      }

      /* Skip rest of record */

      iret = xfseek(file->fp, recsize-datasize, SEEK_CUR);
      if (iret != 0)
      {
         fprintf(stderr,"Error seeking in file %s in ppread \n", file->fname);
         return 3;
      }

      iret1 = -3;
      *readsize = datasize;
   }
   else
   {
      iret = fread(cdata, 1, recsize, file->fp);
      if (iret != recsize)
      {
         fprintf(stderr,"Error reading file %s in ppread \n", file->fname);
         return 4;
      }
      if (file->swap != 0) swap_bytes(cdata,file->swap,recsize/file->swap);

      iret1 = 0;
      *readsize = recsize;
   }

   /* Skip footer to position at start of next record */

   iret = xfseek(file->fp, 4, SEEK_CUR);
   if (iret != 0)
   {
      fprintf(stderr,"Error seeking in file %s in ppread \n", file->fname);
      return 5;
   }

   /* Reset byte position */

   file->pos += recsize + 8;

   return iret1;
}

/* Same as ppread but no byteswapping of data */

int ppreadpack (PPFILEID *file, void *data, int datasize, int *readsize)
{
   int iret, iret1;
#if _INT_TYPE == _CRAY8
   int recsize, recsize1;
#else
   int32 recsize;
#endif
   char *cdata;

#ifdef DEBUG
   printf("reading %d bytes of data from file %s at postion %lu in ppread\n",
           datasize,file->fname,file->pos);
#endif
   cdata = (char *) data;

   /* Get size of data record to be read */

   iret = fread(&recsize, 4, 1, file->fp);
   if (iret != 1)
   {
      if (feof(file->fp) != 0 ) return -1;
      fprintf(stderr,"Error reading file %s in ppread \n", file->fname);
      return 1;
   }
   if (file->swap != 0) swap_bytes(&recsize,4,1);
#if _INT_TYPE == _CRAY8
   i4_to_c8(recsize, recsize1, 1);
   recsize = recsize1;
#endif

   if (recsize > datasize)
   {
      /* Only read partial record */

      if (datasize > 0)
      {
         iret = fread(cdata, 1, datasize, file->fp);
         if (iret != datasize)
         {
            fprintf(stderr,"Error reading file %s in ppread \n", file->fname);
            return 2;
         }
      }

      /* Skip rest of record */

      iret = xfseek(file->fp, recsize-datasize, SEEK_CUR);
      if (iret != 0)
      {
         fprintf(stderr,"Error seeking in file %s in ppread \n", file->fname);
         return 3;
      }

      iret1 = -3;
      *readsize = datasize;
   }
   else
   {
      iret = fread(cdata, 1, recsize, file->fp);
      if (iret != recsize)
      {
         fprintf(stderr,"Error reading file %s in ppread \n", file->fname);
         return 4;
      }

      iret1 = 0;
      *readsize = recsize;
   }

   /* Skip footer to position at start of next record */

   iret = xfseek(file->fp, 4, SEEK_CUR);
   if (iret != 0)
   {
      fprintf(stderr,"Error seeking in file %s in ppread \n", file->fname);
      return 5;
   }

   /* Reset byte position */

   file->pos += recsize + 8;

   return iret1;
}

int ppreadhead (PPFILEID *file, void *data1, int datasize1, int *readsize1,
                                void *data2, int datasize2, int *readsize2)
{
   int iret, iret1;
#if _INT_TYPE == _CRAY8
   int recsize, recsize1;
#else
   int32 recsize;
#endif
   char *cdata1, *cdata2;

   cdata1 = (char *) data1;
   cdata2 = (char *) data2;

   /* Get size of data record to be read */

   iret = fread(&recsize, 4, 1, file->fp);
   if (iret != 1)
   {
      if (feof(file->fp) != 0 ) return -1;
      fprintf(stderr,"Error reading file %s in ppreadhead \n", file->fname);
      return 1;
   }
   if (file->swap != 0) swap_bytes(&recsize,4,1);
#if _INT_TYPE == _CRAY8
   i4_to_c8(recsize, recsize1, 1);
   recsize = recsize1;
#endif

   if (recsize > datasize1+datasize2)
   {
      /* Only read partial record */

      if (datasize1 > 0)
      {
         iret = fread(cdata1, 1, datasize1, file->fp);
         if (iret != datasize1)
         {
            fprintf(stderr,"Error reading file %s in ppreadhead \n", file->fname);
            return 2;
         }
         if (file->swap != 0) swap_bytes(cdata1,file->swap,datasize1/file->swap);
      }

      if (datasize2 > 0)
      {
         iret = fread(cdata2, 1, datasize2, file->fp);
         if (iret != datasize2)
         {
            fprintf(stderr,"Error reading file %s in ppreadhead \n", file->fname);
            return 3;
         }
         if (file->swap != 0) swap_bytes(cdata2,file->swap,datasize2/file->swap);
      }

      /* Skip rest of record */

      iret = xfseek(file->fp, recsize-datasize1-datasize2, SEEK_CUR);
      if (iret != 0)
      {
         fprintf(stderr,"Error seeking in file %s in ppreadhead \n", file->fname);
         return 4;
      }

      iret1 = -3;
      *readsize1 = datasize1;
      *readsize2 = datasize2;
   }
   else
   {
      if (datasize1 > recsize)
      {
         iret = fread(cdata1, 1, recsize, file->fp);
         if (iret != recsize)
         {
            fprintf(stderr,"Error reading file %s in ppreadhead \n", file->fname);
            return 5;
         }
         if (file->swap != 0) swap_bytes(cdata1,file->swap,recsize/file->swap);
         cdata2 = NULL;
         *readsize1 = recsize;
         *readsize2 = 0;
      }
      else
      {
         if (datasize1 > 0)
         {
            iret = fread(cdata1, 1, datasize1, file->fp);
            if (iret != datasize1)
            {
               fprintf(stderr,"Error reading file %s in ppreadhead \n", file->fname);
               return 6;
            }
            if (file->swap != 0) swap_bytes(cdata1,file->swap,datasize1/file->swap);
         }

         iret = fread(cdata2, 1, recsize-datasize1, file->fp);
         if (iret != recsize-datasize1)
         {
            fprintf(stderr,"Error reading file %s in ppreadhead \n", file->fname);
            return 7;
         }
         if (file->swap != 0) swap_bytes(cdata2,file->swap,
                                        (recsize-datasize1)/file->swap);
         *readsize1 = datasize1;
         *readsize2 = recsize-datasize1;
      }

      iret1 = 0;
   }

   /* Skip footer to position at start of next record */

   iret = xfseek(file->fp, 4, SEEK_CUR);
   if (iret != 0)
   {
      fprintf(stderr,"Error seeking in file %s in ppreadhead \n", file->fname);
      return 8;
   }

   /* Reset byte position */

   file->pos += recsize + 8;

   return iret1;
}

int ppwrite (PPFILEID *file, void *data, int datasize)
{
   int iret;
#if _INT_TYPE == _CRAY8
   int header;
#else
   int32 header;
#endif
   char *cdata;

#ifdef DEBUG
   printf("writing %d bytes of data from file %s at postion %lu in ppwrite\n",
           datasize,file->fname,file->pos);
#endif
   cdata = (char *) data;
#if _INT_TYPE == _CRAY8
   c8_to_i4(datasize,header,1);
#else
   header = (int32) datasize;
#endif

   /* Swap bytes in data and header if necessary */

   if (file->swap != 0)
   {
      swap_bytes(&header,4,1);
      swap_bytes(cdata,file->swap,datasize/file->swap);
   }
#ifdef DEBUG
   printf("header = %d \n",header);
#endif

   /* Write header */

   iret = fwrite(&header, 4, 1, file->fp);
   if (iret != 1)
   {
      fprintf(stderr,"Error writing header to file %s in ppwrite \n", file->fname);
      return 1;
   }

   /* Write data */

   iret = fwrite(cdata, 1, datasize, file->fp);
   if (iret != datasize)
   {
      fprintf(stderr,"Error writing data to file %s in ppwrite \n", file->fname);
      return 2;
   }

   /* Write footer */

   iret = fwrite(&header, 4, 1, file->fp);
   if (iret != 1)
   {
      fprintf(stderr,"Error writing footer to file %s in ppwrite \n", file->fname);
      return 3;
   }

   /* Unswap bytes in data if necessary */

   if (file->swap != 0)
   {
      swap_bytes(cdata,file->swap,datasize/file->swap);
   }

   /* Reset byte position */

   file->pos += datasize + 8;

   return 0;
}

#if _FLT_SIZE == 8
int xwritereal(FILEINFO *finfo, REAL *buf, int n)
{
    int num, iret, filetype, n1;
    REAL *buf1;

    if (n == 0) 
       return 0;
    else if (n < 0) 
       return 1;

    filetype = finfo->ftype;

    if (filetype == UMFILE64 || filetype == UMFILE64BS)
    {
#if _FLT_TYPE == _CRAY8
       if ( (buf1 = malloc (n*8)) == NULL )
       {
          if (n != 0)
             xerror("Error unable to allocate memory for buf1 in xwritereal n = %d \n", n);
       }
       c8_to_r8(buf, buf1, n);
       if (filetype == UMFILE64BS) swap_bytes(buf1,8,n);
       num = fwrite(buf1, 8, n, finfo->fid->fp);
       buf1 = free_x (buf1);
#else
       if (filetype == UMFILE64BS) swap_bytes(buf,8,n);
       num = fwrite(buf, 8, n, finfo->fid->fp);
#endif
       if (num == n)
          iret = 0;
       else
          iret = 1;
    }
    else if (filetype == UMFILE || filetype == UMFILEBS)
    {
       n1 = (n+1)/2;
       if ( (buf1 = malloc (n1*8)) == NULL )
       {
          if (n1 != 0)
             xerror("Error unable to allocate memory for buf1 in xwritereal n1 = %d \n", n1);
       }
#if _FLT_TYPE == _IEEE8
       r8_to_r4(buf, buf1, n);
#else
       c8_to_r4(buf, buf1, n);
#endif
       if (filetype == UMFILEBS) 
          swap_bytes(buf1,4,n);
       num = fwrite(buf1, 4, n, finfo->fid->fp);
       if (num == n)
          iret = 0;
       else
          iret = 1;

       buf1 = free_x (buf1);
    }
    else if (filetype == PPFILE64 || filetype == PPFILE64BS || 
             filetype == PPFILEI32R64)
    {
#if _FLT_TYPE == _CRAY8
       if ( (buf1 = malloc (n*8)) == NULL )
       {
          if (n != 0)
             xerror("Error unable to allocate memory for buf1 in xwritereal n = %d \n", n);
       }
       c8_to_r8(buf, buf1, n);
       iret = ppwrite(finfo->fid->ppid, buf, n*8);
       buf1 = free_x (buf1);
#else
       iret = ppwrite(finfo->fid->ppid, buf, n*8);
#endif
    }
    else if (filetype == PPFILE || filetype == PPFILEBS)
    {
       n1 = (n+1)/2;
       if ( (buf1 = malloc (n1*8)) == NULL )
       {
          if (n1 != 0)
             xerror("Error unable to allocate memory for buf1 in xwritereal n1 = %d \n", n1);
       }

       r8_to_r4(buf, buf1, n);
       iret = ppwrite(finfo->fid->ppid, buf1, n1*8);

       buf1 = free_x (buf1);
    }

    return iret;
}
#else
int xwritereal(FILEINFO *finfo, REAL *buf, int n)
{
    int num, iret, filetype;
    float64 *buf64;

    if (n == 0) 
       return 0;
    else if (n < 0) 
       return 1;

    filetype = finfo->ftype;

    if (filetype == UMFILE || filetype == UMFILEBS)
    {
       if (filetype == UMFILEBS) 
          swap_bytes(buf,4,n);
       num = fwrite(buf, 4, n, finfo->fid->fp);
       if (num == n)
          iret = 0;
       else
          iret = 1;
    }
    else if (filetype == UMFILE64 || filetype == UMFILE64BS)
    {
       if ( (buf64 = malloc (n*8)) == NULL )
       {
          if (n != 0)
             xerror("Error unable to allocate memory for buf64 in xwritereal n = %d \n", n);
       }

       r4_to_r8(buf, buf64, n);
       if (filetype == UMFILE64BS) swap_bytes(buf64,8,n);
       num = fwrite(buf64, 8, n, finfo->fid->fp);
       if (num == n)
          iret = 0;
       else
          iret = 1;

       buf64 = free_x (buf64);
    }
    else if (filetype == PPFILE || filetype == PPFILEBS)
    {
       iret = ppwrite(finfo->fid->ppid, buf, n*4);
    }
    else if (filetype == PPFILE64 || filetype == PPFILE64BS || 
             filetype == PPFILEI32R64)
    {
       if ( (buf64 = malloc (n*8)) == NULL )
       {
          if (n != 0)
             xerror("Error unable to allocate memory for buf64 in xwritereal n = %d \n", n);
       }

       r4_to_r8(buf, buf64, n);
       iret = ppwrite(finfo->fid->ppid, buf64, n*8);

       buf64 = free_x (buf64);
    }

    return iret;
}
#endif

FILEINFO *xopen(char *filename, char *filemode, int filetype)
{
    FILEINFO *finfo;
    FILE *fp;
    COSFILE *cosfp;
    PPFILEID *ppfp;
    int iret;
    static int unit=100;
    char swap;
    char *xmode;
    int alloc_xmode, j;

    if ((finfo = (FILEINFO *) malloc(sizeof(FILEINFO))) == NULL)
       return NULL;
    if ((finfo->fid = (FILEID *) malloc(sizeof(FILEID))) == NULL)
       return NULL;

    /* Check if mode 'b' is used, if not add it */

    if (filetype == UMFILE || filetype == UMFILE64 || filetype == UMFILECRAY ||
        filetype == UMFILEBS || filetype == UMFILE64BS ||
        filetype == PPFILE || filetype == PPFILE64 ||
        filetype == PPFILEBS || filetype == PPFILE64BS ||
        filetype == PPFILEI32R64 || filetype == PPFILECRAY ||
        filetype == PPFILECRAYIEEE)
    {
       alloc_xmode = TRUE;
       for (j=0; j<strlen(filemode); j++)
       {
          if (filemode[j] == 'b')
          {
             alloc_xmode = FALSE;
             xmode = filemode;
             break;
          }
       }
       if (alloc_xmode)
       {
          if ((xmode = (char *) malloc(strlen(filemode)+2)) == NULL)
             return NULL;
          strcpy(xmode, filemode);
          strcat(xmode, "b");
       }
    }
    else
    {
       alloc_xmode = FALSE;
       xmode = filemode;
    }

    if (filetype == UMFILE || filetype == UMFILE64 || filetype == UMFILECRAY ||
        filetype == UMFILEBS || filetype == UMFILE64BS)
    {
       if ((fp = fopen(filename, xmode)) == NULL)
       {
          if (alloc_xmode) xmode = free_x(xmode);
          return NULL;
       }
       finfo->fid->fp = fp;
    }
    else if (filetype == PPFILE || filetype == PPFILE64 || 
             filetype == PPFILEBS || filetype == PPFILE64BS || 
             filetype == PPFILEI32R64)
    {
       if (filetype == PPFILEBS)
          swap = 4;
       else if (filetype == PPFILE64BS)
          swap = 8;
       else
          swap = 0;

       if ((ppfp = ppopen(filename, xmode, swap)) == NULL)
       {
          if (alloc_xmode) xmode = free_x(xmode);
          return NULL;
       }

       if ((finfo->fid->ppid = (PPFILEID *) malloc(sizeof(PPFILEID))) == NULL)
          return NULL;

       finfo->fid->ppid = ppfp;
    }
    else if (filetype == PPFILECRAY || filetype == PPFILECRAYIEEE)
    {
       if ((cosfp = cos_open(filename, xmode)) == NULL)
       {
          if (alloc_xmode) xmode = free_x(xmode);
          return NULL;
       }

       if ((finfo->fid->cosid = (COSID *) malloc(sizeof(COSID))) == NULL)
          return NULL;

       finfo->fid->cosid->cosfp = cosfp;
       finfo->fid->cosid->pos = 0;
    }
    else
    {
       printf("Unknown format %d\n", filetype);
       return NULL;
    }

    strcpy(finfo->fname, filename);
    finfo->ftype = filetype;
    if (alloc_xmode) xmode = free_x(xmode);

    return finfo;
}

int xclose(FILEINFO *finfo)
{
    int iret;
    int filetype;

    filetype = finfo->ftype;

    if (filetype == UMFILE || filetype == UMFILE64 || filetype == UMFILECRAY ||
        filetype == UMFILEBS || filetype == UMFILE64BS)
    {
       iret = fclose(finfo->fid->fp);
    }
    else if (filetype == PPFILE || filetype == PPFILE64 || 
             filetype == PPFILEBS || filetype == PPFILE64BS || 
             filetype == PPFILEI32R64)
    {
       iret = ppclose(finfo->fid->ppid);
    }
    else if (filetype == PPFILECRAY || filetype == PPFILECRAYIEEE)
    {
       iret = cos_close(finfo->fid->cosid->cosfp);
       finfo->fid->cosid = free_x (finfo->fid->cosid);
    }

    finfo->fid = free_x (finfo->fid);
    finfo = free_x (finfo);

    return iret;
}

XADDRESS xtell(FILEINFO *finfo)
{
    XADDRESS pos;
    int filetype;

    filetype = finfo->ftype;
    pos = -1;

    if (filetype == UMFILE || filetype == UMFILEBS)
    {
       pos = xftell(finfo->fid->fp);
       if (pos < 0)
          pos = -1;
       else
          pos = pos/4;
    }
    else if (filetype == UMFILE64 || filetype == UMFILE64BS || 
             filetype == UMFILECRAY)
    {
       pos = xftell(finfo->fid->fp);
       if (pos < 0) 
          pos = -1;
       else
          pos = pos/8;
    }
    else if (filetype == PPFILE || filetype == PPFILE64 || 
             filetype == PPFILEBS || filetype == PPFILE64BS || 
             filetype == PPFILEI32R64)
    {
       pos = pptell(finfo->fid->ppid);
    }
    else if (filetype == PPFILECRAY || filetype == PPFILECRAYIEEE)
    {
       pos = (XADDRESS) finfo->fid->cosid->pos;
    }

    return pos;
}

int xseek(FILEINFO *finfo, XADDRESS pos)
{
    int iret;
    int filetype, curpos, j, n;

    filetype = finfo->ftype;

    if (filetype == UMFILE || filetype == UMFILEBS)
    {
       iret = xfseek(finfo->fid->fp, 4 * pos, SEEK_SET);
       if (iret < 0) iret = -iret;
    }
    else if (filetype == UMFILE64 || filetype == UMFILE64BS || 
             filetype == UMFILECRAY)
    {
       iret = xfseek(finfo->fid->fp, 8 * pos, SEEK_SET);
       if (iret < 0) iret = -iret;
    }
    else if (filetype == PPFILE || filetype == PPFILE64 || 
             filetype == PPFILEBS || filetype == PPFILE64BS || 
             filetype == PPFILEI32R64)
    {
       iret = ppseek(finfo->fid->ppid, pos);
    }
    else if (filetype == PPFILECRAY || filetype == PPFILECRAYIEEE)
    {
       iret = 0;
       curpos = finfo->fid->cosid->pos;
       if (pos == curpos)
          ;
       else if (pos == 0)
       {
          iret = cos_rewind(finfo->fid->cosid->cosfp);
       }
       else if (pos > curpos)
       {
          for (j=curpos; j<pos; j++)
             cos_read(finfo->fid->cosid->cosfp, NULL, 0, &n);
       }
       else if (pos >= curpos/2)
       {
          for (j=pos; j<curpos; j++)
             iret = cos_backspace(finfo->fid->cosid->cosfp);
       }
       else
       {
          iret = cos_rewind(finfo->fid->cosid->cosfp);
          for (j=0; j<pos; j++)
             cos_read(finfo->fid->cosid->cosfp, NULL, 0, &n);
       }
       finfo->fid->cosid->pos = pos;
    }

    return iret;
}

int xrewind(FILEINFO *finfo)
{
    int iret, filetype;

    filetype = finfo->ftype;

    if (filetype == UMFILE || filetype == UMFILEBS || 
        filetype == UMFILE64 || filetype == UMFILE64BS || 
        filetype == UMFILECRAY)
    {
       iret = xfseek(finfo->fid->fp, 0L, SEEK_SET);
    }
    else if (filetype == PPFILE || filetype == PPFILE64 || 
             filetype == PPFILEBS || filetype == PPFILE64BS || 
             filetype == PPFILEI32R64)
    {
       iret = pprewind(finfo->fid->ppid);
    }
    else if (filetype == PPFILECRAY || filetype == PPFILECRAYIEEE)
    {
       iret = cos_rewind(finfo->fid->cosid->cosfp);
       finfo->fid->cosid->pos = 0;
    }

    return iret;
}

int xskip(FILEINFO *finfo)
{
    int iret, filetype, n;

    filetype = finfo->ftype;

    if (filetype == PPFILE || filetype == PPFILE64 || 
             filetype == PPFILEBS || filetype == PPFILE64BS || 
             filetype == PPFILEI32R64)
    {
       iret = ppskip(finfo->fid->ppid);
    }
    else if (filetype == PPFILECRAY || filetype == PPFILECRAYIEEE)
    {
       iret = cos_read(finfo->fid->cosid->cosfp, NULL, 0, &n);
       if (iret == -3) iret = 0;
       (finfo->fid->cosid->pos)++;
    }

    return iret;
}

void xerror(char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);

    if (xerror_func == NULL)
       vfprintf(stderr, fmt, args);
    else
       (*xerror_func)(fmt, args);

    va_end(args);

    exit(1);
}

XADDRESS xftell (FILE *fp)
{
   XADDRESS address;

#if _FILE_OFFSET_BITS == 64 || defined _LARGE_FILES
   address = (XADDRESS) ftello(fp);
#else
   address = (XADDRESS) ftell(fp);
#endif
   if (address == -1) perror("ftell error");

   return address;
}

int xfseek (FILE *fp, XADDRESS address, int whence)
{
    int iret;

#if _FILE_OFFSET_BITS == 64 || defined _LARGE_FILES
    iret = fseeko(fp, address, whence);
#else
    iret = fseek(fp, address, whence);
#endif
    if (iret != 0) perror("fseek error");

    return iret;
}

/* Having problems with freeing pointers twice, so replace free with free_x */

void *free_x(void *ptr)
{
   if (ptr != NULL)
   {
      free(ptr);
      ptr = NULL;
   }

   return ptr;
}
