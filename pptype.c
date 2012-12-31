#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "util_xancil.h"

#define TRUE 1
#define FALSE 0

#ifdef LONG64
#define ZERO64 0l
#else
#define ZERO64 0ll
#endif 

#define UMFILE 1
#define PPFILE 2
#define UMFILEBS 201
#define UMFILE64 65
#define PPFILE64 66
#define UMFILE64BS 265
#define UMFILECRAY 165
#define PPFILECRAY 166
#define PPFILECRAYIEEE 167

#if __FORT_SYMBOL == _FS_UC
#define pptype  PPTYPE
#elif __FORT_SYMBOL == _FS_LC_
#define pptype  pptype_
#endif

void pptype(fpchar, INTEGER *, fpchar, int, int);
int get_type(char *);

void pptype(fpchar file, INTEGER *lum, fpchar type, int flen, int tlen)
{
    char *p, *cfile, *ctype;
    int iret, ctlen, i;

    /* convert fortran CHARACTERs to c chars */

#ifdef _CRAY
    flen = _fcdlen(file);
    cfile = ( char *) malloc(flen+1);
    strncpy(cfile, _fcdtocp(file), flen);
    cfile[flen] = '\0';
#else
    cfile = ( char *) malloc(flen+1);
    strncpy(cfile, file, flen);
    cfile[flen] = '\0';
#endif

    /* strip trailing blanks */

    p = cfile+flen-1;
    while(*p == ' ')
    {
       *p = '\0';
       p--;
    }

    iret = get_type(cfile);

    if (iret == 0)
    {
       printf("Error in getting file type \n");
       abort();
    }

    ctype = (char *) malloc(4);
    if (iret == UMFILE || iret == PPFILE)
       strcpy(ctype, "IE4");
    else if (iret == UMFILE64 || iret == PPFILE64)
       strcpy(ctype, "IE8");
    else if (iret == UMFILEBS)
       strcpy(ctype, "IS4");
    else if (iret == UMFILE64BS)
       strcpy(ctype, "IS8");
    else if (iret == PPFILECRAYIEEE)
       strcpy(ctype, "CI8");
    else if (iret == UMFILECRAY || iret == PPFILECRAY)
       strcpy(ctype, "CR8");

    if (iret == UMFILE || iret == UMFILE64 || 
        iret == UMFILEBS || iret == UMFILE64BS || iret == UMFILECRAY)
       *lum = TRUE;
    else if (iret == PPFILE || iret == PPFILE64 || 
             iret == PPFILECRAY || iret == PPFILECRAYIEEE)
       *lum = FALSE;

    /* convert c chars to fortran CHARACTERs */

    ctlen = strlen(ctype);
#ifdef _CRAY
    tlen = _fcdlen(type);
    strncpy(_fcdtocp(type), ctype, ctlen);
    for (i=ctlen; i<tlen; i++)
       (_fcdtocp(type))[i] = ' ';
#else
    strncpy(type, ctype, ctlen);
    for (i=ctlen; i<tlen; i++)
       type[i] = ' ';
#endif

#ifdef _CRAY
    if (*lum == TRUE)
       *lum = _btol(TRUE);
    else
       *lum = _btol(FALSE);
#endif

    free (cfile);
    free (ctype);

    return;
}

int get_type(char *file)
{
    int fwi;
    int64 ibuf64[2], isub64;
    INTEGER ibuf[2], isub;
    INTEGER buf[4], swapbuf[4];
    REAL rbuf1 ,rbuf2;
    REAL rbuf[4];
    FILE *fp;

    if ((fp = fopen(file, "r")) == NULL)
    {
       printf("Error opening file %s \n",file);
       return 0;
    }

    fread(buf, 1, 16, fp);

    if (fclose(fp) != 0)
    {
       printf("Error closing file %s \n",file);
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
       if ((fp = fopen(file, "r")) == NULL)
       {
          printf("Error opening file %s \n",file);
          return 0;
       }

       fseek(fp, 832l, SEEK_SET);
       fread(ibuf64, 8, 1, fp);
       fseek(fp, 8*((long) ibuf64[0] + 1l), SEEK_SET);
       fread(rbuf, 8, 2, fp);

       if (fclose(fp) != 0)
       {
          printf("Error closing file %s \n",file);
          return 0;
       }

#ifdef BIG__ENDIAN
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
       if ((rbuf1 >= -90.0 && rbuf1 <= 90.0) &&
           (rbuf2 >= 0.0 && rbuf2 <= 360.0))
       {
          printf("file %s is a Cray um file \n",file);
          return UMFILECRAY;
       }
#endif

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
       if ((rbuf1 >= -90.0 && rbuf1 <= 90.0) &&
           (rbuf2 >= 0.0 && rbuf2 <= 360.0))
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
       if ((fp = fopen(file, "r")) == NULL)
       {
          printf("Error opening file %s \n",file);
          return 0;
       }

       fseek(fp, 832l, SEEK_SET);
       fread(ibuf64, 8, 1, fp);
       swap_bytes(ibuf64,8,1);
       fseek(fp, 8*((long) ibuf64[0] + 1l), SEEK_SET);
       fread(rbuf, 8, 2, fp);

       if (fclose(fp) != 0)
       {
          printf("Error closing file %s \n",file);
          return 0;
       }

#ifdef LITTLE__ENDIAN
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
       if ((rbuf1 >= -90.0 && rbuf1 <= 90.0) &&
           (rbuf2 >= 0.0 && rbuf2 <= 360.0))
       {
          printf("file %s is a Cray um file \n",file);
          return UMFILECRAY;
       }
#endif

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
       if ((rbuf1 >= -90.0 && rbuf1 <= 90.0) &&
           (rbuf2 >= 0.0 && rbuf2 <= 360.0))
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
       return 0;
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
       return 0;
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
       if ((fp = fopen(file, "r")) == NULL)
       {
          printf("Error opening file %s \n",file);
          return 0;
       }

       fseek(fp, 448l, SEEK_SET);
       fread(rbuf, 8, 2, fp);

       if (fclose(fp) != 0)
       {
          printf("Error closing file %s \n",file);
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
       if ((rbuf1 >= -90.0 && rbuf1 <= 90.0) &&
           (rbuf2 >= 0.0 && rbuf2 <= 360.0))
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
       if ((rbuf1 >= -90.0 && rbuf1 <= 90.0) &&
           (rbuf2 >= 0.0 && rbuf2 <= 360.0))
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
       if ((fp = fopen(file, "r")) == NULL)
       {
          printf("Error opening file %s \n",file);
          return 0;
       }

       fseek(fp, 448l, SEEK_SET);
       fread(rbuf, 8, 2, fp);

       if (fclose(fp) != 0)
       {
          printf("Error closing file %s \n",file);
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
       if ((rbuf1 >= -90.0 && rbuf1 <= 90.0) &&
           (rbuf2 >= 0.0 && rbuf2 <= 360.0))
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
       if ((rbuf1 >= -90.0 && rbuf1 <= 90.0) &&
           (rbuf2 >= 0.0 && rbuf2 <= 360.0))
       {
          printf("file %s is a Cray blocked 64 bit ieee pp file \n",file);
          return PPFILECRAYIEEE;
       }
    }
#endif

    printf("Error file %s has unknown file type \n",file);
    return 0;
}
