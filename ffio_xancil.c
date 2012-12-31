#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "util_xancil.h"

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#ifdef LONG64
#define ZERO64 0l
#else
#define ZERO64 0ll
#endif 

#if _FILE_OFFSET_BITS == 64 || defined _LARGE_FILES
#define ftell ftello
#define fseek fseeko
#endif

#if __FORT_SYMBOL == _FS_UC
#define openff  OPENFF
#define closeff CLOSEFF
#define abortff ABORTFF
#define rdblki  RDBLKI
#define rdblkp  RDBLKP
#define rdblkr  RDBLKR
#define wrtblki WRTBLKI
#define wrtblkp WRTBLKP
#define wrtblkr WRTBLKR
#define skip    SKIP
#elif __FORT_SYMBOL == _FS_LC_
#define openff  openff_
#define closeff closeff_
#define abortff abortff_
#define rdblki  rdblki_
#define rdblkp  rdblkp_
#define rdblkr  rdblkr_
#define wrtblki wrtblki_
#define wrtblkp wrtblkp_
#define wrtblkr wrtblkr_
#define skip    skip_
#endif

#define BUFSIZE 4096
#define MAXFILESIZE 257

/* definition of file information structure */

typedef struct finfo_st {
	char     fname[MAXFILESIZE];   /* file name */
        char     ftype[4];             /* file type */
        FILE     *fp;                  /* file identifier */
} FILEINFO;

void openff(FILEINFO **, fpchar, fpchar, fpchar, int, int, int);
void closeff(FILEINFO **);
void abortff(FILEINFO **);
void rdblki(INTEGER *, INTEGER *, INTEGER *, FILEINFO **, BYTEOFF *, INTEGER *);
void rdblkp(INTEGER *, INTEGER *, INTEGER *, FILEINFO **, BYTEOFF *, INTEGER *);
void rdblkr(REAL *, INTEGER *, INTEGER *, FILEINFO **, BYTEOFF *, INTEGER *);
void wrtblki(INTEGER *, INTEGER *, INTEGER *, FILEINFO **, BYTEOFF *);
void wrtblkp(INTEGER *, INTEGER *, INTEGER *, FILEINFO **, BYTEOFF *);
void wrtblkr(REAL *, INTEGER *, INTEGER *, FILEINFO **, BYTEOFF *);
void skip(FILEINFO **, BYTEOFF *, BYTEOFF *);

/*
  ------------------------------------------------------------------------------
*/

void openff(FILEINFO **unit, fpchar file, fpchar mode, fpchar type, 
            int flen, int mlen, int tlen)
{
    char *p, *cfile, *cmode, *ctype;
    FILE *fp;

    /* Allocate memory for FILEINFO structure */

    if (((*unit) = (FILEINFO *) malloc(sizeof(FILEINFO))) == NULL)
    {
       printf("Error unable to allocate memory in openff \n");
       abort();
    }

    /* convert fortran CHARACTERs to c chars */

    cfile = ( char *) malloc(flen+1);
    strncpy(cfile, file, flen);
    cfile[flen] = '\0';
    cmode = ( char *) malloc(mlen+1);
    strncpy(cmode, mode, mlen);
    cmode[mlen] = '\0';
    ctype = ( char *) malloc(tlen+1);
    strncpy(ctype, type, tlen);
    ctype[tlen] = '\0';

    /* strip trailing blanks */

    p = cfile+flen-1;
    while(*p == ' ')
    {
       *p = '\0';
       p--;
    }
    p = cmode+mlen-1;
    while(*p == ' ')
    {
       *p = '\0';
       p--;
    }
    p = ctype+tlen-1;
    while(*p == ' ')
    {
       *p = '\0';
       p--;
    }

    /* open file */

    if ((fp = fopen(cfile, cmode)) == NULL)
    {
       printf("Error opening file %s \n",cfile);
       perror("fopen error");
       abort();
    }

    (*unit)->fp = fp;
    strcpy((*unit)->fname, cfile);
    strcpy((*unit)->ftype, ctype);

    free(cfile);
    free(cmode);
    free(ctype);

    return;
}
/*
  ------------------------------------------------------------------------------
*/
void closeff(FILEINFO **unit)
{
    int istat;

    istat = fclose((*unit)->fp);

    /* Free memory for FILEINFO structure */

    free(*unit);

    if (istat != 0)
    {
       printf("Error closing file \n");
       perror("fclose error");
       abort();
    }
    return;
}
/*
  ------------------------------------------------------------------------------
*/
void abortff(FILEINFO **unit)
{
    int istat;

    istat = fclose((*unit)->fp);

    /* Free memory for FILEINFO structure */

    free(*unit);

    if (istat != 0)
    {
       printf("Error closing file \n");
       perror("fclose error");
    }
    abort();
}
/*
  ------------------------------------------------------------------------------
*/
void rdblki(INTEGER *a, INTEGER *n1, INTEGER *n2, 
            FILEINFO **unit, BYTEOFF *pos, INTEGER *ieof)
{
    int ierr=0, intsize;
    char *ftype;
    int64 *buf64;
    int32 *buf32;

    *ieof=0;
    intsize = sizeof(INTEGER);

    if ((int) *n2 == 0) return;
    if (*n2 > *n1)
    {
       printf("*** ERROR Array size = %d size of data to be read = %d \n",
              (int) *n1,(int) *n2);
       ierr=1;
    }
    if ((int) *n2 < 0)
    {
       printf("*** ERROR Negative length read requested \n");
       ierr=1;
    }
    if (ierr != 0) abortff(unit);

    *pos = *pos + *n2;
    ftype = (*unit)->ftype;
    if (((strcmp(ftype, "IE8") == 0) || (strcmp(ftype, "IS8") == 0) || 
         (strcmp(ftype, "CR8") == 0)) && intsize == 4)
    {
       if ( (buf64 = malloc ((int) *n2*8)) == NULL )
       {
          printf("Error unable to allocate memory for buf64 in rdblki *n2 = %d \n", (int) *n2);
          abortff(unit);
       }

       ierr = fread(buf64, 8, (size_t) *n2, (*unit)->fp);
       if (ierr != (int) *n2)
       {
          if ( ! feof((*unit)->fp) )
          {
             printf("Error reading file \n");
             abortff(unit);
          }
          else
             *ieof=-1;
       }

       if (strcmp(ftype, "IS8") == 0)
          swap_bytes(buf64, 8, (int) *n2);

       if ((strcmp(ftype, "IE8") == 0) || (strcmp(ftype, "IS8") == 0))
          i8_to_i4(buf64, a, (int) *n2);
       else if (strcmp(ftype, "CR8") == 0)
          c8_to_i4(buf64, a, (int) *n2);

       free(buf64);
    }
    else if (((strcmp(ftype, "IE4") == 0) || 
              (strcmp(ftype, "IS4") == 0)) && intsize == 8)
    {
       if ( (buf32 = malloc (*n2*4)) == NULL )
       {
          printf("Error unable to allocate memory for buf32 in rdblki n2 = %d \n", (int) *n2);
          abortff(unit);
       }

       ierr = fread(buf32, 4, (size_t) *n2, (*unit)->fp);
       if (ierr != (int) *n2)
       {
          if ( ! feof((*unit)->fp) )
          {
             printf("Error reading file \n");
             abortff(unit);
          }
          else
             *ieof=-1;
       }

       if (strcmp(ftype, "IS4") == 0)
          swap_bytes(buf32, 4, (int) *n2);

       i4_to_i8(buf32, a, (int) *n2);

       free(buf32);
    }
    else
    {
       ierr = fread(a, intsize, (size_t) *n2, (*unit)->fp);
       if (ierr != (int) *n2)
       {
          if ( ! feof((*unit)->fp) )
          {
             printf("Error reading file \n");
             abortff(unit);
          }
          else
             *ieof=-1;
       }

       if (strcmp(ftype, "IS4") == 0)
          swap_bytes(a, 4, (int) *n2);
       else if (strcmp(ftype, "IS8") == 0)
          swap_bytes(a, 8, (int) *n2);
    }
    return;
}
/*
  ------------------------------------------------------------------------------
*/
void rdblkp(INTEGER *a, INTEGER *n1, INTEGER *n2, 
            FILEINFO **unit, BYTEOFF *pos, INTEGER *ieof)
{
    int ierr=0, intsize;
    char *ftype;

    *ieof=0;
    intsize = sizeof(INTEGER);

    if ((int) *n2 == 0) return;
    if (*n2 > *n1)
    {
       printf("*** ERROR Array size = %d size of data to be read = %d \n",
              (int) *n1,(int) *n2);
       ierr=1;
    }
    if ((int) *n2 < 0)
    {
       printf("*** ERROR Negative length read requested \n");
       ierr=1;
    }
    if (ierr != 0) abortff(unit);

    ftype = (*unit)->ftype;
    if (((strcmp(ftype, "IE8") == 0) || (strcmp(ftype, "IS8") == 0) || 
         (strcmp(ftype, "CR8") == 0)) && intsize == 4)
       *pos = *pos + *n2/2;
    else if (((strcmp(ftype, "IE4") == 0) || 
              (strcmp(ftype, "IS4") == 0)) && intsize == 8)
       *pos = *pos + (*n2)*2;
    else
       *pos = *pos + *n2;

    ierr = fread(a, intsize, (size_t) *n2, (*unit)->fp);
    if (ierr != (int) *n2)
    {
       if ( ! feof((*unit)->fp) )
       {
          printf("Error reading file \n");
          abortff(unit);
       }
       else
          *ieof=-1;
    }
    return;
}
/*
  ------------------------------------------------------------------------------
*/
void rdblkr(REAL *a, INTEGER *n1, INTEGER *n2, 
            FILEINFO **unit, BYTEOFF *pos, INTEGER *ieof)
{
    int ierr=0, fltsize;
    char *ftype;
    float32 *buf32;
    float64 *buf64;

    *ieof=0;
    fltsize = sizeof(REAL);

    if ((int) *n2 == 0) return;
    if (*n2 > *n1)
    {
       printf("*** ERROR Array size = %d size of data to be read = %d \n",
              (int) *n1,(int) *n2);
       ierr=1;
    }
    if ((int) *n2 < 0)
    {
       printf("*** ERROR Negative length read requested \n");
       ierr=1;
    }
    if (ierr != 0) abortff(unit);

    *pos = *pos + *n2;
    ftype = (*unit)->ftype;
    if (((strcmp(ftype, "IE4") == 0) || 
         (strcmp(ftype, "IS4") == 0)) && fltsize == 8)
    {
       if ( (buf32 = malloc ((int) *n2*4)) == NULL )
       {
          printf("Error unable to allocate memory for buf32 in rdblkr *n2 = %d \n", (int) *n2);
          abortff(unit);
       }

       ierr = fread(buf32, 4, (size_t) *n2, (*unit)->fp);
       if (ierr != (int) *n2)
       {
          if ( ! feof((*unit)->fp) )
          {
             printf("Error reading file \n");
             abortff(unit);
          }
          else
             *ieof=-1;
       }

       if (strcmp(ftype, "IS4") == 0)
          swap_bytes(buf32, 4, (int) *n2);

       r4_to_r8(buf32, a, (int) *n2);

       free(buf32);
    }
    else if (strcmp(ftype, "CR8") == 0 && fltsize == 8)
    {
       if ( (buf64 = malloc ((int) *n2*8)) == NULL )
       {
          printf("Error unable to allocate memory for buf64 in rdblkr *n2 = %d \n", (int) *n2);
          abortff(unit);
       }

       ierr = fread(buf64, 8, (size_t) *n2, (*unit)->fp);
       if (ierr != (int) *n2)
       {
          if ( ! feof((*unit)->fp) )
          {
             printf("Error reading file \n");
             abortff(unit);
          }
          else
             *ieof=-1;
       }
       c8_to_r8(buf64, a, (int) *n2);

       free(buf64);
    }
    else if (((strcmp(ftype, "IE8") == 0) || (strcmp(ftype, "IS8") == 0) || 
              (strcmp(ftype, "CR8") == 0)) && fltsize == 4)
    {
       if ( (buf64 = malloc ((int) *n2*8)) == NULL )
       {
          printf("Error unable to allocate memory for buf64 in rdblkr *n2 = %d \n", (int) *n2);
          abortff(unit);
       }

       ierr = fread(buf64, 8, (size_t) *n2, (*unit)->fp);
       if (ierr != (int) *n2)
       {
          if ( ! feof((*unit)->fp) )
          {
             printf("Error reading file \n");
             abortff(unit);
          }
          else
             *ieof=-1;
       }

       if (strcmp(ftype, "IS8") == 0)
          swap_bytes(buf64, 8, (int) *n2);

       if (strcmp(ftype, "IE8") == 0 || strcmp(ftype, "IS8") == 0)
          r8_to_r4(buf64, a, (int) *n2);
       else if (strcmp(ftype, "CR8") == 0)
          c8_to_r4(buf64, a, (int) *n2);

       free(buf64);
    }
    else
    {
       ierr = fread(a, fltsize, (size_t) *n2, (*unit)->fp);
       if (ierr != (int) *n2)
       {
          if ( ! feof((*unit)->fp) )
          {
             printf("Error reading file \n");
             abortff(unit);
          }
          else
             *ieof=-1;
       }
       if (strcmp(ftype, "IS8") == 0)
          swap_bytes(a, 8, (int) *n2);
       else if (strcmp(ftype, "IS4") == 0)
          swap_bytes(a, 4, (int) *n2);
    }
    return;
}
/*
  ------------------------------------------------------------------------------
*/
void wrtblki(INTEGER *a, INTEGER *n1, INTEGER *n2, 
             FILEINFO **unit, BYTEOFF *pos)
{
    int ierr=0, intsize;
    char *ftype;
    int64 *buf64;
    int32 *buf32;

    intsize = sizeof(INTEGER);

    if ((int) *n2 == 0) return;
    if (*n2 > *n1)
    {
       printf("*** ERROR Array size = %d size of data to be written = %d \n",
              (int) *n1,(int) *n2);
       ierr=1;
    }
    if ((int) *n2 < 0)
    {
       printf("*** ERROR Negative length write requested \n");
       ierr=1;
    }
    if (ierr != 0) abortff(unit);

    *pos = *pos + *n2;
    ftype = (*unit)->ftype;
    if (((strcmp(ftype, "IE8") == 0) || (strcmp(ftype, "IS8") == 0) ||
         (strcmp(ftype, "CR8") == 0)) && intsize == 4)
    {
       if ( (buf64 = malloc ((int) *n2*8)) == NULL )
       {
          printf("Error unable to allocate memory for buf64 in wrtblki *n2 = %d \n", (int) *n2);
          abortff(unit);
       }

       i4_to_i8(a, buf64, (int) *n2);

       if (strcmp(ftype, "IS8") == 0)
          swap_bytes(buf64, 8, (int) *n2);

       ierr = fwrite(buf64, 8, (size_t) *n2, (*unit)->fp);
       if (ierr != (int) *n2)
       {
          printf("Error reading file \n");
          abortff(unit);
       }

       free(buf64);
    }
    else if (((strcmp(ftype, "IE4") == 0) || 
              (strcmp(ftype, "IS4") == 0)) && intsize == 8)
    {
       if ( (buf32 = malloc (*n2*4)) == NULL )
       {
          printf("Error unable to allocate memory for buf32 in wrtblki *n2 = %d \n", (int) *n2);
          abortff(unit);
       }

       i8_to_i4(a, buf32, (int) *n2);

       if (strcmp(ftype, "IS4") == 0)
          swap_bytes(buf32, 4, (int) *n2);

       ierr = fwrite(buf32, 4, (size_t) *n2, (*unit)->fp);
       if (ierr != (int) *n2)
       {
          printf("Error reading file \n");
          abortff(unit);
       }

       free(buf32);
    }
    else
    {
       if (strcmp(ftype, "IS8") == 0)
          swap_bytes(a, 8, (int) *n2);
       else if (strcmp(ftype, "IS4") == 0)
          swap_bytes(a, 4, (int) *n2);

       ierr = fwrite(a, intsize, (size_t) *n2, (*unit)->fp);
       if (ierr != (int) *n2)
       {
          printf("Error writing file \n");
          abortff(unit);
       }

       if (strcmp(ftype, "IS8") == 0)
          swap_bytes(a, 8, (int) *n2);
       else if (strcmp(ftype, "IS4") == 0)
          swap_bytes(a, 4, (int) *n2);
    }

    return;
}
/*
  ------------------------------------------------------------------------------
*/
void wrtblkp(INTEGER *a, INTEGER *n1, INTEGER *n2, 
             FILEINFO **unit, BYTEOFF *pos)
{
    int ierr=0, intsize;
    char *ftype;

    intsize = sizeof(INTEGER);

    if ((int) *n2 == 0) return;
    if (*n2 > *n1)
    {
       printf("*** ERROR Array size = %d size of data to be written = %d \n",
              (int) *n1,(int) *n2);
       ierr=1;
    }
    if ((int) *n2 < 0)
    {
       printf("*** ERROR Negative length write requested \n");
       ierr=1;
    }
    if (ierr != 0) abortff(unit);

    ftype = (*unit)->ftype;
    if (((strcmp(ftype, "IE8") == 0) || (strcmp(ftype, "IS8") == 0) || 
        (strcmp(ftype, "CR8") == 0)) && intsize == 4)
       *pos = *pos + *n2/2;
    else if (((strcmp(ftype, "IE4") == 0) || 
              (strcmp(ftype, "IS4") == 0)) && intsize == 8)
       *pos = *pos + (*n2)*2;
    else
       *pos = *pos + *n2;

    ierr = fwrite(a, intsize, (size_t) *n2, (*unit)->fp);
    if (ierr != (int) *n2)
    {
       printf("Error writing file \n");
       abortff(unit);
    }

    return;
}
/*
  ------------------------------------------------------------------------------
*/
void wrtblkr(REAL *a, INTEGER *n1, INTEGER *n2, 
             FILEINFO **unit, BYTEOFF *pos)
{
    int ierr=0, fltsize;
    char *ftype;
    float32 *buf32;
    float64 *buf64;

    fltsize = sizeof(REAL);

    if ((int) *n2 == 0) return;
    if (*n2 > *n1)
    {
       printf("*** ERROR Array size = %d size of data to be written = %d \n",
              (int) *n1,(int) *n2);
       ierr=1;
    }
    if ((int) *n2 < 0)
    {
       printf("*** ERROR Negative length write requested \n");
       ierr=1;
    }
    if (ierr != 0) abortff(unit);

    *pos = *pos + *n2;
    ftype = (*unit)->ftype;
    if (((strcmp(ftype, "IE4") == 0) || 
         (strcmp(ftype, "IS4") == 0)) && fltsize == 8)
    {
       if ( (buf32 = malloc ((int) *n2*4)) == NULL )
       {
          printf("Error unable to allocate memory for buf32 in wrtblkr *n2 = %d \n", (int) *n2);
          abortff(unit);
       }

       r8_to_r4(a, buf32, (int) *n2);

       if (strcmp(ftype, "IS4") == 0)
          swap_bytes(buf32, 4, (int) *n2);

       ierr = fwrite(buf32, 4, (size_t) *n2, (*unit)->fp);
       if (ierr != *n2)
       {
          printf("Error reading file \n");
          abortff(unit);
       }

       free(buf32);
    }
    else if (((strcmp(ftype, "IE8") == 0) || 
              (strcmp(ftype, "IS8") == 0)) && fltsize == 4)
    {
       if ( (buf64 = malloc ((int) *n2*8)) == NULL )
       {
          printf("Error unable to allocate memory for buf64 in wrtblkr *n2 = %d \n", (int) *n2);
          abortff(unit);
       }

       r4_to_r8(a, buf64, (int) *n2);
       if (strcmp(ftype, "IS8") == 0)
          swap_bytes(buf64, 8, (int) *n2);

       ierr = fwrite(buf64, 8, (size_t) *n2, (*unit)->fp);
       if (ierr != (int) *n2)
       {
          printf("Error reading file \n");
          abortff(unit);
       }

       free(buf64);
    }
    else if ((strcmp(ftype, "CR8") == 0))
    {
       printf("Error cannot write CRAY numbers on this machine\n");
       abortff(unit);
    }
    else
    {
       if (strcmp(ftype, "IS4") == 0)
          swap_bytes(a, 4, (int) *n2);
       else if (strcmp(ftype, "IS8") == 0)
          swap_bytes(a, 8, (int) *n2);

       ierr = fwrite(a, fltsize, (size_t) *n2, (*unit)->fp);
       if (ierr != (int) *n2)
       {
          printf("Error writing file \n");
          abortff(unit);
       }

       if (strcmp(ftype, "IS4") == 0)
          swap_bytes(a, 4, (int) *n2);
       else if (strcmp(ftype, "IS8") == 0)
          swap_bytes(a, 8, (int) *n2);
    }

    return;
}
/*
  ------------------------------------------------------------------------------
*/
void skip(FILEINFO **unit, BYTEOFF *curpos, BYTEOFF *newpos)
{
    int istat, size;
    char *ftype;
    BYTEOFF offset;

    ftype = (*unit)->ftype;
    if ((strcmp(ftype, "IE8") == 0) || (strcmp(ftype, "IS8") == 0) || 
        (strcmp(ftype, "CR8") == 0))
       size = 8;
    else
       size = 4;

/*    
    offset = size * ( *newpos - *curpos );
    istat = fseek((*unit)->fp, offset, SEEK_CUR);
*/
    offset = size * ( *newpos - 1 );
    istat = fseek((*unit)->fp, offset, SEEK_SET);

    if (istat != 0)
    {
       printf("Error in skip \n");
       perror("fseek error");
       abortff(unit);
    }
    *curpos = *newpos;

    return;
}
