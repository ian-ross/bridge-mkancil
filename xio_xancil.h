#if !defined(XIO_XANCIL_HDR)
#define XIO_XANCIL_HDR

#include "util_xancil.h"
#include <stdarg.h>

#if _FILE_OFFSET_BITS == 64 || defined _LARGE_FILES
#define XADDRESS off_t
#define XSIZE off_t
#else
#define XADDRESS long
#define XSIZE long
#endif

#ifdef LONG64
#define ZERO64 0l
#else
#define ZERO64 0ll
#endif

#define UMFILE 1
#define PPFILE 2
#define PPFILEI32R64 34
#define UMFILEBS 201
#define PPFILEBS 202
#define UMFILE64 65
#define PPFILE64 66
#define UMFILE64BS 265
#define PPFILE64BS 266
#define UMFILECRAY 165
#define PPFILECRAY 166
#define PPFILECRAYIEEE 167

#define TRUE 1
#define FALSE 0
#define MAXFILESIZE 257

/* definition of fortran file identifier */

typedef struct ffid_st {
        int               unit;        /* fortran unit number */
        int               pos;         /* file position in records */
} FFID;

/* definition of COS blocked fortran file identifier */

typedef struct cosid_st {
        COSFILE           *cosfp;      /* COS blocked file pointer */
        int               pos;         /* file position in records */
} COSID;

/* definition of PP file identifier */

typedef struct ppfile_st {
	char            *fname;   /* file name */
	FILE            *fp;      /* current file */
	XADDRESS        pos;      /* file position in bytes */
        char            swap;     /* = 0 file is not byte swapped
                                     = n file is byte swapped n bytes */
} PPFILEID;

/* definition of file identifier */

typedef union fid_u {
        FILE              *fp;         /* std c file pointer */
        FFID              *ffid;       /* fortran file identifier */
        COSID             *cosid;      /* COS blocked file identifier */
        PPFILEID          *ppid;       /* PP file identifier */
} FILEID;

/* definition of file information structure */

typedef struct finfo_st {
	char     fname[MAXFILESIZE];   /* file name */
        int      ftype;                /* file type */
        FILEID   *fid;                 /* file identifier */
} FILEINFO;

int get_type(char *);
int xreadint(FILEINFO *, INTEGER *, int);
int xreadreal(FILEINFO *, REAL *, int);
int xreadpack(FILEINFO *, int *, int);
int xreadpphead(FILEINFO *, INTEGER *, int, REAL *, int);
int xwritereal(FILEINFO *, REAL *, int);
FILEINFO *xopen(char *, char *, int);
int xclose(FILEINFO *);
XADDRESS xtell(FILEINFO *);
int xseek(FILEINFO *, XADDRESS);
int xrewind(FILEINFO *);
int xskip(FILEINFO *);
XADDRESS xftell (FILE *);
int xfseek (FILE *, XADDRESS, int);
void *free_x(void *);

PPFILEID *ppopen(char *, char *, int);
int ppclose(PPFILEID *);
int pprewind(PPFILEID *);
int ppbackspace(PPFILEID *);
int ppskip(PPFILEID *);
int ppseek(PPFILEID *, XADDRESS);
XADDRESS pptell(PPFILEID *);
int ppread(PPFILEID *, void *, int , int *);
int ppreadpack(PPFILEID *, void *, int , int *);
int ppreadhead(PPFILEID *, void *, int , int *, void *, int , int *);
int ppwrite (PPFILEID *, void *, int);

void xerror(char *, ...);
extern void (*xerror_func)(char *, va_list);

#endif

