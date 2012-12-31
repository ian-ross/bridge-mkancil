#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tcl.h>
#include <netcdf.h>

int GetppheadersCmd(ClientData, Tcl_Interp *, int , Tcl_Obj *CONST*);
int GetncvarinfoCmd(ClientData, Tcl_Interp *, int , Tcl_Obj *CONST*);
int GetncdiminfoCmd(ClientData, Tcl_Interp *, int , Tcl_Obj *CONST*);
int handle_error(Tcl_Interp *, int);

EXTERN int Xancil_Init(Tcl_Interp *interp)
{
    if (Tcl_InitStubs(interp, "8.1", 0) == NULL) {
       return TCL_ERROR;
    }

    /* Define tcl extension commands */

    Tcl_CreateObjCommand(interp, "getppheaders", GetppheadersCmd,
             (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateObjCommand(interp, "getncvarinfo", GetncvarinfoCmd,
             (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateObjCommand(interp, "getncdiminfo", GetncdiminfoCmd,
             (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    if (Tcl_PkgProvide(interp, "xancil", "1.0") != TCL_OK) {
	return TCL_ERROR;
    }

    return TCL_OK;
}

#include "util_xancil.h"
#include "xio_xancil.h"

#define TRUE 1
#define FALSE 0

#define IMDI -32768
#define N1 256
#define N2 46
#define N3 38
#define N4 45
#define N5 19

int GetppheadersCmd(ClientData clientdata, Tcl_Interp *interp,
                    int objc, Tcl_Obj *CONST objv[])
{
   char *filename;
   int filetype, umfile, ppfile, eof, ppstart, maxfield, nfield, nprog;
   int i, version, model;
   FILEINFO *finfo;
   INTEGER fixhd[N1], ilkup[N4];
   REAL rlkup[N5];
   Tcl_Obj *pphead, *obj_data, *obj_list, *obj_index;

   if (objc != 3)
   {
      Tcl_SetResult(interp, "Wrong number of arguments", TCL_STATIC);
      return TCL_ERROR;
   }

   printf("In GetppheadersCmd\n");

   filename = Tcl_GetStringFromObj(objv[1], NULL);

   printf("filename = %s \n",filename);

   filetype = get_type(filename);

   printf("filetype = %d \n",filetype);

   pphead = objv[2];
   if (Tcl_IsShared(pphead)) pphead = Tcl_DuplicateObj(pphead);

   umfile = FALSE;
   ppfile = FALSE;
   if (filetype == UMFILE || filetype == UMFILE64 || filetype == UMFILECRAY ||
       filetype == UMFILEBS || filetype == UMFILE64BS)
      umfile = TRUE;
   else if (filetype == PPFILE || filetype == PPFILE64 || 
            filetype == PPFILEBS || filetype == PPFILE64BS || 
            filetype == PPFILEI32R64 || filetype == PPFILECRAY || 
            filetype == PPFILECRAYIEEE)
      ppfile = TRUE;
   else
   {
      Tcl_SetResult(interp, "File is not in UM or PP format", TCL_STATIC);
      return TCL_ERROR;
   }

   if ((finfo = xopen(filename, "rb", filetype)) == NULL)
      return TCL_ERROR;

   if (umfile)
   {
      eof = xreadint(finfo, fixhd, N1);  /* read fixed header */
      model = fixhd[1];
      version = fixhd[11];
      ppstart = fixhd[149] - 1;
      maxfield = fixhd[151];
      nprog = fixhd[152];

      printf("ppstart = %d \n",ppstart);
      printf("maxfield = %d \n",maxfield);

      if (xseek(finfo, ppstart))
      {
         Tcl_SetResult(interp, "Error in xseek in getppheaders", TCL_STATIC);
         return TCL_ERROR;
      }
   }

   obj_index = Tcl_NewStringObj("model", -1);
   obj_data = Tcl_NewIntObj(model);
   if (Tcl_ObjSetVar2(interp, pphead, obj_index, obj_data, 
                      TCL_LEAVE_ERR_MSG) == NULL) return TCL_ERROR;

   obj_index = Tcl_NewStringObj("version", -1);
   obj_data = Tcl_NewIntObj(version);
   if (Tcl_ObjSetVar2(interp, pphead, obj_index, obj_data, 
                      TCL_LEAVE_ERR_MSG) == NULL) return TCL_ERROR;

   obj_index = Tcl_NewStringObj("nprog", -1);
   obj_data = Tcl_NewIntObj(nprog);
   if (Tcl_ObjSetVar2(interp, pphead, obj_index, obj_data, 
                      TCL_LEAVE_ERR_MSG) == NULL) return TCL_ERROR;

   nfield = 0;
   eof = 0;
   while (eof == 0)
   {
      eof = xreadpphead(finfo, ilkup, N4, rlkup, N5); /* read lookup table */

      if (eof > 0)
      {
         printf("ERROR: reading file %s errno = %d \n",filename,eof);
         return TCL_ERROR;
      }
      else if (eof < 0 || ilkup[0] == -99 || ilkup[0] == IMDI) 
         break;

      nfield++;

      obj_index = Tcl_NewIntObj(nfield);
      obj_data = Tcl_NewIntObj(ilkup[0]);
      obj_list = Tcl_NewListObj(1,&obj_data);
      for (i=1;i<N4;i++)
      {
         obj_data = Tcl_NewIntObj(ilkup[i]);
         if (Tcl_ListObjAppendElement(interp,obj_list,obj_data) != TCL_OK)
                 return TCL_ERROR;
      }
      for (i=0;i<N5;i++)
      {
         obj_data = Tcl_NewDoubleObj(rlkup[i]);
         if (Tcl_ListObjAppendElement(interp,obj_list,obj_data) != TCL_OK)
                 return TCL_ERROR;
      }
      if (Tcl_ObjSetVar2(interp, pphead, obj_index, obj_list, 
                         TCL_LEAVE_ERR_MSG) == NULL) return TCL_ERROR;

      if (ppfile)
      {
         if ((eof = xskip(finfo)) != 0)
         {
            printf("ERROR: skipping record in file %s errno = %d \n",filename,eof);
            return TCL_ERROR;
         }
      }
      if (umfile && nfield == maxfield) break;
   }

   obj_index = Tcl_NewStringObj("nfield", -1);
   obj_data = Tcl_NewIntObj(nfield);
   if (Tcl_ObjSetVar2(interp, pphead, obj_index, obj_data, 
                      TCL_LEAVE_ERR_MSG) == NULL) return TCL_ERROR;

   printf("nfield = %d \n",nfield);
   printf("nprog = %d \n",nprog);

   if (xclose(finfo) != 0)
     return TCL_ERROR;

   return TCL_OK;
}

int GetncvarinfoCmd(ClientData clientdata, Tcl_Interp *interp,
                    int objc, Tcl_Obj *CONST objv[])
{
   char *ncfilename;
   char var_name[NC_MAX_NAME+1], long_name[1024], standard_name[1024];
   int ncid, status, nvar, nvar1, ndim, dimid, i;
   size_t attlen;
   Tcl_Obj *ncinfo, *obj_index, *obj_data;
   Tcl_Obj *obj_list1, *obj_list2, *obj_list3;

   if (objc != 3)
   {
      Tcl_SetResult(interp, "Wrong number of arguments", TCL_STATIC);
      return TCL_ERROR;
   }

   ncfilename = Tcl_GetStringFromObj(objv[1], NULL);

   ncinfo = objv[2];
   if (Tcl_IsShared(ncinfo)) ncinfo = Tcl_DuplicateObj(ncinfo);

   if (handle_error(interp, nc_open(ncfilename, NC_NOWRITE, &ncid)) != NC_NOERR)
      return TCL_ERROR;

   if (handle_error(interp, nc_inq_nvars(ncid, &nvar)) != NC_NOERR)
      return TCL_ERROR;

   nvar1 = 0;
   for (i=0;i<nvar;i++)
   {
      if (handle_error(interp, nc_inq_varndims(ncid, i, &ndim)) != NC_NOERR)
         return TCL_ERROR;

      if (ndim < 1) continue;

      if (handle_error(interp, nc_inq_varname(ncid, i, var_name)) != NC_NOERR)
         return TCL_ERROR;

      if (ndim == 1 && nc_inq_dimid(ncid, var_name, &dimid) == NC_NOERR) 
         continue;

      if (nc_inq_attlen(ncid, i, "long_name", &attlen) == NC_NOERR)
      {
         if (handle_error(interp, nc_get_att_text(ncid, i, 
                          "long_name", long_name)) != NC_NOERR)
            return TCL_ERROR;

         long_name[attlen] = '\0';
      }
      else
         long_name[0] = '\0';

      if (nc_inq_attlen(ncid, i, "standard_name", &attlen) == NC_NOERR)
      {
         if (handle_error(interp, nc_get_att_text(ncid, i, 
                          "standard_name", standard_name)) != NC_NOERR)
            return TCL_ERROR;

         standard_name[attlen] = '\0';
      }
      else
         standard_name[0] = '\0';

      obj_data = Tcl_NewStringObj(var_name, -1);
      if (nvar1 == 0)
         obj_list1 = Tcl_NewListObj(1,&obj_data);
      else
         if (Tcl_ListObjAppendElement(interp,obj_list1,obj_data) != TCL_OK)
            return TCL_ERROR;

      obj_data = Tcl_NewStringObj(long_name, -1);
      if (nvar1 == 0)
         obj_list2 = Tcl_NewListObj(1,&obj_data);
      else
         if (Tcl_ListObjAppendElement(interp,obj_list2,obj_data) != TCL_OK)
            return TCL_ERROR;

      obj_data = Tcl_NewStringObj(standard_name, -1);
      if (nvar1 == 0)
         obj_list3 = Tcl_NewListObj(1,&obj_data);
      else
         if (Tcl_ListObjAppendElement(interp,obj_list3,obj_data) != TCL_OK)
            return TCL_ERROR;

      nvar1++;
   }

   obj_index = Tcl_NewStringObj("nvar", -1);
   obj_data = Tcl_NewIntObj(nvar1);
   if (Tcl_ObjSetVar2(interp, ncinfo, obj_index, obj_data, 
                      TCL_LEAVE_ERR_MSG) == NULL) return TCL_ERROR;

   if (nvar1 > 0)
   {
      obj_index = Tcl_NewStringObj("var_name", -1);
      if (Tcl_ObjSetVar2(interp, ncinfo, obj_index, obj_list1, 
                         TCL_LEAVE_ERR_MSG) == NULL) return TCL_ERROR;

      obj_index = Tcl_NewStringObj("long_name", -1);
      if (Tcl_ObjSetVar2(interp, ncinfo, obj_index, obj_list2, 
                         TCL_LEAVE_ERR_MSG) == NULL) return TCL_ERROR;

      obj_index = Tcl_NewStringObj("standard_name", -1);
      if (Tcl_ObjSetVar2(interp, ncinfo, obj_index, obj_list3, 
                         TCL_LEAVE_ERR_MSG) == NULL) return TCL_ERROR;
   }

   if (handle_error(interp, nc_close(ncid)) != NC_NOERR)
      return TCL_ERROR;

   return TCL_OK;
}

int GetncdiminfoCmd(ClientData clientdata, Tcl_Interp *interp,
                    int objc, Tcl_Obj *CONST objv[])
{
   char *ncfilename;
   char var_name[NC_MAX_NAME+1], long_name[1024], standard_name[1024];
   int ncid, status, ndim1, nvar1, ndim, i, varid;
   size_t attlen;
   Tcl_Obj *ncinfo, *obj_index, *obj_data;
   Tcl_Obj *obj_list1, *obj_list2, *obj_list3;

   if (objc != 3)
   {
      Tcl_SetResult(interp, "Wrong number of arguments", TCL_STATIC);
      return TCL_ERROR;
   }

   ncfilename = Tcl_GetStringFromObj(objv[1], NULL);

   ncinfo = objv[2];
   if (Tcl_IsShared(ncinfo)) ncinfo = Tcl_DuplicateObj(ncinfo);

   if (handle_error(interp, nc_open(ncfilename, NC_NOWRITE, &ncid)) != NC_NOERR)
      return TCL_ERROR;

   if (handle_error(interp, nc_inq_ndims(ncid, &ndim1)) != NC_NOERR)
      return TCL_ERROR;

   nvar1 = 0;
   for (i=0;i<ndim1;i++)
   {
      if (handle_error(interp, nc_inq_dimname(ncid, i, var_name)) != NC_NOERR)
         return TCL_ERROR;

      if (handle_error(interp, nc_inq_varid(ncid, var_name, &varid)) != NC_NOERR)
         continue;

      if (handle_error(interp, nc_inq_varndims(ncid, varid, &ndim)) != NC_NOERR)
         return TCL_ERROR;

      if (ndim != 1) continue;

      if (nc_inq_attlen(ncid, varid, "long_name", &attlen) == NC_NOERR)
      {
         if (handle_error(interp, nc_get_att_text(ncid, varid, 
                          "long_name", long_name)) != NC_NOERR)
            return TCL_ERROR;

         long_name[attlen] = '\0';
      }
      else
         long_name[0] = '\0';

      if (nc_inq_attlen(ncid, varid, "standard_name", &attlen) == NC_NOERR)
      {
         if (handle_error(interp, nc_get_att_text(ncid, varid, 
                          "standard_name", standard_name)) != NC_NOERR)
            return TCL_ERROR;

         standard_name[attlen] = '\0';
      }
      else
         standard_name[0] = '\0';

      obj_data = Tcl_NewStringObj(var_name, -1);
      if (nvar1 == 0)
         obj_list1 = Tcl_NewListObj(1,&obj_data);
      else
         if (Tcl_ListObjAppendElement(interp,obj_list1,obj_data) != TCL_OK)
            return TCL_ERROR;

      obj_data = Tcl_NewStringObj(long_name, -1);
      if (nvar1 == 0)
         obj_list2 = Tcl_NewListObj(1,&obj_data);
      else
         if (Tcl_ListObjAppendElement(interp,obj_list2,obj_data) != TCL_OK)
            return TCL_ERROR;

      obj_data = Tcl_NewStringObj(standard_name, -1);
      if (nvar1 == 0)
         obj_list3 = Tcl_NewListObj(1,&obj_data);
      else
         if (Tcl_ListObjAppendElement(interp,obj_list3,obj_data) != TCL_OK)
            return TCL_ERROR;

      nvar1++;
   }

   obj_index = Tcl_NewStringObj("nvar", -1);
   obj_data = Tcl_NewIntObj(nvar1);
   if (Tcl_ObjSetVar2(interp, ncinfo, obj_index, obj_data, 
                      TCL_LEAVE_ERR_MSG) == NULL) return TCL_ERROR;

   if (nvar1 > 0)
   {
      obj_index = Tcl_NewStringObj("var_name", -1);
      if (Tcl_ObjSetVar2(interp, ncinfo, obj_index, obj_list1, 
                         TCL_LEAVE_ERR_MSG) == NULL) return TCL_ERROR;

      obj_index = Tcl_NewStringObj("long_name", -1);
      if (Tcl_ObjSetVar2(interp, ncinfo, obj_index, obj_list2, 
                         TCL_LEAVE_ERR_MSG) == NULL) return TCL_ERROR;

      obj_index = Tcl_NewStringObj("standard_name", -1);
      if (Tcl_ObjSetVar2(interp, ncinfo, obj_index, obj_list3, 
                         TCL_LEAVE_ERR_MSG) == NULL) return TCL_ERROR;
   }

   if (handle_error(interp, nc_close(ncid)) != NC_NOERR)
      return TCL_ERROR;

   return TCL_OK;
}

int handle_error(Tcl_Interp *interp, int status)
{
   if (status != NC_NOERR)
      Tcl_SetResult(interp, (char *) nc_strerror(status), TCL_VOLATILE); 

   return status;
}

