
!==============================================================================!

module ncinterface

interface
   subroutine nc_error(ierr,procname,name)
   integer ierr
   character, optional :: procname*(*),name*(*)
   end
end interface

end module ncinterface

!==============================================================================!

subroutine open_ncfile(file, mode, ncid, ierr)

use ncinterface

implicit none

include 'netcdf.inc'

character(*) file,mode
integer ncid,ierr

integer ncmode
logical lcreate

! Convert mode into netcdf mode

if (mode(1:2) == 'wd') then
   lcreate = .true.
   ncmode = NF_CLOBBER
elseif (mode(1:1) == 'w') then
   lcreate = .true.
   ncmode = NF_NOCLOBBER
elseif (mode(1:1) == 'a') then
   lcreate = .false.
   ncmode = NF_WRITE
elseif (mode(1:1) == 'r') then
   lcreate = .false.
   ncmode = NF_NOWRITE
else
   write(*,*) 'ERROR: opening netcdf file ',trim(file),' unknown mode ',mode
   stop
endif

! Open netcdf file

if (lcreate) then
   ierr = nf_create(file, ncmode, ncid)
else
   ierr = nf_open(file, ncmode, ncid)
endif
call nc_error(ierr,name=file,procname='open_ncfile 1')

return
end

!==============================================================================!

subroutine close_ncfile(ncid, ierr)

use ncinterface

implicit none

include 'netcdf.inc'

integer ncid,ierr

! Close netcdf file

ierr = nf_close(ncid)
if (ierr /= nf_noerr) call nc_error(ierr,procname='close_ncfile 1')

return
end

!==============================================================================!

logical function isncvarint(name,ncid)

use ncinterface

implicit none

include 'netcdf.inc'

character(*) name
integer ncid

integer type

call get_nctype(name,ncid,type)

if (type == NF_BYTE .or. type == NF_SHORT .or. type == NF_INT) then
   isncvarint = .true.
else
   isncvarint = .false.
endif

return
end

!==============================================================================!

subroutine get_ncatt_text(name, ncid, attname, atttext, lattexists)

use parameters
use ncinterface
use utils

implicit none

include 'netcdf.inc'

character(*) name,attname,atttext
integer ncid
logical lattexists

integer varid,ierr,type,attlen
character(max_attname_size) upattname

ierr = nf_inq_varid(ncid, name, varid)
if (ierr /= nf_noerr) &
   call nc_error(ierr,name=name,procname='get_ncatt_text 1')

! Check if attribute exists

ierr = nf_inq_att(ncid, varid, attname, type, attlen)
if (ierr /= nf_noerr) then

!  Try upper case version of attribute

   upattname = attname
   call upcase(upattname)
   ierr = nf_inq_att(ncid, varid, trim(upattname), type, attlen)
endif

if (ierr == nf_noerr .and. type == NF_CHAR) then
   if (attlen > len(atttext)) then
      lattexists = .false.
      write(*,*)'ERROR: getting attribute ',trim(attname),' failed'
      write(*,*)'Exceeded maximum attribute length = ',len(atttext)
      write(*,*)'Actual attribute length = ',attlen
      return
   endif
   lattexists = .true.
   ierr = nf_get_att_text(ncid, varid, attname, atttext)
   if (ierr /= nf_noerr) call nc_error(ierr,procname='get_ncatt_text 2')
else
   lattexists = .false.
endif

return
end

!==============================================================================!

subroutine get_ncatt_real(name, ncid, attname, attreal, lattexists)

use getkind
use parameters
use ncinterface
use utils

implicit none

include 'netcdf.inc'

character(*) name,attname
real(rtype) attreal(*)
integer ncid
logical lattexists

integer varid,ierr,type,attlen
character(max_attname_size) upattname

lattexists = .false.

ierr = nf_inq_varid(ncid, name, varid)
if (ierr /= nf_noerr) &
   call nc_error(ierr,name=name,procname='get_ncatt_real 1')

! Check if attribute exists

ierr = nf_inq_att(ncid, varid, attname, type, attlen)
if (ierr /= nf_noerr) then

!  Try upper case version of attribute

   upattname = attname
   call upcase(upattname)
   ierr = nf_inq_att(ncid, varid, trim(upattname), type, attlen)
endif

if (ierr == nf_noerr .and. (type == NF_FLOAT .or. type == NF_DOUBLE)) then
   lattexists = .true.
   if (rtype == r64) then
      ierr = nf_get_att_double(ncid, varid, attname, attreal)
   else
      ierr = nf_get_att_real(ncid, varid, attname, attreal)
   endif
   if (ierr /= nf_noerr) call nc_error(ierr,procname='get_ncatt_real 2')
else
   lattexists = .false.
endif

return
end

!==============================================================================!

subroutine get_ncdiminfo(name, ncid, nd, dimnames, ndims)

use ncinterface

implicit none

include 'netcdf.inc'

character(*) name,dimnames(4)
integer ncid,nd(4),ndims

integer ierr,varid,dimids(4),idim

ierr = nf_inq_varid(ncid, name, varid)
if (ierr /= nf_noerr) &
   call nc_error(ierr,name=name,procname='get_ncdiminfo 1')

ierr = nf_inq_varndims(ncid, varid, ndims)
if (ierr /= nf_noerr) call nc_error(ierr,procname='get_ncdiminfo 2')

if (ndims < 2 .or. ndims > 4) then
   write(*,*)'Error: Number of dimensions for variable ',trim(name), &
             ' = ',ndims, ' should be 2,3 or 4'
   stop
endif

ierr = nf_inq_vardimid(ncid, varid, dimids)
if (ierr /= nf_noerr) call nc_error(ierr,procname='get_ncdiminfo 3')

do idim = 1,ndims
   ierr = nf_inq_dim(ncid, dimids(idim), dimnames(idim), nd(idim))
   if (ierr /= nf_noerr) call nc_error(ierr,procname='get_ncdiminfo 4')
enddo

return
end

!==============================================================================!

subroutine get_ncdimlen(dimname, ncid, dimlen)

use getkind
use ncinterface

implicit none

include 'netcdf.inc'

character(*) dimname
integer ncid,dimlen

integer ierr,dimid

ierr = nf_inq_dimid(ncid, dimname, dimid)
if (ierr /= nf_noerr) &
   call nc_error(ierr,name=dimname,procname='get_ncdimlen 1')

ierr = nf_inq_dimlen(ncid, dimid, dimlen)
if (ierr /= nf_noerr) &
   call nc_error(ierr,name=dimname,procname='get_ncdimlen 2')

return
end

!==============================================================================!

subroutine get_ncdim(dimname, ncid, dim)

use getkind
use ncinterface

implicit none

include 'netcdf.inc'

character(*) dimname
integer ncid
real(rtype) dim(*)

integer ierr, varid

ierr = nf_inq_varid(ncid, dimname, varid)
if (ierr /= nf_noerr) call nc_error(ierr,name=dimname,procname='get_ncdim 1')

if (rtype == r64) then
   ierr = nf_get_var_double(ncid, varid, dim)
else
   ierr = nf_get_var_real(ncid, varid, dim)
endif
if (ierr /= nf_noerr) call nc_error(ierr,procname='get_ncdim 2')

return
end

!==============================================================================!

subroutine get_ncdim1(dimname, ncid, index, dim)

use getkind
use ncinterface

implicit none

include 'netcdf.inc'

character(*) dimname
integer ncid, index
real(rtype) dim

integer ierr, varid

ierr = nf_inq_varid(ncid, dimname, varid)
if (ierr /= nf_noerr) call nc_error(ierr,name=dimname,procname='get_ncdim1 1')

if (rtype == r64) then
   ierr = nf_get_var1_double(ncid, varid, index, dim)
else
   ierr = nf_get_var1_real(ncid, varid, index, dim)
endif
if (ierr /= nf_noerr) call nc_error(ierr,procname='get_ncdim1 2')

return
end

!==============================================================================!

logical function isncmdiatt(name,ncid)

use ncinterface

implicit none

include 'netcdf.inc'

integer ncid
character(*) name

integer ierr, varid, attlen
character att*13

ierr = nf_inq_varid(ncid, name, varid)
if (ierr /= nf_noerr) call nc_error(ierr,name=name,procname='isncmdiatt')

att = 'missing_value'
ierr = nf_inq_attlen(ncid, varid, att, attlen)
if (ierr /= nf_noerr) then
   att = '_FillValue'
   ierr = nf_inq_attlen(ncid, varid, att, attlen)
endif

isncmdiatt = ierr == nf_noerr

return
end

!==============================================================================!

subroutine get_ncmdi_r(name,ncid,rmdi_nc)

use getkind
use ncinterface

implicit none

include 'netcdf.inc'

integer ncid
character(*) name
real(rtype) rmdi_nc

integer ierr, varid, vartype, atttype
character att*13
real(r32) rmdi_32_nc
real(r64) rmdi_64_nc

ierr = nf_inq_varid(ncid, name, varid)
if (ierr /= nf_noerr) call nc_error(ierr,name=name,procname='get_ncmdi_r')
ierr = nf_inq_vartype(ncid, varid, vartype)
if (ierr /= nf_noerr) call nc_error(ierr,name=name,procname='get_ncmdi_r')

att = 'missing_value'
ierr = nf_inq_atttype(ncid, varid, att, atttype)
if (ierr /= nf_noerr) then
   att = '_FillValue'
   ierr = nf_inq_atttype(ncid, varid, att, atttype)
   if (ierr /= nf_noerr) then
      write(*,*)'Error: No missing data value attribute for variable ', &
                trim(name)
      stop
   endif
endif

if (atttype /= vartype) then
   write(*,*)'Warning: variable ',trim(name), &
             ' has different variable type to its attribute ',att
endif
if (atttype /= vartype .and. vartype == NF_REAL) then ! 32 bit real type
   ierr = nf_get_att_real(ncid, varid, att, rmdi_32_nc)
   rmdi_nc = rmdi_32_nc
else if (atttype /= vartype .and. vartype == NF_DOUBLE) then ! 64 bit real type
   ierr = nf_get_att_double(ncid, varid, att, rmdi_64_nc)
   rmdi_nc = rmdi_64_nc
else if (rtype == r64) then
   ierr = nf_get_att_double(ncid, varid, att, rmdi_nc)
else
   ierr = nf_get_att_real(ncid, varid, att, rmdi_nc)
endif

return
end

!==============================================================================!

subroutine get_ncmdi_i(name,ncid,imdi_nc)

use getkind
use ncinterface

implicit none

include 'netcdf.inc'

integer ncid
character(*) name
integer imdi_nc

integer ierr, varid, vartype, atttype
character att*13

ierr = nf_inq_varid(ncid, name, varid)
if (ierr /= nf_noerr) call nc_error(ierr,name=name,procname='get_ncmdi_i')
ierr = nf_inq_vartype(ncid, varid, vartype)
if (ierr /= nf_noerr) call nc_error(ierr,name=name,procname='get_ncmdi_i')

att = 'missing_value'
ierr = nf_inq_atttype(ncid, varid, att, atttype)
if (ierr /= nf_noerr) then
   att = '_FillValue'
   ierr = nf_inq_atttype(ncid, varid, att, atttype)
   if (ierr /= nf_noerr) then
      write(*,*)'Error: No missing data value attribute for variable ', &
                trim(name)
      stop
   endif
endif

if (atttype /= vartype) then
   write(*,*)'Warning: variable ',trim(name), &
             ' has different variable type to its attribute ',att
endif
ierr = nf_get_att_int(ncid, varid, att, imdi_nc)

return
end

!==============================================================================!

subroutine get_ncstartdate(name, ncid, istartdate, intunit, iscale)

use ncinterface
use utils

implicit none

include 'netcdf.inc'

character(*) name
integer ncid, istartdate(6), intunit, iscale

integer ierr, varid, type, unitslen, index1, index2, getmon, time_originlen
character units_att*5, units1*100, units*100
character time_origin_att*11, time_origin*100

ierr = nf_inq_varid(ncid, name, varid)
if (ierr /= nf_noerr) &
   call nc_error(ierr,name=name,procname='get_ncstartdate 1')

units_att = 'units'
ierr = nf_inq_atttype(ncid, varid, units_att, type)
if (ierr /= nf_noerr) then
   units_att = 'UNITS'
   ierr = nf_inq_atttype(ncid, varid, units_att, type)
   if (ierr /= nf_noerr) then
      write(*,*)'Error: No units attribute for variable ',name
      stop
   endif
endif
if (type /= NF_CHAR) then
   write(*,*)'Error: units attribute is not a character type for variable ',name
   stop
endif

ierr = nf_get_att_text(ncid, varid, units_att, units)
if (ierr /= nf_noerr) call nc_error(ierr,procname='get_ncstartdate 2')

ierr = nf_inq_attlen(ncid, varid, units_att, unitslen)
if (ierr /= nf_noerr) call nc_error(ierr,procname='get_ncstartdate 3')
if (ichar(units(unitslen:unitslen)) == 0) unitslen = unitslen-1

call locase(units)
index1 = index(units, 'since')
if (index1 /= 0) then

!  Extract unit from units string

   units1 = units(1:index1-2)

!  Extract reference date from units string

   index1 = index1 + 6
   index2 = index1 + index(units(index1:unitslen),'-') - 2
   if (index2 >= index1) then
      read(units(index1:index2),'(i4)') istartdate(1)

      index1 = index2 + 2
      index2 = index1 + index(units(index1:unitslen),'-') - 2
      if (index2 >= index1) then
         read(units(index1:index2),'(i2)') istartdate(2)

         index1 = index2 + 2
         index2 = index1 + index(units(index1:unitslen),' ') - 2
         if (index2 >= index1) then
            read(units(index1:index2),'(i2)') istartdate(3)

            index1 = index2 + 2
            index2 = index1 + index(units(index1:unitslen),':') - 2
            if (index2 >= index1) then
               read(units(index1:index2),'(i2)') istartdate(4)

               index1 = index2 + 2
               index2 = index1 + index(units(index1:unitslen),':') - 2
               if (index2 >= index1) then
                  read(units(index1:index2),'(i2)') istartdate(5)

                  index1 = index2 + 2
                  index2 = unitslen
                  if (index2 >= index1) then
                     read(units(index1:index2),'(i2)') istartdate(6)
                  else
                     istartdate(6) = 0
                  endif
               else
                  read(units(index1:unitslen),'(i2)') istartdate(5)
                  istartdate(6) = 0
               endif
            else
               read(units(index1:unitslen),'(i2)') istartdate(4)
               istartdate(5) = 0
               istartdate(6) = 0
            endif
         else
            read(units(index1:unitslen),'(i2)') istartdate(3)
            istartdate(4) = 0
            istartdate(5) = 0
            istartdate(6) = 0
         endif
      else
         read(units(index1:unitslen),'(i2)') istartdate(2)
         istartdate(3) = 1
         istartdate(4) = 0
         istartdate(5) = 0
         istartdate(6) = 0
      endif
   else
      read(units(index1:unitslen),'(i4)') istartdate(1)
      istartdate(2) = 1
      istartdate(3) = 1
      istartdate(4) = 0
      istartdate(5) = 0
      istartdate(6) = 0
   endif
else

   units1 = units

   ! Get reference date from time_origin attribute if present

   time_origin_att = 'time_origin'
   ierr = nf_inq_atttype(ncid, varid, time_origin_att, type)
   if (ierr /= nf_noerr) then
      time_origin_att = 'TIME_ORIGIN'
      ierr = nf_inq_atttype(ncid, varid, time_origin_att, type)
      if (ierr /= nf_noerr) then
         write(*,*)'ERROR: No time_origin attribute for variable ',trim(name), &
                   ' therefore cannot determine dates'
         stop
      endif
   endif
   if (type /= NF_CHAR) then
      write(*,*)'ERROR: time_origin attribute is not a character type for variable ',trim(name)
      stop
   endif

   ierr = nf_get_att_text(ncid, varid, time_origin_att, time_origin)
   if (ierr /= nf_noerr) call nc_error(ierr,procname='get_ncstartdate 4')

   ierr = nf_inq_attlen(ncid, varid, time_origin_att, time_originlen)
   if (ierr /= nf_noerr) call nc_error(ierr,procname='get_ncstartdate 5')
   if (ichar(time_origin(time_originlen:time_originlen)) == 0) &
      time_originlen = time_originlen-1

   read(time_origin(8:11),'(i4)')istartdate(1)
   istartdate(2) = getmon(time_origin(4:6))
   read(time_origin(1:2),'(i2)')istartdate(3)
   if (time_originlen > 12) then
      read(time_origin(13:14),'(i2)')istartdate(4)
   else
      istartdate(4) = 0
   endif
   if (time_originlen > 15) then
      read(time_origin(16:17),'(i2)')istartdate(5)
   else
      istartdate(5) = 0
   endif
   if (time_originlen > 18) then
      read(time_origin(19:20),'(i2)')istartdate(6)
   else
      istartdate(6) = 0
   endif

endif

if (index(units1, 'year') /= 0) then
   iscale = 1
   intunit = 0
elseif (index(units1, 'mon') /= 0) then
   iscale = 1
   intunit = 1
elseif (index(units1, 'day') /= 0) then
   iscale = 24*60*60
   intunit = 2
else if (index(units1, 'hour') /= 0) then
   iscale = 60*60
   intunit = 3
else if (index(units1, 'min') /= 0) then
   iscale = 60
   intunit = 4
else if (index(units1, 'sec') /= 0) then
   iscale = 1
   intunit = 5
endif

return
end

!==============================================================================!

subroutine get_ncvar_r(name, ncid, data)

use getkind
use ncinterface

implicit none

include 'netcdf.inc'

character(*) name
integer ncid
real(rtype) data(*)

integer ierr, varid

ierr = nf_inq_varid(ncid, name, varid)
if (ierr /= nf_noerr) call nc_error(ierr,name=name,procname='get_ncvar_r 1')

if (rtype == r64) then
   ierr = nf_get_var_double(ncid, varid, data)
else
   ierr = nf_get_var_real(ncid, varid, data)
endif
if (ierr /= nf_noerr) call nc_error(ierr,procname='get_ncvar_r 2')

return
end


!==============================================================================!

subroutine get_ncdata_r(name, ncid, iz0, itsel, grid, dim, &
                        nx, ny, nz, nt, data)

use getkind
use types
use ncinterface

implicit none

include 'netcdf.inc'

character(*) name
integer ncid,dim(4),nx,ny,nz,nt
integer iz0,itsel(nt)
type(gridinfo) grid
real(rtype) data(nx,ny,nz,nt)

integer iy,iz,it,nx1,ierr,varid
integer start(4),count(4),stride(4),imap(4)
real(rtype) dum(nx)

ierr = nf_inq_varid(ncid, name, varid)
if (ierr /= nf_noerr) call nc_error(ierr,name=name,procname='get_ncdata_r 1')

stride = 1

if (grid%model == 2 .and. nx == grid%nlong) then
   nx1 = nx-2
else
   nx1 = nx
endif

if (dim(1) > 0) then
   start(dim(1)) = 1
   count(dim(1)) = nx1
   imap(dim(1)) = 1
endif
if (dim(2) > 0) then
   start(dim(2)) = 1
   count(dim(2)) = ny
   imap(dim(2)) = nx
endif
if (dim(3) > 0) then
   start(dim(3)) = iz0
   count(dim(3)) = nz
   imap(dim(3)) = nx*ny
endif
if (dim(4) > 0) then
   count(dim(4)) = 1
   imap(dim(4)) = nx*ny*nz
endif

do it=1,nt
   if (dim(4) > 0) then
      start(dim(4)) = itsel(it)
   endif

   if (rtype == r64) then
      ierr = nf_get_varm_double(ncid, varid, start, count, stride, imap, &
                                data(1,1,1,it))
   else
      ierr = nf_get_varm_real(ncid, varid, start, count, stride, imap, &
                              data(1,1,1,it))
   endif
   if (ierr /= nf_noerr) call nc_error(ierr,procname='get_ncdata_r 2')

!  Reverse latitude order of data

   if (grid%llatrev) then
      do iz=1,nz
         do iy=1,ny/2
            dum = data(:,iy,iz,it)
            data(:,iy,iz,it) = data(:,ny-iy+1,iz,it)
            data(:,ny-iy+1,iz,it) = dum
         enddo
      enddo
   endif

!  Shift longitude values to be 0-360

   if (grid%offset_long > 0) then
      data(1:nx1,:,:,it) = cshift(data(1:nx1,:,:,it),grid%offset_long,1)
   endif

!  Add 2 extra columns for ocean data

   if (grid%model == 2 .and. nx == grid%nlong) then
      do iz=1,nz
         data(nx-1,:,iz,it) = data(1,:,iz,it)
         data(nx,:,iz,it) = data(2,:,iz,it)
      enddo
   endif
enddo

return
end

!==============================================================================!

subroutine get_ncdata_i(name, ncid, iz0, itsel, grid, dim, &
                        nx, ny, nz, nt, data)

use types
use ncinterface

implicit none

include 'netcdf.inc'

character(*) name
integer ncid,dim(4),nx,ny,nz,nt
integer iz0,itsel(nt)
type(gridinfo) grid
integer data(nx,ny,nz,nt)

integer iy,iz,it,nx1,ierr,varid
integer start(4),count(4),stride(4),imap(4)
integer dum(nx)

ierr = nf_inq_varid(ncid, name, varid)
if (ierr /= nf_noerr) call nc_error(ierr,name=name,procname='get_ncdata_i 1')

stride = 1

if (grid%model == 2 .and. nx == grid%nlong) then
   nx1 = nx-2
else
   nx1 = nx
endif

if (dim(1) > 0) then
   start(dim(1)) = 1
   count(dim(1)) = nx1
   imap(dim(1)) = 1
endif
if (dim(2) > 0) then
   start(dim(2)) = 1
   count(dim(2)) = ny
   imap(dim(2)) = nx
endif
if (dim(3) > 0) then
   start(dim(3)) = iz0
   count(dim(3)) = nz
   imap(dim(3)) = nx*ny
endif
if (dim(4) > 0) then
   count(dim(4)) = 1
   imap(dim(4)) = nx*ny*nz
endif

do it=1,nt
   if (dim(4) > 0) then
      start(dim(4)) = itsel(it)
   endif

   ierr = nf_get_varm_int(ncid, varid, start, count, stride, imap, &
                          data(1,1,1,it))
   if (ierr /= nf_noerr) call nc_error(ierr,procname='get_ncdata_i 2')

!  Reverse latitude order of data

   if (grid%llatrev) then
      do iz=1,nz
         do iy=1,ny/2
            dum = data(:,iy,iz,it)
            data(:,iy,iz,it) = data(:,ny-iy+1,iz,it)
            data(:,ny-iy+1,iz,it) = dum
         enddo
      enddo
   endif

!  Rotate longitude values to be 0-360

   if (grid%offset_long > 0) then
      data(1:nx1,:,:,it) = cshift(data(1:nx1,:,:,it),grid%offset_long,1)
   endif

!  Add 2 extra columns for ocean data

   if (grid%model == 2 .and. nx == grid%nlong) then
      do iz=1,nz
         data(nx-1,:,iz,it) = data(1,:,iz,it)
         data(nx,:,iz,it) = data(2,:,iz,it)
      enddo
   endif
enddo

return
end

!==============================================================================!

subroutine get_nctype(name,ncid,type)

use ncinterface

implicit none

include 'netcdf.inc'

character(*) name
integer ncid,type

integer ierr,varid

! Get netcdf datatype

ierr = nf_inq_varid(ncid, name, varid)
if (ierr /= nf_noerr) call nc_error(ierr,name=name,procname='get_nctype 1')

ierr = nf_inq_vartype(ncid, varid, type)
if (ierr /= nf_noerr) call nc_error(ierr,procname='get_nctype 2')

return
end

!==============================================================================!

subroutine get_varname_from_stdname(ncid,stdname,varname,ierr)

use parameters
use ncinterface

implicit none

include 'netcdf.inc'

integer ncid,ierr
character(*) stdname,varname

integer varid,nvars,attlen
character(max_stdname_size) stdname1

attlen=len_trim(stdname)

ierr = nf_inq_nvars(ncid,nvars)
if (ierr /= nf_noerr) call nc_error(ierr,procname='get_varname_from_stdname 1')

do varid=1,nvars
   ierr = nf_get_att_text(ncid,varid,"standard_name",stdname1)
   if (ierr == nf_noerr .and. stdname1(1:attlen) == stdname(1:attlen)) then
      ierr = nf_inq_varname(ncid,varid,varname)
      ierr = 0
      return
   endif
enddo

varname = ""
ierr = 1

return
end

!==============================================================================!

subroutine get_nccal(ncid,varname,ical)

use ncinterface

implicit none

include 'netcdf.inc'

integer ncid,ical
character(*) varname

integer ierr,varid,type,month_lengths
character calendar_att*8, calendar*16, month_lengths_att*13

ical = 1 ! Assume default of Gregorian calendar
type = NF_FLOAT

ierr = nf_inq_varid(ncid, varname, varid)
if (ierr /= nf_noerr) call nc_error(ierr,name=varname,procname='get_cal 1')

calendar_att = 'calendar'
ierr = nf_inq_atttype(ncid, varid, calendar_att, type)
if (ierr /= nf_noerr) then
   calendar_att = 'CALENDAR'
   ierr = nf_inq_atttype(ncid, varid, calendar_att, type)
endif
if (type == NF_CHAR) then
   ierr = nf_get_att_text(ncid, varid, calendar_att, calendar)
   if (ierr /= nf_noerr) call nc_error(ierr,procname='get_nccal 2')

   if (calendar(1:3) == '360') ical = 2
   return
endif

calendar_att = 'calendar'
ierr = nf_inq_atttype(ncid, NF_GLOBAL, calendar_att, type)
if (ierr /= nf_noerr) then
   calendar_att = 'CALENDAR'
   ierr = nf_inq_atttype(ncid, NF_GLOBAL, calendar_att, type)
endif
if (type == NF_CHAR) then
   ierr = nf_get_att_text(ncid, NF_GLOBAL, calendar_att, calendar)
   if (ierr /= nf_noerr) call nc_error(ierr,procname='get_nccal 3')

   if (calendar(1:3) == '360') ical = 2
   return
endif

month_lengths_att = 'month_lengths'
ierr = nf_inq_atttype(ncid, varid, month_lengths_att, type)
if (ierr /= nf_noerr) then
   month_lengths_att = 'MONTH_LENGTHS'
   ierr = nf_inq_atttype(ncid, varid, month_lengths_att, type)
endif
if (type == NF_BYTE .or. type == NF_SHORT .or. type == NF_INT) then
   ierr = nf_get_att_int(ncid, varid, month_lengths_att, month_lengths)
   if (ierr /= nf_noerr) call nc_error(ierr,procname='get_nccal 4')

   if (month_lengths == 30) ical = 2
endif

return
end

!==============================================================================!

subroutine get_ncfield_r(ncid,varname,ilev,model, &
                         itimeusage1,itimeusage2,istartdate1, &
                         data,nsize)

use getkind
use types

implicit none

character(*) varname
integer ncid,ilev,model,itimeusage1,itimeusage2,istartdate1(6),nsize
real(rtype) data(nsize)

integer, save :: dim(4),nx,ny,it(1)
type(gridinfo), save :: grid

! Get dimension values

if (ilev == 1) then
   call get_field_info(ncid,varname,model,nsize, &
                       itimeusage1,itimeusage2,istartdate1, &
                       grid,dim,nx,ny,it(1))
endif

! Get data values

call get_ncdata_r(varname,ncid,ilev,it,grid,dim,nx,ny,1,1,data)

return
end

!==============================================================================!

subroutine get_ncfield_i(ncid,varname,ilev,model, &
                         itimeusage1,itimeusage2,istartdate1, &
                         data,nsize)

use getkind
use types

implicit none

character(*) varname
integer ncid,ilev,model,itimeusage1,itimeusage2,istartdate1(6),nsize
integer(itype) data(nsize)

integer, save :: dim(4),nx,ny,it(1)
type(gridinfo), save :: grid

! Get dimension values

if (ilev == 1) then
   call get_field_info(ncid,varname,model,nsize, &
                       itimeusage1,itimeusage2,istartdate1, &
                       grid,dim,nx,ny,it(1))
endif

! Get data values

call get_ncdata_i(varname,ncid,ilev,it,grid,dim,nx,ny,1,1,data)

return
end

!==============================================================================!

subroutine get_field_info(ncid,varname,model,nsize, &
                          itimeusage1,itimeusage2,istartdate1, &
                          grid,dim,nx,ny,it)

use getkind
use parameters
use types

implicit none

character(*) varname
integer ncid,model,type,nsize,itimeusage1,itimeusage2,istartdate1(6)
integer dim(4),nx,ny,it
type(gridinfo) grid

integer intunit,iscale,ical
integer nz,nt,istartdate2(6),idate(6)
real(rtype), dimension(:), allocatable :: time
character(max_varname_size) dimnames(4)

! Get dimension values

type = 1  ! data on atmospheric theta points

call get_gridinfo(varname,ncid,dimnames,dim,nz,nt,model,type,grid)
nx = grid%nlong
ny = grid%nlat
write(*,*)'dim = ',dim
write(*,*)'nx,ny,nz,nt = ',nx,ny,nz,nt
if (nsize /= nx*ny) then
   write(*,*)'ERROR: field size from Netcdf file (',nx*ny, &
             ') doesn''t match dump field size (',nsize,')'
   stop
endif

if (itimeusage1 == 1 .and. itimeusage2 == 1) then
   if (dim(4) < 1) then
      write(*,*)'ERROR: Cannot read date from NetCDF file ', &
                'as it has no time dimension'
      stop 
   endif

   allocate(time(nt))

   call get_ncdim(dimnames(4),ncid,time)
   write(*,*)'time = ',time

   call get_ncstartdate(dimnames(4),ncid,istartdate2,intunit,iscale)
   write(*,*)'istartdate2 = ',istartdate2
   write(*,*)'intunit = ',intunit
   write(*,*)'iscale = ',iscale

   call get_nccal(ncid,varname,ical)
   write(*,*)'ical = ',ical

   call setdumpdate(itimeusage1,itimeusage2,intunit,ical,iscale, &
                    istartdate1,istartdate2,time,nt,it,idate)
   write(*,*)'idate = ',idate

   deallocate(time)
else
   it = 1
endif

write(*,*)'it = ',it

return
end

!==============================================================================!

subroutine get_ncdate(ncid,varname,idate)

use getkind
use parameters

implicit none

character(*) varname
integer ncid,idate(6)

integer intunit,iscale,ical,istartdate(6)
integer ndims,nd(4),dim(4),dimid,i
integer(i64) incr
real(rtype) time
character(max_varname_size) dimnames(4),dimname

! Get calendar type from NetCDF file

call get_nccal(ncid,varname,ical)
write(*,*)'ical = ',ical

! Get time dimension name and id

call get_ncdiminfo(varname,ncid,nd,dimnames,ndims)
call getdimid(varname,ncid,dimnames,dim,ndims)

dimid = -1
do i=1,ndims
   if (dim(i) == 4) then
      dimname = dimnames(i)
      dimid = i
      exit
   endif
enddo
if (dimid < 1) then
   write(*,*)'ERROR: Cannot read date from NetCDF file ', &
             'as it has no time dimension'
   stop 
endif
write(*,*)'dimname = ',dimname
write(*,*)'dimid = ',dimid


! Get first time value from NetCDF file

call get_ncdim1(dimname, ncid, 1, time)
write(*,*)'time = ',time

! Get startdate from NetCDF file

call get_ncstartdate(dimname,ncid,istartdate,intunit,iscale)
write(*,*)'istartdate = ',istartdate
write(*,*)'intunit = ',intunit
write(*,*)'iscale = ',iscale

! Get first date from NetCDF file

if (intunit == 2 .or. intunit == 3 .or. intunit == 4) intunit = 5
incr = nint(time*iscale)
call incr_date(intunit,ical,istartdate,idate,incr,.false.)

write(*,*)'idate = ',idate

return
end

!==============================================================================!

subroutine nc_error(ierr,procname,name)

implicit none

include 'netcdf.inc'

integer ierr
character, optional :: procname*(*),name*(*)

character(256) string

if (ierr /= nf_noerr) then

   if (present(procname) .and. len_trim(procname) > 0) then
      string = 'in procedure '//trim(procname)//' :'
   else
      string = ''
   endif
   
   if (present(name)) then
      write(*,*) 'ERROR: '//trim(string)//' NetCDF error number ',ierr,': ', &
                 trim(nf_strerror(ierr)),' : ',trim(name)
   else if (ierr /= nf_noerr) then
      write(*,*) 'ERROR: '//trim(string)//' NetCDF error number ',ierr,': ', &
                 trim(nf_strerror(ierr))
   endif

   stop
   !call abort
endif
   
return
end
