
!==============================================================================!

subroutine write_head(ochan,pp_pos,data_pos,nfield,datasize,nftype,inthd8, &
                      intunit,interval,lperiodic,ztype,grid,idate,nt)

use getkind
use constants
use parameters
use config
use vert_soil
use vert_od
use vert_nd
use vert_ocean
use types

implicit none

integer(ptype) ochan
integer(otype) pp_pos,data_pos
integer nfield,datasize,nftype,inthd8,intunit,interval,nt
integer ztype !         = 1 - Single level
              !         = 2 - Model levels
              !   ztype = 3 - Ozone levels
              !         = 4 - Soil levels
              !         = 5 - Pseudo levels
              !         = 6 - Depth levels
integer idate(6,nt)
logical lperiodic
type(gridinfo) grid

integer(itype) fixhd(len_fixhd),inthd(len_inthd)
real(rtype) realhd(len_realhd),tmp(nlev+1)
real(rtype), dimension(:,:), allocatable ::  levdepc

integer(itype) len1_levdepc,len2_levdepc
integer icount,nrec,interval2,get_daynum,nx,ny,isign
integer(otype) opos,npos
real(rtype) dx,dy,x0,y0

nx = grid%nlong
ny = grid%nlat
x0 = mod(grid%along(1),360.0_rtype)
y0 = grid%alat(1)
if (nx > 1) then
   dx = abs(grid%along(2) - grid%along(1))
else
   dx = 360.0_rtype
endif
if (ny > 1) then
   dy = abs(grid%alat(2) - grid%alat(1))
else
   dy = 0.0_rtype
endif

! integer/real headers must use Theta/mass grid values

if (grid%model == 2 .or. iversion >= 500) then
   isign = -1
else
   isign = 1
endif
if (grid%type == 2) then ! UV points on Arakwa B grid
   if (nx > 1) then
      x0 = x0 - 0.5_rtype*dx
   endif
   if (ny > 1) then
      ny = ny+1
      y0 = y0 + isign*0.5_rtype*dy
   endif
else if (grid%type == 3) then ! U points on Arakwa C grid
   if (nx > 1) then
      x0 = x0 - 0.5_rtype*dx
   endif
else if (grid%type == 4) then ! V points on Arakwa C grid
   if (ny > 1) then
      ny = ny+1
      y0 = y0 + isign*0.5_rtype*dy
   endif
endif

if (ztype == 1 .or. ztype == 5) then
   len1_levdepc = 1
else if (ztype == 4) then
   len1_levdepc = nsoillev
else if (ztype == 6) then
   len1_levdepc = noclev
else if (iversion < 500) then
   len1_levdepc = nlev
else
   len1_levdepc = nlev+1
endif
if (ztype == 1 .or. ztype == 5 .or. ztype == 6 .or. &
   (ztype == 4 .and. iversion < 500)) then
   len2_levdepc = 1
else
   len2_levdepc = 4
endif

! Fill in values for fixed length header

call init_iarray(fixhd,len_fixhd,imdi)

if (grid%model == 3) then
   fixhd(2) = 1
else
   fixhd(2) = grid%model
endif
if (grid%model == 2) then
   fixhd(3) = 4
else
   fixhd(3) = 1
   !if (iversion < 500) then
   !   fixhd(3) = 1
   !else
   !   fixhd(3) = 5
   !endif
endif
if (grid%lglobal) then
   fixhd(4) = 0
else
   fixhd(4) = 3
endif
if (grid%lrotate) fixhd(4) = fixhd(4)+100
fixhd(5) = 4
fixhd(6) = 1
fixhd(7) = 1
fixhd(8) = ical
if (iversion < 500) then
   fixhd(9) = 2
else
   fixhd(9) = 3
endif
if (nt == 1) then
   fixhd(10) = 0
else if (lperiodic) then
   fixhd(10) = 2
else
   fixhd(10) = 1
endif
fixhd(12) = iversion
fixhd(21) = idate(1,1)
fixhd(22) = idate(2,1)
fixhd(23) = idate(3,1)
fixhd(24) = idate(4,1)
fixhd(25) = idate(5,1)
fixhd(26) = idate(6,1)
fixhd(27) = get_daynum(idate(1,1),idate(2,1),idate(3,1),ical)
fixhd(28) = idate(1,nt)
fixhd(29) = idate(2,nt)
fixhd(30) = idate(3,nt)
fixhd(31) = idate(4,nt)
fixhd(32) = idate(5,nt)
fixhd(33) = idate(6,nt)
fixhd(34) = get_daynum(idate(1,nt),idate(2,nt),idate(3,nt),ical)
fixhd(35) = 0
fixhd(36) = 0
fixhd(37) = 0
fixhd(38) = 0
fixhd(39) = 0
fixhd(40) = 0
fixhd(41) = 0
if (nt > 1) then
   if (ical == 2) then
      interval2 = interval
      if (intunit == 0) then ! years
         fixhd(35) = interval2
      elseif (intunit == 1) then ! months
         fixhd(35) = interval2 / 12
         interval2 = interval2 - 12*fixhd(35)
         fixhd(36) = interval2
      elseif (intunit == 2) then ! days
         fixhd(35) = interval2 / 360
         interval2 = interval2 - 360*fixhd(35)
         fixhd(36) = interval2 / 30
         interval2 = interval2 - 30*fixhd(36)
         fixhd(37) = interval2
      elseif (intunit == 3) then ! hours
         fixhd(35) = interval2 / 8640
         interval2 = interval2 - 8640*fixhd(35)
         fixhd(36) = interval2 / 720
         interval2 = interval2 - 720*fixhd(36)
         fixhd(37) = interval2 / 24
         interval2 = interval2 - 24*fixhd(37)
         fixhd(38) = interval2
      elseif (intunit == 4) then ! minutes
         fixhd(35) = interval2 / 518400
         interval2 = interval2 - 518400*fixhd(35)
         fixhd(36) = interval2 / 43200
         interval2 = interval2 - 43200*fixhd(36)
         fixhd(37) = interval2 / 1440
         interval2 = interval2 - 1440*fixhd(37)
         fixhd(38) = interval2 / 60
         interval2 = interval2 - 60*fixhd(38)
         fixhd(39) = interval2
      elseif (intunit == 5) then ! seconds
         fixhd(35) = interval2 / 31104000
         interval2 = interval2 - 31104000*fixhd(35)
         fixhd(36) = interval2 / 2592000
         interval2 = interval2 - 2592000*fixhd(36)
         fixhd(37) = interval2 / 86400
         interval2 = interval2 - 86400*fixhd(37)
         fixhd(38) = interval2 / 3600
         interval2 = interval2 - 3600*fixhd(38)
         fixhd(39) = interval2 / 60
         interval2 = interval2 - 60*fixhd(39)
         fixhd(40) = interval2
      endif
   else
      fixhd(35+intunit) = interval
   endif
endif
icount = len_fixhd + 1
fixhd(100) = icount
fixhd(101) = len_inthd
icount = icount + len_inthd
fixhd(105) = icount
fixhd(106) = len_realhd
icount = icount + len_realhd
if (ztype /= 1 .and. ztype /= 5) then
   fixhd(110) = icount
   icount = icount + len1_levdepc*len2_levdepc
endif
fixhd(111) = len1_levdepc
fixhd(112) = len2_levdepc
if (lvargrid) then
   fixhd(115) = icount
   fixhd(116) = ny
   fixhd(117) = 1
   icount = icount + ny
   fixhd(120) = icount
   fixhd(121) = nx
   fixhd(122) = 1
   icount = icount + nx
else
   fixhd(116) = 1
   fixhd(117) = 1
   fixhd(121) = 1
   fixhd(122) = 1
endif
fixhd(126) = 1
fixhd(127) = 1
fixhd(131) = 1
fixhd(136) = 1
fixhd(141) = 1
fixhd(143) = 1
fixhd(145) = 1
fixhd(150) = icount
fixhd(151) = len_pphead_int+len_pphead_real
fixhd(152) = nfield
icount = icount + (len_pphead_int+len_pphead_real)*nfield
if (lwfio .and. iwfio_size > 1) then
   nrec =  icount / iwfio_size
   if (mod(icount,iwfio_size) /= 0) nrec = nrec + 1
   icount = nrec*iwfio_size + 1
endif
fixhd(160) = icount
fixhd(161) = datasize

!write(*,*) 'fixhd = ',fixhd

! Fill in values for integer header

call init_iarray(inthd,len_inthd,imdi)

inthd(3) = nt
inthd(6) = nx
inthd(7) = ny
if (inthd8 > 0) then
   inthd(8) = inthd8
else if (ztype == 1 .or. ztype == 5) then
   inthd(8) = 1
else if (ztype == 2) then
   inthd(8) = nlev
else if (ztype == 3) then
   inthd(8) = no3lev
else if (ztype == 4) then
   inthd(8) = nsoillev
else if (ztype == 6) then
   inthd(8) = noclev
endif
inthd(15) = nftype

!write(*,*) 'inthd = ',inthd

! Fill in values for real header

realhd(1) = dx
realhd(2) = dy
realhd(3) = y0
realhd(4) = x0
realhd(5) = grid%north_pole(2)
realhd(6) = grid%north_pole(1)

!write(*,*) 'realhd = ',realhd

! Fill in values for level dependent header

if (ztype /= 1 .and. ztype /= 5) then
   allocate (levdepc(len1_levdepc,len2_levdepc))
   call init_rarray(levdepc,len1_levdepc*len2_levdepc,rmdi)
endif

if (ztype == 4) then
   if (iversion < 500) then
      levdepc(:,1) = soillev(1:nsoillev)
   else
      levdepc(:,4) = soillev(1:nsoillev)
   endif
else if (ztype == 6) then
   levdepc(:,1) = thickness(1:noclev)
else if (ztype /= 1 .and. ztype /= 5) then
   if (iversion < 500) then
      levdepc(:,1) = ak(1:nlev)
      levdepc(:,2) = bk(1:nlev)
      tmp = cshift(akh,1)
      levdepc(:,3) = tmp(1:nlev) - akh(1:nlev)
      tmp = cshift(bkh,1)
      levdepc(:,4) = tmp(1:nlev) - bkh(1:nlev)
   else
      levdepc(:,1) = eta_theta(0:nlev)
      levdepc(1:nlev,2) = eta_rho(1:nlev)
   endif

   !write(*,*) 'levdepc(:,1) = ',levdepc(:,1)
   !write(*,*) 'levdepc(:,2) = ',levdepc(:,2)
   !write(*,*) 'levdepc(:,3) = ',levdepc(:,3)
   !write(*,*) 'levdepc(:,4) = ',levdepc(:,4)

endif

! write out header values

opos=1

! Write fixed length header
call wrtblki(fixhd,len_fixhd,len_fixhd,ochan,opos)

! Write integer constants

if (fixhd(100) > 0) then
   npos = fixhd(100)
   call skip(ochan,opos,npos)
   call wrtblki(inthd,len_inthd,len_inthd,ochan,opos)
endif

! Write real constants

if (fixhd(105) > 0) then
   npos = fixhd(105)
   call skip(ochan,opos,npos)
   call wrtblkr(realhd,len_realhd,len_realhd,ochan,opos)
endif

! Write level dependent constants

if (fixhd(110) > 0) then
   npos = fixhd(110)
   call skip(ochan,opos,npos)
   call wrtblkr(levdepc,len1_levdepc*len2_levdepc,len1_levdepc*len2_levdepc, &
                ochan,opos)
endif

if (lvargrid) then

!  Write row dependent constants

   npos = fixhd(115)
   call skip(ochan,opos,npos)
   call wrtblkr(grid%alat,ny,ny,ochan,opos)

!  Write column dependent constants

   npos = fixhd(120)
   call skip(ochan,opos,npos)
   call wrtblkr(grid%along,nx,nx,ochan,opos)

endif

pp_pos = fixhd(150)
data_pos = fixhd(160)

if (ztype /= 1 .and. ztype /= 5) deallocate (levdepc)

return
end

!==============================================================================!

subroutine write_pphead(ochan,newpos,idate,iz,ztype,ltheta,fieldcode,proccode, &
                        levcode,fftype,fflev,datapos,istart,datatype, &
                        stashcode,grid)

use getkind
use constants
use parameters
use config
use vert_soil
use vert_od
use vert_nd
use vert_ocean
use types

implicit none

integer(ptype) ochan
integer(otype) newpos,datapos
integer iz,fieldcode,proccode,levcode,fftype,fflev
integer ztype !         = 1 - Single level
              !         = 2 - Model levels
              !   ztype = 3 - Ozone levels
              !         = 4 - Soil levels
              !         = 5 - Pseudo levels
              !         = 6 - Depth levels
integer istart,datatype,stashcode
integer idate(6)
logical ltheta
type(gridinfo) grid

integer(itype) ilookup(len_pphead_int)
real(rtype) rlookup(len_pphead_real)

integer(otype) curpos
integer nx,ny,idaynum,get_daynum,nrec,datasize
logical lpack

nx = grid%nlong
ny = grid%nlat

lpack = l32bit .and. abs(datatype) == 1

call skip(ochan,curpos,newpos)

! Fill in values for pp header

call init_iarray(ilookup,len_pphead_int,0)
call init_rarray(rlookup,len_pphead_real,0.0_rtype)

idaynum = get_daynum(idate(1),idate(2),idate(3),ical)

ilookup(1:5) = idate(1:5)
ilookup(6) = idaynum
ilookup(7:11) = idate(1:5)
ilookup(12) = idaynum
ilookup(13) = ical
ilookup(15) = nx*ny
ilookup(16) = 1
if (grid%lrotate) ilookup(16) = ilookup(16)+100
if (grid%lglobal) then
   ilookup(17) = 0
else
   ilookup(17) = 3
endif
ilookup(18) = ny
ilookup(19) = nx
if (lpack) then
   ilookup(21) = 2
else
   ilookup(21) = 0
endif
ilookup(22) = 2
ilookup(23) = fieldcode
ilookup(25) = proccode
ilookup(26) = levcode
if (lwfio) then
   ilookup(29) = datapos - 1
   if (lpack) then
      datasize = (nx*ny+1)/2
   else
      datasize = nx*ny
   endif
   nrec = datasize / iwfio_size
   if (mod(datasize,iwfio_size) /= 0) nrec = nrec + 1
   ilookup(30) = nrec*iwfio_size
else
   ilookup(29) = 0
   ilookup(30) = 0
endif
ilookup(32) = fftype
ilookup(33) = fflev
if (iversion < 503) then
   ilookup(38) = 1111
else
   ilookup(38) = iversion*10000 + 1111
endif
ilookup(39) = datatype
ilookup(40) = istart
ilookup(42) = stashcode
ilookup(45) = grid%model

if (ztype == 2 .or. ztype == 3) then
   if (iversion < 500) then
      rlookup(1) = bkh(iz+1)
      rlookup(2) = akh(iz+1)
      rlookup(7) = bk(iz)
      rlookup(8) = bkh(iz)
      rlookup(9) = ak(iz)
      rlookup(10) = akh(iz)
   else if (iversion < 502) then
      if (ltheta) then
         rlookup(1) = eta_rho(iz+1)
         rlookup(7) = eta_theta(iz)
         rlookup(8) = eta_rho(iz)
      else
         rlookup(1) = eta_theta(iz)
         rlookup(7) = eta_rho(iz)
         rlookup(8) = eta_theta(iz-1)
      endif
   else
      if (ltheta) then
         rlookup(1) = zsea_rho(iz+1)
         rlookup(2) = c_rho(iz+1)
         rlookup(7) = zsea_theta(iz)
         rlookup(9) = c_theta(iz)
         if (iz == 1) then

!           Special case, lower boundary to first theta level is the surface

            rlookup(8) = 0.0_rtype
            rlookup(10) = 1.0_rtype
         else
            rlookup(8) = zsea_rho(iz)
            rlookup(10) = c_rho(iz)
         endif
      else
         rlookup(1) = zsea_theta(iz)
         rlookup(2) = c_theta(iz)
         rlookup(7) = zsea_rho(iz)
         rlookup(8) = zsea_theta(iz-1)
         rlookup(9) = c_rho(iz)
         rlookup(10) = c_theta(iz-1)
      endif
   endif
else if (ztype == 4) then
   rlookup(7) = soillev(iz)
else if (ztype == 5) then
   ilookup(43) = iz
else if (ztype == 6) then
   rlookup(7) = depth(iz)
endif

rlookup(11) = grid%north_pole(2)
rlookup(12) = grid%north_pole(1)
if (lvargrid) then
   rlookup(14) = rmdi
   rlookup(15) = rmdi
   rlookup(16) = rmdi
   rlookup(17) = rmdi
else
   if (nx > 1) then
      rlookup(17) = grid%along(2) - grid%along(1)
   else
      rlookup(17) = 360.0_rtype
   endif
   if (ny > 1) then
      rlookup(15) = grid%alat(2) - grid%alat(1)
   else
      rlookup(15) = 0.0_rtype
   endif
   rlookup(14) = grid%alat(1) - rlookup(15)
   rlookup(16) = grid%along(1) - rlookup(17)
endif
rlookup(18) = rmdi
rlookup(19) = 1.0_rtype

call wrtblki(ilookup,len_pphead_int,len_pphead_int,ochan,curpos)
call wrtblkr(rlookup,len_pphead_real,len_pphead_real,ochan,curpos)

newpos = curpos

return
end

!==============================================================================!

subroutine write_data_r(ochan,newpos,data,nx,ny)

use getkind
use config

implicit none

integer(ptype) ochan
integer(otype) newpos
integer nx,ny
real(rtype) data(nx,ny)

integer(otype) curpos
integer(itype) datasize,datasize2,n1,n2
integer nrec, ierr
real(rtype) dum(iwfio_size)
real(r32), dimension(:), allocatable :: data32

call skip(ochan,curpos,newpos)

if (l32bit) then
   datasize = (nx*ny+1)/2
   allocate (data32(2*datasize))
   if (rtype == r32) then
      data32 = transfer(data,data32,nx*ny)
   else
      call r8tor4(data,data32,nx*ny,ierr)
   endif
   if (2*datasize == nx*ny+1) data32(2*datasize) = 0.0_r32
   if (lbigend .neqv. lbigendout) then
      call swapbytes(data32,4,2*datasize)
   endif
   if (itype == i32) then
      datasize2 = 2*datasize
      call wrtblkp(data32,datasize2,datasize2,ochan,curpos)
   else
      call wrtblkp(data32,datasize,datasize,ochan,curpos)
   endif
   deallocate (data32)
else
   datasize = nx*ny
   call wrtblkr(data,datasize,datasize,ochan,curpos)
endif

if (lwfio .and. iwfio_size > 1) then
   nrec = datasize / iwfio_size
   if (mod(datasize,iwfio_size) /= 0) nrec = nrec + 1
   dum = 0.0_rtype
   n1 = iwfio_size
   n2 = nrec*iwfio_size-datasize
   call wrtblkr(dum,n1,n2,ochan,curpos)
endif

newpos = curpos

return
end

!==============================================================================!

subroutine write_data_i(ochan,newpos,data,nx,ny)

use getkind
use config

implicit none

integer(ptype) ochan
integer(otype) newpos
integer(itype) nsize,n1,n2
integer nx,ny
integer(itype) data(nx,ny),dum(iwfio_size)

integer(otype) curpos
integer nrec

nsize = nx*ny

call skip(ochan,curpos,newpos)

call wrtblki(data,nsize,nsize,ochan,curpos)

if (lwfio .and. iwfio_size > 1) then
   dum = 0.0_rtype
   nrec = nx*ny / iwfio_size
   if (mod(nx*ny,iwfio_size) /= 0) nrec = nrec + 1
   n1 = iwfio_size
   n2 = nrec*iwfio_size-nx*ny
   call wrtblki(dum,n1,n2,ochan,curpos)
endif

newpos = curpos

return
end

!==============================================================================!

subroutine write_data_p(ochan,newpos,data,nx,ny)

use getkind
use config

implicit none

integer(ptype) ochan
integer(otype) newpos
integer(itype) nsize,n1,n2
integer nx,ny
integer(itype) data(nx,ny),dum(iwfio_size)

integer(otype) curpos
integer nrec

nsize = nx*ny

call skip(ochan,curpos,newpos)

call wrtblkp(data,nsize,nsize,ochan,curpos)

if (lwfio .and. iwfio_size > 1) then
   dum = 0.0_rtype
   nrec = nx*ny / iwfio_size
   if (mod(nx*ny,iwfio_size) /= 0) nrec = nrec + 1
   n1 = iwfio_size
   n2 = nrec*iwfio_size-nx*ny
   call wrtblkp(dum,n1,n2,ochan,curpos)
endif

newpos = curpos

return
end

!==============================================================================!

subroutine read_data_r(ochan,newpos,data,nx,ny)

use getkind
use config

implicit none

integer(ptype) ochan
integer(otype) newpos
integer nx,ny
integer(rtype) data(nx,ny)

integer(otype) curpos
integer(itype) ieof,nsize

nsize = nx*ny

call skip(ochan,curpos,newpos)

call rdblkr(data,nsize,nsize,ochan,curpos,ieof)

newpos = curpos

return
end

!==============================================================================!

subroutine read_data_i(ochan,newpos,data,nx,ny)

use getkind
use config

implicit none

integer(ptype) ochan
integer(otype) newpos
integer nx,ny
integer(itype) data(nx,ny)

integer(otype) curpos
integer(itype) ieof,nsize

nsize = nx*ny

call skip(ochan,curpos,newpos)

call rdblki(data,nsize,nsize,ochan,curpos,ieof)

newpos = curpos

return
end

!==============================================================================!

subroutine read_data_p(ochan,newpos,data,nx,ny)

use getkind
use config

implicit none

integer(ptype) ochan
integer(otype) newpos
integer nx,ny
integer(itype) data(nx,ny)

integer(otype) curpos
integer(itype) ieof,nsize

nsize = nx*ny

call skip(ochan,curpos,newpos)

call rdblkp(data,nx*ny,nx*ny,ochan,curpos,ieof)

newpos = curpos

return
end

!==============================================================================!

subroutine readwrite_head_r(ichan,ochan,inewpos,onewpos, &
                            data,nsize,modarr,modidx,ierr)

use getkind
use config

implicit none

integer(ptype) ichan,ochan
integer(otype) inewpos,onewpos
integer nsize,ierr,modidx(*)
real(rtype) data(nsize),modarr(*)

integer(otype) curpos
integer(itype) ieof,nsize1
integer i
real(rtype) temp

ierr = 0
nsize1 = nsize

call skip(ichan,curpos,inewpos)
call rdblkr(data,nsize1,nsize1,ichan,curpos,ieof)
inewpos = curpos
if (ieof < 0) then
   write(*,*)'End of file reached reading data in readwritedata_r'
   ierr = 1
   return
endif

i = 1
do while (modidx(i) > 0)
   if (modidx(i) > nsize) then
      write(*,*) 'Error: Out of bounds in readwritedata_r'
      ierr = 2
      return
   endif
   temp = data(modidx(i))
   data(modidx(i)) = modarr(i)
   modarr(i) = temp
   i = i + 1
enddo

call skip(ochan,curpos,onewpos)
call wrtblkr(data,nsize1,nsize1,ochan,curpos)
onewpos = curpos

return
end

!==============================================================================!

subroutine readwrite_head_i(ichan,ochan,inewpos,onewpos, &
                            data,nsize,modarr,modidx,ierr)

use getkind
use config

implicit none

integer(ptype) ichan,ochan
integer(otype) inewpos,onewpos
integer nsize,ierr,modidx(*)
integer(itype) data(nsize),modarr(*)

integer(otype) curpos
integer(itype) ieof,nsize1,temp
integer i

ierr = 0
nsize1 = nsize

call skip(ichan,curpos,inewpos)
call rdblki(data,nsize1,nsize1,ichan,curpos,ieof)
inewpos = curpos
if (ieof < 0) then
   write(*,*)'End of file reached reading data in readwritedata_r'
   ierr = 1
   return
endif

i = 1
do while (modidx(i) > 0)
   if (modidx(i) > nsize) then
      write(*,*) 'Error: Out of bounds in readwritedata_r'
      ierr = 2
      return
   endif
   temp = data(modidx(i))
   data(modidx(i)) = modarr(i)
   modarr(i) = temp
   i = i + 1
enddo

call skip(ochan,curpos,onewpos)
call wrtblki(data,nsize1,nsize1,ochan,curpos)
onewpos = curpos

return
end

!==============================================================================!

subroutine init_iarray(ia,n,iv)

implicit none

integer n,iv
integer ia(n)

integer i

do i=1,n
   ia(i)=iv
enddo

return
end

!==============================================================================!

subroutine init_rarray(ra,n,rv)

use getkind

implicit none

integer n
real(rtype) ra(n),rv

integer i

do i=1,n
   ra(i)=rv
enddo

return
end

!==============================================================================!

integer function get_daynum(iyear,imon,iday,ical)

implicit none

integer iyear,imon,iday,ical

integer i
logical isleap
integer daytab(12)
save daytab

data daytab /31,28,31,30,31,30,31,31,30,31,30,31/

if (iyear == 0 .and. imon == 0 .and. iday == 0) then
   get_daynum = 0
   return
endif

if (ical == 2) then ! 360 day calendar
   get_daynum = (imon-1)*30 + iday
else ! Gregorian calendar
   get_daynum = iday
   do i=1,imon-1
      get_daynum = get_daynum + daytab(i)
      if (i == 2 .and. isleap(iyear)) get_daynum = get_daynum + 1
   enddo
endif

return
end
