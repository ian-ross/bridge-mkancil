
!==============================================================================!

subroutine create_ostart(umfilein, umfileout, nncfiles1, oncfiles, &
                         itimeusage1, itimeusage2, istartdate, &
                         nitem, ncfileid, ncname, itemid, &
                         cloneitemid, newppcode, &
                         lusestdname, luseconfig, &
                         lbathy, bathyfile, bathyncname, lbathydepthmask, &
                         lireplace, liadd, islandsfile)
use getkind
use lsmask
use constants
use parameters
use config
use types

implicit none

integer nncfiles1,nitem,ncfileid(nitem+1),itemid(nitem+1)
integer cloneitemid(nitem+1),newppcode(nitem+1)
integer itimeusage1,itimeusage2,istartdate(6)
logical lusestdname,luseconfig
character(*) oncfiles(nncfiles1),umfileout,umfilein,ncname(nitem+1)
logical lbathy,lbathydepthmask,lireplace,liadd
character(*) bathyfile,bathyncname,islandsfile

integer(itype), dimension(:), allocatable ::  dum
logical l32bit_save,lwfio_save,lum,lswappack
integer iwfio_size_save,max_isize,max_rsize
integer i,ierr,model
integer, dimension(:), allocatable :: ncid
character(3) intype,outtype
integer(ptype) ichan,ochan
integer(itype) fixhdi(len_fixhd),fixhdo(len_fixhd),istashcode,eof,n1
integer(otype) onewpos,curpos,inewpos
integer header_size,nrec
character(max_stdname_size) stdname
character(max_varname_size) varname
integer get_daynum,date_time(8)
integer modidx(100),ihead_dim,rhead_dim
integer(itype), dimension(:), allocatable :: ihead
real(rtype), dimension(:), allocatable :: rhead
integer(itype) imodarr(100)
real(rtype) rmodarr(100)
integer(itype), dimension(:), allocatable :: data_pack,data_type
integer(itype), dimension(:), allocatable :: stash_code
integer(itype), dimension(:), allocatable :: data_size_i,data_size_o
integer(otype), dimension(:), allocatable :: data_pos_i,data_pos_o
integer(otype) data_pos0,data_pos1
integer(itype) ilookup(len_pphead_int)
real(rtype) rlookup(len_pphead_real)
integer data_size0,data_size1,ic
integer(itype) n2
integer(itype), dimension(:), allocatable :: idata
real(rtype), dimension(:), allocatable :: rdata
integer iitem,ilev
logical lreplace
integer(i32), dimension(:), allocatable :: itmp32
real(rtype), dimension(:), allocatable :: rtmp
logical lgotoceanlevels
integer noceanlevels
real(rtype), dimension(:), allocatable :: oceanlevels
real(rtype) rmdi_nc
integer imdi_nc


write (*,*) 'Writing Ocean start dump ', trim(umfileout)

! Don't use l32bit, only write 32 bit packed data if input UM dump
! data is packed.

l32bit_save = l32bit
l32bit = .false.
if (.not. luseconfig) then
   lwfio_save = lwfio
   iwfio_size_save = iwfio_size
   lwfio = .false.
   iwfio_size = 1
endif
write(*,*)'luseconfig = ',luseconfig
write(*,*)'lwfio = ',lwfio
write(*,*)'iwfio_size = ',iwfio_size
write(*,*)'nncfiles1 = ',nncfiles1
do i=1,nncfiles1
    write(*,*)'oncfiles(',i,') = ',trim(oncfiles(i))
enddo
write(*,*)'nitem = ',nitem
do i=1,nitem
   write(*,*)'ncname(',i,') = ',trim(ncname(i))
   write(*,*)'ncfileid(',i,') = ',ncfileid(i)
enddo

if (luseconfig .and. lwfio .and. iwfio_size > 1) then
   allocate (dum(iwfio_size))
   dum = 0
endif
max_isize = 0
max_rsize = 0
ncfileid(nitem+1) = -999
itemid(nitem+1) = -999
ncname(nitem+1) = ''
model = 2 ! Ocean model

! Open NetCDF files for later use

if (nitem > 0) then
   if (nncfiles1 == 0) then
      write (*,*) 'ERROR: No NetCDF files specified to overwrite '// &
                  'ocean start dump data'
      stop
   endif

   allocate (ncid(nncfiles1))
   do i = 1, nncfiles1
      call open_ncfile(oncfiles(i), 'r', ncid(i), ierr)
   enddo
endif

call pptype(umfilein,lum,intype)
write(*,*)'lum = ',lum
write(*,*)'intype = ',intype
if (.not. lum) then
   write(*,*)'ERROR: file ',trim(umfilein),' is not a UM file'
   stop
endif
if (intype(1:1) == 'C' .or. intype(1:1) == 'c') then
   write(*,*)'ERROR: Cannot modify a CRAY format UM file'
   stop
endif

if (luseconfig) then
   outtype(1:1) = 'I'
   if (lbigend .eqv. lbigendout) then
      outtype(2:2) = 'E'
   else
      outtype(2:2) = 'S'
   endif
   if (isize == 32) then
      outtype(3:3) = '4'
   else
      outtype(3:3) = '8'
   endif
else
   outtype = intype
endif
write(*,*)'outtype = ',outtype
lswappack = intype(2:2) /= outtype(2:2)
write(*,*)'lswappack = ',lswappack

call openff(ichan,umfilein,'r',intype)
call openff(ochan,umfileout,'w',outtype)

! Read/write fixed length header

call rdblki(fixhdi,len_fixhd,len_fixhd,ichan,inewpos,eof)
fixhdo = fixhdi

if (luseconfig) then
   fixhdo(8) = ical
   fixhdo(12) = iversion
   if (lwfio .and. iwfio_size > 1) then
      header_size = fixhdi(150) - 1 + fixhdi(151)*fixhdi(152)
      nrec = header_size / iwfio_size
      if (mod(header_size,iwfio_size) /= 0) nrec = nrec + 1
      header_size = nrec*iwfio_size
      fixhdo(160) = header_size+1
   else
      fixhdo(160) = fixhdi(150) + fixhdi(151)*fixhdi(152)
   endif
endif


! Check that file uses uncompressed data if we're going to try to
! extract additional data from NetCDF files.

if ((fixhdi(141) > 1 .or. fixhdi(143) > 1 .or. fixhdi(145) > 1) &
     .and. nitem > 0) then
   write (*,*) 'ERROR: Can''t convert NetCDF data to compressed UM format'
   stop
endif


! Get new dump date from NetCDF file

if (itimeusage1 == 0) then

   if (nitem == 0) then
      write(*,*)'ERROR: Can''t set dump date as no NetCDF files to be read in'
      stop
   endif

!  Get variable name from standard name

   if (lusestdname) then

      istashcode = itemid(1)
      write(*,*)'istashcode = ',istashcode,' from item 1'

      call get_stdname_from_stashcode(model,istashcode,stdname,ierr)
      if (ierr /= 0) then
         write(*,*)'ERROR: No standard name exists for stashcode ',istashcode
         stop
      endif
      write(*,*)'stdname for stash code ',istashcode,' = ', &
                trim(stdname)
      call get_varname_from_stdname(ncid(ncfileid(1)),stdname,varname,ierr)
      if (ierr /= 0) then
         write(*,*)'ERROR: standard name ',trim(stdname), &
                   ' not found in NetCDF file ',trim(oncfiles(ncfileid(1)))
         stop
      endif
      write(*,*)'variable name for standard name ',trim(stdname),' = ', &
                trim(varname)
   else
      varname = ncname(1)
   endif

!  Get date from NetCDF file

   call get_ncdate(ncid(ncfileid(1)),varname,istartdate)
endif

! Set date and time fixed length header values

if (itimeusage1 == 1 .or. itimeusage1 == 0) then
   fixhdo(21:26) = istartdate(1:6)
   fixhdo(27) = get_daynum(fixhdo(21),fixhdo(22),fixhdo(23),fixhdo(8))
   fixhdo(28:33) = istartdate(1:6)
   fixhdo(34) = get_daynum(fixhdo(28),fixhdo(29),fixhdo(30),fixhdo(8))
endif

! Get current date and time for fixed length header

call date_and_time(values=date_time)
fixhdo(35:37) = date_time(1:3)
fixhdo(38:40) = date_time(5:7)
fixhdo(41) = get_daynum(fixhdo(35),fixhdo(36),fixhdo(37),1)

! This may need to be rewritten later if the size of the island data
! changes.
call wrtblki(fixhdo,len_fixhd,len_fixhd,ochan,onewpos)

modidx(1) = -1

! Find largest integer and real header dimension

ihead_dim = fixhdi(101)
if (fixhdi(141) > ihead_dim) ihead_dim = fixhdi(141)
if (fixhdi(143) > ihead_dim) ihead_dim = fixhdi(143)
if (fixhdi(145) > ihead_dim) ihead_dim = fixhdi(145)

rhead_dim = fixhdi(106)
if (fixhdi(111) /= IMDI .and. fixhdi(111)*fixhdi(112) > rhead_dim) &
   rhead_dim = fixhdi(111)*fixhdi(112)
if (fixhdi(116) /= IMDI .and. fixhdi(116)*fixhdi(117) > rhead_dim) &
   rhead_dim = fixhdi(116)*fixhdi(117)
if (fixhdi(121) /= IMDI .and. fixhdi(121)*fixhdi(122) > rhead_dim) &
   rhead_dim = fixhdi(121)*fixhdi(122)
if (fixhdi(126) /= IMDI .and. fixhdi(126)*fixhdi(127) > rhead_dim) &
   rhead_dim = fixhdi(126)*fixhdi(127)
if (fixhdi(131) > rhead_dim) rhead_dim = fixhdi(131)
if (fixhdi(136) > rhead_dim) rhead_dim = fixhdi(136)

write(*,*)'ihead_dim = ',ihead_dim
write(*,*)'rhead_dim = ',rhead_dim

allocate (ihead(ihead_dim))
allocate (rhead(rhead_dim))

! Read/write headers

if (fixhdi(101) > 0) then
   inewpos = fixhdi(100)
   onewpos = fixhdo(100)
   call readwrite_head_i(ichan,ochan,inewpos,onewpos, &
                         ihead,fixhdi(101),imodarr,modidx,ierr)
endif

if (fixhdi(106) > 0) then
   inewpos = fixhdi(105)
   onewpos = fixhdo(105)
   call readwrite_head_r(ichan,ochan,inewpos,onewpos, &
                         rhead,fixhdi(106),rmodarr,modidx,ierr)
endif

lgotoceanlevels = .FALSE.
if (fixhdi(111) > 0) then
   inewpos = fixhdi(110)
   onewpos = fixhdo(110)
   lgotoceanlevels = .TRUE.
   call readwrite_head_r(ichan,ochan,inewpos,onewpos, &
                         rhead,fixhdi(111)*fixhdi(112),rmodarr,modidx,ierr)
   noceanlevels = fixhdi(111) * fixhdi(112)
   allocate(oceanlevels(noceanlevels))
   oceanlevels(1) = rhead(1)
   do i = 2, noceanlevels
      oceanlevels(i) = oceanlevels(i-1) + rhead(i)
   end do
endif

if (fixhdi(116) > 0) then
   inewpos = fixhdi(115)
   onewpos = fixhdo(115)
   call readwrite_head_r(ichan,ochan,inewpos,onewpos, &
                         rhead,fixhdi(116)*fixhdi(117),rmodarr,modidx,ierr)
endif

if (fixhdi(121) > 0) then
   inewpos = fixhdi(120)
   onewpos = fixhdo(120)
   call readwrite_head_r(ichan,ochan,inewpos,onewpos, &
                         rhead,fixhdi(121)*fixhdi(122),rmodarr,modidx,ierr)
endif

if (fixhdi(126) > 0) then
   inewpos = fixhdi(125)
   onewpos = fixhdo(125)
   if (lbathy) then
      if (lgotoceanlevels) then
         call modify_bathymetry(ochan,onewpos, &
              fixhdi(126)*fixhdi(127),bathyfile,bathyncname,lbathydepthmask, &
              noceanlevels, oceanlevels)
      else
         write (*,*) 'No ocean model levels for bathymetry modification'
         stop
      end if
   else
      call readwrite_head_r(ichan,ochan,inewpos,onewpos, &
           rhead,fixhdi(126)*fixhdi(127),rmodarr,modidx,ierr)
   endif
endif

!==> OVERWRITE ISLAND DATA HERE IF REQUIRED, MODIFYING POSITIONS FOR
!    FOLLOWING DATA
if (lireplace .or. liadd) then
   call process_islands(ichan, ochan, fixhdi, fixhdo, &
                        lireplace, liadd, islandsfile)
else
   ! Default copying of island data.
   if (fixhdi(131) > 0) then
      inewpos = fixhdi(130)
      onewpos = fixhdo(130)
      call readwrite_head_r(ichan,ochan,inewpos,onewpos, &
                            rhead,fixhdi(131),rmodarr,modidx,ierr)
   endif
end if

if (fixhdi(136) > 0) then
   inewpos = fixhdi(135)
   onewpos = fixhdo(135)
   call readwrite_head_r(ichan,ochan,inewpos,onewpos, &
                         rhead,fixhdi(136),rmodarr,modidx,ierr)
endif

if (fixhdi(141) > 0) then
   inewpos = fixhdi(140)
   onewpos = fixhdo(140)
   call readwrite_head_i(ichan,ochan,inewpos,onewpos, &
                         ihead,fixhdi(141),imodarr,modidx,ierr)
endif

if (fixhdi(143) > 0) then
   inewpos = fixhdi(142)
   onewpos = fixhdo(142)
   call readwrite_head_i(ichan,ochan,inewpos,onewpos, &
                         ihead,fixhdi(143),imodarr,modidx,ierr)
endif

if (fixhdi(145) > 0) then
   inewpos = fixhdi(144)
   onewpos = fixhdo(144)
   call readwrite_head_i(ichan,ochan,inewpos,onewpos, &
                         ihead,fixhdi(145),imodarr,modidx,ierr)
endif

deallocate (ihead,rhead)

allocate(data_type(fixhdi(152)))
allocate(data_pack(fixhdi(152)))
allocate(stash_code(fixhdi(152)))
allocate(data_size_i(fixhdi(152)))
allocate(data_size_o(fixhdi(152)))
allocate(data_pos_i(fixhdi(152)))
allocate(data_pos_o(fixhdi(152)))

inewpos = fixhdi(150)
onewpos = fixhdo(150)
data_pos0 = fixhdo(160) - 1
data_pos1 = fixhdi(160) - 1

call skip(ichan,curpos,inewpos)
call skip(ochan,curpos,onewpos)
do i=1,fixhdi(152)

!  Read in PP header

   call rdblki(ilookup,len_pphead_int,len_pphead_int,ichan,inewpos,eof)
   call rdblkr(rlookup,len_pphead_real,len_pphead_real,ichan,inewpos,eof)

   stash_code(i) = ilookup(42)
   if (ilookup(21) /= 0 .and. ilookup(21) /= 2) then
      write (*,*) 'ERROR: only LBPACK=0 or 2 supported for ocean dump files'
      stop
   endif
   if (intype(3:3) == '4') then
      data_pack(i) = 0
   else
      data_pack(i) = ilookup(21)
   endif
   data_pos_i(i) = ilookup(29)
   if (data_pack(i) == 2 .and. itype == i64) then
      data_size_i(i) = (ilookup(15)+1)/2
   else
      data_size_i(i) = ilookup(15)
   endif
   data_type(i) = ilookup(39)

!  Modify PP header

   if (luseconfig) then
      ic = mod(ilookup(13),10)
      if ((ic == 1 .or. ic == 2) .and. ic /= ical) then
         ilookup(13) = (ic/10)*10 + ical
      endif
      if (iversion >= 503) then
         ilookup(38) = iversion*10000+1111
      endif
   endif

   ! Set date and time pp header values

   if (itimeusage1 == 1 .or. itimeusage1 == 0) then
      ic = mod(ilookup(13),10)
      ilookup(1:5) = istartdate(1:5)
      ilookup(6) = get_daynum(istartdate(1),istartdate(2),istartdate(3),ic)
      ilookup(7:11) = istartdate(1:5)
      ilookup(12) = get_daynum(istartdate(1),istartdate(2),istartdate(3),ic)
      ilookup(13) = ic
   endif

   if (lwfio) then
      data_size0 = ilookup(15)
      if (data_pack(i) == 2 .and. outtype(3:3) == '8') &
         data_size0 = (data_size0+1)/2
      nrec = data_size0 / iwfio_size
      if (mod(data_size0,iwfio_size) /= 0) nrec = nrec + 1
      data_size0 = nrec*iwfio_size

      ilookup(29) = data_pos0
      ilookup(30) = data_size0
      data_pos0 = data_pos0 + data_size0
   else if (luseconfig) then
      ilookup(29) = 0
      ilookup(30) = 0
   endif

   data_pos_o(i) = ilookup(29)
   if ((.not. luseconfig .or. lwfio) .and. &
       ilookup(30) /= 0 .and. ilookup(30) /= IMDI) then
      if (ilookup(21) == 2 .and. itype == i32) then
         data_size_o(i) = 2*ilookup(30)
      else
         data_size_o(i) = ilookup(30)
      endif
   else
      data_size_o(i) = data_size_i(i)
   endif

   if (data_pack(i) == 2) then
     if (data_size_o(i) > max_isize) max_isize = data_size_o(i)
   else if (data_type(i) == 1) then
      if (data_size_o(i) > max_rsize) max_rsize = data_size_o(i)
   else
      if (data_size_o(i) > max_isize) max_isize = data_size_o(i)
   endif

!  Write out PP header

   call wrtblki(ilookup,len_pphead_int,len_pphead_int,ochan,onewpos)
   call wrtblkr(rlookup,len_pphead_real,len_pphead_real,ochan,onewpos)

enddo

if (luseconfig .and. lwfio .and. iwfio_size > 1) then
   write(*,*)'writing dummy data after headers of size ', &
             fixhdo(160)-fixhdo(150)-fixhdo(151)*fixhdo(152)
   n1 = iwfio_size
   n2 = fixhdo(160)-fixhdo(150)-fixhdo(151)*fixhdo(152)
   call wrtblki(dum,n1,n2,ochan,onewpos)
   deallocate(dum)
endif

write(*,*)'max_isize = ',max_isize
write(*,*)'max_rsize = ',max_rsize
if (max_isize > 0) allocate(idata(max_isize))
if (max_rsize > 0) allocate(rdata(max_rsize))

inewpos = fixhdi(160)
onewpos = fixhdo(160)
iitem = 1
ilev = 1
lreplace = .false.

call skip(ichan,curpos,inewpos)
call skip(ochan,curpos,onewpos)

do i=1,fixhdi(152)

   lreplace = lreplace .or. (iitem <= nitem .and. stash_code(i) == itemid(iitem))

   if (lreplace) then

!     Replace field with data from netcdf file

      write(*,*)'item = ',i,' stash code = ',stash_code(i)
      if (data_pack(i) == 2 .and. itype == i32) then
         inewpos = inewpos + data_size_i(i)/2
      else
         inewpos = inewpos + data_size_i(i)
      endif
      call skip(ichan,curpos,inewpos)

!     Get variable name from standard name

      if (lusestdname .and. ilev == 1) then
         call get_stdname_from_stashcode(model,stash_code(i),stdname,ierr)
         if (ierr /= 0) then
            write(*,*)'ERROR: No standard name exists for stashcode ', &
                      stash_code(i)
            stop
         endif
         write(*,*)'stdname for stash code ',stash_code(i),' = ', &
                   trim(stdname)
         call get_varname_from_stdname(ncid(ncfileid(iitem)),stdname, &
                                       varname,ierr)
         if (ierr /= 0) then
            write(*,*)'ERROR: standard name ',trim(stdname), &
                      ' not found in NetCDF file ',trim(oncfiles(ncfileid(iitem)))
            stop
         endif
         write(*,*)'variable name for standard name ',trim(stdname),' = ', &
                   trim(varname)
      else if (ilev == 1) then
         varname = ncname(iitem)
      endif
      if (data_type(i) == 1) then
         rdata = 0.0_rtype
         call get_ncfield_r(ncid(ncfileid(iitem)),varname,ilev,model, &
                            itimeusage1,itimeusage2,istartdate, &
                            rdata,data_size_i(i))
         call get_ncmdi_r(varname,ncid(ncfileid(iitem)),rmdi_nc)
         where (rdata == rmdi_nc) rdata = rmdi
      else
         idata = 0
         call get_ncfield_i(ncid(ncfileid(iitem)),varname,ilev,model, &
                            itimeusage1,itimeusage2,istartdate, &
                            idata,data_size_i(i))
         call get_ncmdi_i(varname,ncid(ncfileid(iitem)),imdi_nc)
         where (idata == imdi_nc) idata = imdi
      endif

      if (i == fixhdi(152)) then
         lreplace = .false.
      else if (stash_code(i) /= stash_code(i+1)) then
         ilev = 1
         iitem = iitem + 1
         lreplace = .false.
      else
         ilev = ilev + 1
      endif

   else

!     Read in data from input UM dump

      if (data_pos_i(i) /= 0 .and. data_pos_i(i) /= IMDI) &
         inewpos = data_pos_i(i)+1

      if (i==1 .or. i==fixhdi(152)) &
         write(*,*)i,data_size_i(i),data_pos_i(i),inewpos

      if (data_pack(i) == 2) then
         idata = 0
         call read_data_p (ichan,inewpos,idata,data_size_i(i),1)
         if (lswappack) then
            if (itype == i32) then
               call swapbytes(idata,4,data_size_i(i))
            else
               call swapbytes(idata,4,2*data_size_i(i))
            endif
         endif
      else if (data_type(i) == 1) then
         rdata = 0.0_rtype
         call read_data_r (ichan,inewpos,rdata,data_size_i(i),1)
      else
         idata = 0
         call read_data_i (ichan,inewpos,idata,data_size_i(i),1)
      endif
   endif

!  Write out data

   if (data_pos_o(i) /= 0 .and. data_pos_o(i) /= IMDI) onewpos = data_pos_o(i)+1

   if (i==1 .or. i==fixhdi(152)) &
        write(*,*)i,data_size_o(i),data_pos_o(i),onewpos

   if (data_pack(i) == 2) then
      call write_data_p (ochan,onewpos,idata,data_size_o(i),1)
   else if (data_type(i) == 1) then
      call write_data_r (ochan,onewpos,rdata,data_size_o(i),1)
   else
      call write_data_i (ochan,onewpos,idata,data_size_o(i),1)
   endif

enddo

! Close NetCDF files

if (nitem > 0) then
   do i=1,nncfiles1
      call close_ncfile(ncid(i),ierr)
   enddo
   deallocate (ncid)
endif

deallocate (data_pack,data_type,stash_code)
deallocate (data_size_i,data_size_o,data_pos_i,data_pos_o)
if (max_isize > 0) deallocate(idata)
if (max_rsize > 0) deallocate(rdata)
if (allocated(rtmp)) deallocate(rtmp)
if (allocated(itmp32)) deallocate(itmp32)

call closeff(ichan)
call closeff(ochan)

l32bit = l32bit_save
if (.not. luseconfig) then
   lwfio = lwfio_save
   iwfio_size = iwfio_size_save
endif

return
end


!===============================================================================
!
!  Modify bathymetry mask in "fields of constants" section of header
!  based on input from a NetCDF file (either a depth mask as layer
!  counts, or actual water depth values, which are converted to a
!  depth mask based on comparison with the model layer thicknesses).
!

subroutine modify_bathymetry(ochan, onewpos, &
                             bathysize, bathyfile, bathyncname, &
                             ldepthmask, nlev, levels)

use getkind
use parameters
use types

implicit none

integer(ptype) ochan
integer(otype) onewpos
integer(itype) bathysize
character(*) bathyfile, bathyncname
logical ldepthmask
integer nlev
real(rtype) levels(nlev)

integer ncid,ierr
logical isncvarint
character(max_varname_size) dimnames(4)
integer dim(4), nz, nt
type(gridinfo) grid
integer, dimension(:), allocatable :: idepthmask
real(rtype), dimension(:), allocatable :: rdepthmask
real(rtype), dimension(:), allocatable :: waterdepth
integer i, ilev
integer(otype) curpos


write (*,*) 'Replacing bathymetry data from file: ', trim(bathyfile), &
     ', variable: ', trim(bathyncname)

! Open NetCDF file.
call open_ncfile(bathyfile, 'r', ncid, ierr)

! Check grid against existing ocean grid.
call get_gridinfo(bathyncname, ncid, dimnames, dim, nz, nt, 2, 1, grid)
if (grid%nlat * grid%nlong /= bathysize) then
   write (*,*) 'Grid size mismatch for replacement bathymetry data'
   stop
end if

! Allocate space for depthmask data.
allocate(rdepthmask(bathysize))

! Determine type of replacement bathymetry variable.
if (ldepthmask) then
   ! Depth mask as a layer count: either copy from an integer or read
   ! a real value.
   if (isncvarint(bathyncname, ncid)) then
      write (*,*) 'Bathymetry variable is integer depth mask'
      allocate(idepthmask(bathysize))
      call get_ncdata_i(bathyncname, ncid, 1, 1, grid, dim, &
                        grid%nlong, grid%nlat, 1, 1, idepthmask)
      rdepthmask = idepthmask
      deallocate(idepthmask)
   else
      write (*,*) 'Bathymetry variable is real depth mask'
      call get_ncdata_r(bathyncname, ncid, 1, 1, grid, dim, &
                        grid%nlong, grid%nlat, 1, 1, rdepthmask)
   end if
else
   ! Floating point water depth: calculate the depth mask based on the
   ! ocean model levels.
   write (*,*) 'Bathymetry variable is real water depth'
   allocate(waterdepth(bathysize))
   call get_ncdata_r(bathyncname, ncid, 1, 1, grid, dim, &
                     grid%nlong, grid%nlat, 1, 1, waterdepth)
   where (waterdepth < 0) waterdepth = 0
   rdepthmask = nlev
   do i = 1, bathysize
      if (waterdepth(i) == 0) then
         rdepthmask(i) = 0
      else
         do ilev = 1, nlev
            if (waterdepth(i) < levels(ilev)) then
               rdepthmask(i) = ilev
               exit
            end if
         end do
      end if
   end do
   deallocate(waterdepth)
endif

! Write the new depth mask data.
where (rdepthmask < 0) rdepthmask = 0
where (rdepthmask > nlev) rdepthmask = nlev
call skip(ochan, curpos, onewpos)
call wrtblkr(rdepthmask, bathysize, bathysize, ochan, curpos)
deallocate(rdepthmask)

! Close NetCDF file.
call close_ncfile(ncid, ierr)

return
end



!===============================================================================
!
!  Modify island data in "extra constants" section of header based on
!  input from a ASCII islands file.  Also update output file header
!  offsets as required for header items and data fields following
!  island data.
!
subroutine process_islands(ichan, ochan, fixhdi, fixhdo, &
                           lireplace, liadd, islandsfile)

use getkind
use parameters
use constants
use config
use types

implicit none

integer(ptype) ichan, ochan
integer(itype) fixhdi(len_fixhd), fixhdo(len_fixhd)
logical lireplace, liadd
character(*) islandsfile

integer(otype) inewpos, onewpos, curpos
integer ieof
integer old_len, new_len, out_len, dlen
integer, parameter :: max_islands_len = 8192
real(rtype) old_islands(max_islands_len)
real(rtype) new_islands(max_islands_len)
real(rtype) out_islands(max_islands_len)

if (lireplace) then
   write (*,*) 'Replacing island data from file: ', trim(islandsfile)
else if (liadd) then
   write (*,*) 'Adding island data from file: ', trim(islandsfile)
else
   write (*,*) 'ERROR: unexpected state in process_islands'
   stop
end if

! Extract existing island data from header.
old_len = fixhdi(131)
if (old_len > max_islands_len) then
   write (*,*) 'ERROR: increase max_islands_len and recompile'
   stop
end if
inewpos = fixhdi(130)
call skip(ichan, curpos, inewpos)
call rdblkr(old_islands, old_len, old_len, ichan, curpos, ieof)

! Read new island data.
call read_islands_file(islandsfile, new_len, max_islands_len, new_islands)

! Set up output island data as required.
if (lireplace) then
   out_len = new_len
   out_islands = new_islands
else if (liadd) then
   ! The (-1) is because both sets of data have an island count.
   out_len = old_len + new_len - 1
   if (out_len > max_islands_len) then
      write (*,*) 'ERROR: increase max_islands_len and recompile'
      stop
   end if
   out_islands(1) = old_islands(1) + new_islands(1)
   out_islands(2:old_len) = old_islands(2:old_len)
   out_islands(old_len+1:out_len) = new_islands(2:new_len)
end if

! Adjust output header file offsets.
dlen = out_len - old_len
fixhdo(131) = out_len
if (fixhdo(135) /= IMDI) fixhdo(135) = fixhdo(135) + dlen
if (fixhdo(140) /= IMDI) fixhdo(140) = fixhdo(140) + dlen
if (fixhdo(142) /= IMDI) fixhdo(142) = fixhdo(142) + dlen
if (fixhdo(144) /= IMDI) fixhdo(144) = fixhdo(144) + dlen
if (fixhdo(150) /= IMDI) fixhdo(150) = fixhdo(150) + dlen
if (fixhdo(160) /= IMDI) fixhdo(160) = fixhdo(160) + dlen

! Rewrite output fixed header.
onewpos = 1
call skip(ochan, curpos, onewpos)
call wrtblki(fixhdo, len_fixhd, len_fixhd, ochan, onewpos)

! Write new output island data.
onewpos = fixhdo(130)
call skip(ochan, curpos, onewpos)
call wrtblkr(out_islands, out_len, out_len, ochan, onewpos)

return
end


!================================================================================
!
!  Read islands file, skipping comment lines (introduced by #
!  character) and returning numeric values in file in a single array.
!

subroutine read_islands_file(fname, nvals, maxlen, res)

use getkind

implicit none

character(*) fname
integer nvals, maxlen
real(rtype) res(maxlen)

integer sz, pos, len, i
character(len=1024) tmp, line
integer, parameter :: buff_size = 32768
character(len=buff_size) :: buff
logical in

call ffsize(sz, fname)
if (sz > buff_size) then
   write (*,*) 'Islands file too large: recompile with larger buff_size'
   stop
end if
buff = ' '

pos = 1
open (unit=10, file=fname, form='formatted', status='old')
do while (.true.)
   read (10, '(a)', end=99) tmp
   line = trim(tmp)
   len = len_trim(line)
   if (len == 0 .or. line(1:1) == '#') cycle
   buff(pos:pos+len-1) = line(1:len)
   pos = pos + len + 1
end do
close (unit=10)

99 continue
pos = 1
nvals = 0
if (buff(1:1) /= ' ') then
   in = .true.
   nvals = 1
   do while (pos <= sz)
      if (.not. in) then
         if (buff(pos:pos) /= ' ') then
            in = .true.
            nvals = nvals + 1
         end if
      else if (in .and. buff(pos:pos) == ' ') then
         in = .false.
      end if
      pos = pos + 1
   end do
end if

if (nvals > maxlen) then
   write (*,*) 'ERROR: increase max_islands_len and recompile'
   stop
end if
read (buff,*) res(1:nvals)

return
end
