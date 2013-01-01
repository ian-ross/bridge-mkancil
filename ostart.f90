
!==============================================================================!

subroutine create_ostart(filein,fileout,umfile,ncname, &
                         lusestdname,luseconfig, &
                         nncfiles1,nitem,ncfileid,itemid, &
                         itimeusage1,itimeusage2,istartdate, &
                         lbathy,bathyfile,bathyncname, &
                         lireplace,liadd,islandsfile)
use getkind
use lsmask
use constants
use parameters
use config
use types

implicit none

integer nncfiles1,nitem,ncfileid(nitem+1),itemid(nitem+1)
integer itimeusage1,itimeusage2,istartdate(6)
logical lusestdname,luseconfig
character(*) filein(nncfiles1),fileout,umfile,ncname(nitem+1)
logical lbathy,lireplace,liadd
character(*) bathyfile,bathyncname,islandsfile

integer(itype), dimension(:), allocatable ::  dum
logical l32bit_save,lwfio_save
integer iwfio_size_save
integer i
integer max_isize,max_rsize
integer model
integer, dimension(:), allocatable :: ncid
integer ierr
character(3) intype,outtype
logical lum
logical lswappack
integer(ptype) ichan,ochan
integer(itype) fixhd(len_fixhd)
integer(itype) istashcode,eof,n1
integer(otype) onewpos,curpos,inewpos
integer fixhd_160_i             ! Data start position.
integer header_size,nrec
character(max_stdname_size) stdname
character(max_varname_size) varname
integer get_daynum,date_time(8)
integer modidx(100)
integer ihead_dim,rhead_dim
integer(itype) , dimension(:), allocatable :: ihead
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
integer data_size0,data_size1
integer ic
integer(itype) n2
integer(itype) , dimension(:), allocatable :: idata
real(rtype) , dimension(:), allocatable :: rdata
integer iitem,ilev
logical lreplace
integer(i32) , dimension(:), allocatable :: itmp32
real(rtype) , dimension(:), allocatable :: rtmp


write (*,*) 'Writing Ocean start dump ', trim(fileout)

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
    write(*,*)'filein(',i,') = ',trim(filein(i))
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
      call open_ncfile(filein(i), 'r', ncid(i), ierr)
   enddo
endif

call pptype(umfile,lum,intype)
write(*,*)'lum = ',lum
write(*,*)'intype = ',intype
if (.not. lum) then
   write(*,*)'ERROR: file ',trim(umfile),' is not a UM file'
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

call openff(ichan,umfile,'r',intype)
call openff(ochan,fileout,'w',outtype)

! Read/write fixed length header

call rdblki(fixhd,len_fixhd,len_fixhd,ichan,inewpos,eof)

fixhd_160_i = fixhd(160)

if (luseconfig) then
   fixhd(8) = ical
   fixhd(12) = iversion
   if (lwfio .and. iwfio_size > 1) then
      header_size = fixhd(150) - 1 + fixhd(151)*fixhd(152)
      nrec = header_size / iwfio_size
      if (mod(header_size,iwfio_size) /= 0) nrec = nrec + 1
      header_size = nrec*iwfio_size
      fixhd(160) = header_size+1
   else
      fixhd(160) = fixhd(150) + fixhd(151)*fixhd(152)
   endif
endif


! Check that file uses uncompressed data if we're going to try to
! extract additional data from NetCDF files.

if ((fixhd(141) > 1 .or. fixhd(143) > 1 .or. fixhd(145) > 1) &
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

      inewpos = fixhd(150) + (itemid(1)-1)*64 + 41
      call skip(ichan,curpos,inewpos)
      n1 = 1
      call rdblki(istashcode,n1,n1,ichan,inewpos,eof)
      write(*,*)'istashcode = ',istashcode,' from item ',itemid(1)

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
                   ' not found in NetCDF file ',trim(filein(ncfileid(1)))
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
   fixhd(21:26) = istartdate(1:6)
   fixhd(27) = get_daynum(fixhd(21),fixhd(22),fixhd(23),fixhd(8))
   fixhd(28:33) = istartdate(1:6)
   fixhd(34) = get_daynum(fixhd(28),fixhd(29),fixhd(30),fixhd(8))
endif

! Get current date and time for fixed length header

call date_and_time(values=date_time)
fixhd(35:37) = date_time(1:3)
fixhd(38:40) = date_time(5:7)
fixhd(41) = get_daynum(fixhd(35),fixhd(36),fixhd(37),1)

!==> MAY NEED TO REWRITE THIS LATER IF THE SIZE OF THE ISLAND DATA
!    CHANGES (OR JUST MOVE WRITING THE HEADER UNTIL AFTER WE KNOW
!    EXACTLY HOW BIG IT SHOULD BE)
call wrtblki(fixhd,len_fixhd,len_fixhd,ochan,onewpos)

modidx(1) = -1

! Find largest integer and real header dimension

ihead_dim = fixhd(101)
if (fixhd(141) > ihead_dim) ihead_dim = fixhd(141)
if (fixhd(143) > ihead_dim) ihead_dim = fixhd(143)
if (fixhd(145) > ihead_dim) ihead_dim = fixhd(145)

rhead_dim = fixhd(106)
if (fixhd(111) /= IMDI .and. fixhd(111)*fixhd(112) > rhead_dim) &
   rhead_dim = fixhd(111)*fixhd(112)
if (fixhd(116) /= IMDI .and. fixhd(116)*fixhd(117) > rhead_dim) &
   rhead_dim = fixhd(116)*fixhd(117)
if (fixhd(121) /= IMDI .and. fixhd(121)*fixhd(122) > rhead_dim) &
   rhead_dim = fixhd(121)*fixhd(122)
if (fixhd(126) /= IMDI .and. fixhd(126)*fixhd(127) > rhead_dim) &
   rhead_dim = fixhd(126)*fixhd(126)
if (fixhd(131) > rhead_dim) rhead_dim = fixhd(131)
if (fixhd(136) > rhead_dim) rhead_dim = fixhd(136)

write(*,*)'ihead_dim = ',ihead_dim
write(*,*)'rhead_dim = ',rhead_dim

allocate (ihead(ihead_dim))
allocate (rhead(rhead_dim))

! Read/write headers

if (fixhd(101) > 0) then
   inewpos = fixhd(100)
   onewpos = fixhd(100)
   call readwrite_head_i(ichan,ochan,inewpos,onewpos, &
                         ihead,fixhd(101),imodarr,modidx,ierr)
endif

if (fixhd(106) > 0) then
   inewpos = fixhd(105)
   onewpos = fixhd(105)
   call readwrite_head_r(ichan,ochan,inewpos,onewpos, &
                         rhead,fixhd(106),rmodarr,modidx,ierr)
endif

if (fixhd(111) > 0) then
   inewpos = fixhd(110)
   onewpos = fixhd(110)
   call readwrite_head_r(ichan,ochan,inewpos,onewpos, &
                         rhead,fixhd(111)*fixhd(112),rmodarr,modidx,ierr)
endif

if (fixhd(116) > 0) then
   inewpos = fixhd(115)
   onewpos = fixhd(115)
   call readwrite_head_r(ichan,ochan,inewpos,onewpos, &
                         rhead,fixhd(116)*fixhd(117),rmodarr,modidx,ierr)
endif

if (fixhd(121) > 0) then
   inewpos = fixhd(120)
   onewpos = fixhd(120)
   call readwrite_head_r(ichan,ochan,inewpos,onewpos, &
                         rhead,fixhd(121)*fixhd(122),rmodarr,modidx,ierr)
endif

if (fixhd(126) > 0) then
   inewpos = fixhd(125)
   onewpos = fixhd(125)
   if (lbathy) then
      call modify_bathymetry(ichan,ochan,inewpos,onewpos, &
           fixhd(126)*fixhd(127),bathyfile,bathyncname)
   else
      call readwrite_head_r(ichan,ochan,inewpos,onewpos, &
           rhead,fixhd(126)*fixhd(127),rmodarr,modidx,ierr)
   endif
endif

!==> OVERWRITE ISLAND DATA HERE IF REQUIRED, MODIFYING POSITIONS FOR
!    FOLLOWING DATA
if (fixhd(131) > 0) then
   inewpos = fixhd(130)
   onewpos = fixhd(130)
   call readwrite_head_r(ichan,ochan,inewpos,onewpos, &
                         rhead,fixhd(131),rmodarr,modidx,ierr)
endif

if (fixhd(136) > 0) then
   inewpos = fixhd(135)
   onewpos = fixhd(135)
   call readwrite_head_r(ichan,ochan,inewpos,onewpos, &
                         rhead,fixhd(136),rmodarr,modidx,ierr)
endif

if (fixhd(141) > 0) then
   inewpos = fixhd(140)
   onewpos = fixhd(140)
   call readwrite_head_i(ichan,ochan,inewpos,onewpos, &
                         ihead,fixhd(141),imodarr,modidx,ierr)
endif

if (fixhd(143) > 0) then
   inewpos = fixhd(142)
   onewpos = fixhd(142)
   call readwrite_head_i(ichan,ochan,inewpos,onewpos, &
                         ihead,fixhd(143),imodarr,modidx,ierr)
endif

if (fixhd(145) > 0) then
   inewpos = fixhd(144)
   onewpos = fixhd(144)
   call readwrite_head_i(ichan,ochan,inewpos,onewpos, &
                         ihead,fixhd(145),imodarr,modidx,ierr)
endif

deallocate (ihead,rhead)

allocate(data_type(fixhd(152)))
allocate(data_pack(fixhd(152)))
allocate(stash_code(fixhd(152)))
allocate(data_size_i(fixhd(152)))
allocate(data_size_o(fixhd(152)))
allocate(data_pos_i(fixhd(152)))
allocate(data_pos_o(fixhd(152)))

inewpos = fixhd(150)
onewpos = fixhd(150)
data_pos0 = fixhd(160) - 1
data_pos1 = fixhd_160_i - 1

call skip(ichan,curpos,inewpos)
call skip(ochan,curpos,onewpos)
do i=1,fixhd(152)

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
             fixhd(160)-fixhd(150)-fixhd(151)*fixhd(152)
   n1 = iwfio_size
   n2 = fixhd(160)-fixhd(150)-fixhd(151)*fixhd(152)
   call wrtblki(dum,n1,n2,ochan,onewpos)
   deallocate(dum)
endif

write(*,*)'max_isize = ',max_isize
write(*,*)'max_rsize = ',max_rsize
if (max_isize > 0) allocate(idata(max_isize))
if (max_rsize > 0) allocate(rdata(max_rsize))

inewpos = fixhd_160_i
onewpos = fixhd(160)
iitem = 1
ilev = 1
lreplace = .false.

call skip(ichan,curpos,inewpos)
call skip(ochan,curpos,onewpos)

do i=1,fixhd(152)

   lreplace = lreplace .or. (iitem <= nitem .and. i == itemid(iitem))

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
                      ' not found in NetCDF file ',trim(filein(ncfileid(iitem)))
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
      else
         idata = 0
         call get_ncfield_i(ncid(ncfileid(iitem)),varname,ilev,model, &
                            itimeusage1,itimeusage2,istartdate, &
                            idata,data_size_i(i))
      endif

      if (i == fixhd(152)) then
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

      if (i==1 .or. i==fixhd(152)) &
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

   if (i==1 .or. i==fixhd(152)) write(*,*)i,data_size_o(i),data_pos_o(i),onewpos

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



subroutine modify_bathymetry(ichan,ochan,inewpos,onewpos, &
     bathysize,bathyfile,bathyncname)

use getkind

implicit none

integer(ptype) ichan, ochan
integer(otype) onewpos,inewpos
integer(itype) bathysize
character(*) bathyfile,bathyncname

write (*,*) 'Replacing bathymetry data from file: ', trim(bathyfile), &
     ', variable: ', trim(bathyncname)

! Determine type of replacement bathymetry variable.

! If it's an integer, assume it's the depth mask as a layer count.  If
! it's a floating point variable, assume that it's water depth and
! calculate the depth mask based on the ocean model levels.

! Write the new depth mask data.

return
end
