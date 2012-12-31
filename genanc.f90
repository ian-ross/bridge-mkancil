
!==============================================================================!

subroutine create_genanc(fileout,ncname,lperiodic, &
                         model,inthd8,nfield,istashcode,ippcode, &
                         ilevtype,nlev1,ltheta, &
                         igridtype,idatatype,imasktype,imask,ifileinid, &
                         itimeusage1,itimeusage2,istartdate1,ntimes, &
                         interval,intunit1,lmm)

use getkind
use constants
use parameters
use config
use lsmask
use lslfrac
use types
use vert_od
use vert_nd
use vert_ocean

implicit none

integer nfield
character(*) fileout,ncname(nfield)
logical lperiodic,lmm,ltheta(nfield)
integer model,inthd8
integer istashcode(nfield),ippcode(nfield),ilevtype(nfield),nlev1(nfield)
integer igridtype(nfield),idatatype(nfield)
integer imasktype(nfield),imask(nfield),ifileinid(nfield)
integer itimeusage1,itimeusage2,istartdate1(6)
integer ntimes,interval,intunit1

real(rtype), dimension(:,:), allocatable :: rdata
real(rtype), dimension(:), allocatable :: time
real(rtype) rmdi_nc
integer(itype), dimension(:,:), allocatable :: idata
integer ifield,ifield2,ncid,ierr
integer idatatype1,imasktype1,imask1,imdi_nc
integer datasize,nfieldtot,nftype,ztype
integer(ptype) ochan
integer(otype) pp_pos,data_pos
integer dim(4),nx,nx1,ny,ny1,nz,nt1,nt2,intunit2,iz,iz0,iz1,izz,it,iscale
integer, dimension(:,:), allocatable :: idate
integer, dimension(:), allocatable :: itsel
integer istartdate2(6)
integer proccode,levcode,fftype,fflev,istart
integer fieldcode,stashcode
logical lopen,lzonal,lint,lintvar,isncvarint
type(gridinfo) grid
character(max_filename_size) filein,filein_save
character(max_varname_size) dimnames(4),ncname1

! Find  value of ztype to use in headers

ifield2 = 1
do ifield=1,nfield
   if (ilevtype(ifield) == 1 .or. &
       ilevtype(ifield) == 3 .or. &
       ilevtype(ifield) == 4 .or. &
       ilevtype(ifield) == 5) then
      ifield2 = ifield
      exit
   endif
enddo
if (ilevtype(ifield2) == 0) then      ! Single level
   ztype = 1
else if (ilevtype(ifield2) == 1) then ! Soil levels
   ztype = 4
else if (ilevtype(ifield2) == 2) then ! Pseudo levels
   ztype = 5
else if (ilevtype(ifield2) == 3) then ! Model levels
   ztype = 2
else if (ilevtype(ifield2) == 4) then ! Ozone levels
   ztype = 3
else if (ilevtype(ifield2) == 5) then ! Ocean depth levels
   ztype = 6
endif

! If not specified calculate values of nlev1

if ((iavert == 0 .and. model == 1) .or. &
    (iovert == 0 .and. model == 2)) then
   do ifield=1,nfield
      filein = ncfiles(ifileinid(ifield))
      if (ifield == 1) then
         filein_save = filein
         lopen = .true.
      else
         lopen = filein /= filein_save
         if (lopen) then
            call close_ncfile(ncid,ierr)
            filein_save = filein
         endif
      endif
      if (lopen) call open_ncfile(filein, 'r', ncid, ierr)
   
      call get_diminfo(ncname(ifield),ncid,dimnames,dim,nx1,ny1,nz,nt1)
      nlev1(ifield) = nz

!     Calculate vertical level info for level dependent constants header

      if (ifield == ifield2) then
         if (ztype == 2 .or. ztype == 3) then
            call get_nclevels_atmos(filein,dimnames(3),nlev, &
                                    version,ltheta(ifield2))
         else if (ztype == 6) then
            call get_nclevels_ocean(filein,dimnames(3),noclev)
         endif
      endif
   enddo
   call close_ncfile(ncid,ierr)
else if (iavert == 2 .and. model == 1) then
   do ifield=1,nfield
      if (ilevtype(ifield) == 3) then
         nlev1(ifield) = nlev
      else if (ilevtype(ifield) == 4) then
         nlev1(ifield) = no3lev
      endif
   enddo
else if (iovert == 2 .and. model == 2) then
   do ifield=1,nfield
      if (ilevtype(ifield) == 5) then
         nlev1(ifield) = noclev
      endif
   enddo
endif

! Get dimension information from the first field in netcdf file

filein = ncfiles(ifileinid(1))
call open_ncfile(filein, 'r', ncid, ierr)

call get_gridinfo(ncname(1),ncid,dimnames,dim,nz,nt1,model,igridtype(1),grid)
nx1 = grid%nlong
ny1 = grid%nlat

! Check time selection consistency

if (dim(4) < 1) then
   if (itimeusage1 == 0 .or. &
      (itimeusage1 == 1 .and. itimeusage2 == 1)) then
      write(*,*)'ERROR: Cannot read date from NetCDF file ', &
                trim(filein),' as it has no time dimension'
      stop 
   else if (itimeusage1 == 1 .and. &
            itimeusage2 == 0 .and. ntimes > 1) then
      write(*,*)'ERROR: Can only specify 1 time in xancil as file ', &
                trim(filein),' has no time dimension'
      stop
   endif
endif

if (itimeusage1 == 0) then
   nt2 = nt1
else if (itimeusage1 == 1) then
   nt2 = ntimes
else
   write(*,*)'ERROR: itimeusage1 = ',itimeusage1,' unknown value'
   stop
endif

allocate(time(nt1))
allocate(idate(6,nt2+1))
allocate(itsel(nt2+1))

if (dim(4) > 0) then
   call get_ncdim(dimnames(4),ncid,time)
!   write(*,*)'time = ',time
else
   time = 0.0_rtype
endif

if (itimeusage1 == 0 .or. &
   (itimeusage1 == 1 .and. itimeusage2 == 1)) &
   call get_ncstartdate(dimnames(4),ncid, &
                        istartdate2,intunit2,iscale)

call setdates(itimeusage1,itimeusage2, &
              lmm,interval, &
              intunit1,intunit2,ical,iscale,ntimes, &
              istartdate1,istartdate2,time,nt1,itsel,idate,nt2)

!do it=1,nt2
!   write(*,*)'date = ',idate(1,it),idate(2,it),idate(3,it), &
!                       idate(4,it),idate(5,it),idate(6,it),itsel(it)
!enddo

if (itimeusage1 == 0) intunit1 = intunit2

call close_ncfile(ncid, ierr)

! Work out total number of fields and field types in output file

nfieldtot = 0
nftype = 0
do ifield=1,nfield
   nfieldtot = nfieldtot + nlev1(ifield)*nt2
   if (ilevtype(ifield) /= 2) then
       nftype = nftype + 1
   else
       nftype = nftype + nlev1(ifield)
   endif
enddo

! Do we need to allocate an integer field?

lint = .false.
do ifield=1,nfield
   lint = idatatype(ifield) .ne. 1
   if (lint) exit
enddo

allocate(rdata(nx1,ny1))
if (lint) allocate(idata(nx1,ny1))

! Open ancil file

call open_ancil(ochan,fileout,'w')

! write fixed ancillary file headers

datasize = nfieldtot*nx1*ny1
call write_head(ochan,pp_pos,data_pos,nfieldtot,datasize, &
                nftype,inthd8,intunit1,interval,lperiodic, &
                ztype,grid,idate,nt2)

call free_gridinfo(grid)

! pp header values for all fields

fftype = 0
istart = 1

do it=1,nt2

   do ifield=1,nfield

      ncname1 = ncname(ifield)
      idatatype1 = idatatype(ifield)
      imasktype1 = imasktype(ifield)
      imask1 = imask(ifield)
      stashcode = istashcode(ifield)
      fieldcode = ippcode(ifield)

!     Open netcdf file

      filein = ncfiles(ifileinid(ifield))
      if (ifield == 1 .and. it == 1) then
         filein_save = filein
         lopen = .true.
      else
         lopen = filein /= filein_save
         if (lopen) then
            call close_ncfile(ncid,ierr)
            filein_save = filein
         endif
      endif
      if (lopen) call open_ncfile(filein, 'r', ncid, ierr)

!      if (it == 1) write(*,*)'Getting NetCDF variable ',trim(ncname(ifield)), &
!                             ' from file ',trim(filein)

!     Get ancil dimension values

      call get_gridinfo(ncname1,ncid,dimnames,dim,nz,nt1, &
                        model,igridtype(ifield),grid)
      nx = grid%nlong
      ny = grid%nlat
      lzonal = nx <= 1
      if (iavert == 0 .and. model == 1 .and. &
         (ilevtype(ifield) == 3 .or. ilevtype(ifield) == 4)) then
         call get_nclevels_atmos(filein,dimnames(3),nlev,version,ltheta(ifield))
         no3lev = nlev
      endif
      if (iovert == 0 .and. model == 2 .and. ilevtype(ifield) == 5) then
         call get_nclevels_ocean(filein,dimnames(3),noclev)
      endif

!     Check consistency with mask

      if (imasktype1 /= 0) then
         if (imask1 == 1) call chk_mask_grid(nx,ny)
         if (imask1 == 2) call chk_lfrac_grid(nx,ny)
      endif

      proccode = 0
      if (lzonal) proccode = proccode + 64
      if (nt2 > 1) proccode = proccode + 128

!     Check x,y,z dimensions are always the same

      ierr = 0
      if (nx > 1 .and. nx /= nx1) then
         write(*,*) 'ERROR: inconsistent x values in NetCDF file ',trim(filein)
         write(*,*) 'expected ',nx1,' values but got ',nx
         ierr = 1
      endif
      if (abs(ny-ny1) > 1) then
         write(*,*) 'ERROR: inconsistent y values in NetCDF file ',trim(filein)
         write(*,*) 'expected ',ny1,' values but got ',ny
         ierr = 1
      endif
      if (nlev1(ifield) > nz) then
         write(*,*)'ERROR: number of levels in netcdf file ',nz, &
                   ' is less than value specified in xancil ',nlev1(ifield)
         ierr = 1
      endif
      if (ierr /= 0) stop

!     Set up various level parameters

      levcode = 129
      iz0 = 1
      iz1 = nlev1(ifield)
      if (ilevtype(ifield) == 0) then      ! Single level
         ztype = 1
      else if (ilevtype(ifield) == 1) then ! Soil levels
         ztype = 4
      else if (ilevtype(ifield) == 2) then ! Pseudo levels
         ztype = 5
      else if (ilevtype(ifield) == 3) then ! Model levels
         ztype = 2
         if (iversion > 502) then
            levcode = 65
         else
            levcode = 9
         endif
      else if (ilevtype(ifield) == 4) then ! Ozone levels
         ztype = 3
         if (iversion > 502) then
            levcode = 65
         else
            levcode = 9
         endif
         iz0 = nlev-nlev1(ifield)+1
         iz1 = nlev
      else if (ilevtype(ifield) == 5) then ! Ocean depth levels
         ztype = 6
      endif
      if (model == 2) levcode = 0

!     Get input datatype

      lintvar = isncvarint(ncname1,ncid)

!     Get mask missing data value

      if (imasktype1 /= 0 .and. imask1 == 0) then
         if (lintvar) then
            call get_ncmdi_i(ncname1,ncid,imdi_nc)
!            write(*,*)'imdi_nc = ',imdi_nc
         else
            call get_ncmdi_r(ncname1,ncid,rmdi_nc)
!            write(*,*)'rmdi_nc = ',rmdi_nc
         endif
      endif

      do iz=iz0,iz1
         if (ilevtype(ifield) == 3 .or. ilevtype(ifield) == 4) then
            if ((lvertrev_od .and. iversion < 500) .or. &
                (lvertrev_nd .and. iversion >= 500)) then
               izz = -iz+iz0+iz1
            else
               izz = iz
            endif
         elseif (ilevtype(ifield) == 5) then
            if (ldepthrev) then
               izz = -iz+iz0+iz1
            else
               izz = iz
            endif
         else
            izz = iz
         endif
         if (ilevtype(ifield) == 4) izz = izz-nlev+nz

         if (ztype == 2 .or. ztype == 3 .or. ztype == 4) then
            fflev = iz
         else if (model == 2) then
            fflev = 0
         else
            fflev = 8888
         endif

!        Get data values

         if (lintvar) then
            call get_ncdata_i(ncname1,ncid,izz,itsel(it), &
                              grid,dim,nx,ny,1,1,idata)
         else
            call get_ncdata_r(ncname1,ncid,izz,itsel(it), &
                              grid,dim,nx,ny,1,1,rdata)
         endif

!        Apply land/sea mask

         if (imasktype1 /= 0) then
            if (idatatype1 == 1) then
               if (imask1 == 0 .and. rmdi /= rmdi_nc) then
                  where (rdata == rmdi_nc) rdata = rmdi
               else if (imask1 == 1 .and. imasktype1 == 1) then
                  where (mask) rdata = rmdi                ! Sea data only
               else if (imask1 == 1 .and. imasktype1 == 2) then
                  where (.not. mask) rdata = rmdi          ! Land data only
               else if (imask1 == 2 .and. imasktype1 == 1) then
                  where (alfrac >= 1.0_rtype) rdata = rmdi ! Sea data only
               else if (imask1 == 2 .and. imasktype1 == 2) then
                  where (alfrac <= 0.0_rtype) rdata = rmdi ! Land data only
               endif
            else
               if (.not. lintvar) then
                  if (imask1 == 0) then
                     where (rdata == rmdi_nc)
                        idata = imdi
                     elsewhere
                        idata = rdata
                     endwhere
                  else
                     idata = rdata
                  endif
               endif

               if (imask1 == 0 .and. lintvar .and. imdi /= imdi_nc) then
                  where (idata == imdi_nc) idata = imdi
               else if (imask1 == 1 .and. imasktype1 == 1) then
                  where (mask) idata = imdi                ! Sea data only
               else if (imask1 == 1 .and. imasktype1 == 2) then
                  where (.not. mask) idata = imdi          ! Land data only
               else if (imask1 == 2 .and. imasktype1 == 1) then
                  where (alfrac >= 1.0_rtype) idata = imdi ! Sea data only
               else if (imask1 == 2 .and. imasktype1 == 2) then
                  where (alfrac <= 0.0_rtype) idata = imdi ! Land data only
               endif
            endif
         else if (idatatype1 /= 1 .and. .not. lintvar) then
            idata = rdata
         endif

!        Call subroutine for any user modifications

         if (idatatype1 == 1) then
             call genanc_usermod_r(rdata,grid%along,grid%alat,nx,ny,iz,it, &
                                   idate(1,it),ifield,stashcode)
         else
             call genanc_usermod_i(idata,grid%along,grid%alat,nx,ny,iz,it, &
                                   idate(1,it),ifield,stashcode,idatatype1)
         endif

!        Write pp headers

         call write_pphead(ochan,pp_pos,idate(1,it),iz,ztype,ltheta(ifield), &
                           fieldcode,proccode,levcode,fftype,fflev, &
                           data_pos,istart,idatatype1,stashcode,grid)

!        Write data

         if (idatatype1 == 1) then
            call write_data_r(ochan,data_pos,rdata,nx,ny)
         else
            call write_data_i(ochan,data_pos,idata,nx,ny)
         endif

         istart = istart + nx*ny

      enddo

      call free_gridinfo(grid)

   enddo

enddo

! Close netcdf file

call close_ncfile(ncid, ierr)

! Close ancil file

call close_ancil(ochan)

deallocate(time)
deallocate(idate)
deallocate(itsel)
deallocate(rdata)
if (lint) deallocate(idata)

return
end
