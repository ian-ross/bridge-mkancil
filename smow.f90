
!==============================================================================!

subroutine create_smow_ancil(filein,fileout,ncsnowdepthname, &
                             ncsnowedgename,ncsoilmoistname, &
                             lcalcsnowedge,lperiodic,imask, &
                             itimeusage1,itimeusage2,istartdate1, &
                             ntimes,interval,intunit1,lmm)

use getkind
use constants
use parameters
use config
use lsmask
use lslfrac
use types

implicit none

character(*) filein,fileout,ncsnowdepthname,ncsnowedgename,ncsoilmoistname
logical lcalcsnowedge,lperiodic,lmm
integer imask,itimeusage1,itimeusage2,istartdate1(6)
integer ntimes,interval,intunit1

real(rtype), dimension(:,:,:), pointer :: snowdepth2
real(rtype), dimension(:,:), pointer :: snowdepth,snowdepth_prev,snowdepth_save
real(rtype), dimension(:,:), allocatable :: snowedge,soilmoist
real(rtype), dimension(:), allocatable :: time
real(rtype) asoilmoist_rmdi_nc,asnowdepth_rmdi_nc,asnowedge_rmdi_nc
integer(ptype) ochan
integer ncid,ierr,iz,it,intunit2,istartdate2(6),iscale
integer model,type,nfield,datasize,nrec,nftype,inthd8,datatype,ztype
integer isnowedge_data_size
integer(otype) pp_pos,data_pos,isnowedge_data_pos
integer dim(4),nx,ny,nz,nt1,nt2,nt3,ntif
integer, dimension(:,:), allocatable :: idate
integer, dimension(:), allocatable :: itsel
integer fieldcode,proccode,levcode,stashcode,fftype,fflev,istart
logical ltheta,lsnowdepth,lsnowedge,lsoilmoist
type(gridinfo) grid
character(max_varname_size) dimnames(4),ncname

write(*,*)'Writing Soil moisture and snow depth ancillary file ',trim(fileout)

model = 1 ! atmos model ancillary file
type = 1  ! data on atmospheric theta points
lsoilmoist = len_trim(ncsoilmoistname) /= 0
lsnowdepth = len_trim(ncsnowdepthname) /= 0
lsnowedge = len_trim(ncsnowedgename) /= 0

if (lsoilmoist) then
   ncname = ncsoilmoistname
elseif (lsnowdepth) then
   ncname = ncsnowdepthname
elseif (lsnowedge) then
   ncname = ncsnowedgename
else
   write(*,*)'ERROR: no Soil moisture and snow depth variables defined'
   stop
endif

! Open netcdf file

call open_ncfile(filein, 'r', ncid, ierr)

! Get ancil dimension values

call get_gridinfo(ncname,ncid,dimnames,dim,nz,nt1,model,type,grid)
nx = grid%nlong
ny = grid%nlat

! Check consistency with mask

if (imask == 1) call chk_mask_grid(nx,ny)
if (imask == 2) call chk_lfrac_grid(nx,ny)

! Check consistency with level and time selection

if (nz < nsoillev) then
   write(*,*)'ERROR: number of soil levels in netcdf file ',nz, &
             ' is less than value specified in xancil ',nsoillev
   stop
endif

if (dim(4) < 1) then
   if (itimeusage1 == 0 .or. (itimeusage1 == 1 .and. itimeusage2 == 1)) then
      write(*,*)'ERROR: Cannot read date from NetCDF file ',trim(filein), &
                ' as it has no time dimension'
      stop 
   else if (itimeusage1 == 1 .and. itimeusage2 == 0 .and. ntimes > 1) then
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

if (itimeusage1 == 0 .or. (itimeusage1 == 1 .and. itimeusage2 == 1)) &
   call get_ncstartdate(dimnames(4),ncid,istartdate2,intunit2,iscale)

call setdates(itimeusage1,itimeusage2,lmm,interval,intunit1,intunit2,ical, &
              iscale,ntimes,istartdate1,istartdate2,time,nt1,itsel,idate,nt2)

if (itimeusage1 == 0) intunit1 = intunit2

if (lcalcsnowedge) then
   ntif = 2
   nt3 = nt2+1
   itsel(nt3) = itsel(1)
   idate(:,nt3) = idate(:,1)
   if (l32bit) then
      isnowedge_data_size = (nx*ny+1)/2
   else
      isnowedge_data_size = nx*ny
   endif
   if (lwfio .and. iwfio_size > 1) then
      nrec = isnowedge_data_size / iwfio_size
      if (mod(isnowedge_data_size,iwfio_size) /= 0) nrec = nrec + 1
      isnowedge_data_size = nrec*iwfio_size
   endif
else
   ntif = 1
   nt3 = nt2
endif

allocate(soilmoist(nx,ny))
allocate(snowdepth2(nx,ny,ntif))
snowdepth_prev => snowdepth2(:,:,1)
snowdepth => snowdepth2(:,:,ntif)
allocate(snowedge(nx,ny))

!  Get mask values

if (imask == 0) then
   call get_ncmdi_r(ncsoilmoistname,ncid,asoilmoist_rmdi_nc)
   call get_ncmdi_r(ncsnowdepthname,ncid,asnowdepth_rmdi_nc)
endif
if (.not. lcalcsnowedge) then
   call get_ncmdi_r(ncsnowedgename,ncid,asnowedge_rmdi_nc)
endif

! Open ancil file

call open_ancil(ochan,fileout,'w')

! write fixed ancillary file headers

nfield = (nsoillev+2)*nt2 ! number of fields in ancil file
datasize = nfield*nx*ny
nftype = 3
inthd8 = -1
ztype = 4
call write_head(ochan,pp_pos,data_pos,nfield,datasize,nftype,inthd8, &
                intunit1,interval,lperiodic,ztype,grid,idate,nt2)

! pp header values for all fields

proccode = 128
levcode = 129
!fflev = 8888
istart = 1
datatype = 1
ltheta = .false.

do it=1,nt3
   iz = 1

!  Get Snow depth data values

   call get_ncdata_r(ncsnowdepthname,ncid,iz,itsel(it),grid,dim, &
                     nx,ny,1,1,snowdepth)

!  Apply land/sea mask

   if (imask == 0 .and. rmdi /= asnowdepth_rmdi_nc) then
      where (snowdepth == asnowdepth_rmdi_nc) snowdepth = rmdi
   else if (imask == 1) then
      where (.not. mask) snowdepth = rmdi
   else if (imask == 2) then
      where (alfrac <= 0.0_rtype) snowdepth = rmdi
   endif

!  Call subroutine for any user modifications

   call snowdepth_usermod(snowdepth,grid%along,grid%alat,nx,ny,it,idate(1,it))

   if (lcalcsnowedge .and. it /= 1) then

!     Calculate Snow edge data values

      if (.not. lperiodic .and. it == nt2+1) then
         snowedge = rmdi
      else
         call getfractime(grid,snowdepth_prev,snowdepth,snowedge,nx,ny)
      endif
   endif

   if (it /= nt2+1) then

!     pp header values for snow depth

      fieldcode = 93
      stashcode = 23
      fftype = 121
      fflev = 8888

!     Write Snow depth pp headers

      call write_pphead(ochan,pp_pos,idate(1,it),0,1,ltheta, &
                       fieldcode,proccode,levcode,fftype,fflev, &
                       data_pos,istart,datatype,stashcode,grid)

!     Write Snow depth data

      call write_data_r(ochan,data_pos,snowdepth,nx,ny)

      istart = istart + nx*ny

      if (.not. lcalcsnowedge) then

!        Get Snow edge data values

         call get_ncdata_r(ncsnowedgename,ncid,iz,itsel(it),grid,dim, &
                           nx,ny,1,1,snowedge)

         if (rmdi /= asnowedge_rmdi_nc) then
            where (snowedge == asnowedge_rmdi_nc) snowedge = rmdi
         endif
      endif

!     pp header values for snow edge

      fieldcode = 93
      stashcode = 27
      fftype = 0
      fflev = 0

!     Write Snow edge pp headers

      call write_pphead(ochan,pp_pos,idate(1,it),0,1,ltheta, &
                        fieldcode,proccode,levcode,fftype,fflev, &
                        data_pos,istart,datatype,stashcode,grid)

!     Write Snow edge data

      if (lcalcsnowedge) then
         if (it > 1) then
            call write_data_r(ochan,isnowedge_data_pos,snowedge,nx,ny)
         endif
         isnowedge_data_pos = data_pos
         data_pos = data_pos + isnowedge_data_size
      else
         call write_data_r(ochan,data_pos,snowedge,nx,ny)
      endif

      istart = istart + nx*ny

!     pp header values for soil moisture

      fieldcode = 122
      stashcode = 9
      fftype = 191

      do iz=1,nsoillev

!        Get Soil moisture data values

         call get_ncdata_r(ncsoilmoistname,ncid,iz,itsel(it),grid,dim, &
                           nx,ny,1,1,soilmoist)

!        Apply land/sea mask

         if (imask == 0 .and. rmdi /= asoilmoist_rmdi_nc) then
            where (soilmoist == asoilmoist_rmdi_nc) soilmoist = rmdi
         else if (imask == 1) then
            where (.not. mask) soilmoist = rmdi
         else if (imask == 2) then
            where (alfrac <= 0.0_rtype) soilmoist = rmdi
         endif

!        Call subroutine for any user modifications

         call soilmoist_usermod(soilmoist,grid%along,grid%alat,nx,ny,iz,it, &
                                idate(1,it))

!        Write Soil moisture pp headers

         fflev = iz
         call write_pphead(ochan,pp_pos,idate(1,it),iz,ztype,ltheta, &
                           fieldcode,proccode,levcode,fftype,fflev, &
                           data_pos,istart,datatype,stashcode,grid)

!        Write Soil moisture data

         call write_data_r(ochan,data_pos,soilmoist,nx,ny)

         istart = istart + nx*ny

      enddo
   elseif (lcalcsnowedge .and. it == nt2+1) then
      call write_data_r(ochan,isnowedge_data_pos,snowedge,nx,ny)
   endif

   if (lcalcsnowedge) then
      snowdepth_save => snowdepth_prev
      snowdepth_prev => snowdepth
      snowdepth => snowdepth_save
   endif
enddo

! Close netcdf file

call close_ncfile(ncid, ierr)

! Close ancil file

call close_ancil(ochan)

call free_gridinfo(grid)
deallocate(time)
deallocate(idate)
deallocate(itsel)
deallocate(soilmoist)
deallocate(snowdepth2)
deallocate(snowedge)

return
end

