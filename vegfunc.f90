
!==============================================================================!

subroutine create_vegfunc_ancil(filein,fileout,nclainame,nccanhtname, &
                                nccancondname,lperiodic,imask,nfunctypes, &
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

character(*) filein,fileout,nclainame,nccanhtname,nccancondname
logical lperiodic,lmm
integer imask,nfunctypes,itimeusage1,itimeusage2,istartdate1(6)
integer ntimes,interval,intunit1

real(rtype), dimension(:,:), allocatable :: vegfunc
real(rtype), dimension(:), allocatable :: time
real(rtype) alai_rmdi_nc,acanht_rmdi_nc,acancond_rmdi_nc
integer(ptype) ochan
integer ncid,ierr
integer model,type,nfield,datasize,nftype,inthd8,datatype,ztype
integer(otype) pp_pos,data_pos
integer dim(4),nx,ny,nz,nt1,nt2,iz,it,intunit2,istartdate2(6),iscale
integer, dimension(:,:), allocatable :: idate
integer, dimension(:), allocatable :: itsel
integer proccode,levcode,fftype,fflev,istart,fieldcode,stashcode
logical ltheta,llai,lcanht,lcancond
type(gridinfo) grid
character(max_varname_size) dimnames(4),ncname

write(*,*)'Writing Vegetation functional types ancillary file ',trim(fileout)

model = 1 ! atmos model ancillary file
type = 1  ! data on atmospheric theta points
llai = len_trim(nclainame) /= 0
lcanht = len_trim(nccanhtname) /= 0
lcancond = len_trim(nccancondname) /= 0

if (llai) then
   ncname = nclainame
elseif (lcanht) then
   ncname = nccanhtname
elseif (lcancond) then
   ncname = nccancondname
else
   write(*,*)'ERROR: no Vegetation functional types variables defined'
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

if (nz < nfunctypes) then
   write(*,*)'ERROR: number of plant functional types in netcdf file ',nz, &
             ' is less than value specified in xancil ',nfunctypes
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
allocate(idate(6,nt2))
allocate(itsel(nt2))

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

allocate(vegfunc(nx,ny))

! Get mask values

if (imask == 0) then
   if (llai) call get_ncmdi_r(nclainame,ncid,alai_rmdi_nc)
   if (lcanht) call get_ncmdi_r(nccanhtname,ncid,acanht_rmdi_nc)
   if (lcancond) call get_ncmdi_r(nccancondname,ncid,acancond_rmdi_nc)
endif

! Open ancil file

call open_ancil(ochan,fileout,'w')

! write fixed ancillary file headers

nftype = 0
if (llai) nftype = nftype + nfunctypes
if (lcanht) nftype = nftype + nfunctypes
if (lcancond) nftype = nftype + 1
nfield = nftype*nt2
datasize = nfield*nx*ny
inthd8 = -1
ztype = 5
call write_head(ochan,pp_pos,data_pos,nfield,datasize,nftype,inthd8, &
                intunit1,interval,lperiodic,ztype,grid,idate,nt2)

! pp header values for all fields

proccode = 0
levcode = 129
fftype = 0
fflev = 8888
istart = 1
datatype = 1
ltheta = .false.

do it=1,nt2
   do iz=1,nfunctypes

      if (.not. llai) exit

      fieldcode = 1392
      stashcode = 217

!     Get Leaf area index data values

      call get_ncdata_r(nclainame,ncid,iz,itsel(it),grid,dim,nx,ny,1,1,vegfunc)

!     Apply land/sea mask

      if (imask == 0 .and. rmdi /= alai_rmdi_nc) then
         where (vegfunc == alai_rmdi_nc) vegfunc = rmdi
      else if (imask == 1) then
         where (.not. mask) vegfunc = rmdi
      else if (imask == 2) then
         where (alfrac <= 0.0_rtype) vegfunc = rmdi
      endif

!     Call subroutine for any user modifications

      call vegfunc_usermod(vegfunc,grid%along,grid%alat, &
                           nx,ny,iz,it,idate(1,it),stashcode)

!     Write Leaf area index types pp headers

      call write_pphead(ochan,pp_pos,idate(1,it),iz,ztype,ltheta,fieldcode, &
                        proccode,levcode,fftype,fflev,data_pos,istart, &
                        datatype,stashcode,grid)

!     Write Leaf area index types data

      call write_data_r(ochan,data_pos,vegfunc,nx,ny)

      istart = istart + nx*ny
   enddo

   do iz=1,nfunctypes

      if (.not. lcanht) exit

      fieldcode = 1393
      stashcode = 218

!     Get Canopy height types data values

      call get_ncdata_r(nccanhtname,ncid,iz,itsel(it),grid,dim, &
                        nx,ny,1,1,vegfunc)

!     Apply land/sea mask

      if (imask == 0 .and. rmdi /= acanht_rmdi_nc) then
         where (vegfunc == acanht_rmdi_nc) vegfunc = rmdi
      else if (imask == 1) then
         where (.not. mask) vegfunc = rmdi
      else if (imask == 2) then
         where (alfrac <= 0.0_rtype) vegfunc = rmdi
      endif

!     Call subroutine for any user modifications

      call vegfunc_usermod(vegfunc,grid%along,grid%alat, &
                           nx,ny,iz,it,idate(1,it),stashcode)

!     Write Canopy height types pp headers

      call write_pphead(ochan,pp_pos,idate(1,it),iz,ztype,ltheta,fieldcode, &
                        proccode,levcode,fftype,fflev,data_pos,istart, &
                        datatype,stashcode,grid)

!     Write Canopy height types data

      call write_data_r(ochan,data_pos,vegfunc,nx,ny)

      istart = istart + nx*ny
   enddo

   if (.not. lcancond) cycle

   iz = 1
   fieldcode = 1384
   stashcode = 213

!  Get Canopy conductance data values

   call get_ncdata_r(nccancondname,ncid,iz,itsel(it),grid,dim,nx,ny,1,1,vegfunc)

!  Apply land/sea mask

   if (imask == 0 .and. rmdi /= acancond_rmdi_nc) then
      where (vegfunc == acancond_rmdi_nc) vegfunc = rmdi
   else if (imask == 1) then
      where (.not. mask) vegfunc = rmdi
   else if (imask == 2) then
      where (alfrac <= 0.0_rtype) vegfunc = rmdi
   endif

!  Call subroutine for any user modifications

   call vegfunc_usermod(vegfunc,grid%along,grid%alat, &
                        nx,ny,0,it,idate(1,it),stashcode)

!  Write Canopy conductance pp headers

   call write_pphead(ochan,pp_pos,idate(1,it),0,1,ltheta,fieldcode, &
                     proccode,levcode,fftype,fflev,data_pos,istart, &
                     datatype,stashcode,grid)

!  Write Canopy conductance data

   call write_data_r(ochan,data_pos,vegfunc,nx,ny)

   istart = istart + nx*ny
enddo

! Close netcdf file

call close_ncfile(ncid, ierr)

! Close ancil file

call close_ancil(ochan)

call free_gridinfo(grid)
deallocate(vegfunc)

return
end
