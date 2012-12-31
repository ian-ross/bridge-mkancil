
!==============================================================================!

subroutine create_flux_ancil(filein,fileout,ncheatname,ncsaltname, &
                             lheatflx,lsaltflx,lperiodic,imask, &
                             itimeusage1,itimeusage2,istartdate1, &
                             ntimes,interval,intunit1,lmm)

use getkind
use constants
use parameters
use config
use types

implicit none

character(*) filein,fileout,ncheatname,ncsaltname
logical lheatflx,lsaltflx,lperiodic,lmm
integer imask,itimeusage1,itimeusage2,istartdate1(6)
integer ntimes,interval,intunit1

real(rtype), dimension(:,:), allocatable :: heatflx,saltflx
real(rtype), dimension(:), allocatable :: time
real(rtype) rmdi_nc
integer(ptype) ochan
integer ncid,ierr,iz,it,intunit2,istartdate2(6)
integer iscale,model,type,nfield,datasize,nftype,inthd8,datatype,ztype
integer(otype) pp_pos,data_pos
integer dim(4),nx,ny,nz,nt1,nt2
integer, dimension(:,:), allocatable :: idate
integer, dimension(:), allocatable :: itsel
integer fieldcode,proccode,levcode,stashcode,fftype,fflev,istart
logical ltheta
type(gridinfo) grid
character(max_varname_size) dimnames(4)
character(max_varname_size) ncname

write(*,*)'Writing Flux Correction ancillary file ',trim(fileout)

model = 2 ! ocean model ancillary file
type = 1  ! data on ocean mass points

if (lheatflx) then
   ncname = ncheatname
elseif (lsaltflx) then
   ncname = ncsaltname
else
   write(*,*)'ERROR: no flux correction fields requested'
   stop
endif

! Open netcdf file

call open_ncfile(filein, 'r', ncid, ierr)

! Get ancil dimension values

call get_gridinfo(ncname,ncid,dimnames,dim,nz,nt1,model,type,grid)
nx = grid%nlong
ny = grid%nlat

! Check time selection consistency

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

!do it=1,nt2
!   write(*,*)'date = ',idate(1,it),idate(2,it),idate(3,it), &
!                       idate(4,it),idate(5,it),idate(6,it),itsel(it)
!enddo

if (itimeusage1 == 0) intunit1 = intunit2

! Get mask values

if (imask == 0) then
   call get_ncmdi_r(ncname,ncid,rmdi_nc)
!   write(*,*)'rmdi_nc = ',rmdi_nc
endif

if (lheatflx) allocate(heatflx(nx,ny))
if (lsaltflx) allocate(saltflx(nx,ny))

! Write ancillary file headers

! Open ancil file

call open_ancil(ochan,fileout,'w')

! write fixed ancillary file headers

ztype = 1
nftype = 0
inthd8 = -1
if (lheatflx) nftype = nftype + 1
if (lsaltflx) nftype = nftype + 1
nfield = nftype*nt2 ! number of fields in ancil file
datasize = nfield*nx*ny
call write_head(ochan,pp_pos,data_pos,nfield,datasize,nftype,inthd8, &
                intunit1,interval,lperiodic,ztype,grid,idate,nt2)

! pp header values for all fields

proccode = 128
fftype = 0
fflev = 0
levcode = 0
istart = 1
datatype = 1
ltheta = .false.
iz = 1

do it=1,nt2

   if (lheatflx) then

!     Get heat flux data values

      call get_ncdata_r(ncheatname,ncid,iz,itsel(it),grid,dim, &
                        nx,ny,1,1,heatflx)

!     Apply land/sea mask

      if (imask == 0) then
         where (heatflx == rmdi_nc) heatflx = rmdi
      endif

   endif

   if (lsaltflx) then

!     Get salinity flux data values

      call get_ncdata_r(ncsaltname,ncid,iz,itsel(it),grid,dim, &
                        nx,ny,1,1,saltflx)

!     Apply land/sea mask

      if (imask == 0) then
         where (saltflx == rmdi_nc) saltflx = rmdi
      endif

   endif

!  Call subroutine for any user modifications

   call flux_usermod(heatflx,saltflx,grid%along,grid%alat, &
                     nx,ny,it,idate(1,it))

   if (lheatflx) then

!     Write heat flux pp headers

      fieldcode = 671
      stashcode = 185

      call write_pphead(ochan,pp_pos,idate(1,it),0,ztype,ltheta, &
                        fieldcode,proccode,levcode,fftype,fflev, &
                        data_pos,istart,datatype,stashcode,grid)

!     Write heat flux data

      call write_data_r(ochan,data_pos,heatflx,nx,ny)

      istart = istart + nx*ny

   endif

   if (lsaltflx) then

!     Write salinity flux pp headers

      fieldcode = 672
      stashcode = 186

      call write_pphead(ochan,pp_pos,idate(1,it),0,ztype,ltheta, &
                        fieldcode,proccode,levcode,fftype,fflev, &
                        data_pos,istart,datatype,stashcode,grid)

!     Write salinity flux data

      call write_data_r(ochan,data_pos,saltflx,nx,ny)

      istart = istart + nx*ny

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
if (lheatflx) deallocate(heatflx)
if (lsaltflx) deallocate(saltflx)

return
end
