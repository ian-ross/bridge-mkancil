
!==============================================================================!

subroutine create_slt_ancil(filein,fileout,ncname,lperiodic,imask, &
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

character(*) filein,fileout,ncname
logical lperiodic,lmm
integer imask,itimeusage1,itimeusage2,istartdate1(6)
integer ntimes,interval,intunit1

real(rtype), dimension(:,:), allocatable :: slt
real(rtype), dimension(:), allocatable :: time
real(rtype) aslt_rmdi_nc
integer(ptype) ochan
integer ncid,ierr,iz,it,intunit2,istartdate2(6),iscale
integer model,type,nfield,datasize,nftype,inthd8,datatype,ztype
integer(otype) pp_pos,data_pos
integer dim(4),nx,ny,nz,nt1,nt2
integer, dimension(:,:), allocatable :: idate
integer, dimension(:), allocatable :: itsel
integer fieldcode,proccode,levcode,stashcode,fftype,fflev,istart
logical ltheta
type(gridinfo) grid
character(max_varname_size) dimnames(4)

write(*,*)'Writing Deep soil temperatures ancillary file ',trim(fileout)

model = 1 ! atmos model ancillary file
type = 1  ! data on atmospheric theta points

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

allocate(slt(nx,ny))

!  Get mask values

if (imask == 0) then
   call get_ncmdi_r(ncname,ncid,aslt_rmdi_nc)
!   write(*,*)'aslt_rmdi_nc = ',aslt_rmdi_nc
endif

! Open ancil file

call open_ancil(ochan,fileout,'w')

! write fixed ancillary file headers

nfield = nsoillev*nt2 ! number of fields in ancil file
datasize = nfield*nx*ny
nftype = 1
inthd8 = -1
ztype = 4
call write_head(ochan,pp_pos,data_pos,nfield,datasize,nftype,inthd8, &
                intunit1,interval,lperiodic,ztype,grid,idate,nt2)

! pp header values for all fields

fieldcode = 23
proccode = 128
levcode = 129
fftype = 58
!fflev = 8888
stashcode = 20
istart = 1
datatype = 1
ltheta = .false.

do it=1,nt2
   do iz=1,nsoillev

!     Get Deep soil temperature data values

      call get_ncdata_r(ncname,ncid,iz,itsel(it),grid,dim, &
                        nx,ny,1,1,slt)

!     Apply land/sea mask

      if (imask == 0 .and. rmdi /= aslt_rmdi_nc) then
         where (slt == aslt_rmdi_nc) slt = rmdi
      else if (imask == 1) then
         where (.not. mask) slt = rmdi
      else if (imask == 2) then
         where (alfrac <= 0.0_rtype) slt = rmdi
      endif

!     Call subroutine for any user modifications

      call slt_usermod(slt,grid%along,grid%alat,nx,ny,iz,it,idate(1,it))

!     Write Deep soil temperature pp headers

      fflev = iz
      call write_pphead(ochan,pp_pos,idate(1,it),iz,ztype,ltheta, &
                        fieldcode,proccode,levcode,fftype,fflev, &
                        data_pos,istart,datatype,stashcode,grid)

!     Write Deep soil temperature data

      call write_data_r(ochan,data_pos,slt,nx,ny)

      istart = istart + nx*ny

   enddo
enddo

! Close netcdf file

call close_ncfile(ncid, ierr)

! Close ancil file

call close_ancil(ochan)

call free_gridinfo(grid)
deallocate(time)
deallocate(idate)
deallocate(itsel)
deallocate(slt)

return
end

