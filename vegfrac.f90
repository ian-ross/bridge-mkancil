
!==============================================================================!

subroutine create_vegfrac_ancil(filein,fileout,ncname,imask,nsurftypes)

use getkind
use constants
use parameters
use config
use lsmask
use lslfrac
use types

implicit none

character(*) filein,fileout,ncname
integer imask,nsurftypes

real(rtype), dimension(:,:), allocatable :: vegfrac
real(rtype) avegfrac_rmdi_nc
integer(ptype) ochan
integer ncid,ierr
integer model,type,nfield,datasize,nftype,inthd8,datatype,ztype
integer(otype) pp_pos,data_pos
integer dim(4),nx,ny,nz,nt,intunit,interval,idate(6),iz,it(1)
integer proccode,levcode,fftype,fflev,istart,fieldcode,stashcode
logical lperiodic,ltheta
type(gridinfo) grid
character(max_varname_size) dimnames(4)

write(*,*)'Writing Vegetation fractions ancillary file ',trim(fileout)

model = 1 ! atmos model ancillary file
type = 1  ! data on atmospheric theta points

! Open netcdf file

call open_ncfile(filein, 'r', ncid, ierr)

! Get ancil dimension values

call get_gridinfo(ncname,ncid,dimnames,dim,nz,nt,model,type,grid)
nx = grid%nlong
ny = grid%nlat

! Check consistency with mask

if (imask == 1) call chk_mask_grid(nx,ny)
if (imask == 2) call chk_lfrac_grid(nx,ny)

allocate(vegfrac(nx,ny))

! Get mask values

if (imask == 0) then
   call get_ncmdi_r(ncname,ncid,avegfrac_rmdi_nc)
   !write(*,*)'avegfrac_rmdi_nc = ',avegfrac_rmdi_nc
endif

! Open ancil file

call open_ancil(ochan,fileout,'w')

! write fixed ancillary file headers

nfield = nsurftypes
datasize = nfield*nx*ny
nftype = nfield
inthd8 = -1
ztype = 5
intunit = 0
interval = 0
lperiodic = .false.
idate = 0
call write_head(ochan,pp_pos,data_pos,nfield,datasize,nftype,inthd8, &
                intunit,interval,lperiodic,ztype,grid,idate,1)

! pp header values for all fields

it(1) = 1
fieldcode = 1391
proccode = 0
levcode = 129
fftype = 0
fflev = 8888
stashcode = 216
istart = 1
datatype = 1
ltheta = .false.

do iz=1,nsurftypes

!  Get Vegetation fractions data values

   call get_ncdata_r(ncname,ncid,iz,it,grid,dim,nx,ny,1,1,vegfrac)

!  Apply land/sea mask

   if (imask == 0 .and. rmdi /= avegfrac_rmdi_nc) then
      where (vegfrac == avegfrac_rmdi_nc) vegfrac = rmdi
   else if (imask == 1) then
      where (.not. mask) vegfrac = rmdi
   else if (imask == 2) then
      where (alfrac <= 0.0_rtype) vegfrac = rmdi
   endif

!  Call subroutine for any user modifications

   call vegfrac_usermod(vegfrac,grid%along,grid%alat,nx,ny,iz)

!  Write Vegetation fractions pp headers

   call write_pphead(ochan,pp_pos,idate,iz,ztype,ltheta,fieldcode,proccode, &
                     levcode,fftype,fflev,data_pos,istart,datatype, &
                     stashcode,grid)

!  Write Vegetation fractions data

   call write_data_r(ochan,data_pos,vegfrac,nx,ny)

   istart = istart + nx*ny
enddo

! Close netcdf file

call close_ncfile(ncid, ierr)

! Close ancil file

call close_ancil(ochan)

call free_gridinfo(grid)
deallocate(vegfrac)

return
end
