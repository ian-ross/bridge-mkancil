
!==============================================================================!

subroutine create_vegdist_ancil(filein,fileout,ncname,imask)

use getkind
use constants
use parameters
use config
use lsmask
use lslfrac
use types

implicit none

character(*) filein,fileout,ncname
integer imask

real(rtype), dimension(:,:), allocatable :: vegdist
real(rtype) avegdist_rmdi_nc
integer(ptype) ochan
integer ncid,ierr
integer model,type,nfield,datasize,nftype,inthd8,datatype,ztype
integer(otype) pp_pos,data_pos
integer dim(4),nx,ny,nz,nt,intunit,interval,idate(6),iz,it(1)
integer proccode,levcode,fftype,fflev,istart,fieldcode,stashcode
logical lperiodic,ltheta
type(gridinfo) grid
character(max_varname_size) dimnames(4)

write(*,*)'Writing Disturbed vegetation fraction ancillary file ',trim(fileout)

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

allocate(vegdist(nx,ny))

! Open ancil file

call open_ancil(ochan,fileout,'w')

! write fixed ancillary file headers

nfield = 1
datasize = nfield*nx*ny
nftype = nfield
inthd8 = -1
ztype = 1
intunit = 0
interval = 0
lperiodic = .false.
idate = 0
call write_head(ochan,pp_pos,data_pos,nfield,datasize,nftype,inthd8, &
                intunit,interval,lperiodic,ztype,grid,idate,1)

! pp header values for all fields

iz = 1
it(1) = 1
fieldcode = 1394
proccode = 0
levcode = 129
fftype = 0
fflev = 8888
stashcode = 219
istart = 1
datatype = 1
ltheta = .false.

! Get mask values

if (imask == 0) then
   call get_ncmdi_r(ncname,ncid,avegdist_rmdi_nc)
   !write(*,*)'avegdist_rmdi_nc = ',avegdist_rmdi_nc
endif

! Get Disturbed vegetation fraction data values

call get_ncdata_r(ncname,ncid,iz,it,grid,dim,nx,ny,1,1,vegdist)

! Apply land/sea mask

if (imask == 0 .and. rmdi /= avegdist_rmdi_nc) then
   where (vegdist == avegdist_rmdi_nc) vegdist = rmdi
else if (imask == 1) then
   where (.not. mask) vegdist = rmdi
else if (imask == 2) then
   where (alfrac <= 0.0_rtype) vegdist = rmdi
endif

! Call subroutine for any user modifications

call vegdist_usermod(vegdist,grid%along,grid%alat,nx,ny)

! Write Disturbed vegetation fraction pp headers

call write_pphead(ochan,pp_pos,idate,0,ztype,ltheta,fieldcode,proccode, &
                  levcode,fftype,fflev,data_pos,istart,datatype, &
                  stashcode,grid)

! Write Disturbed vegetation fraction data

call write_data_r(ochan,data_pos,vegdist,nx,ny)

istart = istart + nx*ny

! Close netcdf file

call close_ncfile(ncid, ierr)

! Close ancil file

call close_ancil(ochan)

call free_gridinfo(grid)
deallocate(vegdist)

return
end
