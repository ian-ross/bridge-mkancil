
!==============================================================================!

subroutine create_veg_ancil(filein,fileout, &
                            ncrootdepthname,ncsfaname, &
                            ncsurfresistname,ncz0name, &
                            nccancapname,ncvegfracname, &
                            ncinfiltname,ncdsaname, &
                            nclainame,nccanhtname, &
                            imask)

use getkind
use constants
use parameters
use config
use lsmask
use lslfrac
use types

implicit none

character(*) filein,fileout
character(*) ncrootdepthname,ncsfaname,ncsurfresistname,ncz0name,nccancapname
character(*) ncvegfracname,ncinfiltname,ncdsaname,nclainame,nccanhtname
integer imask

real(rtype), dimension(:,:), allocatable :: vegparm
real(rtype) avegparm_rmdi_nc
integer, parameter :: maxnfield = 10
integer(ptype) ochan
integer ncid,ierr,ifield
integer model,type,nfield,datasize,nftype,inthd8,datatype,ztype
integer(otype) pp_pos,data_pos
integer dim(4),nx,ny,nz,nt,intunit,interval,idate(6),iz,it(1)
integer proccode,levcode,fftype,fflev,istart
integer fieldcode(maxnfield),stashcode(maxnfield)
logical lperiodic,ltheta
type(gridinfo) grid
character(max_varname_size) dimnames(4),ncname(maxnfield)

write(*,*)'Writing Vegetation parameters ancillary file ',trim(fileout)

model = 1 ! atmos model ancillary file
type = 1  ! data on atmospheric theta points

! Open netcdf file

call open_ncfile(filein, 'r', ncid, ierr)

! Check which fields to output

nfield = 0
do ifield=1,maxnfield

   nfield = nfield + 1

   if (ifield == 1) then
      ncname(nfield)=ncrootdepthname
      fieldcode(nfield) = 321
      stashcode(nfield) = 51
   else if (ifield == 2) then
      ncname(nfield)=ncsfaname
      fieldcode(nfield) = 322
      stashcode(nfield) = 52
   else if (ifield == 3) then
      ncname(nfield)=ncsurfresistname
      fieldcode(nfield) = 323
      stashcode(nfield) = 54
   else if (ifield == 4) then
      ncname(nfield)=ncz0name
      fieldcode(nfield) = 324
      stashcode(nfield) = 26
   else if (ifield == 5) then
      ncname(nfield)=nccancapname
      fieldcode(nfield) = 325
      stashcode(nfield) = 55
   else if (ifield == 6) then
      ncname(nfield)=ncvegfracname
      fieldcode(nfield) = 326
      stashcode(nfield) = 50
   else if (ifield == 7) then
      ncname(nfield)=ncinfiltname
      fieldcode(nfield) = 327
      stashcode(nfield) = 56
   else if (ifield == 8) then
      ncname(nfield)=ncdsaname
      fieldcode(nfield) = 328
      stashcode(nfield) = 53
   else if (ifield == 9) then
      ncname(nfield)=nclainame
      fieldcode(nfield) = 1382
      stashcode(nfield) = 208
   else if (ifield == 10) then
      ncname(nfield)=nccanhtname
      fieldcode(nfield) = 1383
      stashcode(nfield) = 209
   else
      exit
   endif

   if (len_trim(ncname(nfield)) == 0) then
      nfield = nfield - 1
      cycle
   endif
enddo

! Get ancil dimension values

call get_gridinfo(ncname(1),ncid,dimnames,dim,nz,nt,model,type,grid)
nx = grid%nlong
ny = grid%nlat

! Check consistency with mask

if (imask == 1) call chk_mask_grid(nx,ny)
if (imask == 2) call chk_lfrac_grid(nx,ny)

allocate(vegparm(nx,ny))

! Open ancil file

call open_ancil(ochan,fileout,'w')

! write fixed ancillary file headers

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
proccode = 0
levcode = 129
fftype = 0
fflev = 8888
istart = 1
datatype = 1
ltheta = .false.

do ifield=1,nfield

!  Get mask values

   if (imask == 0) then
      call get_ncmdi_r(ncname(ifield),ncid,avegparm_rmdi_nc)
      !write(*,*)'avegparm_rmdi_nc = ',avegparm_rmdi_nc
   endif

!  Get Vegetation parameter data values

   call get_ncdata_r(ncname(ifield),ncid,iz,it,grid,dim,nx,ny,1,1,vegparm)

!  Apply land/sea mask

   if (imask == 0 .and. rmdi /= avegparm_rmdi_nc) then
      where (vegparm == avegparm_rmdi_nc) vegparm = rmdi
   else if (imask == 1) then
      where (.not. mask) vegparm = rmdi
   else if (imask == 2) then
      where (alfrac <= 0.0_rtype) vegparm = rmdi
   endif

!  Call subroutine for any user modifications

   call veg_usermod(vegparm,grid%along,grid%alat,nx,ny,stashcode(ifield))

!  Write Vegetation parameter pp headers

   call write_pphead(ochan,pp_pos,idate,0,ztype,ltheta, &
                     fieldcode(ifield),proccode,levcode,fftype,fflev, &
                     data_pos,istart,datatype,stashcode(ifield),grid)

!  Write Vegetation parameter data

   call write_data_r(ochan,data_pos,vegparm,nx,ny)

   istart = istart + nx*ny

enddo

! Close netcdf file

call close_ncfile(ncid, ierr)

! Close ancil file

call close_ancil(ochan)

call free_gridinfo(grid)
deallocate(vegparm)

return
end
