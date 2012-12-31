
!==============================================================================!

subroutine create_soil_ancil(filein,fileout, &
                             ncvsmcwiltname,ncvsmccritname, &
                             ncvsmcfcapname,ncvsmcsatname, &
                             ncclapphornname,ncthermcondname, &
                             ncsoilcondname,ncthermcapname, &
                             ncsoilwatersucname,ncsoilalbname, &
                             ncsoilcarbname, &
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
character(*) ncvsmcwiltname,ncvsmccritname,ncvsmcfcapname,ncvsmcsatname
character(*) ncclapphornname,ncthermcondname,ncsoilcondname,ncthermcapname
character(*) ncsoilwatersucname,ncsoilalbname,ncsoilcarbname
integer imask

real(rtype), dimension(:,:), allocatable :: soilparm
real(rtype) asoilparm_rmdi_nc
integer, parameter :: maxnfield = 11
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

write(*,*)'Writing Soil parameters ancillary file ',trim(fileout)

model = 1 ! atmos model ancillary file
type = 1  ! data on atmospheric theta points

! Open netcdf file

call open_ncfile(filein, 'r', ncid, ierr)

! Check which fields to output

nfield = 0
do ifield=1,maxnfield

   nfield = nfield + 1

   if (ifield == 1) then
      ncname(nfield)=ncvsmcwiltname
      fieldcode(nfield) = 329
      stashcode(nfield) = 40
   else if (ifield == 2) then
      ncname(nfield)=ncvsmccritname
      fieldcode(nfield) = 330
      stashcode(nfield) = 41
   else if (ifield == 3) then
      ncname(nfield)=ncvsmcfcapname
      fieldcode(nfield) = 331
      stashcode(nfield) = 42
   else if (ifield == 4) then
      ncname(nfield)=ncvsmcsatname
      fieldcode(nfield) = 332
      stashcode(nfield) = 43
   else if (ifield == 5) then
      ncname(nfield)=ncclapphornname
      fieldcode(nfield) = 1381
      stashcode(nfield) = 207
   else if (ifield == 6) then
      ncname(nfield)=ncthermcondname
      fieldcode(nfield) = 336
      stashcode(nfield) = 47
   else if (ifield == 7) then
      ncname(nfield)=ncsoilcondname
      fieldcode(nfield) = 333
      stashcode(nfield) = 44
   else if (ifield == 8) then
      ncname(nfield)=ncthermcapname
      fieldcode(nfield) = 335
      stashcode(nfield) = 46
   else if (ifield == 9) then
      ncname(nfield)=ncsoilwatersucname
      fieldcode(nfield) = 342
      stashcode(nfield) = 48
   else if (ifield == 10) then
      ncname(nfield)=ncsoilalbname
      fieldcode(nfield) = 1395
      stashcode(nfield) = 220
   else if (ifield == 11) then
      ncname(nfield)=ncsoilcarbname
      fieldcode(nfield) = 1397
      stashcode(nfield) = 223
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

allocate(soilparm(nx,ny))

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
      call get_ncmdi_r(ncname(ifield),ncid,asoilparm_rmdi_nc)
      !write(*,*)'asoilparm_rmdi_nc = ',asoilparm_rmdi_nc
   endif

!  Get Soil parameter data values

   call get_ncdata_r(ncname(ifield),ncid,iz,it,grid,dim,nx,ny,1,1,soilparm)

!  Apply land/sea mask

   if (imask == 0 .and. rmdi /= asoilparm_rmdi_nc) then
      where (soilparm == asoilparm_rmdi_nc) soilparm = rmdi
   else if (imask == 1) then
      where (.not. mask) soilparm = rmdi
   else if (imask == 2) then
      where (alfrac <= 0.0_rtype) soilparm = rmdi
   endif

!  Call subroutine for any user modifications

   call soil_usermod(soilparm,grid%along,grid%alat,nx,ny,stashcode(ifield))

!  Write Soil parameter pp headers

   call write_pphead(ochan,pp_pos,idate,0,ztype,ltheta,fieldcode(ifield), &
                     proccode,levcode,fftype,fflev,data_pos,istart,datatype, &
                     stashcode(ifield),grid)

!  Write Soil parameter data

   call write_data_r(ochan,data_pos,soilparm,nx,ny)

   istart = istart + nx*ny

enddo

! Close netcdf file

call close_ncfile(ncid, ierr)

! Close ancil file

call close_ancil(ochan)

call free_gridinfo(grid)
deallocate(soilparm)

return
end
