
!==============================================================================!

subroutine create_orog_ancil(filein,fileout, &
                             ncname,ncsdname, &
                             ncxgradname,ncygradname, &
                             ncxxgradname,ncxygradname,ncyygradname, &
                             ncsilname,ncpthtname,ncunfiltname, &
                             lincgrad,lincsqgrad,lincrough,lincunfilt,imask)

use getkind
use constants
use parameters
use config
use lsmask
use lslfrac
use types

implicit none

character(*) filein,fileout
character(*) ncname,ncsdname
character(*) ncxgradname,ncygradname,ncxxgradname
character(*) ncxygradname,ncyygradname
character(*) ncsilname,ncpthtname,ncunfiltname
logical lincgrad,lincsqgrad,lincrough,lincunfilt
integer imask

real(rtype), dimension(:,:), allocatable :: orog,sd
real(rtype), dimension(:,:), allocatable :: xgrad,ygrad,xxgrad,xygrad,yygrad
real(rtype), dimension(:,:), allocatable :: sil,ptht,unfilt
real(rtype) rmdi_nc
integer(ptype) ochan
integer ncid,ierr
integer model,type,nfield,datasize,nftype,inthd8,datatype,ztype
integer(otype) pp_pos,data_pos
integer dim(4),nx,ny,nz,nt,intunit,interval,idate(6),iz,it(1)
integer proccode,levcode,fftype,fflev,istart,fieldcode,stashcode
logical lperiodic,ltheta
type(gridinfo) grid
character(max_varname_size) dimnames(4)

write(*,*)'Writing Orography ancillary file ',trim(fileout)

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

! Get mask values

if (imask == 0) then
   call get_ncmdi_r(ncname,ncid,rmdi_nc)
!   write(*,*)'rmdi_nc = ',rmdi_nc
endif

allocate(orog(nx,ny))
allocate(sd(nx,ny))
if (lincgrad) then
   allocate(xgrad(nx,ny))
   allocate(ygrad(nx,ny))
endif
if (lincsqgrad) then
   allocate(xxgrad(nx,ny))
   allocate(xygrad(nx,ny))
   allocate(yygrad(nx,ny))
endif
if (lincrough) then
   allocate(sil(nx,ny))
   allocate(ptht(nx,ny))
endif
if (lincunfilt) then
   allocate(unfilt(nx,ny))
endif

! Open ancil file

call open_ancil(ochan,fileout,'w')

! write fixed ancillary file headers

ztype = 1
nftype = 2
inthd8 = -1
if (lincgrad) nftype = nftype + 2
if (lincsqgrad) nftype = nftype + 3
if (lincrough) nftype = nftype + 2
if (lincunfilt) nftype = nftype + 1
nfield = nftype
datasize = nfield*nx*ny
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
fflev = 8888
istart = 1
datatype = 1
ltheta = .false.

!  Get Orography data values

call get_ncdata_r(ncname,ncid,iz,it,grid,dim,nx,ny,1,1,orog)

!  Apply land/sea mask

if (imask == 0) then
   where (orog == rmdi_nc) orog = 0.0_rtype
else if (imask == 1) then
   where (.not. mask) orog = 0.0_rtype
else if (imask == 2) then
   where (alfrac <= 0.0_rtype) orog = 0.0_rtype
endif

!  Get Standard deviation data values

call get_ncdata_r(ncsdname,ncid,iz,it,grid,dim,nx,ny,1,1,sd)

!  Apply land/sea mask

if (imask == 0) then
   where (sd == rmdi_nc) sd = 0.0_rtype
else if (imask == 1) then
   where (.not. mask) sd = 0.0_rtype
else if (imask == 2) then
   where (alfrac <= 0.0_rtype) sd = 0.0_rtype
endif

if (lincgrad) then

!  Get x gradient data values

   call get_ncdata_r(ncxgradname,ncid,iz,it,grid,dim,nx,ny,1,1,xgrad)

!  Apply land/sea mask

   if (imask == 0) then
      where (xgrad == rmdi_nc) xgrad = 0.0_rtype
   else if (imask == 1) then
      where (.not. mask) xgrad = 0.0_rtype
   else if (imask == 2) then
      where (alfrac <= 0.0_rtype) xgrad = 0.0_rtype
   endif

!  Get y gradient data values

   call get_ncdata_r(ncygradname,ncid,iz,it,grid,dim,nx,ny,1,1,ygrad)

!  Apply land/sea mask

   if (imask == 0) then
      where (ygrad == rmdi_nc) ygrad = 0.0_rtype
   else if (imask == 1) then
      where (.not. mask) ygrad = 0.0_rtype
   else if (imask == 2) then
      where (alfrac <= 0.0_rtype) ygrad = 0.0_rtype
   endif

endif

if (lincsqgrad) then

!  Get xx gradient data values

   call get_ncdata_r(ncxxgradname,ncid,iz,it,grid,dim,nx,ny,1,1,xxgrad)

!  Apply land/sea mask

   if (imask == 0) then
      where (xxgrad == rmdi_nc) xxgrad = 0.0_rtype
   else if (imask == 1) then
      where (.not. mask) xxgrad = 0.0_rtype
   else if (imask == 2) then
      where (alfrac <= 0.0_rtype) xxgrad = 0.0_rtype
   endif

!  Get xy gradient data values

   call get_ncdata_r(ncxygradname,ncid,iz,it,grid,dim,nx,ny,1,1,xygrad)

!  Apply land/sea mask

   if (imask == 0) then
      where (xygrad == rmdi_nc) xygrad = 0.0_rtype
   else if (imask == 1) then
      where (.not. mask) xygrad = 0.0_rtype
   else if (imask == 2) then
      where (alfrac <= 0.0_rtype) xygrad = 0.0_rtype
   endif

!  Get yy gradient data values

   call get_ncdata_r(ncyygradname,ncid,iz,it,grid,dim,nx,ny,1,1,yygrad)

!  Apply land/sea mask

   if (imask == 0) then
      where (yygrad == rmdi_nc) yygrad = 0.0_rtype
   else if (imask == 1) then
      where (.not. mask) yygrad = 0.0_rtype
   else if (imask == 2) then
      where (alfrac <= 0.0_rtype) yygrad = 0.0_rtype
   endif

endif

if (lincrough) then

!  Get Silhouette of orography data values

   call get_ncdata_r(ncsilname,ncid,iz,it,grid,dim,nx,ny,1,1,sil)

!  Apply land/sea mask

   if (imask == 0) then
      where (sil == rmdi_nc) sil = 0.0_rtype
   else if (imask == 1) then
      where (.not. mask) sil = 0.0_rtype
   else if (imask == 2) then
      where (alfrac <= 0.0_rtype) sil = 0.0_rtype
   endif

!  Get Peak to trough height data values

   call get_ncdata_r(ncpthtname,ncid,iz,it,grid,dim,nx,ny,1,1,ptht)

!  Apply land/sea mask

   if (imask == 0) then
      where (ptht == rmdi_nc) ptht = 0.0_rtype
   else if (imask == 1) then
      where (.not. mask) ptht = 0.0_rtype
   else if (imask == 2) then
      where (alfrac <= 0.0_rtype) ptht = 0.0_rtype
   endif

endif

if (lincunfilt) then

!  Get Unfiltered orography data values

   call get_ncdata_r(ncunfiltname,ncid,iz,it,grid,dim,nx,ny,1,1,unfilt)

!  Apply land/sea mask

   if (imask == 0) then
      where (unfilt == rmdi_nc) unfilt = 0.0_rtype
   else if (imask == 1) then
      where (.not. mask) unfilt = 0.0_rtype
   else if (imask == 2) then
      where (alfrac <= 0.0_rtype) unfilt = 0.0_rtype
   endif

endif

!  Call subroutine for any user modifications

call orog_usermod(orog,sd,xgrad,ygrad,xxgrad,xygrad,yygrad,sil,ptht,unfilt, &
                  lincgrad,lincsqgrad,lincrough,lincunfilt, &
                  grid%along,grid%alat,nx,ny)

!  Write Orography pp headers

fieldcode = 1
stashcode = 33
fftype = 73
call write_pphead(ochan,pp_pos,idate,0,ztype,ltheta,fieldcode,proccode, &
                  levcode,fftype,fflev,data_pos,istart,datatype, &
                  stashcode,grid)

!  Write Orography data

call write_data_r(ochan,data_pos,orog,nx,ny)

istart = istart + nx*ny

!  Write Standard deviation pp headers

fieldcode = 150
stashcode = 34
fftype = 186
call write_pphead(ochan,pp_pos,idate,0,ztype,ltheta,fieldcode,proccode, &
                  levcode,fftype,fflev,data_pos,istart,datatype, &
                  stashcode,grid)

!  Write Standard deviation data

call write_data_r(ochan,data_pos,sd,nx,ny)

istart = istart + nx*ny

if (lincgrad) then

!  Write x gradient pp headers

   fieldcode = 0
   stashcode = 5
   fftype = 0
   call write_pphead(ochan,pp_pos,idate,0,ztype,ltheta,fieldcode,proccode, &
                     levcode,fftype,fflev,data_pos,istart,datatype, &
                     stashcode,grid)

!  Write x gradient data

   call write_data_r(ochan,data_pos,xgrad,nx,ny)

   istart = istart + nx*ny

!  Write y gradient pp headers

   fieldcode = 0
   stashcode = 6
   fftype = 0
   call write_pphead(ochan,pp_pos,idate,0,ztype,ltheta,fieldcode,proccode, &
                     levcode,fftype,fflev,data_pos,istart,datatype, &
                     stashcode,grid)

!  Write y gradient data

   call write_data_r(ochan,data_pos,ygrad,nx,ny)

   istart = istart + nx*ny

endif

if (lincsqgrad) then

!  Write xx gradient pp headers

   fieldcode = 152
   stashcode = 35
   fftype = 218
   call write_pphead(ochan,pp_pos,idate,0,ztype,ltheta,fieldcode,proccode, &
                     levcode,fftype,fflev,data_pos,istart,datatype, &
                     stashcode,grid)

!  Write xx gradient data

   call write_data_r(ochan,data_pos,xxgrad,nx,ny)

   istart = istart + nx*ny

!  Write xy gradient pp headers

   fieldcode = 153
   stashcode = 36
   fftype = 219
   call write_pphead(ochan,pp_pos,idate,0,ztype,ltheta,fieldcode,proccode, &
                     levcode,fftype,fflev,data_pos,istart,datatype, &
                     stashcode,grid)

!  Write xy gradient data

   call write_data_r(ochan,data_pos,xygrad,nx,ny)

   istart = istart + nx*ny

!  Write yy gradient pp headers

   fieldcode = 154
   stashcode = 37
   fftype = 220
   call write_pphead(ochan,pp_pos,idate,0,ztype,ltheta,fieldcode,proccode, &
                     levcode,fftype,fflev,data_pos,istart,datatype, &
                     stashcode,grid)

!  Write yy gradient data

   call write_data_r(ochan,data_pos,yygrad,nx,ny)

   istart = istart + nx*ny

endif

if (lincrough) then

!  Write Silhouette of orography pp headers

   fieldcode = 174
   stashcode = 17
   fftype = 209
   call write_pphead(ochan,pp_pos,idate,0,ztype,ltheta,fieldcode,proccode, &
                     levcode,fftype,fflev,data_pos,istart,datatype, &
                     stashcode,grid)

!  Write Silhouette of orography data

   call write_data_r(ochan,data_pos,sil,nx,ny)

   istart = istart + nx*ny

!  Write Peak to trough height pp headers

   fieldcode = 175
   stashcode = 18
   fftype = 210
   call write_pphead(ochan,pp_pos,idate,0,ztype,ltheta,fieldcode,proccode, &
                     levcode,fftype,fflev,data_pos,istart,datatype, &
                     stashcode,grid)

!  Write Peak to trough height data

   call write_data_r(ochan,data_pos,ptht,nx,ny)

   istart = istart + nx*ny

endif

if (lincunfilt) then

!  Write Unfiltered orography pp headers

   fieldcode = 0
   stashcode = 7
   fftype = 0
   call write_pphead(ochan,pp_pos,idate,0,ztype,ltheta,fieldcode,proccode, &
                     levcode,fftype,fflev,data_pos,istart,datatype, &
                     stashcode,grid)

!  Write Unfiltered orography data

   call write_data_r(ochan,data_pos,unfilt,nx,ny)

endif

! Close netcdf file

call close_ncfile(ncid, ierr)

! Close ancil file

call close_ancil(ochan)

call free_gridinfo(grid)
deallocate(orog)
deallocate(sd)
if (lincgrad) then
   deallocate(xgrad)
   deallocate(ygrad)
endif
if (lincsqgrad) then
   deallocate(xxgrad)
   deallocate(xygrad)
   deallocate(yygrad)
endif
if (lincrough) then
   deallocate(sil)
   deallocate(ptht)
endif
if (lincunfilt) then
   deallocate(unfilt)
endif

return
end
