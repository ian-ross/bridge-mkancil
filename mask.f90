
!==============================================================================!

subroutine get_mask(filein,ncname,luselfrac,lusemdi,lsea,aval,ival)

use getkind
use parameters
use config
use lsmask
use lslfrac
use types

implicit none

character(*) filein,ncname
real(rtype) aval
integer ival
logical luselfrac,lusemdi,lsea

integer, dimension(:,:), allocatable :: imask
integer ierr,ncid,iz,it(1)
integer dim(4),nx,ny,nz,nt,imdi_nc,model,type
character(max_varname_size) dimnames(4)
real(rtype), dimension(:,:), allocatable :: amask
real(rtype) rmdi_nc
logical lintvar,isncvarint

if (luselfrac) then

   if (.not. lnclfrac) then
      write(*,*)'ERROR: Land fraction field not defined, ', &
                  'cannot create Land/Sea Mask'
      stop
   endif

   call copy_gridinfo(lfrac_grid, mask_grid)
   nx = mask_grid%nlong
   ny = mask_grid%nlat

!  Calculate logical Land/Sea mask, land=true, sea=false

   allocate(mask(nx,ny))

   where (alfrac > 0.0_rtype)
      mask = .true.
   elsewhere
      mask = .false.
   endwhere

else

   model = 1 ! atmos model ancillary file
   type = 1  ! data on atmospheric theta points

!  Open netcdf file

   call open_ncfile(filein, 'r', ncid, ierr)

!  Get ancil dimension values

   call get_gridinfo(ncname,ncid,dimnames,dim,nz,nt,model,type,mask_grid)
   nx = mask_grid%nlong
   ny = mask_grid%nlat

!  Get datatype

   lintvar = isncvarint(ncname,ncid)

!  Get mask values

   iz = 1
   it(1) = 1
   if (lintvar) then
      allocate(imask(nx,ny))
      call get_ncdata_i(ncname,ncid,iz,it,mask_grid,dim,nx,ny,1,1,imask)
   else
      allocate(amask(nx,ny))
      call get_ncdata_r(ncname,ncid,iz,it,mask_grid,dim,nx,ny,1,1,amask)
   endif

!  Calculate logical Land/Sea mask, land=true, sea=false

   allocate(mask(nx,ny))

   mask = lsea
   if (lusemdi) then
      if (lintvar) then
         call get_ncmdi_i(ncname,ncid,imdi_nc)
         where(imask == imdi_nc) mask = .not. lsea
      else
         call get_ncmdi_r(ncname,ncid,rmdi_nc)
         where(amask == rmdi_nc) mask = .not. lsea
      endif
   else
      if (lintvar) then
         if (ival == -987789) ival = aval
         where(imask == ival) mask = .not. lsea
      else
         if (aval == -987789.0_rtype) aval = ival
         where(amask == aval) mask = .not. lsea
      endif
   endif

!  Close netcdf file

   call close_ncfile(ncid, ierr)

   if (lintvar) then
      deallocate(imask)
   else
      deallocate(amask)
   endif
endif

! Call subroutine for any user modifications

call mask_usermod(mask,mask_grid%along,mask_grid%alat,nx,ny)

write(*,*)'Number of land points in Land/Sea Mask = ',count(mask)
write(*,*)

return
end

!==============================================================================!

subroutine create_mask_ancil(filein,fileout,ncofname,loutflow)

use getkind
use constants
use parameters
use config
use lsmask
use types

implicit none

character(*) filein,fileout,ncofname
logical loutflow

integer(itype), dimension(:,:), allocatable :: imask,ioutflow
real(rtype), dimension(:,:), allocatable :: aoutflow
real(rtype) rmdi_nc
integer(ptype) ochan
integer nfield,datasize,nftype,inthd8,datatype,ztype
integer(otype) pp_pos,data_pos
integer fieldcode,proccode,levcode,stashcode,fftype,fflev,istart
integer intunit,interval,idate(6)
logical lperiodic,ltheta
integer ncid,ierr,dim(4),nx,ny,nz,nt,iz,it(1),model
character(max_varname_size) dimnames(4)
logical lintvar,isncvarint,isncmdiatt
type(gridinfo) grid

write(*,*)'Writing Land/Sea Mask ancillary file ',trim(fileout)

! Open ancil file

call open_ancil(ochan,fileout,'w')

! write fixed ancillary file headers

nx = mask_grid%nlong
ny = mask_grid%nlat
ztype = 1
inthd8 = -1
if (loutflow) then
   nfield = 2 ! number of fields in ancil file
   nftype = 2 ! number of field types in ancil file
   datasize = 2*nx*ny
else
   nfield = 1 ! number of fields in ancil file
   nftype = 1 ! number of field types in ancil file
   datasize = nx*ny
endif
intunit = 0
interval = 0
lperiodic = .false.
idate = 0

call write_head(ochan,pp_pos,data_pos,nfield,datasize,nftype,inthd8, &
                intunit,interval,lperiodic,ztype,mask_grid,idate,1)

! pp header values for all fields

proccode = 0
levcode = 129
fflev = 8888
ltheta = .false.

! Calculate land/sea mask

allocate(imask(nx,ny))

where (mask) 
   imask = 1
elsewhere
   imask = 0
endwhere

! Write land/sea mask pp headers

fieldcode = 38
stashcode = 30
fftype = 74
istart = 1
datatype = 3

call write_pphead(ochan,pp_pos,idate,0,ztype,ltheta,fieldcode,proccode, &
                  levcode,fftype,fflev,data_pos,istart,datatype, &
                  stashcode,mask_grid)

! Write land/sea mask data

call write_data_i(ochan,data_pos,imask,nx,ny)

deallocate(imask)

if (loutflow) then

   model = 1 ! atmos model ancillary file

!  Open netcdf file

   call open_ncfile(filein, 'r', ncid, ierr)

!  Get ancil dimension values

   call get_gridinfo(ncofname,ncid,dimnames,dim,nz,nt,model,grid)
   nx = grid%nlong
   ny = grid%nlat

   call chk_mask_grid(nx,ny)


!  Get datatype

   lintvar = isncvarint(ncofname,ncid)

!  Get river outflow values

   allocate(ioutflow(nx,ny))

   iz = 1
   it(1) = 1
   if (lintvar) then
      call get_ncdata_i(ncofname,ncid,iz,it,grid,dim,nx,ny,1,1,ioutflow)
      where (.not. mask) ioutflow = imdi
   else
      allocate(aoutflow(nx,ny))
      call get_ncdata_r(ncofname,ncid,iz,it,grid,dim,nx,ny,1,1,aoutflow)
      if (isncmdiatt(ncofname,ncid)) then
         call get_ncmdi_r(ncofname,ncid,rmdi_nc)
         where (mask .and. aoutflow /= rmdi_nc)
            ioutflow = aoutflow
         elsewhere
            ioutflow = imdi
         endwhere
      else
         where (mask)
            ioutflow = aoutflow
         elsewhere
            ioutflow = imdi
         endwhere
      endif
      deallocate(aoutflow)
   endif

!  Close netcdf file

   call close_ncfile(ncid, ierr)

!  Call subroutine for any user modifications

   call outflow_usermod(ioutflow,grid%along,grid%alat,nx,ny)

!  Write river outflow pp headers

   fieldcode = 700
   stashcode = 93
   fftype = 0
   istart = 1 + nx*ny
   datatype = 2

   call write_pphead(ochan,pp_pos,idate,0,ztype,ltheta,fieldcode,proccode, &
                     levcode,fftype,fflev,data_pos,istart,datatype, &
                     stashcode,grid)

!  Write river outflow data

   call write_data_i (ochan,data_pos,ioutflow,nx,ny)

   call free_gridinfo(grid)
   deallocate(ioutflow)
endif
! Close ancil file

call close_ancil(ochan)

return
end

!==============================================================================!

subroutine chk_mask_grid(nx,ny)

use getkind
use lsmask

implicit none

integer nx,ny

integer ierr

ierr = 0

! Make sure NetCDF mask has been selected

if (.not. lncmask) then
   write(*,*)'ERROR: NetCDF Land/Sea mask required but has not been selected'
   stop
endif

if (nx /= mask_grid%nlong) then
   write(*,*)'ERROR: Land/Sea mask longitude dimension incompatible with ancillary file'
   ierr = 1
endif

if (ny /= mask_grid%nlat) then
   write(*,*)'ERROR: Land/Sea mask latitude dimension incompatible with ancillary file'
   ierr = 1
endif

if (ierr /= 0) stop

return
end
