
!==============================================================================!

subroutine get_lfrac(filein,ncname)

use getkind
use parameters
use config
use lslfrac
use types

implicit none

character(*) filein,ncname

integer ierr,ncid,iz,it(1)
integer dim(4),nx,ny,nz,nt,model,type
character(max_varname_size) dimnames(4)

model = 1 ! atmos model ancillary file
type = 1  ! data on atmospheric theta points

! Open netcdf file

call open_ncfile(filein, 'r', ncid, ierr)

! Get ancil dimension values

call get_gridinfo(ncname,ncid,dimnames,dim,nz,nt,model,type,lfrac_grid)
nx = lfrac_grid%nlong
ny = lfrac_grid%nlat

! Get lfrac values

allocate(alfrac(nx,ny))

iz = 1
it(1) = 1
call get_ncdata_r(ncname,ncid,iz,it,lfrac_grid,dim,nx,ny,1,1,alfrac)

! Close netcdf file

call close_ncfile(ncid, ierr)

! Call subroutine for any user modifications

call lfrac_usermod(alfrac,lfrac_grid%along,lfrac_grid%alat,nx,ny)

return
end

!==============================================================================!

subroutine create_lfrac_ancil(fileout)

use getkind
use constants
use parameters
use config
use lslfrac
use types

implicit none

character(*) fileout

integer(ptype) ochan
integer nfield,datasize,nftype,inthd8,datatype,ztype
integer(otype) pp_pos,data_pos
integer fieldcode,proccode,levcode,stashcode,fftype,fflev,istart
integer nx,ny,intunit,interval,idate(6)
logical lperiodic,ltheta

write(*,*)'Writing Land Fraction ancillary file ',trim(fileout)

! Open ancil file

call open_ancil(ochan,fileout,'w')

! write fixed ancillary file headers

nx = lfrac_grid%nlong
ny = lfrac_grid%nlat
nfield = 1 ! number of fields in ancil file
nftype = 1 ! number of field types in ancil file
inthd8 = -1
ztype = 1
datasize = nx*ny
intunit = 0
interval = 0
lperiodic = .false.
idate = 0

call write_head(ochan,pp_pos,data_pos,nfield,datasize,nftype,inthd8, &
                intunit,interval,lperiodic,ztype,lfrac_grid,idate,1)

! Write land fraction pp headers

proccode = 0
levcode = 129
fflev = 1
ltheta = .false.
if (iversion < 500) then
   fieldcode = 501
else
   fieldcode = 395
endif
stashcode = 505
fftype = 0
istart = 1
datatype = 1

call write_pphead(ochan,pp_pos,idate,0,ztype,ltheta,fieldcode,proccode, &
                  levcode,fftype,fflev,data_pos,istart,datatype, &
                  stashcode,lfrac_grid)

! Write land/sea lfrac data

call write_data_r(ochan,data_pos,alfrac,nx,ny)

! Close ancil file

call close_ancil(ochan)

return
end

!==============================================================================!

subroutine chk_lfrac_grid(nx,ny)

use getkind
use lslfrac

implicit none

integer nx,ny

integer ierr

ierr = 0

! Make sure NetCDF Land Fraction has been selected

if (.not. lnclfrac) then
   write(*,*)'ERROR: NetCDF Land Fraction required but has not been selected'
   stop
endif

if (nx /= lfrac_grid%nlong) then
   write(*,*)'ERROR: Land Fraction longitude dimension incompatible with ancillary file'
   ierr = 1
endif

if (ny /= lfrac_grid%nlat) then
   write(*,*)'ERROR: Land Fraction latitude dimension incompatible with ancillary file'
   ierr = 1
endif

if (ierr /= 0) stop

return
end

!==============================================================================!

subroutine chk_lfrac_mask()

use getkind
use lslfrac
use lsmask

implicit none

integer ierr,ix,iy,nx_mask,ny_mask,nx_lfrac,ny_lfrac
logical lopen
character(56) errormess

ierr = 0
lopen = .false.

write(*,*)'Checking consistency of Land/Sea Mask and Land Fraction fields ...'

nx_mask = mask_grid%nlong
ny_mask = mask_grid%nlat
nx_lfrac = lfrac_grid%nlong
ny_lfrac = lfrac_grid%nlat

if (.not. lncmask) then
   write(*,*)'Land/Sea Mask field not selected'
   ierr = 1
endif
if (.not. lnclfrac) then
   write(*,*)'Land Fraction field not selected'
   ierr = 1
endif

if (ierr /= 0) return

if (nx_mask /= nx_lfrac) then
   write(*,*)'Number of longitude points different ',nx_mask,nx_lfrac
   ierr = 1
endif
if (ny_mask /= ny_lfrac) then
   write(*,*)'Number of latitude points different ',ny_mask,ny_lfrac
   ierr = 1
endif

if (ierr /= 0) return

! mask should be land whenever alfrac > 0

do iy=1,ny_mask
   do ix=1,nx_mask

!     Error if land point on mask but sea only point in land frac file or
!     sea point on mask but not sea only point in land frac file

      if (mask(ix,iy) .and. alfrac(ix,iy) <= 0.0_rtype) ierr = 2
      if ((.not. mask(ix,iy)) .and. alfrac(ix,iy) > 0.0_rtype) ierr = 3

      if (ierr /= 0 .and. .not. lopen) then
         lopen = .true.
         open(21,file='landseamask_err.txt')
      endif
      if (ierr == 2) errormess = &
         'land point in mask, sea only point in land fraction   : '
      if (ierr == 3) errormess = &
         'sea point in mask, not sea only point in land fraction: '
      if (ierr /= 0) &
         write(21,*)errormess,'(',ix,',',iy,') : ', &
                              '(',mask_grid%along(ix),',',mask_grid%alat(iy),') '

      ierr = 0
   enddo
enddo

if (lopen) then
   close(21)
   write(*,*)'Land/Sea Mask and Land Fraction fields are inconsistent'
   write(*,*)'See file landseamask_err.txt for incorrect points'
else
   write(*,*)'Land/Sea Mask and Land Fraction fields are consistent'
endif
write(*,*)

return
end
