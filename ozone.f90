
!==============================================================================!

subroutine create_ozone_ancil(filein,fileout,ncname,amrfac,lmmr,lperiodic, &
                              itimeusage1,itimeusage2,istartdate1, &
                              ntimes,interval,intunit1,lmm)

use getkind
use constants
use parameters
use config
use types
use vert_od
use vert_nd
use vert_ocean

implicit none

character(*) filein,fileout,ncname
real(rtype) amrfac
logical lmmr,lperiodic,lmm
integer itimeusage1,itimeusage2,istartdate1(6)
integer ntimes,interval,intunit1

real(rtype), dimension(:,:), allocatable :: ozone
real(rtype), dimension(:), allocatable :: time
real(rtype) afac
integer(ptype) ochan
integer ncid,ierr,iz,iz0,it,intunit2,istartdate2(6),iscale
integer model,type,nfield,datasize,nftype,inthd8,datatype,ztype
integer(otype) pp_pos,data_pos
integer dim(4),nx,ny,nz,nt1,nt2
integer, dimension(:,:), allocatable :: idate
integer, dimension(:), allocatable :: itsel
integer fieldcode,proccode,levcode,stashcode,fftype,fflev,istart
logical ltheta,lzonal,lfac
type(gridinfo) grid
character(max_varname_size) dimnames(4)

write(*,*)'Writing Ozone ancillary file ',trim(fileout)

model = 1 ! atmos model ancillary file
type = 1  ! data on atmospheric theta points

! Open netcdf file

call open_ncfile(filein, 'r', ncid, ierr)

! Get ancil dimension values

call get_gridinfo(ncname,ncid,dimnames,dim,nz,nt1,model,type,grid)
nx = grid%nlong
ny = grid%nlat

if (iavert == 0) then

   ! Get Atmosphere level values from ozone netcdf file

   call get_nclevels_atmos(filein,dimnames(3),nlev,version,.true.)
   no3lev = nlev
endif

! Check consistency with level and time selection

if (nz < no3lev) then
   write(*,*)'ERROR: number of ozone levels in netcdf file ',nz, &
             ' is less than value specified in xancil ',no3lev
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

lzonal = nx <= 1

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

allocate(ozone(nx,ny))

! Write ancillary file headers

! Open ancil file

call open_ancil(ochan,fileout,'w')

! write fixed ancillary file headers

ztype = 3
nfield = no3lev*nt2 ! number of fields in ancil file
datasize = nfield*nx*ny
nftype = 1
inthd8 = -1
call write_head(ochan,pp_pos,data_pos,nfield,datasize,nftype,inthd8, &
                intunit1,interval,lperiodic,ztype,grid,idate,nt2)

! pp header values for all fields

fieldcode = 453
proccode = 128
if (lzonal) proccode = proccode + 64
if (iversion > 502) then
   levcode = 65
else
   levcode = 9
endif
fftype = 0
stashcode = 60
istart = 1
datatype = 1
ltheta = .true.

! Work out any Ozone unit conversion factor

lfac = .not. lmmr .or. amrfac /= 1.0_rtype
if (lfac) then
   if (lmmr) then
      afac = 1.0_rtype/amrfac
   else
      afac = vol2mass/amrfac
   endif
endif

do it=1,nt2
   do iz=nlev-no3lev+1,nlev
      if ((lvertrev_od .and. iversion < 500) .or. &
          (lvertrev_nd .and. iversion >= 500)) then
         iz0 = -iz+nlev-no3lev+1+nz
      else
         iz0 = iz-nlev+nz
      endif

!     Get Ozone data values

      call get_ncdata_r(ncname,ncid,iz0,itsel(it),grid,dim, &
                        nx,ny,1,1,ozone)

      if (lfac) ozone = ozone*afac

!     Call subroutine for any user modifications

      call ozone_usermod(ozone,grid%along,grid%alat,nx,ny,iz,it,idate(1,it))

!     Write Ozone pp headers

      fflev = iz
      call write_pphead(ochan,pp_pos,idate(1,it),iz,ztype,ltheta, &
                        fieldcode,proccode,levcode,fftype,fflev, &
                        data_pos,istart,datatype,stashcode,grid)

!     Write Ozone data

      call write_data_r(ochan,data_pos,ozone,nx,ny)

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
deallocate(ozone)

return
end

