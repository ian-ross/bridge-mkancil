
!==============================================================================!

subroutine create_ts1_ancil(filein,fileout, &
                            ncsstname,ncsssname,ncatname,ncidname, &
                            lrefsst,lrefsss,lclimat,lclimid, & 
                            lperiodic,imask, &
                            itimeusage1,itimeusage2,istartdate1, &
                            ntimes,interval,intunit1,lmm)

use getkind
use constants
use parameters
use config
use types

implicit none

character(*) filein,fileout,ncsstname,ncsssname,ncatname,ncidname
logical lrefsst,lrefsss,lclimat,lclimid,lperiodic,lmm
integer imask,itimeusage1,itimeusage2,istartdate1(6)
integer ntimes,interval,intunit1

real(rtype), dimension(:,:), allocatable :: refsst,refsss,climat,climid
real(rtype), dimension(:), allocatable :: time
real(rtype) rmdi_nc
integer(ptype) ochan
integer ncid,ierr,ix,iy,iz,it,intunit2,istartdate2(6)
integer iscale,model,type,nfield,datasize,nftype,inthd8,datatype,ztype
integer(otype) pp_pos,data_pos
integer dim(4),nx,ny,nz,nt1,nt2
integer, dimension(:,:), allocatable :: idate
integer, dimension(:), allocatable :: itsel
integer fieldcode,proccode,levcode,stashcode,fftype,fflev,istart
logical ldegc,ltheta
type(gridinfo) grid
character(max_varname_size) dimnames(4)
character(max_varname_size) ncname

write(*,*)'Writing Ocean Reference data ancillary file ',trim(fileout)

model = 2 ! ocean model ancillary file
type = 1  ! data on ocean mass points

if (lrefsst) then
   ncname = ncsstname
elseif (lrefsss) then
   ncname = ncsssname
elseif (lclimat) then
   ncname = ncatname
elseif (lclimid) then
   ncname = ncidname
else
   write(*,*)'ERROR: no ocean reference data fields requested'
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

if (lrefsst) allocate(refsst(nx,ny))
if (lrefsss) allocate(refsss(nx,ny))
if (lclimat) allocate(climat(nx,ny))
if (lclimid) allocate(climid(nx,ny))

! Open ancil file

call open_ancil(ochan,fileout,'w')

! write fixed ancillary file headers

ztype = 1
nftype = 0
inthd8 = -1
if (lrefsst) nftype = nftype + 1
if (lrefsss) nftype = nftype + 1
if (lclimat) nftype = nftype + 1
if (lclimid) nftype = nftype + 1
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

   if (lrefsst) then

!     Get Reference SST data values

      call get_ncdata_r(ncsstname,ncid,iz,itsel(it),grid,dim, &
                        nx,ny,1,1,refsst)

!     Apply land/sea mask

      if (imask == 0) then
         where (refsst == rmdi_nc) refsst = rmdi
      endif

!     Get value to determine SST units

      if (it == 1) then
         y: do iy=1,ny
            x: do ix=1,nx
               if (refsst(ix,iy) /= rmdi) then
                  ldegc = refsst(ix,iy) < 150.0_rtype
                  exit y
               endif
            enddo x
         enddo y
      endif

!     Convert kelvin to degrees Celsius

      if (.not. ldegc) then
         where (refsst /= rmdi) refsst = refsst - 273.15
      endif

   endif

   if (lrefsss) then

!     Get Reference SSS data values

      call get_ncdata_r(ncsssname,ncid,iz,itsel(it),grid,dim, &
                        nx,ny,1,1,refsss)

!     Apply land/sea mask

      if (imask == 0) then
         where (refsss == rmdi_nc) refsss = rmdi
      endif

   endif

   if (lclimat) then

!     Get Climatological Air Temperature data values

      call get_ncdata_r(ncatname,ncid,iz,itsel(it),grid,dim, &
                        nx,ny,1,1,climat)

!     Apply land/sea mask

      if (imask == 0) then
         where (climat == rmdi_nc) climat = rmdi
      endif

   endif

   if (lclimid) then

!     Get Climatological Ice Depth data values

      call get_ncdata_r(ncidname,ncid,iz,itsel(it),grid,dim, &
                        nx,ny,1,1,climid)

!     Apply land/sea mask

      if (imask == 0) then
         where (climid == rmdi_nc) climid = rmdi
      endif

   endif

!  Call subroutine for any user modifications

   call ts1_usermod(refsst,refsss,climat,climid,grid%along,grid%alat, &
                    nx,ny,it,idate(1,it))

   if (lrefsst) then

!     Write Reference SST pp headers

      fieldcode = 650
      stashcode = 180

      call write_pphead(ochan,pp_pos,idate(1,it),0,ztype,ltheta, &
                        fieldcode,proccode,levcode,fftype,fflev, &
                        data_pos,istart,datatype,stashcode,grid)

!     Write Reference SST data

      call write_data_r(ochan,data_pos,refsst,nx,ny)

      istart = istart + nx*ny

   endif

   if (lrefsss) then

!     Write Reference SSS pp headers

      fieldcode = 649
      stashcode = 181

      call write_pphead(ochan,pp_pos,idate(1,it),0,ztype,ltheta, &
                        fieldcode,proccode,levcode,fftype,fflev, &
                        data_pos,istart,datatype,stashcode,grid)

!     Write Reference SSS data

      call write_data_r(ochan,data_pos,refsss,nx,ny)

      istart = istart + nx*ny

   endif

   if (lclimat) then

!     Write Climatological Air Temperature pp headers

      fieldcode = 0
      stashcode = 182

      call write_pphead(ochan,pp_pos,idate(1,it),0,ztype,ltheta, &
                        fieldcode,proccode,levcode,fftype,fflev, &
                        data_pos,istart,datatype,stashcode,grid)

!     Write Climatological Air Temperature data

      call write_data_r(ochan,data_pos,climat,nx,ny)

      istart = istart + nx*ny

   endif

   if (lclimid) then

!     Write Climatological Ice Depth pp headers

      fieldcode = 675
      stashcode = 183

      call write_pphead(ochan,pp_pos,idate(1,it),0,ztype,ltheta, &
                        fieldcode,proccode,levcode,fftype,fflev, &
                        data_pos,istart,datatype,stashcode,grid)

!     Write Climatological Ice Depth data

      call write_data_r(ochan,data_pos,climid,nx,ny)

      istart = istart + nx*ny

   endif

enddo

! Close netcdf file

call close_ncfile(ncid, ierr)

! Close ancil file

call close_ancil(ochan)

if (lrefsst) deallocate(refsst)
if (lrefsss) deallocate(refsss)
if (lclimat) deallocate(climat)
if (lclimid) deallocate(climid)

return
end
