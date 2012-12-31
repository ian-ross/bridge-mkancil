
!==============================================================================!

subroutine create_sstice_ancil(lsst,sst_filein,sst_fileout,sst_ncname, &
                               lsst_min,asst_min,lsst_iceval,asst_iceval, &
                               lsst_periodic,isst_mask, &
                               isst_timeusage1,isst_timeusage2, &
                               isst_startdate,isst_ntimes, &
                               isst_interval,isst_intunit,lsst_mm, &
                               lice,ice_filein,ice_fileout, &
                               ice_ncfracname,ice_ncdepthname,ice_ncedgename, &
                               lice_amip2,lice_calcdepth,lice_calcedge, &
                               lice_sstval,aice_sstval, &
                               lice_percent,lice_mkmask,aice_cutoff,&
                               lice_min,aice_min,lice_max,aice_max, &
                               lice_periodic,iice_mask, &
                               iice_timeusage1,iice_timeusage2, &
                               iice_startdate,iice_ntimes, &
                               iice_interval,iice_intunit,lice_mm)

use getkind
use constants
use parameters
use config
use lsmask
use lslfrac
use types

implicit none

real(rtype) asst_min,asst_iceval
character(*) sst_filein,sst_fileout,sst_ncname
logical lsst,lsst_min,lsst_iceval,lsst_periodic,lsst_mm
integer isst_mask,isst_timeusage1,isst_timeusage2,isst_startdate(6)
integer isst_ntimes,isst_interval,isst_intunit

real(rtype) aice_sstval,aice_cutoff,aice_min,aice_max
character(*) ice_filein,ice_fileout
character(*) ice_ncfracname,ice_ncdepthname,ice_ncedgename
logical lice,lice_amip2,lice_sstval,lice_calcdepth,lice_calcedge
logical lice_percent,lice_mkmask,lice_min,lice_max,lice_periodic,lice_mm
integer iice_mask,iice_timeusage1,iice_timeusage2
integer iice_startdate(6),iice_ntimes,iice_interval,iice_intunit

real(rtype), dimension(:,:), allocatable :: sst
real(rtype), dimension(:,:,:), pointer :: icefrac2
real(rtype), dimension(:,:), pointer :: icefrac,icefrac_prev,icefrac_save
real(rtype), dimension(:,:), allocatable :: icedepth,iceedge
real(rtype), dimension(:), allocatable :: time
real(rtype) aice_sstval1
real(rtype) asst_rmdi_nc,aicefrac_rmdi_nc,aicedepth_rmdi_nc,aiceedge_rmdi_nc
integer(ptype) isst_ochan,iice_ochan
integer itimeusage1,itimeusage2,istartdate1(6),ntimes,interval,intunit1
integer ncid, isst_ncid,iice_ncid,ierr,ix,iy,iz,it,intunit2,istartdate2(6)
integer iscale,model,type,nfield,datasize,nrec
integer nftype,inthd8,datatype,ztype,iloc(2)
integer(otype) isst_pp_pos,isst_data_pos
integer(otype) iice_pp_pos,iice_data_pos,iiceedge_data_pos
integer isst_dim(4),iice_dim(4),dim_t,nx,ny,nz,nt1,nt2,nt3,ntif
integer, dimension(:,:), allocatable :: idate
integer, dimension(:), allocatable :: itsel
integer isst_fieldcode,isst_stashcode,isst_fftype,isst_start,iice_start
integer iicefrac_fieldcode,iicefrac_stashcode,iicefrac_fftype
integer iicedepth_fieldcode,iicedepth_stashcode,iicedepth_fftype
integer iiceedge_fieldcode,iiceedge_stashcode,iiceedge_fftype,iiceedge_data_size
integer proccode,levcode,fflev,ihem
logical ldegc,ltheta,lmm,lice_ncfile
type(gridinfo) sst_grid,ice_grid
character(max_filename_size) filein
character(max_varname_size) sst_dimnames(4),ice_dimnames(4),dimnames_t
character(max_varname_size) ice_ncname

if (lsst) &
   write(*,*)'Writing Sea Surface Temperature ancillary file ',trim(sst_fileout)
if (lice) &
   write(*,*)'Writing Sea Ice ancillary file ',trim(ice_fileout)

model = 1 ! atmos model ancillary file
type = 1  ! data on atmospheric theta points

! Does an ice netcdf file need to be read in?

lice_ncfile = (lice_amip2 .and. .not. lice_sstval) .or. &
              .not. lice_amip2 .and. &
              .not. (lice_sstval .and. lice_calcdepth .and. lice_calcedge)

! Open netcdf files

if (lsst) then
   call open_ncfile(sst_filein, 'r', isst_ncid, ierr)
endif
if (lice .and. lice_ncfile) then
   call open_ncfile(ice_filein, 'r', iice_ncid, ierr)
endif

! Get ancil dimension values

if (lsst) then
   call get_gridinfo(sst_ncname,isst_ncid,sst_dimnames,isst_dim, &
                     nz,nt1,model,type,sst_grid)
   nx = sst_grid%nlong
   ny = sst_grid%nlat
endif
if (lice .and. lice_ncfile) then
   if (.not. lice_sstval) then
      ice_ncname = ice_ncfracname
   elseif (.not. lice_calcdepth) then
      ice_ncname = ice_ncdepthname
   elseif (.not. lice_calcedge) then
      ice_ncname = ice_ncedgename
   endif

   call get_gridinfo(ice_ncname,iice_ncid,ice_dimnames,iice_dim, &
                     nz,nt1,model,type,ice_grid)
   nx = ice_grid%nlong
   ny = ice_grid%nlat
else if (lice) then
   call copy_gridinfo(sst_grid,ice_grid)
   nx = ice_grid%nlong
   ny = ice_grid%nlat
endif

! Check consistency with mask

if (lsst) then
   if (isst_mask == 1) call chk_mask_grid(nx,ny)
   if (isst_mask == 2) call chk_lfrac_grid(nx,ny)
elseif (lice) then
   if (iice_mask == 1) call chk_mask_grid(nx,ny)
   if (iice_mask == 2) call chk_lfrac_grid(nx,ny)
endif

! Get time info from either sst or ice netcdf file

if (lsst) then
   filein = sst_filein
   ncid = isst_ncid
   dimnames_t = sst_dimnames(4)
   dim_t = isst_dim(4)
   itimeusage1 = isst_timeusage1
   itimeusage2 = isst_timeusage2
   istartdate1 = isst_startdate
   ntimes = isst_ntimes
   interval = isst_interval
   intunit1 = isst_intunit
   lmm = lsst_mm
else
   filein = ice_filein
   ncid = iice_ncid
   dimnames_t = ice_dimnames(4)
   dim_t = iice_dim(4)
   itimeusage1 = iice_timeusage1
   itimeusage2 = iice_timeusage2
   istartdate1 = iice_startdate
   ntimes = iice_ntimes
   interval = iice_interval
   intunit1 = iice_intunit
   lmm = lice_mm
endif

! Check time selection consistency

if (dim_t < 1) then
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

if (dim_t > 0) then
   call get_ncdim(dimnames_t,ncid,time)
!   write(*,*)'time = ',time
else
   time = 0.0_rtype
endif

if (itimeusage1 == 0 .or. (itimeusage1 == 1 .and. itimeusage2 == 1)) &
   call get_ncstartdate(dimnames_t,ncid,istartdate2,intunit2,iscale)

call setdates(itimeusage1,itimeusage2,lmm,interval,intunit1,intunit2,ical, &
              iscale,ntimes,istartdate1,istartdate2,time,nt1,itsel,idate,nt2)

!do it=1,nt2
!   write(*,*)'date = ',idate(1,it),idate(2,it),idate(3,it), &
!                       idate(4,it),idate(5,it),idate(6,it),itsel(it)
!enddo

if (itimeusage1 == 0) intunit1 = intunit2

! Common header values for all fields

proccode = 128
levcode = 129
fflev = 8888
datatype = 1
inthd8 = -1
ztype = 1
ltheta = .false.
iz = 1
nt3 = nt2

if (lsst) then

   allocate(sst(nx,ny))

!  Get mask values

   if (isst_mask == 0) then
      call get_ncmdi_r(sst_ncname,isst_ncid,asst_rmdi_nc)
!      write(*,*)'asst_rmdi_nc = ',asst_rmdi_nc
   endif

!  Open ancil file

   call open_ancil(isst_ochan,sst_fileout,'w')

!  Write fixed ancillary file headers

   nftype = 1
   nfield = nt2 ! number of fields in ancil file
   datasize = nfield*nx*ny
   call write_head(isst_ochan,isst_pp_pos,isst_data_pos, &
                   nfield,datasize,nftype,inthd8, &
                   intunit1,interval,lsst_periodic,ztype,sst_grid,idate,nt2)

!  SST pp header values for all fields

   isst_start = 1
   isst_fieldcode = 16
   isst_fftype = 91
   isst_stashcode = 24
endif

if (lice) then

   if (.not. lice_amip2 .and. lice_calcedge) then
      ntif = 2
      nt3 = nt2+1
      itsel(nt3) = itsel(1)
      idate(:,nt3) = idate(:,1)
      if (l32bit) then
         iiceedge_data_size = (nx*ny+1)/2
      else
         iiceedge_data_size = nx*ny
      endif
      if (lwfio .and. iwfio_size > 1) then
         nrec = iiceedge_data_size / iwfio_size
         if (mod(iiceedge_data_size,iwfio_size) /= 0) nrec = nrec + 1
         iiceedge_data_size = nrec*iwfio_size
      endif
   else
      ntif = 1
   endif

   allocate(icefrac2(nx,ny,ntif))
   if (.not. lice_amip2) then
      allocate(icedepth(nx,ny))
      allocate(iceedge(nx,ny))
   endif

   icefrac_prev => icefrac2(:,:,1)
   icefrac => icefrac2(:,:,ntif)

!  Get mask values

   if (iice_mask == 0) then
      if (.not. lice_sstval) then
         call get_ncmdi_r(ice_ncfracname,iice_ncid,aicefrac_rmdi_nc)
      endif
      if (.not. lice_amip2 .and. .not. lice_calcdepth) then
         call get_ncmdi_r(ice_ncdepthname,iice_ncid,aicedepth_rmdi_nc)
      endif
   endif
   if (.not. lice_amip2 .and. .not. lice_calcedge) then
      call get_ncmdi_r(ice_ncedgename,iice_ncid,aiceedge_rmdi_nc)
   endif

!  Open ancil file

   call open_ancil(iice_ochan,ice_fileout,'w')

!  Write fixed ancillary file headers

   if (lice_amip2) then
      nftype = 1
   else
      nftype = 3
   endif
   nfield = nftype*nt2 ! number of fields in ancil file
   datasize = nfield*nx*ny
   call write_head(iice_ochan,iice_pp_pos,iice_data_pos, &
                   nfield,datasize,nftype,inthd8, &
                   intunit1,interval,lice_periodic,ztype,ice_grid,idate,nt2)

!  Sea-Ice pp header values for all fields

   iice_start = 1
   iicefrac_fieldcode = 37
   iicefrac_fftype = 134
   iicefrac_stashcode = 31
   if (.not. lice_amip2) then
      iiceedge_fieldcode = 37
      iiceedge_fftype = 0
      iiceedge_stashcode = 38
      iicedepth_fieldcode = 92
      iicedepth_fftype = 153
      iicedepth_stashcode = 32
   endif

endif

do it=1,nt3

   if (lsst) then

!     Get SST data values

      call get_ncdata_r(sst_ncname,isst_ncid,iz,itsel(it),sst_grid,isst_dim, &
                        nx,ny,1,1,sst)

!     Apply land/sea mask

      if (isst_mask == 0 .and. rmdi /= asst_rmdi_nc) then
         where (sst == asst_rmdi_nc) sst = rmdi
      else if (isst_mask == 1) then
         where (mask) sst = rmdi
      else if (isst_mask == 2) then
         where (alfrac >= 1.0_rtype) sst = rmdi
      endif

!     Get value to determine SST units

      if (it == 1) then
         y: do iy=1,ny
            x: do ix=1,nx
               if (sst(ix,iy) /= rmdi) then
                  ldegc = sst(ix,iy) <  150.0_rtype .and. &
                          sst(ix,iy) > -150.0_rtype
                  exit y
               endif
            enddo x
         enddo y
      endif

!     Convert degrees Celsius to kelvin

      if (ldegc) then
         where (sst /= rmdi) sst = sst + 273.15
      endif

!     Call subroutine for any user modifications

      call sst_usermod(sst,sst_grid%along,sst_grid%alat,nx,ny,it,idate(1,it))

   endif

   if (lice) then

!     Get Sea-Ice fraction data values

      if (lice_sstval) then
         iloc = minloc(abs(sst-aice_sstval), mask=(sst /= rmdi))
         aice_sstval1 = sst(iloc(1),iloc(2))

         if (abs(aice_sstval1-aice_sstval) > 0.001) then
            write(*,999)idate(1,it),idate(2,it),idate(3,it), &
                        idate(4,it),idate(5,it),idate(6,it)
 999        format(' Date = ',i4.4,'/',i2.2,'/',i2.2,':',i2.2,'.',i2.2,'.',i2.2)
            write(*,*)'No SST value matches ice SST value, closest match to ', &
                       aice_sstval,' is ',aice_sstval1
            icefrac = 0.0
         else
            where (sst == aice_sstval1)
               icefrac = 1.0
            elsewhere
               icefrac = 0.0
            endwhere
         endif
      else
         call get_ncdata_r(ice_ncfracname,iice_ncid,iz,itsel(it),ice_grid, &
                           iice_dim,nx,ny,1,1,icefrac)
      endif

!     Apply land/sea mask

      if (iice_mask == 0) then
         if (lice_sstval) then
            where (sst == rmdi) icefrac = rmdi
         else if (rmdi /= aicefrac_rmdi_nc) then
            where (icefrac == aicefrac_rmdi_nc) icefrac = rmdi
         endif
      else if (iice_mask == 1) then
         where (mask) icefrac = rmdi
      else if (iice_mask == 2) then
         where (alfrac >= 1.0_rtype) icefrac = rmdi
      endif

!     Convert from percentage to fraction

      if (lice_percent .and. .not. lice_sstval) then
         where (icefrac /= rmdi) icefrac = icefrac*0.01_rtype
      endif

!     Convert Sea-Ice fraction into a mask

      if (lice_mkmask .and. .not. lice_sstval) then
         do iy=1,ny
            do ix=1,nx
               if (icefrac(ix,iy) /= rmdi) then
                  if (icefrac(ix,iy) >= aice_cutoff) then
                     icefrac(ix,iy) = 1.0_rtype
                  else
                     icefrac(ix,iy) = 0.0_rtype
                  endif
               endif
            enddo
         enddo
      endif

!     Set maximum and minimum values for Sea-ice

      if (lice_max) then
         where (icefrac > aice_max .and. icefrac /= rmdi) icefrac = aice_max
      endif
      if (lice_min) then
         where (icefrac < aice_min .and. icefrac /= rmdi) icefrac = 0.0_rtype
      endif

!     Call subroutine for any user modifications

      call ssticefrac_usermod(sst,icefrac,ice_grid%along,ice_grid%alat, &
                              nx,ny,it,idate(1,it))

      if (.not. lice_amip2 .and. lice_calcedge .and. it /= 1) then

!        Calculate Sea-Ice edge data values

         if (.not. lice_periodic .and. it == nt2+1) then
            iceedge = rmdi
         else
            call getfractime(ice_grid,icefrac_prev,icefrac,iceedge,nx,ny)
         endif

      endif

      if (.not. lice_amip2 .and. it /= nt2+1) then

!        Read Sea-Ice edge data values

         if (.not. lice_calcedge) then
            call get_ncdata_r(ice_ncedgename,iice_ncid,iz,itsel(it),ice_grid, &
                              iice_dim,nx,ny,1,1,iceedge)

            if (rmdi /= aiceedge_rmdi_nc) then
               where (iceedge == aiceedge_rmdi_nc) iceedge = rmdi
            endif
         endif

!        Get Sea-Ice depth data values

         if (lice_calcdepth) then
            do iy=1,ny
               do ix=1,nx
                  if (icefrac(ix,iy) > 0) &
                     call gethemisphere(ice_grid,ix,iy,ihem)
                  if (icefrac(ix,iy) == rmdi) then
                     icedepth(ix,iy) = rmdi
                  elseif (icefrac(ix,iy) > 0 .and. ihem == 1) then
                     icedepth(ix,iy) = 2.0_rtype
                  elseif (icefrac(ix,iy) > 0) then
                     icedepth(ix,iy) = 1.0_rtype
                  else
                     icedepth(ix,iy) = 0.0_rtype
                  endif
               enddo
            enddo
         else
            call get_ncdata_r(ice_ncdepthname,iice_ncid,iz,itsel(it),ice_grid, &
                              iice_dim,nx,ny,1,1,icedepth)

!           Apply land/sea mask

            if (iice_mask == 0) then
               if (rmdi /= aicedepth_rmdi_nc) then
                  where (icedepth == aicedepth_rmdi_nc) icedepth = rmdi
               endif
            else if (iice_mask == 1) then
               where (mask) icedepth = rmdi
            else if (iice_mask == 2) then
               where (alfrac >= 1.0_rtype) icedepth = rmdi
            endif
         endif

!        Call subroutine for any user modifications

         call icedepth_usermod(icedepth,ice_grid%along,ice_grid%alat, &
                               nx,ny,it,idate(1,it))

      endif
   endif


   if (lsst .and. it /= nt2+1) then

!     Set minimum SST value

      if (lsst_min) then
         where (sst /= rmdi .and. sst < asst_min) sst = asst_min
      endif

!     Set SST value over Sea-Ice

      if (lsst_iceval) then
         where (sst /= rmdi .and. icefrac /= rmdi .and. icefrac > 0) &
            sst = asst_iceval
      endif

!     Write SST pp header

      call write_pphead(isst_ochan,isst_pp_pos,idate(1,it),0,ztype,ltheta, &
                        isst_fieldcode,proccode,levcode,isst_fftype,fflev, &
                        isst_data_pos,isst_start,datatype,isst_stashcode, &
                        sst_grid)

!     Write SST data

      call write_data_r(isst_ochan,isst_data_pos,sst,nx,ny)

      isst_start = isst_start + nx*ny
   endif

   if (lice .and. it /= nt2+1) then

!     Write Sea-Ice fraction pp header

      call write_pphead(iice_ochan,iice_pp_pos,idate(1,it),0,ztype,ltheta, &
                        iicefrac_fieldcode,proccode,levcode, &
                        iicefrac_fftype,fflev,iice_data_pos,iice_start, &
                        datatype,iicefrac_stashcode,ice_grid)

!     Write Sea-Ice fraction data

      call write_data_r(iice_ochan,iice_data_pos,icefrac,nx,ny)

      iice_start = iice_start + nx*ny

      if (.not. lice_amip2) then

!        Write Sea-Ice edge pp header

         call write_pphead(iice_ochan,iice_pp_pos,idate(1,it),0,ztype,ltheta, &
                           iiceedge_fieldcode,proccode,levcode, &
                           iiceedge_fftype,fflev,iice_data_pos,iice_start, &
                           datatype,iiceedge_stashcode,ice_grid)

!        Write Sea-Ice edge data

         if (lice_calcedge) then
            if (it > 1) then
               call write_data_r(iice_ochan,iiceedge_data_pos,iceedge,nx,ny)
            endif
            iiceedge_data_pos = iice_data_pos
            iice_data_pos = iice_data_pos + iiceedge_data_size
         else
            call write_data_r(iice_ochan,iice_data_pos,iceedge,nx,ny)
         endif
         iice_start = iice_start + nx*ny

!        Write Sea-Ice depth pp header

         call write_pphead(iice_ochan,iice_pp_pos,idate(1,it),0,ztype,ltheta, &
                           iicedepth_fieldcode,proccode,levcode, &
                           iicedepth_fftype,fflev,iice_data_pos,iice_start, &
                           datatype,iicedepth_stashcode,ice_grid)

!        Write Sea-Ice depth data

         call write_data_r(iice_ochan,iice_data_pos,icedepth,nx,ny)

         iice_start = iice_start + nx*ny

      endif
   elseif (lice .and. .not. lice_amip2 .and. lice_calcedge .and. &
           it == nt2+1) then
      call write_data_r(iice_ochan,iiceedge_data_pos,iceedge,nx,ny)
   endif

   if (lice .and. .not. lice_amip2 .and. lice_calcedge) then
      icefrac_save => icefrac_prev
      icefrac_prev => icefrac
      icefrac => icefrac_save
   endif

enddo

! Close netcdf files

if (lsst) then
   call close_ncfile(isst_ncid, ierr)
endif
if (lice .and. lice_ncfile) then
   call close_ncfile(iice_ncid, ierr)
endif

! Close ancil file

if (lsst) call close_ancil(isst_ochan)
if (lice) call close_ancil(iice_ochan)

if (lsst) call free_gridinfo(sst_grid)
if (lice .and. lice_ncfile) call free_gridinfo(ice_grid)
deallocate(time)
deallocate(idate)
deallocate(itsel)

if (lsst) deallocate(sst)
if (lice) then
   deallocate(icefrac2)
   if (.not. lice_amip2) then
      deallocate(icedepth)
      deallocate(iceedge)
   endif
endif

return
end

