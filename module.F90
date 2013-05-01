
!==============================================================================!

module getkind

implicit none

integer, parameter :: r32 = selected_real_kind(6)
integer, parameter :: r64 = selected_real_kind(15)

integer, parameter :: i32 = selected_int_kind(9)
integer, parameter :: i64 = selected_int_kind(18)

#if RTYPE == 64
integer, parameter :: rtype = r64
#elif RTYPE == 32
integer, parameter :: rtype = r32
#else
integer, parameter :: rtype = r64
#endif
#if ITYPE == 64
integer, parameter :: itype = i64
#elif ITYPE == 32
integer, parameter :: itype = i32
#else
integer, parameter :: itype = i32
#endif
#if PTYPE == 64
integer, parameter :: ptype = i64
#elif PTYPE == 32
integer, parameter :: ptype = i32
#else
integer, parameter :: ptype = i32
#endif
#if OTYPE == 64
integer, parameter :: otype = i64
#elif OTYPE == 32
integer, parameter :: otype = i32
#else
integer, parameter :: otype = i32
#endif

end module getkind

!==============================================================================!
!
! allocatable type components are only standard in fortran 2003
!
!==============================================================================!

module types

use getkind

implicit none

type gridinfo


   !real(rtype), dimension(:), allocatable :: alat,along

   real(rtype), dimension(:), pointer :: alat,along
   logical :: lalloc_long=.false.,lalloc_lat=.false.

   real(rtype) north_pole(2)
   integer model,type,nlong,nlat,offset_long
   logical llatrev,lglobal,lrotate
end type gridinfo

end module types

!==============================================================================!

module lsmask

use getkind
use types

implicit none

logical lncmask
logical, dimension(:,:), allocatable :: mask
type(gridinfo), save :: mask_grid

end module lsmask

!==============================================================================!

module lslfrac

use getkind
use types

implicit none

logical lnclfrac
real(rtype), dimension(:,:), allocatable :: alfrac
type(gridinfo), save :: lfrac_grid

end module lslfrac

!==============================================================================!

module constants

use getkind

implicit none

integer, parameter :: imdi = -32768
real(rtype), parameter :: rmdi = -32768.0_rtype*32768.0_rtype
real(rtype), parameter :: pi = 3.14159265358979323846_rtype
real(rtype), parameter :: r = 287.05_rtype
real(rtype), parameter :: cp = 1005.0_rtype
real(rtype), parameter :: kappa = r/cp
real(rtype), parameter :: pref = 100000.0_rtype
real(rtype), parameter :: m_air = 28.96443_rtype
real(rtype), parameter :: m_o3 = 47.9982_rtype
real(rtype), parameter :: vol2mass = m_o3/m_air

end module constants

!==============================================================================!

module parameters

use getkind

implicit none

integer, parameter :: max_nlev=500
integer, parameter :: max_nsoillev=10
integer, parameter :: max_filename_size=256
integer, parameter :: max_varname_size=64
integer, parameter :: max_stdname_size=256
integer, parameter :: max_attname_size=256
integer, parameter :: max_nncfiles=100
integer, parameter :: max_nitem=500
integer, parameter :: max_nancfiles=100
integer, parameter :: max_nancfields=100

integer(itype), parameter :: len_fixhd = 256
integer(itype), parameter :: len_pphead_int = 45
integer(itype), parameter :: len_pphead_real = 19
integer(itype), parameter :: len_inthd = 15
integer(itype), parameter :: len_realhd = 6

end module parameters

!==============================================================================!

module config

use getkind
use parameters

implicit none

logical, parameter  :: lbigend = ichar(transfer(1,'a')) == 0

real(rtype) version
integer ical,isize,iwfio_size,iversion
integer nncfiles,nancfiles,nlev,no3lev,noclev,nsoillev,iavert,iovert
logical l32bit,lwfio,lbigendout,llam,lvargrid
character(max_filename_size) ncfiles(max_nncfiles)

end module config

!==============================================================================!

module vert_soil

use getkind
use parameters

implicit none

real(rtype) soillev(max_nsoillev)

end module vert_soil


!==============================================================================!

module vert_od

use getkind
use parameters

implicit none

real(rtype) etah(max_nlev+1)
real(rtype), dimension(:), allocatable :: ak, bk, akh, bkh
logical lvertrev_od

end module vert_od

!==============================================================================!

module vert_nd

use getkind
use parameters

implicit none

real(rtype) eta_theta(0:max_nlev), eta_rho(max_nlev)
real(rtype), dimension(:), allocatable :: zsea_theta, zsea_rho
real(rtype), dimension(:), allocatable :: c_theta, c_rho
logical lvertrev_nd

end module vert_nd

!==============================================================================!

module vert_ocean

use getkind

implicit none

real(rtype), dimension(:), allocatable ::  depth, thickness
logical ldepthrev

end module vert_ocean

!==============================================================================!

module utils

contains

!==============================================================================!
!
! User defined pure functions are only standard in fortran 95
!
!==============================================================================!

pure function len_trim2(string)

implicit none

character(*), intent(in) :: string
integer :: len_trim2

integer :: i

i = len(string)

do while ((string(i:i) == char(0) .or. string(i:i) == ' ') .and. i > 0)
   i = i-1
enddo

len_trim2 = i

end function len_trim2

!==============================================================================!
!
! User defined specification expressions are only standard in fortran 95
!
!==============================================================================!

function trim2(string)

implicit none

character(*), intent(in) :: string
character(len_trim2(string)) :: trim2

trim2 = string(1:len(trim2))

end function trim2

!==============================================================================!
!
! Allocatable arrays as dummy arguments are only standard in fortran 2003
!
!==============================================================================!

subroutine allocate_array_1d(array,lb,ub)

use getkind

implicit none

real(rtype), intent(inout), dimension(:), allocatable :: array
integer, intent(in) :: lb, ub

if (.not. allocated(array)) then
   allocate(array(lb:ub))
else if (ub-lb+1 > size(array) .or. &
         lb < lbound(array,1) .or. &
         ub > ubound(array,1)) then
   deallocate(array)
   allocate(array(lb:ub))
endif

end subroutine allocate_array_1d

!==============================================================================!

subroutine rev_array(array,n)

use getkind

implicit none

integer, intent(in) :: n
real(rtype), intent(inout) :: array(n)

integer i
real(rtype) dum

do i=1,n/2
   dum = array(i)
   array(i) = array(n-i+1)
   array(n-i+1) = dum
enddo

end subroutine rev_array

!==============================================================================!
!
! Convert string from upper case to lower case
!
!==============================================================================!

subroutine locase(string)

implicit none

character(*), intent(inout) :: string

integer i, ilen, iaup, ialo, idum

ilen = len(string)
iaup = ichar('A')
ialo = ichar('a')
do i = 1, ilen
    idum = ichar(string(i:i))
    if (idum .ge. iaup .and. idum .le. iaup + 25) then
       string(i:i) = char(idum - iaup + ialo)
    endif
enddo

end subroutine locase

!==============================================================================!
!
! Convert string from lower case to upper case
!
!==============================================================================!

subroutine upcase(string)

implicit none

character(*), intent(inout) :: string

integer i, ilen, iaup, ialo, idum

ilen = len(string)
iaup = ichar('A')
ialo = ichar('a')
do i = 1, ilen
    idum = ichar(string(i:i))
    if (idum .ge. ialo .and. idum .le. ialo + 25) then
       string(i:i) = char(idum - ialo + iaup)
    endif
enddo

end subroutine upcase


!==============================================================================!
!
! Date offsetting for dump file header fixups
!
!==============================================================================!

subroutine date_diff(cal, start1, end1, start2, end2)

implicit none

integer, intent(in) :: cal      ! 1 => Gregorian, 2 => 360 day
integer, intent(in), dimension(6) :: start1, end1, start2
integer, intent(out), dimension(6) :: end2

! Basic calculation
end2 = start2 + end1 - start1

! Fix seconds
do while (end2(6) >= 60)
   end2(6) = end2(6) - 60
   end2(5) = end2(5) + 1
end do
do while (end2(6) < 0)
   end2(6) = end2(6) + 60
   end2(5) = end2(5) - 1
end do

! Fix minutes
do while (end2(5) >= 60)
   end2(5) = end2(5) - 60
   end2(4) = end2(4) + 1
end do
do while (end2(5) < 0)
   end2(5) = end2(5) + 60
   end2(4) = end2(4) - 1
end do

! Fix hours
do while (end2(4) >= 24)
   end2(4) = end2(4) - 24
   end2(3) = end2(3) + 1
end do
do while (end2(4) < 0)
   end2(4) = end2(4) + 24
   end2(3) = end2(3) - 1
end do

! Fix date
call update_date(end2(1), end2(2), end2(3), cal)

end subroutine date_diff


!==============================================================================!
!
! Check dump file fixed header arrays
!
!==============================================================================!

pure function has_data(fixhd, offset)

use constants

implicit none

integer, dimension(:), intent(in) :: fixhd
integer, intent(in) :: offset
logical :: has_data

has_data = fixhd(offset) /= IMDI .and. fixhd(offset+1) > 0

end function has_data

end module utils


!==============================================================================!

module stashlevels

contains

function levels_from_stash(lookup, fixhd, intconsts)

use getkind
use parameters

implicit none

integer(itype), dimension(len_fixhd) :: fixhd
integer(itype), dimension(:) :: lookup
integer(itype), dimension(:) :: intconsts
integer :: levels_from_stash

integer(itype) :: modeltype, leveltype

modeltype = fixhd(2)
leveltype = lookup(26)

levels_from_stash = -1

if (modeltype == 1) then
   ! Atmosphere model
   select case (leveltype)
   case (0, 1, 5, 128, 129, 130, 133, 142, 143, 275)
      ! Single level
      levels_from_stash = 1

   case (9)
      ! Model levels
      levels_from_stash = intconsts(8)

   case (6)
      ! Model soil levels
      levels_from_stash = intconsts(10)

   case (8, 19)
      ! Pressure levels: these are used for interpolation of model
      ! output from model levels to standard pressure levels.  It
      ! doesn't make sense to try to use these things for initialising
      ! the model.
      write (*,*) 'Pressure levels in levels_from_stash: not allowed...'
      stop

   case (30, 82, 126, 131, 132, 134, 135, 136, 137, 138, 190, &
        198, 199, 1534, 1535, 1536, 1537, 1538, 1539, 1540)
      write (*,*) 'WARNING: "exotic" level type code ', leveltype, &
           ' in levels_from_stash (treating as single level)'
      levels_from_stash = 1

   case (189)
      write (*,*) 'WARNING: "exotic" level type code ', leveltype, &
           ' in levels_from_stash (treating as model levels)'
      levels_from_stash = intconsts(8)
   end select
else if (modeltype == 2) then
   ! Ocean model
   select case (leveltype)
   case (0, 1, 129)
      ! Single level
      levels_from_stash = 1
   case (2)
      ! Model tracer levels
      levels_from_stash = intconsts(8)
   case default
      write (*,*) 'Unknown level type code ', leveltype, ' for ocean model'
   end select
else
   write (*,*) 'Unknown model type ', modeltype, ' in levels_from_stash'
   stop
end if

if (levels_from_stash == -1) then
   write (*,*) 'Unknown level type ', leveltype, ' in levels_from_stash'
   stop
end if

end function levels_from_stash

end module stashlevels
