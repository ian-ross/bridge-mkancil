
!==============================================================================!
!
! Module is used in all usermod subroutines. Can be used for access to global
! variables.
!
!==============================================================================!

module user_mod

use getkind
use constants

implicit none

end module user_mod

!==============================================================================!

subroutine lfrac_usermod(alfrac,along,alat,nx,ny)

! Any changes here will affect all ancillary fields which use this field to
! calculate the land/sea mask

use user_mod

implicit none

integer nx,ny
real(rtype) alfrac(nx,ny),along(nx),alat(ny)

return
end

!==============================================================================!

subroutine mask_usermod(mask,along,alat,nx,ny)

! Any changes here will affect all ancillary fields which use this field to
! calculate the land/sea mask

use user_mod
use lslfrac

implicit none

integer nx,ny
logical mask(nx,ny)
real(rtype) along(nx),alat(ny)

return
end

!==============================================================================!

subroutine outflow_usermod(outflow,along,alat,nx,ny)

use user_mod
use lslfrac

implicit none

integer nx,ny
integer(itype) outflow(nx,ny)
real(rtype) along(nx),alat(ny)

return
end

!==============================================================================!

subroutine ozone_usermod(ozone,along,alat,nx,ny,iz,it,idate)

!    idate(1) = Year
!    idate(2) = Month
!    idate(3) = Day of month
!    idate(4) = Hour
!    idate(5) = Minute
!    idate(6) = Second

use user_mod
use lslfrac
use lsmask
use vert_od
use vert_nd

implicit none

integer nx,ny,iz,it,idate(6)
real(rtype) ozone(nx,ny),along(nx),alat(ny)

return
end

!==============================================================================!

subroutine soilmoist_usermod(soilmoist,along,alat,nx,ny,iz,it,idate)

!    idate(1) = Year
!    idate(2) = Month
!    idate(3) = Day of month
!    idate(4) = Hour
!    idate(5) = Minute
!    idate(6) = Second

use user_mod
use lslfrac
use lsmask
use vert_soil

implicit none

integer nx,ny,iz,it,idate(6)
real(rtype) soilmoist(nx,ny),along(nx),alat(ny)

return
end

!==============================================================================!

subroutine snowdepth_usermod(snowdepth,along,alat,nx,ny,it,idate)

!    idate(1) = Year
!    idate(2) = Month
!    idate(3) = Day of month
!    idate(4) = Hour
!    idate(5) = Minute
!    idate(6) = Second

use user_mod
use lslfrac
use lsmask

implicit none

integer nx,ny,iz,it,idate(6)
real(rtype) snowdepth(nx,ny),along(nx),alat(ny)

return
end

!==============================================================================!

subroutine slt_usermod(slt,along,alat,nx,ny,iz,it,idate)

!    idate(1) = Year
!    idate(2) = Month
!    idate(3) = Day of month
!    idate(4) = Hour
!    idate(5) = Minute
!    idate(6) = Second

use user_mod
use lslfrac
use lsmask
use vert_soil

implicit none

integer nx,ny,iz,it,idate(6)
real(rtype) slt(nx,ny),along(nx),alat(ny)

return
end

!==============================================================================!

subroutine soil_usermod(soilparm,along,alat,nx,ny,stashcode)

!             = 40  soilparm = VOL. SMC at wilting point
!             = 41  soilparm = VOL. SMC at critical point
!             = 42  soilparm = VOL. SMC at field capacity
!             = 43  soilparm = VOL. SMC at saturation
!             = 207 soilparm = Clapp-Hornberger B parameter
!   stashcode = 47  soilparm = Thermal conductivity of soil
!             = 44  soilparm = Saturated soil conductivity
!             = 46  soilparm = Thermal capacity of soil
!             = 48  soilparm = Saturated soil water suction
!             = 220 soilparm = Snow free soil albedo
!             = 223 soilparm = Soil carbon content

use user_mod
use lslfrac
use lsmask

implicit none

integer nx,ny,stashcode
real(rtype) soilparm(nx,ny),along(nx),alat(ny)

return
end

!==============================================================================!

subroutine veg_usermod(vegparm,along,alat,nx,ny,stashcode)

!             = 51  vegparm = Root Depth
!             = 52  vegparm = Snow free albedo
!             = 54  vegparm = Surface resistance to evaporation
!             = 26  vegparm = Roughness length
!   stashcode = 55  vegparm = Canopy capacity
!             = 50  vegparm = Vegetation fraction
!             = 56  vegparm = Infiltration factor
!             = 53  vegparm = Deep snow albedo
!             = 208 vegparm = Leaf area index
!             = 209 vegparm = Canopy height

use user_mod
use lslfrac
use lsmask

implicit none

integer nx,ny,stashcode
real(rtype) vegparm(nx,ny),along(nx),alat(ny)

return
end

!==============================================================================!

subroutine vegfrac_usermod(vegfrac,along,alat,nx,ny,iz)

!             = 1 surface type = broadleaf trees
!             = 2 surface type = needleleaf trees
!             = 3 surface type = C3 grass
!             = 4 surface type = C4 grass
!          iz = 5 surface type = shrub
!             = 6 surface type = urban
!             = 7 surface type = water
!             = 8 surface type = soil
!             = 9 surface type = ice

use user_mod
use lslfrac
use lsmask

implicit none

integer nx,ny,iz
real(rtype) vegfrac(nx,ny),along(nx),alat(ny)

return
end

!==============================================================================!

subroutine vegfunc_usermod(vegfunc,along,alat,nx,ny,iz,it,idate,stashcode)

!             = 217 vegfunc = Leaf area index of plant functional types
!   stashcode = 218 vegfunc = Canopy height of plant functional types
!             = 213 vegfunc = Canopy conductance

!             = 1 plant functional type = broadleaf trees
!             = 2 plant functional type = needleleaf trees
!          iz = 3 plant functional type = C3 grass
!             = 4 plant functional type = C4 grass
!             = 5 plant functional type = shrub

!    idate(1) = Year
!    idate(2) = Month
!    idate(3) = Day of month
!    idate(4) = Hour
!    idate(5) = Minute
!    idate(6) = Second

use user_mod
use lslfrac
use lsmask

implicit none

integer nx,ny,iz,it,idate(6),stashcode
real(rtype) vegfunc(nx,ny),along(nx),alat(ny)

return
end

!==============================================================================!

subroutine vegdist_usermod(vegdist,along,alat,nx,ny)

use user_mod
use lslfrac
use lsmask

implicit none

integer nx,ny
real(rtype) vegdist(nx,ny),along(nx),alat(ny)

return
end

!==============================================================================!

subroutine sst_usermod(sst,along,alat,nx,ny,it,idate)

!    idate(1) = Year
!    idate(2) = Month
!    idate(3) = Day of month
!    idate(4) = Hour
!    idate(5) = Minute
!    idate(6) = Second

use user_mod
use lslfrac
use lsmask

implicit none

integer nx,ny,it,idate(6)
real(rtype) sst(nx,ny),along(nx),alat(ny)

return
end

!==============================================================================!

subroutine ssticefrac_usermod(sst,icefrac,along,alat,nx,ny,it,idate)

!    idate(1) = Year
!    idate(2) = Month
!    idate(3) = Day of month
!    idate(4) = Hour
!    idate(5) = Minute
!    idate(6) = Second

use user_mod
use lslfrac
use lsmask

implicit none

integer nx,ny,it,idate(6)
real(rtype) sst(nx,ny),icefrac(nx,ny),along(nx),alat(ny)

return
end

!==============================================================================!

subroutine icedepth_usermod(icedepth,along,alat,nx,ny,it,idate)

!    idate(1) = Year
!    idate(2) = Month
!    idate(3) = Day of month
!    idate(4) = Hour
!    idate(5) = Minute
!    idate(6) = Second

use user_mod
use lslfrac
use lsmask

implicit none

integer nx,ny,it,idate(6)
real(rtype) icedepth(nx,ny),along(nx),alat(ny)

return
end

!==============================================================================!

subroutine orog_usermod(orog,sd,xgrad,ygrad,xxgrad,xygrad,yygrad, &
                        sil,ptht,unfilt, &
                        lincgrad,lincsqgrad,lincrough,lincunfilt, &
                        along,alat,nx,ny)

use user_mod
use lslfrac
use lsmask

implicit none

integer nx,ny
logical lincgrad,lincsqgrad,lincrough,lincunfilt
real(rtype) orog(nx,ny),sd(nx,ny)
real(rtype) xgrad(nx,ny),ygrad(nx,ny)
real(rtype) xxgrad(nx,ny),xygrad(nx,ny),yygrad(nx,ny)
real(rtype) sil(nx,ny),ptht(nx,ny),unfilt(nx,ny)
real(rtype) along(nx),alat(ny)

return
end

!==============================================================================!

subroutine ts1_usermod(refsst,refsss,climat,climid,along,alat,nx,ny,it,idate)

!    idate(1) = Year
!    idate(2) = Month
!    idate(3) = Day of month
!    idate(4) = Hour
!    idate(5) = Minute
!    idate(6) = Second

use user_mod
use lslfrac
use lsmask

implicit none

integer nx,ny,it,idate(6)
real(rtype) refsst(nx,ny),refsss(nx,ny),climat(nx,ny),climid(nx,ny)
real(rtype) along(nx),alat(ny)

return
end

!==============================================================================!

subroutine flux_usermod(heatflx,saltflx,along,alat,nx,ny,it,idate)

!    idate(1) = Year
!    idate(2) = Month
!    idate(3) = Day of month
!    idate(4) = Hour
!    idate(5) = Minute
!    idate(6) = Second

use user_mod
use lslfrac
use lsmask

implicit none

integer nx,ny,it,idate(6)
real(rtype) heatflx(nx,ny),saltflx(nx,ny)
real(rtype) along(nx),alat(ny)

return
end

!==============================================================================!

subroutine genanc_usermod_r(anc,along,alat,nx,ny,iz,it,idate,ifield,stashcode)

!    idate(1) = Year
!    idate(2) = Month
!    idate(3) = Day of month
!    idate(4) = Hour
!    idate(5) = Minute
!    idate(6) = Second

use user_mod
use lslfrac
use lsmask
use vert_od
use vert_nd
use vert_soil
use vert_ocean

implicit none

integer nx,ny,iz,it,idate(6),ifield,stashcode
real(rtype) along(nx),alat(ny)
real(rtype) anc(nx,ny)

return
end

!==============================================================================!

subroutine genanc_usermod_i(anc,along,alat,nx,ny,iz,it,idate, &
                            ifield,stashcode,idatatype)

!    idate(1) = Year
!    idate(2) = Month
!    idate(3) = Day of month
!    idate(4) = Hour
!    idate(5) = Minute
!    idate(6) = Second

use user_mod
use lslfrac
use lsmask
use vert_od
use vert_nd
use vert_soil
use vert_ocean

implicit none

integer nx,ny,iz,it,idate(6),ifield,stashcode,idatatype
real(rtype) along(nx),alat(ny)
integer(itype) anc(nx,ny)
!logical lanc(nx,ny)

! If idatatype=3 then anc is really a logical type not integer

!if (idatatype == 3) then
!   where (anc == 0)
!      lanc = .false.
!   elsewhere
!      lanc = .true.
!   endwhere
!endif

return
end
