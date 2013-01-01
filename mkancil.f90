
!==============================================================================!

program mkancil

use getkind
use parameters

use config
use lsmask
use lslfrac
use vert_soil
use vert_od
use vert_nd
use vert_ocean

implicit none

! config namelist

namelist /nam_config/ ical,isize,l32bit,version,lbigendout,lwfio,iwfio_size, &
                      nncfiles,ncfiles

! grid config namelist

logical ldeepsoil,ltheta,lozonelev,llevstoreup
character(max_filename_size) nam_vert,avert_filein,overt_filein
character(max_varname_size) avert_ncname,overt_ncname

namelist /nam_gridconfig/ nlev,no3lev,nsoillev,iovert,iavert, &
                          lvargrid,ldeepsoil,ltheta,lozonelev,llevstoreup, &
                          soillev,nam_vert, &
                          avert_filein,avert_ncname, &
                          overt_filein,overt_ncname

! ozone namelist

real(rtype) aozone_mrfac
integer iozone_timeusage1,iozone_timeusage2
integer iozone_startdate(6),iozone_ntimes,iozone_interval,iozone_intunit
logical lozone,lozone_mmr,lozone_periodic,lozone_mm
character(max_filename_size) ozone_filein,ozone_fileout
character(max_varname_size) ozone_ncname

namelist /nam_ozone/ iozone_timeusage1,iozone_timeusage2, &
                     iozone_startdate,iozone_ntimes,iozone_interval, &
                     iozone_intunit, aozone_mrfac, &
                     lozone,lozone_mmr,lozone_periodic,lozone_mm, &
                     ozone_filein,ozone_fileout,ozone_ncname

! smow namelist

integer ismow_mask,ismow_timeusage1,ismow_timeusage2
integer ismow_startdate(6),ismow_ntimes,ismow_interval,ismow_intunit
logical lsmow,lsmow_calcsnowedge,lsmow_periodic,lsmow_mm
character(max_filename_size) smow_filein,smow_fileout
character(max_varname_size) smow_ncsnowdepthname,smow_ncsnowedgename
character(max_varname_size) smow_ncsoilmoisturename

namelist /nam_smow/ ismow_mask,ismow_timeusage1,ismow_timeusage2, &
                    ismow_startdate,ismow_ntimes,ismow_interval,ismow_intunit, &
                    lsmow,lsmow_calcsnowedge,lsmow_periodic,lsmow_mm, &
                    smow_filein,smow_fileout,smow_ncsnowdepthname, &
                    smow_ncsnowedgename,smow_ncsoilmoisturename

! slt namelist

integer islt_mask,islt_timeusage1,islt_timeusage2
integer islt_startdate(6),islt_ntimes,islt_interval,islt_intunit
logical lslt,lslt_periodic,lslt_mm
character(max_filename_size) slt_filein,slt_fileout
character(max_varname_size) slt_ncname

namelist /nam_slt/ islt_mask,islt_timeusage1,islt_timeusage2, &
                   islt_startdate,islt_ntimes,islt_interval,islt_intunit, &
                   lslt,lslt_periodic,lslt_mm, &
                   slt_filein,slt_fileout,slt_ncname


! soil namelist

integer isoil_mask
logical lsoil
character(max_filename_size) soil_filein,soil_fileout
character(max_varname_size) soil_ncvsmcwiltname,soil_ncvsmccritname
character(max_varname_size) soil_ncvsmcfcapname,soil_ncvsmcsatname
character(max_varname_size) soil_ncclapphornname,soil_ncthermcondname
character(max_varname_size) soil_ncsoilcondname,soil_ncthermcapname
character(max_varname_size) soil_ncsoilwatersucname,soil_ncsoilalbname
character(max_varname_size) soil_ncsoilcarbname

namelist /nam_soil/ isoil_mask,lsoil,soil_filein,soil_fileout, &
                    soil_ncvsmcwiltname,soil_ncvsmccritname, &
                    soil_ncvsmcfcapname,soil_ncvsmcsatname, &
                    soil_ncclapphornname,soil_ncthermcondname, &
                    soil_ncsoilcondname,soil_ncthermcapname, &
                    soil_ncsoilwatersucname,soil_ncsoilalbname, &
                    soil_ncsoilcarbname

! veg namelist

integer iveg_mask
logical lveg
character(max_filename_size) veg_filein,veg_fileout
character(max_varname_size) veg_ncrootdepthname,veg_ncsfaname
character(max_varname_size) veg_ncsurfresistname,veg_ncz0name
character(max_varname_size) veg_nccancapname,veg_ncvegfracname
character(max_varname_size) veg_ncinfiltname,veg_ncdsaname
character(max_varname_size) veg_nclainame,veg_nccanhtname

namelist /nam_veg/ iveg_mask,lveg,veg_filein,veg_fileout, &
                   veg_ncrootdepthname,veg_ncsfaname, &
                   veg_ncsurfresistname,veg_ncz0name, &
                   veg_nccancapname,veg_ncvegfracname, &
                   veg_ncinfiltname,veg_ncdsaname, &
                   veg_nclainame,veg_nccanhtname

! vegfrac namelist

integer ivegfrac_nsurftypes,ivegfrac_mask
logical lvegfrac
character(max_filename_size) vegfrac_filein,vegfrac_fileout
character(max_varname_size) vegfrac_ncname

namelist /nam_vegfrac/ ivegfrac_mask,ivegfrac_nsurftypes,lvegfrac, &
                       vegfrac_filein,vegfrac_fileout,vegfrac_ncname

! vegfunc namelist

integer ivegfunc_nfunctypes,ivegfunc_mask
integer ivegfunc_timeusage1,ivegfunc_timeusage2
integer ivegfunc_startdate(6),ivegfunc_ntimes
integer ivegfunc_interval,ivegfunc_intunit
logical lvegfunc,lvegfunc_periodic,lvegfunc_mm
character(max_filename_size) vegfunc_filein,vegfunc_fileout
character(max_varname_size) vegfunc_nclainame,vegfunc_nccanhtname
character(max_varname_size) vegfunc_nccancondname

namelist /nam_vegfunc/ ivegfunc_mask,ivegfunc_timeusage1,ivegfunc_timeusage2, &
                       ivegfunc_startdate,ivegfunc_ntimes, &
                       ivegfunc_interval,ivegfunc_intunit,ivegfunc_nfunctypes, &
                       lvegfunc,lvegfunc_periodic,lvegfunc_mm, &
                       vegfunc_filein,vegfunc_fileout, &
                       vegfunc_nclainame,vegfunc_nccanhtname, &
                       vegfunc_nccancondname

! vegdist namelist

integer ivegdist_mask
logical lvegdist
character(max_filename_size) vegdist_filein,vegdist_fileout
character(max_varname_size) vegdist_ncname

namelist /nam_vegdist/ ivegdist_mask,lvegdist, &
                       vegdist_filein,vegdist_fileout,vegdist_ncname

! sst namelist

real(rtype) asst_min,asst_iceval
integer isst_mask,isst_timeusage1,isst_timeusage2
integer isst_startdate(6),isst_ntimes,isst_interval,isst_intunit
logical lsst,lsst_min,lsst_iceval,lsst_periodic,lsst_mm
character(max_filename_size) sst_filein,sst_fileout
character(max_varname_size) sst_ncname

namelist /nam_sst/ isst_mask,isst_timeusage1,isst_timeusage2, &
                   isst_startdate,isst_ntimes,isst_interval,isst_intunit, &
                   asst_min,asst_iceval, &
                   lsst,lsst_min,lsst_iceval,lsst_periodic,lsst_mm, &
                   sst_filein,sst_fileout,sst_ncname

! ice namelist

real(rtype) aice_sstval,aice_cutoff,aice_min,aice_max
integer iice_mask,iice_timeusage1,iice_timeusage2
integer iice_startdate(6),iice_ntimes,iice_interval,iice_intunit
logical lice,lice_amip2,lice_sstval,lice_calcdepth,lice_calcedge
logical lice_percent,lice_mkmask,lice_min,lice_max,lice_periodic,lice_mm
character(max_filename_size) ice_filein,ice_fileout
character(max_varname_size) ice_ncfracname,ice_ncdepthname,ice_ncedgename

namelist /nam_ice/ iice_mask,iice_timeusage1,iice_timeusage2, &
                   iice_startdate,iice_ntimes,iice_interval,iice_intunit, &
                   aice_sstval,aice_cutoff,aice_min,aice_max, &
                   lice,lice_amip2,lice_sstval,lice_calcdepth,lice_calcedge, &
                   lice_percent,lice_mkmask,lice_min,lice_max, &
                   lice_periodic,lice_mm, &
                   ice_filein,ice_fileout, &
                   ice_ncfracname,ice_ncdepthname,ice_ncedgename

! orog namelist

integer iorog_mask
logical lorog,lorog_incgrad,lorog_incsqgrad,lorog_incrough,lorog_incunfilt
character(max_filename_size) orog_filein,orog_fileout
character(max_varname_size) orog_ncname,orog_ncsdname
character(max_varname_size) orog_ncxgradname,orog_ncygradname
character(max_varname_size) orog_ncxxgradname,orog_ncxygradname
character(max_varname_size) orog_ncyygradname
character(max_varname_size) orog_ncsilname,orog_ncpthtname
character(max_varname_size) orog_ncunfiltname

namelist /nam_orog/ iorog_mask,lorog, &
                    lorog_incgrad,lorog_incsqgrad, &
                    lorog_incrough,lorog_incunfilt, &
                    orog_filein,orog_fileout,orog_ncname,orog_ncsdname, &
                    orog_ncxgradname,orog_ncygradname, &
                    orog_ncxxgradname,orog_ncxygradname,orog_ncyygradname, &
                    orog_ncsilname,orog_ncpthtname,orog_ncunfiltname

! mask namelist

real(rtype) amask_val
integer imask_val
logical lmask,lmask_use,lmask_uselfrac,lmask_usemdi,lmask_sea,lmask_outflow
character(max_filename_size) mask_filein,mask_fileout
character(max_varname_size) mask_ncname,mask_ncofname

namelist /nam_mask/ amask_val,imask_val, &
                    lmask,lmask_use,lmask_uselfrac, &
                    lmask_usemdi,lmask_sea,lmask_outflow, &
                    mask_filein,mask_fileout,mask_ncname,mask_ncofname

! lfrac namelist

logical llfrac,llfrac_use
character(max_filename_size) lfrac_filein,lfrac_fileout
character(max_varname_size) lfrac_ncname

namelist /nam_lfrac/ llfrac,llfrac_use,lfrac_filein,lfrac_fileout,lfrac_ncname

! ausrmulti namelist

integer iausrmulti_timeusage1,iausrmulti_timeusage2
integer iausrmulti_startdate(6),iausrmulti_ntimes
integer iausrmulti_interval,iausrmulti_intunit
integer iausrmulti_nfield
logical lausrmulti,lausrmulti_periodic,lausrmulti_mm
character(max_filename_size) ausrmulti_fileout

integer iausrmulti_stashcode(max_nancfields),iausrmulti_ppcode(max_nancfields)
integer iausrmulti_gridtype(max_nancfields),iausrmulti_datatype(max_nancfields)
integer iausrmulti_masktype(max_nancfields),iausrmulti_mask(max_nancfields)
integer iausrmulti_fileinid(max_nancfields)
logical lausrmulti_theta(max_nancfields)
character(max_varname_size) ausrmulti_ncname(max_nancfields)

namelist /nam_ausrmulti/ iausrmulti_timeusage1,iausrmulti_timeusage2, &
                         iausrmulti_startdate,iausrmulti_ntimes, &
                         iausrmulti_interval,iausrmulti_intunit, &
                         iausrmulti_nfield, &
                         iausrmulti_stashcode,iausrmulti_ppcode, &
                         iausrmulti_gridtype,iausrmulti_datatype, &
                         iausrmulti_masktype,iausrmulti_mask, &
                         iausrmulti_fileinid, &
                         lausrmulti,lausrmulti_periodic, &
                         lausrmulti_mm,lausrmulti_theta, &
                         ausrmulti_ncname,ausrmulti_fileout

! ausrancil namelist

integer iausrancil_timeusage1,iausrancil_timeusage2
integer iausrancil_startdate(6),iausrancil_ntimes
integer iausrancil_interval,iausrancil_intunit
integer iausrancil_nfield
logical lausrancil,lausrancil_periodic,lausrancil_mm
character(max_filename_size) ausrancil_fileout

integer iausrancil_stashcode(max_nancfields),iausrancil_ppcode(max_nancfields)
integer iausrancil_gridtype(max_nancfields),iausrancil_datatype(max_nancfields)
integer iausrancil_masktype(max_nancfields),iausrancil_mask(max_nancfields)
integer iausrancil_fileinid(max_nancfields)
character(max_varname_size) ausrancil_ncname(max_nancfields)

namelist /nam_ausrancil/ iausrancil_timeusage1,iausrancil_timeusage2, &
                         iausrancil_startdate,iausrancil_ntimes, &
                         iausrancil_interval,iausrancil_intunit, &
                         iausrancil_nfield, &
                         iausrancil_stashcode,iausrancil_ppcode, &
                         iausrancil_gridtype,iausrancil_datatype, &
                         iausrancil_masktype,iausrancil_mask, &
                         iausrancil_fileinid, &
                         lausrancil,lausrancil_periodic,lausrancil_mm, &
                         ausrancil_ncname,ausrancil_fileout

! ts1 namelist

integer its1_mask,its1_timeusage1,its1_timeusage2
integer its1_startdate(6),its1_ntimes,its1_interval,its1_intunit
logical lts1,lts1_refsst,lts1_refsss,lts1_climat,lts1_climid
logical lts1_periodic,lts1_mm
character(max_filename_size) ts1_filein,ts1_fileout
character(max_varname_size) ts1_ncsstname,ts1_ncsssname
character(max_varname_size) ts1_ncatname,ts1_ncidname

namelist /nam_ts1/ its1_mask,its1_timeusage1,its1_timeusage2, &
                   its1_startdate,its1_ntimes,its1_interval,its1_intunit, &
                   lts1,lts1_refsst,lts1_refsss,lts1_climat,lts1_climid, &
                   lts1_periodic,lts1_mm,ts1_filein,ts1_fileout, &
                   ts1_ncsstname,ts1_ncsssname,ts1_ncatname,ts1_ncidname

! flux namelist

integer iflux_mask,iflux_timeusage1,iflux_timeusage2
integer iflux_startdate(6),iflux_ntimes,iflux_interval,iflux_intunit
logical lflux,lflux_heat,lflux_salt
logical lflux_periodic,lflux_mm
character(max_filename_size) flux_filein,flux_fileout
character(max_varname_size) flux_ncheatname,flux_ncsaltname

namelist /nam_flux/ iflux_mask,iflux_timeusage1,iflux_timeusage2, &
                    iflux_startdate,iflux_ntimes,iflux_interval,iflux_intunit, &
                    lflux,lflux_heat,lflux_salt,lflux_periodic,lflux_mm, &
                    flux_filein,flux_fileout,flux_ncheatname,flux_ncsaltname

! ousrmulti namelist

integer iousrmulti_timeusage1,iousrmulti_timeusage2
integer iousrmulti_startdate(6),iousrmulti_ntimes
integer iousrmulti_interval,iousrmulti_intunit
integer iousrmulti_nfield
logical lousrmulti,lousrmulti_periodic,lousrmulti_mm
character(max_filename_size) ousrmulti_fileout

integer iousrmulti_stashcode(max_nancfields),iousrmulti_ppcode(max_nancfields)
integer iousrmulti_gridtype(max_nancfields),iousrmulti_datatype(max_nancfields)
integer iousrmulti_masktype(max_nancfields),iousrmulti_mask(max_nancfields)
integer iousrmulti_fileinid(max_nancfields)
character(max_varname_size) ousrmulti_ncname(max_nancfields)

namelist /nam_ousrmulti/ iousrmulti_timeusage1,iousrmulti_timeusage2, &
                         iousrmulti_startdate,iousrmulti_ntimes, &
                         iousrmulti_interval,iousrmulti_intunit, &
                         iousrmulti_nfield, &
                         iousrmulti_stashcode,iousrmulti_ppcode, &
                         iousrmulti_gridtype,iousrmulti_datatype, &
                         iousrmulti_masktype,iousrmulti_mask, &
                         iousrmulti_fileinid, &
                         lousrmulti,lousrmulti_periodic, lousrmulti_mm, &
                         ousrmulti_ncname,ousrmulti_fileout

! ousrancil namelist

integer iousrancil_timeusage1,iousrancil_timeusage2
integer iousrancil_startdate(6),iousrancil_ntimes
integer iousrancil_interval,iousrancil_intunit
integer iousrancil_nfield
logical lousrancil,lousrancil_periodic,lousrancil_mm
character(max_filename_size) ousrancil_fileout

integer iousrancil_stashcode(max_nancfields),iousrancil_ppcode(max_nancfields)
integer iousrancil_gridtype(max_nancfields),iousrancil_datatype(max_nancfields)
integer iousrancil_masktype(max_nancfields),iousrancil_mask(max_nancfields)
integer iousrancil_fileinid(max_nancfields)
character(max_varname_size) ousrancil_ncname(max_nancfields)

namelist /nam_ousrancil/ iousrancil_timeusage1,iousrancil_timeusage2, &
                         iousrancil_startdate,iousrancil_ntimes, &
                         iousrancil_interval,iousrancil_intunit, &
                         iousrancil_nfield, &
                         iousrancil_stashcode,iousrancil_ppcode, &
                         iousrancil_gridtype,iousrancil_datatype, &
                         iousrancil_masktype,iousrancil_mask, &
                         iousrancil_fileinid, &
                         lousrancil,lousrancil_periodic,lousrancil_mm, &
                         ousrancil_ncname,ousrancil_fileout

! genanc_config namelist

namelist /nam_genanc_config/ nancfiles

! genanc namelist

integer igenanc_timeusage1(max_nancfiles),igenanc_timeusage2(max_nancfiles)
integer igenanc_startdate(6,max_nancfiles),igenanc_ntimes(max_nancfiles)
integer igenanc_interval(max_nancfiles),igenanc_intunit(max_nancfiles)
integer igenanc_model(max_nancfiles),igenanc_nfield(max_nancfiles)
integer igenanc_inthd8(max_nancfiles)
logical lgenanc_file(max_nancfiles)
logical lgenanc_periodic(max_nancfiles),lgenanc_mm(max_nancfiles)
character(max_filename_size) genanc_fileout(max_nancfiles)

integer igenanc_stashcode(max_nancfields,max_nancfiles)
integer igenanc_ppcode(max_nancfields,max_nancfiles)
integer igenanc_levtype(max_nancfields,max_nancfiles)
integer igenanc_nlev(max_nancfields,max_nancfiles)
logical lgenanc_theta(max_nancfields,max_nancfiles)
integer igenanc_gridtype(max_nancfields,max_nancfiles)
integer igenanc_datatype(max_nancfields,max_nancfiles)
integer igenanc_masktype(max_nancfields,max_nancfiles)
integer igenanc_mask(max_nancfields,max_nancfiles)
integer igenanc_fileinid(max_nancfields,max_nancfiles)
character(max_varname_size) genanc_ncname(max_nancfields,max_nancfiles)

namelist /nam_genanc/ igenanc_model,igenanc_timeusage1,igenanc_timeusage2, &
                      igenanc_startdate,igenanc_ntimes,igenanc_interval, &
                      igenanc_intunit,igenanc_nfield,igenanc_inthd8, &
                      igenanc_stashcode,igenanc_ppcode, &
                      igenanc_levtype,igenanc_nlev, &
                      igenanc_gridtype,igenanc_datatype, &
                      igenanc_masktype,igenanc_mask,igenanc_fileinid, &
                      lgenanc_file,lgenanc_periodic, &
                      lgenanc_mm,lgenanc_theta, &
                      genanc_ncname,genanc_fileout

! astart namelist

integer iastart_timeusage1,iastart_timeusage2,iastart_startdate(6)
integer iastart_nncfiles
integer iastart1_nitem,iastart1_ncfileid(max_nitem),iastart1_itemid(max_nitem)
logical lastart,lastart_newlsm
logical lastart1,lastart1_usestdname,lastart1_useconfig
logical lastart2
character(max_filename_size) astart_filein(max_nncfiles)
character(max_filename_size) astart_fileout
character(max_filename_size) astart1_umfile
character(max_varname_size) astart1_ncname(max_nitem)

namelist /nam_astart/ iastart_timeusage1,iastart_timeusage2,iastart_startdate, &
                      iastart_nncfiles,iastart1_nitem, &
                      iastart1_ncfileid,iastart1_itemid, &
                      lastart,lastart_newlsm, &
                      lastart1,lastart1_usestdname,lastart1_useconfig, &
                      lastart2, &
                      astart_filein,astart_fileout,astart1_umfile,astart1_ncname

! ostart namelist

integer iostart_timeusage1,iostart_timeusage2,iostart_startdate(6)
integer iostart_nncfiles
integer iostart_nitem,iostart_ncfileid(max_nitem),iostart_itemid(max_nitem)
logical lostart,lostart_usestdname,lostart_useconfig
character(max_filename_size) ostart_filein(max_nncfiles)
character(max_filename_size) ostart_fileout
character(max_filename_size) ostart_umfile
character(max_varname_size) ostart_ncname(max_nitem)
logical lostart_bathy
character(max_filename_size) ostart_bathy_filein
character(max_varname_size) ostart_bathy_ncname
logical lostart_islands_replace, lostart_islands_add
character(max_filename_size) ostart_islands_filein

namelist /nam_ostart/ iostart_timeusage1,iostart_timeusage2,iostart_startdate, &
                      iostart_nncfiles,iostart_nitem,iostart_ncfileid, &
                      iostart_itemid, &
                      lostart,lostart_usestdname,lostart_useconfig, &
                      ostart_filein,ostart_fileout,ostart_umfile, &
                      ostart_ncname, &
                      lostart_bathy,ostart_bathy_filein,ostart_bathy_ncname, &
                      lostart_islands_replace,lostart_islands_add, &
                      ostart_islands_filein

integer i, j, k, ierr, ifile

! These 3 arrays are used so that user ancil files can call genanc routine

logical lusrancil_theta(max_nancfields)
integer iusrancil_levtype(max_nancfields),iusrancil_nlev(max_nancfields)

! namelist defaults

ical = 2
isize = 64
l32bit = .false.
version = 4.5
lbigendout = .true.
lwfio = .false.
iwfio_size = 2048
nlev = -1
no3lev = -1
noclev = -1
nsoillev = -1
nncfiles = 0
iavert = 0
iovert = 0
llevstoreup = .true.
ltheta = .true.
lvargrid = .false.
ldeepsoil = .true.
lvertrev_od = .false.
lvertrev_nd = .false.
ldepthrev = .false.
lozone = .false.
lsmow = .false.
lslt = .false.
lsoil = .false.
lveg = .false.
lvegfrac = .false.
ivegfrac_nsurftypes=9
lvegfunc = .false.
ivegfunc_nfunctypes=5
lvegdist = .false.
lsst = .false.
lice = .false.
lice_percent = .false.
lorog = .false.
lmask = .false.
lmask_use = .false.
lmask_sea = .false.
amask_val = -987789.0_rtype
imask_val = -987789
llfrac = .false.
llfrac_use = .false.
lausrmulti = .false.
iausrmulti_nfield = 0
lausrancil = .false.
iausrancil_nfield = 0
lts1 = .false.
lflux = .false.
lousrmulti = .false.
iousrmulti_nfield = 0
lousrancil = .false.
iousrancil_nfield = 0
nancfiles = 1
igenanc_nfield = 0
lastart = .false.
iastart_nncfiles = 1
lastart_newlsm = .false.
lastart1 = .false.
lastart1_usestdname = .true.
lastart1_useconfig = .false.
iastart1_nitem = 0
iastart1_ncfileid = 1
lastart2 = .false.
lostart = .false.
iostart_nncfiles = 1
lostart_bathy = .false.
lostart_islands_replace = .false.
lostart_islands_add = .false.
iostart_nitem = 0
iostart_ncfileid = 1

! read namelists

read(*,nam_config)
!write(*,nam_config)

read(*,nam_gridconfig)
!write(*,nam_gridconfig)

read(*,nam_ozone)
!write(*,nam_ozone)

read(*,nam_smow)
!write(*,nam_smow)

read(*,nam_slt)
!write(*,nam_slt)

read(*,nam_soil)
!write(*,nam_soil)

read(*,nam_veg)
!write(*,nam_veg)

read(*,nam_vegfrac)
!write(*,nam_vegfrac)

read(*,nam_vegfunc)
!write(*,nam_vegfunc)

read(*,nam_vegdist)
!write(*,nam_vegdist)

read(*,nam_sst)
!write(*,nam_sst)

read(*,nam_ice)
!write(*,nam_ice)

read(*,nam_orog)
!write(*,nam_orog)

read(*,nam_mask)
!write(*,nam_mask)

read(*,nam_lfrac)
!write(*,nam_lfrac)

read(*,nam_ausrmulti)
!write(*,nam_ausrmulti)

read(*,nam_ausrancil)
!write(*,nam_ausrancil)

read(*,nam_ts1)
!write(*,nam_ts1)

read(*,nam_flux)
!write(*,nam_flux)

read(*,nam_ousrmulti)
!write(*,nam_ousrmulti)

read(*,nam_ousrancil)
!write(*,nam_ousrancil)

read(*,nam_genanc_config)
!write(*,nam_genanc_config)

read(*,nam_genanc)
!write(*,nam_genanc)

read(*,nam_astart)
!write(*,nam_astart)

read(*,nam_ostart)
!write(*,nam_ostart)

if (isize == 32) l32bit = .false.

iversion = (int(version*10)/10)*100 + mod(int(version*10),10)

! UM versions >= 5.0 don't work with non-wfio files

if (iversion >= 500 .and. .not. lwfio) then
   lwfio = .true.
   iwfio_size = 1
endif

! Check namelist arrays don't go out of bounds

ierr = 0

if (nlev > max_nlev) then
   write(*,*)'ERROR: nlev = ',nlev,' > max_nlev = ',max_nlev
   write(*,*)'Increase max_nlev and recompile mkancil'
   ierr = 1
endif

if (nsoillev > max_nsoillev) then
   write(*,*)'ERROR: nsoillev = ',nsoillev,' > max_nsoillev = ',max_nsoillev
   write(*,*)'Increase max_nsoillev and recompile mkancil'
   ierr = 1
endif

if (nncfiles > max_nncfiles) then
   write(*,*)'ERROR: nncfiles = ',nncfiles, &
             ' > max_nncfiles = ',max_nncfiles
   write(*,*)'Increase max_nncfiles and recompile mkancil'
   ierr = 1
endif

if (nancfiles > max_nancfiles) then
   write(*,*)'ERROR: nancfiles = ',nancfiles, &
             ' > max_nancfiles = ',max_nancfiles
   write(*,*)'Increase max_nancfiles and recompile mkancil'
   ierr = 1
endif

if (iausrmulti_nfield > max_nancfields) then
   write(*,*)'ERROR: iausrmulti_nfield = ',iausrmulti_nfield, &
             ' > max_nancfields = ',max_nancfields
   write(*,*)'Increase max_nancfields and recompile mkancil'
   ierr = 1
endif

if (iausrancil_nfield > max_nancfields) then
   write(*,*)'ERROR: iausrancil_nfield = ',iausrancil_nfield, &
             ' > max_nancfields = ',max_nancfields
   write(*,*)'Increase max_nancfields and recompile mkancil'
   ierr = 1
endif

if (iousrmulti_nfield > max_nancfields) then
   write(*,*)'ERROR: iousrmulti_nfield = ',iousrmulti_nfield, &
             ' > max_nancfields = ',max_nancfields
   write(*,*)'Increase max_nancfields and recompile mkancil'
   ierr = 1
endif

if (iousrancil_nfield > max_nancfields) then
   write(*,*)'ERROR: iousrancil_nfield = ',iousrancil_nfield, &
             ' > max_nancfields = ',max_nancfields
   write(*,*)'Increase max_nancfields and recompile mkancil'
   ierr = 1
endif

do k=1,nancfiles
   if (igenanc_nfield(k) > max_nancfields) then
      write(*,*)'ERROR: igenanc_nfield(',k,') = ',igenanc_nfield(k), &
                ' > max_nancfields = ',max_nancfields
      write(*,*)'Increase max_nancfields and recompile mkancil'
      ierr = 1
   endif
enddo

if (iastart_nncfiles > max_nncfiles) then
   write(*,*)'ERROR: iastart_nncfiles = ',iastart_nncfiles, &
             ' > max_nncfiles = ',max_nncfiles
   write(*,*)'Increase max_nncfiles and recompile mkancil'
   ierr = 1
endif

if (iastart1_nitem > max_nitem) then
   write(*,*)'ERROR: iastart1_nitem = ',iastart1_nitem, &
             ' > max_nitem = ',max_nitem
   write(*,*)'Increase max_nitem and recompile mkancil'
   ierr = 1
endif

if (iostart_nncfiles > max_nncfiles) then
   write(*,*)'ERROR: iostart_nncfiles = ',iostart_nncfiles, &
             ' > max_nncfiles = ',max_nncfiles
   write(*,*)'Increase max_nncfiles and recompile mkancil'
   ierr = 1
endif

if (iostart_nitem > max_nitem) then
   write(*,*)'ERROR: iostart_nitem = ',iostart_nitem, &
             ' > max_nitem = ',max_nitem
   write(*,*)'Increase max_nitem and recompile mkancil'
   ierr = 1
endif

if (ierr > 0) stop

! Calculate vertical grid level values

if (iavert == 1) then
   if (iversion < 500) then
      call calc_vert_od(nlev,nam_vert)
      lvertrev_od = .not. llevstoreup
   else
      call calc_vert_nd(nlev,nam_vert)
      lvertrev_nd = .not. llevstoreup
   endif
else if (iavert == 2) then

   ! Get Atmosphere level values from specified netcdf file

   call get_nclevels_atmos(avert_filein,avert_ncname,nlev,version,ltheta)
   if (.not. lozonelev) no3lev = nlev
endif

!if (iavert == 1 .or. iavert == 2) then
!   if (iversion < 500) then
!      write(*,995)'ak','bk','akh','bkh'
!      do k=1,nlev+1
!        if (k == nlev+1) then
!           write(*,996)k,akh(k),bkh(k)
!        else
!           write(*,997)k,ak(k),bk(k),akh(k),bkh(k)
!        endif
!      enddo
!   else
!      write(*,995)'zsea_theta','zsea_rho','c_theta','c_rho'
!      do k=0,nlev+1
!        if (k == 0) then
!           write(*,999)k,zsea_theta(k),c_theta(k)
!        else if (k == nlev+1) then
!           write(*,998)k,zsea_rho(k),c_rho(k)
!        else
!           write(*,997)k,zsea_theta(k),zsea_rho(k),c_theta(k),c_rho(k)
!        endif
!      enddo
!   endif
!endif
!999 format(I2,1X,F16.10,18X,F16.10)
!998 format(I2,18X,F16.10,18X,F16.10)
!997 format(I2,1X,F16.10,1X,F16.10,1X,F16.10,1X,F16.10)
!996 format(I2,35X,F16.10,1X,F16.10)
!995 format(3X,A16,1X,A16,1X,A16,1X,A16)

if (iovert == 2) then

   ! Get Ocean level values from specified netcdf file

   call get_nclevels_ocean(overt_filein,overt_ncname,noclev)

endif

! Check soil levels are defined if needed

if (.not. ldeepsoil) then
   if (lslt) then
      write(*,*)'ERROR: cannot create deep soil temperature file, ', &
                'deep soils levels not defined'
      ierr = 1
   endif
   if (lsmow) then
      write(*,*)'ERROR: cannot create smow file, ', &
                'deep soils levels not defined'
      ierr = 1
   endif
   do i=1,nancfiles
      if (lgenanc_file(i)) then
         do j=1,igenanc_nfield(i)
            if (igenanc_levtype(j,i) == 1) then
               write(*,*)'ERROR: cannot create generalised ancillary file ',i, &
                         'deep soils levels not defined'
               ierr = 1
               exit
            endif
         enddo
      endif
   enddo
endif

if (ierr > 0) stop

! Get land fraction if required

if (llfrac .or. llfrac_use) then
   call get_lfrac(lfrac_filein,lfrac_ncname)
   lnclfrac = .true.
else
   lnclfrac = .false.
endif

! Get land/sea mask if required

if (lmask .or. lmask_use) then
   call get_mask(mask_filein,mask_ncname,lmask_uselfrac, &
                 lmask_usemdi,lmask_sea,amask_val,imask_val)
   lncmask = .true.
else
   lncmask = .false.
endif

! Check land/sea mask and land fraction are consistent

if (lncmask .and. lnclfrac) call chk_lfrac_mask()

! Create ancillary files

if (lmask) &
   call create_mask_ancil(mask_filein,mask_fileout,mask_ncofname,lmask_outflow)

if (llfrac) &
   call create_lfrac_ancil(lfrac_fileout)

if (lozone) &
   call create_ozone_ancil(ozone_filein,ozone_fileout,ozone_ncname, &
                           aozone_mrfac,lozone_mmr,lozone_periodic, &
                           iozone_timeusage1,iozone_timeusage2, &
                           iozone_startdate,iozone_ntimes, &
                           iozone_interval,iozone_intunit,lozone_mm)

if (lsmow) &
   call create_smow_ancil(smow_filein,smow_fileout,smow_ncsnowdepthname, &
                          smow_ncsnowedgename,smow_ncsoilmoisturename, &
                          lsmow_calcsnowedge,lsmow_periodic,ismow_mask, &
                          ismow_timeusage1,ismow_timeusage2, &
                          ismow_startdate,ismow_ntimes, &
                          ismow_interval,ismow_intunit,lsmow_mm)

if (lslt) &
   call create_slt_ancil(slt_filein,slt_fileout,slt_ncname, &
                         lslt_periodic,islt_mask, &
                         islt_timeusage1,islt_timeusage2, &
                         islt_startdate,islt_ntimes, &
                         islt_interval,islt_intunit,lslt_mm)

if (lsoil) &
   call create_soil_ancil(soil_filein,soil_fileout, &
                          soil_ncvsmcwiltname,soil_ncvsmccritname, &
                          soil_ncvsmcfcapname,soil_ncvsmcsatname, &
                          soil_ncclapphornname,soil_ncthermcondname, &
                          soil_ncsoilcondname,soil_ncthermcapname, &
                          soil_ncsoilwatersucname,soil_ncsoilalbname, &
                          soil_ncsoilcarbname, &
                          isoil_mask)

if (lveg) &
   call create_veg_ancil(veg_filein,veg_fileout, &
                         veg_ncrootdepthname,veg_ncsfaname, &
                         veg_ncsurfresistname,veg_ncz0name, &
                         veg_nccancapname,veg_ncvegfracname, &
                         veg_ncinfiltname,veg_ncdsaname, &
                         veg_nclainame,veg_nccanhtname, &
                         iveg_mask)

if (lvegfrac) &
   call create_vegfrac_ancil(vegfrac_filein,vegfrac_fileout,vegfrac_ncname, &
                             ivegfrac_mask,ivegfrac_nsurftypes)

if (lvegfunc) &
   call create_vegfunc_ancil(vegfunc_filein,vegfunc_fileout, &
                             vegfunc_nclainame,vegfunc_nccanhtname, &
                             vegfunc_nccancondname,lvegfunc_periodic, &
                             ivegfunc_mask,ivegfunc_nfunctypes, &
                             ivegfunc_timeusage1,ivegfunc_timeusage2, &
                             ivegfunc_startdate,ivegfunc_ntimes, &
                             ivegfunc_interval,ivegfunc_intunit,lvegfunc_mm)

if (lvegdist) &
   call create_vegdist_ancil(vegdist_filein,vegdist_fileout,vegdist_ncname, &
                             ivegdist_mask)

if (lsst .or. lice) &
   call create_sstice_ancil(lsst,sst_filein,sst_fileout,sst_ncname, &
                            lsst_min,asst_min,lsst_iceval,asst_iceval, &
                            lsst_periodic,isst_mask, &
                            isst_timeusage1,isst_timeusage2, &
                            isst_startdate,isst_ntimes, &
                            isst_interval,isst_intunit,lsst_mm, &
                            lice,ice_filein,ice_fileout, &
                            ice_ncfracname,ice_ncdepthname,ice_ncedgename, &
                            lice_amip2,lice_calcdepth,lice_calcedge, &
                            lice_sstval,aice_sstval, &
                            lice_percent,lice_mkmask,aice_cutoff, &
                            lice_min,aice_min,lice_max,aice_max, &
                            lice_periodic,iice_mask, &
                            iice_timeusage1,iice_timeusage2, &
                            iice_startdate,iice_ntimes, &
                            iice_interval,iice_intunit,lice_mm)

if (lorog) &
   call create_orog_ancil(orog_filein,orog_fileout, &
                          orog_ncname,orog_ncsdname, &
                          orog_ncxgradname,orog_ncygradname, &
                          orog_ncxxgradname,orog_ncxygradname, &
                          orog_ncyygradname, &
                          orog_ncsilname,orog_ncpthtname, &
                          orog_ncunfiltname, &
                          lorog_incgrad,lorog_incsqgrad, &
                          lorog_incrough,lorog_incunfilt,iorog_mask)

if (lausrmulti) then
   write(*,*)'Writing Multi-level User ancillary file ', &
             trim(ausrmulti_fileout)

   iusrancil_levtype = 3
   iusrancil_nlev = nlev

   call create_genanc(ausrmulti_fileout,ausrmulti_ncname, &
                    lausrmulti_periodic, &
                    1,-1,iausrmulti_nfield, &
                    iausrmulti_stashcode,iausrmulti_ppcode, &
                    iusrancil_levtype,iusrancil_nlev,lausrmulti_theta, &
                    iausrmulti_gridtype,iausrmulti_datatype, &
                    iausrmulti_masktype,iausrmulti_mask,iausrmulti_fileinid, &
                    iausrmulti_timeusage1,iausrmulti_timeusage2, &
                    iausrmulti_startdate,iausrmulti_ntimes, &
                    iausrmulti_interval,iausrmulti_intunit,lausrmulti_mm)
endif

if (lausrancil) then
   write(*,*)'Writing Single-level User ancillary file ', &
             trim(ausrancil_fileout)

   iusrancil_levtype = 0
   iusrancil_nlev = 1
   lusrancil_theta = .true.

   call create_genanc(ausrancil_fileout,ausrancil_ncname, &
                    lausrancil_periodic, &
                    1,-1,iausrancil_nfield, &
                    iausrancil_stashcode,iausrancil_ppcode, &
                    iusrancil_levtype,iusrancil_nlev,lusrancil_theta, &
                    iausrancil_gridtype,iausrancil_datatype, &
                    iausrancil_masktype,iausrancil_mask,iausrancil_fileinid, &
                    iausrancil_timeusage1,iausrancil_timeusage2, &
                    iausrancil_startdate,iausrancil_ntimes, &
                    iausrancil_interval,iausrancil_intunit,lausrancil_mm)
endif

if (lts1) &
   call create_ts1_ancil(ts1_filein,ts1_fileout, &
                         ts1_ncsstname,ts1_ncsssname, &
                         ts1_ncatname,ts1_ncidname, &
                         lts1_refsst,lts1_refsss,lts1_climat,lts1_climid, & 
                         lts1_periodic,its1_mask, &
                         its1_timeusage1,its1_timeusage2, &
                         its1_startdate,its1_ntimes, &
                         its1_interval,its1_intunit,lts1_mm)

if (lflux) &
   call create_flux_ancil(flux_filein,flux_fileout, &
                          flux_ncheatname,flux_ncsaltname, &
                          lflux_heat,lflux_salt,lflux_periodic,iflux_mask, &
                          iflux_timeusage1,iflux_timeusage2, &
                          iflux_startdate,iflux_ntimes, &
                          iflux_interval,iflux_intunit,lflux_mm)

if (lousrmulti) then
   write(*,*)'Writing Multi-level Ocean User ancillary file ', &
             trim(ousrmulti_fileout)

   iusrancil_levtype = 5
   iusrancil_nlev = noclev
   lusrancil_theta = .true.

   call create_genanc(ousrmulti_fileout,ousrmulti_ncname, &
                    lousrmulti_periodic, &
                    2,-1,iousrmulti_nfield, &
                    iousrmulti_stashcode,iousrmulti_ppcode, &
                    iusrancil_levtype,iusrancil_nlev,lusrancil_theta, &
                    iousrmulti_gridtype,iousrmulti_datatype, &
                    iousrmulti_masktype,iousrmulti_mask,iousrmulti_fileinid, &
                    iousrmulti_timeusage1,iousrmulti_timeusage2, &
                    iousrmulti_startdate,iousrmulti_ntimes, &
                    iousrmulti_interval,iousrmulti_intunit,lousrmulti_mm)
endif

if (lousrancil) then
   write(*,*)'Writing Ocean Single-level User ancillary file ', &
             trim(ousrancil_fileout)

   iusrancil_levtype = 0
   iusrancil_nlev = 1
   lusrancil_theta = .true.

   call create_genanc(ousrancil_fileout,ousrancil_ncname, &
                    lousrancil_periodic, &
                    2,-1,iousrancil_nfield, &
                    iousrancil_stashcode,iousrancil_ppcode, &
                    iusrancil_levtype,iusrancil_nlev,lusrancil_theta, &
                    iousrancil_gridtype,iousrancil_datatype, &
                    iousrancil_masktype,iousrancil_mask,iousrancil_fileinid, &
                    iousrancil_timeusage1,iousrancil_timeusage2, &
                    iousrancil_startdate,iousrancil_ntimes, &
                    iousrancil_interval,iousrancil_intunit,lousrancil_mm)
endif

do ifile=1,nancfiles
   if (lgenanc_file(ifile)) then
      write(*,*)'Writing Generalised ancillary file ',ifile,' ', &
                 trim(genanc_fileout(ifile))

      call create_genanc(genanc_fileout(ifile), &
                         genanc_ncname(:,ifile), &
                         lgenanc_periodic(ifile), &
                         igenanc_model(ifile), &
                         igenanc_inthd8(ifile), &
                         igenanc_nfield(ifile), &
                         igenanc_stashcode(:,ifile), &
                         igenanc_ppcode(:,ifile), &
                         igenanc_levtype(:,ifile), &
                         igenanc_nlev(:,ifile), &
                         lgenanc_theta(:,ifile), &
                         igenanc_gridtype(:,ifile), &
                         igenanc_datatype(:,ifile), &
                         igenanc_masktype(:,ifile), &
                         igenanc_mask(:,ifile), &
                         igenanc_fileinid(:,ifile), &
                         igenanc_timeusage1(ifile), &
                         igenanc_timeusage2(ifile), &
                         igenanc_startdate(:,ifile), &
                         igenanc_ntimes(ifile), &
                         igenanc_interval(ifile), &
                         igenanc_intunit(ifile), &
                         lgenanc_mm(ifile))
   endif
enddo

if (lastart .and. lastart1) &
  call create_astart1(astart_filein,astart_fileout, &
                      astart1_umfile,astart1_ncname, &
                      lastart_newlsm,lastart1_usestdname,lastart1_useconfig, &
                      iastart_nncfiles,iastart1_nitem, &
                      iastart1_ncfileid,iastart1_itemid, &
                      iastart_timeusage1,iastart_timeusage2, &
                      iastart_startdate)

! if (lastart .and. lastart2) &
!   call create_astart2()

if (lostart) &
  call create_ostart(ostart_filein,ostart_fileout,ostart_umfile,ostart_ncname, &
                     lostart_usestdname,lostart_useconfig, &
                     iostart_nncfiles,iostart_nitem, &
                     iostart_ncfileid,iostart_itemid, &
                     iostart_timeusage1,iostart_timeusage2,iostart_startdate, &
                     lostart_bathy,ostart_bathy_filein,ostart_bathy_ncname, &
                     lostart_islands_replace,lostart_islands_add, &
                     ostart_islands_filein)

write(*,*)'End of mkancil'
write(*,*)

if (lnclfrac) then
   call free_gridinfo(lfrac_grid)
   deallocate (alfrac)
endif
if (lncmask) then
   call free_gridinfo(mask_grid)
   deallocate (mask)
endif
if (iversion < 500) then
   if (allocated(ak)) deallocate (ak)
   if (allocated(bk)) deallocate (bk)
   if (allocated(akh)) deallocate (akh)
   if (allocated(bkh)) deallocate (bkh)
else
   if (allocated(zsea_theta)) deallocate (zsea_theta)
   if (allocated(zsea_rho)) deallocate (zsea_rho)
   if (allocated(c_theta)) deallocate (c_theta)
   if (allocated(c_rho)) deallocate (c_rho)
endif
if (allocated(depth)) deallocate(depth)

stop
end

!==============================================================================!

subroutine open_ancil(chan,file,mode)

use getkind
use config

implicit none

integer(ptype) chan
character(*) file,mode

character(3) type

type(1:1) = 'I'
if (lbigend .eqv. lbigendout) then
   type(2:2) = 'E'
else
   type(2:2) = 'S'
endif
if (isize == 32) then
   type(3:3) = '4'
else
   type(3:3) = '8'
endif

!write(*,*)'Opening file ',trim(file),' mode = ',mode,' type = ',type
call openff(chan,file,mode,type)

return
end

!==============================================================================!

subroutine close_ancil(chan)

use getkind

implicit none

integer(ptype) chan

call closeff(chan)

return
end

!==============================================================================!

subroutine get_diminfo(ncname,ncid,dimnames,dim,nx,ny,nz,nt)

use parameters

implicit none

character(*) ncname,dimnames(4)
integer ncid,dim(4),nx,ny,nz,nt

integer ndims,nd(4),dim1(4),i
character(max_varname_size) dimnames1(4)

nx = 1
ny = 1
nz = 1
nt = 1
dim1 = -1
dim = -1
dimnames = ""

! get dimension values

call get_ncdiminfo(ncname,ncid,nd,dimnames1,ndims)

call getdimid(ncname,ncid,dimnames1,dim1,ndims)

do i=1,ndims
   dim(dim1(i)) = i
   dimnames(dim1(i)) = dimnames1(i)
enddo

if (dim(1) > 0) nx = nd(dim(1))
if (dim(2) > 0) ny = nd(dim(2))
if (dim(3) > 0) nz = nd(dim(3))
if (dim(4) > 0) nt = nd(dim(4))

return
end

!==============================================================================!

subroutine get_gridinfo(ncname,ncid,dimnames,dim,nz,nt,model,type,grid)

use getkind
use parameters
use config
use types

implicit none

character(*) ncname,dimnames(4)
integer ncid,dim(4),nz,nt,model,type
type(gridinfo) grid

integer ndims,nd(4),dim1(4),i,ix,nx,ny,nextra
character(max_varname_size) dimnames1(4)
logical islatrev,lnorthsouth

nx = 1
ny = 1
nz = 1
nt = 1
dim1 = -1
dim = -1
dimnames = ""

if (iversion < 500 .and. model /= 2) then
   lnorthsouth = .true.
else
   lnorthsouth = .false.
endif

! get dimension values

call get_ncdiminfo(ncname,ncid,nd,dimnames1,ndims)

call getdimid(ncname,ncid,dimnames1,dim1,ndims)

do i=1,ndims
   dim(dim1(i)) = i
   dimnames(dim1(i)) = dimnames1(i)
enddo

if (dim(1) > 0) nx = nd(dim(1))
if (dim(2) > 0) ny = nd(dim(2))
if (dim(3) > 0) nz = nd(dim(3))
if (dim(4) > 0) nt = nd(dim(4))

! Set values of grid structure

grid%model = model
grid%type = type

if (model == 2) then
   allocate(grid%along(nx+2))
else
   allocate(grid%along(nx))
endif
grid%lalloc_long = .true.

allocate(grid%alat(ny))
grid%lalloc_lat = .true.

if (dimnames(1) == "") then
   grid%along(1) = 0.0_rtype
else
   call get_ncdim(dimnames(1),ncid,grid%along)
endif
if (dimnames(2) == "") then
   grid%alat(1) = 0.0_rtype
else
   call get_ncdim(dimnames(2),ncid,grid%alat)
   grid%llatrev = islatrev(grid%alat,ny,lnorthsouth)
endif

! Check if a rotated grid

call get_north_pole(ncname,ncid,grid%north_pole,grid%lrotate)

grid%lglobal = .not. grid%lrotate

if (grid%lglobal) then
   nextra = 0
   do ix = 1,nx
      if ((grid%along(ix)-grid%along(1)) >= 360.0_rtype) &
          nextra = nextra + 1
   enddo
   nx = nx - nextra
!   write(*,*)'nextra = ',nextra

   grid%offset_long = 0
   do ix = 1,nx
      if (grid%along(ix) >= 0.0_rtype) exit
      grid%offset_long = grid%offset_long + 1
   enddo
   if (grid%offset_long > 0) then
      grid%along(1:nx) = cshift(grid%along(1:nx),grid%offset_long)
      where (grid%along(1:nx) < 0.0_rtype) &
         grid%along(1:nx) = grid%along(1:nx) + 360.0_rtype
   endif

   if (grid%model == 2) then ! Ocean field with 2 extra wrap-around points
      grid%along(nx+1) = grid%along(1) + 360.0_rtype
      grid%along(nx+2) = grid%along(2) + 360.0_rtype
      nx = nx+2
   endif
else
   grid%offset_long = 0
   if (grid%along(1) < 0.0_rtype) grid%along = grid%along + 360.0_rtype
endif

grid%nlong = nx
grid%nlat = ny

!write(*,*)'grid%model = ',grid%model
!write(*,*)'grid%type = ',grid%type
!write(*,*)'grid%lrotate = ',grid%lrotate
!write(*,*)'grid%lglobal = ',grid%lglobal
!write(*,*)'grid%north_pole(1) = ',grid%north_pole(1)
!write(*,*)'grid%north_pole(2) = ',grid%north_pole(2)
!write(*,*)'grid%offset_long = ',grid%offset_long
!write(*,*)'grid%nlong = ',grid%nlong
!write(*,*)'grid%nlat = ',grid%nlat
!write(*,*)'grid%along = ',grid%along

return
end

!==============================================================================!

subroutine copy_gridinfo(grid1,grid2)

use types

implicit none

type(gridinfo) grid1,grid2

!if (allocated(grid1%along)) then
if (grid1%lalloc_long) then
   !if (.not. allocated(grid2%along)) then
   if (.not. grid2%lalloc_long) then
      allocate(grid2%along(grid1%nlong))
   elseif (size(grid2%along) /= grid1%nlong) then
      deallocate(grid2%along)
      allocate(grid2%along(grid1%nlong))
   endif
endif

!if (allocated(grid1%alat)) then
if (grid1%lalloc_lat) then
   !if (.not. allocated(grid2%alat)) then
   if (.not. grid2%lalloc_lat) then
      allocate(grid2%alat(grid1%nlat))
   elseif (size(grid2%alat) /= grid1%nlat) then
      deallocate(grid2%alat)
      allocate(grid2%alat(grid1%nlat))
   endif
endif

grid2%lalloc_long = grid1%lalloc_long
grid2%lalloc_lat = grid1%lalloc_lat
grid2%along = grid1%along
grid2%alat = grid1%alat
grid2%north_pole = grid1%north_pole
grid2%model = grid1%model
grid2%nlong = grid1%nlong
grid2%nlat = grid1%nlat
grid2%offset_long = grid1%offset_long
grid2%llatrev = grid1%llatrev
grid2%lglobal = grid1%lglobal
grid2%lrotate = grid1%lrotate

return
end

!==============================================================================!

subroutine free_gridinfo(grid)

use types

implicit none

type(gridinfo) grid

!if (allocated(grid%along)) deallocate(grid%along)
if (grid%lalloc_long) then
   deallocate(grid%along)
   grid%lalloc_long = .false.
endif

!if (allocated(grid%alat )) deallocate(grid%alat)
if (grid%lalloc_lat) then
   deallocate(grid%alat)
   grid%lalloc_lat = .false.
endif

return
end

!==============================================================================!

logical function islatrev(alat,ny,lnorthsouth)

use getkind

implicit none

integer ny
real(rtype) alat(ny)
logical lnorthsouth

real dum
integer i
logical llatrev

llatrev = .false.
if (ny > 1) then
   if (lnorthsouth .neqv. alat(2) < alat(1)) then
      llatrev = .true.
      do i=1,ny/2
         dum = alat(i)
         alat(i) = alat(ny-i+1)
         alat(ny-i+1) = dum
      enddo
   endif
endif

islatrev = llatrev

return
end

!==============================================================================!

subroutine get_north_pole(name,ncid,north_pole,lrotate)

use getkind
use parameters

implicit none

character(*) name
integer ncid
real(rtype) north_pole(2)
logical lrotate

character(max_attname_size) grid_mapping,grid_mapping_name
logical lattexists

call get_ncatt_real(name,ncid,'north_pole',north_pole,lrotate)

if (.not. lrotate) then
   call get_ncatt_text(name,ncid,'grid_mapping',grid_mapping,lattexists)
   if (lattexists) then
      call get_ncatt_text(grid_mapping,ncid,'grid_mapping_name', &
                          grid_mapping_name,lattexists)
      if (grid_mapping_name(1:26) == 'rotated_latitude_longitude') then
         lrotate = .true.
         call get_ncatt_real(grid_mapping,ncid,'grid_north_pole_longitude', &
                             north_pole(1),lattexists)
         call get_ncatt_real(grid_mapping,ncid,'grid_north_pole_latitude', &
                             north_pole(2),lattexists)
      endif
   endif
endif

if (.not. lrotate) then
   north_pole(1) = 0.0_rtype
   north_pole(2) = 90.0_rtype
endif

if (lrotate .and. north_pole(1) == 0.0_rtype .and. &
                  north_pole(2) == 90.0_rtype) lrotate = .false.

!write(*,*)'lrotate = ',lrotate
!write(*,*)'north_pole = ',north_pole

return
end

!==============================================================================!

subroutine calc_vert_od(nlev,nam_vert)

use getkind
use constants
use parameters
use vert_od

implicit none

integer nlev
character(*) nam_vert

integer meth_lev_calc, min_prs_hlev, max_sig_hlev

namelist /vertical/ meth_lev_calc, min_prs_hlev, max_sig_hlev, etah

real(rtype), parameter :: tiny = 1.0e-8_rtype

integer ichan, k, ierr
real(rtype) etal(nlev), ah(nlev+1), al(nlev)
real(rtype) eta_s, eta_p
real(rtype) z1, z2, z
logical lexist

ichan = 12

if (nlev > max_nlev) then
   write(*,*)'ERROR: Number of vertical levels (',nlev,') has exceeded ', &
             'maximum allowed (',max_nlev,').'
   stop
endif

inquire(file=nam_vert,exist=lexist)
if (.not. lexist) then
   write(*,*)'ERROR: vertical namelist file ',trim(nam_vert),' does not exist'
   stop
endif

open(ichan,file=nam_vert,iostat=ierr)
if (ierr /= 0) then
   write(*,*)'ERROR: opening vertical namelist file ',trim(nam_vert), &
             ' failed, iostat = ',ierr
   stop
endif

read(ichan,vertical)
!write(*,vertical)

close(ichan)

if (meth_lev_calc /= 4 .and. meth_lev_calc /= 5 .and. meth_lev_calc /= 9) then
   write(*,*)'ERROR: meth_lev_calc = ',meth_lev_calc, &
             ' must = 4,5 or 9'
   stop
endif

if (meth_lev_calc == 9 .and. (nlev /= 19 .and. nlev /= 31)) then
   write(*,*)'ERROR: for meth_lev_calc = 9 number of levels must = 19 or 31'
   stop
endif

allocate (ak(nlev))
allocate (bk(nlev))
allocate (akh(nlev+1))
allocate (bkh(nlev+1))

if (meth_lev_calc == 9) then
   if (nlev ==19) call get_ecmwf_levels_19(ak,bk,akh,bkh)
   if (nlev ==31) call get_ecmwf_levels_31(ak,bk,akh,bkh)
   return
endif

eta_s=etah(max_sig_hlev)
eta_p=etah(min_prs_hlev)

if (eta_s < eta_p) then
   write(*,*)'ERROR: eta_s < eta_p ',eta_s,eta_p
   stop
endif

! Calculate layer centre value of eta

if (meth_lev_calc == 4) then
   do k=1,nlev
      z1=etah(k)**kappa
      z2=etah(k+1)**kappa
      z=0.5_rtype*(z1+z2)
      etal(k) = z**(1.0_rtype/kappa)
   enddo
else if (meth_lev_calc == 5) then
   do k=1,nlev
      z1=etah(k)**(kappa+1.0_rtype)
      z2=etah(k+1)**(kappa+1.0_rtype)
      z=(z2-z1)/((etah(k+1)-etah(k))*(kappa+1.0_rtype))
      etal(k) = z**(1.0_rtype/kappa)
   enddo
endif

! Calculate hybrid coordinate parameter ah

z1=(eta_s-eta_p)*(eta_s-eta_p)*(eta_s-eta_p)
do k=1,nlev+1
   if(etah(k).ge.eta_s) then
      ah(k)=0.0_rtype
   else if(etah(k).le.eta_p) then
      ah(k)=etah(k)
   else
      z2=etah(k)*(eta_s+eta_p)-2.0_rtype*eta_p*eta_p
      ah(k)=(eta_s-etah(k))*(eta_s-etah(k))*z2/z1
      if(abs(ah(k)).lt.tiny) ah(k)=0.0_rtype
   end if
end do

! Calculate a values at levels

do k=1,nlev
   al(k)=ah(k)+(ah(k+1)-ah(k))*(etal(k)-etah(k))/(etah(k+1)-etah(k))
   if(abs(al(k)).lt.tiny) al(k)=0.0_rtype
end do

! Set a and b arrays

do k=1,nlev
   ak(k)=pref*al(k)
   bk(k)=etal(k)-al(k)
   if(abs(bk(k)).lt.tiny) bk(k)=0.0_rtype
end do
do k=1,nlev+1
   akh(k)=pref*ah(k)
   bkh(k)=etah(k)-ah(k)
   if(abs(bkh(k)).lt.tiny) bkh(k)=0.0_rtype
end do

return
end

!==============================================================================!

subroutine calc_vert_nd(nlev,nam_vert)

use getkind
use vert_nd

implicit none

integer nlev
character(*) nam_vert

real(rtype) z_top_of_model
integer first_constant_r_rho_level

namelist /vertlevs/ z_top_of_model, first_constant_r_rho_level, &
                    eta_theta, eta_rho

real(rtype) cc
integer ichan, kc, k, ierr
logical lexist

ichan = 12

if (nlev > max_nlev) then
   write(*,*)'ERROR: Number of vertical levels (',nlev,') has exceeded ', &
             'maximum allowed (',max_nlev,').'
   stop
endif

inquire(file=nam_vert,exist=lexist)
if (.not. lexist) then
   write(*,*)'ERROR: vertical namelist file ',trim(nam_vert),' does not exist'
   stop
endif

open(ichan,file=nam_vert,iostat=ierr)
if (ierr /= 0) then
   write(*,*)'ERROR: opening vertical namelist file ',trim(nam_vert), &
             ' failed, iostat = ',ierr
   stop
endif

read(ichan,vertlevs)
!write(*,vertlevs)

close(ichan)

allocate (zsea_theta(0:nlev))
allocate (zsea_rho(nlev+1))
allocate (c_theta(0:nlev))
allocate (c_rho(nlev+1))

kc = first_constant_r_rho_level

zsea_theta(0) = 0.0_rtype
c_theta(0) = 1.0_rtype
do k=1,nlev
   zsea_theta(k) = eta_theta(k)*z_top_of_model
   zsea_rho(k) = eta_rho(k)*z_top_of_model
   if (k >= kc) then
      c_theta(k) = 0.0_rtype
      c_rho(k) = 0.0_rtype
   else
      cc = (1.0_rtype - (eta_theta(k)/eta_rho(kc)))
      c_theta(k) = cc*cc
      cc = (1.0_rtype - (eta_rho(k)/eta_rho(kc)))
      c_rho(k) = cc*cc
   endif
enddo
zsea_rho(nlev+1) = 2.0_rtype*zsea_theta(nlev) - zsea_rho(nlev)
c_rho(nlev+1) = 2.0_rtype*c_theta(nlev) - c_rho(nlev)

return
end

!==============================================================================!

subroutine get_nclevels_atmos(ncfile,name,nlev,version,ltheta)

use getkind
use constants
use parameters
use vert_od
use vert_nd
use utils

implicit none

real(rtype) version
character(*) ncfile,name
integer nlev
logical ltheta

integer ncid,ierr,ilev
logical lattexists,lpresent,lscale
character(max_attname_size) formula_terms
character(max_varname_size) a,b,ah,bh,p0var
character(max_stdname_size) stdname
real(rtype), dimension(:,:), allocatable :: abnd, bbnd
real(rtype) p0,eta1,eta2

lscale = .false.

call open_ncfile(ncfile, 'r', ncid, ierr)

! Check that standard_name name exists for this variable

call get_ncatt_text(name,ncid,'standard_name',stdname,lattexists)

if (.not. lattexists) then
   write(*,*)'ERROR: variable ',trim(name), &
             ' from NetCDF file ',trim(ncfile), &
             ' doesn''t contain the standard_name attribute needed to', &
             ' calculate vertical level values.'
   stop
!else
!   write(*,*)'standard_name = ',trim2(stdname)
endif

! Check that the standard_name name is correct for this UM version

if (trim2(stdname) == 'atmosphere_hybrid_sigma_pressure_coordinate') then
   if (version < 5.0) then
      write(*,*)'Getting atmosphere hybrid sigma pressure levels from ', &
                'netcdf file ',trim(ncfile),' using variable ',trim(name)
   else
      write(*,*)'ERROR: variable ',trim(name), &
                ' from NetCDF file ',trim(ncfile), &
                ' doesn''t contain the correct level coordinates for this', &
                ' version of the model (',version,').'
      write(*,*)trim(name),' contains data for level type ',trim2(stdname)
      stop
   endif
else if (trim2(stdname) == 'atmosphere_hybrid_height_coordinate') then
   if (version < 5.0) then
      write(*,*)'ERROR: variable ',trim(name), &
                ' from NetCDF file ',trim(ncfile), &
                ' doesn''t contain the correct level coordinates for this', &
                ' version of the model (',version,').'
      write(*,*)trim(name),' contains data for level type ',trim2(stdname)
      stop
   else
      write(*,*)'Getting atmosphere hybrid height levels from ', &
                'netcdf file ',trim(ncfile),' using variable ',trim(name)
   endif
else
   write(*,*)'ERROR: variable ',trim(name), &
               ' from NetCDF file ',trim(ncfile), &
               ' doesn''t contain the correct level coordinates.'
   write(*,*)trim(name),' contains data for level type ',trim2(stdname)
   stop
endif

! Use formula_terms attribute to find variables with level data

call get_ncatt_text(name,ncid,'formula_terms',formula_terms,lattexists)
if (.not. lattexists) then
   write(*,*)'ERROR: variable ',trim(name), &
             ' from NetCDF file ',trim(ncfile), &
             ' doesn''t contain the formula_terms attribute needed to', &
             ' calculate vertical level values.'
   stop
!else
!   write(*,*)'formula_terms = ',trim2(formula_terms)
endif

ierr = 0
if (version < 5.0) then
   call getvar_formula_terms(formula_terms, 'ap', a, lpresent)
   if (.not. lpresent) then
      call getvar_formula_terms(formula_terms, 'a', a, lpresent)
      if (.not. lpresent) then
         write(*,*)'ERROR: formula_terms attribute ',trim2(formula_terms), &
                   ' for variable ',trim(name), &
                   ' from NetCDF file ',trim(ncfile), &
                   ' doesn''t contain the a or ap coordinate needed to', &
                   ' calculate vertical level values.'
         ierr = 1
      else
         !write(*,*)'2 a = ',trim(a)
         lscale = .true.
      endif
   !else
   !   write(*,*)'1 a = ',trim(a)
   endif
   call getvar_formula_terms(formula_terms, 'p0', p0var, lpresent)
   if (lpresent) then
      !write(*,*)'1 p0var = ',trim(p0var)
      call get_ncvar_r(p0var, ncid, p0)
   else
      p0 = pref
   endif
   !write(*,*)'p0 = ',p0
else
   call getvar_formula_terms(formula_terms, 'a', a, lpresent)
   if (.not. lpresent) then
      write(*,*)'ERROR: formula_terms attribute ',trim2(formula_terms), &
                ' for variable ',trim(name), &
                ' from NetCDF file ',trim(ncfile), &
                ' doesn''t contain the a coordinate needed to', &
                ' calculate vertical level values.'
      ierr = 2
   !else
   !   write(*,*)'a = ',trim(a)
   endif
endif

call getvar_formula_terms(formula_terms, 'b', b, lpresent)
if (.not. lpresent) then
   write(*,*)'ERROR: formula_terms attribute ',trim2(formula_terms), &
             ' for variable ',trim(name), &
             ' from NetCDF file ',trim(ncfile), &
             ' doesn''t contain the b coordinate needed to', &
             ' calculate vertical level values.'
   ierr = 3
!else
!   write(*,*)'b = ',trim(b)
endif
if (ierr > 0) stop

! Allocate vertical level boundary arrays and get data

call get_ncdimlen(name,ncid,nlev)
!write(*,*)'Number of atmos levels = ',nlev

allocate(abnd(2,nlev))
allocate(bbnd(2,nlev))

call get_ncatt_text(a,ncid,'bounds',ah,lattexists)
if (lattexists) then
   !write(*,*)'ah = ',trim2(ah)
   call get_ncvar_r(ah, ncid, abnd)
   !write(*,*)'abnd = ',abnd
else
   write(*,*)'WARNING: variable ',trim(a), &
             ' from NetCDF file ',trim(ncfile), &
             ' doesn''t contain the bounds attribute needed to', &
             ' calculate half level values.'
   write(*,*)'Setting half level values to zero.'
   abnd = 0.0_rtype
endif

call get_ncatt_text(b,ncid,'bounds',bh,lattexists)
if (lattexists) then
   !write(*,*)'bh = ',trim2(bh)
   call get_ncvar_r(bh, ncid, bbnd)
   !write(*,*)'bbnd = ',bbnd
else
   write(*,*)'WARNING: variable ',trim(b), &
             ' from NetCDF file ',trim(ncfile), &
             ' doesn''t contain the bounds attribute needed to', &
             ' calculate half level values.'
   write(*,*)'Setting half level values to zero.'
   bbnd = 0.0_rtype
endif

! Allocate vertical level arrays and get data

if (version < 5.0) then
   call allocate_array_1d(ak,1,nlev)
   call allocate_array_1d(bk,1,nlev)
   call allocate_array_1d(akh,1,nlev+1)
   call allocate_array_1d(bkh,1,nlev+1)

   call get_ncdim(a,ncid,ak)
   if (lscale) ak = ak*p0

   call get_ncdim(b,ncid,bk)

   lvertrev_od = .false.
   if (nlev > 1) then
      eta1 = ak(1)/p0+bk(1)
      eta2 = ak(2)/p0+bk(2)
      !write(*,*)'eta1,eta2 = ',eta1,eta2
      if (eta1 < eta2) then
         lvertrev_od = .true.
         call rev_array(ak,nlev)
         call rev_array(bk,nlev)
      endif
   endif

   if (lvertrev_od) then
      ilev = 1
   else
      ilev = nlev
   endif
   if (abnd(1,ilev) > abnd(2,ilev)) then
      akh(1:nlev) = abnd(1,1:nlev)
      akh(nlev+1) = abnd(2,nlev)
   else
      akh(1:nlev) = abnd(2,1:nlev)
      akh(nlev+1) = abnd(1,nlev)
   endif
   if (lscale) akh = akh*p0

   if (lvertrev_od) then
      ilev = nlev
   else
      ilev = 1
   endif
   if (bbnd(1,ilev) > bbnd(2,ilev)) then
      bkh(1:nlev) = bbnd(1,1:nlev)
      bkh(nlev+1) = bbnd(2,nlev)
   else
      bkh(1:nlev) = bbnd(2,1:nlev)
      bkh(nlev+1) = bbnd(1,nlev)
   endif

   if (lvertrev_od) then
      call rev_array(akh,nlev+1)
      call rev_array(bkh,nlev+1)
   endif
else
   call allocate_array_1d(zsea_theta,0,nlev)
   call allocate_array_1d(zsea_rho,1,nlev+1)
   call allocate_array_1d(c_theta,0,nlev)
   call allocate_array_1d(c_rho,1,nlev+1)

   lvertrev_nd = .false.
   if (nlev > 1) then
      call get_ncdim1(a,ncid,1,eta1)
      call get_ncdim1(a,ncid,2,eta2)
      !write(*,*)'eta1,eta2 = ',eta1,eta2
      if (eta1 > eta2) then
         lvertrev_nd = .true.
      endif
   endif
   if (lvertrev_nd) then
      ilev = nlev
   else
      ilev = 1
   endif

   if (ltheta) then
      if (abnd(2,ilev) > abnd(1,ilev)) then
         zsea_rho(1:nlev) = abnd(1,1:nlev)
         zsea_rho(nlev+1) = abnd(2,nlev)
      else
         zsea_rho(1:nlev) = abnd(2,1:nlev)
         zsea_rho(nlev+1) = abnd(1,nlev)
      endif

      if (bbnd(1,ilev) > bbnd(2,ilev)) then
         c_rho(1:nlev) = bbnd(1,1:nlev)
         c_rho(nlev+1) = bbnd(2,nlev)
      else
         c_rho(1:nlev) = bbnd(2,1:nlev)
         c_rho(nlev+1) = bbnd(1,nlev)
      endif

      if (lvertrev_nd) then
         call get_ncdim(a,ncid,zsea_theta(0))
         zsea_theta(nlev) = 0.0_rtype

         call get_ncdim(b,ncid,c_theta(0))
         c_theta(nlev) = 1.0_rtype
      else
         zsea_theta(0) = 0.0_rtype
         call get_ncdim(a,ncid,zsea_theta(1))

         c_theta(0) = 1.0_rtype
         call get_ncdim(b,ncid,c_theta(1))
      endif
   else
      if (abnd(2,ilev) > abnd(1,ilev)) then
         zsea_theta(0) = abnd(1,1)
         zsea_theta(1:nlev) = abnd(2,1:nlev)
      else
         zsea_theta(0) = abnd(2,1)
         zsea_theta(1:nlev) = abnd(1,1:nlev)
      endif

      if (bbnd(1,ilev) > bbnd(2,ilev)) then
         c_theta(0) = bbnd(1,1)
         c_theta(1:nlev) = bbnd(2,1:nlev)
      else
         c_theta(0) = bbnd(2,1)
         c_theta(1:nlev) = bbnd(1,1:nlev)
      endif

      if (lvertrev_nd) then
         call get_ncdim(a,ncid,zsea_rho(2))
         zsea_rho(1) = 2.0_rtype*zsea_theta(0) - zsea_rho(2)

         call get_ncdim(b,ncid,c_rho(2))
         c_rho(1) = 2.0_rtype*c_theta(0) - c_rho(2)
      else
         call get_ncdim(a,ncid,zsea_rho(1))
         zsea_rho(nlev+1) = 2.0_rtype*zsea_theta(nlev) - zsea_rho(nlev)

         call get_ncdim(b,ncid,c_rho(1))
         c_rho(nlev+1) = 2.0_rtype*c_theta(nlev) - c_rho(nlev)
      endif
   endif

   if (lvertrev_nd) then
      call rev_array(zsea_theta(0),nlev+1)
      call rev_array(zsea_rho(1),nlev+1)
      call rev_array(c_theta(0),nlev+1)
      call rev_array(c_rho(1),nlev+1)
   endif

!  Calculate eta_theta and eta_rho

   if (zsea_theta(nlev) > 0.0_rtype) then
      eta_theta(0:nlev) = zsea_theta(0:nlev)/zsea_theta(nlev)
      eta_rho(1:nlev) = zsea_rho(1:nlev)/zsea_theta(nlev)
   else
      eta_theta = 0.0_rtype
      eta_rho = 0.0_rtype
   endif
endif

call close_ncfile(ncid,ierr)

deallocate(abnd,bbnd)

return
end

!==============================================================================!

subroutine get_nclevels_ocean(ncfile,name,nlev)

use getkind
use parameters
use vert_ocean
use utils

implicit none

character(*) ncfile,name
integer nlev

integer ncid,ierr,i
logical lattexists,ldepthneg
character(max_varname_size) bndsvar
real(rtype), dimension(:,:), allocatable :: depth_bnd

write(*,*)'Getting ocean depth levels from netcdf file ', &
          trim(ncfile),' using varname ',trim(name)

call open_ncfile(ncfile, 'r', ncid, ierr)

call get_ncdimlen(name, ncid, nlev)

call allocate_array_1d(depth,1,nlev)
call allocate_array_1d(thickness,1,nlev)

call get_ncdim(name, ncid, depth)

!write(*,*)'1 depth = ',depth

ldepthneg = .false.
if (depth(1) <= 0 .and. depth(nlev) <= 0) then
   ldepthneg = .true.
   depth = -depth
   !write(*,*)'2 depth = ',depth
endif

ldepthrev = .false.
if (nlev > 1) then
   if (depth(1) > depth(2)) then
      ldepthrev = .true.
      call rev_array(depth,nlev)
      !write(*,*)'3 depth = ',depth
   endif
endif

call get_ncatt_text(name,ncid,'bounds',bndsvar,lattexists)
if (lattexists) then
   allocate(depth_bnd(2,nlev))
   call get_ncvar_r(bndsvar, ncid, depth_bnd)
   !write(*,*)'1 depth_bnd = ',depth_bnd
   if (ldepthneg) then
      depth_bnd = -depth_bnd
      !write(*,*)'2 depth_bnd = ',depth_bnd
   endif
   if (depth_bnd(2,1) > depth_bnd(1,1)) then
      thickness = depth_bnd(2,:) - depth_bnd(1,:)
   else
      thickness = depth_bnd(1,:) - depth_bnd(2,:)
   endif
   !write(*,*)'1 thickness = ',thickness
   if (ldepthrev) then
      call rev_array(thickness,nlev)
      !write(*,*)'2 thickness = ',thickness
   endif
   deallocate(depth_bnd)
else
   thickness(1) = depth(2)-depth(1)
   do i=2,nlev
      thickness(i) = 2.0_rtype*(depth(i) - depth(i-1)) - thickness(i-1)
   enddo
   !write(*,*)'3 thickness = ',thickness
endif

call close_ncfile(ncid,ierr)

return
end

!==============================================================================!

subroutine getvar_formula_terms(formula_terms, term, varname, lpresent)

use utils

implicit none

character(*) formula_terms,term,varname
logical lpresent

integer ipos1,ipos2

ipos1 = index(formula_terms, term//': ')

if (ipos1 == 0) then
   lpresent = .false.
else
   lpresent = .true.

   ipos1 = ipos1 + len(term) + 2
   do while (formula_terms(ipos1:ipos1) == ' ')
      ipos1 = ipos1+1
   enddo

   ipos2 = ipos1
   do while (ipos2 <= len(formula_terms) .and. &
             formula_terms(ipos2:ipos2) /= ' ' .and. &
             formula_terms(ipos2:ipos2) /= char(0))
      ipos2 = ipos2+1
   enddo
   ipos2 = ipos2-1

   varname = formula_terms(ipos1:ipos2)
endif

return
end

!==============================================================================!
!
! Subroutine calculates hemisphere of data point ix,iy
!
!            ihem =  1 - Northern hemisphere
!            ihem = -1 - Southern hemisphere
!
!==============================================================================!

subroutine gethemisphere(grid,ix,iy,ihem)

use getkind
use types

implicit none

type(gridinfo) grid
integer ix,iy,ihem

real(rtype) x(1,1),y(1,1)

if (grid%lrotate) then

!  Get unrotated latitude value

   x(1,1) = grid%along(ix)
   y(1,1) = grid%alat(iy)
   call unrotate(x,y,1,1,1,grid%north_pole(1),grid%north_pole(2))
   if (y(1,1) < 0) then
      ihem = -1
   else
      ihem = 1
   endif

else
   if (grid%alat(iy) < 0) then
      ihem = -1
   else
      ihem = 1
   endif
endif

return
end

!==============================================================================!
!
!     Calculate unrotated grid coordinates from rotated grid
!
!==============================================================================!

subroutine unrotate(x,y,nx,ny,ndim,xpole,ypole)

use getkind
use constants

implicit none

integer nx,ny,ndim
real(rtype) x(ndim,ny),y(ndim,ny)
real(rtype) xpole,ypole

real(rtype) tol,x0,sinypole,cosypole,xx,yy,arg,ax,ay,t1,t2
integer jx,jy

tol=1.0e-6_rtype
x0 = mod(xpole+180.0_rtype, 360.0_rtype)   
if (ypole .ge. 0.0_rtype) then
   sinypole=sin(pi*ypole/180.0_rtype)
   cosypole=cos(pi*ypole/180.0_rtype)
else
   sinypole=-sin(pi*ypole/180.0_rtype)
   cosypole=-cos(pi*ypole/180.0_rtype)
endif

do jy=1,ny
   do jx=1,nx
!
!     Scale x between -180 and 180
!
      xx=mod(x(jx,jy),360.0_rtype)
      if (xx.gt.180.0_rtype) xx=xx-360.0_rtype
      if (xx.lt.-180.0_rtype) xx=xx+360.0_rtype
      xx=pi*xx/180.0_rtype
      yy=pi*y(jx,jy)/180.0_rtype

      arg=cosypole*cos(xx)*cos(yy)+sin(yy)*sinypole
      arg=min(arg,1.0_rtype)
      arg=max(arg,-1.0_rtype)
      ay=asin(arg)
      y(jx,jy)=180.0_rtype*ay/pi

      t1=cos(yy)*cos(xx)*sinypole-sin(yy)*cosypole
      t2=cos(ay)
      if (t2.lt.tol) then
         ax=0.0_rtype
      else
         arg=t1/t2
         arg=min(arg,1.0_rtype)
         arg=max(arg,-1.0_rtype)
         ax=180.0_rtype*acos(arg)/pi
         ax=sign(ax,xx)
         ax=ax+x0
      endif
!
!     Scale x between 0 and 360
!
      if (ax.ge.360.0_rtype) ax=ax-360.0_rtype
      if (ax.lt.0.0_rtype) ax=ax+360.0_rtype
      x(jx,jy)=ax

   enddo
enddo

return
end

!==============================================================================!

subroutine getfractime(grid,data1,data2,fractime,nx,ny)

use getkind
use constants
use types

implicit none

integer nx,ny
type(gridinfo) grid
real(rtype) data1(nx,ny),data2(nx,ny),fractime(nx,ny)

real(rtype) a
integer ix,iy,iy1,iy2,iyy,iyyy,iy1y2,is,jy,jy1,jy2,jyy,jyyy
integer itrend,ihem
integer type(nx,ny)

! Slightly different algorithm depending on whether data is north-south
! or south-north

if (ny > 1) then
   if (grid%alat(1) > grid%alat(2)) then
      is = 1   ! north-south
   else
      is = -1  ! south-north
   endif
endif

! Set transition indicators  1= zero -> non zero
!                           -1= non zero -> zero
!                            0= no transition

do iy=1,ny
   do ix=1,nx
      if (is == 1) then
         jy = iy
      else
         jy = ny-iy+1
      endif

      if (data1(ix,jy) == 0.0_rtype .and. data2(ix,jy) > 0.0_rtype) then
         type(ix,jy) = 1
      elseif (data2(ix,jy) == 0.0_rtype .and. data1(ix,jy) > 0.0_rtype) then
         type(ix,jy) = -1
      else
         type(ix,jy) = 0
      endif

! Initialise fractional time to missing data indicator or 0.5 if transition

     if (type(ix,jy) /= 0) then
        fractime(ix,jy) = 0.5_rtype
     else
        fractime(ix,jy) = rmdi
     endif
   enddo
enddo

! Search by longitude for groups where transition occurs

do ix=1,nx
   do iy=1,ny-1
      if (is == 1) then
         jy = iy
      else
         jy = ny-iy+1
      endif

      if (type(ix,jy) == 0.0_rtype .and. type(ix,jy+is) /= 0.0_rtype) then
        iy1=iy+1
        iy2=iy+1
        jy1=jy+is
        jy2=jy+is
        do iyy=iy1+1,ny-1
           if (is == 1) then
              jyy = iyy
           else
              jyy = ny-iyy+1
           endif

           if (type(ix,jyy) == type(ix,jy1)) then
              iy2=iy2+1
              jy2=jy2+is
           elseif (type(ix,jyy) == 0) then
              a = 1.0_rtype/real(iy2-iy1+2,rtype)

!             Compute transition indicators

              iy1y2=(iy1+iy2)/2
              call gethemisphere(grid,ix,iy1y2,ihem)

              if ((data1(ix,jy1-is) == 0.0_rtype .and. &
                   data1(ix,jy2+is) == 0.0_rtype) .or. &
                  (data1(ix,jy1-is) > 0.0_rtype .and. &
                   data1(ix,jy2+is) > 0.0_rtype)) then

!                No transition

                 itrend = 0
              elseif (type(ix,jy1) == 1) then

!                Transition is zero to non-zero

                 itrend = -is*ihem
              elseif (type(ix,jy1) == -1) then

!                Transition is non-zero to zero

                 itrend = is*ihem
              endif

!             itrend indicates how fractime varies with latitude

              if (itrend == 1) then
                 do iyyy=iy1,iy2
                    if (is == 1) then
                       jyyy = iyyy
                    else
                       jyyy = ny-iyyy+1
                    endif
                    fractime(ix,jyyy)=a*(iy2-iyyy+1)
                 enddo
              elseif (itrend == -1) then
                 do iyyy=iy1,iy2
                    if (is == 1) then
                       jyyy = iyyy
                    else
                       jyyy = ny-iyyy+1
                    endif
                    fractime(ix,jyyy)=a*(iyyy-iy1+1)
                 enddo
              endif
              exit
           endif
        enddo
      endif
   enddo
enddo

return
end

!==============================================================================!

subroutine getdimid(varname,ncid,dimnames,dim,ndims)

use getkind
use parameters
use utils

implicit none

integer ncid, ndims
integer dim(ndims)
character(*) varname, dimnames(ndims)

integer idim,i,j,k,l,ii
character(5) att
character(2) dimaxis
character(5) varaxis(ndims)
character(max_varname_size) dimunit,ldimname
logical lvaraxis,ldimaxis,ldimunit,lfound

dim = -1

do idim = 1, ndims

!  Get dimension axis attribute

   att='axis'
   call get_ncatt_text(dimnames(idim), ncid, att, dimaxis, ldimaxis)

!  Check dimension axis attribute

   if (ldimaxis) then
      if (dimaxis(1:1) == 'X' .or. dimaxis(1:1) == 'x') then
         dim(idim) = 1
      else if (dimaxis(1:1) == 'Y' .or. dimaxis(1:1) == 'y') then
         dim(idim) = 2
      else if (dimaxis(1:1) == 'Z' .or. dimaxis(1:1) == 'z') then
         dim(idim) = 3
      else if (dimaxis(1:1) == 'T' .or. dimaxis(1:1) == 't') then
         dim(idim) = 4
      endif

      if (dim(idim) > 0) cycle
   endif

!  Get variable axis attribute

   if (idim == 1) then
      att='axis'
      call get_ncatt_text(varname, ncid, att, varaxis, lvaraxis)
   endif

!  Check variable axis attribute

   if (lvaraxis) then
      ii = ndims-idim+1
      if (varaxis(ii)(1:1) == 'X' .or. varaxis(ii) == 'x') then
         dim(idim) = 1
      else if (varaxis(ii)(1:1) == 'Y' .or. varaxis(ii)(1:1) == 'y') then
         dim(idim) = 2
      else if (varaxis(ii)(1:1) == 'Z' .or. varaxis(ii)(1:1) == 'z') then
         dim(idim) = 3
      else if (varaxis(ii)(1:1) == 'T' .or. varaxis(ii)(1:1) == 't') then
         dim(idim) = 4
      endif

      if (dim(idim) > 0) cycle
   endif

!  Get dimension unit attribute

   dimunit = ""
   att='units'
   call get_ncatt_text(dimnames(idim), ncid, att, dimunit, ldimunit)
   if (ldimunit) call locase(dimunit)

!  Make dimnames lower case

   ldimname = dimnames(idim)
   call locase(ldimname)

!  Use variable name or dimension unit name to get dim id

   if (ldimname == 'x' .or. index(ldimname, 'lon') /= 0) then
      dim(idim) = 1
   elseif (ldimname == 'y' .or. index(ldimname, 'lat') /= 0) then
      dim(idim) = 2
   elseif (ldimname == 'z' .or. index(ldimname, 'surf') /= 0 .or. &
           index(ldimname, 'msl') /= 0 .or. index(ldimname, 'toa') /= 0 .or. &
           index(ldimname, 'depth') /= 0 .or. index(ldimname, 'ht') /= 0 .or. &
           index(ldimname, 'hybrid') /= 0 .or. index(ldimname, 'eta') /= 0 .or. &
           index(ldimname, 'sigma') /= 0 .or. index(ldimname, 'press') /= 0 .or. &
           index(ldimname, 'theta') /= 0 .or. index(ldimname, 'poten') /= 0 .or. &
           index(ldimname, 'level') /= 0 .or. index(dimunit, 'pa') /= 0 .or. &
           index(dimunit, 'layer') /= 0 .or. index(dimunit, 'lev') /= 0 .or. &
           index(dimunit, 'metre') /= 0 .or. index(dimunit, 'meter') /= 0 .or. &
           index(dimunit, 'kelv') /= 0 .or. index(dimunit, 'bar') /= 0)  then
      dim(idim) = 3
   elseif (ldimname == 't' .or. index(ldimname, 'time') /= 0 .or. &
           index(ldimname, 'date') /= 0 .or. index(ldimname, 'day') /= 0 .or. &
           index(ldimname, 'yr') /= 0 .or. index(ldimname, 'year') /= 0 .or. &
           index(ldimname, 'mon') /= 0 .or. index(ldimname, 'mth') /= 0 .or. &
           index(ldimname, 'hr') /= 0 .or. index(ldimname, 'hour') /= 0 .or. &
           index(ldimname, 'min') /= 0 .or. index(ldimname, 'sec') /= 0 .or. &
           index(dimunit, 'yr') /= 0 .or. index(dimunit, 'year') /= 0 .or. &
           index(dimunit, 'mon') /= 0 .or. index(dimunit, 'mth') /= 0 .or. &
           index(dimunit, 'hr') /= 0 .or. index(dimunit, 'hour') /= 0 .or. &
           index(dimunit, 'min') /= 0 .or. index(dimunit, 'sec') /= 0 .or. &
           index(dimunit, 'day') /= 0) then
      dim(idim) = 4
   else
      dim(idim) = idim
   endif
enddo

! Make sure no dimension has been assigned twice

do i = 1,ndims
   do j = 1,ndims
      if (j == i) cycle
      if (dim(j) == dim(i)) then
         do k = 1,ndims
            lfound = .true.
            do l = 1,ndims
               if (k == dim(l)) then
                  lfound = .false.
                  exit
               endif
            enddo
            if (lfound) then
               dim(j) = k
               exit
            endif
         enddo
      endif
   enddo
enddo

return
end

!==============================================================================!

subroutine setdates(itimeusage1,itimeusage2,lmm,interval,intunit1,intunit2, &
                    ical,iscale,ntimes, &
                    istartdate1,istartdate2,time,nt1,itsel,idate,nt2)

use getkind

implicit none

integer itimeusage1,itimeusage2,interval,intunit1,intunit2
integer ical,iscale,ntimes,nt1,nt2
integer istartdate1(6),istartdate2(6),idate(6,nt2),itsel(nt2)
logical lmm
real(rtype) time(nt1)

integer i,j,ncdate1(6),ncdate2(6),intunit3
integer(i64) incr,interval64
logical cmp_dates

! For NetCDF files with time units days,hours,minutes convert times to
! seconds so you have integer value

if (intunit2 == 2 .or. intunit2 == 3 .or. intunit2 == 4) then
   intunit3 = 5
else
   intunit3 = intunit2
endif

interval64 = interval

if (itimeusage1 == 0) then ! Use dates from NetCDF file

   itsel(1) = 1
   incr = nint(time(1)*iscale,i64)
   call incr_date(intunit3,ical,istartdate2,idate(1,1),incr,.false.)

   do i=2,nt2
      itsel(i) = i
      incr = nint((time(i)-time(i-1))*iscale,i64)
      call incr_date(intunit3,ical,idate(1,i-1),idate(1,i),incr,.false.)
   enddo

   ! work out value of interval

   if (nt2 > 1) call diff_dates(idate(1,1),idate(1,2),ical,intunit2,interval)

else if (itimeusage1 == 1) then ! Use dates from namelist

   if (itimeusage2 == 0) then ! Override NetCDF dates with namelist dates

      itsel(1) = 1
      idate(1,1) = istartdate1(1) ! year
      idate(2,1) = istartdate1(2) ! month
      if (lmm .and. intunit1 == 1) then
         call get_midmon(idate(1,1), idate(2,1), idate(3,1), idate(4,1), ical)
         idate(5,1) = 0 ! minute
         idate(6,1) = 0 ! second
      else
         idate(3,1) = istartdate1(3) ! day
         idate(4,1) = istartdate1(4) ! hour
         idate(5,1) = istartdate1(5) ! minute
         idate(6,1) = istartdate1(6) ! second
      endif

      do i=2,nt2
         itsel(i) = i
         call incr_date(intunit1,ical,idate(1,i-1),idate(1,i),interval64,lmm)
      enddo

   else if (itimeusage2 == 1) then ! Extract namelist dates from NetCDF file

      idate(1,1) = istartdate1(1) ! year
      idate(2,1) = istartdate1(2) ! month
      if (lmm .and. intunit1 == 1) then
         call get_midmon(idate(1,1), idate(2,1), idate(3,1), idate(4,1), ical)
         idate(5,1) = 0 ! minute
         idate(6,1) = 0 ! second
      else
         idate(3,1) = istartdate1(3) ! day
         idate(4,1) = istartdate1(4) ! hour
         idate(5,1) = istartdate1(5) ! minute
         idate(6,1) = istartdate1(6) ! second
      endif

      itsel(1) = -1
      incr = nint(time(1)*iscale,i64)
      call incr_date(intunit3,ical,istartdate2,ncdate1,incr,.false.)
      if (cmp_dates(ncdate1,idate(1,1))) then
         itsel(1) = 1
      else
         do j=2,nt1
            incr = nint((time(j)-time(j-1))*iscale,i64)
            call incr_date(intunit3,ical,ncdate1,ncdate2,incr,.false.)
            ncdate1 = ncdate2
            if (cmp_dates(ncdate1,idate(1,1))) then
               itsel(1) = j
               exit
            endif
         enddo
      endif
      if (itsel(1) == -1) then
         write(*,*) 'date ',(idate(j,1),j=1,6),' not in NetCDF file'
         stop
      endif

      do i=2,nt2
         call incr_date(intunit1,ical,idate(1,i-1),idate(1,i),interval64,lmm)

         itsel(i) = -1
         incr = nint(time(1)*iscale,i64)
         call incr_date(intunit3,ical,istartdate2,ncdate1,incr,.false.)
         if (cmp_dates(ncdate1,idate(1,i))) then
            itsel(i) = 1
         else
            do j=2,nt1
               incr = nint((time(j)-time(j-1))*iscale,i64)
               call incr_date(intunit3,ical,ncdate1,ncdate2,incr,.false.)
               ncdate1 = ncdate2
               if (cmp_dates(ncdate1,idate(1,i))) then
                  itsel(i) = j
                  exit
               endif
            enddo
         endif
         if (itsel(i) == -1) then
            write(*,*) 'date ',(idate(j,i),j=1,6),' not in NetCDF file'
            stop
         endif
      enddo

   else
      write(*,*)'ERROR: itimeusage2 = ',itimeusage2,' unknown value'
      stop
   endif

endif

return
end

!==============================================================================!

subroutine setdumpdate(itimeusage1,itimeusage2,intunit1,ical,iscale, &
                       istartdate1,istartdate2,time,nt,itsel,idate)

use getkind

implicit none

integer itimeusage1, itimeusage2
integer ical, intunit1, iscale, nt
integer istartdate1(6), istartdate2(6), idate(6), itsel
real(rtype) time(nt)

integer j, ncdate1(6), ncdate2(6), intunit2
integer(i64) incr
logical cmp_dates

! For NetCDF files with time units days,hours,minutes convert times to
! seconds so you have integer value

if (intunit1 == 2 .or. intunit1 == 3 .or. intunit1 == 4) then
   intunit2 = 5
else
   intunit2 = intunit1
endif

if (itimeusage1 == 0) then ! Use date from NetCDF file

   itsel = 1
   incr = nint(time(1)*iscale,i64)
   call incr_date(intunit2,ical,istartdate2,idate,incr,.false.)

else if (itimeusage1 == 1) then ! Use dates from namelist

   if (itimeusage2 == 0) then ! Override NetCDF dates with namelist dates

      itsel = 1
      idate = istartdate1

   else if (itimeusage2 == 1) then ! Extract namelist dates from NetCDF file

      idate = istartdate1

      itsel = -1
      incr = nint(time(1)*iscale,i64)
      call incr_date(intunit2,ical,istartdate2,ncdate1,incr,.false.)
      if (cmp_dates(ncdate1,idate)) then
         itsel = 1
      else
         do j=2,nt
            incr = nint((time(j)-time(j-1))*iscale,i64)
            call incr_date(intunit2,ical,ncdate1,ncdate2,incr,.false.)
            ncdate1 = ncdate2
            if (cmp_dates(ncdate1,idate)) then
               itsel = j
               exit
            endif
         enddo
      endif
      if (itsel == -1) then
         write(*,*) 'date ',idate,' not in NetCDF file'
         stop
      endif
   else
      write(*,*)'ERROR: itimeusage2 = ',itimeusage2,' unknown value'
      stop
   endif

endif

return
end

!==============================================================================!

subroutine get_midmon(iyear,imon,iday,ihour,ical)

implicit none

integer iyear,imon,iday,ihour,ical

logical isleap

if (ical == 2) then
   iday = 16
   ihour = 0
else
   if (imon == 2 .and. isleap(iyear)) then
      iday = 15
      ihour = 12
   elseif (imon == 2) then
      iday = 15
      ihour = 0
   elseif (imon == 4 .or. imon == 6 .or. imon == 9 .or. imon == 11) then
      iday = 16
      ihour = 0
   else
      iday = 16
      ihour = 12
   endif
endif

return
end

!==============================================================================!
!
! Increase idate0 by incr amount, to get idate1
!
!==============================================================================!

subroutine incr_date(intunit,ical,idate0,idate1,incr,lmm)

use getkind

implicit none

integer intunit, ical
integer(i64) incr
integer idate0(6), idate1(6)
logical lmm

integer(i64) idate1_64(6)

if (intunit == 0) then ! date increment in years
   idate1(6) = idate0(6)
   idate1(5) = idate0(5)
   idate1(4) = idate0(4)
   idate1(3) = idate0(3)
   idate1(2) = idate0(2)
   idate1(1) = idate0(1) + incr
else if (intunit == 1) then ! date increment in months
   idate1(2) = idate0(2) + incr
   idate1(1) = idate0(1) + (idate1(2)-1)/12
   idate1(2) = mod(idate1(2)-1,12) + 1
   if (idate1(2) < 0) then
      idate1(2) = idate1(2) + 12
      idate1(1) = idate1(1) - 1
   endif
   if (lmm) then
      call get_midmon(idate1(1),idate1(2),idate1(3),idate1(4),ical)
      idate1(5) = 0
      idate1(6) = 0
   else
      idate1(3) = idate0(3)
      idate1(4) = idate0(4)
      idate1(5) = idate0(5)
      idate1(6) = idate0(6)
   endif
else if (intunit == 2) then ! date increment in days
   idate1(6) = idate0(6)
   idate1(5) = idate0(5)
   idate1(4) = idate0(4)
   idate1(3) = idate0(3) + incr
   idate1(2) = idate0(2)
   idate1(1) = idate0(1)
   call update_date(idate1(1),idate1(2),idate1(3),ical)
else if (intunit == 3) then ! date increment in hours
   idate1_64(6) = idate0(6)
   idate1_64(5) = idate0(5)
   idate1_64(4) = idate0(4) + incr
   idate1_64(3) = idate0(3) + idate1_64(4)/24
   idate1_64(4) = mod(idate1_64(4),24_i64)
   if (idate1_64(4) < 0) then
      idate1_64(4) = idate1_64(4) + 24
      idate1_64(3) = idate1_64(3) - 1
   endif
   idate1 = idate1_64
   idate1(2) = idate0(2)
   idate1(1) = idate0(1)
   call update_date(idate1(1),idate1(2),idate1(3),ical)
else if (intunit == 4) then ! date increment in minutes
   idate1_64(6) = idate0(6)
   idate1_64(5) = idate0(5) + incr
   idate1_64(4) = idate0(4) + idate1_64(5)/60
   idate1_64(5) = mod(idate1_64(5),60_i64)
   if (idate1_64(5) < 0) then
      idate1_64(5) = idate1_64(5) + 60
      idate1_64(4) = idate1_64(4) - 1
   endif
   idate1_64(3) = idate0(3) + idate1_64(4)/24
   idate1_64(4) = mod(idate1_64(4),24_i64)
   if (idate1_64(4) < 0) then
      idate1_64(4) = idate1_64(4) + 24
      idate1_64(3) = idate1_64(3) - 1
   endif
   idate1 = idate1_64
   idate1(2) = idate0(2)
   idate1(1) = idate0(1)
   call update_date(idate1(1),idate1(2),idate1(3),ical)
else if (intunit == 5) then ! date increment in seconds
   idate1_64(6) = idate0(6) + incr
   idate1_64(5) = idate0(5) + idate1_64(6)/60
   idate1_64(6) = mod(idate1_64(6),60_i64)
   if (idate1_64(6) < 0) then
      idate1_64(6) = idate1_64(6) + 60
      idate1_64(5) = idate1_64(5) - 1
   endif
   idate1_64(4) = idate0(4) + idate1_64(5)/60
   idate1_64(5) = mod(idate1_64(5),60_i64)
   if (idate1_64(5) < 0) then
      idate1_64(5) = idate1_64(5) + 60
      idate1_64(4) = idate1_64(4) - 1
   endif
   idate1_64(3) = idate0(3) + idate1_64(4)/24
   idate1_64(4) = mod(idate1_64(4),24_i64)
   if (idate1_64(4) < 0) then
      idate1_64(4) = idate1_64(4) + 24
      idate1_64(3) = idate1_64(3) - 1
   endif
   idate1 = idate1_64
   idate1(2) = idate0(2)
   idate1(1) = idate0(1)
   call update_date(idate1(1),idate1(2),idate1(3),ical)
endif

return
end

!==============================================================================!
!
! Update month and year given too large value of day of month 
!
!==============================================================================!

subroutine update_date(iyr,imon,iday,ical)

implicit none

integer iyr, imon, iday, ical

logical isleap,lneg
integer nday
integer daytab(12)
save daytab

data daytab /31,28,31,30,31,30,31,31,30,31,30,31/

if (ical == 1) then ! Gregorian calendar
   lneg = iday < 0
   do while (.true.)
      nday = daytab(imon)
      if (imon == 2 .and. isleap(iyr)) nday = nday + 1
      if (lneg) then
         if (iday > 0) exit
         iday = iday + nday
         imon = imon - 1
         if (imon == 0) then
            imon = 12
            iyr = iyr - 1
         endif
      else
         if (iday <= nday) exit
         iday = iday - nday
         imon = imon + 1
         if (imon == 13) then
            imon = 1
            iyr = iyr + 1
         endif
      endif
   enddo
else if (ical == 2) then ! 360 day calendar
   imon = imon + (iday-1)/30
   iday = mod(iday-1,30) + 1
   if (iday < 0) then
      iday = iday + 30
      imon = imon - 1
   endif
   iyr = iyr + (imon-1)/12
   imon = mod(imon-1,12) + 1
   if (imon < 0) then
      imon = imon + 12
      iyr = iyr - 1
   endif
endif

return
end

!==============================================================================!
!
! Work out if given year is a leap year
!
!==============================================================================!

logical function isleap(year)

implicit none

integer year

isleap = ((mod(year,4) == 0) .and. (mod(year,100) /= 0)) .or. &
          (mod(year,400) == 0)

return
end

!==============================================================================!
!
! Return true if dates are equal, false otherwise
!
!==============================================================================!

logical function cmp_dates(date1, date2)

implicit none

integer date1(6), date2(6)

integer i

cmp_dates = .true.

do i=1,6
   if (date1(i) /= date2(i)) then
      cmp_dates = .false.
      exit
   endif
enddo

return
end

!==============================================================================!

subroutine diff_dates(idate1,idate2,ical,intunit,interval)

implicit none

integer idate1(6),idate2(6),ical,intunit,interval

integer i,imon,nmon,iyr,iday1,iday2,day
logical isleap
real rday

integer daytab(12)
save daytab

data daytab /31,28,31,30,31,30,31,31,30,31,30,31/

!write(*,*)'idate1 = ',idate1(1),idate1(2),idate1(3), &
!                      idate1(4),idate1(5),idate1(6)
!write(*,*)'idate2 = ',idate2(1),idate2(2),idate2(3), &
!                      idate2(4),idate2(5),idate2(6)

if (idate1(2) == idate2(2) .and. idate1(3) == idate2(3) .and. &
    idate1(4) == idate2(4) .and. idate1(5) == idate2(5) .and. &
    idate1(6) == idate2(6)) then
   intunit = 0
   interval = idate2(1) - idate1(1)
else if (idate1(3) == idate2(3) .and. idate1(4) == idate2(4) .and. &
         idate1(5) == idate2(5) .and. idate1(6) == idate2(6)) then
   intunit = 1
   interval = (idate2(1) - idate1(1))*12 + idate2(2) - idate1(2)
else if ((idate1(3) == 15 .or. idate1(3) == 16) .and. &
         (idate2(3) == 15 .or. idate2(3) == 16) .and. & 
          idate1(2) /= idate2(2)) then
   intunit = 1
   interval = (idate2(1) - idate1(1))*12 + idate2(2) - idate1(2)
else
   if (ical == 2) then ! 360 day calendar
      interval = (idate2(1) - idate1(1))*360 + (idate2(2) - idate1(2))*30 + &
                  idate2(3) - idate1(3)
   else ! Gregorian calendar
      iday1 = idate1(3)
      nmon = idate1(2)
      iyr = idate1(1)
      do i=1,nmon-1
         iday1 = iday1 + daytab(i)
         if (i == 2 .and. isleap(iyr)) iday1 = iday1 + 1
      enddo

      iday2 = idate2(3)
      nmon = (idate2(1) - idate1(1))*12 + idate2(2)
      imon = 1
      do i=1,nmon-1
         iday2 = iday2 + daytab(imon)
         if (imon == 2 .and. isleap(iyr)) iday2 = iday2 + 1
         imon = imon + 1
         if (imon == 13) then
            imon = 1
            iyr = iyr + 1
         endif
      enddo
      interval = iday2 - iday1
   endif
   rday = interval + (idate2(4) - idate1(4))/24.0 + &
                     (idate2(5) - idate1(5))/24.0*60.0 + &
                     (idate2(6) - idate1(6))/24.0*60.0*60.0
   day = rday
   if (day > 27.9 .and. day < 31.1) then
      intunit = 1
      interval = 1
   endif
   if (idate1(4) == idate2(4) .and. idate1(5) == idate2(5) .and. &
       idate1(6) == idate2(6)) then
      intunit = 2
   elseif (idate1(5) == idate2(5) .and. idate1(6) == idate2(6)) then
      intunit = 3
      interval = interval*24 + idate2(4) - idate1(4)
   elseif (idate1(6) == idate2(6)) then
      intunit = 4
      interval = interval*24*60 + (idate2(4) - idate1(4))*60 + &
                                   idate2(5) - idate1(5)
   else
      intunit = 5
      interval = interval*24*60*60 + (idate2(4) - idate1(4))*60*60 + &
                 (idate2(5) - idate1(5))*60 + idate2(6) - idate1(6)
   endif
endif

return
end

!==============================================================================!

integer function getmon(month)

use utils

implicit none

character(*) month

call locase(month)

select case (month(1:3))
case ('jan')
   getmon = 1
case ('feb')
   getmon = 2
case ('mar')
   getmon = 3
case ('apr')
   getmon = 4
case ('may')
   getmon = 5
case ('jun')
   getmon = 6
case ('jul')
   getmon = 7
case ('aug')
   getmon = 8
case ('sep')
   getmon = 9
case ('oct')
   getmon = 10
case ('nov')
   getmon = 11
case ('dec')
   getmon = 12
case default
   write(*,*)'month = ',month,' is not valid setting getmon = 1'
   getmon = 1
end select

return
end

!==============================================================================!

subroutine get_ecmwf_levels_19(ak,bk,akh,bkh)

use getkind

implicit none

integer, parameter :: nlev = 19

real(rtype) ak(nlev),bk(nlev),akh(nlev+1),bkh(nlev+1)

ak(1)  = 0.000000000000000E+00_rtype
ak(2)  = 0.000000000000000E+00_rtype
ak(3)  = 0.392487113751509E+03_rtype
ak(4)  = 0.166974550550356E+04_rtype
ak(5)  = 0.384399124847531E+04_rtype
ak(6)  = 0.663586265090738E+04_rtype
ak(7)  = 0.962624283624707E+04_rtype
ak(8)  = 0.123735444809637E+05_rtype
ak(9)  = 0.144988573078699E+05_rtype
ak(10) = 0.157414181886933E+05_rtype
ak(11) = 0.159868415143402E+05_rtype
ak(12) = 0.152705515395222E+05_rtype
ak(13) = 0.137588048607413E+05_rtype
ak(14) = 0.117096408074937E+05_rtype
ak(15) = 0.941609875319582E+04_rtype
ak(16) = 0.713415603324707E+04_rtype
ak(17) = 0.499771256951568E+04_rtype
ak(18) = 0.295943321382081E+04_rtype
ak(19) = 0.879415620848072E+03_rtype

bk(1)  = 0.996138729444707E+00_rtype
bk(2)  = 0.982621745531320E+00_rtype
bk(3)  = 0.950815815205693E+00_rtype
bk(4)  = 0.892462793918780E+00_rtype
bk(5)  = 0.807826326862033E+00_rtype
bk(6)  = 0.702535447692988E+00_rtype
bk(7)  = 0.585035737519342E+00_rtype
bk(8)  = 0.464600227257370E+00_rtype
bk(9)  = 0.349852539849334E+00_rtype
bk(10) = 0.247756227536975E+00_rtype
bk(11) = 0.163015578135734E+00_rtype
bk(12) = 0.978437041408294E-01_rtype
bk(13) = 0.520470986482280E-01_rtype
bk(14) = 0.233798779820671E-01_rtype
bk(15) = 0.811952462582929E-02_rtype
bk(16) = 0.181678546141253E-02_rtype
bk(17) = 0.165255638412513E-03_rtype
bk(18) = 0.000000000000000E+00_rtype
bk(19) = 0.000000000000000E+00_rtype

akh(1)  = 0.000000000000000E+00_rtype
akh(2)  = 0.000000000000000E+00_rtype
akh(3)  = 0.000000000000000E+00_rtype
akh(4)  = 0.783196289062500E+03_rtype
akh(5)  = 0.254997290039063E+04_rtype
akh(6)  = 0.512514843750000E+04_rtype
akh(7)  = 0.812714843750000E+04_rtype
akh(8)  = 0.111015664062500E+05_rtype
akh(9)  = 0.136214609375000E+05_rtype
akh(10) = 0.153569218750000E+05_rtype
akh(11) = 0.161162304687500E+05_rtype
akh(12) = 0.158611171875000E+05_rtype
akh(13) = 0.146984882812500E+05_rtype
akh(14) = 0.128510937500000E+05_rtype
akh(15) = 0.106095078125000E+05_rtype
akh(16) = 0.826792578125000E+04_rtype
akh(17) = 0.604610937500000E+04_rtype
akh(18) = 0.400000000000000E+04_rtype
akh(19) = 0.200000000000000E+04_rtype
akh(20) = 0.500000081956387E+02_rtype

bkh(1)  = 0.100000000000000E+01_rtype
bkh(2)  = 0.992281496524811E+00_rtype
bkh(3)  = 0.972985208034515E+00_rtype
bkh(4)  = 0.928746879100800E+00_rtype
bkh(5)  = 0.856437504291534E+00_rtype
bkh(6)  = 0.759698271751404E+00_rtype
bkh(7)  = 0.646107852458954E+00_rtype
bkh(8)  = 0.524932146072388E+00_rtype
bkh(9)  = 0.405409157276154E+00_rtype
bkh(10) = 0.295519709587097E+00_rtype
bkh(11) = 0.201195538043976E+00_rtype
bkh(12) = 0.125916838645935E+00_rtype
bkh(13) = 0.706499814987183E-01_rtype
bkh(14) = 0.340772941708565E-01_rtype
bkh(15) = 0.130701400339603E-01_rtype
bkh(16) = 0.335722905583680E-02_rtype
bkh(17) = 0.339000718668103E-03_rtype
bkh(18) = 0.000000000000000E+00_rtype
bkh(19) = 0.000000000000000E+00_rtype
bkh(20) = 0.000000000000000E+00_rtype

return
end

!==============================================================================!

subroutine get_ecmwf_levels_31(ak,bk,akh,bkh)

use getkind

implicit none

integer, parameter :: nlev = 31

real(rtype) ak(nlev),bk(nlev),akh(nlev+1),bkh(nlev+1)

ak(1)  = 0.000000000000000E+00_rtype
ak(2)  = 0.000000000000000E+00_rtype
ak(3)  = 0.360945395225382E+02_rtype
ak(4)  = 0.172065962016737E+03_rtype
ak(5)  = 0.455366498322024E+03_rtype
ak(6)  = 0.917843715666848E+03_rtype
ak(7)  = 0.157658414817064E+04_rtype
ak(8)  = 0.243487770565203E+04_rtype
ak(9)  = 0.348330256158656E+04_rtype
ak(10) = 0.470093117355484E+04_rtype
ak(11) = 0.605665909044177E+04_rtype
ak(12) = 0.751065861164394E+04_rtype
ak(13) = 0.901595003589688E+04_rtype
ak(14) = 0.105200978215532E+05_rtype
ak(15) = 0.119670310001004E+05_rtype
ak(16) = 0.132989889450563E+05_rtype
ak(17) = 0.144585874032593E+05_rtype
ak(18) = 0.153910095163340E+05_rtype
ak(19) = 0.160463240649462E+05_rtype
ak(20) = 0.163819141916454E+05_rtype
ak(21) = 0.163650499091333E+05_rtype
ak(22) = 0.159755689391754E+05_rtype
ak(23) = 0.152086690530493E+05_rtype
ak(24) = 0.140778329696428E+05_rtype
ak(25) = 0.126178446562412E+05_rtype
ak(26) = 0.108879130339567E+05_rtype
ak(27) = 0.897487725912053E+04_rtype
ak(28) = 0.698292444751578E+04_rtype
ak(29) = 0.497600324894382E+04_rtype
ak(30) = 0.295943321382081E+04_rtype
ak(31) = 0.879415620848072E+03_rtype

bk(1)  = 0.996138729444707E+00_rtype
bk(2)  = 0.982621745531320E+00_rtype
bk(3)  = 0.958574149638797E+00_rtype
bk(4)  = 0.925924138660846E+00_rtype
bk(5)  = 0.886279752489540E+00_rtype
bk(6)  = 0.840990676094138E+00_rtype
bk(7)  = 0.791201680132733E+00_rtype
bk(8)  = 0.737902771477229E+00_rtype
bk(9)  = 0.681971468329504E+00_rtype
bk(10) = 0.624206260436122E+00_rtype
bk(11) = 0.565358422012556E+00_rtype
bk(12) = 0.506153009322551E+00_rtype
bk(13) = 0.447304636427745E+00_rtype
bk(14) = 0.389528580005730E+00_rtype
bk(15) = 0.333542474805819E+00_rtype
bk(16) = 0.280063961920927E+00_rtype
bk(17) = 0.229799981421672E+00_rtype
bk(18) = 0.183431266650151E+00_rtype
bk(19) = 0.141589262660035E+00_rtype
bk(20) = 0.104826762957438E+00_rtype
bk(21) = 0.735830915251995E-01_rtype
bk(22) = 0.481402343652541E-01_rtype
bk(23) = 0.285753243044531E-01_rtype
bk(24) = 0.147052501718621E-01_rtype
bk(25) = 0.602519524620794E-02_rtype
bk(26) = 0.164082635987922E-02_rtype
bk(27) = 0.192708137852118E-03_rtype
bk(28) = 0.000000000000000E+00_rtype
bk(29) = 0.000000000000000E+00_rtype
bk(30) = 0.000000000000000E+00_rtype
bk(31) = 0.000000000000000E+00_rtype
                                         
akh(1)  = 0.000000000000000E+00_rtype
akh(2)  = 0.000000000000000E+00_rtype
akh(3)  = 0.000000000000000E+00_rtype
akh(4)  = 0.720635833740234E+02_rtype
akh(5)  = 0.271626464843750E+03_rtype
akh(6)  = 0.638148925781250E+03_rtype
akh(7)  = 0.119588989257813E+04_rtype
akh(8)  = 0.195480517578125E+04_rtype
akh(9)  = 0.291156933593750E+04_rtype
akh(10) = 0.405071777343750E+04_rtype
akh(11) = 0.534591406250000E+04_rtype
akh(12) = 0.676133984375000E+04_rtype
akh(13) = 0.825321093750000E+04_rtype
akh(14) = 0.977140625000000E+04_rtype
akh(15) = 0.112612304687500E+05_rtype
akh(16) = 0.126652929687500E+05_rtype
akh(17) = 0.139255195312500E+05_rtype
akh(18) = 0.149852695312500E+05_rtype
akh(19) = 0.157915976562500E+05_rtype
akh(20) = 0.162976210937500E+05_rtype
akh(21) = 0.164650039062500E+05_rtype
akh(22) = 0.162666093750000E+05_rtype
akh(23) = 0.156892070312500E+05_rtype
akh(24) = 0.147363554687500E+05_rtype
akh(25) = 0.134313945312500E+05_rtype
akh(26) = 0.118205390625000E+05_rtype
akh(27) = 0.997613671875000E+04_rtype
akh(28) = 0.800000000000000E+04_rtype
akh(29) = 0.600000000000000E+04_rtype
akh(30) = 0.400000000000000E+04_rtype
akh(31) = 0.200000000000000E+04_rtype
akh(32) = 0.500000081956387E+02_rtype

bkh(1)  = 0.100000000000000E+01_rtype
bkh(2)  = 0.992281496524811E+00_rtype
bkh(3)  = 0.972985208034515E+00_rtype
bkh(4)  = 0.944213211536407E+00_rtype
bkh(5)  = 0.907715857028961E+00_rtype
bkh(6)  = 0.864955842494965E+00_rtype
bkh(7)  = 0.817166984081268E+00_rtype
bkh(8)  = 0.765405237674713E+00_rtype
bkh(9)  = 0.710594415664673E+00_rtype
bkh(10) = 0.653564572334290E+00_rtype
bkh(11) = 0.595084249973297E+00_rtype
bkh(12) = 0.535886585712433E+00_rtype
bkh(13) = 0.476688146591187E+00_rtype
bkh(14) = 0.418202280998230E+00_rtype
bkh(15) = 0.361145019531250E+00_rtype
bkh(16) = 0.306235373020172E+00_rtype
bkh(17) = 0.254188597202301E+00_rtype
bkh(18) = 0.205703258514404E+00_rtype
bkh(19) = 0.161441504955292E+00_rtype
bkh(20) = 0.122003614902496E+00_rtype
bkh(21) = 0.878949761390686E-01_rtype
bkh(22) = 0.594876408576965E-01_rtype
bkh(23) = 0.369748584926128E-01_rtype
bkh(24) = 0.203191563487053E-01_rtype
bkh(25) = 0.919413194060326E-02_rtype
bkh(26) = 0.291970069520175E-02_rtype
bkh(27) = 0.390858156606555E-03_rtype
bkh(28) = 0.000000000000000E+00_rtype
bkh(29) = 0.000000000000000E+00_rtype
bkh(30) = 0.000000000000000E+00_rtype
bkh(31) = 0.000000000000000E+00_rtype
bkh(32) = 0.000000000000000E+00_rtype

return
end

!==============================================================================!

subroutine comp_land_data(datain,land,ndata,dataout,nland)

use getkind

implicit none

integer ndata,nland
real(rtype) datain(ndata),dataout(nland)
logical land(ndata)

integer i,j

j=1
do i=1,ndata
   if (land(i)) then
      dataout(j) = datain(i)
      j=j+1
   endif
enddo

return
end

!==============================================================================!

subroutine comp_land_data32(datain,land,ndata,dataout,nland)

use getkind

implicit none

integer ndata,nland
integer(i32) datain(ndata),dataout(nland)
logical land(ndata)

integer i,j

j=1
do i=1,ndata
   if (land(i)) then
      dataout(j) = datain(i)
      j=j+1
   endif
enddo

return
end

!==============================================================================!

subroutine uncomp_land_data(datain,nland,rmdi,dataout,land,ndata)

use getkind

implicit none

integer ndata,nland
real(rtype) datain(nland),dataout(ndata),rmdi
logical land(ndata)

integer i,j

j=1
do i=1,ndata
   if (land(i)) then
      dataout(i) = datain(j)
      j=j+1
   else
      dataout(i) = rmdi
   endif
enddo

return
end

!==============================================================================!

subroutine uncomp_land_data32(datain,nland,irmdi,dataout,land,ndata)

use getkind

implicit none

integer ndata,nland
integer(i32) datain(nland),dataout(ndata),irmdi
logical land(ndata)

integer i,j

j=1
do i=1,ndata
   if (land(i)) then
      dataout(i) = datain(j)
      j=j+1
   else
      dataout(i) = irmdi
   endif
enddo

return
end

!==============================================================================!

subroutine data_extrap_index(maskin1,maskin2,maskout, &
                             index,point,nsum,icount,nrows,nrowsize, &
                             num_unres_pts,rem_unres_pts,max_search, &
                             land,lcyclic)

use getkind

implicit none

integer nrows,nrowsize,max_search,num_unres_pts,rem_unres_pts
integer index(num_unres_pts,(2*max_search+1)*(2*max_search+1)-1)
integer nsum(num_unres_pts),point(num_unres_pts),icount(num_unres_pts)
logical maskin1(nrows*nrowsize),maskin2(nrows*nrowsize),maskout(nrows*nrowsize)
logical land,lcyclic

integer i,j,k,ii,ix,iy,ix1,iy1,unres_pt,npts,ipt,isum
logical linclude
integer, save :: icount0=0

rem_unres_pts = 0
unres_pt = 0
icount0 = icount0 + 1

do i=1,nrows
   do j=1,nrowsize
      ii = (i-1)*nrowsize+j
      if ((.not. maskin1(ii) .and. maskout(ii) .and. land) .or. &
          (maskin1(ii) .and. .not. maskout(ii) .and. .not. land)) then
         unres_pt = unres_pt+1
         if (nsum(unres_pt) < 1) then
            point(unres_pt) = ii
            do k=1,max_search
               isum=0
               npts=2*k+1
               do iy=1,npts
                  iy1 = i-k+iy-1
                  if (iy1 < 1) then
                     linclude = .false.
                     iy1 = 1
                  else if (iy1 > nrows) then
                     linclude = .false.
                     iy1 = nrows
                  else
                     linclude = .true.
                  endif
                  do ix=1,npts
                     ix1 = j-k+ix-1
                     if (ix1 < 1) then
                        if (lcyclic) then
                           linclude = .true.
                           ix1 = ix1 + nrowsize
                        else
                           linclude = .false.
                           ix1 = 1
                        endif
                     else if (ix1 > nrowsize) then
                        if (lcyclic) then
                           linclude = .true.
                           ix1 = ix1 - nrowsize
                        else
                           linclude = .false.
                           ix1 = nrowsize
                        endif
                     else
                        linclude = .true.
                     endif
                     ipt = (iy1-1)*nrowsize+ix1
                     linclude = linclude .and. ii/=ipt
                     if (linclude .and. maskin2(ipt).eqv.land) then
                        isum=isum+1
                        index(unres_pt,isum) = ipt
                     endif
                  enddo
               enddo
               if (isum > 0) then
                  nsum(unres_pt) = isum
                  icount(unres_pt) = icount0
                  maskin2(ii) = land
                  exit
               endif
            enddo
            if (isum == 0) rem_unres_pts = rem_unres_pts+1
         endif
      endif
   enddo
enddo

return
end

!==============================================================================!

subroutine data_extrap(data,index,nsum,point,icount, &
                       max_count,nsize,num_unres_pts,ndim)

use getkind

implicit none

integer nsize,num_unres_pts,ndim,max_count
integer index(num_unres_pts,ndim),nsum(num_unres_pts)
integer point(num_unres_pts),icount(num_unres_pts)
real(rtype) data(nsize)

integer i,j,isum
real(rtype) sum

do j=1,max_count
   do i=1,num_unres_pts
      if (icount(i) == j) then
         sum = 0.0_rtype
         do isum=1,nsum(i)
            sum=sum+data(index(i,isum))
         enddo
         if (nsum(i) > 0) data(point(i)) = sum/real(nsum(i),rtype)
      endif
   enddo
enddo

return
end

!==============================================================================!

subroutine data_extrap32(data,index,nsum,point,icount, &
                         max_count,nsize,num_unres_pts,ndim)

use getkind
use config

implicit none

integer nsize,num_unres_pts,ndim,max_count
integer index(num_unres_pts,ndim),nsum(num_unres_pts)
integer point(num_unres_pts),icount(num_unres_pts)
integer(i32) data(nsize)

integer(i32) idum
integer i,j,isum
real(r32) sum

do j=1,max_count
   do i=1,num_unres_pts
      if (icount(i) == j) then
         sum = 0.0_r32
         do isum=1,nsum(i)
            idum = data(index(i,isum))
            if (lbigend .neqv. lbigendout) call swapbytes(idum,4,1)
            sum=sum+transfer(idum,0.0_r32)
         enddo
         if (nsum(i) > 0) then
            sum = sum/real(nsum(i),r32)
            idum = transfer(sum,0_i32)
            if (lbigend .neqv. lbigendout) call swapbytes(idum,4,1)
            data(point(i)) = idum
         endif
      endif
   enddo
enddo

return
end

!==============================================================================!

subroutine get_stdname_from_stashcode(model,stash_code,stdname,ierr)

implicit none

integer model,stash_code,ierr
character(*) stdname

integer section,item

section = stash_code / 1000
item = mod(stash_code,1000)

stdname = ""
ierr = 0

if (model == 1) then
   if (section == 0) then
      if (item == 1) then
         stdname = "surface_air_pressure"
      else if (item == 2) then
         stdname = "eastward_wind"
      else if (item == 3) then
         stdname = "northward_wind"
      else if (item == 4) then
         stdname = "air_potential_temperature"
      else if (item == 10) then
         stdname = "specific_humidity"
      else if (item == 12) then
         stdname = "mass_fraction_of_cloud_ice_in_air"
      else if (item == 13) then
         stdname = "convective_cloud_area_fraction"
      else if (item == 23) then
         stdname = "snowfall_amount"
      else if (item == 24) then
         stdname = "surface_temperature"
      else if (item == 25) then
         stdname = "atmosphere_boundary_layer_thickness"
      else if (item == 28) then
         stdname = "eastward_sea_water_velocity"
      else if (item == 29) then
         stdname = "northward_sea_water_velocity"
      else if (item == 31) then
         stdname = "sea_ice_area_fraction"
      else if (item == 32) then
         stdname = "sea_ice_draft"
      else if (item == 101) then
         stdname = "mass_fraction_of_sulfur_dioxide_in_air"
      else if (item == 102) then
         stdname = "mass_fraction_of_dimethyl_sulfide_in_air"
      else if (item == 150) then
         stdname = "upward_air_velocity"
      else if (item == 254) then
         stdname = "mass_fraction_of_cloud_liquid_water_in_air"
      else if (item == 407) then
         stdname = "air_pressure"
      else if (item == 408) then
         stdname = "air_pressure"
      else if (item == 409) then
         stdname = "surface_air_pressure"
      else if (item == 506) then
         stdname = "surface_temperature_where_land"
      else if (item == 507) then
         stdname = "surface_temperature_where_open_sea"
      endif
   else if (section == 1) then
      if (item == 201) then
         stdname = "surface_net_downward_shortwave_flux"
      else if (item == 207) then
         stdname = "toa_incoming_shortwave_flux"
      else if (item == 208) then
         stdname = "toa_outgoing_shortwave_flux"
      else if (item == 209) then
         stdname = "toa_outgoing_shortwave_flux_assuming_clear_sky"
      else if (item == 210) then
         stdname = "surface_downwelling_shortwave_flux_assuming_clear_sky"
      else if (item == 211) then
         stdname = "surface_upwelling_shortwave_flux_assuming_clear_sky"
      else if (item == 232) then
         stdname = "tendency_of_air_temperature_due_to_shortwave_heating"
      else if (item == 233) then
         stdname = "tendency_of_air_temperature_due_to_shortwave_heating_assuming_clear_sky"
      else if (item == 235) then
         stdname = "surface_downwelling_shortwave_flux"
      else if (item == 237) then
         stdname = "tropopause_net_downward_shortwave_flux"
      else if (item == 238) then
         stdname = "tropopause_upwelling_shortwave_flux"
      endif
   else if (section == 2) then
      if (item == 201) then
         stdname = "surface_net_downward_longwave_flux"
      else if (item == 204) then
         stdname = "cloud_area_fraction"
      else if (item == 205) then
         stdname = "toa_outgoing_longwave_flux"
      else if (item == 206) then
         stdname = "toa_outgoing_longwave_flux_assuming_clear_sky"
      else if (item == 207) then
         stdname = "surface_downwelling_longwave_flux"
      else if (item == 232) then
         stdname = "tendency_of_air_temperature_due_to_longwave_heating"
      else if (item == 233) then
         stdname = "tendency_of_air_temperature_due_to_longwave_heating_assuming_clear_sky"
      else if (item == 237) then
         stdname = "tropopause_net_downward_longwave_flux"
      else if (item == 238) then
         stdname = "tropopause_downwelling_longwave_flux"
      else if (item == 260) then
         stdname = "mass_fraction_of_o3_in_air"
      endif
   else if (section == 3) then
      if (item == 201) then
         stdname = "downward_heat_flux_in_sea_ice"
      else if (item == 202) then
         stdname = "downward_heat_flux_in_soil"
      else if (item == 217) then
         stdname = "surface_upward_sensible_heat_flux"
      else if (item == 219) then
         stdname = "surface_downward_eastward_stress"
      else if (item == 220) then
         stdname = "surface_downward_northward_stress"
      else if (item == 224) then
         stdname = "wind_mixing_energy_flux_into_ocean"
      else if (item == 225) then
         stdname = "eastward_wind"
      else if (item == 226) then
         stdname = "northward_wind"
      else if (item == 227) then
         stdname = "wind_speed"
      else if (item == 234) then
         stdname = "surface_upward_latent_heat_flux"
      else if (item == 236) then
         stdname = "air_temperature"
      else if (item == 237) then
         stdname = "specific_humidity"
      else if (item == 238) then
         stdname = "soil_temperature"
      else if (item == 245) then
         stdname = "relative_humidity"
      else if (item == 249) then
         stdname = "wind_speed"
      else if (item == 258) then
         stdname = "surface_snow_melt_heat_flux"
      else if (item == 261) then
         stdname = "gross_primary_productivity_of_carbon"
      else if (item == 262) then
         stdname = "net_primary_productivity_of_carbon"
      else if (item == 298) then
         stdname = "water_sublimation_flux"
      else if (item == 313) then
         stdname = "soil_moisture_content_at_field_capacity"
      else if (item == 332) then
         stdname = "toa_outgoing_longwave_flux"
      endif
   else if (section == 4) then
      if (item == 203) then
         stdname = "large_scale_rainfall_rate"
      else if (item == 204) then
         stdname = "large_scale_snowfall_flux"
      endif
   else if (section == 5) then
      if (item == 182) then
         stdname = "tendency_of_specific_humidity_due_to_convection"
      else if (item == 185) then
         stdname = "tendency_of_eastward_wind_due_to_convection"
      else if (item == 186) then
         stdname = "tendency_of_northward_wind_due_to_convection"
      else if (item == 205) then
         stdname = "convective_rainfall_rate"
      else if (item == 206) then
         stdname = "convective_snowfall_flux"
      else if (item == 209) then
         stdname = "air_temperature"
      else if (item == 214) then
         stdname = "rainfall_flux"
      else if (item == 215) then
         stdname = "snowfall_flux"
      else if (item == 216) then
         stdname = "precipitation_flux"
      endif
   else if (section == 6) then
      if (item == 185) then
         stdname = "tendency_of_eastward_wind_due_to_gravity_wave_drag"
      else if (item == 186) then
         stdname = "tendency_of_northward_wind_due_to_gravity_wave_drag"
      else if (item == 201) then
         stdname = "atmosphere_eastward_stress_due_to_gravity_wave_drag"
      else if (item == 202) then
         stdname = "atmosphere_northward_stress_due_to_gravity_wave_drag"
      endif
   else if (section == 8) then
      if (item == 23) then
         stdname = "surface_snow_amount"
      else if (item == 202) then
         stdname = "surface_snow_melt_heat_flux"
      else if (item == 208) then
         stdname = "soil_moisture_content"
      else if (item == 209) then
         stdname = "canopy_water_amount"
      else if (item == 223) then
         stdname = "moisture_content_of_soil_layer"
      else if (item == 225) then
         stdname = "soil_temperature"
      else if (item == 229) then
         stdname = "mass_fraction_of_unfrozen_water_in_soil_moisture"
      else if (item == 230) then
         stdname = "mass_fraction_of_frozen_water_in_soil_moisture"
      else if (item == 231) then
         stdname = "surface_snow_melt_flux"
      else if (item == 234) then
         stdname = "runoff_flux"
      endif
   else if (section == 12) then
      if (item == 181) then
         stdname = "tendency_of_air_temperature_due_to_advection"
      else if (item == 182) then
         stdname = "tendency_of_specific_humidity_due_to_advection"
      else if (item == 183) then
         stdname = "tendency_of_mass_fraction_of_cloud_liquid_water_in_air_due_to_advection"
      else if (item == 184) then
         stdname = "tendency_of_mass_fraction_of_cloud_ice_in_air_due_to_advection"
      else if (item == 185) then
         stdname = "tendency_of_eastward_wind_due_to_advection"
      else if (item == 186) then
         stdname = "tendency_of_northward_wind_due_to_advection"
      else if (item == 187) then
         stdname = "tendency_of_upward_air_velocity_due_to_advection"
      else if (item == 201) then
         stdname = "omega"
      endif
   else if (section == 13) then
      if (item == 181) then
         stdname = "tendency_of_air_temperature_due_to_diffusion"
      else if (item == 182) then
         stdname = "tendency_of_specific_humidity_due_to_diffusion"
      else if (item == 185) then
         stdname = "tendency_of_eastward_wind_due_to_diffusion"
      else if (item == 186) then
         stdname = "tendency_of_northward_wind_due_to_diffusion"
      endif
   else if (section == 15) then
      if (item == 201) then
         stdname = "eastward_wind"
      else if (item == 202) then
         stdname = "northward_wind"
      else if (item == 214) then
         stdname = "ertel_potential_vorticity"
      else if (item == 215) then
         stdname = "product_of_eastward_wind_and_northward_wind"
      else if (item == 216) then
         stdname = "air_temperature"
      else if (item == 217) then
         stdname = "product_of_northward_wind_and_air_temperature"
      else if (item == 219) then
         stdname = "square_of_air_temperature"
      else if (item == 220) then
         stdname = "square_of_eastward_wind"
      else if (item == 221) then
         stdname = "square_of_northward_wind"
      else if (item == 222) then
         stdname = "omega"
      else if (item == 223) then
         stdname = "product_of_omega_and_air_temperature"
      else if (item == 224) then
         stdname = "product_of_eastward_wind_and_omega"
      else if (item == 225) then
         stdname = "product_of_northward_wind_and_omega"
      else if (item == 226) then
         stdname = "specific_humidity"
      else if (item == 227) then
         stdname = "product_of_eastward_wind_and_specific_humidity"
      else if (item == 228) then
         stdname = "product_of_northward_wind_and_specific_humidity"
      else if (item == 235) then
         stdname = "product_of_omega_and_specific_humidity"
      else if (item == 238) then
         stdname = "geopotential_height"
      else if (item == 239) then
         stdname = "product_of_eastward_wind_and_geopotential_height"
      else if (item == 240) then
         stdname = "product_of_northward_wind_and_geopotential_height"
      endif
   else if (section == 16) then
      if (item == 202) then
         stdname = "geopotential_height"
      else if (item == 203) then
         stdname = "air_temperature"
      else if (item == 204) then
         stdname = "relative_humidity"
      else if (item == 222) then
         stdname = "air_pressure_at_sea_level"
      endif
   else if (section == 30) then
      if (item == 4) then
         stdname = "air_temperature"
      else if (item == 5) then
         stdname = "specific_humidity"
      else if (item == 111) then
         stdname = "air_temperature"
      else if (item == 113) then
         stdname = "relative_humidity"
      else if (item == 181) then
         stdname = "tendency_of_air_temperature"
      else if (item == 182) then
         stdname = "tendency_of_specific_humidity"
      else if (item == 183) then
         stdname = "tendency_of_mass_fraction_of_cloud_liquid_water_in_air"
      else if (item == 184) then
         stdname = "tendency_of_mass_fraction_of_cloud_ice_in_air"
      else if (item == 185) then
         stdname = "tendency_of_eastward_wind"
      else if (item == 186) then
         stdname = "tendency_of_northward_wind"
      else if (item == 187) then
         stdname = "tendency_of_upward_air_velocity"
      else if (item == 188) then
         stdname = "tendency_of_air_density"
      else if (item == 201) then
         stdname = "eastward_wind"
      else if (item == 202) then
         stdname = "northward_wind"
      else if (item == 203) then
         stdname = "upward_air_velocity"
      else if (item == 204) then
         stdname = "air_temperature"
      else if (item == 205) then
         stdname = "specific_humidity"
      else if (item == 206) then
         stdname = "relative_humidity"
      else if (item == 207) then
         stdname = "geopotential_height"
      else if (item == 208) then
         stdname = "lagrangian_tendency_of_air_pressure"
      else if (item == 211) then
         stdname = "square_of_eastward_wind"
      else if (item == 212) then
         stdname = "product_of_eastward_wind_and_northward_wind"
      else if (item == 213) then
         stdname = "product_of_eastward_wind_and_upward_air_velocity"
      else if (item == 214) then
         stdname = "product_of_eastward_wind_and_air_temperature"
      else if (item == 215) then
         stdname = "product_of_eastward_wind_and_specific_humidity"
      else if (item == 217) then
         stdname = "product_of_eastward_wind_and_geopotential_height"
      else if (item == 218) then
         stdname = "product_of_eastward_wind_and_omega"
      else if (item == 222) then
         stdname = "square_of_northward_wind"
      else if (item == 223) then
         stdname = "product_of_northward_wind_and_upward_air_velocity"
      else if (item == 224) then
         stdname = "product_of_northward_wind_and_air_temperature"
      else if (item == 225) then
         stdname = "product_of_northward_wind_and_specific_humdity"
      else if (item == 227) then
         stdname = "product_of_northward_wind_and_geopotential_height"
      else if (item == 228) then
         stdname = "product_of_northward_wind_and_omega"
      else if (item == 233) then
         stdname = "square_of_upward_air_velocity"
      else if (item == 234) then
         stdname = "product_of_upward_air_velocity_and_air_temperature"
      else if (item == 235) then
         stdname = "product_of_upward_air_velocity_and_specific_humidity"
      else if (item == 244) then
         stdname = "square_of_air_temperature"
      else if (item == 245) then
         stdname = "product_of_air_temperature_and_specific_humidity"
      else if (item == 248) then
         stdname = "product_of_air_temperature_and_omega"
      else if (item == 258) then
         stdname = "product_of_specific_humidity_and_omega"
      else if (item == 277) then
         stdname = "square_of_geopotential_height"
      else if (item == 278) then
         stdname = "product_of_geopotential_height_and_omega"
      else if (item == 288) then
         stdname = "square_of_lagrangian_tendency_of_air_pressure"
      else if (item == 417) then
         stdname = "surface_air_pressure"
      else if (item == 418) then
         stdname = "surface_air_pressure"
      else if (item == 451) then
         stdname = "tropopause_air_pressure"
      else if (item == 452) then
         stdname = "tropopause_air_temperature"
      else if (item == 453) then
         stdname = "tropopause_altitude"
      endif
   endif
else if (model == 2) then
   if (section == 0) then
      if (item == 101) then
         stdname = "sea_water_potential_temperature"
      else if (item == 102) then
         stdname = "sea_water_salinity"
      else if (item == 121) then
         stdname = "baroclinic_eastward_sea_water_velocity"
      else if (item == 122) then
         stdname = "baroclinic_northward_sea_water_velocity"
      else if (item == 130) then
         stdname = "ocean_barotropic_streamfunction"
      else if (item == 132) then
         stdname = "tendency_of_ocean_barotropic_streamfunction"
      else if (item == 137) then
         stdname = "ocean_mixed_layer_thickness"
      else if (item == 143) then
         stdname = "upward_sea_ice_basal_heat_flux"
      else if (item == 146) then
         stdname = "sea_ice_area_fraction"
      else if (item == 147) then
         stdname = "sea_ice_thickness"
      else if (item == 150) then
         stdname = "surface_downward_eastward_stress"
      else if (item == 151) then
         stdname = "surface_downward_northward_stress"
      else if (item == 152) then
         stdname = "wind_mixing_energy_flux_into_ocean"
      else if (item == 166) then
         stdname = "water_flux_into_ocean_from_rivers"
      else if (item == 171) then
         stdname = "surface_snow_thickness_where_sea_ice"
      else if (item == 186) then
         stdname = "water_flux_correction"
      else if (item == 191) then
         stdname = "downward_heat_flux_in_sea_ice"
      endif
   else if (section == 30) then
      if (item == 201) then
         stdname = "upward_sea_water_velocity"
      else if (item == 320) then
         stdname = "eastward_sea_water_velocity"
      else if (item == 321) then
         stdname = "northward_sea_water_velocity"
      endif
   else if (section == 32) then
      if (item == 209) then
         stdname = "eastward_sea_ice_velocity"
      else if (item == 210) then
         stdname = "northward_sea_ice_velocity"
      else if (item == 212) then
         stdname = "tendency_of_sea_ice_thickness_due_to_thermodynamics"
      endif
   endif
endif

if (stdname == "") ierr = 1

return
end

