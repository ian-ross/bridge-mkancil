#!/bin/sh
# \
exec convsh "$0" ${1+"$@"}

#  Use STASH code to get UM variable long names
set umlong 1

#  Create 64 bit netcdf file
set ncprec 64

#  Select netcdf attributes

setvaratt all att_nouse
setvaratt long_name units missing_value _Fillvalue source att_use
setvaratt valid_min valid_max att_use
setvaratt north_pole standard_name att_cond

setdimatt all att_nouse
setdimatt units axis time_origin positive att_use
setdimatt calendar att_cond

#  Write out Netcdf file
set outformat netcdf

#  Automatically work out input file type
set filetype 0

#  Convert all fields in input files to Netcdf
set fieldlist -1

#  Read in ancillary input files and write out netcdf file

foreach file $argv {
   readfile $filetype $file
   writefile $outformat $file.nc $fieldlist
   clearall
}
