proc soilwin {win} {

   global soil mask lfrac

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  ww1 is a hidden frame

   set ww1 $ww0.ww1
   frame $ww1

   title $ww0.title "Soil Parameters"
   spacer $ww0.spacer1
   create_win2 $ww0.create $ww1 "Create Soil Parameters ancillary file? " soil(create)
   spacer $ww1.spacer2
   get_filename1 $ww1.infile "Enter input Soil Parameters NetCDF file name: " \
                 soil(file_in) nc soil_ncfile
   spacer $ww1.spacer3
   select_varname1 $ww1.varname1  "NetCDF VOL. SMC at wilting point variable name:    " soil(vsmcwiltname) soil(file_in)
   select_varname1 $ww1.varname2  "NetCDF VOL. SMC at critical point variable name:   " soil(vsmccritname) soil(file_in)
   select_varname1 $ww1.varname3  "NetCDF VOL. SMC at field capacity variable name:   " soil(vsmcfcapname) soil(file_in)

   select_varname1 $ww1.varname4  "NetCDF VOL. SMC at saturation variable name:       " soil(vsmcsatname) soil(file_in)
   select_varname1 $ww1.varname5  "NetCDF Clapp-Hornberger B parameter variable name: " soil(clapphornname) soil(file_in)
   select_varname1 $ww1.varname6  "NetCDF Thermal conductivity of soil variable name: " soil(thermcondname) soil(file_in)
   select_varname1 $ww1.varname7  "NetCDF Saturated soil conductivity variable name:  " soil(soilcondname) soil(file_in)
   select_varname1 $ww1.varname8  "NetCDF Thermal capacity of soil variable name:     " soil(thermcapname) soil(file_in)
   select_varname1 $ww1.varname9  "NetCDF Saturated soil water suction variable name: " soil(soilwatersucname) soil(file_in)
   select_varname1 $ww1.varname10 "NetCDF Snow free soil albedo variable name:        " soil(soilalbname) soil(file_in)
   select_varname1 $ww1.varname11 "NetCDF Soil carbon content variable name:          " soil(soilcarbname) soil(file_in)
   spacer $ww1.spacer4
   set_filename $ww1.outfile "Enter output Soil Parameters ancillary file name: " soil(file_out)
   spacer $ww1.spacer5
   howto_calc_lsm $ww1.mask soil(mask) "sea" \
      "$mask(use) || $mask(create)" "$lfrac(use) || $lfrac(create)"
   spacer $ww1.spacer6

#  Pack hidden frames if required

   if {$soil(create)} {pack $ww1 -expand yes -fill x -side bottom}
}

proc soil_ncfile {win ncfile} {

   global config soil
   
   add_ncfile $win $ncfile
   
   if {$win == ""} {return}
   
   set version $config(version)
   set iversion [expr {int(int($version)*100 + ($version-int($version))*10)}]

   set soil(vsmcwiltname)     [lindex [getvarname $ncfile 1 $iversion 40 \
                                                  $soil(vsmcwiltname)] 0]
   set soil(vsmccritname)     [lindex [getvarname $ncfile 1 $iversion 41 \
                                                  $soil(vsmccritname)] 0]
   set soil(vsmcfcapname)     [lindex [getvarname $ncfile 1 $iversion 42 \
                                                  $soil(vsmcfcapname)] 0]
   set soil(vsmcsatname)      [lindex [getvarname $ncfile 1 $iversion 43 \
                                                  $soil(vsmcsatname)] 0]
   set soil(clapphornname)    [lindex [getvarname $ncfile 1 $iversion 207 \
                                                  $soil(clapphornname)] 0]
   set soil(thermcondname)    [lindex [getvarname $ncfile 1 $iversion 47 \
                                                  $soil(thermcondname)] 0]
   set soil(soilcondname)     [lindex [getvarname $ncfile 1 $iversion 44 \
                                                  $soil(soilcondname)] 0]
   set soil(thermcapname)     [lindex [getvarname $ncfile 1 $iversion 46 \
                                                  $soil(thermcapname)] 0]
   set soil(soilwatersucname) [lindex [getvarname $ncfile 1 $iversion 48 \
                                                  $soil(soilwatersucname)] 0]
   set soil(soilalbname)      [lindex [getvarname $ncfile 1 $iversion 220 \
                                                  $soil(soilalbname)] 0]
   set soil(soilcarbname)     [lindex [getvarname $ncfile 1 $iversion 223 \
                                                  $soil(soilcarbname)] 0]
}
