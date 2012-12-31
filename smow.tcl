proc smowwin {win} {

   global smow mask lfrac

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  ww1 and ww2 are hidden frames

   set ww1 $ww0.ww1
   frame $ww1
   set ww2 $ww1.setdate
   frame $ww2

   title $ww0.title "Soil Moisture and Snow Depth"
   spacer $ww0.spacer1
   create_win2 $ww0.create $ww1 "Create Soil Moisture and Snow Depth ancillary file? " smow(create)
   spacer $ww1.spacer2
   get_filename1 $ww1.infile \
                 "Enter input Soil Moisture and Snow Depth NetCDF file name: " \
                 smow(file_in) nc smow_ncfile
   spacer $ww1.spacer3
   select_varname1 $ww1.varconcname "NetCDF Snow depth variable name:    " smow(snowdepthname) smow(file_in)
   select_varname1 $ww1.varedgename "NetCDF Snow edge variable name:     " smow(snowedgename) smow(file_in)
   select_varname1 $ww1.varsoilname "NetCDF Soil moisture variable name: " smow(soilmoisturename) smow(file_in)
   spacer $ww1.spacer4
   set_boolvar2 $ww1.calcedge \
      "Calculate Snow edge values from Snow depth? " smow(calcsnowedge) \
      "set_win_state $ww1.varedgename \"! \$smow(calcsnowedge)\""
   spacer $ww1.spacer5
   set_filename $ww1.outfile "Enter output Soil Moisture and Snow Depth ancillary file name: " smow(file_out)
   spacer $ww1.spacer6
   set_boolvar2 $ww1.per "Is Soil Moisture and Snow Depth \nancillary data periodic in time? " smow(periodic)
   spacer $ww1.spacer8
   howto_calc_lsm $ww1.mask smow(mask) "sea" \
      "$mask(use) || $mask(create)" "$lfrac(use) || $lfrac(create)"
   spacer $ww1.spacer9
   select_window $ww1.date1 $ww2 \
      "Use dates from NetCDF file                                " \
      "Specify Soil Moisture and Snow Depth ancillary file dates " \
      smow(timeusage1)
   spacer $ww1.spacer10
   set_date $ww2.date2 "Soil Moisture and Snow Depth" smow

#  Pack hidden frames if required

   if {$smow(create)} {pack $ww1 -expand yes -fill x -side bottom}
   if {$smow(timeusage1) == 1} {pack $ww2 -expand yes -fill x -side top}
   set_win_state $ww1.varedgename "! $smow(calcsnowedge)"
}

proc smow_ncfile {win ncfile} {

   global config smow
   
   add_ncfile $win $ncfile
   
   if {$win == ""} {return}
   
   set version $config(version)
   set iversion [expr {int(int($version)*100 + ($version-int($version))*10)}]

   set smow(snowdepthname) [lindex [getvarname $ncfile 1 $iversion 23 \
                                               $smow(snowdepthname)] 0]
   set snow_edgename [getvarname $ncfile 1 $iversion 27 $smow(snowedgename) 93]
   set smow(snowedgename) ""
   foreach name $snow_edgename {
      if {$name != $smow(snowdepthname)} {
         set smow(snowedgename) $name
         break
      }
   }
   if {$smow(snowedgename) == ""} {
      set smow(snowedgename) [lindex $snow_edgename 0]
   }
   set smow(soilmoisturename) [lindex [getvarname $ncfile 1 $iversion 9 \
                                                  $smow(soilmoisturename)] 0]
}
