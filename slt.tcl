proc sltwin {win} {

   global slt mask lfrac

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  ww1 and ww2 are hidden frames

   set ww1 $ww0.ww1
   frame $ww1
   set ww2 $ww1.setdate
   frame $ww2

   title $ww0.title "Deep Soil Temperatures"
   spacer $ww0.spacer1
   create_win2 $ww0.create $ww1 "Create Deep Soil Temperature ancillary file? " slt(create)
   spacer $ww1.spacer2
   get_filename1 $ww1.infile \
                 "Enter input Deep Soil Temperature NetCDF file name: " \
                 slt(file_in) nc slt_ncfile
   spacer $ww1.spacer3
   select_varname1 $ww1.varname "NetCDF Deep Soil Temperature variable name: " slt(name) slt(file_in)
   spacer $ww1.spacer4
   set_filename $ww1.outfile "Enter output Deep Soil Temperature ancillary file name: " slt(file_out)
   spacer $ww1.spacer5
   #set_boolvar2 $ww1.per "Is Deep Soil Temperature \nancillary data periodic in time? " slt(periodic)
   set_boolvar2 $ww1.per "Is Deep Soil Temperature ancillary data periodic in time? " slt(periodic)
   spacer $ww1.spacer7
   howto_calc_lsm $ww1.mask slt(mask) "sea" \
      "$mask(use) || $mask(create)" "$lfrac(use) || $lfrac(create)"
   spacer $ww1.spacer8
   select_window $ww1.date1 $ww2 \
      "Use dates from NetCDF file                           " \
      "Specify Deep Soil Temperature ancillary file dates   " slt(timeusage1)
   spacer $ww1.spacer9
   set_date $ww2.date2 "Deep Soil Temperature" slt
   
#  Pack hidden frames if required

   if {$slt(create)} {pack $ww1 -expand yes -fill x -side bottom}
   if {$slt(timeusage1) == 1} {pack $ww2 -expand yes -fill x -side top}
   
}

proc slt_ncfile {win ncfile} {

   global config slt
   
   add_ncfile $win $ncfile
   
   if {$win == ""} {return}
   
   set version $config(version)
   set iversion [expr {int(int($version)*100 + ($version-int($version))*10)}]

   set slt(name) [lindex [getvarname $ncfile 1 $iversion 20 \
                                               $slt(name)] 0]
}
