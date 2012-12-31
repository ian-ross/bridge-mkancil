proc sstwin {win} {

   global sst ice mask lfrac

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  ww1 and ww2 are hidden frames

   set ww1 $ww0.ww1
   frame $ww1
   set ww2 $ww1.setdate
   frame $ww2

   title $ww0.title "Sea Surface Temperatures"
   spacer $ww0.spacer1
   create_win2 $ww0.create $ww1 "Create Sea surface temperature ancillary file? " sst(create)
   spacer $ww1.spacer2
   get_filename1 $ww1.infile "Enter input SST NetCDF file name: " \
                 sst(file_in) nc sst_ncfile
   spacer $ww1.spacer3
   select_varname1 $ww1.varname "NetCDF SST variable name: " sst(name) sst(file_in)
   spacer $ww1.spacer4
   set_filename $ww1.outfile "Enter output SST ancillary file name: " sst(file_out)
   spacer $ww1.spacer5
   set_var_chk $ww1.minval "Enter minimum allowed SST value: " sst(setminval) sst(minval) 8
   set_var_chk $ww1.iceval "Enter SST value over Sea-Ice:    " sst(seticeval) sst(iceval) 8
   spacer $ww1.spacer6
   set_boolvar2 $ww1.per "Is SST ancillary data periodic in time? " sst(periodic)
   spacer $ww1.spacer7
   howto_calc_lsm $ww1.mask sst(mask) "land" \
      "$mask(use) || $mask(create)" "$lfrac(use) || $lfrac(create)"
   spacer $ww1.spacer8
   select_window $ww1.date1 $ww2 \
      "Use dates from NetCDF file                           " \
      "Specify SST ancillary file dates                     " sst(timeusage1)
   spacer $ww1.spacer9
   set_date $ww2.date2 "SST" sst
   
#  Pack hidden frames if required

   if {$sst(create)} {pack $ww1 -expand yes -fill x -side bottom}
   if {$sst(timeusage1) == 1} {pack $ww2 -expand yes -fill x -side top}
   
   if {! $ice(create)} {
      set_win_state $ww1.iceval false
      set sst(seticeval) 0
   }
}

proc sst_ncfile {win ncfile} {

   global config sst
   
   add_ncfile $win $ncfile
   
   if {$win == ""} {return}
   
   set version $config(version)
   set iversion [expr {int(int($version)*100 + ($version-int($version))*10)}]

   set sst(name) [lindex [getvarname $ncfile 1 $iversion 24 \
                                               $sst(name)] 0]
}
