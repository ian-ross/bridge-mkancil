proc icewin {win} {

   global ice sst mask lfrac

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  ww1 and ww2 are hidden frames

   set ww1 $win.ww1
   frame $ww1
   set ww2 $ww1.setdate
   frame $ww2

   title $ww0.title "Sea Ice"
   spacer $ww0.spacer1
   create_win2 $ww0.create $ww1 "Create Sea-Ice ancillary file? " ice(create)
   spacer $ww1.spacer2
   get_filename1 $ww1.infile "Enter input Sea-Ice NetCDF file name: " \
                 ice(file_in) nc ice_ncfile
   spacer $ww1.spacer3
   select_varname1 $ww1.varconcname "NetCDF Sea-Ice fraction variable name: " ice(concname) ice(file_in)
   select_varname1 $ww1.vardepthname "NetCDF Sea-Ice depth variable name:    " ice(depthname) ice(file_in)
   select_varname1 $ww1.varedgename "NetCDF Sea-Ice edge variable name:     " ice(edgename) ice(file_in)
   spacer $ww1.spacer4
   set_boolvar2 $ww1.amip2 \
      "Use AMIPII ancillary Sea-Ice format? " ice(amip2) \
      "set_win_state \"$ww1.calcdepth $ww1.calcedge\" \"! \$ice(amip2)\" ; \
       set_win_state $ww1.vardepthname \"! \(\$ice(amip2) || \$ice(calcdepth)\)\"; \
       set_win_state $ww1.varedgename \"! \(\$ice(amip2) || \$ice(calcedge)\)\""
   set_boolvar2 $ww1.usesstval \
      "Calculate Sea-Ice fraction values from SST value? " ice(usesstval) \
      "set_win_state \"$ww1.varconcname $ww1.percent $ww1.mkmask\" \"! \$ice(usesstval)\" ; \
       set_win_state $ww1.cutoffval \"! \$ice(usesstval) && \$ice(mkmask)\" ; \
       set_win_state $ww1.sstval \$ice(usesstval)"
   set_boolvar2 $ww1.calcdepth \
      "Calculate Sea-Ice depth values from Sea-Ice fraction? " ice(calcdepth) \
      "set_win_state $ww1.vardepthname \"! \$ice(calcdepth)\""
   set_boolvar2 $ww1.calcedge \
      "Calculate Sea-Ice edge values from Sea-Ice fraction? " ice(calcedge) \
      "set_win_state $ww1.varedgename \"! \$ice(calcedge)\""
   set_boolvar2 $ww1.percent \
      "Convert to Sea-Ice fraction from percentage? " ice(percent)
   set_boolvar2 $ww1.mkmask \
      "Convert Sea-Ice fraction to mask (0,1)? " ice(mkmask) \
      "set_win_state $ww1.cutoffval \$ice(mkmask)"
   set_var $ww1.sstval "Enter SST value for Sea-Ice mask:         " ice(sstval) 8
   set_var $ww1.cutoffval "Enter Sea-Ice fraction mask cutoff value: " ice(cutoffval) 8
   spacer $ww1.spacer5
   set_filename $ww1.outfile "Enter output Sea-Ice ancillary file name: " ice(file_out)
   spacer $ww1.spacer6
   set_var_chk $ww1.minval "Enter minimum allowed Sea-Ice fraction value: " ice(setminval) ice(minval) 8
   set_var_chk $ww1.maxval "Enter maximum allowed Sea-Ice fraction value: " ice(setmaxval) ice(maxval) 8
   spacer $ww1.spacer7
   set_boolvar2 $ww1.per "Is Sea-Ice ancillary data periodic in time? " ice(periodic)
   spacer $ww1.spacer8
   howto_calc_lsm $ww1.mask ice(mask) "land" \
      "$mask(use) || $mask(create)" "$lfrac(use) || $lfrac(create)"
   spacer $ww1.spacer9
   select_window_ice $ww1.date1 $ww2 \
      "Use dates from NetCDF file                           " \
      "Specify Sea-Ice ancillary file dates                 " \
      "Use dates specified for SST ancillary file           " ice(timeusage1) $sst(create)
   spacer $ww1.spacer10
   set_date $ww2.date2 "Sea-Ice" ice
   
#  Pack hidden frames if required

   if {$ice(create)} {pack $ww1 -expand yes -fill x -side bottom}
   
   if {! $sst(create)} {
      set_win_state $ww1.usesstval false
      set_win_state $ww1.sstval false
      set ice(usesstval) 0
      if {$ice(timeusage1) == 2} {
         set ice(timeusage1) 0
	 set_win_state $ww1.date1.sel3 false
      }
   } else {
      set ice(periodic) $sst(periodic)
      set ice(timeusage1) 2
      set_win_state $ww1.per false
   }
   set_win_state $ww1.sstval $ice(usesstval)
   set_win_state $ww1.cutoffval $ice(mkmask)
   set_win_state "$ww1.varconcname $ww1.percent $ww1.mkmask" "! $ice(usesstval)"
   set_win_state $ww1.cutoffval "! $ice(usesstval) && $ice(mkmask)"
   set_win_state $ww1.vardepthname "! $ice(calcdepth)"
   set_win_state $ww1.varedgename "! $ice(calcedge)"
   set_win_state "$ww1.calcdepth $ww1.calcedge" "! $ice(amip2)"
   set_win_state $ww1.vardepthname "! ($ice(amip2) || $ice(calcdepth))"
   set_win_state $ww1.varedgename "! ($ice(amip2) || $ice(calcedge))"
       
   if {$ice(timeusage1) == 1} {pack $ww2 -expand yes -fill x -side top}
}

proc ice_ncfile {win ncfile} {

   global config ice
   
   add_ncfile $win $ncfile
   
   if {$win == ""} {return}
   
   set version $config(version)
   set iversion [expr {int(int($version)*100 + ($version-int($version))*10)}]

   set ice(concname)  [lindex [getvarname $ncfile 1 $iversion 31 \
                                                    $ice(concname)] 0]
   set ice(depthname) [lindex [getvarname $ncfile 1 $iversion 32 \
                                                    $ice(depthname) 92] 0]
   if {$ice(depthname) == ""} {
      set ice(depthname) [lindex [getvarname $ncfile 1 $iversion 32 \
                                                       $ice(depthname) 687] 0]
   }
   set ice_edgename   [getvarname $ncfile 1 $iversion 38 $ice(edgename) 37]
   set ice(edgename)  ""
   foreach name $ice_edgename {
      if {$name != $ice(concname)} {
         set ice(edgename) $name
         break
      }
   }
   if {$ice(edgename) == ""} {
      set ice(edgename) [lindex $ice_edgename 0]
   }
}
