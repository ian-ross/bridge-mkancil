proc orogwin {win} {

   global orog mask lfrac

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  ww1 is a hidden frame

   set ww1 $ww0.ww1
   frame $ww1

   title $ww0.title "Orography"
   spacer $ww0.spacer1
   create_win2 $ww0.create $ww1 "Create Orography ancillary file? " orog(create)
   spacer $ww1.spacer2
   get_filename1 $ww1.infile "Enter input Orography NetCDF file name: " \
                 orog(file_in) nc orog_ncfile
   spacer $ww1.spacer3
   select_varname1 $ww1.varname1 "NetCDF Orography variable name:                        " \
                   orog(name) orog(file_in)
   select_varname1 $ww1.varname2 "NetCDF Standard deviation of orography variable name:  " \
                   orog(sdname) orog(file_in)
   select_varname1 $ww1.varname8 "NetCDF Orographic gradient  x component variable name: " \
                   orog(xgradname) orog(file_in)
   select_varname1 $ww1.varname9 "NetCDF Orographic gradient  y component variable name: " \
                   orog(ygradname) orog(file_in)
   select_varname1 $ww1.varname3 "NetCDF Orographic gradient xx component variable name: " \
                   orog(xxgradname) orog(file_in)
   select_varname1 $ww1.varname4 "NetCDF Orographic gradient xy component variable name: " \
                   orog(xygradname) orog(file_in)
   select_varname1 $ww1.varname5 "NetCDF Orographic gradient yy component variable name: " \
                   orog(yygradname) orog(file_in)
   select_varname1 $ww1.varname6 "NetCDF Silhouette orographic roughness variable name:  " \
                   orog(silname) orog(file_in)
   select_varname1 $ww1.varname7 "NetCDF Half peak to trough height variable name:       " \
                   orog(pthtname) orog(file_in)
   select_varname1 $ww1.varname10 "NetCDF Unfiltered orography variable name:             " \
                   orog(unfiltname) orog(file_in)
   spacer $ww1.spacer4
   set_boolvar2 $ww1.incgrad \
      "Include orographic gradient component fields? " orog(incgrad) \
      "set_win_state \"$ww1.varname8 $ww1.varname9\" \"\$orog(incgrad)\""
   set_boolvar2 $ww1.incsqgrad \
      "Include orographic square gradient component fields? " orog(incsqgrad) \
      "set_win_state \"$ww1.varname3 $ww1.varname4 $ww1.varname5\" \"\$orog(incsqgrad)\""
   set_boolvar2 $ww1.incrough \
      "Include orographic roughness fields? " orog(incrough) \
      "set_win_state \"$ww1.varname6 $ww1.varname7\" \"\$orog(incrough)\""
   set_boolvar2 $ww1.incunfilt \
      "Include unfiltered orograhpy field? " orog(incunfilt) \
      "set_win_state \"$ww1.varname10\" \"\$orog(incunfilt)\""
   spacer $ww1.spacer5
   set_filename $ww1.outfile "Enter output Orography ancillary file name: "\
                orog(file_out)
   spacer $ww1.spacer6
   howto_calc_lsm $ww1.mask orog(mask) "sea" \
      "$mask(use) || $mask(create)" "$lfrac(use) || $lfrac(create)"
   spacer $ww1.spacer7

#  Pack hidden frames if required

   if {$orog(create)} {pack $ww1 -expand yes -fill x -side bottom}
   set_win_state "$ww1.varname8 $ww1.varname9" $orog(incgrad)
   set_win_state "$ww1.varname3 $ww1.varname4 $ww1.varname5" $orog(incsqgrad)
   set_win_state "$ww1.varname6 $ww1.varname7" $orog(incrough)
   set_win_state "$ww1.varname10" $orog(incunfilt)
}

proc orog_ncfile {win ncfile} {

   global config orog
   
   add_ncfile $win $ncfile
   
   if {$win == ""} {return}
   
   set version $config(version)
   set iversion [expr {int(int($version)*100 + ($version-int($version))*10)}]

   set orog(name)       [lindex [getvarname $ncfile 1 $iversion 33 \
                                                      $orog(name)] 0]
   set orog(sdname)     [lindex [getvarname $ncfile 1 $iversion 34 \
                                                      $orog(sdname)] 0]
   set orog(xgradname)  [lindex [getvarname $ncfile 1 $iversion 5 \
                                                      $orog(xgradname)] 0]
   set orog(ygradname)  [lindex [getvarname $ncfile 1 $iversion 6 \
                                                      $orog(ygradname)] 0]
   set orog(xxgradname) [lindex [getvarname $ncfile 1 $iversion 35 \
                                                      $orog(xxgradname)] 0]
   set orog(xygradname) [lindex [getvarname $ncfile 1 $iversion 36 \
                                                      $orog(xygradname)] 0]
   set orog(yygradname) [lindex [getvarname $ncfile 1 $iversion 37 \
                                                      $orog(yygradname)] 0]
   set orog(silname)    [lindex [getvarname $ncfile 1 $iversion 17 \
                                                      $orog(silname)] 0]
   set orog(pthtname)   [lindex [getvarname $ncfile 1 $iversion 18 \
                                                      $orog(pthtname)] 0]
   set orog(unfiltname) [lindex [getvarname $ncfile 1 $iversion 7 \
                                                      $orog(unfiltname)] 0]
}
