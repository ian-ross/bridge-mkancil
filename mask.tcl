proc maskwin {win} {

   global mask lfrac

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  ww1 and ww2 are hidden frames

   set ww1 $ww0.ww1
   frame $ww1
   set ww2 $ww1.ww2
   frame $ww2

   title $ww0.title "Land/Sea Mask"
   spacer $ww0.spacer1
   use_file_create_win2 $ww0.usecreate $ww1 $ww2 \
      "Use Land/Sea mask NetCDF variable? " \
      "Create Land/Sea mask ancillary file? " mask(use) mask(create)
   spacer $ww1.spacer2
   get_filename1 $ww1.infile "Enter input Land/Sea mask NetCDF file name: " \
                 mask(file_in) nc mask_ncfile
   spacer $ww1.spacer3
   set_boolvar $ww1.uselfrac \
      "Use Land Fraction to create Land/Sea mask? " mask(uselfrac) \
      "set_win_state \"$ww1.usemdi $ww1.sea $ww1.val $ww1.varname\" \"! \$mask(uselfrac)\""
   set_boolvar $ww1.usemdi \
      "Use missing data values for Land/Sea mask? " mask(usemdi) \
      "set_win_state $ww1.val \"! \$mask(usemdi)\""
   set_var2 $ww1.sea "Mask value is:   " mask(sea) "Land " 0 "Sea  " 1
   set_var $ww1.val "Enter value of mask points: " mask(val) 8
   spacer $ww1.spacer4
   select_varname1 $ww1.varname "NetCDF Land/Sea mask variable name:   " mask(name) mask(file_in)
   select_varname1 $ww2.varofname "NetCDF River catchment variable name: " mask(ofname) mask(file_in)
   spacer $ww2.spacer5
   set_boolvar2 $ww2.outflow \
      "Include river catchment field (coupled model only)? " mask(outflow) \
      "set_win_state $ww2.varofname \$mask(outflow)"
   spacer $ww2.spacer6
   set_filename $ww2.outfile "Enter output Land/Sea mask ancillary file name: " mask(file_out)

#  Pack hidden frames if required

   if {$mask(create)} {
      pack $ww1 -expand yes -fill x -side top
      pack $ww2 -expand yes -fill x -side bottom
   } elseif {$mask(use)} {
      pack $ww1 -expand yes -fill x -side top
   }
   
   if {(! $lfrac(create)) && (! $lfrac(use))} {
      set_win_state $ww1.uselfrac false
      set mask(uselfrac) 0
   }

   set_win_state "$ww1.usemdi $ww1.sea $ww1.val $ww1.varname" "! $mask(uselfrac)"
   set_win_state $ww1.val "! $mask(usemdi)"
   set_win_state $ww2.varofname $mask(outflow)
}

proc mask_ncfile {win ncfile} {

   global config mask
   
   add_ncfile $win $ncfile
   
   if {$win == ""} {return}
   
   set version $config(version)
   set iversion [expr {int(int($version)*100 + ($version-int($version))*10)}]

   set mask(name)   [lindex [getvarname $ncfile 1 $iversion 30 \
                                                  $mask(name)] 0]
   set mask(ofname) [lindex [getvarname $ncfile 1 $iversion 93 \
                                                  $mask(ofname) 700] 0]
}
