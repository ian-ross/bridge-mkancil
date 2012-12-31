proc lfracwin {win} {

   global lfrac

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  ww1 and ww2 are hidden frames

   set ww1 $ww0.ww1
   frame $ww1
   set ww2 $ww1.ww2
   frame $ww2

   title $ww0.title "Land Fraction"
   spacer $ww0.spacer1
   use_file_create_win2 $ww0.usecreate $ww1 $ww2 \
      "Use Land Fraction NetCDF variable? " \
      "Create Land Fraction ancillary file? " lfrac(use) lfrac(create)
   spacer $ww1.spacer2
   get_filename1 $ww1.infile "Enter input Land Fraction NetCDF file name: " \
                 lfrac(file_in) nc lfrac_ncfile
   spacer $ww1.spacer3
   select_varname1 $ww1.varname "NetCDF Land Fraction variable name:   " \
                  lfrac(name) lfrac(file_in)
   spacer $ww2.spacer4
   set_filename $ww2.outfile "Enter output Land Fraction ancillary file name: "\
                lfrac(file_out)

#  Pack hidden frames if required

   if {$lfrac(create)} {
      pack $ww1 -expand yes -fill x -side top
      pack $ww2 -expand yes -fill x -side bottom
   } elseif {$lfrac(use)} {
      pack $ww1 -expand yes -fill x -side top
   }
}

proc lfrac_ncfile {win ncfile} {

   global config lfrac
   
   add_ncfile $win $ncfile
   
   if {$win == ""} {return}
   
   set version $config(version)
   set iversion [expr {int(int($version)*100 + ($version-int($version))*10)}]

   set lfrac(name) [lindex [getvarname $ncfile 1 $iversion 505 \
                                                 $lfrac(name)] 0]
   if {$lfrac(name) == ""} {
      set lfrac(name) [lindex [getvarname $ncfile 1 $iversion 301 \
                                                    $lfrac(name)] 0]
   }
}
