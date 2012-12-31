proc astart_configwin {win} {

   global astart

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  ww1 and ww2 are hidden frames

   set ww1 $ww0.ww1
   frame $ww1
   set ww2 $ww1.setdate
   frame $ww2

   set ww3 $ww1.ncfiles
   frame $ww3

   title $ww0.title "Atmosphere Start Dump Configuration"
   spacer $ww0.spacer1
   create_win2 $ww0.create $ww1 "Create Atmosphere Start Dump file? " astart(create)
   set_boolvar2 $ww1.useexist "Use existing Atmosphere Start Dump file? " astart(useexist)
   set_boolvar2 $ww1.addextra "Add extra Atmosphere Start Dump fields? " astart(addextra)
   spacer $ww1.spacer2
   pack $ww3 -expand yes -fill x -side top
   set_var $ww3.nncfile "Enter number of NetCDF input files: " astart(nncfile) 5
   set_filename $ww1.outfile "Enter output UM Atmosphere Start Dump file name: " astart(file_out)
   spacer $ww1.spacer3
   select_window_dump $ww1.date1 $ww2 \
      "Use date from NetCDF file                            " \
      "Use date from UM start dump                          " \
      "Specify Atmosphere start dump date                   " astart(timeusage1)
   spacer $ww1.spacer4
   set_dumpdate $ww2.date2 "Atmosphere start" astart

#  Pack hidden frames if required

   if {$astart(create)} {pack $ww1 -expand yes -fill x -side bottom}
   if {$astart(timeusage1) == 1} {pack $ww2 -expand yes -fill x -side top}

   bind $ww3.nncfile.entry <Map> "getncfiles $ww3 astart"
   bind $ww3.nncfile.entry <Return> "getncfiles $ww3 astart"
}
