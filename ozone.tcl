proc ozonewin {win} {

   global ozone

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  ww1 and ww2 are hidden frames

   set ww1 $ww0.ww1
   frame $ww1
   set ww2 $ww1.setdate
   frame $ww2

   title $ww0.title "Ozone"
   spacer $ww0.spacer1
   create_win2 $ww0.create $ww1 "Create Ozone ancillary file? " ozone(create)
   spacer $ww1.spacer2
   get_filename1 $ww1.infile "Enter input Ozone NetCDF file name: " \
                 ozone(file_in) nc ozone_ncfile
   spacer $ww1.spacer3
   select_varname1 $ww1.varname "NetCDF Ozone variable name: " ozone(name) ozone(file_in)
   spacer $ww1.spacer4
   set_filename $ww1.outfile "Enter output Ozone ancillary file name: " ozone(file_out)
   spacer $ww1.spacer5
   set_var2 $ww1.mixtype "Input Ozone mixing ratio type: " ozone(mixtype) "Mass   " 1 "Volume " 0
   set_var $ww1.mixfac "Input Ozone mixing ratio factor: " ozone(mixfac) 10
   spacer $ww1.spacer6
   set_boolvar2 $ww1.per "Is Ozone ancillary data periodic in time? " ozone(periodic)
   spacer $ww1.spacer7
   select_window $ww1.date1 $ww2 \
      "Use dates from NetCDF file                           " \
      "Specify Ozone ancillary file dates                   " ozone(timeusage1)
   spacer $ww1.spacer8
   set_date $ww2.date2 "Ozone" ozone

#  Pack hidden frames if required

   if {$ozone(create)} {pack $ww1 -expand yes -fill x -side bottom}
   if {$ozone(timeusage1) == 1} {pack $ww2 -expand yes -fill x -side top}
}

proc ozone_ncfile {win ncfile} {

   global config ozone
   
   add_ncfile $win $ncfile
   
   if {$win == ""} {return}
   
   set version $config(version)
   set iversion [expr {int(int($version)*100 + ($version-int($version))*10)}]

   set ozone(name) [lindex [getvarname $ncfile 1 $iversion 60 \
                                                 $ozone(name)] 0]
}
