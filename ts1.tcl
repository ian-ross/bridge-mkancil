proc ts1win {win} {

   global ts1

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  ww1 and ww2 are hidden frames

   set ww1 $ww0.ww1
   frame $ww1
   set ww2 $ww1.setdate
   frame $ww2

   title $ww0.title "Reference SST, SSS, Air-Temp & Ice-Depth"
   spacer $ww0.spacer1
   create_win2 $ww0.create $ww1 "Create Ocean Reference data ancillary file? " ts1(create)
   spacer $ww1.spacer2
   get_filename1 $ww1.infile \
                 "Enter input Ocean Reference data NetCDF file name: " \
                 ts1(file_in) nc ts1_ncfile
   spacer $ww1.spacer3
   set_boolvar2 $ww1.refsst \
      "Include Sea Surface Temperature field? " ts1(refsst) \
      "set_win_state $ww1.varsstname \$ts1(refsst)"
   set_boolvar2 $ww1.refsss \
      "Include Sea Surface Salinity field? " ts1(refsss) \
      "set_win_state $ww1.varsssname \$ts1(refsss)"
   set_boolvar2 $ww1.climat \
      "Include Air Temperature field? " ts1(climat) \
      "set_win_state $ww1.varatname \$ts1(climat)"
   set_boolvar2 $ww1.climid \
      "Include Ice Depth field? " ts1(climid) \
      "set_win_state $ww1.varidname \$ts1(climid)"
   spacer $ww1.spacer4
   select_varname1 $ww1.varsstname "NetCDF Sea Surface Temperature variable name: " ts1(sstname) ts1(file_in)
   select_varname1 $ww1.varsssname "NetCDF Sea Surface Salinity variable name:    " ts1(sssname) ts1(file_in)
   select_varname1 $ww1.varatname  "NetCDF Air Temperature variable name:         " ts1(atname) ts1(file_in)
   select_varname1 $ww1.varidname  "NetCDF Ice Depth variable name:               " ts1(idname) ts1(file_in)
   spacer $ww1.spacer5
   set_filename $ww1.outfile "Enter output Ocean Reference data ancillary file name: " ts1(file_out)
   spacer $ww1.spacer6
   set_boolvar $ww1.mask \
      "Use missing data value to calculate land mask " ts1(mask)
   spacer $ww1.spacer7
   set_boolvar2 $ww1.per "Is Ocean Reference ancillary data periodic in time? " ts1(periodic)
   spacer $ww1.spacer8
   select_window $ww1.date1 $ww2 \
      "Use dates from NetCDF file                           " \
      "Specify Ocean Reference data ancillary file dates    " ts1(timeusage1)
   spacer $ww1.spacer9
   set_date $ww2.date2 "Ocean Reference" ts1

#  Pack hidden frames if required

   if {$ts1(create)} {pack $ww1 -expand yes -fill x -side bottom}
   if {$ts1(timeusage1) == 1} {pack $ww2 -expand yes -fill x -side top}

   set_win_state $ww1.varsstname $ts1(refsst)
   set_win_state $ww1.varsssname $ts1(refsss)
   set_win_state $ww1.varatname $ts1(climat)
   set_win_state $ww1.varidname $ts1(climid)
}

proc ts1_ncfile {win ncfile} {

   global config ts1
   
   add_ncfile $win $ncfile
   
   if {$win == ""} {return}
   
   set version $config(version)
   set iversion [expr {int(int($version)*100 + ($version-int($version))*10)}]

   set ts1(sstname) [lindex [getvarname $ncfile 2 $iversion 180 \
                                                  $ts1(sstname)] 0]
   set ts1(sssname) [lindex [getvarname $ncfile 2 $iversion 181 \
                                                  $ts1(sssname)] 0]
   set ts1(atname)  [lindex [getvarname $ncfile 2 $iversion 182 \
                                                  $ts1(atname)] 0]
   set ts1(idname)  [lindex [getvarname $ncfile 2 $iversion 183 \
                                                  $ts1(idname)] 0]
}
