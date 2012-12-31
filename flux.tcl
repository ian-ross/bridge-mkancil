proc fluxwin {win} {

   global flux

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  ww1 and ww2 are hidden frames

   set ww1 $ww0.ww1
   frame $ww1
   set ww2 $ww1.setdate
   frame $ww2

   title $ww0.title "Flux Correction Fields"
   spacer $ww0.spacer1
   create_win2 $ww0.create $ww1 "Create Flux Correction ancillary file? " flux(create)
   spacer $ww1.spacer2
   get_filename1 $ww1.infile "Enter input Flux Correction NetCDF file name: " \
                 flux(file_in) nc flux_ncfile
   spacer $ww1.spacer3
   set_boolvar2 $ww1.refsst \
      "Include Heat Flux field? " flux(heat) \
      "set_win_state $ww1.varheatname \$flux(heat)"
   set_boolvar2 $ww1.refsss \
      "Include Salinity Flux field? " flux(salt) \
      "set_win_state $ww1.varsaltname \$flux(salt)"
   spacer $ww1.spacer4
   select_varname1 $ww1.varheatname "NetCDF Heat Flux variable name:     " flux(heatname) flux(file_in)
   select_varname1 $ww1.varsaltname "NetCDF Salinity Flux variable name: " flux(saltname) flux(file_in)
   spacer $ww1.spacer5
   set_filename $ww1.outfile "Enter output Flux Correction ancillary file name: " flux(file_out)
   spacer $ww1.spacer6
   set_boolvar $ww1.mask \
      "Use missing data value to calculate land mask " flux(mask)
   spacer $ww1.spacer7
   set_boolvar2 $ww1.per "Is Flux Correction ancillary data periodic in time? " flux(periodic)
   spacer $ww1.spacer8
   select_window $ww1.date1 $ww2 \
      "Use dates from NetCDF file                           " \
      "Specify Flux Correction data ancillary file dates    " flux(timeusage1)
   spacer $ww1.spacer9
   set_date $ww2.date2 "Flux Correction" flux

#  Pack hidden frames if required

   if {$flux(create)} {pack $ww1 -expand yes -fill x -side bottom}
   if {$flux(timeusage1) == 1} {pack $ww2 -expand yes -fill x -side top}

   set_win_state $ww1.varheatname $flux(heat)
   set_win_state $ww1.varsaltname $flux(salt)
}

proc flux_ncfile {win ncfile} {

   global config flux
   
   add_ncfile $win $ncfile
   
   if {$win == ""} {return}
   
   set version $config(version)
   set iversion [expr {int(int($version)*100 + ($version-int($version))*10)}]

   set flux(heatname) [lindex [getvarname $ncfile 2 $iversion 185 \
                                                    $flux(heatname)] 0]
   set flux(saltname) [lindex [getvarname $ncfile 2 $iversion 186 \
                                                    $flux(saltname)] 0]
}
