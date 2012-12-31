# Grid configuration functions

proc gridconfig {} {
   global w4 config gridconfig

   clear_right

   scrolledframe::scrolledframe $w4.frame -yscrollcommand "$w4.yscroll set" \
                                -fill x -bd 2
   pack $w4.frame -side left -fill both -expand yes

   scrollbar $w4.yscroll -command "$w4.frame yview" -width 0 -bd 0
   pack $w4.yscroll -side right -fill y

   set ww $w4.frame.scrolled
   bindtags $ww "[bindtags $ww] RightBox"

   set ww0 $ww.ww0
   frame $ww0
   pack $ww0 -expand yes -fill x -side top

   set ww3 $ww.ww3
   frame $ww3
   pack $ww3 -expand yes -fill x -side top

   set ww1 $ww.ww1
   frame $ww1
   pack $ww1 -expand yes -fill x -side top

   set ww2 $ww.ww2
   frame $ww2
   pack $ww2 -expand yes -fill x -side top

#  wh0, wh1, wh2 and wh3 are hidden frames

   set wh0 $ww0.wh0
   frame $wh0
   set wh1 $ww1.wh1
   frame $wh1
   set wh2 $ww2.wh2
   frame $wh2
   set wh3 $ww3.wh3
   frame $wh3
   set wh4 $ww0.wh4
   frame $wh4

   title $ww0.title "Grid configuration"
   spacer $ww0.spacer0

   set_boolvar $ww0.vargrid "Ancillary file on variable resolution grid " gridconfig(vargrid)
   spacer $ww0.spacer1

#   create_win1 $ww0.avert $wh0 "Specify atmosphere vertical levels " gridconfig(avert)
   select_window3 $wh0 $ww0.avert $wh4 \
      "Specify atmosphere vertical levels                               " \
      "Use input NetCDF variables to specify atmosphere vertical levels " \
      "Enter NetCDF dimension to specify atmosphere vertical levels     " \
       gridconfig(avert)
   spacer $ww0.spacer2

   set_var $wh0.nlev "Enter number of model levels: " gridconfig(nlev) 5
   set_var $wh0.nolev "Enter number of ozone levels: " gridconfig(nolev) 5
   set_var2 $wh0.levstore "Is level data stored upwards? " \
            gridconfig(levstoreup) "yes   " 1 "no    " 0
   spacer $wh0.spacer1
   get_filename $wh0.outfile "Enter vertical level namelist file: " gridconfig(vert_namelist)
   spacer $wh0.spacer2

   get_filename3 $wh4.ncfile "NetCDF file:    " \
                 gridconfig(avert_file_in) gridconfig(avert_file_in) "nc" \
                 avert_ncfile
   select_varname1 $wh4.varname "Dimension name: " \
                   gridconfig(avert_name) gridconfig(avert_file_in) true
   spacer $wh4.spacer1
   set_var2 $wh4.theta "Select model level type: " gridconfig(theta) \
                       "Theta " 1 "Rho   " 0
   set_win_state $wh4.theta "$config(version) >= 5.0"
   set_var2 $wh4.ozone "Is number of ozone levels different \nto number of model levels? " \
                       gridconfig(ozonelev) "yes   " 1 "no    " 0 \
                       "set_win_state $wh4.nolev \$gridconfig(ozonelev)"
   spacer $wh4.spacer2
   set_var $wh4.nolev "Enter number of ozone levels: " gridconfig(nolev) 5
   set_win_state $wh4.nolev $gridconfig(ozonelev)
   spacer $wh4.spacer3

   select_window $ww3.overt $wh3 \
      "Use input NetCDF variables to specify ocean vertical levels " \
      "Enter NetCDF dimension to specify ocean vertical levels     " \
      gridconfig(overt) 0 2
   spacer $ww3.spacer3

   get_filename3 $wh3.ncfile "NetCDF file:    " \
                 gridconfig(overt_file_in) gridconfig(overt_file_in) "nc" \
                 overt_ncfile
   select_varname1 $wh3.varname "Dimension name: " \
                   gridconfig(overt_name) gridconfig(overt_file_in) true
   spacer $wh3.spacer1

   create_win1 $ww1.deepsoil $wh1 "Specify deep soil levels " gridconfig(deepsoil)
   spacer $ww1.spacer1

   set_var $wh1.nlev "Enter number of deep soil levels: " gridconfig(nsoillev) 5
   spacer $wh1.spacer1

   create_win1 $ww2.date $wh2 "Specify ancillary file dates " gridconfig(useglobtime)
   spacer $ww2.spacer1

   set_date $wh2.date "ancillary file" gridconfig
   spacer $wh2.spacer1

#  Pack hidden frames if required

   if {$gridconfig(avert) == 1} {pack $wh0 -expand yes -fill x -side top}
   if {$gridconfig(avert) == 2} {pack $wh4 -expand yes -fill x -side top}
   if {$gridconfig(overt) == 2} {pack $wh3 -expand yes -fill x -side top}
   if {$gridconfig(deepsoil)} {
      pack $wh1 -expand yes -fill x -side top
      getsoillevels $wh1
   }
   if {$gridconfig(useglobtime)} {pack $wh2 -expand yes -fill x -side top}
   
   bind $wh1.nlev.entry <Map> "getsoillevels $wh1"
   bind $wh1.nlev.entry <Return> "getsoillevels $wh1"
}

proc getsoillevels {win} {

   global gridconfig

#  Destroy existing soillev entries

   set i 1
   while {true} {
      if {[winfo exists $win.soillev$i]} {
         destroy $win.soillev$i
         incr i
      } else {
         break
      }
   }
   destroy $win.spacer7
   
#  Insert new soillev entries

   if {[isint $gridconfig(nsoillev)]} {
      set i 1
      while {$i <= $gridconfig(nsoillev)} {
         set_var $win.soillev$i "Deep soil level [format "%2d" $i] thickness:     " \
                 gridconfig(soillev$i) 8
         incr i
      }
      spacer $win.spacer7
   }
}

proc avert_ncfile {win ncfile} {

   global config gridconfig
   
   add_ncfile $win $ncfile
   
   if {$win == ""} {return}
   
   set version $config(version)
   set iversion [expr {int(int($version)*100 + ($version-int($version))*10)}]

   if {$iversion < 500} {
      set standardname atmosphere_hybrid_sigma_pressure_coordinate
   } else {
      set standardname atmosphere_hybrid_height_coordinate
   }

   set gridconfig(avert_name) [lindex [getdimname $ncfile "" "" \
                                                  $standardname] 0]
}

proc overt_ncfile {win ncfile} {

   global config gridconfig
   
   add_ncfile $win $ncfile
   
   if {$win == ""} {return}
   
   set standardname depth

   set gridconfig(overt_name) [lindex [getdimname $ncfile \
                                                  $gridconfig(overt_name) 2 \
                                                  $standardname] 0]
}
