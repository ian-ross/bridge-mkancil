proc astart2win {win} {

   global astart

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  ww1 and ww2 are hidden frames

   set ww1 $ww0.ww1
   frame $ww1
   set ww2 $ww1.input
   frame $ww2

   title $ww0.title "Atmosphere Start Dump Extra Fields"
   spacer $ww0.spacer1
   create_win2 $ww0.create $ww1 "Create Atmosphere Start Dump file? " astart(create)
   create_win2 $ww1.addextra $ww2 "Add extra Atmosphere Start Dump fields? " astart(addextra)
   spacer $ww2.spacer2

#  Pack hidden frames if required

   if {$astart(create)} {pack $ww1 -expand yes -fill x -side bottom}
   if {$astart(addextra)} {pack $ww2 -expand yes -fill x -side bottom}
   
}
