proc ousrmultiwin {win} {

   global ousrmulti ww2 ousrmulti_xold gfieldnum setlev stashrange

   set ousrmulti_xold 0
   set gfieldnum 1
   set setlev(ousrmulti) 0
   set stashrange(ousrmulti) "351 354"

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  wh0 is a hidden frame

   set wh0 $ww0.wh0
   frame $wh0

   set ww1 $wh0.ww1
   frame $ww1 -bd 2
   pack $ww1 -expand yes -fill x -side top

   set ww2 $wh0.ww2
   frame $ww2 -bd 2
   pack $ww2 -expand yes -fill x -side top

#  wh1 is a hidden frame

   set wh1 $ww1.wh1
   frame $wh1

   title $ww0.title "Multi-level User Fields"
   spacer $ww0.spacer0
   create_win2 $ww0.create $wh0 "Create Multi-level User ancillary file? " \
               ousrmulti(create)

   spacer $ww0.spacer1
   set_filename $ww1.outfile \
                "Enter output Multi-level User ancillary file name: " \
                ousrmulti(file_out)
   spacer $ww1.spacer2
   set_boolvar2 $ww1.per \
                "Is Multi-level User ancillary file periodic in time? " \
                ousrmulti(periodic)
   spacer $ww1.spacer4
   select_window $ww1.date1 $wh1 \
      "Use dates from NetCDF file                    " \
      "Specify Multi-level User ancillary file dates " \
      ousrmulti(timeusage1)
   spacer $ww1.spacer5
   set_date $wh1.date2 "Multi-level User ancillary file" ousrmulti
   spacer $wh1.spacer6

   set_var_spin $ww2.nfields "Enter number of ancillary fields: " \
                ousrmulti(nfield) 5
   spacer $ww2.spacer7

   select_field_list $ww2.fieldlist "Fields in Ancillary file"
   spacer $ww2.spacer8

   update_fieldlist ousrmulti ""
   $ww2.fieldlist.box.list selection set 0
   set_field_val ousrmulti $ww2 "" 1

#  Pack hidden frames if required

   if {$ousrmulti(create)} {pack $wh0 -expand yes -fill x -side bottom}
   if {$ousrmulti(timeusage1) == 1} {pack $wh1 -expand yes -fill x -side top}
   set_win_state $ww2.frame.mask1.but3 false
   set_win_state $ww2.frame.mask2.mask2 false
   set_win_state $ww2.frame.mask2.mask3 false

#  Add actions to variable changes

   trace add variable ousrmulti(nfield) write "update_fieldlist ousrmulti {}"

#  Add action to field selections

   bind $ww2.fieldlist.box.list <ButtonPress-1> {+
      global gfieldnum
      set gfieldnum [expr {[%W index @%x,%y]+1}]
      if {$gfieldnum > 0} {set_field_val ousrmulti $ww2 "" $gfieldnum}
   }
}
