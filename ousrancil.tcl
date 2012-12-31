proc ousrancilwin {win} {

   global ousrancil ww2 ousrancil_xold gfieldnum setlev stashrange

   set ousrancil_xold 0
   set gfieldnum 1
   set setlev(ousrancil) 0
   set stashrange(ousrancil) "331 340"

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

   title $ww0.title "Single-level User Fields"
   spacer $ww0.spacer0
   create_win2 $ww0.create $wh0 "Create Single-level User ancillary file? " \
               ousrancil(create)

   spacer $ww0.spacer1
   set_filename $ww1.outfile \
                "Enter output Single-level User ancillary file name: " \
                ousrancil(file_out)
   spacer $ww1.spacer2
   set_boolvar2 $ww1.per \
                "Is Single-level User ancillary file periodic in time? " \
                ousrancil(periodic)
   spacer $ww1.spacer4
   select_window $ww1.date1 $wh1 \
      "Use dates from NetCDF file                    " \
      "Specify Single-level User ancillary file dates " \
      ousrancil(timeusage1)
   spacer $ww1.spacer5
   set_date $wh1.date2 "Single-level User ancillary file" ousrancil
   spacer $wh1.spacer6

   set_var_spin $ww2.nfields "Enter number of ancillary fields: " \
                ousrancil(nfield) 5
   spacer $ww2.spacer7

   select_field_list $ww2.fieldlist "Fields in Ancillary file"
   spacer $ww2.spacer8

   update_fieldlist ousrancil ""
   $ww2.fieldlist.box.list selection set 0
   set_field_val ousrancil $ww2 "" 1

#  Pack hidden frames if required

   if {$ousrancil(create)} {pack $wh0 -expand yes -fill x -side bottom}
   if {$ousrancil(timeusage1) == 1} {pack $wh1 -expand yes -fill x -side top}
   set_win_state $ww2.frame.mask1.but3 false
   set_win_state $ww2.frame.mask2.mask2 false
   set_win_state $ww2.frame.mask2.mask3 false

#  Add actions to variable changes

   trace add variable ousrancil(nfield) write "update_fieldlist ousrancil {}"

#  Add action to field selections

   bind $ww2.fieldlist.box.list <ButtonPress-1> {+
      global gfieldnum
      set gfieldnum [expr {[%W index @%x,%y]+1}]
      if {$gfieldnum > 0} {set_field_val ousrancil $ww2 "" $gfieldnum}
   }
}
