proc ausrmultiwin {win} {

   global ausrmulti ww2 ausrmulti_xold gfieldnum setlev stashrange

   set ausrmulti_xold 0
   set gfieldnum 1
   set setlev(ausrmulti) 1
   set stashrange(ausrmulti) "321 340"

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
               ausrmulti(create)

   spacer $ww0.spacer1
   set_filename $ww1.outfile \
                "Enter output Multi-level User ancillary file name: " \
                ausrmulti(file_out)
   spacer $ww1.spacer2
   set_boolvar2 $ww1.per \
                "Is Multi-level User ancillary file periodic in time? " \
                ausrmulti(periodic)
   spacer $ww1.spacer4
   select_window $ww1.date1 $wh1 \
      "Use dates from NetCDF file                    " \
      "Specify Multi-level User ancillary file dates " \
      ausrmulti(timeusage1)
   spacer $ww1.spacer5
   set_date $wh1.date2 "Multi-level User ancillary file" ausrmulti
   spacer $wh1.spacer6

   set_var_spin $ww2.nfields "Enter number of ancillary fields: " \
                ausrmulti(nfield) 5
   spacer $ww2.spacer7

   select_field_list $ww2.fieldlist "Fields in Ancillary file"
   spacer $ww2.spacer8

   update_fieldlist ausrmulti ""
   $ww2.fieldlist.box.list selection set 0
   set_field_val ausrmulti $ww2 "" 1

#  Pack hidden frames if required

   if {$ausrmulti(create)} {pack $wh0 -expand yes -fill x -side bottom}
   if {$ausrmulti(timeusage1) == 1} {pack $wh1 -expand yes -fill x -side top}

#  Add actions to variable changes

   trace add variable ausrmulti(nfield) write "update_fieldlist ausrmulti {}"

#  Add action to field selections

   bind $ww2.fieldlist.box.list <ButtonPress-1> {+
      global gfieldnum
      set gfieldnum [expr {[%W index @%x,%y]+1}]
      if {$gfieldnum > 0} {set_field_val ausrmulti $ww2 "" $gfieldnum}
   }
}
