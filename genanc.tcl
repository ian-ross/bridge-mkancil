proc genancwin {win filenum} {

   global genanc ww2 genanc_xold gfilenum gfieldnum setlev

   set genanc_xold 0
   set gfilenum $filenum
   set gfieldnum 1
   set setlev(genanc) 2

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

   title $ww0.title "Generalised ancillary file $filenum"
   spacer $ww0.spacer0
   create_win2 $ww0.create $wh0 "Create Generalised ancillary file $filenum? " \
               genanc($filenum,create)

   spacer $ww0.spacer1
   set_filename $ww1.outfile \
                "Enter output Generalised ancillary file $filenum name: " \
                genanc($filenum,file_out)
   spacer $ww1.spacer2
   set_var3 $ww1.model "Model type: " genanc($filenum,model) \
            "Atmosphere " 1 "Ocean      " 2 "Slab       " 3 \
            "update_all_field_val genanc \$ww2 \$gfilenum ; \
             set_field_val genanc \$ww2 \$gfilenum \$gfieldnum; \
             if {\$genanc(\$gfilenum,\$gfieldnum,masktype) != 0 && \
                 \$genanc(\$gfilenum,model) == 2} { \
                 set genanc(\$gfilenum,\$gfieldnum,mask) 0} ; \
             if {\$genanc(\$gfilenum,\$gfieldnum,masktype) == 2 && \
                 \$genanc(\$gfilenum,model) == 2} { \
                 set genanc(\$gfilenum,\$gfieldnum,masktype) 1} ; \
             set_win_state $ww2.frame.mask1.but3 \
                           \"\$genanc($gfilenum,model) != 2\" ; \
             set_win_state \"$ww2.frame.mask2.mask2\" \
                           \"(\$genanc($gfilenum,model) == 1 || \$genanc($gfilenum,model) == 3) && \
                             \$genanc($gfilenum,$gfieldnum,masktype) != 0 && \
                             (\$mask(use) || \$mask(create))\" ; \
             set_win_state \"$ww2.frame.mask2.mask3\" \
                           \"(\$genanc($gfilenum,model) == 1 || \$genanc($gfilenum,model) == 3) && \
                             \$genanc($gfilenum,$gfieldnum,masktype) != 0 && \
                             (\$lfrac(use) || \$lfrac(create))\""
   spacer $ww1.spacer3
   set_boolvar2 $ww1.per \
                "Is Generalised ancillary file $filenum periodic in time? " \
                genanc($filenum,periodic)
   set_boolvar2 $ww1.inthd8 "Change value of integer header 8? " \
                genanc($filenum,inthd8) \
                "set_win_state $ww1.inthd8val \$genanc($filenum,inthd8)"
   set_var $ww1.inthd8val "Enter value of integer header 8: " \
                          genanc($filenum,inthd8val) 8
   spacer $ww1.spacer4
   select_window $ww1.date1 $wh1 \
      "Use dates from NetCDF file                 " \
      "Specify Generalised ancillary file $filenum dates " \
      genanc($filenum,timeusage1)
   spacer $ww1.spacer5
   set_date $wh1.date2 "Generalised ancillary file $filenum" genanc $filenum
   spacer $wh1.spacer6

   set_var_spin $ww2.nfields "Enter number of ancillary fields: " \
                genanc($filenum,nfield) 5
   spacer $ww2.spacer7

   select_field_list $ww2.fieldlist "Fields in Ancillary file"
   spacer $ww2.spacer8

   update_fieldlist genanc $filenum
   $ww2.fieldlist.box.list selection set 0
   set_field_val genanc $ww2 $filenum 1

#  Pack hidden frames if required

   if {$genanc($filenum,create)} {pack $wh0 -expand yes -fill x -side bottom}
   if {$genanc($filenum,timeusage1) == 1} {pack $wh1 -expand yes -fill x -side top}

#  Add actions to variable changes

   trace add variable genanc($filenum,nfield) write \
         "update_fieldlist genanc $filenum"

#  Add action to field selections

   bind $ww2.fieldlist.box.list <ButtonPress-1> {+
      global gfieldnum
      set gfieldnum [expr {[%W index @%x,%y]+1}]
      if {$gfieldnum > 0} {set_field_val genanc $ww2 $gfilenum $gfieldnum}
   }

   set_win_state $ww1.inthd8val $genanc($filenum,inthd8)
}
