proc astart1win {win} {

   global astart selectname

   if {! [info exists selectname]} {
      set selectname stash
   }

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  ww1 and ww2 are hidden frames

   set ww1 $ww0.ww1
   frame $ww1
   set ww2 $ww1.input
   frame $ww2

   title $ww0.title "Atmosphere Start Dump Fields, Use Existing Dump"
   spacer $ww0.spacer1
   create_win2 $ww0.create $ww1 "Create Atmosphere Start Dump file? " \
               astart(create)
   create_win2 $ww1.useexist $ww2 "Use existing Atmosphere Start Dump file? " \
                astart(useexist)
   spacer $ww2.spacer2
   #get_filename $ww2.infile "Enter input UM Atmosphere Start Dump file name: " \
   #             astart(umfile_in) "*" "\"umfilelist $ww2\""
   get_filename $ww2.infile "Enter input UM Atmosphere Start Dump file name: " \
                astart(umfile_in) "*" umfilelist
   spacer $ww2.spacer3
   set_boolvar2 $ww2.usestdname \
      "Identify fields by standardname if available? " astart(usestdname)
   spacer $ww2.spacer4
   set_var2 $ww2.selectname "Select field name type: " selectname \
            "Stash    " stash "Standard " standard \
            "umfilelist $ww2 \$astart(umfile_in) 0"
   spacer $ww2.spacer5
   select_fields $ww2.selectfields "#" "Stash Name" "Var Name" "File ID" \
                 astart(itemid) astart(ncname) astart(ncfileid)
   spacer $ww2.spacer6

#  Pack hidden frames if required

   if {$astart(create)} {pack $ww1 -expand yes -fill x -side bottom}
   if {$astart(useexist)} {pack $ww2 -expand yes -fill x -side bottom}
   
   bind $ww2.infile.file.fileEntry <Map> "puts {***2} ; umfilelist $ww2 \$astart(umfile_in) 0"
   bind $ww2.infile.file.fileEntry <Return> "puts {***3} ; umfilelist $ww2 \$astart(umfile_in) 1"
}
