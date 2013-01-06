proc astartwin {win} {

   global astart

   set astart(sctmp) ""
   set astart(ncftmp) ""
   set astart(ncvtmp) ""

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  wh0 is a hidden frames

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

   title $ww0.title "Atmosphere Start Dump Configuration"
   spacer $ww0.spacer0
   create_win2 $ww0.create $wh0 "Create Atmosphere Start Dump file? " astart(create)

   spacer $ww0.spacer1
   get_filename $ww1.infile "Enter input UM Atmosphere Start Dump file name: " \
                astart(umfile_in) "*" astart_uminfile
   spacer $ww1.spacer2
   set_filename $ww1.outfile "Enter output UM Atmosphere Start Dump file name: " astart(file_out)
   spacer $ww1.spacer3

   label $ww1.selectlabel -text "Dump file fields to modify:"
   pack $ww1.selectlabel -side top -fill both -expand yes
   tablelist::tablelist $ww1.mods \
       -columns [list 0 "#" left 0 "Stash Name" left \
                      0 "NC Variable" left 0 "NC File" left] \
       -height 0 -width 10 -stretch all -selectmode single -activestyle none \
       -listvariable astart(mods) -showarrow 0
   for {set c 0} {$c < 4} {incr c} { $ww1.mods columnconfigure $c -editable no }
   $ww1.mods columnconfigure 0 -sortmode integer
   bind $ww1.mods <<TablelistSelect>> "astart_selection_change %W"
   pack $ww1.mods -side top -expand yes -fill both

   spacer $ww1.spacer4
   frame $ww1.butbox -bd 2
   button $ww1.butbox.add -width 8 -text "Add" -command "add_astart_entry $ww1.mods"
   button $ww1.butbox.upd -width 8 -text "Update" -command "upd_astart_entry $ww1.mods"
   button $ww1.butbox.del -width 8 -text "Delete" -command "del_astart_entry $ww1.mods"
   pack $ww1.butbox -side top -fill both -expand yes
   pack $ww1.butbox.add $ww1.butbox.upd $ww1.butbox.del -side left -expand yes
   spacer $ww1.spacer5

   frame $ww1.stash
   pack $ww1.stash -side top -fill both -expand yes
   label $ww1.stash.label -text "Stash code:"
   combobox::combobox $ww1.stash.stashcode -textvariable astart(sctmp) \
                      -listvar stashlabellist \
                      -width 5 -borderwidth 2 -elementborderwidth 2 \
                      -highlightthickness 1 -editable no
   pack $ww1.stash.label -side left -fill none -expand no
   pack $ww1.stash.stashcode -side left -fill both -expand yes -anchor e
   get_astart_ncfilename $ww1.ncfile "NetCDF file name: " astart(ncftmp) nc astart_ncfile
   select_varname1 $ww1.varname "NetCDF variable name: " astart(ncvtmp) astart(ncftmp)
   spacer $ww1.spacer7

   select_window_dump $ww1.date1 $wh1 \
      "Use date from NetCDF file                            " \
      "Use date from UM start dump                          " \
      "Specify Atmosphere start dump date                   " astart(timeusage1)
   spacer $ww1.spacer8
   set_dumpdate $wh1.date2 "Atmosphere start" astart
   spacer $ww1.spacer9

#  Pack hidden frames if required

   if {$astart(create)} {pack $wh0 -expand yes -fill x -side bottom}
   if {$astart(timeusage1) == 2} {pack $wh1 -expand yes -fill x -side top}
}


proc astart_uminfile {win file} {
    global stashlist stashlabellist astart
    if {$file == "" || ![file exists $file]} {
        set stashlabellist ""
        set astart(mods) {}
    } else {
        umfilelist $win $file
        set mnew {}
        foreach m $astart(mods) {
            if {[lsearch $stashlist [lindex $m 0]] != -1} { set mnew [lappend $mnew $m] }
        }
        if {[llength $astart(mods)] > [llength $mnew]} {
            write_message "Removing modifications for stash codes not in selected UM input file"
        }
        set astart(mods) $mnew
        set astart(sctmp) ""
        set astart(ncftmp) ""
        set astart(ncvtmp) ""
    }
}


proc get_astart_ncfilename {win label filevar {filter *} {filecom {}}} {
    global astart
    frame $win
    pack $win -side top -fill both -expand yes
    upvar #0 $filevar file

    label $win.fileLabel -text $label
    frame $win.file
    combobox::combobox $win.file.fileEntry -textvariable $filevar \
                       -listvar astart(ncfile) \
                       -borderwidth 2 -elementborderwidth 2 \
                       -command get_filename_select \
                       -highlightthickness 1 -editable false
    button $win.file.fileButton -padx 2 -pady 1 -text "Browse" \
           -command "getfile $filevar $filter \"$filecom $win.file.fileEntry\""
    pack $win.fileLabel -side left -fill none -expand no
    pack $win.file -side left -fill both -expand yes
    pack $win.file.fileEntry -side left -fill both -expand yes
    pack $win.file.fileButton -ipady 1 -padx 4 -side left -fill both -expand no

    if {$filecom != ""} {
        bind [$win.file.fileEntry subwidget entry] <Destroy> \
             "$filecom {} $$filevar"
        bind [$win.file.fileEntry subwidget entry] <Return> \
             "$filecom $win.file.fileEntry $$filevar"
        bind [$win.file.fileEntry subwidget listbox] <Unmap> \
             "$filecom $win.file.fileEntry $$filevar"
        $filecom $win.file.fileEntry $file
    }
}

proc astart_ncfile {win ncfile} {
    global astart
    if {$ncfile == ""} {return}
    if {! [file exists $ncfile]} {return}
    set index [lsearch -exact $astart(ncfile) $ncfile]
    if {$index == -1} {
        lappend astart(ncfile) $ncfile
        set index [expr {[llength $astart(ncfile)] - 1}]
        set astart(ncvtmp) ""
    } else {
        getncvarinfo $astart(ncftmp) ncinfo
        set ncvars $ncinfo(var_name)
        if {[lsearch -exact $ncinfo(var_name) $astart(ncvtmp)] == -1} {
            set astart(ncvtmp) ""
        }
    }
    if {$win != ""} {$win select $index}
}


proc add_astart_entry {win} {
    global astart
    if {$astart(sctmp)=="" || $astart(ncftmp)=="" || $astart(ncvtmp)==""} {return}
    set stashcode [lindex [split $astart(sctmp)] 0]
    set stashname [string trim [string range $astart(sctmp) [string wordend $astart(sctmp) 0] end]]
    set ok 1
    foreach chk $astart(mods) {
        if {$stashcode==[lindex $chk 0]} {
            errorbox "Stash code is already in use"
            set ok 0
        }
    }
    if {$ok} {
        lappend astart(mods) [list $stashcode $stashname $astart(ncvtmp) $astart(ncftmp)]
    }
    $win sortbycolumn 0
}


proc upd_astart_entry {win} {
    global astart
    set selidx [$win curselection]
    if {[llength $selidx]==0} {return}
    if {$astart(sctmp)=="" || $astart(ncftmp)=="" || $astart(ncvtmp)==""} {return}
    set scode [lindex [split $astart(sctmp)] 0]
    set sname [string trim [string range $astart(sctmp) [string wordend $astart(sctmp) 0] end]]
    set ok 1
    set idx 0
    foreach chk $astart(mods) {
        if {$idx != $selidx} {
            if {$scode==[lindex $chk 0]} {
                errorbox "Stash code is already in use"
                set ok 0
            }
        }
        incr idx
    }
    if {$ok} {
        set newent [list $scode $sname $astart(ncvtmp) $astart(ncftmp)]
        set astart(mods) [lreplace $astart(mods) $selidx $selidx $newent]
        $win sortbycolumn 0
    }
}


proc del_astart_entry {win} {
    global astart
    set s [lindex [$win curselection] 0]
    set astart(mods) [lreplace $astart(mods) $s $s]
    $win sortbycolumn 0
}


proc astart_selection_change {win} {
    global astart
    set sel [lindex $astart(mods) [$win curselection]]
    set astart(sctmp) [join [lrange $sel 0 1]]
    set astart(ncvtmp) [lindex $sel 2]
    set astart(ncftmp) [lindex $sel 3]
}
