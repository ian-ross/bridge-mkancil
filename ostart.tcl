proc ostartwin {win} {

   global ostart ostart_ppwin ostart_newstashwin ostart_clstashsel

   set ostart(sctmp) ""
   set ostart(ncftmp) ""
   set ostart(ncvtmp) ""
   set ostart(newsctmp) "n/a"
   set ostart(sccltmp) "n/a"
   set ostart(pptmp) "n/a"

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

   set wh0 $ww0.wh0
   frame $wh0

   set ww1 $wh0.ww1
   frame $ww1 -bd 2
   pack $ww1 -expand yes -fill x -side top
   set ww2 $wh0.ww2
   frame $ww2 -bd 2
   pack $ww2 -expand yes -fill x -side top
   set ww3 $wh0.ww3
   frame $ww3 -bd 2
   pack $ww3 -expand yes -fill x -side top
   set ww4 $wh0.ww4
   frame $ww4 -bd 2
   pack $ww4 -expand yes -fill x -side top

   set wh2 $ww2.wh2
   frame $wh2
   set wh3 $ww3.wh3
   frame $wh3
   set wh4 $ww4.wh4
   frame $wh4

   title $ww0.title "Ocean Start Dump Configuration"
   spacer $ww0.spacer0
   create_win2 $ww0.create $wh0 "Create Ocean Start Dump file? " ostart(create)

   spacer $ww0.spacer1
   get_filename $ww1.infile "Enter input UM Ocean Start Dump file name: " \
                ostart(umfile_in) "*" ostart_uminfile
   spacer $ww1.spacer2
   set_filename $ww1.outfile \
       "Enter output UM Ocean Start Dump file name: " ostart(file_out)
   spacer $ww1.spacer3

   label $ww1.selectlabel -text "Dump file fields from NetCDF files:"
   pack $ww1.selectlabel -side top -fill both -expand yes
   tablelist::tablelist $ww1.mods \
       -columns [list 0 "#" left 0 "Clone" left 0 "PP" left \
                      0 "Stash Name" left 0 "NC Variable" left \
                      0 "NC File" left] \
       -height 0 -width 10 -stretch all -selectmode single -activestyle none \
       -listvariable ostart(mods) -showarrow 0
   for {set c 0} {$c < 6} {incr c} { $ww1.mods columnconfigure $c -editable no }
   $ww1.mods columnconfigure 0 -sortmode integer
   bind $ww1.mods <<TablelistSelect>> "ostart_selection_change %W"
   pack $ww1.mods -side top -expand yes -fill both

   spacer $ww1.spacer4
   frame $ww1.butbox -bd 2
   button $ww1.butbox.add -width 8 -text "Add" \
       -command "add_ostart_entry $ww1.mods"
   button $ww1.butbox.upd -width 8 -text "Update" \
       -command "upd_ostart_entry $ww1.mods"
   button $ww1.butbox.del -width 8 -text "Delete" \
       -command "del_ostart_entry $ww1.mods"
   pack $ww1.butbox -side top -fill both -expand yes
   pack $ww1.butbox.add $ww1.butbox.upd $ww1.butbox.del -side left -expand yes
   spacer $ww1.spacer5

   frame $ww1.stash
   pack $ww1.stash -side top -fill both -expand yes
   label $ww1.stash.label -text "Stash code:"
   combobox::combobox $ww1.stash.stashcode -textvariable ostart(sctmp) \
                      -listvar stashlabellist \
                      -width 5 -borderwidth 2 -elementborderwidth 2 \
                      -highlightthickness 1 -editable no
   pack $ww1.stash.label -side left -fill none -expand no
   bind [$ww1.stash.stashcode subwidget listbox] <<ListboxSelect>> \
       "ostart_stashcode_selection_change %W"
   pack $ww1.stash.stashcode -side left -fill both -expand yes -anchor e
   get_ostart_ncfilename $ww1.ncfile "NetCDF file name: " \
       ostart(ncftmp) nc ostart_ncfile
   select_varname1 $ww1.varname "NetCDF variable name: " \
       ostart(ncvtmp) ostart(ncftmp)
   spacer $ww1.spacer7
   set_var $ww1.newstash "New stashcode:" ostart(newsctmp) 0 yes
   frame $ww1.stash2
   pack $ww1.stash2 -side top -fill both -expand yes
   label $ww1.stash2.label -text "Clone stashcode:"
   combobox::combobox $ww1.stash2.stashcode -textvariable ostart(sccltmp) \
                      -listvar clonestashlabellist \
                      -width 5 -borderwidth 2 -elementborderwidth 2 \
                      -highlightthickness 1 -editable no
   pack $ww1.stash2.label -side left -fill none -expand no
   pack $ww1.stash2.stashcode -side left -fill both -expand yes -anchor e
   set_var $ww1.pp "PP code:" ostart(pptmp) 0 yes
   set ostart_newstashwin $ww1.newstash
   set ostart_ppwin $ww1.pp
   set ostart_clstashsel $ww1.stash2
   set_win_state $ostart_clstashsel false
   set_win_state $ostart_ppwin false
   set_win_state $ostart_newstashwin false
   spacer $ww1.spacer8

   select_window_dump $ww2.date1 $wh2 \
      "Use date from NetCDF file                            " \
      "Use date from UM start dump                          " \
      "Specify Ocean start dump date                   " ostart(timeusage1)
   spacer $wh2.spacer9
   set_dumpdate $wh2.date2 "Ocean start" ostart
   spacer $wh2.spacerA

   create_win2 $ww3.bathycreate $wh3 "Modify Ocean Bathymetry? " ostart(bathy)
   get_filename $wh3.bathyfile "Bathymetry NetCDF file name: " \
       ostart(bathyfile) nc
   select_varname1 $wh3.bathyncname "Bathymetry NetCDF variable name: " \
       ostart(bathyncname) ostart(bathyfile)
   set_var2 $wh3.bathydepthmask "Bathymetry variable is depth mask" \
       ostart(bathydepthmask) "yes" 1 "no " 0
   spacer $wh3.spacerB

   create_win2 $ww4.islandcreate $wh4 "Modify Islands Data? " ostart(islandmod)
   set_var2 $wh4.islandtype "Replace or add islands? " \
       ostart(islandtype) "Replace" 1 "Add    " 2
   get_filename $wh4.islandfile "Island input data file name: " \
       ostart(islandfile) "*"
   spacer $wh4.spacerC

#  Pack hidden frames if required

   if {$ostart(create)} {pack $wh0 -expand yes -fill x -side bottom}
   if {$ostart(timeusage1) == 2} {pack $wh2 -expand yes -fill x -side top}
   if {$ostart(bathy)} {pack $wh3 -expand yes -fill x -side bottom}
   if {$ostart(islandmod)} {pack $wh4 -expand yes -fill x -side bottom}
}


proc ostart_uminfile {win file} {
    global stashlist stashlabellist clonestashlabellist origstashlabellist
    global ostart
    if {$file == "" || ![file exists $file]} {
        set origstashlabellist [list "n/a"]
        set clonestashlabellist [list "n/a"]
        set stashlabellist ""
        set ostart(mods) {}
    } else {
        umfilelist $win $file
        set mnew {}
        foreach m $ostart(mods) {
            if {[lsearch $stashlist [lindex $m 0]] != -1} {
                set mnew [lappend $mnew $m]
            }
        }
        if {[llength $ostart(mods)] > [llength $mnew]} {
            write_message \
                "Removing mods for stash codes not in selected UM input file"
        }
        set ostart(mods) $mnew
        set ostart(sctmp) ""
        set ostart(ncftmp) ""
        set ostart(ncvtmp) ""
        set ostart(newsctmp) "n/a"
        set ostart(sccltmp) "n/a"
        set ostart(pptmp) ""

        # Add possible new stash codes
        set origstashlabellist $stashlabellist
        set clonestashlabellist [list "n/a"]
        set stashlabellist [linsert $stashlabellist 0 "New stashcode..."]
    }
}


proc get_ostart_ncfilename {win label filevar {filter *} {filecom {}}} {
    global ostart
    frame $win
    pack $win -side top -fill both -expand yes
    upvar #0 $filevar file

    label $win.fileLabel -text $label
    frame $win.file
    combobox::combobox $win.file.fileEntry -textvariable $filevar \
                       -listvar ostart(ncfile) \
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

proc ostart_ncfile {win ncfile} {
    global ostart
    if {$ncfile == ""} {return}
    if {! [file exists $ncfile]} {return}
    set index [lsearch -exact $ostart(ncfile) $ncfile]
    if {$index == -1} {
        lappend ostart(ncfile) $ncfile
        set index [expr {[llength $ostart(ncfile)] - 1}]
        set ostart(ncvtmp) ""
    } else {
        getncvarinfo $ostart(ncftmp) ncinfo
        set ncvars $ncinfo(var_name)
        if {[lsearch -exact $ncinfo(var_name) $ostart(ncvtmp)] == -1} {
            set ostart(ncvtmp) ""
        }
    }
    if {$win != ""} {$win select $index}
}


proc add_ostart_entry {win} {
    global ostart stashlist
    if {$ostart(sctmp)=="" || $ostart(ncftmp)=="" || \
            $ostart(ncvtmp)==""} {return}
    set stashcode [lindex [split $ostart(sctmp)] 0]
    set clstashcode [lindex [split $ostart(sccltmp)] 0]
    set stashname [string trim [string range $ostart(sctmp) \
                                    [string wordend $ostart(sctmp) 0] end]]
    set newpp [string trim $ostart(pptmp)]
    set err ""
    if {$stashcode == "New"} {
        set newstash [string trim $ostart(newsctmp)]
        if {! [regexp {[0-9]+} $newstash]} {
            set err "Invalid new stashcode"
        } else {
            if {! [regexp {[0-9]+} $newpp]} {
                set err "Invalid new PP code"
            } else {
                if {[lsearch $stashlist $ostart(newsctmp)] != -1} {
                    set err "New stash code already exists in dump file"
                } else {
                    set stashcode $newstash
                    set stashname "New field"
                }
            }
        }
    }
    foreach chk $ostart(mods) {
        if {$stashcode == [lindex $chk 0]} {
            set err "Stash code is already in use"
        }
    }
    if {$err == ""} {
        lappend ostart(mods) \
            [list $stashcode $clstashcode $newpp $stashname \
                 $ostart(ncvtmp) $ostart(ncftmp)]
        $win sortbycolumn 0
    } else {
        errorbox $err
    }
}


proc upd_ostart_entry {win} {
    global ostart stashlist
    set selidx [$win curselection]
    if {[llength $selidx]==0} {return}
    if {$ostart(sctmp)=="" || $ostart(ncftmp)=="" || \
            $ostart(ncvtmp)==""} {return}
    set stashcode [lindex [split $ostart(sctmp)] 0]
    set clstashcode [lindex [split $ostart(sccltmp)] 0]
    set stashname [string trim [string range $ostart(sctmp) \
                                    [string wordend $ostart(sctmp) 0] end]]
    set newpp [string trim $ostart(pptmp)]
    set err ""
    if {$stashcode == "New"} {
        set newstash [string trim $ostart(newsctmp)]
        if {! [regexp {[0-9]+} $newstash]} {
            set err "Invalid new stashcode"
        } else {
            if {! [regexp {[0-9]+} $newpp]} {
                set err "Invalid new PP code"
            } else {
                if {[lsearch $stashlist $ostart(newsctmp)] != -1} {
                    set err "New stash code already exists in dump file"
                } else {
                    set stashcode $newstash
                    set stashname "New field"
                }
            }
        }
    }
    set idx 0
    foreach chk $ostart(mods) {
        if {$idx != $selidx} {
            if {$stashcode == [lindex $chk 0]} {
                set err "Stash code is already in use"
            }
        }
        incr idx
    }
    if {$err == ""} {
        set newent [list $stashcode $clstashcode $newpp \
                        $stashname $ostart(ncvtmp) $ostart(ncftmp)]
        set ostart(mods) [lreplace $ostart(mods) $selidx $selidx $newent]
        $win sortbycolumn 0
    } else {
        errorbox $err
    }
}


proc del_ostart_entry {win} {
    global ostart
    set s [lindex [$win curselection] 0]
    $win selection clear 0 [$win size]
    set ostart(mods) [lreplace $ostart(mods) $s $s]
    $win sortbycolumn 0
    set ostart(sctmp) ""
    set ostart(ncftmp) ""
    set ostart(ncvtmp) ""
    set ostart(newsctmp) "n/a"
    set ostart(sccltmp) "n/a"
    set ostart(pptmp) "n/a"
}


proc ostart_selection_change {win} {
    global ostart stashlist clonestashlabellist origstashlabellist
    global ostart_ppwin ostart_newstashwin ostart_clstashsel
    set sel [lindex $ostart(mods) [$win curselection]]
    if {[lsearch $stashlist [lindex $sel 0]] == -1} {
        set ostart(sctmp) "New stashcode..."
        set ostart(newsctmp) [lindex $sel 0]
    } else {
        set ostart(sctmp) [join [list [lindex $sel 0] [lindex $sel 3]]]
        set ostart(newsctmp) "n/a"
    }
    set ostart(ncvtmp) [lindex $sel 4]
    set ostart(ncftmp) [lindex $sel 5]
    if {$ostart(sctmp) == "New stashcode..."} {
        set clonestashlabellist $origstashlabellist
        set clsre [join [list "^" [lindex $sel 1] " "] ""]
        set clidx [lsearch -regexp $clonestashlabellist $clsre]
        if {$clidx == -1} {
            set ostart(sccltmp) "n/a"
        } else {
            set ostart(sccltmp) [lindex $clonestashlabellist $clidx]
        }
        set ostart(pptmp) [lindex $sel 2]
        set_win_state $ostart_clstashsel true
        set_win_state $ostart_ppwin true
        set_win_state $ostart_newstashwin true
    } else {
        set clonestashlabellist {"n/a"}
        set ostart(sccltmp) "n/a"
        set ostart(newsctmp) "n/a"
        set ostart(pptmp) "n/a"
        set_win_state $ostart_clstashsel false
        set_win_state $ostart_ppwin false
        set_win_state $ostart_newstashwin false
    }
}


proc ostart_stashcode_selection_change {win} {
    global ostart stashlabellist clonestashlabellist origstashlabellist
    global ostart_ppwin ostart_newstashwin ostart_clstashsel
    set stashsel [lindex $stashlabellist [$win curselection]]
    if {[regexp {^New} $stashsel]} {
        set clonestashlabellist $origstashlabellist
        set ostart(sccltmp) [lindex $clonestashlabellist 0]
        set ostart(newsctmp) ""
        set ostart(pptmp) ""
        set_win_state $ostart_clstashsel true
        set_win_state $ostart_ppwin true
        set_win_state $ostart_newstashwin true
    } else {
        set clonestashlabellist {"n/a"}
        set ostart(newsctmp) "n/a"
        set ostart(sccltmp) "n/a"
        set ostart(pptmp) "n/a"
        set_win_state $ostart_clstashsel false
        set_win_state $ostart_ppwin false
        set_win_state $ostart_newstashwin false
    }
}
