# Right hand side functions

proc config {} {
   global w4
   global config

   clear_right

   scrolledframe::scrolledframe $w4.frame -yscrollcommand "$w4.yscroll set" \
                                -fill x -bd 2
   pack $w4.frame -side left -fill both -expand yes

   scrollbar $w4.yscroll -command "$w4.frame yview" -width 0 -bd 0
   pack $w4.yscroll -side right -fill y

   set ww $w4.frame.scrolled
   bindtags $ww "[bindtags $ww] RightBox"

   title $ww.title "General configuration"
   spacer $ww.spacer1
   set_var $ww.ver "Version number: " config(version) 5
   spacer $ww.spacer2
   set_var2 $ww.cal "Calendar type: " config(cal) "Gregorian " 1 "360       " 2
   spacer $ww.spacer4
   set_var2 $ww.size "Ancillary file data size: " config(size) "32 bit    " 32 "64 bit    " 64 \
      "set_win_state $ww.pack32 \"\$config(size) == 64\""
   set_var2 $ww.pack32 "Use 32 bit data packing? " config(pack32) "yes       " 1 "no        " 0
   set_var2 $ww.end "Endianness of ancillary file: " config(end) "little    " 0 "big       " 1
   set_var2 $ww.wfio "Output well-formed ancillary files? " config(wfio) \
      "yes       " 1 "no        " 0 "set_win_state $ww.wfiosize \$config(wfio)"
   spacer $ww.spacer5
   set_var $ww.wfiosize "Well-formed I/O sector size: " config(wfio_size) 8
   
   set_win_state $ww.pack32 "$config(size) == 64"
   set_win_state $ww.wfiosize $config(wfio)

   spacer $ww.spacer6
   getfilebox $ww.getncfiles "NetCDF files to be read" config(ncfile) "nc"
   spacer $ww.spacer7

   spacer $ww.spacer8
   getfilebox $ww.getsmfiles "STASH Master files to be read" config(smfile) "*" sm_com
   spacer $ww.spacer9

#  Add action to netcdf file double clicks

   bind $ww.getncfiles.box.list <Double-1> {
      set ncfile [%W get @%x,%y]
      select_ncvarname $ncfile
   }
}

proc sm_com {action filelist} {
   if {$action == "add"} {
      adduserstashmaster $filelist
   } else {
      deluserstashmaster $filelist
   }
}

proc switchancil {} {
   global w4 bold_font_large spacerwidth spacerht
   global ancil_types
   foreach type $ancil_types {global $type}

   clear_right

   scrolledframe::scrolledframe $w4.frame -yscrollcommand "$w4.yscroll set" \
                                -fill x -bd 2
   pack $w4.frame -side left -fill both -expand yes

   scrollbar $w4.yscroll -command "$w4.frame yview" -width 0 -bd 0
   pack $w4.yscroll -side right -fill y

   set ww $w4.frame.scrolled
   bindtags $ww "[bindtags $ww] RightBox"

   title $ww.title "Select Ancillary files to be created"
   spacer $ww.spacer1
   
   ancil_onoff $ww.ozone "Ozone " ozone(create) 1
   ancil_onoff $ww.smow "Soil moisture and snow depth " smow(create) 1
   ancil_onoff $ww.slt "Deep soil temperature " slt(create) 1
   ancil_onoff $ww.soil "Soil parameters " soil(create) 1
   ancil_onoff $ww.veg "Vegetation parameters " veg(create) 1
   ancil_onoff $ww.vegfrac "Vegetation fractions " vegfrac(create) 1
   ancil_onoff $ww.vegfunc "Vegetation functional types " vegfunc(create) 1
   ancil_onoff $ww.vegdist "Disturbed vegetation fraction " vegdist(create) 1
   ancil_onoff $ww.sst "Sea surface temperature " sst(create) 1
   ancil_onoff $ww.ice "Sea ice " ice(create) 1
   ancil_onoff $ww.orog "Orography " orog(create) 1
   ancil_onoff $ww.mask "Land/Sea Mask " mask(create) 1
   ancil_onoff $ww.lfrac "Land fraction " lfrac(create) 1
   ancil_onoff $ww.ausrmulti "Atmosphere multi-level user fields " ausrmulti(create) 1
   ancil_onoff $ww.ausrancil "Atmosphere single-level user fields " ausrancil(create) 1

   spacer $ww.spacer2
   ancil_onoff $ww.ws "Wind and pressure forcing " ws(create) 0
   ancil_onoff $ww.htflux "Thermal forcing " htflux(create) 0
   ancil_onoff $ww.pme "Fresh-water input and water type " pme(create) 0
   ancil_onoff $ww.ts1 "Reference SST, SSS, Air-Temp & Ice-Depth " ts1(create) 1
   ancil_onoff $ww.iceff "Ice Model forcing " iceff(create) 0
   ancil_onoff $ww.flux "Flux Correction " flux(create) 1
   ancil_onoff $ww.ousrmulti "Ocean multi-level user fields " ousrmulti(create) 1
   ancil_onoff $ww.ousrancil "Ocean single-level user fields " ousrancil(create) 1
   ancil_onoff $ww.usrtr "User defined tracer " usrtr(create) 0

   spacer $ww.spacer3
   for {set i 1} {$i <= $genanc_config(nancfile)} {incr i} {
      ancil_onoff $ww.genanc$i "Generalised ancillary file $i " genanc($i,create) 1
   }
   spacer $ww.spacer4
}

proc ancil {type {xarg 0}} {
   global w4

   clear_right

   scrolledframe::scrolledframe $w4.frame -yscrollcommand "$w4.yscroll set" \
                                -fill x -bd 2
   pack $w4.frame -side left -fill both -expand yes

   scrollbar $w4.yscroll -command "$w4.frame yview" -width 0 -bd 0
   pack $w4.yscroll -side right -fill y

   set ww $w4.frame.scrolled
   bindtags $ww "[bindtags $ww] RightBox"

   if {$type == "ozone"} {
      ozonewin $ww
   } elseif {$type == "sst"} {
      sstwin $ww
   } elseif {$type == "smow"} {
      smowwin $ww
   } elseif {$type == "slt"} {
      sltwin $ww
   } elseif {$type == "soil"} {
      soilwin $ww
   } elseif {$type == "veg"} {
      vegwin $ww
   } elseif {$type == "vegfrac"} {
      vegfracwin $ww
   } elseif {$type == "vegfunc"} {
      vegfuncwin $ww
   } elseif {$type == "vegdist"} {
      vegdistwin $ww
   } elseif {$type == "ice"} {
      icewin $ww
   } elseif {$type == "orog"} {
      orogwin $ww
   } elseif {$type == "mask"} {
      maskwin $ww
   } elseif {$type == "lfrac"} {
      lfracwin $ww
   } elseif {$type == "ausrmulti"} {
      ausrmultiwin $ww
   } elseif {$type == "ausrancil"} {
      ausrancilwin $ww
   } elseif {$type == "ousrmulti"} {
      ousrmultiwin $ww
   } elseif {$type == "ousrancil"} {
      ousrancilwin $ww
   } elseif {$type == "ts1"} {
      ts1win $ww
   } elseif {$type == "flux"} {
      fluxwin $ww
   } elseif {$type == "genanc_config"} {
      genanc_configwin $ww $xarg
   } elseif {$type == "genanc"} {
      genancwin $ww $xarg
   } elseif {$type == "astart_config"} {
      astart_configwin $ww
   } elseif {$type == "astart1"} {
      astart1win $ww
   } elseif {$type == "astart2"} {
      astart2win $ww
   } else {
      label $ww.titler -text "$type function not implemented yet" -fg red
      pack $ww.titler -side top
   }

   spacer $ww.spacer1
}

proc ancil_onoff {win text ancil use} {

   if {! $use} {return}

   checkbutton $win -text $text -variable $ancil -highlightthickness 0
   pack $win -side top -fill none -expand no -anchor w
}

# Resize right hand side box widgets when main window is resized

bind RightBox <Configure> {+
   set w [winfo parent %W]
   set sb [lindex [$w cget -yscrollcommand] 0]
   set sbwidth [$sb cget -width]
   #update
   #puts "$w yview = [$w yview]"
   set xy [$w yview]
   set factor [expr [lindex $xy 1]-[lindex $xy 0]]
   if {$factor < 1 && $sbwidth != $scrollbarwidth} {
      #puts "draw sb"
      $sb configure -width $scrollbarwidth -bd 2
   } elseif {$factor >= 1 && $sbwidth != 0} {
      #puts "remove sb"
      $sb configure -width 0 -bd 0
   }
}

