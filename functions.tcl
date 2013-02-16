# Right hand side vertical scroll bar

proc yscrollbar {w1 w2} {
 
   global scrollbarwidth

   scrollbar $w1 -command "$w2 yview" -width $scrollbarwidth
   pack $w1 -side right -fill y
}

# outputmessage
# outputs a (warning/error) message
#
# Arguments:
# message-           text to be output

proc outputmessage {message} {

   .info configure -state normal
   .info insert end "\n  "
   .info insert end "$message"
   .info yview end
   .info configure -state disabled

}

# Run when mouse wheel moved

proc wheelEvent {x y delta} {

    # Find out what widget we're on

    set act 0
    set widget [winfo containing $x $y]

    # Listboxs already work with mouse wheel so skip rest of proc unless
    # there is no scrolling in listbox
    if {[winfo class $widget] == "Listbox"} {
       set xy [$widget yview]
       set factor [expr [lindex $xy 1]-[lindex $xy 0]]
       if {$factor == 1} {
          set widget [winfo parent $widget]
       } else {
          return
       }
    }

    if {$widget != ""} {
        # Make sure we've got a vertical scrollbar for this widget
        if {[catch "$widget cget -yscrollcommand" cmd]} {

           # Otherwise see if widget is inside a canvas or scrolled frame widget
           while {$widget != ""} {
              if {[winfo class $widget] != "Canvas" && \
                  [winfo class $widget] != "Frame"} {
                 set widget [winfo parent $widget]
              } else {
                 if {[catch "$widget cget -yscrollcommand" cmd]} {
                    set widget [winfo parent $widget]
                    continue
                 } else {
                    break
                 }
              }
           }
           if {$widget == ""} return
        }

        if {$cmd != ""} {
            # Find out the scrollbar widget we're using
            set scroller [lindex $cmd 0]

            # Make sure we act
            set act 1
        }
    }

    if {$act == 1} {
        # Now we know we have to process the wheel mouse event
        set xy [$widget yview]
        set factor [expr [lindex $xy 1]-[lindex $xy 0]]
        #puts "xy = $xy factor = $factor delta = $delta [expr $delta/(120*$factor)]"

        # Make sure we activate the scrollbar's command
        set cmd "[$scroller cget -command] scroll [expr -round($delta/(120*$factor))] units"
        #puts "cmd = $cmd"
        eval $cmd
    }
}

#  Destroy all right hand box widgets

proc clear_right {} {
   global w4

   foreach w [winfo children $w4] {destroy $w}
}

# Bottom button functions

proc bb_loadjob {} {
   set types {{{Job Files} {.job}} {{All Files} *}}
   set file [tk_getOpenFile -filetypes $types -title "Load Job"]
   if {$file != ""} {
      global ancil_types
      foreach type $ancil_types {global $type}

      write_message "Loading job configuration from file $file"
      clear_right

      set error [catch {source $file} msg]
      if {$error} {
         write_message "Error reading job configuration file $file"
         write_message $msg
      }
   }
}

proc bb_savejob {} {
   global savefile
   
   if {"$savefile" == ""} {
      set types {{{Job Files} {.job}} {{All Files} *}}
      set savefile [tk_getSaveFile -initialfile $savefile \
                                   -filetypes $types -title "Save Job"]
   }

   if {$savefile != ""} {savejob $savefile}
}

proc bb_savejobas {} {
   global savefile
   
   set savefileold $savefile

   set types {{{Job Files} {.job}} {{All Files} *}}
   set savefile [tk_getSaveFile -initialfile $savefile \
                                -filetypes $types -title "Save Job"]

   if {$savefile != ""} {
      savejob $savefile
   } else {
      set savefile $savefileold
   }
}

proc savejob {file} {

   global ancil_types savefile xancil_version versiondate

   set savefile $file
   set line1 "# Xancil version $xancil_version ($versiondate)"
   set line2 "# Job file created on [clock format [clock seconds]]"

   #if {[file exists $file] && [file size $file] > 0} {
   #   set fid [open $file r]
   #   if {[gets $fid] != "$line1"} {
   #      set ret [mesgbox "Overwrite non xancil job file $savefile?"]
   #      if {$ret == 1} {
   #         write_message "Not saving job configuration to file $file"
   #         close $fid
   #         return
   #      }
   #   }
   #   close $fid
   #}

   write_message "Saving job configuration to file $file"

   set fid [open $file w]

   puts $fid $line1
   puts $fid $line2

   foreach type $ancil_types {
      global $type
      upvar #0 $type ancil
      puts $fid "\n# $type variables\n"
      foreach el [lsort [array names $type]] {
         if {"$ancil($el)" == ""} {
            puts $fid "set ${type}($el) \"\""
         } elseif {[llength $ancil($el)] > 1} {
            puts $fid "set ${type}($el) \"$ancil($el)\""
         } else {
            puts $fid "set ${type}($el) $ancil($el)"
         }
     }
   }
   close $fid
}

proc bb_savenl {} {
   global namelist

   if {$namelist == ""} {
      set types {{{Namelist Files} {.namelist}} {{All Files} *}}
      set namelist [tk_getSaveFile -initialfile $namelist \
                                   -filetypes $types -title "Save Namelist"]
   }

   if {$namelist != ""} {create_namelist $namelist}
}

proc bb_savenlas {} {
   global namelist

   set namelistold $namelist

   set types {{{Namelist Files} {.namelist}} {{All Files} *}}
   set namelist [tk_getSaveFile -initialfile $namelist \
                                -filetypes $types -title "Save Namelist"]

   if {$namelist != ""} {
      create_namelist $namelist
   } else {
      set namelist $namelistold
   }
}

proc bb_createancil {} {
   global namelist exec

   create_namelist $namelist
   run_exec $exec
}

proc update_vars {} {

   global ancil_types
   foreach type $ancil_types {global $type}
   
   if {! $ice(create)} {
      set sst(seticeval) 0
   }

   if {! $sst(create)} {
      set ice(usesstval) 0
      if {$ice(timeusage1) == 2} {set ice(timeusage1) 0}
   } else {
      set ice(periodic) $sst(periodic)
      set ice(timeusage1) 2
   }

   if {(! $lfrac(use)) && (! $lfrac(create))} {
      set mask(uselfrac) 0
      if {$slt(mask) == 2} {set slt(mask) 0}
      if {$smow(mask) == 2} {set smow(mask) 0}
      if {$soil(mask) == 2} {set soil(mask) 0}
      if {$veg(mask) == 2} {set veg(mask) 0}
      if {$vegfrac(mask) == 2} {set vegfrac(mask) 0}
      if {$vegfunc(mask) == 2} {set vegfunc(mask) 0}
      if {$vegdist(mask) == 2} {set vegdist(mask) 0}
      if {$sst(mask) == 2} {set sst(mask) 0}
      if {$ice(mask) == 2} {set ice(mask) 0}
   }

   if {(! $mask(use)) && (! $mask(create))} {
      if {$slt(mask) == 1} {set slt(mask) 0}
      if {$smow(mask) == 1} {set smow(mask) 0}
      if {$soil(mask) == 1} {set soil(mask) 0}
      if {$veg(mask) == 1} {set veg(mask) 0}
      if {$vegfrac(mask) == 1} {set vegfrac(mask) 0}
      if {$vegfunc(mask) == 1} {set vegfunc(mask) 0}
      if {$vegdist(mask) == 1} {set vegdist(mask) 0}
      if {$sst(mask) == 1} {set sst(mask) 0}
      if {$ice(mask) == 1} {set ice(mask) 0}
   }

   foreach type $ancil_types {
      upvar #0 $type ancil
      if {$ancil(create)} {
         if {[info exists ancil(file_in)]} {
            set ncfile $ancil(file_in)
            if {$ncfile != "" && \
                [lsearch -exact $config(ncfile) $ncfile] == -1} {
               lappend config(ncfile) $ncfile
            }
         }

         if {[info exists gridconfig(useglobtime)]} {
            if {$gridconfig(useglobtime)} {
               if {[info exists ancil(useglobtime)]} {
                  if {$ancil(useglobtime)} {
                     set ancil(startyear) $gridconfig(startyear)
                     set ancil(startmon) $gridconfig(startmon)
                     set ancil(startday) $gridconfig(startday)
                     set ancil(starthour) $gridconfig(starthour)
                     set ancil(startmin) $gridconfig(startmin)
                     set ancil(startsec) $gridconfig(startsec)
                     set ancil(ntimes) $gridconfig(ntimes)
                     set ancil(interval) $gridconfig(interval)
                     set ancil(intunit) $gridconfig(intunit)
                     set ancil(mm) $gridconfig(mm)
                  }
               }
            }
         }
      }
   }

   for {set i 1} {$i<=$genanc_config(nancfile)} {incr i} {
      if {$genanc($i,create)} {
         for {set j 1} {$j<=$genanc($i,nfield)} {incr j} {
            if {[info exists genanc($i,$j,file_in)]} {
               set ncfile $genanc($i,$j,file_in)
               if {$ncfile != "" && \
                   [lsearch -exact $config(ncfile) $ncfile] == -1} {
                  lappend config(ncfile) $ncfile
               }
            }

            if {[info exists genanc($i,$j,levtype)]} {
               if {$genanc($i,$j,levtype) == 1} {
                  set genanc($i,$j,nlev) $gridconfig(nsoillev)
               } elseif {$genanc($i,$j,levtype) == 3} {
                  set genanc($i,$j,nlev) $gridconfig(nlev)
               } elseif {$genanc($i,$j,levtype) == 4} {
                  set genanc($i,$j,nlev) $gridconfig(nolev)
               }
            }
         }

         if {[info exists gridconfig(useglobtime)]} {
            if {$gridconfig(useglobtime)} {
               if {[info exists genanc($i,useglobtime)]} {
                  if {$genanc($i,useglobtime)} {
                     set genanc($i,startyear) $gridconfig(startyear)
                     set genanc($i,startmon) $gridconfig(startmon)
                     set genanc($i,startday) $gridconfig(startday)
                     set genanc($i,starthour) $gridconfig(starthour)
                     set genanc($i,startmin) $gridconfig(startmin)
                     set genanc($i,startsec) $gridconfig(startsec)
                     set genanc($i,ntimes) $gridconfig(ntimes)
                     set genanc($i,interval) $gridconfig(interval)
                     set genanc($i,intunit) $gridconfig(intunit)
                     set genanc($i,mm) $gridconfig(mm)
                  }
               }
            }
         }
      }
   }
}

proc create_namelist {{namelist_arg {}}} {

   global namelist namelisttype ancil_types

   if {$namelist_arg != {}} {set namelist $namelist_arg}
   
   if {$namelisttype == "old"} {
      set endrec " &end"
   } else {
      set endrec " /"
   }
   
   update_vars

   write_message "Creating namelist file $namelist"

   set fid [open $namelist w]

   foreach type $ancil_types {
      global $type
      upvar #0 $type ancil

      puts $fid " &nam_$type"
      if {$ancil(create)} {
         if {$type == "config"} {
            puts $fid "  ICAL = $config(cal),"
            puts $fid "  ISIZE = $config(size),"
            if {$config(size) == 64} {
	       if {$config(pack32)} {
                  puts $fid "  L32BIT = .TRUE.,"
               } else {
                  puts $fid "  L32BIT = .FALSE.,"
               }
            }
            puts $fid "  VERSION = $config(version),"
	    if {$config(end)} {
               puts $fid "  LBIGENDOUT = .TRUE.,"
            } else {
               puts $fid "  LBIGENDOUT = .FALSE.,"
            }
            if {$config(wfio)} {
               puts $fid "  LWFIO = .TRUE.,"
               puts $fid "  IWFIO_SIZE = $config(wfio_size),"
            } else {
               puts $fid "  LWFIO = .FALSE.,"
            }
            set nncfiles [llength $config(ncfile)]
            puts $fid "  NNCFILES = $nncfiles"
            if {$nncfiles > 0} {
               puts -nonewline $fid "  NCFILES = "
               for {set i 0} {$i < [expr {$nncfiles-1}]} {incr i} {
                  set ncfile [lindex $config(ncfile) $i]
                  puts -nonewline $fid "\"$ncfile\","
               }
               set ncfile [lindex $config(ncfile) [expr {$nncfiles-1}]]
               puts $fid "\"$ncfile\""
            }
         } elseif {$type == "gridconfig"} {
            if {$gridconfig(vargrid)} {
               puts $fid "  LVARGRID = .TRUE.,"
            } else {
               puts $fid "  LVARGRID = .FALSE.,"
            }
            puts $fid "  IAVERT = $gridconfig(avert),"
            if {$gridconfig(avert) == 1} {
               puts $fid "  NLEV = $gridconfig(nlev),"
               puts $fid "  NO3LEV = $gridconfig(nolev),"
               if {$gridconfig(levstoreup)} {
                  puts $fid "  LLEVSTOREUP = .TRUE.,"
               } else {
                  puts $fid "  LLEVSTOREUP = .FALSE.,"
               }
               puts $fid "  NAM_VERT = \"$gridconfig(vert_namelist)\","
            } elseif {$gridconfig(avert) == 2} {
               puts $fid "  AVERT_FILEIN = \"$gridconfig(avert_file_in)\","
               puts $fid "  AVERT_NCNAME = \"$gridconfig(avert_name)\","
               if {$gridconfig(theta)} {
                  puts $fid "  LTHETA = .TRUE.,"
               } else {
                  puts $fid "  LTHETA = .FALSE.,"
               }
               if {$gridconfig(ozonelev)} {
                  puts $fid "  LOZONELEV = .TRUE.,"
                  puts $fid "  NO3LEV = $gridconfig(nolev),"
               } else {
                  puts $fid "  LOZONELEV = .FALSE.,"
               }
            }
            puts $fid "  IOVERT = $gridconfig(overt),"
            if {$gridconfig(overt) == 2} {
               puts $fid "  OVERT_FILEIN = \"$gridconfig(overt_file_in)\","
               puts $fid "  OVERT_NCNAME = \"$gridconfig(overt_name)\","
            }
            if {$gridconfig(deepsoil)} {
               puts $fid "  LDEEPSOIL = .TRUE.,"
               puts $fid "  NSOILLEV = $gridconfig(nsoillev),"
               set i 1
               while {$i < $gridconfig(nsoillev)} {
                  puts $fid "  SOILLEV($i) = $gridconfig(soillev$i),"
                  incr i
               }
               puts $fid "  SOILLEV($i) = $gridconfig(soillev$i)"
            } else {
               puts $fid "  LDEEPSOIL = .FALSE."
            }
         } elseif {$type == "ozone"} {
            puts $fid "  LOZONE = .TRUE.,"
            puts $fid "  OZONE_FILEIN = \"$ozone(file_in)\","
            puts $fid "  OZONE_NCNAME = \"$ozone(name)\","
            puts $fid "  OZONE_FILEOUT = \"$ozone(file_out)\","
            if {$ozone(mixtype)} {
               puts $fid "  LOZONE_MMR = .TRUE.,"
            } else {
               puts $fid "  LOZONE_MMR = .FALSE.,"
            }
            puts $fid "  AOZONE_MRFAC = [expr double($ozone(mixfac))],"
            if {$ozone(periodic)} {
               puts $fid "  LOZONE_PERIODIC = .TRUE.,"
            } else {
               puts $fid "  LOZONE_PERIODIC = .FALSE.,"
            }
            puts $fid "  IOZONE_TIMEUSAGE1 = $ozone(timeusage1),"
            puts $fid "  IOZONE_TIMEUSAGE2 = $ozone(timeusage2),"
            puts $fid "  IOZONE_STARTDATE(1) = $ozone(startyear),"
            puts $fid "  IOZONE_STARTDATE(2) = $ozone(startmon),"
            puts $fid "  IOZONE_STARTDATE(3) = $ozone(startday),"
            puts $fid "  IOZONE_STARTDATE(4) = $ozone(starthour),"
            puts $fid "  IOZONE_STARTDATE(5) = $ozone(startmin),"
            puts $fid "  IOZONE_STARTDATE(6) = $ozone(startsec),"
            puts $fid "  IOZONE_NTIMES = $ozone(ntimes),"
            puts $fid "  IOZONE_INTERVAL = $ozone(interval),"
            puts $fid "  IOZONE_INTUNIT = $ozone(intunit),"
            if {$ozone(mm)} {
               puts $fid "  LOZONE_MM = .TRUE."
            } else {
               puts $fid "  LOZONE_MM = .FALSE."
            }
         } elseif {$type == "smow"} {
            puts $fid "  LSMOW = .TRUE.,"
            puts $fid "  SMOW_FILEIN = \"$smow(file_in)\","
            puts $fid "  SMOW_NCSNOWDEPTHNAME = \"$smow(snowdepthname)\","
            if {$smow(calcsnowedge)} {
                  puts $fid "  LSMOW_CALCSNOWEDGE = .TRUE.,"
            } else {
                  puts $fid "  LSMOW_CALCSNOWEDGE = .FALSE.,"
                  puts $fid "  SMOW_NCSNOWEDGENAME = \"$smow(snowedgename)\","
            }
            puts $fid "  SMOW_NCSOILMOISTURENAME = \"$smow(soilmoisturename)\","
            puts $fid "  SMOW_FILEOUT = \"$smow(file_out)\","
            if {$smow(periodic)} {
               puts $fid "  LSMOW_PERIODIC = .TRUE.,"
            } else {
               puts $fid "  LSMOW_PERIODIC = .FALSE.,"
            }
            puts $fid "  ISMOW_MASK = $smow(mask),"
            puts $fid "  ISMOW_TIMEUSAGE1 = $smow(timeusage1),"
            puts $fid "  ISMOW_TIMEUSAGE2 = $smow(timeusage2),"
            puts $fid "  ISMOW_STARTDATE(1) = $smow(startyear),"
            puts $fid "  ISMOW_STARTDATE(2) = $smow(startmon),"
            puts $fid "  ISMOW_STARTDATE(3) = $smow(startday),"
            puts $fid "  ISMOW_STARTDATE(4) = $smow(starthour),"
            puts $fid "  ISMOW_STARTDATE(5) = $smow(startmin),"
            puts $fid "  ISMOW_STARTDATE(6) = $smow(startsec),"
            puts $fid "  ISMOW_NTIMES = $smow(ntimes),"
            puts $fid "  ISMOW_INTERVAL = $smow(interval),"
            puts $fid "  ISMOW_INTUNIT = $smow(intunit),"
            if {$smow(mm)} {
               puts $fid "  LSMOW_MM = .TRUE."
            } else {
               puts $fid "  LSMOW_MM = .FALSE."
            }
         } elseif {$type == "slt"} {
            puts $fid "  LSLT = .TRUE.,"
            puts $fid "  SLT_FILEIN = \"$slt(file_in)\","
            puts $fid "  SLT_NCNAME = \"$slt(name)\","
            puts $fid "  SLT_FILEOUT = \"$slt(file_out)\","
            if {$slt(periodic)} {
               puts $fid "  LSLT_PERIODIC = .TRUE.,"
            } else {
               puts $fid "  LSLT_PERIODIC = .FALSE.,"
            }
            puts $fid "  ISLT_MASK = $slt(mask),"
            puts $fid "  ISLT_TIMEUSAGE1 = $slt(timeusage1),"
            puts $fid "  ISLT_TIMEUSAGE2 = $slt(timeusage2),"
            puts $fid "  ISLT_STARTDATE(1) = $slt(startyear),"
            puts $fid "  ISLT_STARTDATE(2) = $slt(startmon),"
            puts $fid "  ISLT_STARTDATE(3) = $slt(startday),"
            puts $fid "  ISLT_STARTDATE(4) = $slt(starthour),"
            puts $fid "  ISLT_STARTDATE(5) = $slt(startmin),"
            puts $fid "  ISLT_STARTDATE(6) = $slt(startsec),"
            puts $fid "  ISLT_NTIMES = $slt(ntimes),"
            puts $fid "  ISLT_INTERVAL = $slt(interval),"
            puts $fid "  ISLT_INTUNIT = $slt(intunit),"
            if {$slt(mm)} {
               puts $fid "  LSLT_MM = .TRUE."
            } else {
               puts $fid "  LSLT_MM = .FALSE."
            }
         } elseif {$type == "soil"} {
            puts $fid "  LSOIL = .TRUE.,"
            puts $fid "  SOIL_FILEIN = \"$soil(file_in)\","
            puts $fid "  SOIL_NCVSMCWILTNAME = \"$soil(vsmcwiltname)\","
            puts $fid "  SOIL_NCVSMCCRITNAME = \"$soil(vsmccritname)\","
            puts $fid "  SOIL_NCVSMCFCAPNAME = \"$soil(vsmcfcapname)\","
            puts $fid "  SOIL_NCVSMCSATNAME = \"$soil(vsmcsatname)\","
            puts $fid "  SOIL_NCCLAPPHORNNAME = \"$soil(clapphornname)\","
            puts $fid "  SOIL_NCTHERMCONDNAME = \"$soil(thermcondname)\","
            puts $fid "  SOIL_NCSOILCONDNAME = \"$soil(soilcondname)\","
            puts $fid "  SOIL_NCTHERMCAPNAME = \"$soil(thermcapname)\","
            puts $fid "  SOIL_NCSOILWATERSUCNAME = \"$soil(soilwatersucname)\","
            puts $fid "  SOIL_NCSOILALBNAME = \"$soil(soilalbname)\","
            puts $fid "  SOIL_NCSOILCARBNAME = \"$soil(soilcarbname)\","
            puts $fid "  SOIL_FILEOUT = \"$soil(file_out)\","
            puts $fid "  ISOIL_MASK = $soil(mask)"
         } elseif {$type == "veg"} {
            puts $fid "  LVEG = .TRUE.,"
            puts $fid "  VEG_FILEIN = \"$veg(file_in)\","
            puts $fid "  VEG_NCROOTDEPTHNAME = \"$veg(rootdepthname)\","
            puts $fid "  VEG_NCSFANAME = \"$veg(sfaname)\","
            puts $fid "  VEG_NCSURFRESISTNAME = \"$veg(surfresistname)\","
            puts $fid "  VEG_NCZ0NAME = \"$veg(z0name)\","
            puts $fid "  VEG_NCCANCAPNAME = \"$veg(cancapname)\","
            puts $fid "  VEG_NCVEGFRACNAME = \"$veg(vegfracname)\","
            puts $fid "  VEG_NCINFILTNAME = \"$veg(infiltname)\","
            puts $fid "  VEG_NCDSANAME = \"$veg(dsaname)\","
            puts $fid "  VEG_NCLAINAME = \"$veg(lainame)\","
            puts $fid "  VEG_NCCANHTNAME = \"$veg(canhtname)\","
            puts $fid "  VEG_FILEOUT = \"$veg(file_out)\","
            puts $fid "  IVEG_MASK = $veg(mask)"
         } elseif {$type == "vegfrac"} {
            puts $fid "  LVEGFRAC = .TRUE.,"
            puts $fid "  VEGFRAC_FILEIN = \"$vegfrac(file_in)\","
            puts $fid "  VEGFRAC_NCNAME = \"$vegfrac(name)\","
            puts $fid "  IVEGFRAC_NSURFTYPES = $vegfrac(surftypes),"
            puts $fid "  VEGFRAC_FILEOUT = \"$vegfrac(file_out)\","
            puts $fid "  IVEGFRAC_MASK = $vegfrac(mask)"
         } elseif {$type == "vegfunc"} {
            puts $fid "  LVEGFUNC = .TRUE.,"
            puts $fid "  VEGFUNC_FILEIN = \"$vegfunc(file_in)\","
            puts $fid "  VEGFUNC_NCLAINAME = \"$vegfunc(lainame)\","
            puts $fid "  VEGFUNC_NCCANHTNAME = \"$vegfunc(canhtname)\","
            puts $fid "  VEGFUNC_NCCANCONDNAME = \"$vegfunc(cancondname)\","
            puts $fid "  IVEGFUNC_NFUNCTYPES = $vegfunc(functypes),"
            puts $fid "  VEGFUNC_FILEOUT = \"$vegfunc(file_out)\","
            if {$vegfunc(periodic)} {
               puts $fid "  LVEGFUNC_PERIODIC = .TRUE.,"
            } else {
               puts $fid "  LVEGFUNC_PERIODIC = .FALSE.,"
            }
            puts $fid "  IVEGFUNC_MASK = $vegfunc(mask),"
            puts $fid "  IVEGFUNC_TIMEUSAGE1 = $vegfunc(timeusage1),"
            puts $fid "  IVEGFUNC_TIMEUSAGE2 = $vegfunc(timeusage2),"
            puts $fid "  IVEGFUNC_STARTDATE(1) = $vegfunc(startyear),"
            puts $fid "  IVEGFUNC_STARTDATE(2) = $vegfunc(startmon),"
            puts $fid "  IVEGFUNC_STARTDATE(3) = $vegfunc(startday),"
            puts $fid "  IVEGFUNC_STARTDATE(4) = $vegfunc(starthour),"
            puts $fid "  IVEGFUNC_STARTDATE(5) = $vegfunc(startmin),"
            puts $fid "  IVEGFUNC_STARTDATE(6) = $vegfunc(startsec),"
            puts $fid "  IVEGFUNC_NTIMES = $vegfunc(ntimes),"
            puts $fid "  IVEGFUNC_INTERVAL = $vegfunc(interval),"
            puts $fid "  IVEGFUNC_INTUNIT = $vegfunc(intunit),"
            if {$vegfunc(mm)} {
               puts $fid "  LVEGFUNC_MM = .TRUE."
            } else {
               puts $fid "  LVEGFUNC_MM = .FALSE."
            }
         } elseif {$type == "vegdist"} {
            puts $fid "  LVEGDIST = .TRUE.,"
            puts $fid "  VEGDIST_FILEIN = \"$vegdist(file_in)\","
            puts $fid "  VEGDIST_NCNAME = \"$vegdist(name)\","
            puts $fid "  VEGDIST_FILEOUT = \"$vegdist(file_out)\","
            puts $fid "  IVEGDIST_MASK = $vegdist(mask)"
         } elseif {$type == "sst"} {
            puts $fid "  LSST = .TRUE.,"
            puts $fid "  SST_FILEIN = \"$sst(file_in)\","
            puts $fid "  SST_NCNAME = \"$sst(name)\","
            puts $fid "  SST_FILEOUT = \"$sst(file_out)\","
            if {$sst(setminval)} {
               puts $fid "  LSST_MIN = .TRUE.,"
               puts $fid "  ASST_MIN = $sst(minval),"
            } else {
               puts $fid "  LSST_MIN = .FALSE.,"
            }
            if {$sst(seticeval)} {
               puts $fid "  LSST_ICEVAL = .TRUE.,"
               puts $fid "  ASST_ICEVAL = $sst(iceval),"
            } else {
               puts $fid "  LSST_ICEVAL = .FALSE.,"
            }
            if {$sst(periodic)} {
               puts $fid "  LSST_PERIODIC = .TRUE.,"
            } else {
               puts $fid "  LSST_PERIODIC = .FALSE.,"
            }
            puts $fid "  ISST_MASK = $sst(mask),"
            puts $fid "  ISST_TIMEUSAGE1 = $sst(timeusage1),"
            puts $fid "  ISST_TIMEUSAGE2 = $sst(timeusage2),"
            puts $fid "  ISST_STARTDATE(1) = $sst(startyear),"
            puts $fid "  ISST_STARTDATE(2) = $sst(startmon),"
            puts $fid "  ISST_STARTDATE(3) = $sst(startday),"
            puts $fid "  ISST_STARTDATE(4) = $sst(starthour),"
            puts $fid "  ISST_STARTDATE(5) = $sst(startmin),"
            puts $fid "  ISST_STARTDATE(6) = $sst(startsec),"
            puts $fid "  ISST_NTIMES = $sst(ntimes),"
            puts $fid "  ISST_INTERVAL = $sst(interval),"
            puts $fid "  ISST_INTUNIT = $sst(intunit),"
            if {$sst(mm)} {
               puts $fid "  LSST_MM = .TRUE."
            } else {
               puts $fid "  LSST_MM = .FALSE."
            }
         } elseif {$type == "ice"} {
            puts $fid "  LICE = .TRUE.,"
            puts $fid "  ICE_FILEIN = \"$ice(file_in)\","
            if {$ice(amip2)} {
               puts $fid "  LICE_AMIP2 = .TRUE.,"
            } else {
               puts $fid "  LICE_AMIP2 = .FALSE.,"
	       if {$ice(calcdepth)} {
                  puts $fid "  LICE_CALCDEPTH = .TRUE.,"
	       } else {
                  puts $fid "  LICE_CALCDEPTH = .FALSE.,"
                  puts $fid "  ICE_NCDEPTHNAME = \"$ice(depthname)\","
	       }
	       if {$ice(calcedge)} {
                  puts $fid "  LICE_CALCEDGE = .TRUE.,"
	       } else {
                  puts $fid "  LICE_CALCEDGE = .FALSE.,"
                  puts $fid "  ICE_NCEDGENAME = \"$ice(edgename)\","
	       }
            }
            if {$ice(usesstval)} {
               puts $fid "  LICE_SSTVAL = .TRUE.,"
               puts $fid "  AICE_SSTVAL = $ice(sstval),"
            } else {
               puts $fid "  LICE_SSTVAL = .FALSE.,"
               puts $fid "  ICE_NCFRACNAME = \"$ice(concname)\","
            }
	    if {$ice(percent)} {
               puts $fid "  LICE_PERCENT = .TRUE.,"
	    } else {
               puts $fid "  LICE_PERCENT = .FALSE.,"
	    }
	    if {$ice(mkmask)} {
               puts $fid "  LICE_MKMASK = .TRUE.,"
               puts $fid "  AICE_CUTOFF = $ice(cutoffval),"
	    } else {
               puts $fid "  LICE_MKMASK = .FALSE.,"
	    }
            puts $fid "  ICE_FILEOUT = \"$ice(file_out)\","
            if {$ice(setminval)} {
               puts $fid "  LICE_MIN = .TRUE.,"
               puts $fid "  AICE_MIN = $ice(minval),"
            } else {
               puts $fid "  LICE_MIN = .FALSE.,"
            }
            if {$ice(setmaxval)} {
               puts $fid "  LICE_MAX = .TRUE.,"
               puts $fid "  AICE_MAX = $ice(maxval),"
            } else {
               puts $fid "  LICE_MAX = .FALSE.,"
            }
            if {$ice(periodic)} {
               puts $fid "  LICE_PERIODIC = .TRUE.,"
            } else {
               puts $fid "  LICE_PERIODIC = .FALSE.,"
            }
            puts $fid "  IICE_MASK = $ice(mask),"
	    if {$ice(timeusage1) != 2} {
               puts $fid "  IICE_TIMEUSAGE1 = $ice(timeusage1),"
               puts $fid "  IICE_TIMEUSAGE2 = $ice(timeusage2),"
               puts $fid "  IICE_STARTDATE(1) = $ice(startyear),"
               puts $fid "  IICE_STARTDATE(2) = $ice(startmon),"
               puts $fid "  IICE_STARTDATE(3) = $ice(startday),"
               puts $fid "  IICE_STARTDATE(4) = $ice(starthour),"
               puts $fid "  IICE_STARTDATE(5) = $ice(startmin),"
               puts $fid "  IICE_STARTDATE(6) = $ice(startsec),"
               puts $fid "  IICE_NTIMES = $ice(ntimes),"
               puts $fid "  IICE_INTERVAL = $ice(interval),"
               puts $fid "  IICE_INTUNIT = $ice(intunit),"
               if {$ice(mm)} {
                  puts $fid "  LICE_MM = .TRUE."
               } else {
                  puts $fid "  LICE_MM = .FALSE."
               }
	    } else {
               puts $fid "  IICE_TIMEUSAGE1 = $ice(timeusage1)"
            }
         } elseif {$type == "orog"} {
            puts $fid "  LOROG = .TRUE.,"
            puts $fid "  OROG_FILEIN = \"$orog(file_in)\","
            puts $fid "  OROG_NCNAME = \"$orog(name)\","
            puts $fid "  OROG_NCSDNAME = \"$orog(sdname)\","
            if {$orog(incgrad)} {
               puts $fid "  LOROG_INCGRAD = .TRUE.,"
               puts $fid "  OROG_NCXGRADNAME = \"$orog(xgradname)\","
               puts $fid "  OROG_NCYGRADNAME = \"$orog(ygradname)\","
	    } else {
               puts $fid "  LOROG_INCGRAD = .FALSE.,"
	    }
            if {$orog(incsqgrad)} {
               puts $fid "  LOROG_INCSQGRAD = .TRUE.,"
               puts $fid "  OROG_NCXXGRADNAME = \"$orog(xxgradname)\","
               puts $fid "  OROG_NCXYGRADNAME = \"$orog(xygradname)\","
               puts $fid "  OROG_NCYYGRADNAME = \"$orog(yygradname)\","
	    } else {
               puts $fid "  LOROG_INCSQGRAD = .FALSE.,"
	    }
            if {$orog(incrough)} {
               puts $fid "  LOROG_INCROUGH = .TRUE.,"
               puts $fid "  OROG_NCSILNAME = \"$orog(silname)\","
               puts $fid "  OROG_NCPTHTNAME = \"$orog(pthtname)\","
	    } else {
               puts $fid "  LOROG_INCROUGH = .FALSE.,"
	    }
            if {$orog(incunfilt)} {
               puts $fid "  LOROG_INCUNFILT = .TRUE.,"
               puts $fid "  OROG_NCUNFILTNAME = \"$orog(unfiltname)\","
	    } else {
               puts $fid "  LOROG_INCUNFILT = .FALSE.,"
	    }
            puts $fid "  OROG_FILEOUT = \"$orog(file_out)\","
            puts $fid "  IOROG_MASK = $orog(mask)"
         } elseif {$type == "mask"} {
            puts $fid "  LMASK = .TRUE.,"
            if {$mask(use)} {
               puts $fid "  LMASK_USE = .TRUE.,"
            } else {
               puts $fid "  LMASK_USE = .FALSE.,"
            }
            puts $fid "  MASK_FILEIN = \"$mask(file_in)\","
	    if {$mask(uselfrac)} {
               puts $fid "  LMASK_USELFRAC = .TRUE.,"
	    } else {
               puts $fid "  LMASK_USELFRAC = .FALSE.,"
               if {$mask(sea)} {
                  puts $fid "  LMASK_SEA = .TRUE.,"
               } else {
                  puts $fid "  LMASK_SEA = .FALSE.,"
               }
               if {$mask(usemdi)} {
                  puts $fid "  LMASK_USEMDI = .TRUE.,"
               } else {
                  puts $fid "  LMASK_USEMDI = .FALSE.,"
	          if {[isint $mask(val)]} {
                     puts $fid "  AMASK_VAL = [expr double($mask(val))],"
                     puts $fid "  IMASK_VAL = $mask(val),"
                  } else {
                     puts $fid "  AMASK_VAL = $mask(val),"
                     puts $fid "  IMASK_VAL = [expr int($mask(val))],"
	          }
               }
               puts $fid "  MASK_NCNAME = \"$mask(name)\","
	    }
            if {$mask(outflow)} {
               puts $fid "  LMASK_OUTFLOW = .TRUE.,"
               puts $fid "  MASK_NCOFNAME = \"$mask(ofname)\","
            } else {
               puts $fid "  LMASK_OUTFLOW = .FALSE.,"
            }
            puts $fid "  MASK_FILEOUT = \"$mask(file_out)\""
         } elseif {$type == "lfrac"} {
            puts $fid "  LLFRAC = .TRUE.,"
            if {$lfrac(use)} {
               puts $fid "  LLFRAC_USE = .TRUE.,"
            } else {
               puts $fid "  LLFRAC_USE = .FALSE.,"
            }
            puts $fid "  LFRAC_FILEIN = \"$lfrac(file_in)\","
            puts $fid "  LFRAC_NCNAME = \"$lfrac(name)\","
            puts $fid "  LFRAC_FILEOUT = \"$lfrac(file_out)\""
         } elseif {$type == "ausrmulti"} {
            puts $fid "  LAUSRMULTI = .TRUE.,"
     	    puts $fid "  AUSRMULTI_FILEOUT = \"$ausrmulti(file_out)\","
     	    if {$ausrmulti(periodic)} {
     	       puts $fid "  LAUSRMULTI_PERIODIC = .TRUE.,"
     	    } else {
     	       puts $fid "  LAUSRMULTI_PERIODIC = .FALSE.,"
     	    }
     	    puts $fid "  IAUSRMULTI_TIMEUSAGE1 = $ausrmulti(timeusage1),"
     	    puts $fid "  IAUSRMULTI_TIMEUSAGE2 = $ausrmulti(timeusage2),"
     	    puts $fid "  IAUSRMULTI_STARTDATE(1) = $ausrmulti(startyear),"
     	    puts $fid "  IAUSRMULTI_STARTDATE(2) = $ausrmulti(startmon),"
     	    puts $fid "  IAUSRMULTI_STARTDATE(3) = $ausrmulti(startday),"
     	    puts $fid "  IAUSRMULTI_STARTDATE(4) = $ausrmulti(starthour),"
     	    puts $fid "  IAUSRMULTI_STARTDATE(5) = $ausrmulti(startmin),"
     	    puts $fid "  IAUSRMULTI_STARTDATE(6) = $ausrmulti(startsec),"
     	    puts $fid "  IAUSRMULTI_NTIMES = $ausrmulti(ntimes),"
     	    puts $fid "  IAUSRMULTI_INTERVAL = $ausrmulti(interval),"
     	    puts $fid "  IAUSRMULTI_INTUNIT = $ausrmulti(intunit),"
     	    if {$ausrmulti(mm)} {
     	       puts $fid "  LAUSRMULTI_MM = .TRUE."
     	    } else {
     	       puts $fid "  LAUSRMULTI_MM = .FALSE."
     	    }
     	    puts $fid "  IAUSRMULTI_NFIELD = $ausrmulti(nfield)"
   
     	    for {set j 1} {$j <= $ausrmulti(nfield)} {incr j} {
     	       set ncfileid [expr {[lsearch -exact $config(ncfile) $ausrmulti($j,file_in)]+1}]
     	       puts $fid "  IAUSRMULTI_FILEINID($j) = $ncfileid,"
     	       puts $fid "  IAUSRMULTI_STASHCODE($j) = $ausrmulti($j,stashcode),"
     	       puts $fid "  IAUSRMULTI_PPCODE($j) = $ausrmulti($j,ppcode),"
     	       puts $fid "  AUSRMULTI_NCNAME($j) = \"$ausrmulti($j,varname)\","
               if {$ausrmulti($j,theta)} {
                  puts $fid "  LAUSRMULTI_THETA($j) = .TRUE."
               } else {
                  puts $fid "  LAUSRMULTI_THETA($j) = .FALSE."
               }
     	       puts $fid "  IAUSRMULTI_GRIDTYPE($j) = $ausrmulti($j,gridtype),"
     	       puts $fid "  IAUSRMULTI_DATATYPE($j) = $ausrmulti($j,datatype),"
     	       puts $fid "  IAUSRMULTI_MASKTYPE($j) = $ausrmulti($j,masktype),"
     	       puts $fid "  IAUSRMULTI_MASK($j) = $ausrmulti($j,mask),"
            }
         } elseif {$type == "ausrancil"} {
     	    puts $fid "  LAUSRANCIL = .TRUE.,"
     	    puts $fid "  AUSRANCIL_FILEOUT = \"$ausrancil(file_out)\","
     	    if {$ausrancil(periodic)} {
     	       puts $fid "  LAUSRANCIL_PERIODIC = .TRUE.,"
     	    } else {
     	       puts $fid "  LAUSRANCIL_PERIODIC = .FALSE.,"
     	    }
     	    puts $fid "  IAUSRANCIL_TIMEUSAGE1 = $ausrancil(timeusage1),"
     	    puts $fid "  IAUSRANCIL_TIMEUSAGE2 = $ausrancil(timeusage2),"
     	    puts $fid "  IAUSRANCIL_STARTDATE(1) = $ausrancil(startyear),"
     	    puts $fid "  IAUSRANCIL_STARTDATE(2) = $ausrancil(startmon),"
     	    puts $fid "  IAUSRANCIL_STARTDATE(3) = $ausrancil(startday),"
     	    puts $fid "  IAUSRANCIL_STARTDATE(4) = $ausrancil(starthour),"
     	    puts $fid "  IAUSRANCIL_STARTDATE(5) = $ausrancil(startmin),"
     	    puts $fid "  IAUSRANCIL_STARTDATE(6) = $ausrancil(startsec),"
     	    puts $fid "  IAUSRANCIL_NTIMES = $ausrancil(ntimes),"
     	    puts $fid "  IAUSRANCIL_INTERVAL = $ausrancil(interval),"
     	    puts $fid "  IAUSRANCIL_INTUNIT = $ausrancil(intunit),"
     	    if {$ausrancil(mm)} {
     	       puts $fid "  LAUSRANCIL_MM = .TRUE."
     	    } else {
     	       puts $fid "  LAUSRANCIL_MM = .FALSE."
     	    }
     	    puts $fid "  IAUSRANCIL_NFIELD = $ausrancil(nfield)"
   
     	    for {set j 1} {$j <= $ausrancil(nfield)} {incr j} {
     	       set ncfileid [expr {[lsearch -exact $config(ncfile) $ausrancil($j,file_in)]+1}]
     	       puts $fid "  IAUSRANCIL_FILEINID($j) = $ncfileid,"
     	       puts $fid "  IAUSRANCIL_STASHCODE($j) = $ausrancil($j,stashcode),"
     	       puts $fid "  IAUSRANCIL_PPCODE($j) = $ausrancil($j,ppcode),"
     	       puts $fid "  AUSRANCIL_NCNAME($j) = \"$ausrancil($j,varname)\","
     	       puts $fid "  IAUSRANCIL_GRIDTYPE($j) = $ausrancil($j,gridtype),"
     	       puts $fid "  IAUSRANCIL_DATATYPE($j) = $ausrancil($j,datatype),"
     	       puts $fid "  IAUSRANCIL_MASKTYPE($j) = $ausrancil($j,masktype),"
     	       puts $fid "  IAUSRANCIL_MASK($j) = $ausrancil($j,mask),"
            }
         } elseif {$type == "ts1"} {
            puts $fid "  LTS1 = .TRUE.,"
            puts $fid "  TS1_FILEIN = \"$ts1(file_in)\","
            if {$ts1(refsst)} {
               puts $fid "  LTS1_REFSST = .TRUE.,"
               puts $fid "  TS1_NCSSTNAME = \"$ts1(sstname)\","
            } else {
               puts $fid "  LTS1_REFSST = .FALSE.,"
            }
            if {$ts1(refsss)} {
               puts $fid "  LTS1_REFSSS = .TRUE.,"
               puts $fid "  TS1_NCSSSNAME = \"$ts1(sssname)\","
            } else {
               puts $fid "  LTS1_REFSSS = .FALSE.,"
            }
            if {$ts1(climat)} {
               puts $fid "  LTS1_CLIMAT = .TRUE.,"
               puts $fid "  TS1_NCATNAME = \"$ts1(atname)\","
            } else {
               puts $fid "  LTS1_CLIMAT = .FALSE.,"
            }
            if {$ts1(climid)} {
               puts $fid "  LTS1_CLIMID = .TRUE.,"
               puts $fid "  TS1_NCIDNAME = \"$ts1(idname)\","
            } else {
               puts $fid "  LTS1_CLIMID = .FALSE.,"
            }
            puts $fid "  TS1_FILEOUT = \"$ts1(file_out)\","
            if {$ts1(periodic)} {
               puts $fid "  LTS1_PERIODIC = .TRUE.,"
            } else {
               puts $fid "  LTS1_PERIODIC = .FALSE.,"
            }
            if {$ts1(mask)} {
               puts $fid "  ITS1_MASK = 0,"
            } else {
               puts $fid "  ITS1_MASK = -1,"
            }
            puts $fid "  ITS1_TIMEUSAGE1 = $ts1(timeusage1),"
            puts $fid "  ITS1_TIMEUSAGE2 = $ts1(timeusage2),"
            puts $fid "  ITS1_STARTDATE(1) = $ts1(startyear),"
            puts $fid "  ITS1_STARTDATE(2) = $ts1(startmon),"
            puts $fid "  ITS1_STARTDATE(3) = $ts1(startday),"
            puts $fid "  ITS1_STARTDATE(4) = $ts1(starthour),"
            puts $fid "  ITS1_STARTDATE(5) = $ts1(startmin),"
            puts $fid "  ITS1_STARTDATE(6) = $ts1(startsec),"
            puts $fid "  ITS1_NTIMES = $ts1(ntimes),"
            puts $fid "  ITS1_INTERVAL = $ts1(interval),"
            puts $fid "  ITS1_INTUNIT = $ts1(intunit),"
            if {$ts1(mm)} {
               puts $fid "  LTS1_MM = .TRUE."
            } else {
               puts $fid "  LTS1_MM = .FALSE."
            }
         } elseif {$type == "flux"} {
            puts $fid "  LFLUX = .TRUE.,"
            puts $fid "  FLUX_FILEIN = \"$flux(file_in)\","
            if {$flux(heat)} {
               puts $fid "  LFLUX_HEAT = .TRUE.,"
               puts $fid "  FLUX_NCHEATNAME = \"$flux(heatname)\","
            } else {
               puts $fid "  LFLUX_HEAT = .FALSE.,"
            }
            if {$flux(salt)} {
               puts $fid "  LFLUX_SALT = .TRUE.,"
               puts $fid "  FLUX_NCSALTNAME = \"$flux(saltname)\","
            } else {
               puts $fid "  LFLUX_SALT = .FALSE.,"
            }
            puts $fid "  FLUX_FILEOUT = \"$flux(file_out)\","
            if {$flux(periodic)} {
               puts $fid "  LFLUX_PERIODIC = .TRUE.,"
            } else {
               puts $fid "  LFLUX_PERIODIC = .FALSE.,"
            }
            if {$flux(mask)} {
               puts $fid "  IFLUX_MASK = 0,"
            } else {
               puts $fid "  IFLUX_MASK = -1,"
            }
            puts $fid "  IFLUX_TIMEUSAGE1 = $flux(timeusage1),"
            puts $fid "  IFLUX_TIMEUSAGE2 = $flux(timeusage2),"
            puts $fid "  IFLUX_STARTDATE(1) = $flux(startyear),"
            puts $fid "  IFLUX_STARTDATE(2) = $flux(startmon),"
            puts $fid "  IFLUX_STARTDATE(3) = $flux(startday),"
            puts $fid "  IFLUX_STARTDATE(4) = $flux(starthour),"
            puts $fid "  IFLUX_STARTDATE(5) = $flux(startmin),"
            puts $fid "  IFLUX_STARTDATE(6) = $flux(startsec),"
            puts $fid "  IFLUX_NTIMES = $flux(ntimes),"
            puts $fid "  IFLUX_INTERVAL = $flux(interval),"
            puts $fid "  IFLUX_INTUNIT = $flux(intunit),"
            if {$flux(mm)} {
               puts $fid "  LFLUX_MM = .TRUE."
            } else {
               puts $fid "  LFLUX_MM = .FALSE."
            }
         } elseif {$type == "ousrmulti"} {
            puts $fid "  LOUSRMULTI = .TRUE.,"
     	    puts $fid "  OUSRMULTI_FILEOUT = \"$ousrmulti(file_out)\","
     	    if {$ousrmulti(periodic)} {
     	       puts $fid "  LOUSRMULTI_PERIODIC = .TRUE.,"
     	    } else {
     	       puts $fid "  LOUSRMULTI_PERIODIC = .FALSE.,"
     	    }
     	    puts $fid "  IOUSRMULTI_TIMEUSAGE1 = $ousrmulti(timeusage1),"
     	    puts $fid "  IOUSRMULTI_TIMEUSAGE2 = $ousrmulti(timeusage2),"
     	    puts $fid "  IOUSRMULTI_STARTDATE(1) = $ousrmulti(startyear),"
     	    puts $fid "  IOUSRMULTI_STARTDATE(2) = $ousrmulti(startmon),"
     	    puts $fid "  IOUSRMULTI_STARTDATE(3) = $ousrmulti(startday),"
     	    puts $fid "  IOUSRMULTI_STARTDATE(4) = $ousrmulti(starthour),"
     	    puts $fid "  IOUSRMULTI_STARTDATE(5) = $ousrmulti(startmin),"
     	    puts $fid "  IOUSRMULTI_STARTDATE(6) = $ousrmulti(startsec),"
     	    puts $fid "  IOUSRMULTI_NTIMES = $ousrmulti(ntimes),"
     	    puts $fid "  IOUSRMULTI_INTERVAL = $ousrmulti(interval),"
     	    puts $fid "  IOUSRMULTI_INTUNIT = $ousrmulti(intunit),"
     	    if {$ousrmulti(mm)} {
     	       puts $fid "  LOUSRMULTI_MM = .TRUE."
     	    } else {
     	       puts $fid "  LOUSRMULTI_MM = .FALSE."
     	    }
     	    puts $fid "  IOUSRMULTI_NFIELD = $ousrmulti(nfield)"
   
     	    for {set j 1} {$j <= $ousrmulti(nfield)} {incr j} {
     	       set ncfileid [expr {[lsearch -exact $config(ncfile) $ousrmulti($j,file_in)]+1}]
     	       puts $fid "  IOUSRMULTI_FILEINID($j) = $ncfileid,"
     	       puts $fid "  IOUSRMULTI_STASHCODE($j) = $ousrmulti($j,stashcode),"
     	       puts $fid "  IOUSRMULTI_PPCODE($j) = $ousrmulti($j,ppcode),"
     	       puts $fid "  OUSRMULTI_NCNAME($j) = \"$ousrmulti($j,varname)\","
     	       puts $fid "  IOUSRMULTI_GRIDTYPE($j) = $ousrmulti($j,gridtype),"
     	       puts $fid "  IOUSRMULTI_DATATYPE($j) = $ousrmulti($j,datatype),"
     	       puts $fid "  IOUSRMULTI_MASKTYPE($j) = $ousrmulti($j,masktype),"
     	       puts $fid "  IOUSRMULTI_MASK($j) = $ousrmulti($j,mask),"
            }
         } elseif {$type == "ousrancil"} {
     	    puts $fid "  LOUSRANCIL = .TRUE.,"
     	    puts $fid "  OUSRANCIL_FILEOUT = \"$ousrancil(file_out)\","
     	    if {$ousrancil(periodic)} {
     	       puts $fid "  LOUSRANCIL_PERIODIC = .TRUE.,"
     	    } else {
     	       puts $fid "  LOUSRANCIL_PERIODIC = .FALSE.,"
     	    }
     	    puts $fid "  IOUSRANCIL_TIMEUSAGE1 = $ousrancil(timeusage1),"
     	    puts $fid "  IOUSRANCIL_TIMEUSAGE2 = $ousrancil(timeusage2),"
     	    puts $fid "  IOUSRANCIL_STARTDATE(1) = $ousrancil(startyear),"
     	    puts $fid "  IOUSRANCIL_STARTDATE(2) = $ousrancil(startmon),"
     	    puts $fid "  IOUSRANCIL_STARTDATE(3) = $ousrancil(startday),"
     	    puts $fid "  IOUSRANCIL_STARTDATE(4) = $ousrancil(starthour),"
     	    puts $fid "  IOUSRANCIL_STARTDATE(5) = $ousrancil(startmin),"
     	    puts $fid "  IOUSRANCIL_STARTDATE(6) = $ousrancil(startsec),"
     	    puts $fid "  IOUSRANCIL_NTIMES = $ousrancil(ntimes),"
     	    puts $fid "  IOUSRANCIL_INTERVAL = $ousrancil(interval),"
     	    puts $fid "  IOUSRANCIL_INTUNIT = $ousrancil(intunit),"
     	    if {$ousrancil(mm)} {
     	       puts $fid "  LOUSRANCIL_MM = .TRUE."
     	    } else {
     	       puts $fid "  LOUSRANCIL_MM = .FALSE."
     	    }
     	    puts $fid "  IOUSRANCIL_NFIELD = $ousrancil(nfield)"
   
     	    for {set j 1} {$j <= $ousrancil(nfield)} {incr j} {
     	       set ncfileid [expr {[lsearch -exact $config(ncfile) $ousrancil($j,file_in)]+1}]
     	       puts $fid "  IOUSRANCIL_FILEINID($j) = $ncfileid,"
     	       puts $fid "  IOUSRANCIL_STASHCODE($j) = $ousrancil($j,stashcode),"
     	       puts $fid "  IOUSRANCIL_PPCODE($j) = $ousrancil($j,ppcode),"
     	       puts $fid "  OUSRANCIL_NCNAME($j) = \"$ousrancil($j,varname)\","
     	       puts $fid "  IOUSRANCIL_GRIDTYPE($j) = $ousrancil($j,gridtype),"
     	       puts $fid "  IOUSRANCIL_DATATYPE($j) = $ousrancil($j,datatype),"
     	       puts $fid "  IOUSRANCIL_MASKTYPE($j) = $ousrancil($j,masktype),"
     	       puts $fid "  IOUSRANCIL_MASK($j) = $ousrancil($j,mask),"
            }
         } elseif {$type == "genanc_config"} {
            puts $fid "  NANCFILES = $genanc_config(nancfile)"
         } elseif {$type == "genanc"} {
            for {set i 1} {$i <= $genanc_config(nancfile)} {incr i} {
               if {$genanc($i,create)} {
                  puts $fid "  LGENANC_FILE($i) = .TRUE.,"
                  puts $fid "  GENANC_FILEOUT($i) = \"$genanc($i,file_out)\","
                  puts $fid "  IGENANC_MODEL($i) = $genanc($i,model),"
                  if {$genanc($i,inthd8)} {
                     puts $fid "  IGENANC_INTHD8($i) = $genanc($i,inthd8val),"
                  } else {
                     puts $fid "  IGENANC_INTHD8($i) = -1,"
                  }
                  if {$genanc($i,periodic)} {
                     puts $fid "  LGENANC_PERIODIC($i) = .TRUE.,"
                  } else {
                     puts $fid "  LGENANC_PERIODIC($i) = .FALSE.,"
                  }
                  puts $fid "  IGENANC_TIMEUSAGE1($i) = $genanc($i,timeusage1),"
                  puts $fid "  IGENANC_TIMEUSAGE2($i) = $genanc($i,timeusage2),"
                  puts $fid "  IGENANC_STARTDATE(1,$i) = $genanc($i,startyear),"
                  puts $fid "  IGENANC_STARTDATE(2,$i) = $genanc($i,startmon),"
                  puts $fid "  IGENANC_STARTDATE(3,$i) = $genanc($i,startday),"
                  puts $fid "  IGENANC_STARTDATE(4,$i) = $genanc($i,starthour),"
                  puts $fid "  IGENANC_STARTDATE(5,$i) = $genanc($i,startmin),"
                  puts $fid "  IGENANC_STARTDATE(6,$i) = $genanc($i,startsec),"
                  puts $fid "  IGENANC_NTIMES($i) = $genanc($i,ntimes),"
                  puts $fid "  IGENANC_INTERVAL($i) = $genanc($i,interval),"
                  puts $fid "  IGENANC_INTUNIT($i) = $genanc($i,intunit),"
                  if {$genanc($i,mm)} {
                     puts $fid "  LGENANC_MM($i) = .TRUE."
                  } else {
                     puts $fid "  LGENANC_MM($i) = .FALSE."
                  }
                  puts $fid "  IGENANC_NFIELD($i) = $genanc($i,nfield)"
   
                  for {set j 1} {$j <= $genanc($i,nfield)} {incr j} {
                     set ncfileid [expr {[lsearch -exact $config(ncfile) $genanc($i,$j,file_in)]+1}]
                     puts $fid "  IGENANC_FILEINID($j,$i) = $ncfileid,"
                     puts $fid "  IGENANC_STASHCODE($j,$i) = $genanc($i,$j,stashcode),"
                     puts $fid "  IGENANC_PPCODE($j,$i) = $genanc($i,$j,ppcode),"
                     puts $fid "  GENANC_NCNAME($j,$i) = \"$genanc($i,$j,varname)\","
                     puts $fid "  IGENANC_LEVTYPE($j,$i) = $genanc($i,$j,levtype),"
                     puts $fid "  IGENANC_NLEV($j,$i) = $genanc($i,$j,nlev),"
                     if {$genanc($i,$j,theta)} {
                        puts $fid "  LGENANC_THETA($j,$i) = .TRUE."
                     } else {
                        puts $fid "  LGENANC_THETA($j,$i) = .FALSE."
                     }
                     puts $fid "  IGENANC_GRIDTYPE($j,$i) = $genanc($i,$j,gridtype),"
                     puts $fid "  IGENANC_DATATYPE($j,$i) = $genanc($i,$j,datatype),"
                     puts $fid "  IGENANC_MASKTYPE($j,$i) = $genanc($i,$j,masktype),"
                     puts $fid "  IGENANC_MASK($j,$i) = $genanc($i,$j,mask),"
                  }
               } else {
                  puts $fid "  LGENANC_FILE($i) = .FALSE.,"
               }
               puts $fid  ""
            }
         } elseif {$type == "astart"} {
            puts $fid "  LASTART = .TRUE.,"
            puts $fid "  ASTART_UMFILEIN = \"$astart(umfile_in)\","
            puts $fid "  ASTART_UMFILEOUT = \"$astart(file_out)\","
            puts $fid "  IASTART_TIMEUSAGE1 = $astart(timeusage1),"
            puts $fid "  IASTART_TIMEUSAGE2 = $astart(timeusage2),"
            puts $fid "  IASTART_STARTDATE(1) = $astart(year),"
            puts $fid "  IASTART_STARTDATE(2) = $astart(mon),"
            puts $fid "  IASTART_STARTDATE(3) = $astart(day),"
            puts $fid "  IASTART_STARTDATE(4) = $astart(hour),"
            puts $fid "  IASTART_STARTDATE(5) = $astart(min),"
            puts $fid "  IASTART_STARTDATE(6) = $astart(sec),"
            if {$astart(usestdname)} {
                puts $fid "  LASTART_USESTDNAME = .TRUE.,"
            } else {
                puts $fid "  LASTART_USESTDNAME = .FALSE.,"
            }
            if {$astart(useconfig)} {
                puts $fid "  LASTART_USECONFIG = .TRUE.,"
            } else {
                puts $fid "  LASTART_USECONFIG = .FALSE.,"
            }
             set ncfs {}
             foreach m $astart(mods) {
                 if {[lsearch $ncfs [lindex $m 5]] == -1} {
                     lappend ncfs [lindex $m 5]
                 }
             }
             puts $fid "  IASTART_NNCFILES = [llength $ncfs],"
             set i 1
             foreach f $ncfs {
                 puts  $fid "  ASTART_NCFILES($i) = \"$f\","
                 incr i
             }
             puts $fid "  IASTART_NITEM = [llength $astart(mods)],"
             set i 1
             foreach m $astart(mods) {
                 set ncfileid [expr [lsearch $ncfs [lindex $m 5]] + 1]
                 set clstash -1
                 if {[lindex $m 1] != "n/a"} {set clstash [lindex $m 1]}
                 set pp -1
                 if {[lindex $m 2] != "n/a"} {set pp [lindex $m 2]}
                 puts  $fid "  IASTART_NCFILEID($i) = $ncfileid,"
                 puts  $fid "  ASTART_NCNAME($i) = \"[lindex $m 4]\","
                 puts  $fid "  IASTART_STASHCODE($i) = [lindex $m 0],"
                 puts  $fid "  IASTART_CLONE_STASHCODE($i) = $clstash,"
                 puts  $fid "  IASTART_PPCODE($i) = $pp,"
                 incr i
             }
         } elseif {$type == "ostart"} {
            puts $fid "  LOSTART = .TRUE.,"
            puts $fid "  OSTART_UMFILEIN = \"$ostart(umfile_in)\","
            puts $fid "  OSTART_UMFILEOUT = \"$ostart(file_out)\","
            puts $fid "  IOSTART_TIMEUSAGE1 = $ostart(timeusage1),"
            puts $fid "  IOSTART_TIMEUSAGE2 = $ostart(timeusage2),"
            puts $fid "  IOSTART_STARTDATE(1) = $ostart(year),"
            puts $fid "  IOSTART_STARTDATE(2) = $ostart(mon),"
            puts $fid "  IOSTART_STARTDATE(3) = $ostart(day),"
            puts $fid "  IOSTART_STARTDATE(4) = $ostart(hour),"
            puts $fid "  IOSTART_STARTDATE(5) = $ostart(min),"
            puts $fid "  IOSTART_STARTDATE(6) = $ostart(sec),"
            if {$ostart(usestdname)} {
                puts $fid "  LOSTART_USESTDNAME = .TRUE.,"
            } else {
                puts $fid "  LOSTART_USESTDNAME = .FALSE.,"
            }
            if {$ostart(useconfig)} {
                puts $fid "  LOSTART_USECONFIG = .TRUE.,"
            } else {
                puts $fid "  LOSTART_USECONFIG = .FALSE.,"
            }
             set ncfs {}
             foreach m $ostart(mods) {
                 if {[lsearch $ncfs [lindex $m 5]] == -1} {
                     lappend ncfs [lindex $m 5]
                 }
             }
             puts $fid "  IOSTART_NNCFILES = [llength $ncfs],"
             set i 1
             foreach f $ncfs {
                 puts  $fid "  OSTART_NCFILES($i) = \"$f\","
                 incr i
             }
             puts $fid "  IOSTART_NITEM = [llength $ostart(mods)],"
             set i 1
             foreach m $ostart(mods) {
                 set ncfileid [expr [lsearch $ncfs [lindex $m 5]] + 1]
                 set clstash -1
                 if {[lindex $m 1] != "n/a"} {set clstash [lindex $m 1]}
                 set pp -1
                 if {[lindex $m 2] != "n/a"} {set pp [lindex $m 2]}
                 puts  $fid "  IOSTART_NCFILEID($i) = $ncfileid,"
                 puts  $fid "  OSTART_NCNAME($i) = \"[lindex $m 4]\","
                 puts  $fid "  IOSTART_STASHCODE($i) = [lindex $m 0],"
                 puts  $fid "  IOSTART_CLONE_STASHCODE($i) = $clstash,"
                 puts  $fid "  IOSTART_PPCODE($i) = $pp,"
                 incr i
             }
            if {$ostart(bathy)} {
                puts $fid "  LOSTART_BATHY = .TRUE.,"
                puts $fid "  OSTART_BATHYFILE = \"$ostart(bathyfile)\","
                puts $fid "  OSTART_BATHYNCNAME = \"$ostart(bathyncname)\","
                if {$ostart(bathydepthmask)} {
                    puts $fid "  LOSTART_BATHY_DEPTHMASK = .TRUE.,"
                } else {
                    puts $fid "  LOSTART_BATHY_DEPTHMASK = .FALSE.,"
                }
            }
            if {$ostart(islandmod)} {
                if {$ostart(islandtype) == 1} {
                    puts $fid "  LOSTART_ISLANDS_REPLACE = .TRUE.,"
                    puts $fid "  LOSTART_ISLANDS_ADD = .FALSE.,"
                } else {
                    puts $fid "  LOSTART_ISLANDS_REPLACE = .FALSE.,"
                    puts $fid "  LOSTART_ISLANDS_ADD = .TRUE.,"
                }
                puts $fid "  OSTART_ISLANDS_FILEIN = \"$ostart(islandfile)\","
            }
         }
      } elseif {$type == "mask" && $mask(use)} {
         puts $fid "  LMASK = .FALSE.,"
         puts $fid "  LMASK_USE = .TRUE.,"
         puts $fid "  MASK_FILEIN = \"$mask(file_in)\","
         if {$mask(uselfrac)} {
            puts $fid "  LMASK_USELFRAC = .TRUE."
	 } else {
            puts $fid "  LMASK_USELFRAC = .FALSE.,"
            if {$mask(sea)} {
               puts $fid "  LMASK_SEA = .TRUE.,"
            } else {
               puts $fid "  LMASK_SEA = .FALSE.,"
            }
            if {$mask(usemdi)} {
               puts $fid "  LMASK_USEMDI = .TRUE.,"
            } else {
               puts $fid "  LMASK_USEMDI = .FALSE.,"
	       if {[isint $mask(val)]} {
                  puts $fid "  AMASK_VAL = [expr double($mask(val))],"
                  puts $fid "  IMASK_VAL = $mask(val),"
               } else {
                  puts $fid "  AMASK_VAL = $mask(val),"
                  puts $fid "  IMASK_VAL = [expr int($mask(val))],"
	       }
            }
            puts $fid "  MASK_NCNAME = \"$mask(name)\""
         }
      } elseif {$type == "lfrac" && $lfrac(use)} {
         puts $fid "  LLFRAC = .FALSE.,"
         puts $fid "  LLFRAC_USE = .TRUE.,"
         puts $fid "  LFRAC_FILEIN = \"$lfrac(file_in)\","
         puts $fid "  LFRAC_NCNAME = \"$lfrac(name)\""
      }
      puts $fid "$endrec\n"
   }
   close $fid
}

proc isReadable {f} {

   # The channel is readable; try to read it
    
   set status [catch { gets $f line } result]
   if { $status != 0 } {
      # Error on the channel
      puts stderr "Error reading $f: $result"
      set ::DONE 2
   } elseif { $result >= 0 } {
      # Successfully read the channel
      write_message $line
   } elseif { [eof $f] } {
      # End of file on the channel
      #puts "end of file"
      set ::DONE 1
   } elseif { [fblocked $f] } {
      # Read blocked.  Just return
   } else {
      # Something else
      puts "can't happen"
      set ::DONE 3
   }
}

proc run_exec {{exec_arg {}}} {

   global namelist exec errorInfo

#  set environment variable to stop pgi "FORTRAN STOP" message appearing 
#  on standard error and causing exec to return an error

   set ::env(NO_STOP_MESSAGE) 1

   if {$exec_arg == {}} {set exec_arg $exec}
   
   write_message "Running mkancil executable $exec_arg"

#  close should give errors if exec_arg fails but it doesn't work

   #set foutput [open "| $exec_arg < $namelist" r]
   #fconfigure $foutput -buffering none -blocking off
   #fileevent $foutput readable [list isReadable $foutput]
   #vwait ::DONE
   #close $foutput

   set output xancil.output
   set error false
   if {[catch {exec $exec_arg < $namelist > $output} results]} {
      set error true
      set savedInfo $errorInfo
   }
   write_message "Output from $exec_arg executable: \n"
   write_file $output
   file delete $output
   if {$error} {error "$exec_arg failed" $savedInfo}
}

# loadDir --
# This procedure reloads the directory listbox from the current directory
#
# Arguments:
# w -			Name of the toplevel window.
# dirname  -		Name of the current directory.
# filter1  -		Option filter to use, use global variable filter if
#                       not defined

proc loadDir {w dirname} {
   global filter

#  check directory exists

   if {! [file isdirectory $dirname]} {
      set dirname [file dirname $dirname]
      while {! [file isdirectory $dirname]} {
         set dirname [file dirname $dirname]
      }
      errorbox "incorrect directory selected"
   }

#   set dirname [noslash $dirname]
   cd $dirname
   set dirname [pwd]

   foreach i $filter {
      append filelist " [glob -nocomplain $dirname/$i]"
   }
   set a " "
   $w delete 0 end
   if {[info exists filelist]} {
      foreach i [lsort $filelist] {
         if {$a != $i} {
            if {[file isdirectory $i]} {
               $w insert end [file tail $i]/
            } else {
               $w insert end [file tail $i]
            }
         }
         set a $i
      }
   }
   return $dirname
}

# noslash --
# If dir has a trailing / remove it
#
# Arguments:
# dir -			Name of the directory.

proc noslash {dir} {

   if {[file tail $dir] == ""} {
      if {$dir == "/" || $dir == "" } {
         set dir1 ""
      } else {
         set dir1 [file dirname $dir]
      }
   } else {
      set dir1 $dir
   }

   return $dir1
}

# errorbox --
# Window to display error message
#
# Arguments:
# msg -			Error message to display.

proc errorbox {msg} {
   set w .errorbox
   catch {destroy $w}
   toplevel $w
   wm title $w "Error message"

   set ww $w.frame
   frame $ww -relief flat -borderwidth 8
   pack $ww -side left -fill both -expand yes

   message $ww.errmsg -width 80m -justify left -text $msg
   frame $ww.spacer -height 2m -width 20
   frame $ww.errbut -relief groove -borderwidth 4
   pack $ww.errmsg -fill both -expand yes
   pack $ww.spacer $ww.errbut -fill x -expand yes

   button $ww.errbut.ok -width 4 -text OK -command "destroy $w"
   pack $ww.errbut.ok -side left -expand yes

   tkwait visibility $w
   grab set $w
}

proc compactdirname {file} {

   set curdir [pwd]

   if [file isdirectory $file] {
      cd $file
      set file [pwd]
   } else {
      set newdir [file dirname $file]
      set filename [file tail $file]
      cd $newdir
      set file [pwd]/$filename
   }
   cd $curdir

   return $file
}

#  Write messages to output box

proc write_message {message} {

   set error [catch {.info configure -state normal}]
   if {! $error} {
      .info insert end "$message\n"
      .info yview end
      .info configure -state disabled
   }

   puts $message
}

#  Write files to output box

proc write_file {file} {

   set error [catch {.info configure -state normal}]
   set f [open $file]
   while {![eof $f]} {
      set segment [read $f 1000]
      if {! $error} {.info insert end $segment}
      puts -nonewline $segment
   }
   close $f
   if {! $error} {
      .info yview end
      .info configure -state disabled
   }
}

proc mesgbox {msg} {
   global ret

   set w .mesgbox
   catch {destroy $w}
   toplevel $w
   wm title $w "Message box"

   set ww $w.frame
   frame $ww -relief flat -borderwidth 8
   pack $ww -side left -fill both -expand yes

   message $ww.errmsg -width 80m -justify left -text $msg
   frame $ww.spacer -height 2m -width 20
   frame $ww.errbut -relief groove -borderwidth 4
   pack $ww.errmsg -fill both -expand yes
   pack $ww.spacer $ww.errbut -fill x -expand yes

   button $ww.errbut.y -width 4 -text Yes -command "set ret 0"
   button $ww.errbut.n -width 4 -text No -command "set ret 1"
   pack $ww.errbut.y $ww.errbut.n -side left -expand yes

   tkwait visibility $w
   grab set $w
   tkwait variable ret
   destroy $w
   return $ret
}

# readarg --
# Read and process command line arguments
#

proc readarg {argv} {

    global jobfilename savefile namelist namelisttype exec

    set execute false
    
    #puts "argv = $argv"
    set unknown false
    set arglen [llength $argv]
    set index 0
    while {$index < $arglen} {
       set arg [lindex $argv $index]
       switch -glob -- $arg {
          -execute      {set execute true}
          -x            {set execute true}
          -execname     {set exec [nextarg $argv index]}
          -xn           {set exec [nextarg $argv index]}
          -jobfile      {set jobfile [lappend jobfile [nextarg $argv index]]}
          -j            {set jobfile [lappend jobfile [nextarg $argv index]]}
          -namelist     {set namelist [nextarg $argv index]}
          -nl           {set namelist [nextarg $argv index]}
          -namelisttype {set namelisttype [nextarg $argv index]}
          -script       {set script [lappend script [nextarg $argv index]]}
          -sc           {set script [lappend script [nextarg $argv index]]}
          -usage        {usage ; exit}
          -*            {puts stderr "unknown option $arg" ; set unknown true}
       }
       incr index
    }
    
    if {$unknown} {usage}

    if {[info exists jobfile]} {
       set jobfilename [file normalize [lindex $jobfile 0]]
       if {[llength $jobfile] > 1} {
          puts stderr "Only one job file can be specified, using $jobfilename"
       }

       global ancil_types
       foreach type $ancil_types {global $type}

       puts "Loading job configuration from file $jobfilename"

       set error [catch {source $jobfilename} msg]
       if {$error} {
          puts stderr "Error reading job configuration file $jobfilename"
          puts stderr $msg
       }
       set savefile $jobfilename
    }
    
    if {[info exists script]} {
       set scriptname [file normalize [lindex $script 0]]
       if {[llength $script] > 1} {
          puts stderr "Only one job file can be specified, using $scriptname"
       }

       global ancil_types
       foreach type $ancil_types {global $type}

       set error [catch {source $scriptname} msg]
       if {$error} {
          puts stderr "Error running script file $scriptname"
          puts stderr $msg
       }
       exit
    }

    if {[info exists exec]} {
       set exec [file normalize $exec]
    }

    if {[info exists namelist]} {
       set namelist [file normalize $namelist]
    }

    if {$execute} {
       if {! [info exists jobfilename]} {
          puts stderr "Error no job file specified on command line"
       } else {
          create_namelist $namelist
          run_exec $exec
	  exit
       }
    }
}

proc nextarg {argv indexref} {

   upvar index $indexref
   set arg [lindex $argv $index]
   set nextarg [lindex $argv [expr $index + 1]]
   
   if {[string range $nextarg 0 0] == "-" || "$nextarg" == "" } {
      puts stderr "option $arg needs value"
      return ""
   } else {
      incr index
      return $nextarg
   }
}

#  Print out command line options

proc usage {} {

   global xancil_version versiondate argv0 exec namelist namelisttype

   if {[file tail $argv0] == "main.tcl"} {
      set name [info nameofexecutable]
   } else {
      set name $argv0
   }
   puts "\n$name : version $xancil_version ($versiondate)\n"
   puts "Command line options\n"
   puts " -execute             : Create namelist and run ancillary executable"
   puts " -x                   : As -execute"
   puts " -execname execfile   : Specify pathname of ancillary executable"
   puts "                        (default $exec)"
   puts " -xn execfile         : As -execname"
   puts " -jobfile jobfile     : Load jobfile into xancil"
   puts " -j jobfile           : As -jobfile"
   puts " -namelist namelist   : Create namelist file called namelist"
   puts "                        (default $namelist)"
   puts " -nl namelist         : As -namelist"
   puts " -namelisttype nltype : Specify namelist type, either old or f90"
   puts "                        (default $namelisttype)"
   puts " -script script       : Use xancil to run tcl script"
   puts " -sc script           : As -script"
   puts " -usage               : Print this message"
   puts ""
}

#  Setup xancil defaults

proc setupdefaults {} {

   global ancil_types defanc0 defanc00
   foreach type $ancil_types {global $type}

   set config(create) 1
   set config(version) 4.5
   set config(cal) 2
   set config(size) 64
   set config(pack32) 0
   set config(end) 1
   set config(wfio) 0
   set config(wfio_size) 2048
   set config(ncfile) ""
   set config(smfile) ""

   set gridconfig(create) 1
   set gridconfig(vargrid) 0
   set gridconfig(avert) 0
   set gridconfig(avert_file_in) ""
   set gridconfig(avert_name) ""
   set gridconfig(ozonelev) 0
   set gridconfig(nlev) ""
   set gridconfig(nolev) ""
   set gridconfig(levstoreup) 1
   set gridconfig(vert_namelist) ""
   set gridconfig(theta) 1

   set gridconfig(overt) 0
   set gridconfig(overt_file_in) ""
   set gridconfig(overt_name) ""

   set gridconfig(deepsoil) 1
   set gridconfig(nsoillev) 4
   set gridconfig(soillev1) 0.1
   set gridconfig(soillev2) 0.25
   set gridconfig(soillev3) 0.65
   set gridconfig(soillev4) 2.0
   
   set gridconfig(useglobtime) 1
   set gridconfig(timeusage2) 0
   set gridconfig(startyear) 0000
   set gridconfig(startmon) 1
   set gridconfig(startday) 16
   set gridconfig(starthour) 0
   set gridconfig(startmin) 0
   set gridconfig(startsec) 0
   set gridconfig(ntimes) 12
   set gridconfig(interval) 1
   set gridconfig(intunit) 1
   set gridconfig(mm) 1

   set ozone(create) 0
   set ozone(file_in) [pwd]/ozone.nc
   set ozone(name) ""
   set ozone(file_out) [pwd]/ozone
   set ozone(mixtype) 1
   set ozone(mixfac) 1.0
   set ozone(periodic) 1
   set ozone(timeusage1) 0
   set ozone(useglobtime) 1
   set ozone(timeusage2) 0
   set ozone(startyear) 0000
   set ozone(startmon) 1
   set ozone(startday) 16
   set ozone(starthour) 0
   set ozone(startmin) 0
   set ozone(startsec) 0
   set ozone(ntimes) 12
   set ozone(interval) 1
   set ozone(intunit) 1
   set ozone(mm) 1

   set smow(create) 0
   set smow(file_in) [pwd]/smow.nc
   set smow(snowdepthname) ""
   set smow(snowedgename) ""
   set smow(soilmoisturename) ""
   set smow(file_out) [pwd]/smow
   set smow(calcsnowedge) 1
   set smow(periodic) 1
   set smow(mask) 0
   set smow(timeusage1) 0
   set smow(useglobtime) 1
   set smow(timeusage2) 0
   set smow(startyear) 0000
   set smow(startmon) 1
   set smow(startday) 16
   set smow(starthour) 0
   set smow(startmin) 0
   set smow(startsec) 0
   set smow(ntimes) 12
   set smow(interval) 1
   set smow(intunit) 1
   set smow(mm) 1

   set slt(create) 0
   set slt(file_in) [pwd]/slt.nc
   set slt(name) ""
   set slt(file_out) [pwd]/slt
   set slt(periodic) 1
   set slt(mask) 0
   set slt(timeusage1) 0
   set slt(useglobtime) 1
   set slt(timeusage2) 0
   set slt(startyear) 0000
   set slt(startmon) 1
   set slt(startday) 16
   set slt(starthour) 0
   set slt(startmin) 0
   set slt(startsec) 0
   set slt(ntimes) 12
   set slt(interval) 1
   set slt(intunit) 1
   set slt(mm) 1

   set soil(create) 0
   set soil(file_in) [pwd]/soil.nc
   set soil(vsmcwiltname) ""
   set soil(vsmccritname) ""
   set soil(vsmcfcapname) ""
   set soil(vsmcsatname) ""
   set soil(clapphornname) ""
   set soil(thermcondname) ""
   set soil(soilcondname) ""
   set soil(thermcapname) ""
   set soil(soilwatersucname) ""
   set soil(soilalbname) ""
   set soil(soilcarbname) ""
   set soil(file_out) [pwd]/soil
   set soil(mask) 0

   set veg(create) 0
   set veg(file_in) [pwd]/veg.nc
   set veg(rootdepthname) ""
   set veg(sfaname) ""
   set veg(surfresistname) ""
   set veg(z0name) ""
   set veg(cancapname) ""
   set veg(vegfracname) ""
   set veg(infiltname) ""
   set veg(dsaname) ""
   set veg(lainame) ""
   set veg(canhtname) ""
   set veg(file_out) [pwd]/veg
   set veg(mask) 0

   set vegfrac(create) 0
   set vegfrac(file_in) [pwd]/vegfrac.nc
   set vegfrac(name) ""
   set vegfrac(surftypes) 9
   set vegfrac(file_out) [pwd]/vegfrac
   set vegfrac(mask) 0

   set vegfunc(create) 0
   set vegfunc(file_in) [pwd]/vegfunc.nc
   set vegfunc(lainame) ""
   set vegfunc(canhtname) ""
   set vegfunc(cancondname) ""
   set vegfunc(functypes) 5
   set vegfunc(file_out) [pwd]/vegfunc
   set vegfunc(periodic) 1
   set vegfunc(mask) 0
   set vegfunc(timeusage1) 0
   set vegfunc(useglobtime) 1
   set vegfunc(timeusage2) 0
   set vegfunc(startyear) 0000
   set vegfunc(startmon) 1
   set vegfunc(startday) 16
   set vegfunc(starthour) 0
   set vegfunc(startmin) 0
   set vegfunc(startsec) 0
   set vegfunc(ntimes) 12
   set vegfunc(interval) 1
   set vegfunc(intunit) 1
   set vegfunc(mm) 1

   set vegdist(create) 0
   set vegdist(file_in) [pwd]/vegdist.nc
   set vegdist(name) ""
   set vegdist(file_out) [pwd]/vegdist
   set vegdist(mask) 0

   set sst(create) 0
   set sst(file_in) [pwd]/sst.nc
   set sst(name) ""
   set sst(file_out) [pwd]/sst
   set sst(setminval) 1
   set sst(minval) 271.4
   set sst(seticeval) 1
   set sst(iceval) 271.35
   set sst(periodic) 1
   set sst(mask) 0
   set sst(timeusage1) 0
   set sst(useglobtime) 1
   set sst(timeusage2) 0
   set sst(startyear) 0000
   set sst(startmon) 1
   set sst(startday) 16
   set sst(starthour) 0
   set sst(startmin) 0
   set sst(startsec) 0
   set sst(ntimes) 12
   set sst(interval) 1
   set sst(intunit) 1
   set sst(mm) 1

   set ice(create) 0
   set ice(usesstval) 1
   set ice(sstval) 273.35
   set ice(amip2) 0
   set ice(calcedge) 1
   set ice(calcdepth) 1
   set ice(file_in) [pwd]/ice.nc
   set ice(concname) ""
   set ice(edgename) ""
   set ice(depthname) ""
   set ice(file_out) [pwd]/ice
   set ice(percent) 0
   set ice(mkmask) 0
   set ice(cutoffval) 0.5
   set ice(setminval) 0
   set ice(minval) 0.5
   set ice(setmaxval) 0
   set ice(maxval) 1.0
   set ice(periodic) 1
   set ice(mask) 0
   set ice(timeusage1) 2
   set ice(useglobtime) 1
   set ice(timeusage2) 0
   set ice(startyear) 0000
   set ice(startmon) 1
   set ice(startday) 16
   set ice(starthour) 0
   set ice(startmin) 0
   set ice(startsec) 0
   set ice(ntimes) 12
   set ice(interval) 1
   set ice(intunit) 1
   set ice(mm) 1

   set orog(create) 0
   set orog(file_in) [pwd]/orog.nc
   set orog(name) ""
   set orog(sdname) ""
   set orog(xgradname) ""
   set orog(ygradname) ""
   set orog(xxgradname) ""
   set orog(xygradname) ""
   set orog(yygradname) ""
   set orog(silname) ""
   set orog(pthtname) ""
   set orog(unfiltname) ""
   set orog(file_out) [pwd]/orog
   set orog(incgrad) 0
   set orog(incsqgrad) 1
   set orog(incrough) 1
   set orog(incunfilt) 0
   set orog(mask) 0

   set mask(use) 0
   set mask(create) 0
   set mask(outflow) 0
   set mask(file_in) [pwd]/mask.nc
   set mask(uselfrac) 0
   set mask(usemdi) 0
   set mask(sea) 0
   set mask(val) 1
   set mask(name) ""
   set mask(ofname) ""
   set mask(file_out) [pwd]/mask

   set lfrac(use) 0
   set lfrac(create) 0
   set lfrac(file_in) [pwd]/lfrac.nc
   set lfrac(name) ""
   set lfrac(file_out) [pwd]/lfrac

   set ws(create) 0

   set htflux(create) 0

   set pme(create) 0

   set ts1(create) 0
   set ts1(file_in) [pwd]/sss_sst.nc
   set ts1(refsst) 1
   set ts1(refsss) 1
   set ts1(climat) 0
   set ts1(climid) 0
   set ts1(sstname) ""
   set ts1(sssname) ""
   set ts1(atname) ""
   set ts1(idname) ""
   set ts1(file_out) [pwd]/sss_sst
   set ts1(periodic) 1
   set ts1(mask) 1
   set ts1(timeusage1) 0
   set ts1(useglobtime) 1
   set ts1(timeusage2) 0
   set ts1(startyear) 0000
   set ts1(startmon) 1
   set ts1(startday) 16
   set ts1(starthour) 0
   set ts1(startmin) 0
   set ts1(startsec) 0
   set ts1(ntimes) 12
   set ts1(interval) 1
   set ts1(intunit) 1
   set ts1(mm) 1

   set iceff(create) 0

   set flux(create) 0
   set flux(file_in) [pwd]/fluxc.nc
   set flux(heat) 1
   set flux(salt) 1
   set flux(heatname) ""
   set flux(saltname) ""
   set flux(file_out) [pwd]/fluxc
   set flux(periodic) 1
   set flux(mask) 1
   set flux(timeusage1) 0
   set flux(useglobtime) 1
   set flux(timeusage2) 0
   set flux(startyear) 0000
   set flux(startmon) 1
   set flux(startday) 16
   set flux(starthour) 0
   set flux(startmin) 0
   set flux(startsec) 0
   set flux(ntimes) 12
   set flux(interval) 1
   set flux(intunit) 1
   set flux(mm) 1

   set defanc0(create) 0
   set defanc0(file_out) [pwd]/anc
   set defanc0(model) 1
   set defanc0(inthd8) 0
   set defanc0(inthd8val) 1
   set defanc0(periodic) 1
   set defanc0(timeusage1) 0
   set defanc0(useglobtime) 1
   set defanc0(timeusage2) 0
   set defanc0(startyear) 0000
   set defanc0(startmon) 1
   set defanc0(startday) 16
   set defanc0(starthour) 0
   set defanc0(startmin) 0
   set defanc0(startsec) 0
   set defanc0(ntimes) 12
   set defanc0(interval) 1
   set defanc0(intunit) 1
   set defanc0(mm) 1
   set defanc0(nfield) 1

   set defanc00(file_in) ""
   set defanc00(stashcode) ""
   set defanc00(ppcode) ""
   set defanc00(varname) ""
   set defanc00(levtype) 0
   set defanc00(nlev) 0
   set defanc00(theta) 1
   set defanc00(gridtype) 1
   set defanc00(datatype) 1
   set defanc00(masktype) 0
   set defanc00(mask) 0

   set ausrmulti(create) 0
   set ausrmulti(file_out) [pwd]/ausrmulti
   set ausrmulti(model) 1
   set ausrmulti(periodic) 1
   set ausrmulti(timeusage1) 0
   set ausrmulti(useglobtime) 1
   set ausrmulti(timeusage2) 0
   set ausrmulti(startyear) 0000
   set ausrmulti(startmon) 1
   set ausrmulti(startday) 16
   set ausrmulti(starthour) 0
   set ausrmulti(startmin) 0
   set ausrmulti(startsec) 0
   set ausrmulti(ntimes) 12
   set ausrmulti(interval) 1
   set ausrmulti(intunit) 1
   set ausrmulti(mm) 1
   set ausrmulti(nfield) 1
   foreach el [array names defanc00] {
      if {! [info exists ausrmulti(1,$el)]} {
         set ausrmulti(1,$el) $defanc00($el)
      }
   }

   set ausrancil(create) 0
   set ausrancil(file_out) [pwd]/ausrancil
   set ausrancil(model) 1
   set ausrancil(periodic) 1
   set ausrancil(timeusage1) 0
   set ausrancil(useglobtime) 1
   set ausrancil(timeusage2) 0
   set ausrancil(startyear) 0000
   set ausrancil(startmon) 1
   set ausrancil(startday) 16
   set ausrancil(starthour) 0
   set ausrancil(startmin) 0
   set ausrancil(startsec) 0
   set ausrancil(ntimes) 12
   set ausrancil(interval) 1
   set ausrancil(intunit) 1
   set ausrancil(mm) 1
   set ausrancil(nfield) 1
   foreach el [array names defanc00] {
      if {! [info exists ausrancil(1,$el)]} {
         set ausrancil(1,$el) $defanc00($el)
      }
   }

   set ousrmulti(create) 0
   set ousrmulti(file_out) [pwd]/ousrmulti
   set ousrmulti(model) 2
   set ousrmulti(periodic) 1
   set ousrmulti(timeusage1) 0
   set ousrmulti(useglobtime) 1
   set ousrmulti(timeusage2) 0
   set ousrmulti(startyear) 0000
   set ousrmulti(startmon) 1
   set ousrmulti(startday) 16
   set ousrmulti(starthour) 0
   set ousrmulti(startmin) 0
   set ousrmulti(startsec) 0
   set ousrmulti(ntimes) 12
   set ousrmulti(interval) 1
   set ousrmulti(intunit) 1
   set ousrmulti(mm) 1
   set ousrmulti(nfield) 1
   foreach el [array names defanc00] {
      if {! [info exists ousrmulti(1,$el)]} {
         set ousrmulti(1,$el) $defanc00($el)
      }
   }

   set ousrancil(create) 0
   set ousrancil(file_out) [pwd]/ousrancil
   set ousrancil(model) 2
   set ousrancil(periodic) 1
   set ousrancil(timeusage1) 0
   set ousrancil(useglobtime) 1
   set ousrancil(timeusage2) 0
   set ousrancil(startyear) 0000
   set ousrancil(startmon) 1
   set ousrancil(startday) 16
   set ousrancil(starthour) 0
   set ousrancil(startmin) 0
   set ousrancil(startsec) 0
   set ousrancil(ntimes) 12
   set ousrancil(interval) 1
   set ousrancil(intunit) 1
   set ousrancil(mm) 1
   set ousrancil(nfield) 1
   foreach el [array names defanc00] {
      if {! [info exists ousrancil(1,$el)]} {
         set ousrancil(1,$el) $defanc00($el)
      }
   }

   set usrtr(create) 0

   set genanc_config(create) 1
   set genanc_config(nancfile) 1

   set genanc(create) 1
   set genanc(1,create) 0

   set astart(create) 0
   set astart(umfile_in) ""
   set astart(file_out) [pwd]/newdump.astart
   set astart(timeusage1) 0
   set astart(timeusage2) 0
   set astart(year) 0000
   set astart(mon) 1
   set astart(day) 16
   set astart(hour) 0
   set astart(min) 0
   set astart(sec) 0
   set astart(usestdname) 0
   set astart(useconfig) 0
   set astart(ncfile) ""
   set astart(mods) {}

   set ostart(create) 0
   set ostart(umfile_in) ""
   set ostart(file_out) [pwd]/newdump.ostart
   set ostart(timeusage1) 0
   set ostart(timeusage2) 0
   set ostart(year) 0000
   set ostart(mon) 1
   set ostart(day) 16
   set ostart(hour) 0
   set ostart(min) 0
   set ostart(sec) 0
   set ostart(usestdname) 0
   set ostart(useconfig) 0
   set ostart(ncfile) ""
   set ostart(mods) {}
   set ostart(bathy) 0
   set ostart(bathyfile) ""
   set ostart(bathyncname) ""
   set ostart(bathydepthmask) 1
   set ostart(islandmod) 0
   set ostart(islandtype) 1
   set ostart(islandfile) ""
}

# procs to create multiple use widgets

proc spacer {win} {

   global spacerwidth spacerht

   frame $win -height $spacerht -width $spacerwidth
   pack $win -side top
}

proc title {win title} {

   global bold_font_large

   frame $win
   pack $win -side top -fill both -expand yes

   label $win.title -text $title -font $bold_font_large
   pack $win.title -side top
}

proc create_win1 {win1 win2 label var} {

   frame $win1
   pack $win1 -side top -fill both -expand yes

   checkbutton $win1.cbut -text $label -variable $var -anchor w \
       -command "if {$$var} {
                    pack $win2 -expand yes -fill x -side top
		 } else {
		    pack forget $win2
		 }"
		 
   pack $win1.cbut -side top -fill none -expand yes -anchor w
}

proc create_win2 {win1 win2 label var} {

   frame $win1
   pack $win1 -side top -fill both -expand yes

   label $win1.label -text $label
   radiobutton $win1.button1 -text "yes " -variable $var -value 1 \
       -command "pack $win2 -expand yes -fill x -side bottom"
   radiobutton $win1.button2 -text "no  " -variable $var -value 0 \
       -command "pack forget $win2"

   pack $win1.label -side left -fill none -expand yes -anchor w
   pack $win1.button1 $win1.button2 -side left -fill none -expand no
}

proc use_file_create_win2 {win1 win2 win3 label1 label2 var1 var2} {

   frame $win1
   pack $win1 -side top -fill both -expand yes

   frame $win1.use
   label $win1.use.label -text $label1
   radiobutton $win1.use.button1 -text "yes " -variable $var1 -value 1 \
       -command "use_create $$var1 $$var2 $win2 $win3"
   radiobutton $win1.use.button2 -text "no  " -variable $var1 -value 0 \
       -command "use_create $$var1 $$var2 $win2 $win3"

   pack $win1.use -side top -fill both -expand yes
   pack $win1.use.label -side left -fill none -expand yes -anchor w
   pack $win1.use.button1 $win1.use.button2 -side left -fill none -expand no
   
   frame $win1.create
   label $win1.create.label -text $label2
   radiobutton $win1.create.button1 -text "yes " -variable $var2 -value 1 \
       -command "use_create $$var1 $$var2 $win2 $win3"
   radiobutton $win1.create.button2 -text "no  " -variable $var2 -value 0 \
       -command "use_create $$var1 $$var2 $win2 $win3"

   pack $win1.create -side top -fill both -expand yes
   pack $win1.create.label -side left -fill none -expand yes -anchor w
   pack $win1.create.button1 $win1.create.button2 -side left -fill none -expand no
}

proc get_filename {win label filevar {filter *} {filecom {}}} {

   frame $win
   pack $win -side top -fill both -expand yes

   upvar #0 $filevar file
   
   label $win.fileLabel -text $label
   frame $win.file
   button $win.file.fileButton -padx 2 -pady 1 -text "Browse" \
          -command "getfile $filevar $filter \"$filecom {}\""
   entry $win.file.fileEntry -textvariable $filevar

   pack $win.fileLabel -side top -fill both -expand yes
   pack $win.file -side top -fill both -expand yes
   pack $win.file.fileEntry -side left -fill both -expand yes
   pack $win.file.fileButton -ipady 1 -padx 4 -side left -fill both -expand no

   if {$filecom != ""} {
      bind $win.file.fileEntry <Destroy> "$filecom {} $$filevar"
      bind $win.file.fileEntry <Return> "$filecom $win.file.fileEntry $$filevar"
      $filecom $win.file.fileEntry $file
   }
}

proc get_filename1 {win label filevar {filter *} {filecom {}}} {

   global config

   frame $win
   pack $win -side top -fill both -expand yes

   upvar #0 $filevar file

   label $win.fileLabel -text $label
   frame $win.file
   combobox::combobox $win.file.fileEntry -textvariable $filevar \
                      -listvar config(ncfile) \
                      -borderwidth 2 -elementborderwidth 2 \
                      -command get_filename_select \
                      -highlightthickness 1 -editable true
   button $win.file.fileButton -padx 2 -pady 1 -text "Browse" \
          -command "getfile $filevar $filter \"$filecom $win.file.fileEntry\""

   pack $win.fileLabel -side top -fill both -expand yes
   pack $win.file -side top -fill both -expand yes
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

proc get_filename2 {win label filevar {filter *} {filecom {}}} {

   frame $win
   pack $win -side top -fill both -expand yes

   upvar #0 $filevar file

   label $win.fileLabel -text $label
   entry $win.fileEntry -textvariable $filevar
   button $win.fileButton -padx 2 -pady 1 -text "Browse" \
          -command "getfile $filevar $filter \"$filecom {}\""

   pack $win.fileLabel -side left -fill x -expand no
   pack $win.fileEntry -side left -fill x -expand yes
   pack $win.fileButton -ipady 1 -padx 4 -side left -fill x -expand no

   if {$filecom != ""} {
      bind $win.fileEntry <Destroy> "$filecom {} $$filevar"
      bind $win.fileEntry <Return> "$filecom $win.fileEntry $$filevar"
      $filecom $win.fileEntry $file
   }
}

proc get_filename3 {win label filevar0 filevar1 {filter *} {filecom {}}} {

   global config

   frame $win
   pack $win -side top -fill both -expand yes

   upvar #0 $filevar1 file1

   if {$file1 == ""} {
      upvar #0 $filevar0 file0
      set file1 $file0
   }

   label $win.fileLabel -text $label
   combobox::combobox $win.fileEntry -textvariable $filevar1 \
                      -listvar config(ncfile) \
                      -borderwidth 2 -elementborderwidth 2 \
                      -command get_filename_select \
                      -highlightthickness 1 -editable true
   button $win.fileButton -padx 2 -pady 1 -text "Browse" \
          -command "getfile $filevar1 $filter \"$filecom $win.fileEntry\""

   pack $win.fileLabel -side left -fill both -expand no
   pack $win.fileEntry -side left -fill both -expand yes
   pack $win.fileButton -ipady 1 -padx 4 -side right -fill both -expand no

   if {$filecom != ""} {
      bind [$win.fileEntry subwidget entry] <Destroy> \
           "$filecom {} $$filevar1"
      bind [$win.fileEntry subwidget entry] <Return> \
           "$filecom $win.fileEntry $$filevar1"
      bind [$win.fileEntry subwidget listbox] <Unmap> \
           "$filecom $win.fileEntry $$filevar1"
      $filecom $win.fileEntry $file1
   }
}

proc get_filename_select {win val} {
   $win xview moveto 1
}

proc set_filename {win label file {filter *}} {

   frame $win
   pack $win -side top -fill both -expand yes

   label $win.fileLabel -text $label
   frame $win.file
   entry $win.file.fileEntry -textvariable $file
   button $win.file.fileButton -padx 2 -pady 1 -text "Browse" \
                               -command "setfile $file $filter"

   pack $win.fileLabel -side top -fill both -expand yes
   pack $win.file -side top -fill both -expand yes
   pack $win.file.fileEntry -side left -fill both -expand yes
   pack $win.file.fileButton -ipady 1 -padx 4 -side left -fill both -expand no
}

proc select_varname {win label var {width 5}} {

   frame $win
   pack $win -side top -fill both -expand yes

   label $win.label -text $label
   entry $win.entry -width $width -textvariable $var

   pack $win.label -side left -fill none -expand no
   pack $win.entry -side left -fill both -expand yes -anchor e
}

proc select_varname1 {win label namevar filevar \
                      {dim false} {width 5} {stdnamevar ""}} {

   global backgroundcolour
   
   frame $win
   pack $win -side top -fill both -expand yes

   label $win.label -text $label
   label $win.entry -width $width -textvariable $namevar -relief sunken -anchor w
   button $win.select -padx 2 -pady 1 -text "Select" \
          -command "select_ncvarname $$filevar [list $dim $namevar $stdnamevar]"
   bind ncvarselectwin <Destroy> ""

   pack $win.label -side left -fill none -expand no
   pack $win.entry -side left -fill both -expand yes -anchor e
   pack $win.select -ipady 1 -padx 4 -side right -fill both -expand no
}

proc select_varname_chk {win label var1 var2 {width 5}} {

   global disablecolour foregroundcolour

   frame $win
   pack $win -side top -fill both -expand yes

   checkbutton $win.cbut -text $label -variable $var1 -anchor w \
       -highlightthickness 0 \
       -command "if \"! $$var1\" {
                   $win.entry configure -state disabled
                   $win.cbut configure -foreground $disablecolour
                 } else {
                   $win.entry configure -state normal
                   $win.cbut configure -foreground $foregroundcolour
		 }"
   entry $win.entry -width $width -textvariable $var2
		 
   pack $win.cbut -side left -fill none -expand no -anchor w
   pack $win.entry -side left -fill both -expand yes -anchor w
   
   upvar #0 $var1 var3
   if {! $var3} {
      $win.entry configure -state disabled
      $win.cbut configure -foreground $disablecolour
   }
}

proc select_varname1_chk {win label var1 var2 filevar {dim false} {width 5}} {

   global disablecolour foregroundcolour

   frame $win
   pack $win -side top -fill both -expand yes

   checkbutton $win.cbut -text $label -variable $var1 -anchor w \
       -highlightthickness 0 \
       -command "if \"! $$var1\" {
                   $win.entry configure -state disabled
                   $win.cbut configure -foreground $disablecolour
                 } else {
                   $win.entry configure -state normal
                   $win.cbut configure -foreground $foregroundcolour
		 }"
   entry $win.entry -width $width -textvariable $var2
   button $win.select -padx 2 -pady 1 -text "Select" \
          -command "select_ncvarname $$filevar $dim $var2 "" $win.entry"
   bind ncvarselectwin <Destroy> ""
		 
   pack $win.cbut -side left -fill none -expand no -anchor w
   pack $win.entry -side left -fill both -expand yes -anchor w
   pack $win.select -ipady 1 -padx 4 -side right -fill both -expand no

   upvar #0 $var1 var3
   if {! $var3} {
      $win.entry configure -state disabled
      $win.cbut configure -foreground $disablecolour
   }
}

proc set_var {win label var {width 5} {expand no}} {

   frame $win
   pack $win -side top -fill both -expand yes

   label $win.label -text $label
   entry $win.entry -width $width -textvariable $var

   pack $win.label -side left -fill none -expand no
   if {$expand} {
      pack $win.entry -side left -fill both -expand yes -anchor e
   } else {
      pack $win.entry -side left -fill y -expand no -anchor e
   }
}

proc set_var2 {win label var text1 val1 text2 val2 {command {}}} {

   frame $win
   pack $win -side top -fill both -expand yes

   label $win.label -text $label -justify left
   radiobutton $win.but1 -text $text1 -variable $var -value $val1 -command $command
   radiobutton $win.but2 -text $text2 -variable $var -value $val2 -command $command

   pack $win.label -side left -fill none -expand yes -anchor w
   pack $win.but1 $win.but2 -side left -fill none -expand no
}

proc set_var3 {win label var text1 val1 text2 val2 text3 val3 {command {}}} {

   frame $win
   pack $win -side top -fill both -expand yes

   label $win.label -text $label -justify left
   radiobutton $win.but1 -text $text1 -variable $var -value $val1 -command $command
   radiobutton $win.but2 -text $text2 -variable $var -value $val2 -command $command
   radiobutton $win.but3 -text $text3 -variable $var -value $val3 -command $command

   pack $win.label -side left -fill none -expand yes -anchor w
   pack $win.but1 $win.but2 $win.but3 -side left -fill none -expand no
}

proc set_2var {win label1 label2 var1 var2 {width1 5} {width2 5}} {

   frame $win
   pack $win -side top -fill both -expand yes

   label $win.label1 -text $label1
   entry $win.entry1 -width $width1 -textvariable $var1
   label $win.label2 -text $label2
   entry $win.entry2 -width $width2 -textvariable $var2

   pack $win.label1 -side left -fill none -expand no
   pack $win.entry1 -side left -fill both -expand yes -anchor e
   pack $win.label2 -side left -fill none -expand no
   pack $win.entry2 -side left -fill both -expand yes -anchor e
}

proc set_2var1 {win label1 label2 var1 var2 range {width1 5} {width2 5}} {

   global selectlist

   frame $win
   pack $win -side top -fill both -expand yes

   set selectlist ""
   for {set i [lindex $range 0]} {$i <= [lindex $range 1]} {incr i} {
      lappend selectlist $i
   }

   label $win.label1 -text $label1
   combobox::combobox $win.entry1 -textvariable $var1 -listvar selectlist \
                      -width $width1 -borderwidth 2 -elementborderwidth 2 \
                      -highlightthickness 1 -editable no

   label $win.label2 -text $label2
   entry $win.entry2 -width $width2 -textvariable $var2

   pack $win.label1 -side left -fill none -expand no
   pack $win.entry1 -side left -fill both -expand yes -anchor e
   pack $win.label2 -side left -fill none -expand no
   pack $win.entry2 -side left -fill both -expand yes -anchor e
}

proc set_3var {win label1 label2 label3 var1 var2 var3 {width1 5} {width2 5} {width3 5}} {

   frame $win
   pack $win -side top -fill both -expand yes

   label $win.label1 -text $label1
   entry $win.entry1 -width $width1 -textvariable $var1
   label $win.label2 -text $label2
   entry $win.entry2 -width $width2 -textvariable $var2
   label $win.label3 -text $label3
   entry $win.entry3 -width $width3 -textvariable $var3

   pack $win.label1 -side left -fill none -expand no
   pack $win.entry1 -side left -fill both -expand no -anchor e
   pack $win.label2 -side left -fill none -expand no
   pack $win.entry2 -side left -fill both -expand no -anchor e
   pack $win.label3 -side left -fill none -expand no
   pack $win.entry3 -side left -fill both -expand no -anchor e
}

proc set_var_spin {win label var {width 5}} {

   frame $win
   pack $win -side top -fill both -expand yes

   label $win.label -text $label
   spinbox $win.spinbox -width $width -textvariable $var -increment 1 -from 0 -to 100

   pack $win.label -side left -fill none -expand no
   pack $win.spinbox -ipady 1 -side left -fill y -expand no -anchor e
}

proc set_var_chk {win label var1 var2 {width 5}} {

   global disablecolour foregroundcolour

   frame $win
   pack $win -side top -fill both -expand yes

   checkbutton $win.cbut -text $label -variable $var1 -anchor w \
       -highlightthickness 0 \
       -command "if \"! $$var1\" {
                   $win.entry configure -state disabled
                   $win.cbut configure -foreground $disablecolour
                 } else {
                   $win.entry configure -state normal
                   $win.cbut configure -foreground $foregroundcolour
		 }"
   entry $win.entry -width $width -textvariable $var2
		 
   pack $win.cbut -side left -fill y -expand no -anchor w
   pack $win.entry -side left -fill y -expand no -anchor w
   
   upvar #0 $var1 var3
   if {! $var3} {
      $win.entry configure -state disabled
      $win.cbut configure -foreground $disablecolour
   }
}

proc set_boolvar {win label var {command {}}} {

   frame $win
   pack $win -side top -fill both -expand yes

   checkbutton $win.cbut -text $label -variable $var -anchor w \
       -command $command
		 
   pack $win.cbut -side top -fill none -expand yes -anchor w
}

proc set_boolvar2 {win label var {command {}}} {

   frame $win
   pack $win -side top -fill both -expand yes

   label $win.label -text $label -justify left
   radiobutton $win.button1 -text "yes " -variable $var -value 1 -command $command
   radiobutton $win.button2 -text "no  " -variable $var -value 0 -command $command

   pack $win.label -side left -fill none -expand yes -anchor w
   pack $win.button1 $win.button2 -side left -fill none -expand no
}

proc set_levtype {win levtype nlev theta model} {

   global config gridconfig

   upvar #0 $levtype levtype1
   upvar #0 $nlev nlev1
   upvar #0 $model model1
   
   #puts "win=$win"
   #puts "$levtype=$levtype1"
   #puts "1 $nlev=$nlev1"
   #puts "$theta"
   #puts "$model=$model1"

   if {! [info exists levtype1]} {set levtype1 0}

   set_levcombobox $model1 $levtype1 $nlev
   #puts "2 $nlev=$nlev1"

   frame $win
   pack $win -side top -fill both -expand yes

   set w1 $win.frame1
   set w2 $win.frame2

   frame $w1
   pack $w1 -side top -fill both -expand yes

   label $w1.levtypelabel -text "Select level type: "
   combobox::combobox $w1.levtypeentry -textvariable levtype_var \
                      -listvar levtype_selectlist \
                      -command "combobox_setindex $levtype levtype_selectlist" \
                      -borderwidth 2 -elementborderwidth 2 -width 14 \
                      -highlightthickness 1 -editable false

   pack $w1.levtypelabel -side left -fill both -expand no
   pack $w1.levtypeentry -side left -fill both -expand no

   label $w1.nlevlabel -text " Number of levels: "
   entry $w1.nleventry -width 5 -textvariable $nlev

   pack $w1.nlevlabel -side left -fill none -expand no
   pack $w1.nleventry -pady 1 -side left -fill y -expand no -anchor e

   spacer $win.spacer

   if {$levtype1 == 2} {
      $w1.nleventry configure -state normal
   } else {
      $w1.nleventry configure -state readonly
   }

   set_var2 $w2 "Select model level type: " $theta "Theta     " 1 "Rho       " 0
   set_win_state $w2 "$config(version) >= 5.0 && \
                      ($levtype1 == 3 || $levtype1 == 4)"
}

proc set_levcombobox {model levtype nlevvar} {
   global gridconfig levtype_selectlist levtype_var

   upvar #0 $nlevvar nlev
   if {$levtype == 0} {
      set nlev 1
   } elseif {$levtype == 1} {
      set nlev $gridconfig(nsoillev)
   } elseif {$levtype == 3} {
      set nlev $gridconfig(nlev)
   } elseif {$levtype == 4} {
      set nlev $gridconfig(nolev)
   } elseif {$levtype == 5} {
      set nlev ""
   }

   if {$model == 2} {
      set levtype_selectlist "{Single level} {Depth levels}"
      if {$levtype == 5} {
         set levtype_var {Depth levels}
      } else {
         set levtype_var {Single level}
      }
   } else {
      set levtype_selectlist "{Single level} {Soil levels} {Pseudo levels} \
                              {Model levels} {Ozone levels}"
      set levtype_var [lindex $levtype_selectlist $levtype]
   }
}

proc combobox_setindex {var listvar win val} {

   upvar #0 $listvar listvar1
   upvar #0 $var var1
   set var1 [lsearch -exact $listvar1 $val]
}

proc howto_calc_lsm {win var lors lsmask fracmask} {

   upvar #0 $var var1
   if {! [info exists var1]} {set var1 1}

   frame $win
   pack $win -side top -fill both -expand yes

   if {$lors != ""} {
      set lors1 "$lors "
   } else {
      set lors1 ""
   }

   radiobutton $win.mask0 -variable $var -value -1 \
      -text "Don't calculate ${lors1}mask                            "
   radiobutton $win.mask1 -variable $var -value 0 \
      -text "Use missing data value to calculate ${lors1}mask        "
   radiobutton $win.mask2 -variable $var -value 1 \
      -text "Use land-sea mask NetCDF file to calculate ${lors1}mask "
   radiobutton $win.mask3 -variable $var -value 2 \
      -text "Use land fraction NetCDF file to calculate ${lors1}mask "

   pack $win.mask0 $win.mask1 $win.mask2 $win.mask3 \
        -side top -fill none -expand yes -anchor w

   if {[expr $lsmask]} {
      $win.mask2 configure -state normal
   } else {
      $win.mask2 configure -state disabled
      if {$var1 == 1} {set var1 0}
   }
   if {[expr $fracmask]} {
      $win.mask3 configure -state normal
   } else {
      $win.mask3 configure -state disabled
      if {$var1 == 2} {set var1 0}
   }
}

proc howto_calc_lsm1 {win var lors lsmask fracmask} {

   upvar #0 $var var1
   if {! [info exists var1]} {set var1 1}

   frame $win
   pack $win -side top -fill both -expand yes

   if {$lors != ""} {
      set lors1 "$lors "
   } else {
      set lors1 ""
   }

   radiobutton $win.mask1 -variable $var -value 0 \
      -text "Use missing data value to calculate ${lors1}mask        "
   radiobutton $win.mask2 -variable $var -value 1 \
      -text "Use land-sea mask NetCDF file to calculate ${lors1}mask "
   radiobutton $win.mask3 -variable $var -value 2 \
      -text "Use land fraction NetCDF file to calculate ${lors1}mask "

   pack $win.mask1 $win.mask2 $win.mask3 \
        -side top -fill none -expand yes -anchor w

   if {[expr $lsmask]} {
      $win.mask2 configure -state normal
   } else {
      $win.mask2 configure -state disabled
      if {$var1 == 1} {set var1 0}
   }
   if {[expr $fracmask]} {
      $win.mask3 configure -state normal
   } else {
      $win.mask3 configure -state disabled
      if {$var1 == 2} {set var1 0}
   }
}

proc select_window {win1 win2 label1 label2 var {val0 0} {val1 1}} {

   frame $win1
   pack $win1 -side top -fill both -expand yes

   radiobutton $win1.sel1 -variable $var -value $val0 \
      -text $label1 -command "pack forget $win2"
   radiobutton $win1.sel2 -variable $var -value $val1 \
      -text $label2 -command "pack $win2 -expand yes -fill x -side top"

   pack $win1.sel1 $win1.sel2 -side top -fill none -expand yes -anchor w
}

proc select_window3 {win1 win2 win3 label1 label2 label3 var} {

   frame $win2
   pack $win2 -side top -fill both -expand yes

   radiobutton $win2.sel1 -variable $var -value 1 \
      -text $label1 -command "pack forget $win3 ; \
                              pack $win1 -expand yes -fill x -side top"
   radiobutton $win2.sel2 -variable $var -value 0 \
      -text $label2 -command "pack forget $win1 ; pack forget $win3"
   radiobutton $win2.sel3 -variable $var -value 2 \
      -text $label3 -command "pack forget $win1 ; \
                              pack $win3 -expand yes -fill x -side top"

   pack $win2.sel1 $win2.sel2 $win2.sel3 -side top -fill none \
                                         -expand yes -anchor w
}

proc select_window_dump {win1 win2 label1 label2 label3 var} {

   frame $win1
   pack $win1 -side top -fill both -expand yes

   radiobutton $win1.sel1 -variable $var -value 0 \
      -text $label1 -command "pack forget $win2"
   radiobutton $win1.sel2 -variable $var -value 3 \
      -text $label2 -command "pack forget $win2"
   radiobutton $win1.sel3 -variable $var -value 1 \
      -text $label3 -command "pack $win2 -expand yes -fill x -side top"

   pack $win1.sel1 $win1.sel2 $win1.sel3 -side top -fill none -expand yes -anchor w
}

proc select_window_ice {win1 win2 label1 label2 label3 var use3} {

   frame $win1
   pack $win1 -side top -fill both -expand yes

   radiobutton $win1.sel1 -variable $var -value 0 \
      -text $label1 -command "pack forget $win2"
   radiobutton $win1.sel2 -variable $var -value 1 \
      -text $label2 -command "pack $win2 -expand yes -fill x -side top"
   radiobutton $win1.sel3 -variable $var -value 2 \
      -text $label3 -command "pack forget $win2"

   pack $win1.sel1 $win1.sel2 $win1.sel3 \
        -side top -fill none -expand yes -anchor w

   upvar #0 $var var1
   if {$use3} {
      set var1 2
      set_win_state "$win1.sel1 $win1.sel2" false
      set_win_state $win1.sel3 true
   } else {
      if {$var1 == 2} {set var1 0}
      set_win_state "$win1.sel1 $win1.sel2" true
      set_win_state $win1.sel3 false
   }
}

proc select_field_list {win label} {

   global select_field_list_boxheight

   set select_field_list_boxheight 10

   frame $win
   pack $win -side top -fill both -expand yes

   label $win.label -text $label -justify center
   frame $win.box -bd 2
   listbox $win.box.list \
           -width 20 -height $select_field_list_boxheight \
           -yscrollcommand "$win.box.scroll set" \
           -selectmode browse -exportselection no -relief sunken
   scrollbar $win.box.scroll -command "$win.box.list yview" -width 0 -bd 0

   pack $win.label -side top -fill both -expand yes -anchor w
   pack $win.box -side top -fill both -expand yes -anchor w
   pack $win.box.list -side left -fill both -expand yes -anchor w
   pack $win.box.scroll -side right -fill y
}

proc set_date {win name var {el1 ""}} {

   global gridconfig $var
   
   set w1 $win.frame1
   set w2 $win.frame2

   if {$el1 != ""} {
      set el $el1,
   } else {
      set el ""
   }

   frame $win
   pack $win -side top -fill both -expand yes
   
   if {$var != "gridconfig" } {
      frame $w1
      pack $w1 -side top -fill both -expand yes

      checkbutton $w1.date23 -variable ${var}(${el}useglobtime) \
          -text "Use dates from Grid Configuration panel" \
          -command "copy_dates $w2 \$${var}(${el}useglobtime) gridconfig $var \"\" $el1"

      pack $w1.date23 -side top -fill none -expand yes -anchor w
      
      spacer $w1.datespacer1
   }

   frame $w2
   pack $w2 -side top -fill both -expand yes

   radiobutton $w2.date21 -variable ${var}(${el}timeusage2) -value 0 \
      -text "Overwrite NetCDF dates with specified dates          "
   radiobutton $w2.date22 -variable ${var}(${el}timeusage2) -value 1 \
      -text "Extract only specified dates from NetCDF file        "
   pack $w2.date21 $w2.date22 -side top -fill none -expand yes -anchor w
   spacer $w2.datespacer0
   label $w2.date3 -text [join [list "Enter Start date of" $name "data: "]]
   frame $w2.date4
   label $w2.date4.year1 -text "Year   "
   entry $w2.date4.year2 -width 10 -textvariable ${var}(${el}startyear)
   label $w2.date4.mon1 -text " Month  "
   entry $w2.date4.mon2 -width 10 -textvariable ${var}(${el}startmon)
   label $w2.date4.day1 -text " Day    "
   entry $w2.date4.day2 -width 10 -textvariable ${var}(${el}startday)
   frame $w2.date5
   label $w2.date5.hour1 -text "Hour   "
   entry $w2.date5.hour2 -width 10 -textvariable ${var}(${el}starthour)
   label $w2.date5.min1 -text " Minute "
   entry $w2.date5.min2 -width 10 -textvariable ${var}(${el}startmin)
   label $w2.date5.sec1 -text " Second "
   entry $w2.date5.sec2 -width 10 -textvariable ${var}(${el}startsec)

   pack $w2.date3 -side top -fill none -expand yes -anchor w
   pack $w2.date4 -side top -fill both -expand yes
   pack $w2.date4.year1 $w2.date4.year2 $w2.date4.mon1 $w2.date4.mon2 \
        $w2.date4.day1 $w2.date4.day2 -side left -fill none -expand no
   pack $w2.date5 -side top -fill both -expand yes
   pack $w2.date5.hour1 $w2.date5.hour2 $w2.date5.min1 $w2.date5.min2 \
        $w2.date5.sec1 $w2.date5.sec2 -side left -fill none -expand no

   spacer $w2.datespacer2

   frame $w2.ntime
   label $w2.ntime.label -text "Enter number of times needed in ancillary file: "
   entry $w2.ntime.entry -width 9 -textvariable ${var}(${el}ntimes)

   pack $w2.ntime -side top -fill both -expand yes
   pack $w2.ntime.label -side left -fill none -expand no
   pack $w2.ntime.entry -side left -fill both -expand no -anchor e

   spacer $w2.datespacer3

   frame $w2.int1
   label $w2.int1.label -text "Enter value of time interval: "
   entry $w2.int1.entry -width 11 -textvariable ${var}(${el}interval)
   frame $w2.int2
   label $w2.int2.label -text "Time interval unit:           "
   radiobutton $w2.int2.but1 -text "Years   " -variable ${var}(${el}intunit) -value 0 \
       -command "set_win_state $w2.mm false"
   radiobutton $w2.int2.but2 -text "Months  " -variable ${var}(${el}intunit) -value 1 \
       -command "set_win_state $w2.mm true"
   radiobutton $w2.int2.but3 -text "Days    " -variable ${var}(${el}intunit) -value 2 \
       -command "set_win_state $w2.mm false"
   frame $w2.int3
   label $w2.int3.label -text "                              "
   radiobutton $w2.int3.but4 -text "Hours   " -variable ${var}(${el}intunit) -value 3 \
       -command "set_win_state $w2.mm false"
   radiobutton $w2.int3.but5 -text "Minutes " -variable ${var}(${el}intunit) -value 4 \
       -command "set_win_state $w2.mm false"
   radiobutton $w2.int3.but6 -text "Seconds " -variable ${var}(${el}intunit) -value 5 \
       -command "set_win_state $w2.mm false"

   pack $w2.int1 -side top -fill both -expand yes
   pack $w2.int1.label -side left -fill none -expand no
   pack $w2.int1.entry -side left -fill both -expand no -anchor e
   pack $w2.int2 -side top -fill both -expand yes
   pack $w2.int2.label -side left -fill none -expand no
   pack $w2.int2.but1 $w2.int2.but2 $w2.int2.but3 \
        -side left -fill x -expand no -anchor e
   pack $w2.int3 -side top -fill both -expand yes
   pack $w2.int3.label -side left -fill none -expand no
   pack $w2.int3.but4 $w2.int3.but5 $w2.int3.but6 \
        -side left -fill x -expand no -anchor e

   spacer $w2.datespacer4

   frame $w2.mm
   label $w2.mm.label -text "Define monthly mean to be middle of the month? "
   radiobutton $w2.mm.button1 -text "yes " -variable ${var}(${el}mm) -value 1
   radiobutton $w2.mm.button2 -text "no  " -variable ${var}(${el}mm) -value 0
   
   pack $w2.mm -side top -fill both -expand yes
   pack $w2.mm.label -side left -fill none -expand yes -anchor w
   pack $w2.mm.button1 $w2.mm.button2 -side left -fill none -expand no

   eval set var1 $${var}(${el}intunit)
   if {$var1 == 1} {
      set_win_state $w2.mm true
   } else {
      set_win_state $w2.mm false
   }

   if {$var != "gridconfig" } {
      if {$gridconfig(useglobtime)} {
         eval set var1 $${var}(${el}useglobtime)
         copy_dates $w2 $var1 gridconfig $var "" $el1
      } else {
         #set ${var}(${el}useglobtime) 0
         set_win_state $w1 false
      }
   }
}

proc set_dumpdate {win name var} {

   global gridconfig $var
   
   set w1 $win.frame1
   set w2 $win.frame2

   frame $win
   pack $win -side top -fill both -expand yes
   
   frame $w2
   pack $w2 -side top -fill both -expand yes

   radiobutton $w2.date21 -variable ${var}(timeusage2) -value 0 \
      -text "Overwrite UM/NetCDF date with specified date         "
   radiobutton $w2.date22 -variable ${var}(timeusage2) -value 1 \
      -text "Extract only specified date from NetCDF file         "
   pack $w2.date21 $w2.date22 -side top -fill none -expand yes -anchor w
   spacer $w2.datespacer0
   label $w2.date3 -text [join [list "Enter date of" $name "data: "]]
   frame $w2.date4
   label $w2.date4.year1 -text "Year   "
   entry $w2.date4.year2 -width 10 -textvariable ${var}(year)
   label $w2.date4.mon1 -text " Month  "
   entry $w2.date4.mon2 -width 10 -textvariable ${var}(mon)
   label $w2.date4.day1 -text " Day    "
   entry $w2.date4.day2 -width 10 -textvariable ${var}(day)
   frame $w2.date5
   label $w2.date5.hour1 -text "Hour   "
   entry $w2.date5.hour2 -width 10 -textvariable ${var}(hour)
   label $w2.date5.min1 -text " Minute "
   entry $w2.date5.min2 -width 10 -textvariable ${var}(min)
   label $w2.date5.sec1 -text " Second "
   entry $w2.date5.sec2 -width 10 -textvariable ${var}(sec)

   pack $w2.date3 -side top -fill none -expand yes -anchor w
   pack $w2.date4 -side top -fill both -expand yes
   pack $w2.date4.year1 $w2.date4.year2 $w2.date4.mon1 $w2.date4.mon2 \
        $w2.date4.day1 $w2.date4.day2 -side left -fill none -expand no
   pack $w2.date5 -side top -fill both -expand yes
   pack $w2.date5.hour1 $w2.date5.hour2 $w2.date5.min1 $w2.date5.min2 \
        $w2.date5.sec1 $w2.date5.sec2 -side left -fill none -expand no
}

proc use_create {use create win1 win2} {

   if {$create} {
      pack $win1 -expand yes -fill x -side top
      pack $win2 -expand yes -fill x -side bottom
   } elseif {$use && ! $create} {
      pack $win1 -expand yes -fill x -side top
      pack forget $win2
   } elseif {! $use && ! $create} {
      pack forget $win1
      pack forget $win2
   }
}

proc set_win_state {win enable} {

   #puts "win = $win"
   #puts "enable = $enable"

   if {[expr $enable]} {
      set state normal
   } else {
      set state disabled
   }
   
   set list $win
   foreach w $win {
      set list [concat $list [winfo children $w]]
   }

   while {$list != ""} {
      set next {}
      foreach w $list {
         set next [concat $next [winfo children $w]]
	 set class [winfo class $w]
         if {$class == "Entry" || $class == "Label" || $class == "Button" || \
	     $class == "Radiobutton" || $class == "Checkbutton" || \
	     $class == "Menubutton" || $class == "Listbox" || \
	     $class == "Spinbox" || $class == "Scale"} {
            $w configure -state $state
         }
      }
      set list $next
   }
}

proc copy_dates {win copy var1 var2 {el1 ""} {el2 ""}} {

  global $var1 $var2
   
  if {$el1 != ""} {
     set el1 $el1,
  }

  if {$el2 != ""} {
     set el2 $el2,
  }

  if {! $copy} {
      set_win_state $win true
      if "$${var2}(${el2}intunit) != 1" {
         set_win_state $win.mm false
      }
   } else {
      eval set use $${var1}(${el1}useglobtime)
      if {$use} {
         eval set ${var2}(${el2}startyear) $${var1}(${el1}startyear)
         eval set ${var2}(${el2}startmon) $${var1}(${el1}startmon)
         eval set ${var2}(${el2}startday) $${var1}(${el1}startday)
         eval set ${var2}(${el2}starthour) $${var1}(${el1}starthour)
         eval set ${var2}(${el2}startmin) $${var1}(${el1}startmin)
         eval set ${var2}(${el2}startsec) $${var1}(${el1}startsec)
         eval set ${var2}(${el2}ntimes) $${var1}(${el1}ntimes)
         eval set ${var2}(${el2}interval) $${var1}(${el1}interval)
         eval set ${var2}(${el2}intunit) $${var1}(${el1}intunit)
         eval set ${var2}(${el2}mm) $${var1}(${el1}mm)
         set_win_state $win false
      }
   }
}

proc getfile {file1 {type {}} {com {}} {multi "no"}} {

   if {$type == "nc"} {
      set filetype {{{NetCDF Files} {.nc *.cdf *.netcdf}} {{All Files} *}}
   } else {
      set filetype {{{All Files} *}}
   }
   
   set file3 [tk_getOpenFile -filetypes $filetype -title {Select file} -multiple $multi]

   if {$file3 != ""} {
      upvar #0 $file1 file2
      set file2 $file3
   }

   if {[lindex $com 0] != "" && $file3 != ""} {
      eval $com $file3
   }
}

proc setfile {file1 {type {}}} {

   if {$type == "nc"} {
      set filetype {{{NetCDF Files} {.nc *.cdf *.netcdf}} {{All Files} *}}
   } else {
      set filetype {{{All Files} *}}
   }
   
   set file3 [tk_getSaveFile -filetypes $filetype -title {Select file}]

   if {$file3 != ""} {
      upvar #0 $file1 file2
      set file2 $file3
   }
}

proc set_field_val {var win filenum fieldnum} {

   global $var mask lfrac w4 setlev stashrange config

   #puts "=== set_field_val $var $win $filenum $fieldnum"

   if {$filenum == 0} return
   if {$fieldnum == 0} return

   upvar #0 $var ancvar

   if {$filenum == ""} {
      set el1 ""
      set el2 $fieldnum
   } else {
      set el1 "$filenum,"
      set el2 "$filenum,$fieldnum"
   }

#  Scrolled frame keeps jumping when clicking on field listbox,
#  remove Configure bind to stop it. Reinstate bind at end of the proc

   set savebind [bind $w4.frame.scrolled <Configure>]
   bind $w4.frame.scrolled <Configure> ""

   set ww $win.frame
   destroy $ww
   frame $ww
   pack $ww -expand yes -fill x -side top

   set fieldnum_prev [expr $fieldnum - 1]
   if {$fieldnum_prev < 1} {set fieldnum_prev 1}
   get_filename3 $ww.ncfile "NetCDF file:   " \
                 ${var}(${el1}$fieldnum_prev,file_in) \
                 ${var}($el2,file_in) "nc" \
                 add_ncfile
   if {! [info exists stashrange($var)] || [lindex $stashrange($var) 0] <= -1} {
      set_2var $ww.code "STASH code:    " " PP code: " \
               ${var}($el2,stashcode) ${var}($el2,ppcode) 7 7
   } else {
      set_2var1 $ww.code "STASH code:    " " PP code: " \
                ${var}($el2,stashcode) ${var}($el2,ppcode) $stashrange($var) 5 7
   }
   button $ww.code.update -padx 2 -pady 1 -text "Update" \
      -command [list update_field_val $var $win $filenum $fieldnum yes yes yes]
   pack $ww.code.update -ipady 1 -padx 4 -side right -fill both -expand no
   select_varname1 $ww.varname "Variable name: " \
                   ${var}($el2,varname) ${var}($el2,file_in) false 5 \
                   ${var}_stdname($el2)
   spacer $ww.spacer2

   if {$setlev($var) > 1} {
      set_levtype $ww.levtype ${var}($el2,levtype) ${var}($el2,nlev) \
                              ${var}($el2,theta) ${var}(${el1}model)
      spacer $ww.spacer3
   } elseif {$setlev($var) == 1} {
      set_var2 $ww.levtype "Select model level type: " ${var}($el2,theta) \
                           "Theta     " 1 "Rho       " 0
      set_win_state $ww.levtype "$config(version) >= 5.0"
      spacer $ww.spacer3
   }
   
   if {$config(version) < 5.0 || $ancvar(${el1}model) == 2 || 
                                 $ancvar(${el1}model) == 3} {
      set_var2 $ww.gridtype "Select horizontal grid type: " ${var}($el2,gridtype) \
               "Theta     " 1 "UV        " 2
   } else {
      set_var3 $ww.gridtype "Select horizontal grid type: " ${var}($el2,gridtype) \
               "Theta     " 1 "U         " 3  "V         " 4
   }
   spacer $ww.spacer4

   set_var3 $ww.datatype "Select data type: " ${var}($el2,datatype) \
            "Real      " 1 "Integer   " 2  "Logical   " 3
   spacer $ww.spacer5

   set_var3 $ww.mask1 "Select mask type: " ${var}($el2,masktype) \
   	    "No mask   " 0 "Land mask " 1  "Sea mask  " 2 \
   	    "set_win_state $ww.mask2.mask1 \
   	       \"\$${var}($el2,masktype) != 0\" ; \
   	     set_win_state $ww.mask2.mask2 \
   	       \"\$${var}($el2,masktype) != 0 && \
   		 (\$mask(use) || \$mask(create)) && \
   		 (\$${var}(${el1}model) == 1 || \$${var}(${el1}model) == 3)\" ; \
   	     set_win_state $ww.mask2.mask3 \
   	       \"\$${var}($el2,masktype) != 0 && \
   		 (\$lfrac(use) || \$lfrac(create)) && \
   		 (\$${var}(${el1}model) == 1 || \$${var}(${el1}model) == 3)\""
   spacer $ww.spacer6

   howto_calc_lsm1 $ww.mask2 ${var}($el2,mask) "" \
      "($mask(use) || $mask(create)) && ($ancvar(${el1}model) == 1 || $ancvar(${el1}model) == 3)" \
      "($lfrac(use) || $lfrac(create)) && ($ancvar(${el1}model) == 1 || $ancvar(${el1}model) == 3)"

   if {$ancvar(${el1}model) == 2} {
      if {$ancvar($el2,masktype) == 2} {
         set ancvar($el2,masktype) 1
      }
      set_win_state $ww.mask1.but3 off
   }
   if {$ancvar($el2,masktype) == 0} {
      set_win_state $ww.mask2 off
   }

   if {$setlev($var) > 1} {
      trace add variable ancvar($el2,levtype) \
            write "update_nlev ${var} $filenum $fieldnum $ww.levtype"
   }

   bind [$ww.ncfile.fileEntry subwidget entry] <Destroy> \
        "+ update_ncfilelist ${var}($el2,file_in)"
   bind [$ww.ncfile.fileEntry subwidget entry] <Unmap> \
        "+ update_ncfilelist ${var}($el2,file_in)"
   bind [$ww.ncfile.fileEntry subwidget listbox] <Map> \
        "+ update_ncfilelist ${var}($el2,file_in)"

   bind $ww.code.entry1 <Return> \
        [list update_field_val $var $win $filenum $fieldnum yes no no]
   if {[info exists stashrange($var)] && [lindex $stashrange($var) 0] > -1} {
      bind [$ww.code.entry1 subwidget listbox] <Unmap> \
           [list update_field_val $var $win $filenum $fieldnum yes no no]
   } else {
      bind $ww.code.entry2 <Return> \
           [list update_field_val $var $win $filenum $fieldnum no yes no]
   }
   bind ncvarselectwin <Destroy> \
        [list update_field_val $var $win $filenum $fieldnum no no yes]

   update idletasks
   bind $w4.frame.scrolled <Configure> $savebind
}

proc update_field_val {var win filenum fieldnum {usestashcode yes} \
                      {useppcode no} {usevarname no}} {

   global config $var ${var}_fieldname ${var}_stashcode ${var}_stdname

   #puts "=== update_field_val $var $win $filenum $fieldnum \
   #                           $usestashcode $useppcode $usevarname"
   upvar #0 $var ancvar
   upvar #0 ${var}_stashcode ancvar_stashcode
   upvar #0 ${var}_stdname ancvar_stdname
   
   if {$filenum == ""} {
      set el1 ""
      set el2 $fieldnum
   } else {
      set el1 "$filenum,"
      set el2 "$filenum,$fieldnum"
   }
   
   set ncfile $ancvar($el2,file_in)
   set model $ancvar(${el1}model)
   set version $config(version)
   set iversion [expr {int(int($version)*100 + ($version-int($version))*10)}]

   if {! [isint $model]} {return}
   if {! [isint $iversion]} {return}

   set stashcode $ancvar($el2,stashcode)
   set ppcode $ancvar($el2,ppcode)
   
   if {$usevarname} {
      set usestashcode yes
      set useppcode yes
   }

   if {$usestashcode && $useppcode} {
      if {! [isint $stashcode]} {set usestashcode no}
      if {! [isint $ppcode]} {set useppcode no}
      if {! $usestashcode && ! $useppcode && ! $usevarname} {return}
      if {$usestashcode && $useppcode} {set useppcode no}
   }
   if {$usestashcode || $useppcode} {set usevarname no}
   if {[isint $stashcode] && [isint $ppcode] && $usevarname} {return}
   
   if {$usevarname && [info exists ancvar_stdname($el2)] && \
       $ancvar_stdname($el2) != ""} {
       set stashcode [getstashcode_fromstdname $model $ancvar_stdname($el2)]
       if {[isint $stashcode]} {
          set usestashcode yes
          set ancvar($el2,stashcode) $stashcode
       }
   }

   if {$usestashcode} {
      if {! [isint $stashcode]} {return}
      set ppcode [lindex [getppcode $model $iversion $stashcode] 0]
      set ancvar($el2,ppcode) $ppcode
   } elseif {$useppcode} {
      if {! [isint $ppcode]} {return}
      set stashcode [getstashcode_fromppcode $model $iversion $ppcode]
      if {! [isint $stashcode]} {return}
      set ancvar($el2,stashcode) $stashcode
   } elseif {$usevarname} {
      set varname $ancvar($el2,varname)
      if {$varname == ""} {return}
      regsub {(.*)(_[0-9]*$)} $varname {\1} shortname
      set ppcode [lindex [getppcode_fromshortname $shortname] 0]
      set ancvar($el2,ppcode) $ppcode
      if {! [isint $ppcode] || [isint $stashcode]} {return}
      set stashcode [getstashcode_fromppcode $model $iversion $ppcode]
      if {! [isint $stashcode]} {return}
      set ancvar($el2,stashcode) $stashcode
   }

   set stashname [lindex [getstashname $model $iversion $stashcode] 0]
   set gridtype [lindex [getgridtype $model $iversion $stashcode] 0]
   set datatype [lindex [getdatatype $model $iversion $stashcode] 0]
   set masktype [lindex [getmasktype $model $iversion $stashcode] 0]
   set levtype [lindex [getlevtype $model $iversion $stashcode] 0]
   #puts "$model $iversion $stashcode $gridtype $datatype $masktype $levtype $stashname"

   set ancvar($el2,gridtype) $gridtype
   set ancvar($el2,datatype) $datatype
   set ancvar($el2,masktype) $masktype
   if {$masktype == 2} {
      $win.frame.mask1.but3 invoke
   } elseif {$masktype == 1} {
      $win.frame.mask1.but2 invoke
   } else {
      $win.frame.mask1.but1 invoke
   }

   if {$iversion >= 500} {
      if {$levtype == 4 || $levtype == 6} {
         set ancvar($el2,theta) 1
      } elseif {$levtype == 3} {
         set ancvar($el2,theta) 0
      }
   }
   if {$levtype == 6} {set levtype 3}
   if {$levtype == -1} {set levtype 0}
   set ancvar($el2,levtype) $levtype
   set_levcombobox $model $levtype ${var}($el2,nlev)

   set ${var}_fieldname($el2) $stashname
   set index [expr {$fieldnum-1}]
   $win.fieldlist.box.list delete $index
   $win.fieldlist.box.list insert $index $stashname
   $win.fieldlist.box.list selection set $index

   if {! $usevarname} {
      if {[info exists ancvar_stashcode($el2)] && \
          $ancvar_stashcode($el2) == $stashcode} {
         set varname $ancvar($el2,varname)
      } else {
         set varname ""
      }
      set varname1 \
          [lindex [getvarname $ncfile $model $iversion $stashcode $varname] 0]
      if {$varname1 != ""} {set ancvar($el2,varname) $varname1}
   }

   set ${var}_stashcode($el2) $stashcode
}

proc update_all_field_val {var win filenum} {

   #puts "=== update_all_field_val $var $win $filenum"
   upvar #0 $var ancvar
   set cursel [$win.fieldlist.box.list curselection]
   
   if {$filenum == ""} {
      set nfield $ancvar(nfield)
   } else {
      set nfield $ancvar($filenum,nfield)
   }

   set ifield 1
   while {$ifield<=$nfield} {
      update_field_val $var $win $filenum $ifield yes yes yes
      incr ifield
   }
   $win.fieldlist.box.list selection clear 0 end
   $win.fieldlist.box.list selection set $cursel
   update_field_val $var $win $filenum [expr $cursel + 1] yes yes yes
}

proc update_fieldlist {var filenum {args {}}} {

   global config $var defanc00 ${var}_fieldname ${var}_xold ww2
   global scrollbarwidth select_field_list_boxheight

   puts "=== update_fieldlist var=$var filenum=$filenum args=$args"

   if {! [winfo exists $ww2]} {return}
   set win $ww2
   if {$filenum == ""} {
      set el1 ""
   } else {
      set el1 "$filenum,"
   }

   upvar #0 $var ancvar
   upvar #0 ${var}(${el1}nfield) x
   upvar #0 ${var}_xold xold
   #puts "${var}(${el1}nfield) = $x $xold"

   if {$x != $xold && [isint $x] && $x >= 0} {
      if {$x > $xold} {
         for {set xx [expr {$xold+1}]} {$xx <= $x} {incr xx} {
            if {! [info exists ${var}_fieldname(${el1}$xx)]} {
               if {[info exists ancvar(${el1}$xx,stashcode)] &&
                   [isint $ancvar(${el1}$xx,stashcode)] &&
                   $ancvar(${el1}$xx,stashcode) > 0} {
                  set stashcode $ancvar(${el1}$xx,stashcode)
                  set model $ancvar(${el1}model)
                  set version $config(version)
                  set iversion [expr {int(int($version)*100 + \
                                      ($version-int($version))*10)}]
                  set ${var}_fieldname(${el1}$xx) \
                      [lindex [getstashname $model $iversion $stashcode] 0]
               } else {
                  set ${var}_fieldname(${el1}$xx) "Ancillary field $xx"
               }
            }
            eval $win.fieldlist.box.list insert end $${var}_fieldname(${el1}$xx)
            foreach el2 [array names defanc00] {
               if {! [info exists ancvar(${el1}$xx,$el2)]} {
                  set ancvar(${el1}$xx,$el2) $defanc00($el2)
               }
            }
         }
      } else {
         $win.fieldlist.box.list delete $x end
         set index [$win.fieldlist.box.list curselection]
         set size [$win.fieldlist.box.list size]
         if {$index == "" && $size > 0} {
            $win.fieldlist.box.list selection set end
            set_field_val $var $win $filenum $x
         }
         for {set xx $xold} {$xx > $x} {incr xx -1} {
            foreach el2 [array names defanc00] {
               if {[info exists ancvar(${el1}$xx,$el2)]} {
                  unset ancvar(${el1}$xx,$el2)
               }
            }
            if {[info exists ${var}_fieldname(${el1}$xx)]} {
               unset ${var}_fieldname(${el1}$xx)
            }
         }
      }
      if {$x > $select_field_list_boxheight && \
          $xold <= $select_field_list_boxheight} {
          $win.fieldlist.box.scroll configure -width $scrollbarwidth -bd 2
      } elseif {$x <= $select_field_list_boxheight && \
                $xold > $select_field_list_boxheight} {
         $win.fieldlist.box.scroll configure -width 0 -bd 0
      }
      set xold $x
   }
}

proc update_ncfilelist {file} {

   global config

   upvar #0 $file ncfile

   if {$ncfile == ""} {return}

   if {[lsearch -exact $config(ncfile) $ncfile] == -1} {
      lappend config(ncfile) $ncfile
   }
}

proc update_nlev {var filenum fieldnum win args} {

   global $var config gridconfig
   
   #puts "=== update_nlev $var $filenum $fieldnum $win $args"

   upvar #0 $var ancvar

   if {$filenum == ""} {
      set el2 $fieldnum
   } else {
      set el2 "$filenum,$fieldnum"
   }

   set levtype $ancvar($el2,levtype)

   if {$levtype == 0} {
      set ancvar($el2,nlev) 1
   } elseif {$levtype == 1} {
      set ancvar($el2,nlev) $gridconfig(nsoillev)
   } elseif {$levtype == 3} {
      set ancvar($el2,nlev) $gridconfig(nlev)
   } elseif {$levtype == 4} {
      set ancvar($el2,nlev) $gridconfig(nolev)
   }

   if {[winfo exists $win]} {
      if {$levtype == 2} {
         $win.frame1.nleventry configure -state normal
      } else {
         $win.frame1.nleventry configure -state readonly
      }

      set_win_state $win.frame2 "$config(version) >= 5.0 && \
                                 ($levtype == 3 || $levtype == 4)"
   }
}

proc getfilebox {win label var {type {}} {com {}}} {

   global [getarrayname $var]
   global scrollbarwidth getfiles_boxheight

   upvar #0 $var filelist

   set getfiles_boxheight 15

   frame $win
   pack $win -side top -fill both -expand yes

   label $win.label -text $label -justify center
   frame $win.box -bd 2
   listbox $win.box.list -listvar $var \
           -width 20 -height $getfiles_boxheight \
           -xscrollcommand "$win.box.xscroll set" \
           -yscrollcommand "$win.box.yscroll set" \
           -selectmode extended -exportselection no -relief sunken
   scrollbar $win.box.xscroll -command "$win.box.list xview" \
                              -orient horizontal -width 0 -bd 0
   scrollbar $win.box.yscroll -command "$win.box.list yview" -width 0 -bd 0

   if {$com == ""} {set add_com ""} else {set add_com "\"$com add\""}
   if {$com == ""} {set del_com ""} else {set del_com "\"$com del\""}
   frame $win.butbox -bd 2
   button $win.butbox.add -width 8 -text "Add" \
          -command "getfilebox_add $var $win.box $type $add_com"
   button $win.butbox.del -width 8 -text "Delete" \
          -command "getfilebox_del $var $win.box $del_com"

   pack $win.label -side top -fill both -expand yes -anchor w
   pack $win.box -side top -fill both -expand yes
   
   grid $win.box.list -row 0 -column 1 -sticky nsew
   grid $win.box.yscroll -row 0 -sticky ns -column 2
   grid $win.box.xscroll -row 1 -column 1 -sticky ew
   grid columnconfigure $win.box 1 -weight 1
   grid rowconfigure $win.box 0 -weight 1
   
   pack $win.butbox -side top -fill both -expand yes
   pack $win.butbox.add $win.butbox.del -side left -expand yes

   update
   set view [$win.box.list yview]
   set pos0 [lindex $view 0]
   set pos1 [lindex $view 1]
   if {$pos0 > 0 || $pos1 < 1} {
      $win.box.yscroll configure -width $scrollbarwidth -bd 2
   }
   set view [$win.box.list xview]
   set pos0 [lindex $view 0]
   set pos1 [lindex $view 1]
   if {$pos0 > 0 || $pos1 < 1} {
      $win.box.xscroll configure -width $scrollbarwidth -bd 2
   }
   
   bind $win.box <Configure> {getfilebox_resize %W}
}

proc getfilebox_resize {win} {

   global scrollbarwidth
   
   set listbox $win.list
   set xscroll $win.xscroll
   set yscroll $win.yscroll

   update
   set view [$listbox yview]
   set pos0 [lindex $view 0]
   set pos1 [lindex $view 1]
   if {$pos0 > 0 || $pos1 < 1} {
      $yscroll configure -width $scrollbarwidth -bd 2
   } elseif {$pos0 == 0 && $pos1 == 1} {
      $yscroll configure -width 0 -bd 0
   }
   set view [$listbox xview]
   set pos0 [lindex $view 0]
   set pos1 [lindex $view 1]
   if {$pos0 > 0 || $pos1 < 1} {
      $xscroll configure -width $scrollbarwidth -bd 2
   } elseif {$pos0 == 0 && $pos1 == 1} {
      $xscroll configure -width 0 -bd 0
   }
}

proc getfilebox_add {filelist win {type {}} {com {}}} {

   global filebox2 scrollbarwidth
   
   set listbox $win.list
   set xscroll $win.xscroll
   set yscroll $win.yscroll

   set filebox2 ""
   getfile filebox2 $type "" "yes"

   upvar #0 $filelist filebox1

   set filebox3 ""
   foreach file $filebox2 {
      if {[lsearch -exact $filebox1 $file] == -1} {
         lappend filebox3 $file
      }
   }
   if {[llength $filebox3] > 0} {
      set filebox1 [concat $filebox1 $filebox3]
   }
   
   update
   set view [$listbox yview]
   set pos0 [lindex $view 0]
   set pos1 [lindex $view 1]
   if {$pos0 > 0 || $pos1 < 1} {
      $yscroll configure -width $scrollbarwidth -bd 2
   }
   set view [$listbox xview]
   set pos0 [lindex $view 0]
   set pos1 [lindex $view 1]
   if {$pos0 > 0 || $pos1 < 1} {
      $xscroll configure -width $scrollbarwidth -bd 2
   }

   if {$com != ""} {eval $com [list $filebox2]}
}

proc getfilebox_del {filelist win {com {}}} {

   set listbox $win.list
   set xscroll $win.xscroll
   set yscroll $win.yscroll

   upvar #0 $filelist filebox1
   set size [llength $filebox1]

   set cursel [$listbox curselection]
   if {$cursel == ""} return

   set n 0
   set filebox3 $filebox1
   foreach sel $cursel {
      set sel1 [expr {$sel-$n}]
      incr n

      lappend filebox2 [lindex $filebox3 $sel1]
      set filebox3 [lreplace $filebox3 $sel1 $sel1]
      incr size -1
   }
   set filebox1 $filebox3
   $listbox selection clear 0 end
   
   update
   set view [$listbox yview]
   set pos0 [lindex $view 0]
   set pos1 [lindex $view 1]
   if {$pos0 == 0 && $pos1 == 1} {
      $yscroll configure -width 0 -bd 0
   }
   set view [$listbox xview]
   set pos0 [lindex $view 0]
   set pos1 [lindex $view 1]
   if {$pos0 == 0 && $pos1 == 1} {
      $xscroll configure -width 0 -bd 0
   }

   if {$com != ""} {eval $com [list $filebox2]}
}

proc add_ncfile {win ncfile} {

   global config
   
   if {$ncfile == ""} {return}
   if {! [file exists $ncfile]} {return}

   set index [lsearch -exact $config(ncfile) $ncfile]

   if {$index == -1} {
      lappend config(ncfile) $ncfile
      set index [expr {[llength $config(ncfile)]-1}]
   }

   if {$win != ""} {$win select $index}
}

proc select_ncvarname {ncfile {dim false} {var ""} {stdnamevar ""} {entry ""}} {

   global foregroundcolour scrollbarwidth

   #puts "ncfile = $ncfile"
   #puts "dim = $dim"
   #puts "var = $var"
   #puts "stdnamevar = $stdnamevar"
   #puts "entry = $entry"

   if {! [file exists $ncfile]} {
      if {$ncfile == ""} {
         set message "Error: No NetCDF file specified"
      } else {
         set message "Error: Cannot open NetCDF file $ncfile"
      }
      errorbox $message
      return
   }

   if {$dim} {
      getncdiminfo $ncfile ncinfo
   } else {
      getncvarinfo $ncfile ncinfo
   }
   #puts "NetCDF file $ncfile field info"
   #puts "ncinfo(nvar) = $ncinfo(nvar)"
   #puts "ncinfo(var_name) = $ncinfo(var_name)"
   #puts "ncinfo(long_name) = $ncinfo(long_name)"
   #puts "ncinfo(standard_name) = $ncinfo(standard_name)"

   set max1 0
   set max2 0
   set max3 0
   for {set i 0} {$i < $ncinfo(nvar)} {incr i} {
      set len1 [string length [lindex $ncinfo(var_name) $i]]
      set len2 [string length [lindex $ncinfo(long_name) $i]]
      set len3 [string length [lindex $ncinfo(standard_name) $i]]

      if {$len1 > $max1} {set max1 $len1}
      if {$len2 > $max2} {set max2 $len2}
      if {$len3 > $max3} {set max3 $len3}
   }
   if {$max2 == 0 && $max3 == 0} {
      set width1 [expr {$max1 + 1}]
   } elseif {$max2 == 0} {
      set width1 [expr {$max1 + $max3 + 4}]
   } elseif {$max3 == 0} {
      set width1 [expr {$max1 + $max2 + 4}]
   } else {
      set width1 [expr {$max1 + $max2 + $max3 + 7}]
   }

   set minht 5
   set maxht 40
   set maxwidth 100
   if {$ncinfo(nvar) < $minht} {
      set ht $minht
   } elseif {$ncinfo(nvar) > $maxht} {
      set ht $maxht
   } else {
      set ht $ncinfo(nvar)
   }
   if {$width1 > $maxwidth} {
      set width $maxwidth
   } else {
      set width $width1
   }

   set w .ncvarselect
   catch {destroy $w}
   toplevel $w
   wm title $w "NetCDF field info"
   bindtags $w "[bindtags $w] ncvarselectwin"

   set ww $w.frame
   frame $ww -relief flat -borderwidth 8
   pack $ww -side left -fill both -expand yes

   label $ww.label -text "NetCDF file $ncfile field info"
   pack $ww.label -side top -fill both -expand yes -anchor w
   spacer $ww.spacer1

   frame $ww.box1
   pack $ww.box1 -side top -fill both -expand yes -anchor w

   listbox $ww.box1.list -width $width -height $ht \
           -selectmode browse -exportselection no -relief sunken
   pack $ww.box1.list -side left -fill both -expand yes -anchor w

   if {$ncinfo(nvar) > $maxht} {
      $ww.box1.list configure -yscrollcommand "$ww.box1.yscroll set"
      scrollbar $ww.box1.yscroll -command "$ww.box1.list yview" \
                -width $scrollbarwidth
      pack $ww.box1.yscroll -side right -fill y
   }
   if {$width1 > $maxwidth} {
      frame $ww.box2
      $ww.box1.list configure -xscrollcommand "$ww.box2.xscroll set"
      scrollbar $ww.box2.xscroll -command "$ww.box1.list xview" \
                -orient horizontal -width $scrollbarwidth
      pack $ww.box2 -side top -fill x
      pack $ww.box2.xscroll -side left -fill x -expand yes
      if {$ncinfo(nvar) > $maxht} {
         set corner_width [expr {[$ww.box1.yscroll cget -width] + \
                               2*[$ww.box1.yscroll cget -borderwidth] + \
                               2*[$ww.box1.yscroll cget -highlightthickness]}]
         frame $ww.box2.corner -width $corner_width
         pack $ww.box2.corner -side right -fill both
      }
   }

   spacer $ww.spacer2
   frame $ww.but -relief groove -borderwidth 4
   pack $ww.but -fill x -expand yes

   if {$var == ""} {
      button $ww.but.ok -width 4 -text "  OK  " -command "destroy $w"
      pack $ww.but.ok -side left -expand yes
   } else {
      upvar #0 $var name
      set name0 $name
 
      button $ww.but.ok -width 4 -text "  OK  " -command "destroy $w"
      button $ww.but.cancel -width 4 -text Cancel -command " \
         unset -nocomplain newvar newstdname ; \
         destroy $w \
      "
      pack $ww.but.ok $ww.but.cancel -side left -expand yes

      bind $ww.box1.list <Button-1> "
         set newvar \[lindex \"$ncinfo(var_name)\" \[%W index @%x,%y\]\]
      "
      if {$entry != ""} {
          bind $ww.box1.list <Double-1> "
            set $var \[lindex \"$ncinfo(var_name)\" \[%W index @%x,%y\]\]
            $entry selection range 0 end
          "
          bind $ww.box1.list <Destroy> "
            if {\[info exists newvar\]} {
               set $var \$newvar
               unset -nocomplain newvar
            } else {
               set $var $name0
            }
            $entry selection range 0 end
            #$entry selection clear
         "
      } else {
          bind $ww.box1.list <Double-1> \
              "set $var \[lindex \"$ncinfo(var_name)\" \[%W index @%x,%y\]\]"
          bind $ww.box1.list <Destroy> "
            if {\[info exists newvar\]} {
               set $var \$newvar
               unset -nocomplain newvar
            } else {
               set $var $name0
            }
         "
      }
   }

   if {$stdnamevar != ""} {
      upvar #0 $stdnamevar stdname
      if {[info exists stdname]} {
         set stdname0 $stdname
      } else {
         set stdname0 "no_standard_name"
      }
 
      bind $ww.box1.list <Button-1> "+
         set newstdname \[lindex \"$ncinfo(standard_name)\" \[%W index @%x,%y\]\]
      "
      bind $ww.box1.list <Double-1> "+
         set $stdnamevar \[lindex \"$ncinfo(standard_name)\" \[%W index @%x,%y\]\]
      "
      bind $ww.box1.list <Destroy> "+
         if {\[info exists newstdname\]} {
            set $stdnamevar \$newstdname
            unset -nocomplain newstdname
         } else {
            set $stdnamevar $stdname0
         }
      "
   }

   for {set i 0} {$i < $ncinfo(nvar)} {incr i} {
      if {$max2 == 0 && $max3 == 0} {
         set line [format "%-${max1}s " [lindex $ncinfo(var_name) $i]]
      } elseif {$max2 == 0} {
         set line [format "%-${max1}s | %-${max3}s " \
                          [lindex $ncinfo(var_name) $i] \
                          [lindex $ncinfo(standard_name) $i]]
      } elseif {$max3 == 0} {
         set line [format "%-${max1}s | %-${max2}s " \
                          [lindex $ncinfo(var_name) $i] \
                          [lindex $ncinfo(long_name) $i]]
      } else {
         set line [format "%-${max1}s | %-${max2}s | %-${max3}s " \
                          [lindex $ncinfo(var_name) $i] \
                          [lindex $ncinfo(long_name) $i] \
                          [lindex $ncinfo(standard_name) $i]]
      }
      $ww.box1.list insert $i $line
   }

   if {$var == ""} {
      $ww.box1.list configure -state disabled \
                              -disabledforeground $foregroundcolour
   }
}

proc getncfiles {win var} {

   global $var ncfile

   eval set nfile $${var}(nncfile)

#  Destroy existing entries

   set i 1
   while {true} {
      if {[winfo exists $win.ncfile$i]} {
         destroy $win.ncfile$i
         incr i
      } else {
         break
      }
   }
   destroy $win.fspacer1 $win.fspacer2
   
#  Insert new entries

   if {[isint $nfile]} {
      set i 1
      if {$nfile > 0} {spacer $win.fspacer1}
      eval set ncfilevar $${var}(ncfile)
      #set ${var}(ncfile) ""
      #puts "1 ${var}(ncfile) = $ncfilevar"
      #puts "astart(ncfile) = $astart(ncfile)"
      while {$i <= $nfile} {
         set ncfile($i) [lindex $ncfilevar $i]
         #set ncfile($i) [lindex ${var}(ncfile) $i]
         #set ncfile($i) [lindex astart(ncfile) $i]
         #set_var $win.ncfile$i "Netcdf file [format "%2d" $i]: " ${var}(ncfile$i) 58
         #get_filename2 $win.ncfile$i "Netcdf file [format "%2d" $i]: " ${var}(ncfile$i) nc
         get_filename2 $win.ncfile$i "Netcdf file [format "%2d" $i]: " ncfile($i) nc
         lappend ${var}(ncfile) $ncfile($i)
	 incr i
      }
      #puts "2 ${var}(ncfile) = $ncfilevar"
   }
   spacer $win.fspacer2
}

proc select_fields {win label1 label2 label3 label4 var1 var2 var3} {

   global itemindex stashnamelist standardnamelist shortnamelist

   frame $win
   pack $win -side top -fill both -expand yes

   set tbl $win.tbl

   tablelist::tablelist $tbl \
      -columns [list 0 $label1 left 0 $label2 left \
                     0 $label3 left 0 $label4 left] \
      -height 0 -width 10 -stretch all -selectmode extended -activestyle none
   set_tablelist_bind

   $tbl configure -selectmode extended
   if {[$tbl cget -selectborderwidth] == 0} {
      $tbl configure -spacing 1
   }
   $tbl columnconfigure 0 -stretchable no
   $tbl columnconfigure 3 -stretchable no
   $tbl columnconfigure 2 -editable yes
   $tbl columnconfigure 3 -editable yes

   grid $tbl -sticky news
   grid rowconfigure    $win {0 1} -weight 1
   grid columnconfigure $win {0} -weight 1
   pack $win -side top -expand yes -fill both

   if [info exist itemindex] {
      set nitem [llength $itemindex]
   } else {
      set nitem 0
   }
   puts "nitem = $nitem"
   puts "1 $tbl get 0 [$tbl get 0]"
   for {set row 0} {$row < $nitem} {incr row} {
      #$tbl cellconfigure $row,0 -text [lindex $itemindex $row]
      #$tbl cellconfigure $row,1 -text [lindex $stashnamelist $row]
      #$tbl cellconfigure $row,2 -text [lindex $shortnamelist $row]
      $tbl insert end [list [lindex $itemindex $row] \
                      [lindex $stashnamelist $row] \
                      [lindex $shortnamelist $row] "1"]
   }
   puts "2 $tbl get 0 [$tbl get 0]"

   #$tbl columnconfigure 0 -text $itemindex
   #$tbl columnconfigure 1 -text $stashnamelist
   #$tbl columnconfigure 2 -text $shortnamelist
}

proc umfilelist {win file {readfile 1}} {
   global w4 selectname
   global nitem itemindex stashlist stashnamelist standardnamelist shortnamelist
   global stashlabellist

#   puts "umfilelist ..."
   set stashlist {}

   if {$file == ""} {return}
   if {! [file exists $file]} {
      write_message "$file: No such file or directory"
      return
   }
   
   if {[file isdirectory $file]} {
      write_message "$file: Is a directory"
      return
   }

   if {! [info exists itemindex]} {set readfile 1}

   if {$readfile} {
      set error [catch {getppheaders $file pphead} msg]
      if {$error} {
         write_message $msg
         return
      }

      set model $pphead(model)
      set version $pphead(version)
      set nfield $pphead(nfield)
      set nprog $pphead(nprog)
      if {$nprog == -32768} {set nprog $nfield}

#      write_message "file = $file"
#      write_message "win = $win"
#      write_message "pphead(model) = $pphead(model)"
#      write_message "pphead(version) = $pphead(version)"
#      write_message "pphead(nfield) = $pphead(nfield)"
#      write_message "pphead(nprog) = $pphead(nprog)"
#      write_message "pphead(1) = $pphead(1)"
#      write_message "pphead(2) = $pphead(2)"
#      write_message "pphead($nfield) = $pphead($nfield)"
   
      set stash1 -1
      set stash2 [lindex $pphead(1) 41]
      lappend stashlist $stash2
      lappend itemindex 1
      lappend ppcodelist [lindex $pphead(1) 22]
      for {set i 2} {$i <= $nprog} {incr i} {
         set stash1 [lindex $pphead($i) 41]
         if {$stash2 != $stash1} {
            lappend stashlist $stash1
            lappend itemindex $i
            lappend ppcodelist [lindex $pphead($i) 22]
         }
         set stash2 $stash1
      }
      set nitem [llength $itemindex]
   
#      write_message "nitem = $nitem"
#      write_message "stashlist = $stashlist [llength $stashlist]"
#      write_message "itemindex = $itemindex [llength $itemindex]"
#      write_message "ppcodelist = $ppcodelist [llength $ppcodelist]"

      set shortnamelist [getshortname_fromppcode $ppcodelist]
      set field 0
      foreach shortname $shortnamelist {
         set ashortname($field) $shortname
         incr field
      }
      set stashnamelist [getstashname $model $version $stashlist]
      set standardnamelist [getstandardname $model $version $stashlist]
      set field 0
      set stashlabellist ""
      foreach stashname $stashnamelist {
          lappend stashlabellist \
              "[lindex $stashlist $field] [string trim $stashname]"
          incr field
      }
#      write_message "stashlabellist = $stashlabellist [llength $stashlabellist]"
   }
}

proc umfilelistXXX {win file {readfile 1}} {

   global w4 selectname
   global itemindex ashortname stashnamelist standardnamelist

   if {! [file exists $file]} {
      write_message "$file: No such file or directory"
      return
   }
   
   if {[file isdirectory $file]} {
      write_message "$file: Is a directory"
      return
   }

   if {! [info exists itemindex]} {set readfile 1}

   if {$readfile} {
      set error [catch {getppheaders $file pphead} msg]
      if {$error} {
         write_message $msg
         return
      }

      set model $pphead(model)
      set version $pphead(version)
      set nfield $pphead(nfield)
      set nprog $pphead(nprog)
      if {$nprog == -32768} {set nprog $nfield}

      write_message "file = $file"
      write_message "win = $win"
      write_message "pphead(model) = $pphead(model)"
      write_message "pphead(version) = $pphead(version)"
      write_message "pphead(nfield) = $pphead(nfield)"
      write_message "pphead(nprog) = $pphead(nprog)"
      write_message "pphead(1) = $pphead(1)"
      write_message "pphead(2) = $pphead(2)"
      write_message "pphead($nfield) = $pphead($nfield)"
   
      set stash1 -1
      set stash2 [lindex $pphead(1) 41]
      lappend stashlist $stash2
      lappend itemindex 1
      lappend ppcodelist [lindex $pphead(1) 22]
      for {set i 2} {$i <= $nprog} {incr i} {
         set stash1 [lindex $pphead($i) 41]
         if {$stash2 != $stash1} {
            lappend stashlist $stash1
            lappend itemindex $i
            lappend ppcodelist [lindex $pphead($i) 22]
         }
         set stash2 $stash1
      }
      set nitem [llength $itemindex]
   
      write_message "nitem = $nitem"
      write_message "stashlist = $stashlist [llength $stashlist]"
      write_message "itemindex = $itemindex [llength $itemindex]"
      write_message "ppcodelist = $ppcodelist [llength $ppcodelist]"

      set shortnamelist [getshortname_fromppcode $ppcodelist]
      set field 0
      foreach shortname $shortnamelist {
         set ashortname($field) $shortname
         incr field
      }
      set stashnamelist [getstashname $model $version $stashlist]
      set standardnamelist [getstandardname $model $version $stashlist]
   }      

   $w4.canvasancil configure -scrollregion {0 0 0 100c}

   destroy $win.fields
   frame $win.fields
   pack $win.fields -fill both -expand yes
   #pack $win.fields
   if {$selectname == "stash" } {
      set field 0
      foreach stashname $stashnamelist item $itemindex {
         sel_dump_field $win.fields $field $stashname
         incr field
      }
   } elseif {$selectname == "standard" } {
      set field 0
      foreach standardname $standardnamelist item $itemindex {
         sel_dump_field $win.fields $field $standardname
         incr field
      }
   }
}

proc sel_dump_field {win field longname} {

   set f [frame $win.$field]
   pack $f -side top -anchor w -fill x

   global ashortname

   #button $f.infobutton -bitmap info -fg blue
   label $f.labelbox1 -text [format "%3d:" $field] -relief flat
   label $f.labelbox2 -text [format "%-50s" "$longname"] -relief flat
   entry $f.entry -textvariable ashortname($field) -relief sunken
   #pack $f.infobutton -side left
   pack $f.labelbox1 -side left -fill x
   pack $f.labelbox2 -side left -fill x
   pack $f.entry -side left -fill x
}

proc getvarname {ncfile model iversion stashcode name \
                {ppcode -1} {standardname ""}} {

   set varname ""

   if {[file exists $ncfile]} {
      getncvarinfo $ncfile ncinfo
   } else {
      return $varname
   }

   #puts "name = $name"
   if {$name != ""} {
      set index [lsearch -exact $ncinfo(var_name) $name]
      if {$index > -1} {
         lappend varname $name
      }
   }

   if {$standardname == ""} {
      set standardname [lindex [getstandardname $model $iversion $stashcode] 0]
   }
   #puts "standardname = $standardname"

   if {$standardname != "no_standard_name"} {
      set indexlist [lsearch -exact -all $ncinfo(standard_name) $standardname]
      foreach index $indexlist {
         lappend varname [lindex $ncinfo(var_name) $index]
      }
   }

   if {$ppcode == -1} {
      set ppcode [lindex [getppcode $model $iversion $stashcode] 0]
   }
   #puts "ppcode = $ppcode"

   if {[isint $ppcode]} {
      set shortname [lindex [getshortname_fromppcode $ppcode] 0]
      #puts "shortname = $shortname"
      
      set index [lsearch -exact $ncinfo(var_name) $shortname]
      if {$index > -1} {
         lappend varname $shortname
      }

      set indexlist [lsearch -glob -all $ncinfo(var_name) ${shortname}_*]
      foreach index $indexlist {
         lappend varname [lindex $ncinfo(var_name) $index]
      }
   }
   #puts "varname = $varname"
   
   return $varname
}

proc getdimname {ncfile name ppcode {standardname ""}} {

   set dimname ""

   if {[file exists $ncfile]} {
      getncdiminfo $ncfile ncinfo
   } else {
      return $dimname
   }

   #puts "name = $name"
   if {$name != ""} {
      set index [lsearch -exact $ncinfo(var_name) $name]
      if {$index > -1} {
         lappend dimname $name
      }
   }

   #puts "standardname = $standardname"
   if {$standardname != ""} {
      set indexlist [lsearch -exact -all $ncinfo(standard_name) $standardname]
      foreach index $indexlist {
         lappend dimname [lindex $ncinfo(var_name) $index]
      }
    }

   #puts "ppcode = $ppcode"
   if {[isint $ppcode]} {
      set shortname [lindex [getshortname_fromppcode $ppcode] 0]
      #puts "shortname = $shortname"
      
      set index [lsearch -exact $ncinfo(var_name) $shortname]
      if {$index > -1} {
         lappend dimname $shortname
      }

      set indexlist [lsearch -glob -all $ncinfo(var_name) ${shortname}_*]
      foreach index $indexlist {
         lappend dimname [lindex $ncinfo(var_name) $index]
      }
   }
   #puts "dimname = $dimname"
   
   return $dimname
}

proc getstashname {model version stashlist} {

   global MK

   set view1 db.userstashmaster
   if {$version < 500} {
      set view2 db.stashmaster_405
   } elseif {$version >= 500 && $version < 600} {
      set view2 db.stashmaster_505
   } elseif {$version >= 600 && $version < 700} {
      set view2 db.stashmaster_601
   } elseif {$version >= 700} {
      set view2 db.stashmaster_701
   }
   
   foreach stashcode $stashlist {
   
      set section [expr $stashcode/1000]
      set item [expr $stashcode - $section*1000]

      set row [::mk::select $view1 -count 1 model $model \
                            section $section item $item]
#      set row [::mk::select $view1 -count 1 -exact model $model \
#                            -exact section $section -exact item $item]

      if {[llength $row] == 0} {
         set row [::mk::select $view2 -count 1 model $model \
                               section $section item $item]
#         set row [::mk::select $view2 -count 1 -exact model $model \
#                               -exact section $section -exact item $item]

         if {[llength $row] == 0} {
            lappend stashname "STASHCODE = $stashcode"
         } else {
            lappend stashname [::mk::get $view2!$row name]
         }
      } else {
         lappend stashname [::mk::get $view1!$row name]
      }
   }
   
   return $stashname
}

proc getppcode {model version stashlist} {

   global MK

   set view1 db.userstashmaster
   if {$version < 500} {
      set view2 db.stashmaster_405
   } elseif {$version >= 500 && $version < 600} {
      set view2 db.stashmaster_505
   } elseif {$version >= 600 && $version < 700} {
      set view2 db.stashmaster_601
   } elseif {$version >= 700} {
      set view2 db.stashmaster_701
   }
   
   foreach stashcode $stashlist {
   
      set section [expr $stashcode/1000]
      set item [expr $stashcode - $section*1000]

      set row [::mk::select $view1 -count 1 model $model \
                            section $section item $item]
#      set row [::mk::select $view1 -count 1 -exact model $model \
#                            -exact section $section -exact item $item]

      if {[llength $row] == 0} {
         set row [::mk::select $view2 -count 1 model $model \
                               section $section item $item]
#         set row [::mk::select $view2 -count 1 -exact model $model \
#                               -exact section $section -exact item $item]

         if {[llength $row] == 0} {
            lappend ppcode ""
         } else {
            lappend ppcode [::mk::get $view2!$row ppcode]
         }
      } else {
         lappend ppcode [::mk::get $view1!$row ppcode]
      }
   }
   
   return $ppcode
}

proc getlevtype {model version stashlist} {

   global MK

   set view1 db.userstashmaster
   if {$version < 500} {
      set view2 db.stashmaster_405
   } elseif {$version >= 500 && $version < 600} {
      set view2 db.stashmaster_505
   } elseif {$version >= 600 && $version < 700} {
      set view2 db.stashmaster_601
   } elseif {$version >= 700} {
      set view2 db.stashmaster_701
   }
   
   foreach stashcode $stashlist {
   
      set section [expr $stashcode/1000]
      set item [expr $stashcode - $section*1000]

      set row [::mk::select $view1 -count 1 model $model \
                            section $section item $item]
#      set row [::mk::select $view1 -count 1 -exact model $model \
#                            -exact section $section -exact item $item]

      if {[llength $row] == 0} {
         set row [::mk::select $view2 -count 1 model $model \
                               section $section item $item]
#         set row [::mk::select $view2 -count 1 -exact model $model \
#                               -exact section $section -exact item $item]

         if {[llength $row] == 0} {
            lappend levtype 0
         } else {
            lappend levtype [::mk::get $view2!$row levtype]
         }
      } else {
         lappend levtype [::mk::get $view1!$row levtype]
      }
   }
   
   return $levtype
}

proc getgridtype {model version stashlist} {

   global MK
   
   set view1 db.userstashmaster
   if {$version < 500} {
      set view2 db.stashmaster_405
   } elseif {$version >= 500 && $version < 600} {
      set view2 db.stashmaster_505
   } elseif {$version >= 600 && $version < 700} {
      set view2 db.stashmaster_601
   } elseif {$version >= 700} {
      set view2 db.stashmaster_701
   }
   
   foreach stashcode $stashlist {
   
      set section [expr $stashcode/1000]
      set item [expr $stashcode - $section*1000] 

      set row [::mk::select $view1 -count 1 model $model \
                            section $section item $item]
#      set row [::mk::select $view1 -count 1 -exact model $model \
#                            -exact section $section -exact item $item]

      if {[llength $row] == 0} {
         set row [::mk::select $view2 -count 1 model $model \
                               section $section item $item]
#         set row [::mk::select $view2 -count 1 -exact model $model \
#                               -exact section $section -exact item $item]

         if {[llength $row] == 0} {
            lappend gridtype 1
         } else {
            lappend gridtype [::mk::get $view2!$row gridtype]
         }
      } else {
         lappend gridtype [::mk::get $view1!$row gridtype]
      }
   }
   
   return $gridtype
}

proc getdatatype {model version stashlist} {

   global MK

   set view1 db.userstashmaster
   if {$version < 500} {
      set view2 db.stashmaster_405
   } elseif {$version >= 500 && $version < 600} {
      set view2 db.stashmaster_505
   } elseif {$version >= 600 && $version < 700} {
      set view2 db.stashmaster_601
   } elseif {$version >= 700} {
      set view2 db.stashmaster_701
   }
   
   foreach stashcode $stashlist {
   
      set section [expr $stashcode/1000]
      set item [expr $stashcode - $section*1000]

      set row [::mk::select $view1 -count 1 model $model \
                            section $section item $item]
#      set row [::mk::select $view1 -count 1 -exact model $model \
#                            -exact section $section -exact item $item]

      if {[llength $row] == 0} {
         set row [::mk::select $view2 -count 1 model $model \
                               section $section item $item]
#         set row [::mk::select $view2 -count 1 -exact model $model \
#                               -exact section $section -exact item $item]

         if {[llength $row] == 0} {
            lappend datatype 1
         } else {
            lappend datatype [::mk::get $view2!$row datatype]
         }
      } else {
         lappend datatype [::mk::get $view1!$row datatype]
      }
   }
   
   return $datatype
}

proc getmasktype {model version stashlist} {

   global MK

   set view1 db.userstashmaster
   if {$version < 500} {
      set view2 db.stashmaster_405
   } elseif {$version >= 500 && $version < 600} {
      set view2 db.stashmaster_505
   } elseif {$version >= 600 && $version < 700} {
      set view2 db.stashmaster_601
   } elseif {$version >= 700} {
      set view2 db.stashmaster_701
   }
   
   foreach stashcode $stashlist {
   
      set section [expr $stashcode/1000]
      set item [expr $stashcode - $section*1000]

      set row [::mk::select $view1 -count 1 model $model \
                            section $section item $item]
#      set row [::mk::select $view1 -count 1 -exact model $model \
#                            -exact section $section -exact item $item]

      if {[llength $row] == 0} {
         set row [::mk::select $view2 -count 1 model $model \
                               section $section item $item]
#         set row [::mk::select $view2 -count 1 -exact model $model \
#                               -exact section $section -exact item $item]

         if {[llength $row] == 0} {
            lappend masktype 0
         } else {
            lappend masktype [::mk::get $view2!$row masktype]
         }
      } else {
         lappend masktype [::mk::get $view1!$row masktype]
      }
   }
   
   return $masktype
}

proc getstandardname {model version stashlist} {

   global MK

   set view db.standard_name
   
   foreach stashcode $stashlist {
   
      set section [expr $stashcode/1000]
      set item [expr $stashcode - $section*1000]

      set row [::mk::select $view -count 1 model $model \
                            section $section item $item]
#      set row [::mk::select $view -count 1 -exact model $model \
#                            -exact section $section -exact item $item]

      if {[llength $row] == 0} {
         lappend standardname "no_standard_name"
      } else {
         lappend standardname [::mk::get $view!$row standard_name]
      }
   }
   
   return $standardname
}

proc getshortname_fromppcode {ppcode} {

   global MK

   set view db.pp
   
   foreach code $ppcode {
#      set row [::mk::select $view -count 1 -exact ppcode $code]
      set row [::mk::select $view -count 1 ppcode $code]

      if {[llength $row] == 0} {
         lappend shortname "field$code"
      } else {
         lappend shortname [::mk::get $view!$row ppname]
      }
   }
   
   return $shortname
}

proc getppcode_fromshortname {shortname} {

   global MK

   set view db.pp
   
   foreach name $shortname {

      if {[string compare -length 5 $name "field"] == 0} {
         set idx [string first "_" $name]
         if {$idx == -1} {
            set idx end
         } else {
            incr idx -1
         }
         set code [string range $name 5 $idx]
         if {[isint $code]} {
            lappend ppcode $code
         } else {
            lappend ppcode ""
         }
      } else {
         set row [::mk::select $view -count 1 ppname $name]
#         set row [::mk::select $view -count 1 -exact ppname $name]

         if {[llength $row] == 0} {
            lappend ppcode ""
         } else {
            lappend ppcode [::mk::get $view!$row ppcode]
         }
      }
   }
   
   return $ppcode
}

proc getstashcode_fromppcode {model version ppcode} {

   global MK

   set view1 db.userstashmaster
   if {$version < 500} {
      set view2 db.stashmaster_405
   } elseif {$version >= 500 && $version < 600} {
      set view2 db.stashmaster_505
   } elseif {$version >= 600 && $version < 700} {
      set view2 db.stashmaster_601
   } elseif {$version >= 700} {
      set view2 db.stashmaster_701
   }

   set row [::mk::select $view1 -count 1 model $model ppcode $ppcode]
#   set row [::mk::select $view1 -count 1 -exact model $model \
#                                         -exact ppcode $ppcode]

   if {[llength $row] == 0} {
      set row [::mk::select $view2 -count 1 model $model ppcode $ppcode]
#      set row [::mk::select $view2 -count 1 -exact model $model \
#                                            -exact ppcode $ppcode]

      if {[llength $row] == 0} {
         set stashcode ""
      } else {
         set section [::mk::get $view2!$row section]
         set item [::mk::get $view2!$row item]
         set stashcode [expr $section*1000 + $item]
      }
   } else {
      set section [::mk::get $view1!$row section]
      set item [::mk::get $view1!$row item]
      set stashcode [expr $section*1000 + $item]
   }
   return $stashcode
}

proc getstashcode_fromstdname {model standardname} {

   global MK

   set view db.standard_name
   
   set row [::mk::select $view -count 1 model $model standard_name $standardname]
#   set row [::mk::select $view -count 1 -exact model $model \
#                         -exact standard_name $standardname]

   if {[llength $row] == 0} {
      set stashcode ""
   } else {
      set section [::mk::get $view!$row section]
      set item [::mk::get $view!$row item]
      set stashcode [expr $section*1000 + $item]
   }
   
   return $stashcode
}

proc chkuserstashmaster {filelistvar} {

   upvar #0 $filelistvar filelist
   set filelist1 $filelist

   foreach file $filelist1 {
      if {! [file exists $file]} {
         write_message "Error: Cannot open User STASHMaster file $file"
         set index [lsearch -exact $filelist $file]
         if {$index >= 0} {
            set filelist [lreplace $filelist $index $index]
         }
      }
   }
}

proc adduserstashmaster {filelist} {

   global MK

   set view db.userstashmaster

   foreach file $filelist {
      if {! [file exists $file]} {
         write_message "Error: Cannot open User STASHMaster file $file"
         continue
      }
      set fd [open $file "r"]
      while {[gets $fd line] >= 0} {
         set l0 [string index $line 0]
         if {$l0 == "1"} {
            set model [string range $line 2 7]
            if {[expr {$model}] == -1} break
            set section [string range $line 9 14]
            set item [string range $line 16 21]
            set name [string range $line 23 58]
         } elseif {$l0 == "2"} {
            set grid [string range $line 23 28]
            set levelt [string range $line 30 35]
            set levelf [string range $line 37 42]
            set levell [string range $line 44 49]
            set pseudt [string range $line 51 56]
            if {$levelt == 1 && $levelf == 21} { # ocean depth levels
               set levtype 5
            } elseif {$levell == 23} { # ozone levels
               set levtype 4
            } elseif {$levelt == 1} {  # atmos model levels
               set levtype 3
            } elseif {$levelt == 2} {  # atmos model half levels
               set levtype 6
            } elseif {$levelt == 6} {  # soil levels
               set levtype 1
            } elseif {$pseudt != 0} {  # pseudo levels
               set levtype 2
            } elseif {$levelt == 5} {  # single level
               set levtype 0
            } else {                   # unknown level
               set levtype -1
            }
            if {$grid == 2 || $grid == 12 || $grid == 21} { # land data only
               set masktype 2
            } elseif {$grid == 3 || $grid == 13} {          # sea data only
               set masktype 1
            } else {                                        # land+sea data
               set masktype 0
            }
            if {($grid >= 11 && $grid <= 15) || 
                 $grid == 32 || $grid == 37 || $grid == 42 || $grid == 44 || $grid == 46} {
               set gridtype 2
            } elseif {$grid == 18 || $grid == 38} {
               set gridtype 3
            } elseif {$grid == 19 || $grid == 39} {
               set gridtype 4
            } else {
               set gridtype 1
            }
         } elseif {$l0 == "4"} {
            set datatype [string range $line 2 7]
         } elseif {$l0 == "5"} {
            set ppcode [string range $line 9 14]
            set row [::mk::select $view -count 1 \
                                  model [expr $model] \
                                  section [expr $section] \
                                  item [expr $item]]
#            set row [::mk::select $view -count 1 \
#                                  -exact model [expr $model] \
#                                  -exact section [expr $section] \
#                                  -exact item [expr $item]]
            if {[llength $row] != 0} {
               ::mk::row delete $view!$row
            }
            ::mk::row append $view "model $model section $section item $item \
                                    name {$name} ppcode $ppcode \
                                    levtype $levtype gridtype $gridtype \
                                    datatype $datatype masktype $masktype"
         }
      }
      close $fd
   }
   #puts "$view size [::mk::view size $view]"
   #::mk::loop cursor $view {
   #   puts [::mk::get $cursor]
   #}
}

proc deluserstashmaster {filelist} {

   global MK config

   foreach file $filelist {
      set index [lsearch -exact $config(smfile) $file]
      if {$index >= 0} {
         set config(smfile) [lreplace $config(smfile) $index $index]
      }
   }

   set view db.userstashmaster
   ::mk::view size $view 0

   foreach file $config(smfile) {
      if {! [file exists $file]} {
         write_message "Error: Cannot open User STASHMaster file $file"
         continue
      }
      set fd [open $file "r"]
      while {[gets $fd line] >= 0} {
         set l0 [string index $line 0]
         if {$l0 == "1"} {
            set model [string range $line 2 7]
            if {[expr {$model}] == -1} break
            set section [string range $line 9 14]
            set item [string range $line 16 21]
            set name [string range $line 23 58]
         } elseif {$l0 == "2"} {
            set grid [string range $line 23 28]
            set levelt [string range $line 30 35]
            set levelf [string range $line 37 42]
            set levell [string range $line 44 49]
            set pseudt [string range $line 51 56]
            if {$levelt == 1 && $levelf == 21} { # ocean depth levels
               set levtype 5
            } elseif {$levell == 23} { # ozone levels
               set levtype 4
            } elseif {$levelt == 1} {  # atmos model levels
               set levtype 3
            } elseif {$levelt == 2} {  # atmos model half levels
               set levtype 6
            } elseif {$levelt == 6} {  # soil levels
               set levtype 1
            } elseif {$pseudt != 0} {  # pseudo levels
               set levtype 2
            } elseif {$levelt == 5} {  # single level
               set levtype 0
            } else {                   # unknown level
               set levtype -1
            }
            if {$grid == 2 || $grid == 12 || $grid == 21} { # land data only
               set masktype 2
            } elseif {$grid == 3 || $grid == 13} {          # sea data only
               set masktype 1
            } else {                                        # land+sea data
               set masktype 0
            }
         } elseif {$l0 == "4"} {
            set datatype [string range $line 2 7]
         } elseif {$l0 == "5"} {
            set ppcode [string range $line 9 14]
            ::mk::row append $view "model $model section $section item $item \
                                    name {$name} ppcode $ppcode \
                                    levtype $levtype datatype $datatype \
                                    masktype $masktype"
         }
      }
      close $fd
   }
   #puts "$view size [::mk::view size $view]"
   #::mk::loop cursor $view {
   #   puts [::mk::get $cursor]
   #}
}

proc getarrayname {var} {

   string range $var 0 [expr [string first "(" $var] - 1]
}

proc isint {var} {
    expr {![catch {expr {~$var}}]}
}
    
proc isfloat {var} {
    expr {![catch {expr {double($var)}}]}
}

proc set_tablelist_bind {} {

# Redefine bindings for Tablelist widgets

   bind TablelistBody <Button-1> {
      if {[winfo exists %W]} {
         foreach {tablelist::W tablelist::x tablelist::y} \
            [tablelist::convEventFields %W %x %y] {}

         set tablelist::priv(x) $tablelist::x
         set tablelist::priv(y) $tablelist::y
         set tablelist::priv(row) [$tablelist::W nearest       $tablelist::y]
         set tablelist::priv(col) [$tablelist::W nearestcolumn $tablelist::x]
         set tablelist::priv(clicked) 1
         set tablelist::priv(clickTime) %t
         set tablelist::priv(clickedInEditWin) 0
         if {[$tablelist::W cget -setfocus]} {
            focus [$tablelist::W bodypath]
         }
         tablelist::condEditContainingCell $tablelist::W \
                                           $tablelist::x $tablelist::y
         tablelist::condBeginMove $tablelist::W $tablelist::priv(row)
         set col [tablelist::containingCol $tablelist::W $tablelist::x]
         puts "Column $col [$tablelist::W columncget $col -editable]"
         if {! [$tablelist::W columncget $col -editable]} {
            tablelist::beginToggle $tablelist::W \
                                   $tablelist::priv(row) $tablelist::priv(col)
         }
      }
   }

   bind TablelistBody <Button-2> {
      foreach {tablelist::W tablelist::x tablelist::y} \
         [tablelist::convEventFields %W %x %y] {}

      set col [tablelist::containingCol $tablelist::W $tablelist::x]
      puts "Column $col [$tablelist::W columncget $col -editable]"
      if {! [$tablelist::W columncget $col -editable]} {
         tablelist::beginSelect $tablelist::W \
            [$tablelist::W nearest       $tablelist::y] \
            [$tablelist::W nearestcolumn $tablelist::x]
      }
   }

   bind TablelistBody <Button-3> {
      foreach {tablelist::W tablelist::x tablelist::y} \
         [tablelist::convEventFields %W %x %y] {}

      set col [tablelist::containingCol $tablelist::W $tablelist::x]
      puts "Column $col [$tablelist::W columncget $col -editable]"
      if {! [$tablelist::W columncget $col -editable]} {
         tablelist::beginExtend $tablelist::W \
             [$tablelist::W nearest       $tablelist::y] \
             [$tablelist::W nearestcolumn $tablelist::x]
      }
   }
}
