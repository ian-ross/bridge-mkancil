proc genanc_configwin {win xarg} {

   global ww0 parent_group parent_entry group entry 
   global config genanc_config genanc_config_xold

   set parent_group [lindex $xarg 0]
   set parent_entry [lindex $xarg 1]
   set group [lindex $xarg 2]
   set entry [lindex $xarg 3]
   set genanc_config_xold $genanc_config(nancfile)

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

   title $ww0.title "Generalised Ancillary file configuration"
   spacer $ww0.spacer1
   set_var_spin $ww0.nancfile "Enter number of ancillary files: " genanc_config(nancfile) 5

   spacer $ww0.spacer2
   getfilebox $ww0.getncfiles "NetCDF files to be read" config(ncfile) "nc"

#  Add actions to variable changes

   trace add variable genanc_config(nancfile) write genanc_config_addentry

#  Add action to netcdf file double clicks

   bind $ww0.getncfiles.box.list <Double-1> {
      set ncfile [%W get @%x,%y]
      select_ncvarname $ncfile
   }
}

proc genanc_config_addentry {name element op} {

   global ww0 parent_group parent_entry group entry
   global defanc0 defanc00 genanc genanc_config_xold status

   if {$element != ""} {
      set name ${name}($element)
   }
   upvar #0 $name x

   if {$x != $genanc_config_xold && [isint $x] && $x >= 0} {
      #puts "$name = $x $genanc_config_xold"
      for {set xx [expr {$genanc_config_xold+1}]} {$xx <= $x} {incr xx} {
         foreach el [array names defanc0] {
            if {! [info exists genanc($xx,$el)]} {
               set genanc($xx,$el) $defanc0($el)
            }
         }
         foreach el [array names defanc00] {
            if {! [info exists genanc($xx,1,$el)]} {
               set genanc($xx,1,$el) $defanc00($el)
            }
         }
      }
      addentry $x $parent_group $parent_entry $group $entry
      if {$status($group)} {
         group_entry_func $group $entry
         focus $ww0.nancfile.spinbox
      }
      set genanc_config_xold $x
   }
}
