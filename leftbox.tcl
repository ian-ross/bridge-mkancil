
# Left hand side canvas

set ngroup 6
#set ngroup 8
set xorigin 20
set yorigin 20
set xspace 20
set yspace 20
set yscrollbot [expr {$yorigin+$yspace}]

set nentry(0) 1  ; set level(0) 0
set nentry(1) 4  ; set level(1) 1
#set nentry(1) 6  ; set level(1) 1
set nentry(2) 3  ; set level(2) 2
set nentry(3) 15 ; set level(3) 2
set nentry(4) 4  ; set level(4) 2
set nentry(5) 1  ; set level(5) 2
set nentry(6) 3  ; set level(6) 2
set nentry(7) 5  ; set level(7) 2

set status(0) "on"
for {set i 0} {$i < $ngroup} {incr i} {
   set prev [expr $i-1]
   set xpos($i) [expr $xorigin + $level($i)*$xspace]
   if {$i > 0} {
      set status($i) "off"
   }

   for {set j 1} {$j <= $nentry($i)} {incr j} {
      set ypos($i,$j) $yorigin
      set title($i,$j) "DUMMY"
      set next($i,$j) dummy_func
   }
}

set title(0,1) "Xancil"                               ; set next(0,1) group1

set title(1,1) "Configuration"                        ; set next(1,1) group2
set title(1,2) "Atmosphere Ancillary Files"           ; set next(1,2) group3
set title(1,3) "Ocean Ancillary Files"                ; set next(1,3) group4
set title(1,4) "Generalised Ancillary Files"          ; set next(1,4) group5
set title(1,5) "Atmosphere Start Dump"                ; set next(1,5) group6
set title(1,6) "Ocean Start Dump"                     ; set next(1,6) group7

set title(2,1) "General Configuration"                ; set next(2,1) {config}
set title(2,2) "Grid Configuration"                   ; set next(2,2) {gridconfig}
set title(2,3) "Select Ancillary Files to be Created" ; set next(2,3) {switchancil}

#set title(3,1) "Ozone"                                           ; set next(3,1) {ancil ozone}
#set title(3,2) "Soil moisture and snow depth"                    ; set next(3,2) {ancil smow}
#set title(3,3) "Deep soil temperatures"                          ; set next(3,3) {ancil slt}
#set title(3,4) "Soil : VSMC, hydrological/thermal conductivity etc."; set next(3,4) {ancil soil}
#set title(3,5) "Effective vegetation parameters"                 ; set next(3,5) {ancil veg}
#set title(3,6) "Vegetation Distribution: Area"                   ; set next(3,6) {ancil veg.frac}
#set title(3,7) "Vegetation Distribution: Structure"              ; set next(3,7) {ancil veg.func}
#set title(3,8) "Vegetation Distribution: Disturbance"            ; set next(3,8) {ancil veg.dist}
#set title(3,9) "Sea surface temperatures"                        ; set next(3,9) {ancil sst}
#set title(3,10) "Sea ice fields"                                 ; set next(3,10) {ancil ice}
#set title(3,11) "User multi-level ancillary file & fields"       ; set next(3,11) {ancil ausrmulti}
#set title(3,12) "User single-level ancillary file & fields"      ; set next(3,12) {ancil ausrancil}
#set title(3,13) "Orography ancillary file and fields"            ; set next(3,13) {ancil orog}
#set title(3,14) "Land-Sea-Mask ancillary file and fields"        ; set next(3,14) {ancil mask}
#set title(3,15) "Land fraction file"                             ; set next(3,15) {ancil lfrac}
#set title(4,1) "Wind and pressure forcing"                       ; set next(4,1) {ancil ws}
#set title(4,2) "Thermal forcing"                                 ; set next(4,2) {ancil htflux}
#set title(4,3) "Fresh-water input and water type"                ; set next(4,3) {ancil pme}
#set title(4,4) "Reference SST, SSS, Air-Temp & Ice-Depth"        ; set next(4,4) {ancil ts1}
#set title(4,5) "Ice Model forcing fields"                        ; set next(4,5) {ancil iceff}
#set title(4,6) "Flux Correction fields"                          ; set next(4,6) {ancil flux}
#set title(4,7) "User multi-level ancillary file & fields"        ; set next(4,7) {ancil ousrmulti}
#set title(4,8) "User single-level ancillary file & fields"       ; set next(4,8) {ancil ousrancil}
#set title(4,9) "User Defined Tracer fields. Extra Tracers"       ; set next(4,9) {ancil usrtr}

set title(3,1) "Ozone"                         ; set next(3,1)  {ancil ozone}
set title(3,2) "Soil Moisture and Snow Depth"  ; set next(3,2)  {ancil smow}
set title(3,3) "Deep Soil Temperatures"        ; set next(3,3)  {ancil slt}
set title(3,4) "Soil Parameters"               ; set next(3,4)  {ancil soil}
set title(3,5) "Vegetation Parameters"         ; set next(3,5)  {ancil veg}
set title(3,6) "Vegetation Fractions"          ; set next(3,6)  {ancil vegfrac}
set title(3,7) "Vegetation Functional Types"   ; set next(3,7)  {ancil vegfunc}
set title(3,8) "Disturbed Vegetation Fraction" ; set next(3,8)  {ancil vegdist}
set title(3,9) "Sea Surface Temperatures"      ; set next(3,9)  {ancil sst}
set title(3,10) "Sea Ice"                      ; set next(3,10) {ancil ice}
set title(3,11) "Orography"                    ; set next(3,11) {ancil orog}
set title(3,12) "Land/Sea Mask"                ; set next(3,12) {ancil mask}
set title(3,13) "Land Fraction"                ; set next(3,13) {ancil lfrac}
set title(3,14) "Multi-level User Fields"      ; set next(3,14) {ancil ausrmulti}
set title(3,15) "Single-level User Fields"     ; set next(3,15) {ancil ausrancil}

set title(4,1) "Reference SST, SSS, Air-Temp & Ice-Depth" ; set next(4,1) {ancil ts1}
set title(4,2) "Flux Correction Fields"        ; set next(4,2)  {ancil flux}
set title(4,3) "Multi-level User Fields"       ; set next(4,3)  {ancil ousrmulti}
set title(4,4) "Single-level User Fields"      ; set next(4,4)  {ancil ousrancil}

set title(5,1) "Configuration" ; set next(5,1) {ancil genanc_config \"1 4 5 1\"}

set title(6,1) "Configuration" ; set next(6,1) {ancil astart_config}
set title(6,2) "Start Dump Fields, Use Existing Dump" ; set next(6,2) {ancil astart1}
set title(6,3) "Start Dump Extra Fields" ; set next(6,3) {ancil astart2}

set title(7,1) "Configuration" ; set next(7,1) {}
set title(7,2) "Ocean Mask" ; set next(7,2) {}
set title(7,3) "Ocean Islands" ; set next(7,3) {}
set title(7,4) "Start Dump Fields, Use Existing Dump" ; set next(7,4) {}
set title(7,5) "Start Dump Extra Fields" ; set next(7,5) {}

canvas .canvas -yscrollcommand "$w3.yscroll set" -height 600 -width 330 \
               -scrollregion [concat {0 0} $yorigin $yscrollbot]
yscrollbar $w3.yscroll .canvas
pack .canvas -side left -fill both -expand yes -in $w3

image create photo redbullet -file [file join $xancil_dir redbullet.gif]
image create photo bluebullet -file [file join $xancil_dir bluebullet.gif]

.canvas create image $xorigin $yorigin -image redbullet -tags {tag01 image}
.canvas create text [expr $xorigin + $xspace] $yorigin -anchor w \
               -text $title(0,1) -font $leftbox_font -tags {tag01 text}

.canvas bind all <Any-Enter> {
   set id [.canvas find withtag current]
   set tag [.canvas gettags current]
   if {[lsearch $tag text] >= 0} {
      set id [expr $id-1]
   }
   if {[lsearch $tag line] < 0} {
      .canvas itemconfigure $id -image bluebullet
      .canvas itemconfigure [expr $id+1] -fill white
   }
}
.canvas bind all <Any-Leave> {
   set id [.canvas find withtag current]
   set tag [.canvas gettags current]
   if {[lsearch $tag text] >= 0} {
      set id [expr $id-1]
   }
   if {[lsearch $tag line] < 0} {
      .canvas itemconfigure $id -image redbullet
      .canvas itemconfigure [expr $id+1] -fill black
   }
}

for {set i 0} {$i < $ngroup} {incr i} {
   for {set j 1} {$j <= $nentry($i)} {incr j} {
      .canvas bind tag$i$j <Button-1> "group_entry_func $i $j"
   }
}

proc dummy_func {} {
   puts "This is a dummy function"
}

proc mk_item {x1 y1 text group tags} {
   global ngroup level xspace yspace leftbox_font

   set x0 [expr $x1 - $xspace]
   set x2 [expr $x1 + $xspace]
   set y0 [expr $y1 - $yspace]

   .canvas create image $x1 $y1 -image redbullet -tags [concat $tags image]
   .canvas create text $x2 $y1 -anchor w -text $text -font $leftbox_font -tags [concat $tags text]
   .canvas create line $x0 $y0 $x0 $y1 $x1 $y1 -tags [concat $tags line]

   if {$group != [expr $ngroup-1]} {
      for {set i [expr $level($group)-1]} {$i > 0} {incr i -1} {
         incr x0 -$xspace
         .canvas create line $x0 $y0 $x0 $y1 -tags [concat $tags line]
      }
   }
}

proc move_entries {group yshift} {
   global nentry ypos ngroup status

   for {set i 0} {$i < $ngroup} {incr i} {
      if {$i != $group && $status($i) == "on"} {
         for {set j 1} {$j <= $nentry($i)} {incr j} {
            if {$ypos($i,$j) >= $ypos($group,1)} {
               .canvas move tag$i$j 0 $yshift
               incr ypos($i,$j) $yshift
            }
         }
      }
   }
}

proc del_group {group} {
   global status nentry next

   if {$status($group) == "on"} {
      .canvas delete group$group
      set status($group) "off"

      for {set j 1} {$j <= $nentry($group)} {incr j} {
         foreach next_item $next($group,$j) {
            if {[string range $next_item 0 4] == "group"} {
               set nextgroup [string range $next_item 5 end]
               del_group $nextgroup
            }
         }
      }
   }
}

proc group_entry_func {group entry} {
   global nentry level xpos ypos status title next
   global xspace yspace yorigin yscrollbot

   set run_proc ""
   foreach next_item $next($group,$entry) {
      if {[string range $next_item 0 4] == "group"} {
         set nextgroup [string range $next_item 5 end]

         if {$status($nextgroup) == "off"} {
            set status($nextgroup) "on"
            set x $xpos($nextgroup)
            set y [expr $ypos($group,$entry)+$yspace]
            set yshift [expr $nentry($nextgroup)*$yspace]
            incr yscrollbot $yshift
            set nextentry 1

            for {set k 1} {$k <= $nentry($nextgroup)} {incr k} {
               set tag $nextgroup$k
               mk_item $x $y $title($nextgroup,$nextentry) \
                       $nextgroup "tag$tag group$nextgroup"
               set ypos($nextgroup,$nextentry) $y
               incr y $yspace
               incr nextentry
            }
            move_entries $nextgroup $yshift
         } else {
            set yshift [expr $ypos($nextgroup,$nentry($nextgroup))- \
                             $ypos($nextgroup,1)+$yspace]
            incr yscrollbot [expr -$yshift]
            del_group $nextgroup
            move_entries $nextgroup [expr -$yshift]
         }
         #puts "yscrollbot = $yscrollbot"
         .canvas config -scrollregion [concat {0 0} $yorigin $yscrollbot]
         clear_right
      } else {
         set run_proc "$run_proc $next_item"
      }
   }
   if {$run_proc != ""} $run_proc
}

proc addentry {n parent_group parent_entry group entry} {
   global status nentry title next

   set open_close $status($group)

#  Close group

   if {$open_close} {group_entry_func $parent_group $parent_entry}

#  Add extra entries to group

   set nentry($group) [expr $entry + $n]

   for {set jj 1} {$jj <= $n} {incr jj} {
      set j [expr $jj + $entry]
      set title($group,$j) "Ancillary file $jj"
      set next($group,$j) "ancil genanc $jj"

      .canvas bind tag$group$j <Button-1> "group_entry_func $group $j"
   }

#  Reopen group

   if {$open_close} {group_entry_func $parent_group $parent_entry}
}
