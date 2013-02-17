package provide xancil "1.0"

set xancil_version 0.57
set versiondate 17-February-2013
#set versiondate Development
set HOMEDIR [pwd]
set filter ".* *"
set dirName [pwd]
set savefile ""
set w .main
set w1 .window1
set w2 .window2
set w3 .window3
set w4 .window4
set w5 .window5
set rbwidth 64
set spacerwidth 20
set spacerht 4m
set jobfilename ""
set namelist xancil.namelist
set namelisttype f90

# On IBMs namelist type can be set via environment variable XLFRTEOPTS

if {! [catch {set xlfrteopts $env(XLFRTEOPTS)}]} {
   set xlfrteopts [split $xlfrteopts :]
   set index [lsearch -regexp $xlfrteopts "namelist="]
   if {$index >= 0} {
      set namelisttype [string range [lindex $xlfrteopts $index] 9 end]
   }
}

# This is needed for starkits

#puts "argv0 = $argv0"
#puts "info script = [info script]"
#puts "info nameofexecutable = [info nameofexecutable]"

set xancil_dir [file join [pwd] [file dirname [info script]]]
lappend auto_path $xancil_dir

# Assume mkancil exec is in same directory as xancil

if {[file tail $argv0] == "main.tcl"} {
   set exec_dir [file join [pwd] [file dirname [info nameofexecutable]]]
} else {
   set exec_dir [file join [pwd] [file dirname $argv0]]
}
set exec [file normalize [file join $exec_dir mkancil$xancil_version]]
if {! [file exists $exec]} {
   set exec [file normalize [file join $exec_dir mkancil]]
}

set ancil_types "config gridconfig \
                 ozone smow slt soil veg vegfrac vegfunc vegdist \
                 sst ice orog mask lfrac \
                 ausrmulti ausrancil \
                 ts1 flux \
                 ousrmulti ousrancil \
                 genanc_config genanc \
                 astart ostart"

# Just in case default values are not set

if {! [info exists cursor]} {
   set cursor left_ptr
}
if {! [info exists font]} {
#   set font -dt-interface user-medium-r-normal-s*-*-*-*-*-*-*-*-*
   set font -b&h-lucidatypewriter-medium-r-*-*-12-120-*-*-*-*-iso8859-1
#   set font {{bitstream vera sans mono} 10}
}
if {! [info exists bold_font]} {
   set bold_font -b&h-lucidatypewriter-bold-r-*-*-12-120-*-*-*-*-iso8859-1
}
if {! [info exists bold_font_large]} {
   set bold_font_large -b&h-lucidatypewriter-bold-r-*-*-14-120-*-*-*-*-iso8859-1
}
if {! [info exists leftbox_font]} {
   set leftbox_font {Helvetica 12 bold}
}
if {! [info exists scrollbarwidth]} {
   set scrollbarwidth 11
}
if {! [info exists foregroundcolour]} {
   set foregroundcolour black
}
if {! [info exists activeforegroundcolour]} { 
   set activeforegroundcolour black
}
if {! [info exists selectforegroundcolour]} { 
   set selectforegroundcolour black
}
if {! [info exists backgroundcolour]} {
   set backgroundcolour wheat
}
if {! [info exists activebackgroundcolour]} {
   set activebackgroundcolour #efefef
}
if {! [info exists selectbackgroundcolour]} {
   set selectbackgroundcolour #bfdfff
}
if {! [info exists disablecolour]} {
   set disablecolour #a3a3a3
}

# Source command file if any

if {[file exists ~/.xancilrc]} {source ~/.xancilrc}

# Source various functions

source [file join $xancil_dir "functions.tcl"]

# Setup xancil defaults

setupdefaults

# Read any command line arguments

readarg $argv

# Open xancil mk database of field names

if {! [catch {package require Mk4tcl}]} {
   #puts "using package Mk4tcl"
} elseif {! [catch {package require mklite}]} {
   #puts "using package mklite"
   ::mklite::emulateMk4tcl
} else {
   puts "Error: no package available to read Metakit database"
   exit
}

# Next line doesn't work when mk file is inside starkit so use alternative

#::mk::file open db [file join $xancil_dir xancildb.mk] -readonly
::mk::file open db
set fd [open [file join $xancil_dir xancildb.mk]]
::mk::file load db $fd
close $fd

# Define views

::mk::view layout db.stashmaster_405 {model:I section:I item:I name ppcode:I \
                                      levtype:I gridtype:I datatype:I masktype:I}
::mk::view layout db.stashmaster_505 {model:I section:I item:I name ppcode:I \
                                      levtype:I gridtype:I datatype:I masktype:I}
::mk::view layout db.stashmaster_601 {model:I section:I item:I name ppcode:I \
                                      levtype:I gridtype:I datatype:I masktype:I}
::mk::view layout db.stashmaster_701 {model:I section:I item:I name ppcode:I \
                                      levtype:I gridtype:I datatype:I masktype:I}
::mk::view layout db.userstashmaster {model:I section:I item:I name ppcode:I \
                                      levtype:I gridtype:I datatype:I masktype:I}
::mk::view layout db.pp {ppcode:I ppname}
::mk::view layout db.standard_name {model:I section:I item:I standard_name}

# Check user stashmaster files from command line job file exists

chkuserstashmaster config(smfile)

# Add in any user defined stashmaster files

adduserstashmaster $config(smfile)

# Load auxillary Tcl packages

package require Tk
package require Tablelist

# For Tk versions >= 8.5 restore old UI behaviour

catch {tk::classic::restore}

# Specify main font and cursor

option add *font "$font"
option add *cursor "$cursor"

#if {[catch {::tk::pkgconfig get fontsystem} xft]} {set xft no-xft}
#puts "font system is $xft"
#puts "font families [font families]"
#puts "font $font is [font actual $font]"
#puts "bold_font $bold_font is [font actual $bold_font]"
#puts "bold_font_large $bold_font_large is [font actual $bold_font_large]"
#puts "$leftbox_font is [font actual $leftbox_font]"

# Colour scheme

#set tk_strictMotif 1

option add *foreground $foregroundcolour
option add *activeForeground $activeforegroundcolour
option add *selectForeground $selectforegroundcolour
option add *background $backgroundcolour
option add *activeBackground $activebackgroundcolour
option add *selectBackground $selectbackgroundcolour
option add *disabledForeground $disablecolour

option add *labelBackground $backgroundcolour
option add *labelActiveBackground $activebackgroundcolour
option add *labelForeground $foregroundcolour
option add *labelActiveForeground $activeforegroundcolour
option add *labelDisabledForeground $disablecolour

option add *Scrollbar*Width $scrollbarwidth

#option add *Tablelist.borderWidth 3
#option add *Tablelist.selectBorderWidth 3

bind all <MouseWheel> "+wheelEvent %X %Y %D"
if {[string equal "unix" $tcl_platform(platform)]} { 
   bind all <4> "+wheelEvent %X %Y 100"
   bind all <5> "+wheelEvent %X %Y -100"
}

set screenheight [winfo screenheight .]
set screenwidth [winfo screenwidth .]

if {! [info exists ysize]} {
   #if {! [info exists yfac]} {set yfac 0.823}
   #set ysize [expr round($screenheight * $yfac) ]
   set ysize 843
}

if {! [info exists xsize]} {
   #if {! [info exists xyratio]} {set xyratio 1.25}
   #set xsize [expr $ysize * $xyratio]
   set xsize 1040
}

#puts "xsize = $xsize"
#puts "ysize = $ysize"

wm title . "Xancil Version $xancil_version ($versiondate)"
wm iconname . "Xancil"
wm minsize . 1 1
wm geometry . [expr round($xsize)]x[expr round($ysize)]

# Main window frames

frame $w
pack $w -side top -fill both -expand yes

frame $w1 -relief flat -borderwidth 6
frame $w2 -relief flat -borderwidth 6  
frame $w3 -relief sunken -borderwidth 2
frame $w4 -relief sunken -borderwidth 2
frame $w5 -relief flat -borderwidth 6

grid $w1 -in $w -row 0 -column 0 -sticky news
grid $w5 -in $w -row 1 -column 0 -columnspan 2 -sticky news
grid $w2 -in $w -row 2 -column 0 -columnspan 2 -sticky news

grid rowconfigure $w 0 -weight 4
grid rowconfigure $w 1 -weight 1 -minsize 143
grid rowconfigure $w 2 -weight 0
grid columnconfigure $w 0 -weight 1

grid $w3 -in $w1 -row 0 -column 0 -sticky news
grid $w4 -in $w1 -row 0 -column 1 -sticky news

grid rowconfigure $w1 0 -weight 1
grid columnconfigure $w1 0 -weight 1
grid columnconfigure $w1 1 -weight 8

grid propagate . off

# Left hand box

source [file join $xancil_dir "leftbox.tcl"]

# Right hand box

source [file join $xancil_dir "rightbox.tcl"]
source [file join $xancil_dir "gridconfig.tcl"]
source [file join $xancil_dir "ozone.tcl"]
source [file join $xancil_dir "sst.tcl"]
source [file join $xancil_dir "ice.tcl"]
source [file join $xancil_dir "orog.tcl"]
source [file join $xancil_dir "mask.tcl"]
source [file join $xancil_dir "lfrac.tcl"]
source [file join $xancil_dir "smow.tcl"]
source [file join $xancil_dir "slt.tcl"]
source [file join $xancil_dir "soil.tcl"]
source [file join $xancil_dir "veg.tcl"]
source [file join $xancil_dir "vegfrac.tcl"]
source [file join $xancil_dir "vegfunc.tcl"]
source [file join $xancil_dir "vegdist.tcl"]
source [file join $xancil_dir "ts1.tcl"]
source [file join $xancil_dir "flux.tcl"]
source [file join $xancil_dir "ausrmulti.tcl"]
source [file join $xancil_dir "ausrancil.tcl"]
source [file join $xancil_dir "ousrmulti.tcl"]
source [file join $xancil_dir "ousrancil.tcl"]
source [file join $xancil_dir "genanc_config.tcl"]
source [file join $xancil_dir "genanc.tcl"]
source [file join $xancil_dir "astart.tcl"]
source [file join $xancil_dir "ostart.tcl"]
source [file join $xancil_dir "combobox.tcl"]
source [file join $xancil_dir "scrolledframe.tcl"]

# Bottom buttons

set butwidth 15

frame .botbuttons -relief groove -borderwidth 4
pack .botbuttons -side top -fill x -pady 2m -in $w2
#button .help -width $butwidth -text "Help" -command bb_help
button .loadjob -width $butwidth -text "Load Job" -command bb_loadjob
button .savejob -width $butwidth -text "Save Job" -command bb_savejob
button .savejobas -width $butwidth -text "Save Job As" -command bb_savejobas
button .savenl -width $butwidth -text "Save Namelist" -command bb_savenl
button .savenlas -width $butwidth -text "Save Namelist As" -command bb_savenlas
button .create -width $butwidth -text "Create Anc. files" -command bb_createancil
button .quit -width $butwidth -text "Quit" -command "destroy ."
pack .loadjob .savejob .savejobas .savenl .savenlas .create .quit \
     -in .botbuttons -side left -expand yes

# text widget for output messages

label .infolabel -text "Output messages"
pack .infolabel -side top -fill x -in $w5

frame .field
pack .field -side bottom -fill both -expand yes -in $w5
text .info -wrap word -relief sunken -bd 2 -height 8  \
           -yscroll ".field.yscroll set" -state disabled
yscrollbar .field.yscroll .info
pack .info -side top -fill both -expand yes -in .field

# Make sure any optional entries are added

# Add entries for Generalised ancillary files

if {[info exists genanc_config(nancfile)] && $genanc_config(nancfile) > 0} {

   set genanc_config_xold 0
   set parent_group 1
   set parent_entry 4
   set group 5
   set entry 1
   genanc_config_addentry genanc_config(nancfile) "" ""
}

update
#puts "w1 height = [winfo height $w1] width = [winfo width $w1]"
#puts "w5 height = [winfo height $w5] width = [winfo width $w5]"
#puts "w2 height = [winfo height $w2] width = [winfo width $w2]"
#puts "w3 height = [winfo height $w3] width = [winfo width $w3]"
#puts "w4 height = [winfo height $w4] width = [winfo width $w4]"
