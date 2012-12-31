proc vegfracwin {win} {

   global vegfrac mask lfrac

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  ww1 is a hidden frame

   set ww1 $ww0.ww1
   frame $ww1

   title $ww0.title "Vegetation Fractions"
   spacer $ww0.spacer1
   create_win2 $ww0.create $ww1 "Create Vegetation Fractions ancillary file? " vegfrac(create)
   spacer $ww1.spacer2
   get_filename1 $ww1.infile \
                 "Enter input Vegetation Fractions NetCDF file name: " \
                 vegfrac(file_in) nc vegfrac_ncfile
   spacer $ww1.spacer3
   select_varname1 $ww1.varname1  "NetCDF Fractions of surface types variable name: " vegfrac(name) vegfrac(file_in)
   spacer $ww1.spacer4
   set_var $ww1.surftype "Number of surface types: " vegfrac(surftypes) 5
   spacer $ww1.spacer5
   set_filename $ww1.outfile "Enter output Vegetation Fractions ancillary file name: " vegfrac(file_out)
   spacer $ww1.spacer6
   howto_calc_lsm $ww1.mask vegfrac(mask) "sea" \
      "$mask(use) || $mask(create)" "$lfrac(use) || $lfrac(create)"
   spacer $ww1.spacer7

#  Pack hidden frames if required

   if {$vegfrac(create)} {pack $ww1 -expand yes -fill x -side bottom}
}

proc vegfrac_ncfile {win ncfile} {

   global config vegfrac
   
   add_ncfile $win $ncfile
   
   if {$win == ""} {return}
   
   set version $config(version)
   set iversion [expr {int(int($version)*100 + ($version-int($version))*10)}]

   set vegfrac(name) [lindex [getvarname $ncfile 1 $iversion 216 \
                                                   $vegfrac(name)] 0]
}
