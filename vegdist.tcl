proc vegdistwin {win} {

   global vegdist config mask lfrac

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  ww1 is a hidden frame

   set ww1 $ww0.ww1
   frame $ww1

   title $ww0.title "Disturbed Vegetation Fraction"
   spacer $ww0.spacer1
   create_win2 $ww0.create $ww1 "Create Disturbed Vegetation Fraction ancillary file? " vegdist(create)
   spacer $ww1.spacer2
   get_filename1 $ww1.infile \
            "Enter input Disturbed Vegetation Fraction NetCDF file name: " \
             vegdist(file_in) nc vegdist_ncfile
   spacer $ww1.spacer3
   select_varname1 $ww1.varname1  "NetCDF Disturbed fraction of vegetation variable name: " vegdist(name) vegdist(file_in)
   spacer $ww1.spacer4
   set_filename $ww1.outfile "Enter output Disturbed Vegetation Fraction ancillary file name: " vegdist(file_out)
   spacer $ww1.spacer5
   howto_calc_lsm $ww1.mask vegdist(mask) "sea" \
      "$mask(use) || $mask(create)" "$lfrac(use) || $lfrac(create)"
   spacer $ww1.spacer6

#  Pack hidden frames if required

   if {$vegdist(create)} {pack $ww1 -expand yes -fill x -side bottom}
}

proc vegdist_ncfile {win ncfile} {

   global config vegdist
   
   add_ncfile $win $ncfile
   
   if {$win == ""} {return}
   
   set version $config(version)
   set iversion [expr {int(int($version)*100 + ($version-int($version))*10)}]

   set vegdist(name) [lindex [getvarname $ncfile 1 $iversion 219 \
                                                   $vegdist(name)] 0]
}
