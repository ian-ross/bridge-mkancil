proc vegwin {win} {

   global veg mask lfrac

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  ww1 is a hidden frame

   set ww1 $ww0.ww1
   frame $ww1

   title $ww0.title "Vegetation Parameters"
   spacer $ww0.spacer1
   create_win2 $ww0.create $ww1 "Create Vegetation Parameters ancillary file? " veg(create)
   spacer $ww1.spacer2
   get_filename1 $ww1.infile \
                 "Enter input Vegetation Parameters NetCDF file name: " \
                 veg(file_in) nc veg_ncfile
   spacer $ww1.spacer3
   select_varname1 $ww1.varname1  "NetCDF Root Depth variable name:                  " veg(rootdepthname) veg(file_in)
   select_varname1 $ww1.varname2  "NetCDF Snow free albedo variable name:            " veg(sfaname) veg(file_in)
   select_varname1 $ww1.varname3  "NetCDF Surface resistance to evap. variable name: " veg(surfresistname) veg(file_in)
   select_varname1 $ww1.varname4  "NetCDF Roughness length variable name:            " veg(z0name) veg(file_in)
   select_varname1 $ww1.varname5  "NetCDF Canopy capacity variable name:             " veg(cancapname) veg(file_in)
   select_varname1 $ww1.varname6  "NetCDF Vegetation fraction variable name:         " veg(vegfracname) veg(file_in)
   select_varname1 $ww1.varname7  "NetCDF Infiltration factor variable name:         " veg(infiltname) veg(file_in)
   select_varname1 $ww1.varname8  "NetCDF Deep snow albedo variable name:            " veg(dsaname) veg(file_in)
   select_varname1 $ww1.varname9  "NetCDF Leaf area index variable name:             " veg(lainame) veg(file_in)
   select_varname1 $ww1.varname10 "NetCDF Canopy height variable name:               " veg(canhtname) veg(file_in)
   spacer $ww1.spacer4
   set_filename $ww1.outfile "Enter output Vegetation Parameters ancillary file name: " veg(file_out)
   spacer $ww1.spacer5
   howto_calc_lsm $ww1.mask veg(mask) "sea" \
      "$mask(use) || $mask(create)" "$lfrac(use) || $lfrac(create)"
   spacer $ww1.spacer6

#  Pack hidden frames if required

   if {$veg(create)} {pack $ww1 -expand yes -fill x -side bottom}
}

proc veg_ncfile {win ncfile} {

   global config veg
   
   add_ncfile $win $ncfile
   
   if {$win == ""} {return}
   
   set version $config(version)
   set iversion [expr {int(int($version)*100 + ($version-int($version))*10)}]

   set veg(rootdepthname)  [lindex [getvarname $ncfile 1 $iversion 51 \
                                               $veg(rootdepthname)] 0]
   set veg(sfaname)        [lindex [getvarname $ncfile 1 $iversion 52 \
                                               $veg(sfaname)] 0]
   set veg(surfresistname) [lindex [getvarname $ncfile 1 $iversion 54 \
                                               $veg(surfresistname)] 0]
   set veg(z0name)         [lindex [getvarname $ncfile 1 $iversion 26 \
                                               $veg(z0name)] 0]
   set veg(cancapname)     [lindex [getvarname $ncfile 1 $iversion 55 \
                                               $veg(cancapname)] 0]
   set veg(vegfracname)    [lindex [getvarname $ncfile 1 $iversion 50 \
                                               $veg(vegfracname)] 0]
   set veg(infiltname)     [lindex [getvarname $ncfile 1 $iversion 56 \
                                               $veg(infiltname)] 0]
   set veg(dsaname)        [lindex [getvarname $ncfile 1 $iversion 53 \
                                               $veg(dsaname)] 0]
   set veg(lainame)        [lindex [getvarname $ncfile 1 $iversion 208 \
                                               $veg(lainame)] 0]
   set veg(canhtname)      [lindex [getvarname $ncfile 1 $iversion 209 \
                                               $veg(canhtname)] 0]
}
