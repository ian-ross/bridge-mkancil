proc vegfuncwin {win} {

   global vegfunc mask lfrac

   set ww0 $win.ww0
   frame $ww0 -bd 2
   pack $ww0 -expand yes -fill x -side top

#  ww1 ww2 are hidden frames

   set ww1 $ww0.ww1
   frame $ww1
   set ww2 $ww1.setdate
   frame $ww2

   title $ww0.title "Vegetation Functional Types"
   spacer $ww0.spacer1
   create_win2 $ww0.create $ww1 "Create Vegetation Functional Types ancillary file? " vegfunc(create)
   spacer $ww1.spacer2
   get_filename1 $ww1.infile \
                 "Enter input Vegetation Functional Types NetCDF file name: " \
                 vegfunc(file_in) nc vegfunc_ncfile
   spacer $ww1.spacer3
   select_varname1 $ww1.varname1  "NetCDF Leaf area index of plant func. types variable name: " vegfunc(lainame) vegfunc(file_in)
   select_varname1 $ww1.varname2  "NetCDF Canopy height of plant func. types variable name:   " vegfunc(canhtname) vegfunc(file_in)
   select_varname1 $ww1.varname3  "NetCDF Canopy conductance variable name:                   " vegfunc(cancondname) vegfunc(file_in)
   spacer $ww1.spacer4
   set_var $ww1.functype "Number of plant functional types: " vegfunc(functypes) 5
   spacer $ww1.spacer5
   set_filename $ww1.outfile "Enter output Vegetation Functional Types ancillary file name: " vegfunc(file_out)
   spacer $ww1.spacer6
   set_boolvar2 $ww1.per "Is Vegetation Functional Types \nancillary data periodic in time? " vegfunc(periodic)
   spacer $ww1.spacer7
   howto_calc_lsm $ww1.mask vegfunc(mask) "sea" \
      "$mask(use) || $mask(create)" "$lfrac(use) || $lfrac(create)"
   spacer $ww1.spacer8
   select_window $ww1.date1 $ww2 \
      "Use dates from NetCDF file                               " \
      "Specify Vegetation Functional Types ancillary file dates " vegfunc(timeusage1)
   spacer $ww1.spacer9
   set_date $ww2.date2 "Vegetation Functional Types" vegfunc

#  Pack hidden frames if required

   if {$vegfunc(create)} {pack $ww1 -expand yes -fill x -side bottom}
   if {$vegfunc(timeusage1) == 1} {pack $ww2 -expand yes -fill x -side top}
}

proc vegfunc_ncfile {win ncfile} {

   global config vegfunc
   
   add_ncfile $win $ncfile
   
   if {$win == ""} {return}
   
   set version $config(version)
   set iversion [expr {int(int($version)*100 + ($version-int($version))*10)}]

   set vegfunc(lainame)     [lindex [getvarname $ncfile 1 $iversion 217 \
                                                $vegfunc(lainame)] 0]
   set vegfunc(canhtname)   [lindex [getvarname $ncfile 1 $iversion 218 \
                                                $vegfunc(canhtname)] 0]
   set vegfunc(cancondname) [lindex [getvarname $ncfile 1 $iversion 213 \
                                                $vegfunc(cancondname)] 0]
}
