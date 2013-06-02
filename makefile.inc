VERSION=0.59
EXEC=mkancil$(VERSION)
XANCIL=xancil$(VERSION)
SHLIB=xancil.so

all: $(EXEC) $(XANCIL)
mkancil: $(EXEC)
xancil: $(XANCIL)
shlib: $(SHLIB)

OBJ= module.o mkancil.o writeancil.o netcdf.o usermod.o \
     util_xancil.o ffio_xancil.o pptype.o \
     mask.o lfrac.o ozone.o smow.o slt.o soil.o veg.o vegfrac.o vegfunc.o \
     vegdist.o sstice.o orog.o ts1.o flux.o genanc.o astart.o ostart.o

POBJ= util_xancil.po xancil_Init.po xio_xancil.po

TCL= xancil_main.tcl functions.tcl leftbox.tcl rightbox.tcl gridconfig.tcl \
     ozone.tcl smow.tcl slt.tcl soil.tcl veg.tcl vegfrac.tcl vegfunc.tcl \
     vegdist.tcl sst.tcl ice.tcl orog.tcl mask.tcl lfrac.tcl ts1.tcl flux.tcl \
     genanc_config.tcl genanc.tcl astart.tcl ostart.tcl combobox.tcl

mkancil.o: mkancil.f90 module.F90
netcdf.o: netcdf.f90 module.F90
writeancil.o: writeancil.f90 module.F90
usermod.o: usermod.f90 module.F90
mask.o: mask.f90 module.F90
lfrac.o: lfrac.f90 module.F90
ozone.o: ozone.f90 module.F90
smow.o: smow.f90 module.F90
slt.o: slt.f90 module.F90
soil.o: soil.f90 module.F90
veg.o: veg.f90 module.F90
vegfrac.o: vegfrac.f90 module.F90
vegfunc.o: vegfunc.f90 module.F90
vegdist.o: vegdist.f90 module.F90
sstice.o: sstice.f90 module.F90
orog.o: orog.f90 module.F90
ts1.o: ts1.f90 module.F90
flux.o: flux.f90 module.F90
genanc.o: genanc.f90 module.F90
astart.o: astart.f90 module.F90
ostart.o: ostart.f90 module.F90
util_xancil.o: util_xancil.c util_xancil.h
ffio_conv3264.o: ffio_conv3264.c util_xancil.h
pptype.o: pptype.c util_xancil.h
util_xancil.po: util_xancil.c util_xancil.h
xio_xancil.po: xio_xancil.c xio_xancil.h
xancil_Init.po: xancil_Init.c util_xancil.h xio_xancil.h
