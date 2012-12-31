include makefile.inc

LIBDIR=-L/usr/lib
INCDIR=-I/usr/include
BINDIR=$(HOME)/$(PREFIXDIR)/bin

LIBS=-lnetcdff -lnetcdf

LIBDIR_SHLIB=
INCDIR_SHLIB=-I$(HOME)/include

LIBS_SHLIB=-lnetcdff -lnetcdf -ltclstub8.5

F90C=gfortran
CC=gcc

OPT=-O
DEFS= -DRTYPE=64 -DITYPE=32 -DPTYPE=64 -DOTYPE=64

CPPFLAGS= $(INCDIR) $(DEFS)
#F90FLAGS= -std=f2003 -Wall -pedantic -W $(OPT) $(CPPFLAGS)
F90FLAGS= $(OPT) $(CPPFLAGS) -I/usr/include
CFLAGS= $(OPT)
LDFLAGS= $(LIBDIR) $(LIBS)
LD=gfortran

CPPFLAGS_SHLIB= $(INCDIR_SHLIB) $(DEFS) -DUSE_TCL_STUBS
CFLAGS_SHLIB= $(OPT) $(CPPFLAGS_SHLIB) -fPIC
LDFLAGS_SHLIB= $(LIBDIR_SHLIB) $(LIBS_SHLIB) -shared

.SUFFIXES : .f90 .F90 .po

.f90.o:
	$(F90C) $(F90FLAGS) -c $<
.F90.o:
	$(F90C) $(F90FLAGS) -c $<
.c.po:
	$(CC) $(CFLAGS_SHLIB) -c -o $*.tmp.o $<
	mv $*.tmp.o $@

$(EXEC): $(OBJ)
	$(LD) $(OBJ) $(LDFLAGS) -o $(EXEC)

$(SHLIB): $(POBJ)
	$(CC) $(POBJ) $(LDFLAGS_SHLIB) -o $(SHLIB)

$(XANCIL): mkstarpack $(TCL) $(SHLIB)
	./mkstarpack $(XANCIL) $(SHLIB)

bin: $(EXEC) $(XANCIL)
	cp $(EXEC) $(XANCIL) $(BINDIR)

mkancil:
	@

xancil:
	@

clean:
	rm -rf *.o *.po *.mod $(EXEC) $(XANCIL) $(SHLIB) xancil.vfs $(XANCIL).kit tclkitcopy
