include makefile.inc

LIBDIR=-L$(HOME)/$(PREFIXDIR)/sun/lib
INCDIR=-I$(HOME)/include
BINDIR=$(HOME)/$(PREFIXDIR)/bin

LIBS=-lnetcdf

LIBDIR_SHLIB=-L$(HOME)/$(PREFIXDIR)/sun/lib
INCDIR_SHLIB=-I$(HOME)/include

LIBS_SHLIB=-lnetcdf -ltclstub8.4

F90C=f95
CC=cc

OPT=-g
DEFS= -DRTYPE=64 -DITYPE=32 -DPTYPE=32 -DOTYPE=64 -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64

CPPFLAGS= $(INCDIR) $(DEFS)
F90FLAGS= $(OPT) $(CPPFLAGS)
CFLAGS= $(OPT)
LDFLAGS= $(LIBDIR) $(LIBS)

CPPFLAGS_SHLIB= $(INCDIR_SHLIB) $(DEFS) -DUSE_TCL_STUBS
CFLAGS_SHLIB= $(OPT) $(CPPFLAGS_SHLIB) -KPIC
LDFLAGS_SHLIB= $(LIBDIR_SHLIB) $(LIBS_SHLIB) -G

.SUFFIXES : .f90 .F90 .po

.f90.o:
	$(F90C) $(F90FLAGS) -c $<
.F90.o:
	$(F90C) $(F90FLAGS) -c $<
.c.po:
	$(CC) $(CFLAGS_SHLIB) -c -o $*.tmp.o $<
	mv $*.tmp.o $@

$(EXEC): $(OBJ)
	$(F90C) $(OBJ) $(LDFLAGS) -o $(EXEC)

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
