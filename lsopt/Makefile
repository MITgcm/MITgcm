
#***********************************************************************
# Makefile for the ECCO off-line large scale optimization.
#
# started: Patrick Heimbach heimbach@mit.edu 19-Jun-2000
#
# prebuilt Fortran77 reference implementation BLAS libraries at
# http://www.netlib.org/blas/archives/
#
# Further high-performance BLAS libraries by Kazushige Goto at
# http://www.cs.utexas.edu/users/kgoto/
#
#***********************************************************************

# The optimization routines.
SRC = lsopt_top.F	\
	lsupdxx.F	\
	lsline.F	\
	hessupd.F	\
	cubic.F		\
	dgscale.F	\
	instore.F	\
	dostore.F	\
	outstore.F	\
	lswri.F

# Location of cpp preprocessor
# default is (Linux)
CPP             = cat $< | /lib/cpp -P -traditional
# on SUNOS
# CPP		= cat $< | /usr/ccs/lib/cpp

ARFLAGS     = rv
ECCOOPTLIB  = liblsopt_ecco.a

# Altix
#---------
#CPPFLAGS      = -DIS_DOUBLE
#FC              = ifort
#FFLAGS          = -mp -132 -r8 -i4 -w95 -W0 -WB -CB -fpe0 -traceback -convert big_endian -assume byterecl

# O3K
#---------
#CPPFLAGS	= -DIS_DOUBLE
#FC              = f77
#FFLAGS          = -extend_source -bytereclen -mips4 -r8 -static

# Linux
#---------
CPPFLAGS        = -DIS_DOUBLE
FC              = f77
FFLAGS          =

# SUN double precision.
#---------
#FFLAGS      = -u -r8 -e -g
#CPPFLAGS    = -I. -DIS_DOUBLE -DINTEGER_BYTE=4 -DONLINE

# Cray.
#---------
#FC          = f90
#FFLAGS      = -I. -e0 -N132 -O scalar3,vector3,task3
#CPPFLAGS    = -Wp"-DREAL_BYTE=8  -DINTEGER_BYTE=8"

# File removal.
RM		= rm -f

F77FILES = $(SRC:.F=.f)
OBJ      = $(SRC:.F=.o)

.SUFFIXES: .o .f .F

all: $(ECCOOPTLIB)

$(ECCOOPTLIB): $(OBJ) $(F77FILES)
	$(FC) -c $(FFLAGS) $(F77FILES)
	$(AR) $(ARFLAGS) $@ $?

depend:
	makedepend -o .f $(SRC)

# The normal chain of rules is (  .F - .f - .o  )
.F.f:
	$(CPP) $(CPPFLAGS) > $@
.f.o:
	$(FC) $(FFLAGS) -c $<

# Cleaning options.
clean:
	$(RM) *.o *.f
# DO NOT DELETE

lsupdxx.f: blas1.h
lsline.f: blas1.h
hessupd.f: blas1.h
dgscale.f: blas1.h
