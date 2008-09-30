
#***********************************************************************
# Makefile for the ECCO off-line large scale optimization.
#
# started: Patrick Heimbach heimbach@mit.edu 19-Jun-2000
#
# changed: 
#
#***********************************************************************

# The optimization routines.
SRC		=	optim_main.F			\
			optim_sub.F			\
			optim_numbmod.F			\
			optim_initmod.F			\
			optim_readdata.F		\
			optim_writedata.F		\
			optim_write_control.F		\
			xerbla.F                        \
			simul.F

EXEDIR     = ./

# Location of cpp preprocessor
# default is (Linux)
CPP             = cat $< | /lib/cpp -P -traditional
# on SUNOS
# CPP		= cat $< | /usr/ccs/lib/cpp

INCLUDEDIRS     = -I.				\
		  -I../verification/natl_box_adjoint/build/

LIBDIRS         = -L.				\
                  -L../lsopt/

LIBS            = -llsopt_ecco			\
		  -lblas1

# OPTMODE is either equal to OFFLINE or ONLINE
OPTMODE         = OFFLINE
EXECUTABLE      = $(EXEDIR)optim.x

# The cpp flags.
CPPFLAGS = -DREAL_BYTE=4		\
	-DMAX_INDEPEND=1000000		\
	-D_RL='double precision'	\
	-D$(OPTMODE)

#                -DMAX_INDEPEND=293570968        \
# FORTRAN compiler and its flags.
# default is (Linux)
FC              = f77

# ifort compilers
#FC              = ifort
#FFLAGS          = -mp -132 -r8 -i4 -w95 -W0 -WB -CB -fpe0 -traceback -convert big_endian -assume byterecl

# SGI o3k IRIX64
#FC              = f77
#FFLAGS          = -extend_source -bytereclen -mips4 -r8 -static

# AER cluster
#FC		= /opt/mpich/bin/mpif77
#FFLAGS		= -byteswapio -r8 -Mnodclchk -Mextend

# File removal.
RM		= rm -f

SMALLF      = $(SRC:.F=.f)
OBJECTS     = $(SRC:.F=.o)

.SUFFIXES: .o .f .F

all: $(EXECUTABLE)
$(EXECUTABLE): $(OBJECTS)
	$(FC) -o $@ $(FFLAGS) $(OBJECTS) $(LIBDIRS) $(LIBS)

depend:
	makedepend -o .f $(INCLUDEDIRS) $(SRC)

# The normal chain of rules is (  .F - .f - .o  )
.F.f:
	$(CPP) $(CPPFLAGS) $(INCLUDEDIRS) > $@
xerbla.f: xerbla.F
	$(CPP) $(INCLUDEDIRS) > $@
.f.o:
	$(FC) $(FFLAGS) -c $<

# Cleaning options.
clean:
	$(RM) $(EXEDIR)optim.x *.o *.f

scratch:
	$(RM) OPWARMD OPWARMI $(EXEDIR)optim.x $(OBJ)

# DO NOT DELETE

