
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
			simul.F

OBJ		= optim_main.o			\
                  optim_sub.o			\
                  optim_numbmod.o		\
                  optim_initmod.o		\
                  optim_readdata.o		\
                  optim_writedata.o		\
                  optim_write_control.o		\
                  simul.o

EXEDIR     = ./

# Location of library for optimization.
# on SUNOS
# CPP		= cat $< | /usr/ccs/lib/cpp
# on linux
CPP		= cat $< | /lib/cpp
INCLUDEDIRS     = -I.				\
		  -I../bin/			\
                  -I../eesupp/inc/		\
                  -I../model/inc/ 		\
                  -I../diags/inc/ 		\
                  -I../pkg/ecco/ 		\
                  -I../pkg/ctrl/

LIBDIRS         = -L.				\
                  -L../lsopt/			\
		  -L/opt/SUNWspro/SC4.2/lib/v7

LIBS            = -llsopt_ecco			\
		  -lblas1

# OPTMODE is either equal to OFFLINE or ONLINE
OPTMODE         = OFFLINE
EXECUTABLE      = $(EXEDIR)optim.x

# The cpp flags.
CPPFLAGS	= -DREAL_BYTE=8			\
                  -DMAX_INDEPEND=29804		\
                  -D_RL='double precision'	\
                  -D$(OPTMODE)

# FORTRAN compiler and its flags.
# for SUN
#FC		= f77
#FFLAGS		= -C
# for Linux
FC              = g77
FFLAGS          = -Wimplicit -Wunused -Wuninitialized

# File removal.
RM		= rm -f

.SUFFIXES: .o .f .F

all: $(EXECUTABLE)
$(EXECUTABLE): $(OBJ)
	$(FC) -o $@ $(FFLAGS) $(OBJ) $(LIBDIRS) $(LIBS)
depend:
	makedepend -o .f $(INCLUDEDIRS) $(SRC)

# The normal chain of rules is (  .F - .f - .o  )
.F.f:
	$(CPP) $(CPPFLAGS) $(INCLUDEDIRS) > $@
.f.o:
	$(FC) $(FFLAGS) -c $<

# Cleaning options.
clean:
	$(RM) $(EXEDIR)optim.x $(OBJ)

scratch:
	$(RM) OPWARMD OPWARMI $(EXEDIR)optim.x $(OBJ)

# DO NOT DELETE

optim_main.f: blas1.h
optim_sub.f: ../bin/EEPARAMS.h ../bin/SIZE.h ../bin/ecco.h ../bin/ctrl.h
optim_sub.f: optim.h
optim_numbmod.f: ../bin/EEPARAMS.h ../bin/SIZE.h ../bin/ecco.h ../bin/ctrl.h
optim_numbmod.f: optim.h minimization.h
optim_initmod.f: ../bin/EEPARAMS.h ../bin/SIZE.h ../bin/ecco.h ../bin/ctrl.h
optim_initmod.f: optim.h
optim_readdata.f: ../bin/EEPARAMS.h ../bin/SIZE.h ../bin/ecco.h ../bin/ctrl.h
optim_readdata.f: optim.h minimization.h
optim_writedata.f: ../bin/EEPARAMS.h ../bin/SIZE.h ../bin/ecco.h
optim_writedata.f: ../bin/ctrl.h optim.h minimization.h
optim_write_control.f: ../bin/EEPARAMS.h ../bin/SIZE.h ../bin/ecco.h
optim_write_control.f: ../bin/ctrl.h optim.h
simul.f: ../bin/EEPARAMS.h ../bin/SIZE.h ../bin/ctrl.h
