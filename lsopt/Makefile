
#-----------------------------------------------------------------
#	L A R G E   S C A L E   O P T I M I Z A T I O N
#-----------------------------------------------------------------
#
# Version   : lsoptv1  2.1.0
#
# Author    : Ralf Giering                 22-Mar-1995
# Modified  : Michael Peters               28-Aug-1995
# Rewritten : Patrick Heimbach (MIT/EAPS)  25-Feb-2000
# ECCO      : Christian Eckert (MIT/EAPS)  10-Mar-2000
#
#-----------------------------------------------------------------

SRC         = lsopt_top.F   lsupdxx.F     lsline.F   \
              hessupd.F     cubic.F       dgscale.F  \
              instore.F     dostore.F     outstore.F \
              lswri.F

ECCOSRC     = lsopt_top.F   lsupdxx.F     lsline.F   \
              hessupd.F     cubic.F       dgscale.F  \
              instore.F     dostore.F     outstore.F \
              lswri.F

# SUN double precision.
FFLAGS      = -u -r8 -e -g
CPPFLAGS    = -I. -DIS_DOUBLE -DINTEGER_BYTE=4 -DONLINE

# Note for MIT users:
# -------------------
#   LAPACK/BLAS routines can be obtained directly from e.g.
#   /opt/SUNWspro/SC4.2/lib/v7.
#   --> man lapack

# SUN single precision.
#FFLAGS      = -u 
#CPPFLAGS    = -I. -DREAL_BYTE=4  -DINTEGER_BYTE=4

# Cray.
#FC          = f90
#FFLAGS      = -I. -e0 -N132 -O scalar3,vector3,task3
#CPPFLAGS    = -Wp"-DREAL_BYTE=8  -DINTEGER_BYTE=8"


OBJECTS     = $(SRC:.F=.o) 
ECCOOBJECTS = $(ECCOSRC:.F=.o) 

OPTLIB      = liblsopt.a
ECCOOPTLIB  = liblsopt_ecco.a
LIB         = .

ARFLAGS     = rv
RM          = rm -f
CPP         = /lib/cpp
# RANLIB on SUN:
#RANLIB      = /usr/ccs/bin/ranlib
# RANLIB on linux:
RANLIB      = /usr/bin/ranlib

#-----------------------------------------------------------------
# Rules
#-----------------------------------------------------------------

# Generate the lsopt library.
all: $(OPTLIB)

$(OPTLIB): $(OBJECTS)
	$(AR) $(ARFLAGS) $@ $?
	$(RANLIB) $@
	chmod a+r $@

install: $(OPTLIB)
	cp $(OPTLIB) $(LIB)/$(OPTLIB)

# Generate the ECCO lsopt library.
ecco: $(ECCOOPTLIB)

$(ECCOOPTLIB): $(ECCOOBJECTS)
	$(AR) $(ARFLAGS) $@ $?
	$(RANLIB) $@
	chmod a+r $@

ecco_install: $(ECCOOPTLIB)
	cp $(OPTLIB) $(LIB)/$(OPTLIB)

# Generating object files.
.o:
	$(FC) -o $@ $(FFLAGS) $(CPPFLAGS) $(LDFLAGS) $< $(OBJECT) $(LDLIBS)

# File removal.
clean:
	$(RM) $(OBJECTS) $(ECCOOBJECTS)
	$(RM) *~

scratch: clean
	$(RM) $(OPTLIB)


