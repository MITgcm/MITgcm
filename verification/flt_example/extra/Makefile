# Tested on faulks

FC = gfortran
LD = gfortran

#NETCDF = /usr/include/netcdf-3/
IPATH=-I/usr/include/netcdf-3/
LPATH=-L/usr/lib/netcdf-3/
LIBS=-lnetcdf
FFLAGS=-O2 -fconvert=big-endian $(IPATH)
LDFLAGS=-O2 $(LPATH) $(LIBS) 
 
%.o : %.F90
	$(FC) -c $(FFLAGS) $< 
 
all: cvfloat.o
	$(LD) -o cvfloat cvfloat.o $(LDFLAGS)
