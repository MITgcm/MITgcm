# $Header: /u/gcmpack/MITgcm/utils/knudsen2/Makefile,v 1.2 2002/08/07 16:55:52 mlosch Exp $

     FC = f77
F77OPTS = -r8

all: knudsen2 unesco

knudsen2: knudsen2.o
	$(FC) -o knudsen2 knudsen2.o
unesco: unesco.o
	$(FC) -o unesco unesco.o

.f.o:
	$(FC) $(F77OPTS) -c $<

clean:
	-\rm -f knudsen2 unesco *.o
