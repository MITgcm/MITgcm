# $Header: /u/gcmpack/MITgcm/utils/knudsen2/Makefile,v 1.1 1998/05/28 16:26:56 adcroft Exp $

     FC = f77
F77OPTS = -r8

knudsen2: knudsen2.o
	$(FC) -o knudsen2 knudsen2.o

.f.o:
	$(FC) $(F77OPTS) -c $<

clean:
	-rm knudsen2 *.o
