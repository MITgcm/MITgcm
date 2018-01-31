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
