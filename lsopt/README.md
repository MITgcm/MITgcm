This directory contains code to generate the libary `lsopt_ecco` need
for the optimization routine "optim.x" in `../optim`. Please refer to
rudimentary instructions in `../optim/README.md`.

The following are instruction how the different precompiled
BLAS-libraries in the directory have been obtained. If your compiling
environment includes the BLAS-library, you do not need to worry about
this.

> Obtaining optimized BLAS routines for HPC platforms
> (e.g. O3K, Altix, Opteron, ...)
> heimbach@mit.edu, 9-Sep-2004
>
> 1.
> Go to
> http://www.cs.utexas.edu/users/kgoto/
> which has a set of high performance BLAS libraries for
> all kinds of platforms.
>
> 2.
> Download (e.g. for Opteron)
> http://www.cs.utexas.edu/users/kgoto/libraries/libgoto_opt64p-r0.94-2.so.gz
> which seems to be the suitable one for your Opteron 64 bit multi-threaded
> and put them in a directory, e.g. ~/mylib/
>
> 3.
> As per website instruction,
> also need the file  xerbla.f
> http://www.cs.utexas.edu/users/kgoto/libraries/xerbla.f
> Download it into the MITgcm/optim/ directory as
> xerbla.F (note capital F)
> (maybe cleaner to generate an object file and keep it in ~/mylib/ )
>
> 4.
> in .bashrc added line
> export LD_LIBRARY_PATH=~/mylib
> (consistent with the directory where I keep the library)
>
> 5.
> in MITgcm/optim/
> edit the Makefile as follows
> * add xerbla.F to             $SRC
> * add -L~/mylib/              $LIBDIRS
> * add -lgoto_opt64-r0.94-2    $LIBS
> and ignore
> -lblas1
>
> That works.

```
Some BLAS    libraries provided in lsopt/ are
SGI O3K:     libblas1.a_IRIX64.gz
SUN:         libblas1.a_SUN.gz
SGI Altix:   libgoto_it2-r0.95.so.gz
PC Pentium4: libgoto_p4_512-r0.94.so.gz
```
