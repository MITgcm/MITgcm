Example: Subtropical North Atlantic Subduction area
====================================================
- with KPP & shortwave heating
- no GMRedi

Configure and compile the code:
  cd build
  ../../../tools/genmake2 -mods ../code [-of my_platform_optionFile]
  make depend
  make
  cd ..

To run:
  cd run
  ln -s ../input/* .
  ../input/prepare_run
  ln -s ../build/mitgcmuv .
  ./mitgcmuv > output.txt
  cd ..

There is comparison output in the directory:
  results/output.txt

----------------------------------------------------
A FORTRAN program provides a verification procedure.
It is based on the boundary layer depth and will either issue:
'North Atlantic test passed.  KPPhbl file is unchanged.'
or:
'North Atlantic test failed.  KPPhbl file has changed.'

  cd run
  ln -sf ../output/KPPhbl.001.001.data KPPhbl_orig
  f77 ../results/comp_orig.F -o comp_orig
  comp_orig

N.B.: On ORIGIN 2000 use f77 -bytereclen

A matlab script, matlab/comp_output.m, creates plots for
reference (c32) and new surface temperature, boundary layer depth,
meridional section of tracer diffusivities, and
corresponding differences.

Comments:
The input data is real*4.
