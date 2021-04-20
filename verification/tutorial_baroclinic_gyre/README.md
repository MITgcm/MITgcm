Tutorial Example: "Baroclinic gyre"
(Baroclinic Ocean Gyre In Spherical Coordinates)
============================================================
(formerly "exp1" verification ;
 also "baroclinic_gyre_on_a_sphere" in release.1 branch)

Configure and compile the code:
```
  cd build
  ../../../tools/genmake2 -mods ../code [-of my_platform_optionFile]
  make depend
  make
  cd ..
```

To run:
```
  cd run
  ln -s ../input/* .
  ../build/mitgcmuv > output.txt
```

There is comparison output in the directory:
  results/output.txt

Comments:
  The input data is real*4 and generated using the MATLAB script
  gendata.m.
