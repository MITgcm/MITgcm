Tutorial Example: "Reentrant channel"
(Southern Ocean Reentrant Channel Example)
==========================================

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
  ln -s ../build/mitgcmuv .
  ./mitgcmuv > output.txt
  cd ..
```

There is comparison output in the directory:
results/output.txt

Comments:
  The input data is real*4 and generated using the MATLAB script gendata_50km.m.
