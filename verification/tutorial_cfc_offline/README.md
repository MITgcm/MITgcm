Tutorial Example: "Offline CFC Experiments"
============================================================
(formerly "cfc_offline" verification )

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
  ./prepare_run
  ../build/mitgcmuv > output.txt
```

There is comparison output in the directory:
  results/output.txt

Comments:
  The input data is real*4

And to run the simpler (no CFC) offline test:
```
  cd run ; rm -f *
  ln -s ../input_tutorial/* .
  ln -s ../input/* .
  ./prepare_run
  ../build/mitgcmuv > output.tut
```
