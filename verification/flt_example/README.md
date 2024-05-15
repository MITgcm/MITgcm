Example using floats (pkg/flt) based on verification/exp4
=========================================================
This experiment has been moved (PR #830) inside `exp4` and is now run there
as a secondary test ("exp4.with_flt") using `input.with_flt/`.

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
  `results/output.txt`

There is comparison output in the directory:
 `../verification/flt_example/results`

Comments:
The input data is `real*8`.

