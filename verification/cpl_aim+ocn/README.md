Atmosphere-Ocean coupled set-up example "cpl_aim+ocn" using simplified
 atmospheric physics (AIM), in realistic configuration (orography & continent)
 with Land and Sea-ice component, on Cubed-Sphere (cs-32) grid.
================================================================================

Used "in-house" MITgcm coupler (pkg/atm_ocn_coupler, pkg/compon_communic,
 pkg/atm_compon_interf, pkg/ocn_compon_interf ) with each component config and
customized src code in: code_cpl, code_atm, code_ocn ;
and input parameter files in: input_cpl, input_atm, input_ocn.

Atmos set-up and parameter is similar to "aim_5l_cs/" experiment
Ocean set-up and parameter is similar to "global_ocean_cs32x15/" experiment

Requires the use of MPI ; default is using 1 procs for each component


To clean everything:
```
  ../../tools/run_cpl_test 0
```

Configure and compile, for e.g., using gfortran optfile:
```
  ../../tools/run_cpl_test 1 -of ../../tools/build_options/linux_amd64_gfortran
```

To run primary set-up (no seaice dynamics, only thermodynamics):
```
  /bin/rm -rf rank_{0,1,2}
  ../../tools/run_cpl_test 2
  ../../tools/run_cpl_test 3
```

To run secondary test (with seaice dynamics in ocean component),
using input parameter files in: input_cpl.icedyn, input_atm.icedyn, input_ocn.icedyn
```
  /bin/rm -rf rank_{0,1,2}
  ../../tools/run_cpl_test 2 icedyn
  ../../tools/run_cpl_test 3
```

Results are written in rank_{0,1,2} dir, for coupler, ocean and atmos comp. respectively

There is comparison output corresponding to primary set-up  in the directory:
  results/atmSTDOUT.0000
  results/ocnSTDOUT.0000
and for secondary test, in the same directory:
  results/atmSTDOUT.icedyn
  results/ocnSTDOUT.icedyn

Note/Comments:
To check the results, monitor output could be compared using "step 4",
for primary set-up:
  ../../tools/run_cpl_test 4
and for secondary test:
  ../../tools/run_cpl_test 4 icedyn
but this requires to have, in the path, a simple comparaison script "comp_res" (which is 
not provided here but could be found in: 
 http://wwwcvs.mitgcm.org/viewvc/MITgcm/MITgcm_contrib/jmc_script/ ).
