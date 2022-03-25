Atmosphere-Ocean coupled set-up example "cpl_aim+ocn"
================================================================================
using simplified atmospheric physics (AIM), in realistic configuration (orography
& continent) with land and seaice component, on cubed-sphere (cs-32) grid.

### Overview:
Uses "in-house" MITgcm coupler<br>
(pkg/atm_ocn_coupler, pkg/compon_communic, pkg/atm_compon_interf, pkg/ocn_compon_interf )<br>
with each component config and customized src code in: code_cpl, code_atm, code_ocn ;<br>
and input parameter files in: input_cpl, input_atm, input_ocn.

- Atmos set-up and parameter is similar to "aim_5l_cs/" experiment
- Ocean set-up and parameter is similar to "global_ocean.cs32x15/" experiment

Requires the use of MPI; as default, use 1 proc for each component.

### Instructions:
To clean everything:
```
  ../../tools/run_cpl_test 0
```

Configure and compile, e.g., using gfortran optfile:
```
  ../../tools/run_cpl_test 1 -of ../../tools/build_options/linux_amd64_gfortran
```

To run primary setup, thermodynamic seaice only (no seaice dynamics):
```
  ../../tools/run_cpl_test 2
  ../../tools/run_cpl_test 3
```
Step 2 above copies input files and directories, step 3 runs the coupled model.

To run secondary test (with seaice dynamics as part of ocean component), using input parameter files in: input_cpl.icedyn, input_atm.icedyn, input_ocn.icedyn:
```
  ../../tools/run_cpl_test 2 icedyn
  ../../tools/run_cpl_test 3
```

Results are written in rank_{0,1,2} dir, for coupler, ocean and atmos comp. respectively

There is comparison output corresponding to primary set-up in the directory:<br>
 *results/atmSTDOUT.0000* & *results/ocnSTDOUT.0000*<br>
and for secondary test, in the same directory:<br>
 *results/atmSTDOUT.icedyn* & *results/ocnSTDOUT.icedyn*

Note:<br>
To check the results, monitor output could be compared to reference (in results/) using "run_cpl_test", step 4.<br>
For primary set-up:
```
  ../../tools/run_cpl_test 4
```
and for secondary test:
```
  ../../tools/run_cpl_test 4 icedyn
```
but this requires, in your path, a simple comparison script "comp_res"
(which is not provided here but could be found in:
 http://wwwcvs.mitgcm.org/viewvc/MITgcm/MITgcm_contrib/jmc_script/ ), along with some other files found in this archive.
