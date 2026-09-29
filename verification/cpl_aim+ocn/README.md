Atmosphere-Ocean coupled set-up example "cpl_aim+ocn"
================================================================================
using simplified atmospheric physics (AIM), in realistic configuration (orography
& continent) with land and seaice component, on cubed-sphere (cs-32) grid.

### Overview:
Uses "in-house" MITgcm coupler<br>
(`pkg/atm_ocn_coupler`, `pkg/compon_communic`, `pkg/atm_compon_interf`,
`pkg/ocn_compon_interf`)<br>
with each component config and customized src code in: `code_cpl`, `code_atm`,
`code_ocn` ;<br>
and input parameter files in: `input_cpl`, `input_atm`, `input_ocn`.

- Atmos set-up and parameter is similar to `aim_5l_cs/` experiment
- Ocean set-up and parameter is similar to `global_ocean.cs32x15/` experiment

Requires the use of MPI; as default, use 1 proc for each component.

### Instructions:
To help getting started with this coupled set-up, the bash script
[../../tools/run_cpl_test](https://github.com/MITgcm/MITgcm/blob/master/tools/run_cpl_test)
(a short option summary is displayed when run without argument)
is provided and detailed instructions follow.

To clean everything:

    ../../tools/run_cpl_test 0

Configure and compile, e.g., using gfortran optfile:

    ../../tools/run_cpl_test 1 -of ../../tools/build_options/linux_amd64_gfortran

To run primary setup, thermodynamic seaice only (no seaice dynamics):

    ../../tools/run_cpl_test 2
    ../../tools/run_cpl_test 3

Step 2 above copies input files and directories, step 3 runs the coupled model.

To run secondary test (with seaice dynamics as part of ocean component), using
input parameter files in: `input_cpl.icedyn`, `input_atm.icedyn`, `input_ocn.icedyn`:

    ../../tools/run_cpl_test 2 icedyn
    ../../tools/run_cpl_test 3

Results are written in directories `rank_{0,1,2}`, for coupler, ocean and atmos
components, respectively.

There is comparison output corresponding to the primary set-up in the directory:<br>
`results/atmSTDOUT.0000` & `results/ocnSTDOUT.0000`<br>
and the secondary test, in the same directory:<br>
`results/atmSTDOUT.icedyn` & `results/ocnSTDOUT.icedyn`

##### Note:
To check the results, the monitor output can be compared to the reference (in `results/`)
using `run_cpl_test`, but this requires the additional comparison script `comp_res` and
the program `cmpnum.f`. Both can be found in GitHub repository
[MITgcm-contrib/jmc_scripts](https://github.com/MITgcm-contrib/jmc_scripts),
see README there. Once in place, step 4 performs the output comparison:<br>
For primary set-up:

    ../../tools/run_cpl_test 4

and for secondary test:

    ../../tools/run_cpl_test 4 icedyn
