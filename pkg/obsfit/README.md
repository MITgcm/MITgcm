# obsfit
`obsfit` package for MITgcm 


# Summary:
An alternative to the `profiles` package for model-observations comparisons. Given an observational dataset, `obsfit` samples the model during the run at the time and location of observations, calculates the cost (sum of weighted misfits), and produces a model-equivalent output file that is directly comparable to the input file. Observations do not need to be on a regular grid or a fixed set of depths, making it an efficient tool for sparse datasets (e.g., altimetry or tomography data). Observations can be made of multiple samples that are averaged or integrated spatially and/or temporally.


# How to use: 1) observations pre-processing

ObsFit input files are in netcdf format and must include the following fields:

- `obs_val` (observed value) 
- `obs_uncert` (uncertainty on the observed value) 
- `obs_YYYYMMDD` (observation start time [year,month,day])
- `obs_HHMMSS` (observation start time [hour,min,sec]) 
- `sample_type` (variable type: 1 for temperature, 2 for salinity, 3 / 4 for zonal / meridional velocity, or 5 for SSH) 
- `sample_lon` (longitude) 
- `sample_lat` (latitude) 
- `sample_depth` (depth) 

The following fields are optional: 

- obs_delt (the observation duration (default=0; negative for time integration)) 
- obs_np (the number of samples in the observation (default=1)) 
- sample_weight (weighing factor (default=1)) 

See `make_obsfit_example.m` for a matlab example 

In the simplest case, the number of samples per observation is 1; then obs_np = 1 (by default), `sample_weight = 1` (by default), and `sample_{type/x/y/z}` give the variable `type/longitude/latitude/depth` of the observation. If there are `{N}` observations, each field listed above is a vector of size `{1xN}`.

If desired, ObsFit allows for observations to be the weighted average of multiple samples that can differ in type and/or location. In that case, one must specify the number of samples that make the observation, as well as their relative weight. If there are {N} observations, obs* fields are vectors of size `{1xN}` and sample* fields are vectors of size $\sum_N$`(obs_np)`. If no sample weights are provided, it is assumed that all samples are weighed equally. 

Observations with a positive duration are averaged in time, whereas a negative `duration` is used to indicate time integration, and instantaneous observations have `duration=0`; if no `duration` is provided `duration=0` is assumed. 
 

# How to use: 2) compiling the code

1. Copy the folder <b>`obsfit`</b> into MITgcm/pkg
2. Copy the contents of <b>`code_folder`</b> into your local `code` or `code_ad` folder

Note that obsfit requires the cost package to be compiled (in `packages.conf`)
 

Follow the steps below to run an example on pleiades (or modify build options and build scripts for other machines):

#obtain MITgcm checkpoint 69a
```
git clone https://github.com/MITgcm/MITgcm.git 
cd MITgcm 
git checkout checkpoint69a
```

#obtain obsfit package code and example 
```
git clone https://github.com/averdy/obsfit_mitgcm.git 
cd obsfit_mitgcm 
mv obsfit ../pkg 
mv global_oce_biogeo_bling/* ../verification/global_oce_biogeo_bling/
cd ../verification/global_oce_biogeo_bling/ 
```

#compile executable 
```
cd build_fwd_obsfit 
mv pleiades_build_options ../../ 
./makescript_pleiades_fwd 
```

#compile adjoint 
```
cd ../build_ad_obsfit 
./makescript_pleiades_adj
```

# How to use: 3) model run

Observations input files are specified in data.obsfit (e.g. `global_oce_biogeo_bling/input_obsfit/data.obsfit`). Note that the file extension ".nc" should not be included. 

During the model run, model values at sampled locations are saved in tiled files. After the run, sampled values are read and averaged to calculate the model-equivalent for each observation. The results are written in a global netcdf file which is read during cost calculation. This should make the package compatible with multigrid adjoint runs.

Output files include two variables, `mod_val` and `mod_mask`. They are in the same format as the input files, thus `obs_val` and `mod_val` are directly comparable. The mask indicates missing model-equivalent values. 


# Notes:

- Contact me with any questions or comments: `averdy@ucsd.edu`
- ObsFit may not be the best name for this package; alternative suggestions are welcome!
- `obsfit_cost.F` can be very slow for large datasets. This is where tiled fields are read, combined, and written to a global file. There is probably a more efficient way to do this (help appreciated!)
- I/O `routines active_file_*` are adapted from `pkg/profiles`, which has a known problem with tangent linear runs (PR #873)
- Compatibility with LLC grids is in development


