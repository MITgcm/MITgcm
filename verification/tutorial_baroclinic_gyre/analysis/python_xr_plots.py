# import python modules
import numpy as np
import matplotlib.pyplot as plt
import xarray as xr

# python package xarray is used to load netcdf files http://xarray.pydata.org
# xarray loads netcdf data and metadata into xarray structure Dataset (containing data in xarray DataArray)
# which includes information about dimensions, coordinates etc.
# Xarray simplifies some simple tasks, e.g. it is not necessary to provide axes arguments when plotting,
# but also more complex tasks such as broadcasting data to fill missing dimensions in DataArray arithmetic, for example.


###########   load grid data   ########### 

# Load grid variables (these are internal MITgcm source code variables; if using standard
# binary output instead of netcdf, these are dumped to individual files, e.g. 'RC.data' etc.)
# assumes output in separated tiles has been concatenated into global files (used script utils/python/MITgcmutils/scripts/gluemncbig)
# and moved into the top directory (see MITgcm user manual section 4.2.4.1) 

# Using a spherical polar grid, all X,Y variables are in longitude, latitude coordinates
# vertical grid provided in meters, area in m^2

grid = xr.open_dataset('grid.nc')

# We will be using the following fields from the grid file:
# 1-D fields
#   RC:     vertical grid, cell center locations (this is coordinate Z)
#   drF:    spacing of grid cells in the vertical (separation between cell top and bottoms)
#   X:      1-D version of XC data
#   Y:      1-D version of YC data
#   Xp1:    x-location of gridcell lower left corner (1-D version of XG w/extra end point)
#   Yp1:    y-location of gridcell lower left corner (1-D version of YG w/extra end point)
# 2-D fields (y,x)
#   XC:     x-location of gridcell centers
#   YC:     y-location of gridcell centers
#   dyG:    grid spacing in y-dimension (i.e. separation between corners)
#   rA:     surface area of gridcells
# 3-D fields (z,y,x)
#   HFacC:  vertical fraction of cell which is ocean


###########   load diagnostics   ########### 

# unit for temperature is degrees Celsius, velocity in m/s, surface height in m, heat flux in W/m^2
# see run output file available_diagnostics.log
   
# load statistical diagnostic output (set to monthly time-averaged output)
# only one output region is defined: global (the default)
dynStDiag = xr.open_dataset('dynStDiag.nc')
# includes diagnostics:
# TRELAX_ave:   (time, region, depth); region=0 (global), depth=0 (surface-only)
# THETA_lv_ave: (time, region, depth); region=0 (global)
# THETA_lv_std: (time, region, depth); region=0 (global)
   
# load 2-D and 3-D variable diagnostic output, annual mean data
surfDiag = xr.open_dataset('surfDiag.nc')
# TRELAX:       (time, depth, y, x); depth=0 (surface-only)
# ETAN:         (time, depth, y, x); depth=0 (surface-only)
dynDiag = xr.open_dataset('dynDiag.nc')
dynDiag = dynDiag.rename({'Zmd000015':'Z'})
# UVEL:         (time, depth, y, x); x dim. is 63, includes eastern edge
# VVEL:         (time, depth, y, x); y dim. is 63, includes northern edge
# THETA:        (time, depth, y, x)
# Note the Z dimension here has a different name, we rename to the standard name 'Z'
# which will make life easier when we do some calculations using the raw data.
# This is because /pkg/diagnostics has an option to allow specific levels of a 3-D variable
# to be output. If 'levels' are selected, the following additional statement is needed:
#  dynDiag['Z'] = grid['Z'][dynDiag.diag_levels.astype(int)-1]


###########   plot diagnostics   ########### 

# figure 4.6 - time series of global mean TRELAX and THETA by level
#
# plot THETA at MITgcm k levels 1,5,15 (SST, -305 m, -1705 m)
klev1=0; klev2= 4; klev3=14;
plt.figure(figsize=(16,10))
plt.subplot(221)
# MITgcm time units (dim='T') is in seconds, so create new coordinate for monthly time averages (mid-month)
# and convert into years. Note that the MITgcm time array are times at the ENDs of the time-avg periods.   
Tmon = (dynStDiag['T']/(86400*360)-1/24).assign_attrs(units='years')
# and we tell xarray to plot using this new abscissa instead of T in seconds
# xarray is smart enough to ignore the singleton dimensions for region and depth
dynStDiag.TRELAX_ave.assign_coords(T=Tmon).plot(color='b',linewidth=4);plt.grid('both') 
plt.title('a) Net Heat Flux into Ocean (TRELAX_ave)')
plt.ylim(-400,0)
# Alternatively, a global mean area-weighted TRELAX (annual mean) could be computed as follows,
# using HfacC[0,:,:] i.e. HfacC in the surface layer, as a land-ocean mask, using xarray .where()
tot_ocean_area= (grid.rA.where(grid.HFacC[0,:,:]!=0)).sum(dim='Y').sum(dim='X') # compute total surface area of ocean points
# broadcasting with xarray is more flexible than basic numpy because it matches axis name (not dependent on axis position)
# grid area is broadcast across the time dimension below
TRELAX_ave_ann= (surfDiag.TRELAX*grid.rA.where(grid.HFacC[0,:,:]!=0)).sum(dim='Y').sum(dim='X')/tot_ocean_area
Tann=(TRELAX_ave_ann['T']/(86400*360)-0.5).assign_attrs(units='years') # again creating a new coordinate for annual mean time avg
TRELAX_ave_ann.assign_coords(T=Tann).plot(color='m',linewidth=4,linestyle='dashed')
plt.subplot(223)
dynStDiag.THETA_lv_ave.isel(Zmd000015=klev1).assign_coords(T=Tmon).plot(color='c',label='T_surf',linewidth=4);plt.grid('both')
dynStDiag.THETA_lv_ave.isel(Zmd000015=klev2).assign_coords(T=Tmon).plot(color='g',label='T_300m',linewidth=4)
dynStDiag.THETA_lv_ave.isel(Zmd000015=klev3).assign_coords(T=Tmon).plot(color='r',label='T_abyss',linewidth=4)
plt.title('b) Mean Potential Temp. by Level (THETA_lv_avg)')
plt.ylim(0,30);plt.legend()
plt.subplot(224)
dynStDiag.THETA_lv_std.isel(Zmd000015=klev1).assign_coords(T=Tmon).plot(color='c',label='T_surf',linewidth=4);plt.grid('both')
dynStDiag.THETA_lv_std.isel(Zmd000015=klev2).assign_coords(T=Tmon).plot(color='g',label='T_300m',linewidth=4)
dynStDiag.THETA_lv_std.isel(Zmd000015=klev3).assign_coords(T=Tmon).plot(color='r',label='T_abyss',linewidth=4)
plt.title('c) Std. Dev. Potential Temp. by Level (THETA_lv_std)')
plt.ylim(0,8);plt.legend()
plt.show()

# figure 4.7 - 2-D plot of TRELAX and contours of free surface height (ETAN) at t=100 yrs.
#
plt.figure(figsize=(10,8)) 
surfDiag.TRELAX[-1,0,:,:].plot(cmap='RdBu_r', vmin=-250, vmax=250);plt.xlim(0,60);plt.ylim(15,75);
surfDiag.ETAN[-1,0,:,:].where(grid.HFacC[0,:,:]!=0).plot.contour(levels=np.arange(-.6,.7,.1), colors='k')
plt.title('Free surface height (contours, CI .1 m) and TRELAX (shading)')
plt.show()
# By default this uses a plotting routine similar to pcolormesh to plot TRELAX
# Note that xarray uses the coordinates of TRELAX automatically from the netcdf metadata! With axis labels!
# Even though the plotted TRELAX DataArray has coordinates Y,X (cell centers) it is plotted correctly 
# (effectively shading cells between Yp1,Xp1 locations, which are the lower left corners of grid cells, i.e. YG,XG).
# Also note we mask the land values when contouring the free surface height
# Alternatively, one could make a smoother color plot using contourf: 
# surfDiag.TRELAX[-1,0,:,:].plot.contourf(cmap='RdBu_r', levels=np.linspace(-250,250,1000))

# figure 4.8 - barotropic streamfunction at t=100 yrs. (w/overlaid labeled contours)
#
plt.figure(figsize=(10,8))
ubt = (dynDiag.UVEL*grid.drF).sum(dim='Z') # here is an example where the xarray broadcasting is superior
psi = (-ubt*grid.dyG).cumsum(dim='Y')/1.e6
psi.pad(Y=1,constant_values=0)[-1,:-1,:].assign_coords(Y=grid.Yp1.values).plot.contourf(levels=np.linspace(-30, 30, 13), cmap='RdYlBu_r')
cs=psi.pad(Y=1,constant_values=0)[-1,:-1,:].assign_coords(Y=grid.Yp1.values).plot.contour(levels=np.arange(-35,40,5), colors='k')
plt.clabel(cs, fmt = '%.0f')
plt.xlim(0,60);plt.ylim(15,75)
plt.title('Barotropic Streamfunction (Sv)');
plt.show()
# Note psi is computed and plotted at the grid cell corners and is dimensioned 62x63
# cumsum is done in y-direction; and, we have a row of wall at southern boundary
# (i.e. no reentrant flow from north).  We need to add a row of zeros to specify psi(j=0).
# xarray .pad() adds a row at the top and bottom, we just need the bottom row (j=0) padding the computed psi array.
# Finally, since psi is computed at grid cell corners, we need to re-assign Yp1 coordinate to psi.
# (xxx.values extracts the numpy-array data from xarray DataArray xxx) 

# figure 4.9 - potential temperature at 220m depth (MITgcm k=4) and xz slice at 28.5N (MITgcm j=15) at t=100 yrs.
#
klev=3; jloc=14;
plt.figure(figsize=(16,6))
plt.subplot(121)
# again we do a pcolor plot for this plan view, this time showing THETA(k=3)
dynDiag.THETA[-1,klev,:,:].plot(cmap='coolwarm',vmin=0,vmax=30 )
# and overlay contours w/masked out boundary/land cells
dynDiag.THETA[-1,klev,:,:].where(grid.HFacC[klev,:,:]!=0).plot.contour(levels=np.linspace(0,30,16), colors='k')
plt.xlim(0,60);plt.ylim(15,75);
plt.title('THETA at 220 m depth')

plt.subplot(122)
# Here, our limited vertical resolution makes for an ugly pcolor plot, so we'll shade using contour instead.
# also masking out land cells at the boundary, which results in slight white space at the domain edges.
dynDiag.THETA[-1,:,jloc,:].where(grid.HFacC[:,jloc,:]!=0).plot.contourf(levels=np.arange(0,30,.2),cmap='coolwarm')
dynDiag.THETA[-1,:,jloc,:].where(grid.HFacC[:,jloc,:]!=0).plot.contour(levels=np.arange(0,30,2),colors='k')
plt.title('THETA at 28.5 N');plt.xlim(0,60);plt.ylim(-1800,0)
plt.show()
# One approach to avoiding the white space at boundaries/top/bottom (this occurs because contour plot
# uses data at cell centers, with values masked/undefined beyond the cell centers toward boundaries)
# is to copy neighboring tracer values to land cells prior to contouring (and then do not mask the data, but set xlim and ylim),
# and augment a row of z=0 data at the ocean surface and a row at the ocean bottom.
# To use pcolor instead: dynDiag.THETA[-1,:,14,:].assign_coords(Z=grid.RC.values).plot(cmap='coolwarm')
