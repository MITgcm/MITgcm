# import python modules
import numpy as np
import matplotlib.pyplot as plt
import xarray as xr

# python pkg xarray used to load netcdf files http://xarray.pydata.org
# xarray loads netcdf data and metadata into xarray structure Dataset
# (containing data in xarray DataArray) which includes information
# about dimensions, coordinates etc.
# Xarray simplifies some basic tasks, e.g. it is not necessary to
# provide axes arguments when plotting, but also more complex tasks
# such as broadcasting data to fill missing dimensions in DataArray
# arithmetic, for example.


###########   load grid data   ########### 

# Load grid variables; names correspond with MITgcm source code.
# (if using standard binary output instead of netcdf, these are dumped
# to individual files, e.g. 'RC.data' etc.)
# Assumes that output in multiple tiles has been concatenated into
# global files (used script utils/python/MITgcmutils/scripts/gluemncbig)
# and moved into the top directory;
# see MITgcm user manual section 4.2.4.1 

# Using a spherical polar grid, all X,Y variables in lon,lat coordinates
# vertical grid provided in meters, area in m^2

grid = xr.open_dataset('grid.nc')

# We will be using the following fields from the grid file:
# 1-D fields
#   RC:     vertical grid, cell center locations (this is coordinate Z)
#   drF:    vertical spacing of grid cells (thickness of cells)
#   X:      1-D version of XC data
#   Y:      1-D version of YC data
#   Xp1:    x-location of gridcell lower left corner
#   Yp1:    y-location of gridcell lower left corner
#   Xp1 and Yp1 are effectively 1-D versions of grid variables XG,YG with
#   an extra data value at the eastern and northern ends of the domain.
# 2-D fields (y,x)
#   XC:     x-location of gridcell centers
#   YC:     y-location of gridcell centers
#   dyG:    grid spacing in y-dim (i.e. separation between corners)
#   rA:     surface area of gridcells
# 3-D fields (z,y,x)
#   HFacC:  vertical fraction of cell which is ocean
# See MITgcm users manual section 2.11 for additional MITgcm grid info

# Number of gridcells in x,y for full domain:
Nx = grid.X.size
Ny = grid.Y.size
# out-of-the-box tutorial configuration is 62x62


###########   load diagnostics   ########### 

# unit for temperature is degrees Celsius, velocity in m/s,
# surface height in m, heat flux in W/m^2
# see run output file available_diagnostics.log
   
# load statistical diagnostic output (set to monthly time-avg output)
# only one output region is defined: global (the default)
dynStDiag = xr.open_dataset('dynStDiag.nc')
dynStDiag = dynStDiag.rename({'Zmd000015':'Z'})
# includes diagnostics:
# TRELAX_ave:   (time, region, depth); region=0 (global), depth=0 (surf-only)
# THETA_lv_ave: (time, region, depth); region=0 (global)
# THETA_lv_std: (time, region, depth); region=0 (global)
   
# load 2-D and 3-D variable diagnostic output, annual mean data
surfDiag = xr.open_dataset('surfDiag.nc')
# TRELAX:       (time, depth, y, x); depth=0 (surface-only)
# ETAN:         (time, depth, y, x); depth=0 (surface-only)
dynDiag = xr.open_dataset('dynDiag.nc')
dynDiag = dynDiag.rename({'Zmd000015':'Z'})
# UVEL:         (time, depth, y, x); x dim. is Nx+1, includes eastern edge
# VVEL:         (time, depth, y, x); y dim. is Ny+1, includes northern edge
# THETA:        (time, depth, y, x)

# Note the Z dimension above has a different name, we rename to the
# standard name 'Z' which will make life easier when we do some
# calculations using the raw data. This is because pkg/diagnostics
# has an option to select specific levels of a 3-D variable to be output.
# If 'levels' are selected in data.diagnostics, the following
# additional statement would be needed:
#   dynDiag['Z'] = grid['Z'][dynDiag.diag_levels.astype(int)-1]


###########   plot diagnostics   ########### 

# figure 4.6 - time series of global mean TRELAX and THETA by level
#
# plot THETA at MITgcm k levels 1,5,15 (SST, -305 m, -1705 m)
klevs = [0, 4, 14]

# MITgcm time unit (dim='T') is seconds, so create new coordinate
# for (dynStDiag) monthly time averages and convert into years.
# Note that the MITgcm time array values correspond to time at the end
# of the time-avg periods, i.e. subtract 1/24 to plot at mid-month.  
Tmon = (dynStDiag['T']/(86400*360) - 1/24).assign_attrs(units='years')
# and repeat to create time series for annual mean time output,
# subtract 0.5 to plot at mid-year.
Tann = (surfDiag['T']/(86400*360) - 0.5).assign_attrs(units='years')

plt.figure(figsize=(16,10))
# global mean TRELAX
plt.subplot(221)
# we tell xarray to plot using this new abscissa Tmon (instead of T)
# xarray is smart enough to ignore the singleton dimensions
# for region and depth
dynStDiag.TRELAX_ave.assign_coords(T=Tmon).plot(color='b', linewidth=4)
plt.grid('both') 
plt.xlim(0, np.ceil(Tmon[-1]))
plt.ylim(-400, 0)
# Alternatively, a global mean area-weighted TRELAX (annual mean)
# could be computed as follows, using HfacC[0,:,:], i.e. HfacC in
# the surface layer, as a land-ocean mask, using xarray .where()
# First, compute total surface area of ocean points:
tot_ocean_area = (grid.rA.where(grid.HFacC[0,:,:]!=0)).sum(('Y', 'X'))
# broadcasting with xarray is more flexible than basic numpy
# because it matches axis name (not dependent on axis position)
# grid area is broadcast across the time dimension below
TRELAX_ave_ann = (surfDiag.TRELAX * grid.rA.where(grid.HFacC[0,:,:]!=0)
                 ).sum(('Y', 'X')) / tot_ocean_area
TRELAX_ave_ann.assign_coords(T=Tann).plot(
                 color='m', linewidth=4, linestyle='dashed')
plt.title('a) Net Heat Flux into Ocean (TRELAX_ave)')

plt.subplot(223)
# mean THETA by level
# Here is an example of allowing xarray to do even more of the work
# and labeling for you, given selected levels, using 'hue' parameter
THETAmon = dynStDiag.THETA_lv_ave.assign_coords(T=Tmon, Z=grid.RC)
THETAmon.isel(Z=klevs).plot(hue='Z', linewidth=4)
plt.grid('both')
plt.title('b) Mean Potential Temp. by Level (THETA_lv_avg)')
plt.xlim(0, np.ceil(Tmon[-1]))
plt.ylim(0, 30);
# Note a legend (of Z depths) was included automatically.
# Specifying colors is not so simple however, either change
# the default a priori, e.g. ax.set_prop_cycle(color=['r','g','c'])
# or after the fact, e.g. lh[0].set_color('r') etc.

plt.subplot(224)
# standard deviation of THETA by level
THETAmon = dynStDiag.THETA_lv_std.assign_coords(T=Tmon, Z=grid.RC)
THETAmon.isel(Z=klevs).plot(hue='Z', linewidth=4)
plt.grid('both')
plt.title('c) Std. Dev. Potential Temp. by Level (THETA_lv_std)')
plt.xlim(0, np.ceil(Tmon[-1]))
plt.ylim(0, 8)
plt.show()

# figure 4.7 - 2-D plot of TRELAX and contours of free surface height
# (ETAN) at simulation end ( endTime = 3110400000. is t=100 yrs).
##
eta_masked = surfDiag.ETAN[-1,0,:,:].where(grid.HFacC[0,:,:]!=0)
plt.figure(figsize=(10,8)) 
surfDiag.TRELAX[-1,0,:,:].plot(cmap='RdBu_r', vmin=-250, vmax=250)
plt.xlim(0, 60)
plt.ylim(15, 75)
eta_masked.plot.contour(levels=np.arange(-.6,.7,.1), colors='k')
plt.title('Free surface height (contours, CI .1 m) and '
          'TRELAX (shading, $\mathregular{W/m^2}$)')
plt.show()
# By default, this uses a plotting routine similar to
# pcolormesh to plot TRELAX.
# Note that xarray uses the coordinates of TRELAX automatically from
# the netcdf metadata! With axis labels! Even though the plotted TRELAX
# DataArray has coordinates Y,X (cell centers) it is plotted correctly 
# (effectively shading cells between Yp1,Xp1 locations, which are the
# lower left corners of grid cells, i.e. YG,XG). Also note we mask the
# land values when contouring the free surface height. Alternatively,
# one could make a smoother color plot using contourf: 
# surfDiag.TRELAX[-1,0,:,:].plot.contourf(
#          cmap='RdBu_r',levels=np.linspace(-250,250,1000))

# figure 4.8 - barotropic streamfunction, plot at simulation end
# (w/overlaid labeled contours)
#
# here is an example where the xarray broadcasting is superior:
ubt = (dynDiag.UVEL * grid.drF).sum('Z') # depth-integrated u velocity
psi = (-ubt * grid.dyG).cumsum('Y').pad(Y=(1,0),constant_values=0.) / 1E6
# Note psi is computed and plotted at the grid cell corners which we
# compute as dimensioned (Ny,Nx+1); as noted, UVEL contains an extra
# data point in x, at the eastern edge. cumsum is done in y-direction.
# We have a wall at southern boundary (i.e. no reentrant flow from
# north). We need to add a row of zeros to specify psi(j=0).
# xarray .pad() allows one to add a row at the top and/or bottom,
# we just need the bottom row (j=0) padding the computed psi array.
# Thus after padding psi is shape (dynDiag.UVEL['T'].size,Ny+1,Nx+1) 
# Finally, since psi is computed at grid cell corners,
# we need to re-assign Yp1 coordinate to psi.
# As an aside comment, if data manipulation with xarray still seems a
# bit cumbersome, you are not mistaken... there are tools developed
# such as xgcm (https://xgcm.readthedocs.io/en/latest/) specifically
# for this purpose.
# Note xxx.values extracts the numpy-array data from xarray
# DataArray xxx; this is done in the plot statement below: 

plt.figure(figsize=(10,8))
psi.isel(T=-1).assign_coords(Y=grid.Yp1.values).plot.contourf(
             levels=np.linspace(-30, 30, 13), cmap='RdYlBu_r')
cs = psi.isel(T=-1).assign_coords(Y=grid.Yp1.values).plot.contour(
             levels=np.arange(-35, 40, 5), colors='k')
plt.clabel(cs, fmt = '%.0f')
plt.xlim(0, 60)
plt.ylim(15, 75)
plt.title('Barotropic Streamfunction (Sv)');
plt.show()

# figure 4.9 - potential temp at depth and xz slice, at simulation end
#
# plot THETA at MITgcm k=4 (220 m depth) and j=15 (28.5N)
klev =  3
jloc = 14

theta_masked = dynDiag.THETA[-1,klev,:,:].where(grid.HFacC[klev,:,:]!=0)
plt.figure(figsize=(16,6))
plt.subplot(121)
# again we use pcolor for this plan view, grabs coordinates automatically
dynDiag.THETA[-1,klev,:,:].plot(cmap='coolwarm', vmin=0, vmax=30 )
# and overlay contours w/masked out boundary/land cells
theta_masked.plot.contour(levels=np.linspace(0, 30, 16), colors='k')
plt.xlim(0, 60)
plt.ylim(15, 75)
plt.title('a) THETA at %g m Depth ($\mathregular{^oC}$)' %grid.RC[klev])

# For the xz slice, our limited vertical resolution makes for an ugly
# pcolor plot, we'll shade using contour instead, providing the centers
# of the vertical grid cells and cell centers in the x-dimension.
# Also mask out land cells at the boundary, which results in slight
# white space at domain edges.
theta_masked = dynDiag.THETA[-1,:,jloc,:].where(grid.HFacC[:,jloc,:]!=0)
plt.subplot(122)
theta_masked.plot.contourf(levels=np.arange(0,30,.2), cmap='coolwarm')
theta_masked.plot.contour(levels=np.arange(0,30,2), colors='k')
plt.title('b) THETA at %gN ($\mathregular{^oC}$)' %grid.YC[jloc,0])
plt.xlim(0, 60)
plt.ylim(-1800, 0)
plt.show()
# One approach to avoiding the white space at boundaries/top/bottom
# is to copy neighboring tracer values to land cells prior to contouring
# (and don't mask), and augment a row of z=0 data at the ocean surface
# and a row at the ocean bottom.
# (see tutorial Southern Ocean Reentrant Channel for an example)
# The white space occurs because contour uses data at cell centers, with
# values masked/undefined beyond the cell centers toward boundaries.

# To instead plot using pcolor: 
# theta_masked.assign_coords(Z=grid.RC.values).plot(cmap='coolwarm')
