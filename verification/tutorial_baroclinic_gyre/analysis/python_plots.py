# import python modules
import numpy as np
import matplotlib.pyplot as plt
import netCDF4 as nc


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

grid = nc.Dataset('grid.nc')
#  1-D fields
RC = grid['RC'][:]    # vertical grid, cell center locations
drF = grid['drF'][:]  # vertical spacing of grid cells (thickness of cells)
#  2-D fields (y,x)
XC = grid['XC'][:]    # x-location of gridcell centers
YC = grid['YC'][:]    # y-location of gridcell centers
dyG = grid['dyG'][:]  # grid spacing in y-dim (separation between corners)
rA = grid['rA'][:]    # surface area of gridcells
#  3-D fields (z,y,x)
HFacC = grid['HFacC'][:]  # vertical fraction of cell which is ocean

# For convenience, load additional dimensional variables from
# netcdf files (these variables are NOT included in binary output)
# These grid variables are also present in netcdf diagnostic data files.
# 1-D fields
X = grid['X'][:]      # 1-D version of XC data
Y = grid['Y'][:]      # 1-D version of YC data
Xp1 = grid['Xp1'][:]  # x-location of lower left corner
Yp1 = grid['Yp1'][:]  # y-location of lower left corner
# Xp1 and Yp1 are effectively 1-D versions of grid variables XG,YG with
# an extra data value at the eastern and northern ends of the domain.
# See MITgcm users manual section 2.11 for additional MITgcm grid info

# Number of gridcells in x,y for full domain:
Nx = X.size
Ny = Y.size
# out-of-the-box tutorial configuration is 62x62


###########   load diagnostics   ########### 

# unit for temperature is degrees Celsius, velocity in m/s,
# surface height in m, heat flux in W/m^2
# see run output file available_diagnostics.log
   
# load statistical diagnostic output (set to monthly time-avg output)
# only one output region is defined: global (the default)
dynStDiag = nc.Dataset('dynStDiag.nc')
TRELAX_ave = dynStDiag['TRELAX_ave'][:]      # (time, region, depth); region=0 (global), depth=0 (surf-only)
THETA_lv_ave = dynStDiag['THETA_lv_ave'][:]  # (time, region, depth); region=0 (global)
THETA_lv_std = dynStDiag['THETA_lv_std'][:]  # (time, region, depth); region=0 (global)
  
# load 2-D and 3-D variable diagnostic output, annual mean data
surfDiag = nc.Dataset('surfDiag.nc')
TRELAX = surfDiag['TRELAX'][:]  # (time, depth, y, x); depth=0 (surface-only)
ETAN = surfDiag['ETAN'][:]      # (time, depth, y, x); depth=0 (surface-only)
dynDiag = nc.Dataset('dynDiag.nc')
UVEL = dynDiag['UVEL'][:]       # (time, depth, y, x); x dim is Nx+1, includes eastern edge
VVEL = dynDiag['VVEL'][:]       # (time, depth, y, x); y dim is Ny+1, includes northern edge
THETA = dynDiag['THETA'][:]     # (time, depth, y, x)
 

###########   plot diagnostics   ########### 

# figure 4.6 - time series of global mean TRELAX and THETA by level
#
# plot THETA at MITgcm k levels 1,5,15 (SST, -305 m, -1705 m)
klevs = [0, 4, 14]

# MITgcm time unit (dim='T') is seconds, so create new time series
# for (dynStDiag) monthly time averages and convert into years.
# Note that the MITgcm time array values correspond to time at the end
# of the time-avg periods, i.e. subtract 1/24 to plot at mid-month.
Tmon = dynStDiag['T'][:]/(86400*360) - 1/24 
# and repeat to create time series for annual mean time output,
# subtract 0.5 to plot at mid-year.
Tann = surfDiag['T'][:]/(86400*360) - 0.5

plt.figure(figsize=(16,10))
# global mean TRELAX
plt.subplot(221)
plt.plot(Tmon, TRELAX_ave[:,0,0], 'b', linewidth=4)
plt.grid('both')
plt.title('a) Net Heat Flux into Ocean (TRELAX_ave)')
plt.xlabel('Time (yrs)')
plt.ylabel('$\mathregular{W/m^2}$')
plt.xlim(0, np.ceil(Tmon[-1]))
plt.ylim(-400, 0)
# Alternatively, a global mean area-weighted TRELAX (annual mean)
# could be computed as follows, using HfacC[0,:,:], i.e. HfacC in
# the surface layer, as a land-ocean mask.
# First, compute total surface area of ocean points:
total_ocn_area = (rA * HFacC[0,:,:]).sum()
# numpy is often smart enough to figure out broadcasting,
# depending on axis position.
# In next line, note a np.tile command is NOT necessary to span
# the grid array across the time axis:
TRELAX_ave_ann = (TRELAX[:,0,:,:] * rA * HFacC[0,:,:]).sum((1,2)) / total_ocn_area
plt.plot(Tann, TRELAX_ave_ann, 'm--', linewidth=4)

plt.subplot(223)
plt.plot(Tmon, THETA_lv_ave[:,0,klevs], linewidth=4)
# To specify colors for specific lines, either change the
# default a priori, e.g. ax.set_prop_cycle(color=['r','g','c'])
# or after the fact, e.g. lh[0].set_color('r') etc.
plt.grid('both')
plt.title('b) Mean Potential Temp. by Level (THETA_lv_avg)')
plt.xlabel('Time (yrs)')
plt.ylabel('$\mathregular{^oC}$')
plt.xlim(0, np.ceil(Tmon[-1]))
plt.ylim(0, 30)
plt.legend(RC[klevs], title='Z (m)')

plt.subplot(224)
plt.plot(Tmon, THETA_lv_std[:,0,klevs], linewidth=4)
plt.grid('both')
plt.title('c) Std. Dev. Potential Temp. by Level (THETA_lv_std)')
plt.xlabel('Time (years)')
plt.ylabel('$\mathregular{^oC}$')
plt.xlim(0, np.ceil(Tmon[-1]))
plt.ylim(0, 8)
plt.legend(RC[klevs], title='Z (m)')
plt.show()

# figure 4.7 - 2-D plot of TRELAX and contours of free surface height
# (ETAN) at simulation end ( endTime = 3110400000. is t=100 yrs).
#
eta_masked = np.ma.MaskedArray(ETAN[-1,0,:,:], HFacC[0,:,:]==0)
plt.figure(figsize=(10,8)) 
plt.pcolormesh(Xp1, Yp1, TRELAX[-1,0,:,:], cmap='RdBu_r')
plt.xlim(0, 60)
plt.ylim(15, 75)
plt.colorbar()
plt.clim(-250, 250)
plt.contour(X, Y, eta_masked, levels=np.r_[-.6:.7:.1], colors='black')
plt.title('Free surface height (contours, CI .1 m) and '
          'TRELAX (shading, $\mathregular{W/m^2}$)')
plt.xlabel('Longitude')
plt.ylabel('Latitude')
plt.show()
# Note we have used routine pcolormesh with Xp1, Yp1, which are the
# locations of the lower left corners of grid cells
# (here, length Nx+1,Ny+1 as they include the ending right and upper
# locations of the grid, respectively).
# Alternative one could plot shading using contourf with dimensions
# X and Y, the grid cell center locations.
# Also note we mask the land values when contouring the free
# surface height.

# figure 4.8 - barotropic streamfunction, plot at simulation end
# (w/overlaid labeled contours)
#
ubt = (UVEL * drF[:,np.newaxis,np.newaxis]).sum(1)  # depth-integrated u velocity
# For ubt calculation numpy needs a bit of help with broadcasting
# the drF vector across [time,y,x] axes
psi = np.zeros((dynDiag['T'].size, Ny+1, Nx+1))
psi[:,1:,:] = (-ubt * dyG).cumsum(1) / 1E6  # compute streamfn in Sv (for each yr)
# Note psi is computed and plotted at the grid cell corners and we
# compute as dimensioned (Ny,Nx+1); as noted, UVEL contains an extra
# data point in x, at the eastern edge. cumsum is done in y-direction.
# We have a wall at southern boundary (i.e. no reentrant flow from
# north), ergo psi(j=0) is accomplished by declaring psi 
# to be shape (dynDiag['T'].size,Ny+1,Nx+1). 

plt.figure(figsize=(10,8))
plt.contourf(Xp1, Yp1, psi[-1], np.arange(-35, 40, 5), cmap='RdYlBu_r')
plt.colorbar()
cs = plt.contour(Xp1, Yp1, psi[-1], np.arange(-35, 40, 5), colors='black')
plt.clabel(cs, fmt = '%.0f')
plt.xlim(0, 60)
plt.ylim(15, 75)
plt.title('Barotropic Streamfunction (Sv)')
plt.xlabel('Longitude')
plt.ylabel('Latitude');
plt.show()

# figure 4.9 - potential temp at depth and xz slice, at simulation end
#
# plot THETA at MITgcm k=4 (220 m depth) and j=15 (28.5N)
klev =  3
jloc = 14

theta_masked = np.ma.MaskedArray(THETA[-1, klev,:,:], HFacC[klev,:,:]==0)
plt.figure(figsize=(16,6))
plt.subplot(121)
# again we use pcolor for the plan view and provide the
# corner point locations XG,YG thru Xp1,Yp1
plt.pcolormesh(Xp1, Yp1, THETA[-1,klev,:,:], cmap='coolwarm')
plt.clim(0, 30)
plt.colorbar()
# but to overlay contours we provide the cell centers
# and mask out boundary/land cells
plt.contour(X, Y, theta_masked, np.arange(0,30,2), colors='black')
plt.title('a) THETA at %g m Depth ($\mathregular{^oC}$)' %RC[klev])
plt.xlim(0, 60)
plt.ylim(15, 75)
plt.xlabel('Longitude')
plt.ylabel('Latitude')

# For the xz slice, our limited vertical resolution makes for an ugly
# pcolor plot, we'll shade using contour instead, providing the centers
# of the vertical grid cells and cell centers in the x-dimension.
# Also mask out land cells at the boundary, which results in slight
# white space at domain edges.
theta_masked = np.ma.MaskedArray(THETA[-1,:,jloc,:], HFacC[:,jloc,:]==0)
plt.subplot(122)
plt.contourf(X, RC, theta_masked, np.arange(0,30,.2), cmap='coolwarm')
plt.colorbar()
plt.contour(X, RC, theta_masked, np.arange(0,32,2), colors='black')
plt.title('b) THETA at %gN ($\mathregular{^oC}$)' %YC[jloc,0])
plt.xlim(0, 60)
plt.ylim(-1800, 0)
plt.xlabel('Longitude')
plt.ylabel('Depth (m)')
plt.show()
# One approach to avoiding the white space at boundaries/top/bottom
# is to copy neighboring tracer values to land cells prior to contouring
# (and don't mask), and augment a row of z=0 data at the ocean surface
# and a row at the ocean bottom.
# (see tutorial Southern Ocean Reentrant Channel for an example)
# The white space occurs because contour uses data at cell centers, with
# values masked/undefined beyond the cell centers toward boundaries.

# To instead plot using pcolor, pass location of vertical cell faces RF:
# RF=grid['RF'][:]
# plt.pcolormesh(Xp1, RF, THETA[-1,:,jloc,:], cmap='coolwarm')


# Note, the gluemncbig steps outlined in tutorial section 4.2.4.1 can be
# avoided by using the python package "MITgcmutils". Loading the grid
# and diagnostic output, in place of steps at the beginning of this
# script, can be accomplished by reading directly from the mnc output
# directories:
#
# from MITgcmutils import mnc
# grid=mnc.mnc_files('mnc_test_*/grid.t*.nc')
# XC=grid.variables['XC'][:]
# ...
# dynStDiag=mnc.mnc_files('mnc_test_*/dynStDiag.0000000000.t*.nc')
# TRELAX_ave=dynStDiag.variables['TRELAX_ave'][:]
# ...
# surfDiag=mnc.mnc_files('mnc_test_*/surfDiag.0000000000.t*.nc')
# TRELAX=surfDiag.variables['TRELAX'][:]
# ...
# dynDiag=mnc.mnc_files('mnc_test_*/dynDiag.0000000000.t*.nc')
# THETA=dynDiag.variables['THETA'][:]
# ...

# An advantage using mnc_files() is that you aren't required to waste
# disk space assembling glued files. See MITgcm users manual chapter 11
# for reference information on MITgcmutils. Note mnc_file() will
# generally NOT work with wildcard mnc_test_* directories from multiple
# runs are present unless you pass it the specific directories to read,
# e.g. for a 4-core mpi run output in mnc_test_0009 thru mnc_test_0012:
# dynStDiag=mnc.mnc_files('mnc_test_{0009..0012}/dynStDiag.0000000000.t*.nc')
