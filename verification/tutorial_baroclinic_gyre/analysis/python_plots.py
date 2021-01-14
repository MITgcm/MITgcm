# import python modules
import numpy as np
import matplotlib.pyplot as plt
import netCDF4 as nc


###########   load grid data   ########### 

# Load grid variables (these are internal MITgcm source code variables; if using standard
# binary output instead of netcdf, these are dumped to individual files, e.g. 'RC.data' etc.)
# assumes output in separated tiles pre-concatenated into global files (used script utils/python/MITgcmutils/scripts/gluemncbig)
# and moved into the top directory (see MITgcm user manual section 4.2.4.1) 

# Using a spherical polar grid, all X,Y variables are in longitude, latitude coordinates
# vertical grid provided in meters, area in m^2

grid=nc.Dataset('grid.nc')
#  1-D fields
RC=grid['RC'][:]   # vertical grid, cell center locations
drF=grid['drF'][:] # spacing of grid cells in the vertical (separation between cell top and bottoms)
#  2-D fields (y,x)
XC=grid['XC'][:]   # x-location of gridcell centers
YC=grid['YC'][:]   # y-location of gridcell centers
dyG=grid['dyG'][:] # grid spacing in y-dimension (i.e. separation between corners)
rA=grid['rA'][:]   # surface area of gridcells
#  3-D fields (z,y,x)
HFacC=grid['HFacC'][:] # vertical fraction of cell which is ocean
   
# For convenience, load additional dimensional variables from netcdf files (not incl. in binary output);
# these are also present in netcdf diagnostic data files.
# 1-D fields
X=grid['X'][:]     # 1-D version of XC data
Y=grid['Y'][:]     # 1-D version of YC data
Xp1=grid['Xp1'][:] # x-location of gridcell lower left corner (1-D version of XG w/extra end point)
Yp1=grid['Yp1'][:] # y-location of gridcell lower left corner (1-D version of YG w/extra end point)


###########   load diagnostics   ########### 

# unit for temperature is degrees Celsius, velocity in m/s, surface height in m, heat flux in W/m^2
# see run output file available_diagnostics.log
   
# load statistical diagnostic output (set to monthly time-averaged output)
# only one output region is defined: global (the default)
dynStDiag=nc.Dataset('dynStDiag.nc')
TRELAX_ave=dynStDiag['TRELAX_ave'][:]     # (time, region, depth); region=0 (global), depth=0 (surface-only)
THETA_lv_ave=dynStDiag['THETA_lv_ave'][:] # (time, region, depth); region=0 (global)
THETA_lv_std=dynStDiag['THETA_lv_std'][:] # (time, region, depth); region=0 (global)
   
# load 2-D and 3-D variable diagnostic output, annual mean data
surfDiag=nc.Dataset('surfDiag.nc')
TRELAX=surfDiag['TRELAX'][:] # (time, depth, y, x); depth=0 (surface-only)
ETAN=surfDiag['ETAN'][:]     # (time, depth, y, x); depth=0 (surface-only)
dynDiag=nc.Dataset('dynDiag.nc')
UVEL=dynDiag['UVEL'][:]      # (time, depth, y, x); x dim. is 63, includes eastern edge
VVEL=dynDiag['VVEL'][:]      # (time, depth, y, x); y dim. is 63, includes northern edge
THETA=dynDiag['THETA'][:]    # (time, depth, y, x)
 

###########   plot diagnostics   ########### 

# figure 4.6 - time series of global mean TRELAX and THETA by level
#
plt.figure(figsize=(16,10))
plt.subplot(221)
#  Plot time axis as the mid-point of the (monthly) time average period (unit: years).         
plt.plot(np.linspace(1/12,100,1200)-1/24,TRELAX_ave[:,0,0],'b',linewidth=4);plt.grid('both')
plt.title('a) Net Heat Flux into Ocean (TRELAX_ave)')
plt.xlabel('Time (yrs)'); plt.ylabel('$\mathregular{W/m^2}$')
plt.xlim(0,100);plt.ylim(-400,0)
# Alternatively, a global mean area-weighted TRELAX (annual mean) could be computed as follows,
# using HfacC[0,:,:] i.e. HfacC in the surface layer, as a land-ocean mask.
total_ocn_area= (rA*HFacC[0,:,:]).sum(1).sum(0) # compute total surface area of ocean points
# numpy is often smart enough to figure out broadcasting, depending on axis position
# in next line, note a np.tile command is NOT necessary to span the grid array across the time axis:
TRELAX_ave_ann= (TRELAX[:,0,:,:]*(rA*HFacC[0,:,:])).sum(2).sum(1)/total_ocn_area
plt.plot(np.linspace(0.5,99.5,100),TRELAX_ave_ann,'m--',linewidth=4)
plt.subplot(223)
plt.plot(np.linspace(1/12,100,1200),THETA_lv_ave[:,0,0],'c',linewidth=4,label='$\mathregular{T_{surf}}$');plt.grid('both')
plt.plot(np.linspace(1/12,100,1200),THETA_lv_ave[:,0,4],'g',linewidth=4,label='$\mathregular{T_{300m}}$')
plt.plot(np.linspace(1/12,100,1200),THETA_lv_ave[:,0,14],'r',linewidth=4,label='$\mathregular{T_{abyss}}$')
plt.title('b) Mean Potential Temp. by Level (THETA_lv_avg)')
plt.xlabel('Time (yrs)');plt.ylabel('$\mathregular{^oC}$');
plt.xlim(0,100);plt.ylim(0,30);plt.legend()
plt.subplot(224)
plt.plot(np.linspace(1/12,100,1200),THETA_lv_std[:,0,0],'c',linewidth=4,label='$\mathregular{T_{surf}}$');plt.grid('both')
plt.plot(np.linspace(1/12,100,1200),THETA_lv_std[:,0,4],'g',linewidth=4,label='$\mathregular{T_{300m}}$')
plt.plot(np.linspace(1/12,100,1200),THETA_lv_std[:,0,14],'r',linewidth=4,label='$\mathregular{T_{abyss}}$')
plt.title('c) Std. Dev. Potential Temp. by Level (THETA_lv_std)')
plt.xlabel('Time (years)');plt.ylabel('$\mathregular{^oC}$');
plt.xlim(0,100);plt.ylim(0,8);plt.legend()
plt.show()

# figure 4.7 - 2-D plot of TRELAX and contours of free surface height (ETAN) at t=100 yrs.
#
plt.figure(figsize=(10,8)) 
plt.pcolormesh(Xp1,Yp1,TRELAX[-1,0,:,:],cmap='RdBu_r')
plt.xlim(0,60);plt.ylim(15,75);plt.colorbar();plt.clim(-250,250)
plt.contour(X,Y,np.ma.array(ETAN[-1,0,:,:],mask=(HFacC[0,:,:]==0)),levels=np.arange(-.6,.7,.1),colors='black'); 
plt.title('Free surface height (contours, CI .1 m) and TRELAX (shading, $\mathregular{W/m^2}$)')
plt.xlabel('Longitude');plt.ylabel('Latitude');
plt.show()
# Note we have used routine pcolormesh with Xp1, Yp1, which are the locations of the lower left corners
# of grid cells (here, both length 63 as they include the ending right and upper locations of the grid,
# respectively). Alternative one could plot shading using contourf with dimensions X and Y,
# the grid cell center locations.
# Also note we mask the land values when contouring the free surface height.

# figure 4.8 - barotropic streamfunction at t=100 yrs. (w/overlaid labeled contours)
#
plt.figure(figsize=(10,8))
ubt= np.array(UVEL*drF[:,np.newaxis,np.newaxis]).sum(1) # depth-integrated u velocity
# above numpy needs a bit of help with broadcasting the drF vector across [time,y,x] axes
# but below it manages broadcasting across the time axis
psi= (-ubt*dyG).cumsum(1)/1.e6  # compute streamfunction in Sv (for each year)
plt.contourf(Xp1,Yp1,np.concatenate((np.zeros((63,1)).T,psi[-1,:,:])),np.arange(-35,40,5),cmap='RdYlBu_r')
plt.colorbar()
cs=plt.contour(Xp1,Yp1,np.concatenate((np.zeros((63,1)).T,psi[-1,:,:])),np.arange(-35,40,5),colors='black')
plt.clabel(cs, fmt = '%.0f')
plt.xlim(0,60);plt.ylim(15,75)
plt.title('Barotropic Streamfunction (Sv)');plt.xlabel('Longitude');plt.ylabel('Latitude');
plt.show()
# Note psi is computed and plotted at the grid cell corners and is dimensioned 62x63
# cumsum is done in y-direction; we have a wall at southern boundary
# (i.e. no reentrant flow from north), so we need to add a row of zeros to specify psi(j=0).   

# figure 4.9 - potential temperature at 220m depth (k=3) and xz slice at 28.5N (j=14) at t=100 yrs.
plt.figure(figsize=(16,6))
plt.subplot(121)
# again we use pcolor for the plan view and provide the corner points XG,YG
plt.pcolormesh(Xp1,Yp1,THETA[-1,3,:,:],cmap='coolwarm');plt.clim(0,30)
plt.colorbar()
# but to overlay contours we provide the cell centers and mask out boundary/land cells
plt.contour(X,Y,np.ma.array(THETA[-1,3,:,:],mask=(HFacC[3,:,:]==0)),np.arange(0,30,2),colors='black')
plt.title('a) THETA 220m Depth ($\mathregular{^oC}$)')
plt.xlim(0,60);plt.ylim(15,75);
plt.xlabel('Longitude');plt.ylabel('Latitude');
plt.subplot(122);
# Here, our limited vertical resolution makes for an ugly pcolor plot, we'll shade using contour instead,
# providing the centers of the vertical grid cells and cell centers in the x-dimension.
# Also mask out land cells at the boundary, which results in slight white space at domain edges.
plt.contourf(X,RC,np.ma.array(THETA[-1,:,14,:],mask=(HFacC[:,14,:]==0)),np.arange(0,30,.2),cmap='coolwarm')
plt.colorbar()
plt.contour(X,RC,np.ma.array(THETA[-1,:,14,:],mask=(HFacC[:,14,:]==0)),np.arange(0,32,2),colors='black')
plt.title('b) THETA at 28.5N ($\mathregular{^oC}$)');plt.xlim(0,60);plt.ylim(-1800,0)
plt.xlabel('Longitude');plt.ylabel('Depth (m)');
plt.show()
# One approach to avoiding the white space at boundaries/top/bottom (this occurs because contour plot
# uses data at cell centers, with values masked/undefined beyond the cell centers toward boundaries)
# is to copy neighboring tracer values to land cells prior to contouring (and don't mask),
# and augment a row of z=0 data at the ocean surface and a row at the ocean bottom.
# To instead plot using pcolor, provide location of vertical cell faces RF:
# RF=grid['RF'][:];plt.pcolormesh(Xp1,RF,THETA[-1,:,14,:],cmap='coolwarm')


# The gluemncbig steps outlined in tutorial section 4.2.4.1 can be avoided by using the python package "MITgcmutils".
# Loading up variables to plot, in place of above, can be accomplished by reading directly from the mnc output directories:
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

# An advantage using mnc_files() is that you aren't required to waste disk space assembling glued files.
# See MITgcm users guide chapter 11 for reference information on MITgcmutils.
# Note mnc_file() will generally NOT work with wildcard mnc_test_* directories from multiple runs are present
# unless you pass it the specific directories to read, e.g. from a 4-core mpi run in mnc_test_0009 through mnc_test_0012:
# dynStDiag=mnc.mnc_files('mnc_test_{0009..0012}/dynStDiag.0000000000.t*.nc')
