# import python modules
import numpy as np
import matplotlib.pyplot as plt
import netCDF4 as nc


###########   load grid data   ########### 

# Load grid variables (these are internal MITgcm source code variables; if using standard
# binary output instead of netcdf, these are dumped to individual files, e.g. 'RC.data' etc.)
# assumes output in separated tiles pre-concatenated into global files (used script utils/python/MITgcmutils/scripts/gluemncbig)
# and moved into the top directory (see MITgcm user manual section 4.2.4.1) 

grid=nc.Dataset('grid.nc')
#  1-D fields
RC=grid['RC'][:]   # vertical grid, cell center locations
drF=grid['drF'][:] # spacing of grid cells in the vertical (separation between cell top and bottoms)
#  2-D fields
XC=grid['XC'][:]   # x-location of gridcell centers
YC=grid['YC'][:]   # y-location of gridcell centers
dyG=grid['dyG'][:] # grid spacing in y-dimension (i.e. separation between corners)
rA=grid['rA'][:]   # surface area of gridcells
#  3-D fields
HFacC=grid['HFacC'][:] # vertical fraction of cell which is ocean
   
# For convenience, load additional dimensional variables from netcdf files (not incl. in binary output)
# note these are also present in netcdf diagnostic data files.
# 1-D fields
X=grid['X'][:]     # 1-D version of XC data
Y=grid['Y'][:]     # 1-D version of YC data
Xp1=grid['Xp1'][:] # x-location of gridcell lower left corner (1-D version of XG w/extra point)
Yp1=grid['Yp1'][:] # y-location of gridcell lower left corner (1-D version of YG w/extra point)


###########   load diagnostics   ########### 
   
# load statistical diagnostic output (monthly time-averaged output)
# only one output region is defined, global (the default)
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
 

########### plot diagnostics   ########### 

# figure 4.6 - time series of global mean TRELAX and THETA by level
#
plt.figure(figsize=(16,10))
plt.subplot(221)
plt.plot(np.linspace(1/12,100,1200),TRELAX_ave[:,0,0],'b',linewidth=4);plt.grid('both')
plt.title('a) Net Heat Flux into Ocean (TRELAX_ave)')
plt.xlabel('Time (yrs)'); plt.ylabel('$\mathregular{W/m^2}$')
plt.xlim(0,100);plt.ylim(-400,0)
#  Aternatively, a global mean TRELAX (annual mean) could be computed as following,
#  using HfacC[0,:,:] i.e. HfacC in the surface layer, as a land-ocean mask.
TRELAX_ave_ann= (TRELAX[:,0,:,:]*np.tile(rA*HFacC[0,:,:],(100,1,1))).sum(2).sum(1) \
                / (np.tile(rA*HFacC[0,:,:],(100,1,1))).sum(2).sum(1)
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

# figure 4.7 - 2-D plot of TRELAX and contours of free surface height (ETAN) at t=100 yrs
#
plt.figure(figsize=(10,8)) 
plt.pcolormesh(Xp1,Yp1,TRELAX[-1,0,:,:],cmap='RdBu_r')
plt.xlim(0,60);plt.ylim(15,75);plt.colorbar();plt.clim(-250,250)
plt.contour(X,Y,np.ma.array(ETAN[-1,0,:,:],mask=(HFacC[0,:,:]==0)),levels=np.arange(-.6,.7,.1),colors='black'); 
plt.title('Free surface height (contours, CI .1 m) and TRELAX (shading, $\mathregular{W/m^2}$)')
plt.xlabel('Longitude');plt.ylabel('Latitude');
plt.show()
#  Note we have used routine pcolormesh with Xp1, Yp1, which are the locations of
#  the lower left corners of grid cells (here, both length 63 as
#  they include the ending right and upper locations of the grid,
#  respectively). Alternative one could plot shading using contourf 
#  using dimensions X and Y, the grid cell center points (see fig 4.9).
#  Also note we mask the land values when contouring the free surface height.

# figure 4.8 - barotropic streamfunction at t=100 yrs
#
plt.figure(figsize=(10,8))
ubt= (UVEL*np.transpose(np.tile(drF,(100,62,63,1)),(0,3,1,2))).sum(1) # depth-integrated u velocity
psi= (-ubt*np.tile(dyG,(100,1,1))).cumsum(1)/1e6  # compute streamfunction in Sv
plt.contourf(Xp1,Yp1,np.concatenate((np.zeros((63,1)).T,psi[-1,:,:])),np.arange(-35,40,5),cmap='RdYlBu_r')
plt.colorbar()
cs=plt.contour(Xp1,Yp1,np.concatenate((np.zeros((63,1)).T,psi[-1,:,:])),np.arange(-35,40,5),colors='black')
plt.clabel(cs, fmt = '%.0f')
plt.xlim(0,60);plt.ylim(15,75)
plt.title('Barotropic Streamfunction (Sv)');plt.xlabel('Longitude');plt.ylabel('Latitude');
plt.show()
#  Note psi is computed and plotted at the grid cell corners and is dimensioned 63x63
#  cumsum is done in y-direction; we have a wall at southern boundary
#  (i.e. no reentrant flow from north), so we need to add a row of zeros to specify psi(j=0).   

# figure 4.9 - potential temperature at 220m depth and xz slice at 28.5N at t=100 yrs.
plt.figure(figsize=(16,6))
plt.subplot(121)
# here, we use pcolor and provide the corner points XG,YG to locate individual gridcell
plt.pcolormesh(Xp1,Yp1,THETA[-1,3,:,:],cmap='coolwarm');plt.clim(0,30)
plt.colorbar()
# but to overlay contours we provide the cell centers and mask out the boundary/land cells
plt.contour(XC,YC,np.ma.array(THETA[-1,3,:,:],mask=(HFacC[3,:,:]==0)),np.arange(0,30,2),colors='black')
plt.title('a) THETA 220m Depth ($\mathregular{^oC}$)')
plt.xlim(0,60);plt.ylim(15,75);
plt.xlabel('Longitude');plt.ylabel('Latitude');
plt.subplot(122);
# here, our limited vertical resolution makes for an ugly pcolor plot, we'll shade using contour instead
# providing the centers of the vertical grid cells and cell centers in the x-dimension,
# also masking out land cells at the boundary, which results in slight white space at the domain edges.
# Using pcolor syntax, providing location of vertical cell "faces" RF: plt.pcolormesh(Xp1,RF,THETA[-1,:,14,:],cmap='coolwarm')
plt.contourf(X,RC,np.ma.array(THETA[-1,:,14,:],mask=(HFacC[:,14,:]==0)),np.arange(0,30,.2),cmap='coolwarm')
plt.colorbar()
plt.contour(X,RC,np.ma.array(THETA[-1,:,14,:],mask=(HFacC[:,14,:]==0)),np.arange(0,32,2),colors='black')
plt.title('b) THETA at 28.5N ($\mathregular{^oC}$)');plt.xlim(0,60)
plt.xlabel('Longitude');plt.ylabel('Depth (m)');
plt.show()
# one approach to avoid the white space at boundaries is to copy neighboring tracer values to land cells prior to contouring
