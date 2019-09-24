import numpy as np
import xmitgcm
import matplotlib.pyplot as plt



# # Quick analysis of the channel model simulations
#
# Load the two simulations, do some cursory analysis, and plot the output.
#
# We also need to do a bit of work to make the output from `pkg/layers` easier to interpret.


# output from pkg/layers doesn't come with a nice coordinate, so copy this from the config.
layer_bounds = np.array([-2.00, -1.75, -1.50, -1.25,
                        -1.00, -0.75, -0.50, -0.25,
                         0.00,  0.25,  0.50,  0.75,
                         1.00,  1.25,  1.50,  1.75,
                         2.00,  2.25,  2.50,  2.75,
                         3.00,  3.25,  3.50,  3.75,
                         4.00,  4.25,  4.50,   5.0,
                         5.5,    6.0,   6.5,   7.0,
                         7.5,    8.0,   8.5,   9.0,
                         9.5,   10.0,])
layer_centres = (layer_bounds[1:] + layer_bounds[:-1])/2


# Load model output

ds = xmitgcm.open_mdsdataset('../run/Diags', grid_dir='../run',
                             prefix=['state', '2D_diags', 'heat_3D', 'heat_2D', 'layDiag'],
                             delta_t=1000)
# pkg/layers doesn't give a nice name for the layer coordinate
# We're using temperature to define the layers, so let's call it 'temperature'
ds = ds.rename({'_UNKNOWN_':'temperature'})
ds = ds.assign_coords(temperature=layer_centres)


ds_GM = xmitgcm.open_mdsdataset('../run_GM/Diags', grid_dir='../run_GM/',
                                prefix=['state', '2D_diags', 'heat_3D', 'heat_2D', 'layDiag', 'GM_diags'],
                                delta_t=1000)
# pkg/layers doesn't give a nice name for the layer coordinate
ds_GM = ds_GM.rename({'_UNKNOWN_':'temperature'})
ds_GM = ds_GM.assign_coords(temperature=layer_centres)


# plot free surface elevation
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(10,5))

ds['ETAN'][-1,:,:].plot(ax=axarr[0])
ds_GM['ETAN'][-1,:,:].plot(ax=axarr[1])

axarr[0].set_aspect('equal')
axarr[1].set_aspect('equal')


# plot barotropic streamfunction
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(10,5))

(ds['PsiVEL'][-1,:,:,:].sum(dim='Z')/1e6).plot(ax=axarr[0])
axarr[0].contour(ds['XC'], ds['YC'], ds['Depth'], np.arange(0,4000, 500), colors='k')
axarr[0].set_aspect('equal')

(ds_GM['PsiVEL'][-1,:,:,:].sum(dim='Z')/1e6).plot(ax=axarr[1])
axarr[1].contour(ds['XC'], ds['YC'], ds['Depth'], np.arange(0,4000, 500), colors='k')
axarr[1].set_aspect('equal')


# plot surface temperature
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(10,5))

ds['THETA'][-1,0,:,:].plot(ax=axarr[0], cmap='plasma', vmin=-2, vmax=10)
ds_GM['THETA'][-1,0,:,:].plot(ax=axarr[1], cmap='plasma', vmin=-2, vmax=10)
axarr[0].set_aspect('equal')
axarr[1].set_aspect('equal')


# plot surface temperature evolution
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(10,5))
ds['THETA'][:,0,:,:].mean(dim=['XC','YC']).plot(label='mean', ax=axarr[0])
ds['THETA'][:,0,:,:].std(dim=['XC','YC']).plot(label='std', ax=axarr[0])
axarr[0].legend()

ds_GM['THETA'][:,0,:,:].mean(dim=['XC','YC']).plot(label='mean', ax=axarr[1])
ds_GM['THETA'][:,0,:,:].std(dim=['XC','YC']).plot(label='std', ax=axarr[1])
axarr[1].legend()

fig.suptitle('surface')


# plot abyssal temperature evolution
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(10,5))

ds['THETA'][:,-10,:,:].mean(dim=['XC','YC']).plot(label='mean', ax=axarr[0])
# ds['THETA'][:,-10,:,:].std(dim=['XC','YC']).plot(label='std', ax=axarr[0])
axarr[0].legend()

ds_GM['THETA'][:,-10,:,:].mean(dim=['XC','YC']).plot(label='mean', ax=axarr[1])
# ds['THETA'][:,-10,:,:].std(dim=['XC','YC']).plot(label='std', ax=axarr[0])
axarr[1].legend()

fig.suptitle('Abyss')


# plot surface heat flux
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(10,5))

ds['TFLUX'][-1,:,:].plot(ax=axarr[0])
ds_GM['TFLUX'][-1,:,:].plot(ax=axarr[1])
axarr[0].set_aspect('equal')
axarr[1].set_aspect('equal')


# plot surface heat flux evolution
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(10,5))

ds['TFLUX'].mean(dim=['XC','YC']).plot(label='mean', ax=axarr[0])
ds['TFLUX'].std(dim=['XC','YC']).plot(label='std', ax=axarr[0])
axarr[0].legend()

ds_GM['TFLUX'].mean(dim=['XC','YC']).plot(label='mean', ax=axarr[1])
ds_GM['TFLUX'].std(dim=['XC','YC']).plot(label='std', ax=axarr[1])
axarr[1].legend()


# plot zonal mean temperature and mixed layer depth
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(14,5))
fig.subplots_adjust(wspace=0.1)

axarr[0].pcolormesh(ds['YC']/1e3, ds['Z'],
                    ds['THETA'][-1,:,1:,:].where(ds['hFacC'][:,:,:]!=0).mean(dim='XC'),
                    cmap='plasma', vmin=-2, vmax=8)
axarr[0].plot(ds['YC']/1e3, (-ds['MXLDEPTH'][-1,:,:]).mean(dim='XC'), color='k', linewidth=2)
axarr[0].fill_between(ds['YC']/1e3, -4000, -ds['Depth'].mean(dim='XC'), color='grey')
axarr[0].set_xlabel('y-coordinate (m)')
axarr[0].set_ylabel('Depth (m)')
axarr[0].set_title('Horzontal diff')
axarr[0].set_ylim(-4000, 0)

im = axarr[1].pcolormesh(ds_GM['YC']/1e3, ds_GM['Z'],
                    ds_GM['THETA'][-1,:,1:,:].where(ds_GM['hFacC'][:,:,:]!=0).mean(dim='XC'),
                    cmap='plasma', vmin=-2, vmax=8)
CB = plt.colorbar(im, ax =axarr)
CB.ax.set_ylabel('Temperature ($^{\circ}$C)')
axarr[1].plot(ds_GM['YC']/1e3, (-ds_GM['MXLDEPTH'][-1,:,:]).mean(dim='XC'), color='k', linewidth=2)
axarr[1].fill_between(ds_GM['YC']/1e3, -4000, -ds_GM['Depth'].mean(dim='XC'), color='grey')
axarr[1].set_xlabel('y-coordinate (m)')
axarr[1].set_title('GM')
axarr[1].set_ylim(-4000, 0)

fig.suptitle('Zonal mean temperature and mixed layer depth')
fig.savefig('ZM_temperature_MLD.png', bbox_inches='tight', dpi=180)


# plot zonal mean temperature difference
plt.figure()

plt.pcolormesh(ds['YC']/1e3, ds['Z'], (ds['THETA']-ds_GM['THETA'])[-1,:,1:,:].mean(dim='XC'),
              cmap='RdBu_r', vmin=-2.5, vmax=2.5)
CB = plt.colorbar()
CB.ax.set_ylabel('Temperature difference ($^{\circ}$C)')
plt.fill_between(ds['YC']/1e3, -4000, -ds['Depth'].mean(dim='XC'), color='grey')
plt.ylim(-4000, 0)

plt.ylabel('Depth (m)')
plt.xlabel('y-coordinate (km)')
plt.title('Zonal-mean temperature difference ($^{\circ}$C)')

plt.savefig('ZM_temperature_difference.png', bbox_inches='tight', dpi=180)


# plot surface zonal velocity
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(10,5))

t= -10
ds['UVEL'][t:,0,:,:].mean(dim='time').plot(ax=axarr[0])
ds_GM['UVEL'][t:,0,:,:].mean(dim='time').plot(ax=axarr[1])

axarr[0].set_aspect('equal')
axarr[1].set_aspect('equal')


# plot surface zonal velocity evolution
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(10,5))

ds['UVEL'][:,0,:,:].mean(dim=['XG','YC']).plot(label='mean', ax=axarr[0])
ds['UVEL'][:,0,:,:].std(dim=['XG','YC']).plot(label='std', ax=axarr[0])
axarr[0].legend()
axarr[0].set_title('Horizontal')

ds_GM['UVEL'][:,0,:,:].mean(dim=['XG','YC']).plot(label='mean', ax=axarr[1])
ds_GM['UVEL'][:,0,:,:].std(dim=['XG','YC']).plot(label='std', ax=axarr[1])
axarr[1].legend()
axarr[1].set_title('GM')

fig.suptitle('surface')


# plot abyssal zonal velocity evolution
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(10,5))

ds['UVEL'][:,-10,:,:].mean(dim=['XG','YC']).plot(label='mean', ax=axarr[0])
ds['UVEL'][:,-10,:,:].std(dim=['XG','YC']).plot(label='std', ax=axarr[0])
axarr[0].legend()

ds_GM['UVEL'][:,-10,:,:].mean(dim=['XG','YC']).plot(label='mean', ax=axarr[1])
ds_GM['UVEL'][:,-10,:,:].std(dim=['XG','YC']).plot(label='std', ax=axarr[1])
axarr[1].legend()

fig.suptitle('Abyss')


# plot surface meridional velocity
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(10,5))

t = -10
ds['VVEL'][t:,1,:,:].mean(dim='time').plot(ax=axarr[0])
ds_GM['VVEL'][t:,1,:,:].mean(dim='time').plot(ax=axarr[1])

axarr[0].set_aspect('equal')
axarr[1].set_aspect('equal')

# plot abyssal meridional velocity evolution
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(10,5))

ds['VVEL'][:,-10,:,:].mean(dim=['XC','YG']).plot(label='mean', ax=axarr[0])
ds['VVEL'][:,-10,:,:].std(dim=['XC','YG']).plot(label='std', ax=axarr[0])
axarr[0].legend()

ds_GM['VVEL'][:,-10,:,:].mean(dim=['XC','YG']).plot(label='mean', ax=axarr[1])
ds_GM['VVEL'][:,-10,:,:].std(dim=['XC','YG']).plot(label='std', ax=axarr[1])
axarr[1].legend()

fig.suptitle('Abyss')


# plot viscosity from QG Leith dynamical viscosity scheme
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(10,5))
ds['VAHDLTHQ'][-1,1,:,:].plot(ax=axarr[0])
ds_GM['VAHDLTHQ'][-1,1,:,:].plot(ax=axarr[1])

axarr[0].set_aspect('equal')
axarr[1].set_aspect('equal')
plt.suptitle('Viscosity from QG Leith scheme')


# plot mixed layer depth
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(10,5))

ds['MXLDEPTH'][-1,:,:].plot(vmax=600, ax=axarr[0])
ds_GM['MXLDEPTH'][-1,:,:].plot(vmax=600, ax=axarr[1])

axarr[0].set_aspect('equal')
axarr[1].set_aspect('equal')


# plot difference in mixed layer depth between the two simulations
fig, axarr = plt.subplots(1, 1, figsize=(5,5))

(ds['MXLDEPTH'][-1,:,:] - ds_GM['MXLDEPTH'][-1,:,:]).plot()

axarr.set_aspect('equal')

fig.suptitle('Difference in MLD (hor. diff - GM)')


# plot convective adjustment map
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(10,5))

ds['CONVADJ'][-1,:,:,:].mean(dim='XC').plot(ax=axarr[0])
ds_GM['CONVADJ'][-1,:,:,:].mean(dim='XC').plot(ax=axarr[1])


# plot depth-latitude convective adjustment map
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(7,5))
fig.subplots_adjust(wspace=0.1)


axarr[0].pcolormesh(ds['XC']/1e3, ds['YC']/1e3,
                    ds['CONVADJ'][-1,1,:,:], vmin=0, vmax=1)
axarr[0].contour(ds['XC']/1e3, ds['YC']/1e3,
                    ds['Depth'], np.arange(0, 4500, 500), colors='C0')
axarr[0].set_xlabel('x-coordinate (km)')
axarr[0].set_ylabel('y-coordinate (km)')
axarr[0].set_title('Horzontal diff')
axarr[0].set_aspect('equal')

im = axarr[1].pcolormesh(ds_GM['XC']/1e3, ds_GM['YC']/1e3,
                    ds_GM['CONVADJ'][-1,1,:,:], vmin=0, vmax=1)
axarr[1].contour(ds_GM['XC']/1e3, ds_GM['YC']/1e3,
                    ds_GM['Depth'], np.arange(0, 4500, 500), colors='C0')
CB = plt.colorbar(im, ax=axarr)
axarr[1].set_xlabel('x-coordinate (km)')
axarr[1].set_title('GM')
axarr[1].set_aspect('equal')

fig.suptitle('Near surface convective adjustment')
fig.savefig('convadj.png', bbox_inches='tight', dpi=180)



# Overturning streamfunction
psi = (ds['VVEL']*ds['drF']*ds['dxG']).sum(dim='XC').cumsum(dim='Z')/1e6
psi_GM = (ds_GM['VVEL']*ds['drF']*ds['dxG']).sum(dim='XC').cumsum(dim='Z')/1e6


# plot Eulerian-mean overturning from velocity field
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(10,5))

psi[-1,:,:].plot(ax=axarr[0])
psi_GM[-1,:,:].plot(ax=axarr[1])
fig.suptitle('Eulerian-mean overturning streamfunction (Sv) in depth space')

# Package Layers diagnostics

# In density (temperature) space

# plot residual overturning from pkg/layers
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(10,5))

t = -10
(ds['LaVH1TH'][t:,::-1,:,:].mean(dim=['time'])*ds['dxG']/1e6).sum(dim=['XC']).cumsum(
    dim='temperature').plot.contourf(
                        levels=np.linspace(-2.3,2.3,24), ax=axarr[0],)
(ds_GM['LaVH1TH'][t:,::-1,:,:].mean(dim=['time'])*ds['dxG']/1e6).sum(dim='XC').cumsum(
    dim='temperature').plot.contourf(
                        levels=np.linspace(-2.3,2.3,24), ax=axarr[1])
fig.suptitle('Residual overturning streamfunction (Sv) in temperature space')

# In depth space

# We need to do a little bit of work to get these fields to plot in depth space.
# Essentially, we need to know the zonal-mean depth of each layer. To calculate
# this, we take a cummulative sum of `LaHs1TH`, which is the layer thickness at
# the southern face of each cell.

# define the y coordinate grid for plotting
X, Y = np.meshgrid(ds['temperature'], ds['YG']/1e3)

# plot residual overturning from pkg/layers in depth space.
fig, axarr = plt.subplots(1, 2, sharey=True, figsize=(14,5))
fig.subplots_adjust(wspace=0.)

t = -10

im = axarr[0].contourf(Y, -ds['LaHs1TH'][t:,::-1,:,:].cumsum(dim='temperature').mean(dim=['XC', 'time']).values.T,
              (ds['LaVH1TH'][t:,::-1,:,:].mean(dim=['time'])*ds['dxG']/1e6).sum(
                  dim='XC').cumsum(dim='temperature').values.T,
                    np.linspace(-2.3,2.3,24), vmin=-2.4, vmax=2.4, cmap='RdBu_r', extend='min')
plt.colorbar(im, ax=axarr[0])
axarr[0].contour(ds['YC']/1e3, ds['Z'], ds['THETA'][t:,:,:,:].where(
                        ds['hFacC']!=0).mean(dim=['XC', 'time']),
                np.arange(-2.5, 11, 1), colors='grey', alpha=0.7)
axarr[0].fill_between(ds['YC']/1e3, -4000, -ds['Depth'].mean(dim='XC'), color='grey')
axarr[0].set_title('Horizontal diff')
axarr[0].set_ylabel('Depth (m)')
axarr[0].set_xlabel('y-coordinate (km)')
axarr[0].set_ylim(-4000, 0)

im = axarr[1].contourf(Y, -ds_GM['LaHs1TH'][t:,::-1,:,:].cumsum(
                            dim='temperature').mean(dim=['XC', 'time']).values.T,
              (ds_GM['LaVH1TH'][t:,::-1,:,:].mean(dim=['time'])*ds_GM['dxG']/1e6).sum(
                  dim='XC').cumsum(dim='temperature').values.T,
                    np.linspace(-2.3,2.3,24), vmin=-2.4, vmax=2.4, cmap='RdBu_r')
plt.colorbar(im, ax=axarr[1])
axarr[1].contour(ds_GM['YC']/1e3, ds_GM['Z'], ds_GM['THETA'][t:,:,:,:].where(
                        ds_GM['hFacC']!=0).mean(dim=['XC', 'time']),
                np.arange(-2.5, 11, 1), colors='grey', alpha=0.7)
axarr[1].fill_between(ds['YC']/1e3, -4000, -ds['Depth'].mean(dim='XC'), color='grey')
axarr[1].set_title('GM')
axarr[1].set_xlabel('y-coordinate (km)')
axarr[1].set_ylim(-4000, 0)

fig.suptitle('Residual overturning streamfunction (Sv)')

fig.savefig('residual_overturning.png', bbox_inches='tight', dpi=180)

plt.show()
