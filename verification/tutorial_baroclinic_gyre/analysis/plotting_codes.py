# import python modules
import numpy as np
import matplotlib.pyplot as plt
import xarray as xr

# load grid
grid = xr.open_dataset('grid.nc')

# load diagnostics
# statistical diagnostics
dynStDiag = xr.open_dataset('mnc_test_0004/dynStDiag.0000000000.t001.nc')
# surface (2D) diagnostics
surfDiag = xr.open_dataset('surfDiag.nc')
# 3D diagnostics
dynDiag = xr.open_dataset('dynDiag.nc')


# plot diagnostics

# figure 4.6 - time series of global mean TRELAX and THETA by level
plt.subplot(211)
dynStDiag.TRELAX_ave.plot()
plt.ylabel('W/m^2')
plt.title('a)    Net Heat Flux into Ocean (TRELAX_ave)')

plt.subplot(223)
dynStDiag.THETA_lv_ave.isel(Zmd000015=0).plot(label='T_surf')
dynStDiag.THETA_lv_ave.isel(Zmd000015=4).plot(label='T_300m')
dynStDiag.THETA_lv_ave.isel(Zmd000015=14).plot(label='T_abyss')
plt.legend()
plt.ylabel('Degrees Celsius')
plt.title('b)    Mean Potential Temperature by level (THETA_lv_ave)')

plt.subplot(224)
dynStDiag.THETA_lv_std.isel(Zmd000015=0).plot(label='T_surf')
dynStDiag.THETA_lv_std.isel(Zmd000015=4).plot(label='T_300m')
dynStDiag.THETA_lv_std.isel(Zmd000015=14).plot(label='T_abyss')
plt.legend()
plt.ylabel('Degrees Celsius')
plt.title('c)    Std. Dev. Potential Temperature by level (THETA_lv_std)')

plt.show()


# figure 4.7 - 2 D plot of TRELAX and contours of
# free surface height (ETAN) at t=100 years

surfDiag.TRELAX[-1,0,:,:].plot(cmap='RdBu', vmin=-250, vmax=250)
surfDiag.ETAN[-1,0,:,:].plot.contour(levels=np.linspace(-0.6,0.6,13), colors='k')

plt.show()


# figure 4.8 - barotropic streamfunction at t = 100 years

ubt = (dynDiag.UVEL.rename({'Zmd000015':'Z'})*grid.drF).sum(dim='Z')
psi = (ubt*grid.dyG).cumsum(dim='Y')

(psi[-1,:,:]/1e6).plot.contourf(levels=np.linspace(-30, 30, 13), cmap='RdBu')
plt.show()


# figure 4.9 - potential temperature at 220 m depth and xz slice at 28.5 N

plt.subplot(121)
dynDiag.THETA[-1,3,:,:].plot.contourf(levels=np.linspace(0,30,16), cmap='inferno')
plt.title('THETA at 220 m depth')

plt.subplot(122)
plt.contourf(grid.X, grid.Z, dynDiag.THETA[-1,:,14,:], np.linspace(0,30,16), cmap='inferno')
plt.colorbar()
plt.ylabel('Depth (m)')
plt.xlabel('Longitude')
plt.title('THETA at 28.5 N')

plt.show()

