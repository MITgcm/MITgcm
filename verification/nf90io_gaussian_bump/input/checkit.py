import numpy as np
import xarray as xr

data = xr.open_dataset('../run/statevars.nc')
print('*****************')
print('statevars.nc:\n\n')

print(data)
data.close()

print('\n\n*****************')
print('statevars2d.nc:\n\n')
data = xr.open_dataset('../run/statevars2d.nc')
print(data)
data.close()
