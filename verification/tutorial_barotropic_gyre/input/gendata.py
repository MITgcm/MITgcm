import numpy as np
import sys
Ho=5000 # ocean depth in meters
nx=62 # number of gridpoints in x-direction
ny=62 # number of gridpoints in y-direction

# Flat bottom at z=-Ho
h = -Ho*np.ones((ny,nx))

# Walls (surrounding domain) - generate bathymetry file
h[:,(0,-1)]=0
h[(0,-1),:]=0
# save as single precision with big-endian byte-ordering
h.astype('>f4').tofile('bathy.bin')

# Zonal wind-stress, located at u-points
tauMax=0.1 # wind stress maximum
x = (np.arange(nx)-1)/(nx-2)  # x-coordinate, located at XG points
y = (np.arange(ny)-.5)/(ny-2) # y-coordinate, located at YC points
Y, X = np.meshgrid(y, x,indexing='ij')

tau = -tauMax*np.cos(Y*np.pi) # generate file for -cos(y) profile between 0-1200km
tau.astype('>f4').tofile('windx_cosy.bin')
tau = tauMax*np.sin(Y*np.pi) # generate file for +sin(y) profile between 0-1200km
tau.astype('>f4').tofile('windx_siny.bin')

# Meridional wind-stress, if desired, would be located at v-points (XC, YG)
