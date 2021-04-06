import numpy as np
from numpy import cos, sin, pi

Ho = 5000  # ocean depth in meters
nx = 62    # number of gridpoints in x-direction
ny = 62    # number of gridpoints in y-direction
xo = 0     # origin in x,y for ocean domain
yo = 0     # (i.e. southwestern corner of ocean domain)
dx = 20    # grid spacing in x (km)
dy = 20    # grid spacing in y (km)
xeast  = xo + (nx-2)*dx   # eastern extent of ocean domain
ynorth = yo + (ny-2)*dy   # northern extent of ocean domain

# Flat bottom at z=-Ho
h = -Ho * np.ones((ny, nx))

# Walls (surrounding domain); generate bathymetry file
h[:, [0,-1]] = 0   # set ocean depth to zero at east and west walls
h[[0,-1], :] = 0   # set ocean depth to zero at south and north walls
# save as single-precision (float32) with big-endian byte ordering
h.astype('>f4').tofile('bathy.bin')

# Ocean domain extends from (xo,yo) to (xeast,ynorth)
# (i.e. the ocean spans nx-2, ny-2 grid cells)
# out-of-box-config: xo=yo=0, dx=dy=20 km, ocean extent (0,0)-(1200,1200) km
# model domain includes a land cell surrounding the ocean domain
# The full model domain cell centers are located at:
#    XC[0,:] = -10, +10, ..., +1210 (km)
#    YC[:,0] = -10, +10, ..., +1210 (km)
# and full model domain cell corners are located at:
#    XG[0,:] = -20, 0, ..., 1200 [, 1220] (km)
#    YG[:,0] = -20, 0, ..., 1200 [, 1220] (km)
# where the last value in brackets is not included in the MITgcm grid variable
# and reflects the eastern and northern edge of the model domain respectively.
# See section 2.11.4 of the MITgcm users manual.

# Zonal wind-stress, located at u-points (see section 2.11.4)
# here we non-dimensionalize: 0 at southern and western ocean boundary 
# to 1.0 at eastern and northern ocean boundary
# for the purpose of applying sinusoidal-shaped wind stress curve
tauMax = 0.1  # wind stress maximum
x = (np.arange(nx)-1) / (nx-2)   # x-coordinate, located at XG points
y = (np.arange(ny)-.5) / (ny-2)  # y-coordinate, located at YC points
Y, X = np.meshgrid(y, x, indexing='ij')

tau = -tauMax * cos(Y*pi) # generate file for -cos(y) profile between 0-1200km
tau.astype('>f4').tofile('windx_cosy.bin')
tau = tauMax * sin(Y*pi) # generate file for +sin(y) profile between 0-1200km
tau.astype('>f4').tofile('windx_siny.bin')

# Meridional wind-stress, if desired, would be located at v-points (XC, YG)
