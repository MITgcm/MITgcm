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

# ocean domain extends from (xo,yo) to (xo+dx*(nx-2),yo+dy*(ny-2))
# (i.e. the ocean spans nx-2, ny-2 grid cells)
# out-of-box-config: xo=yo=0, dx=dy=20 km, ocean extent (0,0)-(1200,1200) km
# model domain includes a land cell surrounding the ocean domain
# The full model domain cell centers are located at:
#    XC[0,:]=-10,+10,...,+1210
#    YC[:,0]=-10,+10,...,+1210
# and full model domain cell corners are located at:
#    XG[0,:]=-20,0,...,1200[,1220]
#    YG[:,0]=-20,0,...,1200[,1220]
# where the last value in brackets is not included in the MITgcm grid variable
# but reflects the eastern and northern edge of the model domain respectively
# see section 2.11.4 of the MITgcm users manual

# Zonal wind-stress, located at u-points
# here we non-dimensional: xo=yo=0 to 1.0 at eastern and northern ocean boundary
# for the purpose of applying sinusoidal-shaped wind stress curve
tauMax=0.1 # wind stress maximum
x = (np.arange(nx)-1)/(nx-2)  # x-coordinate, located at XG points
y = (np.arange(ny)-.5)/(ny-2) # y-coordinate, located at YC points
Y, X = np.meshgrid(y, x,indexing='ij')

tau = -tauMax*np.cos(Y*np.pi) # generate file for -cos(y) profile between 0-1200km
tau.astype('>f4').tofile('windx_cosy.bin')
tau = tauMax*np.sin(Y*np.pi) # generate file for +sin(y) profile between 0-1200km
tau.astype('>f4').tofile('windx_siny.bin')

# Meridional wind-stress, if desired, would be located at v-points (XC, YG)
