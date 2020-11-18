import numpy as np
import sys
Ho=1800 # depth of ocean
nx=62   # gridpoints in x
ny=62   # gridpoints in y
xo=0; yo=15 # origin in x,y for ocean
dx=1; dy=1  # grid spacing in x, y

# Flat bottom at z=-Ho
h = -Ho*np.ones((ny,nx))

# create a border ring of walls around edge of domain
h[:,(0,-1)]=0
h[(0,-1),:]=0
h.astype('>f4').tofile('bathy.bin')

# Zonal wind-stress
tauMax=0.1
x = np.linspace(-xo-dx,xo+dx*(nx-2),nx)
y = np.linspace(yo-dy,yo+dy*(ny-2),ny)+dy/2
Y, X = np.meshgrid(y, x,indexing='ij') # zonal wind-stress on (XG,YC) points
tau = -tauMax*np.cos(2*np.pi*((Y-yo)/(ny-2)/dy)) # ny-2 accounts for walls at N,S boundaries
tau.astype('>f4').tofile('windx_cosy.bin')

# restoring temperature
Tmax=30; Tmin=0
Trest = (Tmax-Tmin)/(ny-2)*((yo+dy*(ny-2)-Y))
Trest.astype('>f4').tofile('SST_relax.bin')
