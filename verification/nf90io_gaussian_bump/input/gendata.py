import numpy as np
import numpy.matlib as matlib
import os
import shutil

H = 2000.
h0=500.
N0 = 5.2e-3

outdir='./'

# These must match ../code/SIZE.h
ny = 2 * 6
nx = 4 * 4
nz = 6

dy = 1000.
dx = 1000.

dx = np.zeros(nx)+dx
x = np.cumsum(dx)
x = x - x[int(nx/2)]

with open(outdir+"/delXvar.bin", "wb") as f:
  dx.astype('>f8').tofile(f)

dy = np.zeros(ny) + dy
y=np.cumsum(dy)

with open(outdir+"/delYvar.bin", "wb") as f:
  dy.astype('>f8').tofile(f)
f.close()

# topo
sigma = 4000. # m

topo = 1500*np.exp(-x*x/(sigma**2))-1500+h0
#topo = h0*exp(-x*x/(3000**2))
topo[topo<0.]=0.
topo=-H+topo
topo[topo<-H]=-H

TT0 = topo[ None, :] + y[:, None] * 0
with open(outdir+"/topo.bin", "wb") as f:
  TT0.astype('>f8').tofile(f)
f.close()
# dz is from the surface down.  Its saved as positive.

dz=np.zeros(nz)+H/nz

with open(outdir+"/delZvar.bin", "wb") as f:
  dz.astype('>f8').tofile(f)
f.close()

# temperature profile...
g=9.8
alpha = 2e-4
T0 = 28+np.cumsum(N0**2/g/alpha*(-dz))

with open(outdir+"/TRef.bin", "wb") as f:
  T0.astype('>f8').tofile(f)
f.close()

# save T0 over whole domain
slope = np.arange(nx) * 0.2
TT0 = T0[:, None, None] + 0 * y[None, :, None] + slope[ None, None, :]
with open(outdir+"/T0.bin", "wb") as f:
  TT0.astype('>f8').tofile(f)


if 0: ## Copy some other files
    shutil.copy('data', outdir+'/data')
    shutil.copy('eedata', outdir)
    shutil.copy('data.diagnostics', outdir)
    shutil.copy('data.pkg', outdir+'/data.pkg')
    # also store these.  They are small and helpful to document what we did
    for nm in {'input','code'}:
      to_path = outdir+'/'+nm
      if os.path.exists(to_path):
        shutil.rmtree(to_path)
        shutil.copytree('../'+nm, outdir+'/'+nm)
    shutil.copy('../build/mitgcmuv', outdir)
