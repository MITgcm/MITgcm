#!/usr/bin/env python
# -*- coding: iso-8859-15 -*-
######################## -*- coding: utf-8 -*-

# simple script to generate p-coordinate specific input from standard experiment
# but unfortunately this script does not reproduce the values in
# tutorial_global_oce_in_p, yet

import numpy as np
import sys, os
# requires that the path contains utils/python/MITgcmutils or that the utils
# are installed via pip or similar:
import MITgcmutils as mit

# some helper routines
def sq(a):
    a = np.squeeze(a)
    masked_array=np.ma.masked_where(a==0., a)
    return masked_array

def readfield(fname,dims,datatype):
    """Call signatures::

    readfield(filename, dims, numpy.datatype)

    Read unblocked binary data with dimentions "dims".
    """

    try:
        fid = open(fname,"rb")
    except:
        sys.exit( fname+": no such file or directory")

    v   = np.fromfile(fid, datatype)
    fid.close()

    if sys.byteorder == 'little': v.byteswap(True)

    if   len(v) == np.prod(dims):     v = v.reshape(dims)
    elif len(v) == np.prod(dims[1:]): v = v.reshape(dims[1:])
    else:
        errstr = (  "dimensions do not match: \n len(data) = " + str(len(v))
                  + ", but prod(dims) = " + str(np.prod(dims)) )
        raise RuntimeError(errstr)

    return v

def writefield(fname,data):
    """Call signatures::

    writefield(filename, numpy.ndarray)

    Write unblocked binary data.
    """

    if True: pass
    else:
        if sys.byteorder == 'little': data.byteswap(True)

        fid = open(fname,"wb")
        data.tofile(fid)
        fid.close()

        # switch back to machine format
        if sys.byteorder == 'little': data.byteswap(True)

def calc_hydrostatic_pressure(s,t,p0,dz,gravity=9.81,rhoConst=1035.):
    from MITgcmutils import jmd95
    mskz = np.copy(t)
    mskz[mskz!=0]=1.
    dp = np.copy(p0)
    dims=np.asarray(t.shape)
    dims[0]=dims[0]+1
    pf = np.zeros(dims)
    grho = gravity*rhoConst
    rhoInSitu0 = jmd95.dens(s,t,p0/grho)*mskz
    # integration of non-linear hydrostatic equation requires iteration:
    resid = 1
    while resid>1e-15:
        rhoInSitu = jmd95.dens(s,t,p0/grho)*mskz
        # save old pressure
        dp = np.copy(p0)
        # compute new pressure
        pf[0,...] = 0.
        for k in range(nr):
            dpk =  dz[k,...]*gravity*rhoInSitu[k,...]
            p0[k,...]   = (pf[k,...] + 0.5*dpk)*mskz[k,...]
            pf[k+1,...] = (p0[k,...] + 0.5*dpk)*mskz[k,...]

        # check convergence
        dp = dp-p0
        resid = np.sqrt((dp**2).sum())
        print('hydrostatic pressure: resdiual = %e, '%np.sqrt((dp**2).sum()))

    print()
    return p0, pf, rhoInSitu

gravity = 9.81
rhoConst= 1035.
grho=gravity*rhoConst
nr=15
ny=40
nx=90

# bathymetry
prec = 'float64'
prec = 'float32'
bdir = '../../tutorial_global_oce_latlon/input'
bz=np.float64(readfield(os.path.join(bdir,'bathymetry.bin'),[ny,nx],prec))
# writefield('topog.bin',-(b)*grho)
b = readfield('topog.bin',[ny,nx],'float64')
dgp = readfield('deltageopotjmd95.bin',[12,ny,nx],'float64')[0,:,:]

# turn fields upside down again for integration in z-levels
t=readfield('lev_t.bin',[nr,ny,nx],'float64')[::-1,:,:]
s=readfield('lev_s.bin',[nr,ny,nx],'float64')[::-1,:,:]
mskz = mit.rdmds('../run/hFacC')[::-1,:,:]
rac  = mit.rdmds('../run/RAC')
mskz[mskz!=0] = 1.
# create geopotential anomaly file:
delz = np.asarray([50., 70., 100., 140., 190.,
                   240., 290., 340., 390., 440.,
                   490., 540., 590., 640., 690.])
delp = delz*grho
# this is from "data", but it is unclear where these numbers come from
delp = np.asarray([7103300.720021, 6570548.440790, 6041670.010249,
      5516436.666057, 4994602.034410, 4475903.435290,
      3960063.245801, 3446790.312651, 2935781.405664,
      2426722.705046, 1919291.315988, 1413156.804970,
      1008846.750166,  705919.025481,  504089.693499])[::-1]

# integrate initial fields vertically minus reference potential
pf0 = np.hstack([0,np.cumsum(delp)])
pc0 = 0.5*(pf0[:-1]+pf0[1:])
pc3d = np.tile(pc0.reshape((nr,1,1)),(1,ny,nx))*mskz
dp3d = np.tile(delp.reshape((nr,1,1)),(1,ny,nx))
dz3d = np.tile(delz.reshape((nr,1,1)),(1,ny,nx))
# first guess of hydrostatic pressure at center-points based on delp
# pf is the hydrostatic pressure at interfaces (w-points)
pc = np.copy(pc3d)
pc,pf,rhoInSitu = calc_hydrostatic_pressure(s,t,pc,dz3d)

# the new pressure also implies different delp, here computed as an average
# over the model domain
pm = np.zeros((nr,))
tm = np.zeros((nr,))
sm = np.zeros((nr,))
rhom = np.zeros((nr,))
for k in range(nr):
    racz = rac*mskz[k,:,:]
    pm[k] = (pc[k,:,:]*racz).sum()/racz.sum()
    tm[k] = (t[k,:,:]*racz).sum()/racz.sum()
    sm[k] = (s[k,:,:]*racz).sum()/racz.sum()
    rhom[k] = (rhoInSitu[k,:,:]*racz).sum()/racz.sum()

# hydrostatic pressure from averaged temperature and salinity profiles
pmm = np.copy(pm)
pmm,pff,rr = calc_hydrostatic_pressure(sm,tm,pmm,delz)
# this is very similar to diff(pfm), see below
dp = np.diff(pff)
print('hydrostatic pressure layer thickness from averaged hydrography:')
print(' delR = %14f, %14f, %14f,'%(dp[-1],dp[-2],dp[-3]))
for k in range(nr-4,0,-3):
    print('        %14f, %14f, %14f,'%(dp[k],dp[k-1],dp[k-2]))

# averaged pressure at interfaces to compute delP
pfm = np.zeros((nr+1,))
for k in range(nr):
    racz = rac*mskz[k,:,:]
    pfm[k] = (pf[k,:,:]*racz).sum()/racz.sum()

pfm[nr] = (pf[nr,:,:]*racz).sum()/racz.sum()
dp = np.diff(pfm)

print('hydrostatic pressure layer thickness from averaged pressure:')
print(' delR = %14f, %14f, %14f,'%(dp[-1],dp[-2],dp[-3]))
for k in range(nr-4,0,-3):
    print('        %14f, %14f, %14f,'%(dp[k],dp[k-1],dp[k-2]))

# now we would like to compute delRc (distance between c-points)
dp = np.zeros((nr,))
dp[0]  = pm[0] # assuming zero surface pressure
dp[1:] = pm[1:]-pm[:-1]

print(' delRc = %14f, %14f, %14f,'%(dp[-1],dp[-2],dp[-3]))
for k in range(nr-4,0,-3):
    print('         %14f, %14f, %14f,'%(dp[k],dp[k-1],dp[k-2]))

dp3d = np.tile(dp.reshape((nr,1,1)),(1,ny,nx))
# this is the correct way of computing the geopotential anomaly
# (if integr_geoPot = 1)
geopotanom = -((1./sq(rhoInSitu) - 1/rhoConst)*mskz*dp3d).sum(axis=0)
# this is equivalent
geopotanom1= b/rhoConst-(1./sq(rhoInSitu)*mskz*dp3d).sum(axis=0)
# these are approximation that are not quite accurate
geopotanom2= ((rhoInSitu - rhoConst)*mskz*dz3d).sum(axis=0)*gravity/rhoConst
geopotanom3= -((1./sq(rhoInSitu) - 1/rhoConst)*grho*mskz*dz3d).sum(axis=0)

# the correct version
writefield('geopotanom.bin',geopotanom)

# plot field
import matplotlib.pyplot as plt

xg = mit.rdmds('../run/XG')
yg = mit.rdmds('../run/YG')
f1=plt.figure()
f1.clf()
plt.pcolormesh(xg,yg,geopotanom)
plt.colorbar()


f2=plt.figure()
f2.clf()
kindex = -(np.arange(15)+1)
plt.plot((delp/delz)/gravity,kindex,'x-',label='from delR in "data"')
plt.plot((np.diff(pff)/delz)/gravity,kindex,':',label='from mean T/S profile')
plt.plot((np.diff(pfm)/delz)/gravity,kindex,'-.',label='from mean density profile')
plt.ylabel('k-level')
plt.title('(delP/delZ)/gravity')
plt.grid()
plt.plot([rhoConst,rhoConst],[-1,-15],'--')
plt.text(rhoConst,-15,'rhoConst=1035.',rotation=270)
plt.legend()

plt.show()
