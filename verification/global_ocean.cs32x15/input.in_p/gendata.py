#!/usr/bin/env python
# -*- coding: iso-8859-15 -*-
######################## -*- coding: utf-8 -*-

# simple script to generate p-coordinate specific input from standard experiment

import numpy as np
import matplotlib.pyplot as plt
import sys, os
# requires that the path contains utils/python/MITgcmutils or that the utils
# are installed via pip or similar:
import MITgcmutils as mit

# some helper routines
def sqinf(a):
    """ replace zeros by Inf
    """
    b = np.copy(np.squeeze(a))
    b[b==0] = np.Inf
    return b

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

    if False: pass
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
nn=32
ny=nn
nx=6*nn

# from now on we assume that ./testreport has been run for this experiment
# and that we have output available in ../tr_run.seaice
# bathymetry
prec = 'float64'
#b=readfield('../input/bathy_Hmin50.bin',[nn,nn*6],prec)
b= - mit.rdmds('../tr_run.seaice/Depth')
writefield('bathy_Hmin50.bin',-b*grho)

# hydrography
t=readfield('../input/lev_T_cs_15k.bin',[nr,nn,nn*6],prec)
s=readfield('../input/lev_S_cs_15k.bin',[nr,nn,nn*6],prec)
writefield('lev_T_cs_15k.bin',t[::-1,:,:])
writefield('lev_S_cs_15k.bin',s[::-1,:,:])

hfz = mit.rdmds('../tr_run.seaice/hFacC')
rac  = mit.rdmds('../tr_run.seaice/RAC')
xg = mit.rdmds('../tr_run.seaice/XG')
yg = mit.rdmds('../tr_run.seaice/YG')
mskz = np.copy(hfz)
mskz[mskz!=0] = 1.
# create geopotential anomaly file:
delz = np.asarray([50., 70., 100., 140., 190.,
                   240., 290., 340., 390., 440.,
                   490., 540., 590., 640., 690.])
delp = delz*grho
# # this is from "data", but it is unclear where these numbers come from
# delp = np.asarray([7103300.720021, 6570548.440790, 6041670.010249,
#       5516436.666057, 4994602.034410, 4475903.435290,
#       3960063.245801, 3446790.312651, 2935781.405664,
#       2426722.705046, 1919291.315988, 1413156.804970,
#       1008846.750166,  705919.025481,  504089.693499])[::-1]

# integrate initial fields vertically minus reference potential
pf0 = np.hstack([0,np.cumsum(delp)])
pc0 = 0.5*(pf0[:-1]+pf0[1:])
pc3d = np.tile(pc0.reshape((nr,1,1)),(1,ny,nx))*mskz
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

# hydrostatic pressure from averaged temperatuer and salinity profiles
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
recip_rho = 1./sqinf(rhoInSitu)
geopotanom = -((recip_rho - 1/rhoConst)*hfz*dp3d).sum(axis=0)
# this is equivalent
geopotanom1= b/rhoConst-(recip_rho*hfz*dp3d).sum(axis=0)
# these are approximation that are not quite accurate
geopotanom2= ((rhoInSitu - rhoConst)*hfz*dz3d).sum(axis=0)*gravity/rhoConst
geopotanom3= -((recip_rho - 1/rhoConst)*grho*hfz*dz3d).sum(axis=0)

# the correct version
writefield('geopotanom.bin',geopotanom)


# # pickup (this is tricky)
# p = readfield('../input/pickup.0000072000',[nr*8+3,nn,nn*6],prec)

# nflds=8
# p3d = np.copy(p[:nflds*nr,:,:])
# for k in range(nflds):
#     pp = p3d[(k-1)*nr:k*nr,:,:]
#     p[(k-1)*nr:k*nr] = pp[::-1,:,:]

# writefield('pickup.0000072000',p)

# plot field
#plt.clf()
#mit.cs.pcol(xg,yg,geopotanom)
#plt.colorbar(orientation='horizontal')
#plt.show()
