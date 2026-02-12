#!/usr/bin/env python
# -*- coding: iso-8859-15 -*-
######################## -*- coding: utf-8 -*-
"""Usage: gendata.py
generates extra input files of input_ad.obcs test
"""

import numpy as np
import sys

nx=80
ny=42
nr=1
nt=1

def writefield(fname,data):
    """Call signatures::

    writefield(filename, numpy.ndarray)

    Write unblocked binary data.
    """

    if sys.byteorder == 'little': data.byteswap(True)

    fid = open(fname,"wb")
    data.tofile(fid)
    fid.close()

    # go back to original byte ordering
    if sys.byteorder == 'little': data.byteswap(True)

def readfield(fname,dims,datatype):
    """Call signatures::

    readfield(filename, dims, numpy.datatype)

    Read unblocked binary data with dimentions "dims".
    """
    fid = open(fname,"rb")
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

# use default topography and modify it
fld = readfield('../input/bathy_3c.bin',[ny,nx],'float64')

fmin = fld.min()

bnew = np.copy(fld)
bnew[0,:17] = fmin
bnew[0,63:] = fmin

writefield('./bathy_3c.obcs',bnew)

# use initial conditions (of constant ocean) to derive obcs conditions
# for ocean
ufld = readfield('../input/uVel_3c0.bin',[ny,nx],'float64')
vfld = readfield('../input/vVel_3c0.bin',[ny,nx],'float64')

uS = ufld[1,:]
vS = vfld[2,:]
uN = ufld[-1,:]
vN = vfld[-1,:]

uW = ufld[:,1]
vW = vfld[:,2]
uE = ufld[:,-1]
vE = vfld[:,-1]

writefield('OBSu.bin',uS)
writefield('OBSv.bin',vS)
writefield('OBNu.bin',uN)
writefield('OBNv.bin',vN)
writefield('OBEu.bin',uE)
writefield('OBEv.bin',vE)
writefield('OBWu.bin',uW)
writefield('OBWv.bin',vW)

# initial conditions for sea ice variables
iceC0 = 1.
iceConc = np.ones((ny,nx))*iceC0
iceConc[bnew==0] = 0.
writefield('ice0_area.obcs',iceConc)

iceH0=0.2
iceVol=iceConc*iceH0
writefield('ice0_heff.obcs',iceVol)

wind0 = 10.
windy = np.ones((ny,nx))*wind0
windy[:,:nx//2] = -wind0
writefield('windy.bin',windy)
