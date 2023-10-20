# simple python script to generate additional input data
import numpy as np
import sys

kwr, kprt =1, 0
nx, ny, nr = 50, 100, 30

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

y0 = 50
# use default topography and modify it
fld = readfield("../input/icetopo.exp1",[ny,nx],'float64')
topo = np.copy(fld)
topo[y0:,:] = 0.
writefield("icetopo.obcs",topo)

fld = readfield("../input/iceShelf_Mass.bin",[ny,nx],'float64')
# new ice mass
iceMass = np.copy(fld)
iceMass[y0:,:] = 0.
writefield("iceShelf_Mass.obcs",iceMass)
# ice mass tendency
iceMassTend = np.zeros(iceMass.shape)
dt = 1800.
iceMassTend[:40,:]=iceMass[:40,:]*1.e-5/dt
writefield("iceShelf_MassTend.obcs",iceMassTend)

# for averaging
msk = np.ones(iceMass.shape)
msk[y0:,:] = 2.
writefield("iceMask.obcs",msk)

# salinity profile for stability
dz = 30.
N2 = 1.e-5
sBeta = 7e-4
gravity=9.81
dSdz = N2/gravity/sBeta * np.ones((30,))
S0 = 34.4
sRef0 = np.cumsum(dSdz*dz)
sRef = np.maximum(sRef0-sRef0[9],0.) + S0
#sRef = np.maximum(sRef0-sRef0[0],0.) + S0
