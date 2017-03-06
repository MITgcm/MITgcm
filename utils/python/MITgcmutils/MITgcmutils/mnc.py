import sys
import glob
import numpy as np
from .netcdf import netcdf_file

_exclude_global = ['close',
                   'createDimension',
                   'createVariable',
                   'dimensions',
                   'filename',
                   'flush',
                   'fp',
                   'mode',
                   'sync',
                   'use_mmap',
                   'variables',
                   'version_byte',
                  ]

_exclude_var = ['assignValue',
                'data',
                'dimensions',
                'getValue',
                'isrec',
                'itemsize',
                'shape',
                'typecode',
               ]

def getattributes(nc, exclude=[]):
    # in order not to rely on implementation, provide fallback
    try:
        a = dict(nc._attributes)
    except AttributeError:
        a = dict((k, getattr(nc, k)) for k in dir(nc) if k[0] != '_' and k not in exclude)
    return a


class MNC:
    """
    A file object for MNC (tiled NetCDF) data.

    Should behave mostly like scipy.io.netcdf.netcdf_file in 'r' mode.

    Parameters
    ----------
    fpatt     :: glob pattern for tile files
    layout    :: which global layout to use:
                 'model' : use layout implied by Nx, Ny
                 'exch2' : use exch2 global layout
                 'faces' : variables are lists of exch2 faces
                 default is to use exch2 layout if present, model otherwise

    Example:

    nc = mnc_files('mnc_*/state.0000000000.t*.nc')
    temp = nc.variables['Temp'][:]
    salt = nv.variables['S'][:]
    nc.close()

    temp and salt are now assembled (global) arrays of shape (Nt, Nr, Ny, Nx)
    where Nt is the number iterations found in the file (in this case probably 1).
    """

    # avoid problems with __del__
    nc = []

    def __init__(self, fpatt, layout=None, multitime=False):
        fnames = glob.glob(fpatt)
#        if multitime:
#            iters = [ f[-18:-8] for f in fnames if f.endswith('.t001.nc') ]
#            iters.sort()
#            fnames_first = [ f for f in fnames if f[-18:-8] == iters[0] ]
#        else:
#            fnames_first = fnames
        fnames.sort()

        # open files
        self.nc = [ netcdf_file(f,'r') for f in fnames ]

        # global attributes
        # get from first file, but remove/reset tile-specific ones
        self._attributes = getattributes(self.nc[0], _exclude_global)
        self._attributes['tile_number'] = 1
        self._attributes['bi'] = 1
        self._attributes['bj'] = 1
        haveexch2 = False
        for k in list(self._attributes):
            if k.startswith('exch2_'):
                del self._attributes[k]
                haveexch2 = True

        sNx = self.sNx
        sNy = self.sNy
        ntx = self.nSx*self.nPx
        nty = self.nSy*self.nPy

        if layout is None:
            if haveexch2:
                layout = 'exch2'
            else:
                layout = 'model'

        self.layout = layout

        # precompute indices
        self._i0 = []
        self._ie = []
        self._j0 = []
        self._je = []
        self._fn = []
        self._nf = 0
        if layout == 'model':
            self._nx = self.Nx
            self._ny = self.Ny
            for nc in self.nc:
                tn = nc.tile_number
                bj,bi = divmod(tn-1, ntx)
                ie = sNx*(bi+1-ntx)
                je = sNy*(bj+1-nty)
                self._i0.append(sNx*bi)
                self._j0.append(sNy*bj)
                self._ie.append(ie or None)
                self._je.append(je or None)
        elif layout == 'exch2':
            self._nx = 0
            self._ny = 0
            for nc in self.nc:
                i0 = nc.exch2_txGlobalo - 1
                j0 = nc.exch2_tyGlobalo - 1
                ie = i0 + sNx
                je = j0 + sNy
                self._i0.append(i0)
                self._j0.append(j0)
                self._ie.append(ie)
                self._je.append(je)
                self._nx = max(self._nx, ie)
                self._ny = max(self._ny, je)
            # make ie, je relative to end (for non-tracer points)
            for i in range(len(self._i0)):
                ie = self._ie[i] - self._nx
                je = self._je[i] - self._ny
                self._ie[i] = ie or None
                self._je[i] = je or None
        elif layout == 'faces':
            self._nx = {}
            self._ny = {}
            for nc in self.nc:
                fn = nc.exch2_myFace
                i0 = nc.exch2_tBasex
                j0 = nc.exch2_tBasey
                ie = i0 + sNx
                je = j0 + sNy
                self._fn.append(fn)
                self._i0.append(i0)
                self._j0.append(j0)
                self._ie.append(ie)
                self._je.append(je)
                self._nx[fn] = max(self._nx.get(fn, 0), ie)
                self._ny[fn] = max(self._ny.get(fn, 0), je)
            # make ie, je relative to end (for non-tracer points)
            for i in range(len(self._fn)):
                fn = self._fn[i]
                ie = self._ie[i] - self._nx[fn]
                je = self._je[i] - self._ny[fn]
                self._ie[i] = ie or None
                self._je[i] = je or None
            self._fns = sorted(self._nx.keys())
            self._nf = len(self._fns)
            for i in range(len(self._fn)):
                self._fn[i] = self._fns.index(self._fn[i])
            self._nx = np.array([self._nx[fn] for fn in self._fns])
            self._ny = np.array([self._ny[fn] for fn in self._fns])
        else:
            raise ValueError('Unknown layout: {}'.format(layout))

        # dimensions
        self.dimensions = {}
        for k,n in self.nc[0].dimensions.items():
            # compute size of dimension in global array for X* and Y*
            if k[0] == 'X':
                n += self._nx - sNx
            if k[0] == 'Y':
                n += self._ny - sNy
            self.dimensions[k] = n

        # variables
        var0 = self.nc[0].variables
        # find size of record dimension first
        if 'T' in self.dimensions and self.dimensions['T'] is None:
            self.times = list(var0.get('T', [])[:])
            self.iters = list(var0.get('iter', self.times)[:])
            self.nrec = len(self.iters)

        self.variables = dict((k, MNCVariable(self, k)) for k in var0)

    def __getattr__(self, k):
        try:
            return self._attributes[k]
        except KeyError:
            raise AttributeError("'MNC' object has no attribute '" + k + "'")

    def __dir__(self):
        return self.__dict__.keys() + self._attributes.keys()

    def close(self):
        """Close tile files"""
        for nc in self.nc:
            nc.close()

    __del__ = close

    @property
    def faces(self):
        if self.layout == 'faces':
            return self._fns
        else:
            return None


def calcstrides(slices, dims):
    try:
        slices[0]
    except TypeError:
        slices = (slices,)

    if Ellipsis in slices:
        cut = slices.index(Ellipsis)
        slices = slices[:cut] + (len(dims)-len(slices)+1)*(slice(0,None,None),) + slices[cut+1:]
    else:
        slices = slices + (len(dims)-len(slices))*(slice(0,None,None),)

#    return tuple( hasattr(s,'indices') and s.indices(dim) or s for s,dim in zip(slices,dims) )
    strides = []
    shape = []
    fullshape = []
    for s,dim in zip(slices,dims):
        try:
            stride = s.indices(dim)
        except AttributeError:
            stride = (s, s+1, 1)
            n = 1
        else:
            # real slice, will make a dimension
            start,stop,step = stride
            n = (stop-start+step-1)//step
            shape.append(n)

        fullshape.append(n)
        strides.append(stride)

    return tuple(strides), tuple(shape), tuple(fullshape)


class MNCVariable(object):
    def __init__(self, mnc, name):
        self._name = name
        self.nc = mnc.nc
        self.layout = mnc.layout
        self._i0 = mnc._i0
        self._ie = mnc._ie
        self._j0 = mnc._j0
        self._je = mnc._je
        self._nf = mnc._nf
        self._fn = mnc._fn
        v0 = mnc.nc[0].variables[name]
        self._attributes = getattributes(v0, _exclude_var)
        self.itemsize = v0.data.itemsize
        self.typecode = v0.typecode
        self.dtype = np.dtype(self.typecode())
        self.dimensions = v0.dimensions
        self.shape = tuple( mnc.dimensions[d] for d in self.dimensions )
        self.isrec = self.shape[0] is None
        if self.isrec:
            self.shape = (mnc.nrec,) + self.shape[1:]

        # which dimensions are tiled
        self._Xdim = None
        self._Ydim = None
        for i,d in enumerate(self.dimensions):
            if d[0] == 'X': self._Xdim = i
            if d[0] == 'Y': self._Ydim = i

    def __getattr__(self, k):
        try:
            return self._attributes[k]
        except KeyError:
            raise AttributeError("'MNCVariable' object has no attribute '" + k + "'")

    def __dir__(self):
        return self.__dict__.keys() + self._attributes.keys()

    def __getitem__(self, ind):
        if self.layout == 'faces':
            return self._getfaces(ind)

        if ind in [Ellipsis, slice(None)]:
            # whole array
            res = np.zeros(self.shape, self.typecode())
            s = [slice(None) for d in self.shape]
            for i,nc in enumerate(self.nc):
                if self._Xdim is not None:
                    s[self._Xdim] = slice(self._i0[i], self._ie[i])
                if self._Ydim is not None:
                    s[self._Ydim] = slice(self._j0[i], self._je[i])
                res[s] = nc.variables[self._name][:]

            return res
        else:
            # read only required data
            strides,resshape,fullshape = calcstrides(ind, self.shape)
            res = np.zeros(fullshape, self.dtype)
            s = [slice(*stride) for stride in strides]
            sres = [slice(None) for d in fullshape]
            if self._Xdim is not None: I0,Ie,Is = strides[self._Xdim]
            if self._Ydim is not None: J0,Je,Js = strides[self._Ydim]
            for i,nc in enumerate(self.nc):
                if self._Xdim is not None:
                    i0 = self._i0[i]
                    ie = self.shape[self._Xdim] + (self._ie[i] or 0)
                    a,b = divmod(I0 - i0, Is)
                    e = np.clip(ie, I0, Ie)
                    sres[self._Xdim] = slice(max(-a, 0), (e - I0)//Is)
                    s[self._Xdim] = slice(max(I0 - i0, b), max(Ie - i0, 0), Is)
                if self._Ydim is not None:
                    j0 = self._j0[i]
                    je = self.shape[self._Ydim] + (self._je[i] or 0)
                    a,b = divmod(J0 - j0, Js)
                    e = np.clip(je, J0, Je)
                    sres[self._Ydim] = slice(max(-a, 0), (e - J0)//Js)
                    s[self._Ydim] = slice(max(J0 - j0, b), max(Je - j0, 0), Js)
                res[sres] = nc.variables[self._name][s]

            return res.reshape(resshape)

    def _getfaces(self, ind):
        res = []
        for f in range(self._nf):
            shape = tuple(np.isscalar(d) and d or d[f] for d in self.shape)
            a = np.zeros(shape, self.typecode())
            res.append(a)
        s = [slice(None) for d in self.shape]
        for i,nc in enumerate(self.nc):
            fn = self._fn[i]
            if self._Xdim is not None:
                s[self._Xdim] = slice(self._i0[i], self._ie[i])
            if self._Ydim is not None:
                s[self._Ydim] = slice(self._j0[i], self._je[i])
            res[fn][s] = nc.variables[self._name][:]
        for f in range(self._nf):
            res[f] = res[f][ind]

        return res

    def face(self, fn):
        shape = tuple(np.isscalar(d) and d or d[fn] for d in self.shape)
        res = np.zeros(shape, self.typecode())
        s = [slice(None) for d in self.shape]
        for i,nc in enumerate(self.nc):
            if self._fn[i] == fn:
                if self._Xdim is not None:
                    s[self._Xdim] = slice(self._i0[i], self._ie[i])
                if self._Ydim is not None:
                    s[self._Ydim] = slice(self._j0[i], self._je[i])
                res[s] = nc.variables[self._name][:]

        return res


def mnc_files(fpatt, layout=None):
    return MNC(fpatt, layout)

mnc_files.__doc__ = MNC.__doc__


def rdmnc(fpatt, varnames=None, iters=None, slices=Ellipsis, layout=None):
    ''' Read one or more variables from an mnc file set. 
    
    Parameters
    ----------
    fpatt    :: glob pattern for netcdf files comprising the set
    varnames :: list of variables to read (default all)
    iters    :: list of iterations (not time) to read
    slices   :: tuple of slices to read from each variable
                (typically given as numpy.s_[...])

    Returns a dictionary of arrays.

    Example:

    S = rdmnc("mnc_*/state.0000000000.*', ['U', 'V'], slices=numpy.s_[..., 10:-10, 10:-10])
    u = S['U']
    v = S['V']

    Can currently read only one file set (i.e., 1 file per tile),
    not several files split in time.

    Consider using mnc_files for more control (and similar convenience).
    The same restriction about multiple files applies, however.
    '''
    mnc = MNC(fpatt, layout)
    if varnames is None:
        varnames = mnc.variables.keys()
    elif isinstance(varnames, str):
        varnames = [varnames]

    if iters is not None:
        try:
            iters[0]
        except TypeError:
            iters = [iters]

        iits = [ mnc.iters.index(it) for it in iters ]

        if not isinstance(slices, tuple):
            slices = (slices,)

    res = {}
    for varname in varnames:
        var = mnc.variables[varname]
        if iters is not None and var.dimensions[0] == 'T':
            res[varname] = np.array([var[(iit,)+slices] for iit in iits])
        else:
            res[varname] = var[slices]

    mnc.close()
    return res

