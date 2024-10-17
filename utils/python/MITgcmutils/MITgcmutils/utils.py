# Created by EGavilan Pascual-Ahuir on 2023-04-07
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.colors import ListedColormap
from mpl_toolkits.axes_grid1.inset_locator import (mark_inset,inset_axes,
                                                  zoomed_inset_axes)
__doc__ = """
Utilities for MITgcm.
"""

_typeprefixes = {'ieee-be': '>',
                 'b': '>',
                 '>': '>',
                 'ieee-le': '<',
                 'l': '<',
                 '<': '<',
                 }
_typesuffixes = {'float32': 'f4',
                 'float64': 'f8',
                 }

cmap_lm =  ListedColormap(["lightsteelblue"])

def gen_blanklist(depth, sNx, sNy, tilemap=False,fill_value=0):
    """
    Computes blanklist for data.exch2


    Parameters
    ----------
    depth      : 2D array_like
                 depth [m].
    sNx        : int
                 x point in each tile.
    sNy        : int
                 y point in each tile.
    tilemap    : bool
                 True : output tile contourplot,
                 default False.

    Returns
    -------
    blank         : list of int
                    empty tiles numbers
    fig(optional) : matplotlib figure
                    tile plot

    Usage
    -----
    >>> blank=gen_blanklist(bathy, 5, 5, tilemap=False)
    10,11,12,..,103
    >>> [blank,fig]=gen_blanklist(bathy, 5, 5, tilemap=True)
    10,11,12,..,103

    Example
    -------
    >>> eg_blanklist()
    10,11,12,..,103

    """

    assert depth.ndim==2,'check_stp: depth must be 2D'

    [Ny,Nx]=depth.shape
    nPx = Nx//sNx
    nPy = Ny//sNy
    blank = []
    tile_order = np.zeros([nPy, nPx], dtype=int)
    for n in range(0, nPy):
      for m in range(0, nPx):
        tile_sum = np.sum(depth[n*sNy:(n+1)*sNy, m*sNx:(m+1)*sNx])
        tile_order[n, m] = int(n*nPx+m+1)
        if tile_sum == 0:  # the tile with only land cells
          blank.append(tile_order[n, m])
          
    assert len(blank)>0,'There are not land tiles'
    
    if tilemap:

      # plot ocean and blank tiles
      mland = np.copy(depth)
      mland[mland!=fill_value] = 1
      mland[mland==fill_value] = np.nan

      [cn_x, cn_y] = np.meshgrid(np.arange(sNx//2, Nx, sNx),
                                np.arange(sNy//2, Ny, sNy))
      p0 = 0
      fig = plt.figure()
      ax = fig.add_subplot(111)
      ax.pcolormesh(mland,cmap=cmap_lm)
      major_xticks = np.arange(0, Nx+sNx, sNx)
      major_yticks = np.arange(0, Ny+sNy, sNy)
      for a, b, c in zip(cn_x.flat, cn_y.flat, tile_order.flat):
        if c==blank[p0]:
         rect = patches.Rectangle((a-sNx//2, b-sNy//2),
                 sNx, sNy, linewidth=2,edgecolor='r', facecolor='none')
         ax.add_patch(rect)
         rect.set_clip_path(rect)
         p0+=1
        ax.annotate(str(c), (a, b), color='black',
                    ha='center', va='center')
        if p0==len(blank):p0=0
      ax.set_xticks(major_xticks)
      ax.set_yticks(major_yticks)
      ax.set_aspect('equal', adjustable='box')
      ax.set_xlim([0,Nx])
      ax.set_ylim([0,Ny])
      ax.set_xlabel('x')
      ax.set_ylabel('y')

      ax.grid()

      return blank,fig

    else:

      return blank


def hfac(depth,rF,hFacMin=0.3,hFacMinDr=50,htype='C'):
    """
    Computes hFacC,W,S


    Parameters
    ----------
    depth      : 2D array_like
                 Depth [m].
    rF         : 1D array_like
                 Depth at the f point.
    hFacMin    : float
                 Min fraction for partial vertical levels.
    hFacMinDr  : float
                 Min depth for partial vertical levels.
    htype      : string
                 Types of hfac: one or more of 'C','S','W', default='C'.

    Returns
    -------
    hFacC,W,S : tuple of array_like
                The hfac arrays requested in htype.
    Usage
    -----
    >>> [hFacC]=mit.hfac(depth,rF,0.3,50,'C')

    Example
    -------
    >>> [hFacC]=eg_hfac()

    Notes
    -----

    The first row and column are filled with zeros
    for the hFacS and hFacW, respectively.
    """

    assert depth.ndim==2,'check_stp: depth must be 2D'
    assert rF.ndim==1,'check_stp: rF must be 1D'

    assert np.any(depth<0),'check_stp: depth does not have no negative values'
    assert np.all(rF<=0),'check_stp: rF must be negative'

    [Ny,Nx] = depth.shape
    dRF = abs(np.diff(rF))
    Nr = dRF.size

    recip_drF = 1/dRF

    hFacC = np.zeros([Nr,Ny,Nx])
    hFacW = np.zeros([Nr,Ny,Nx])
    hFacS = np.zeros([Nr,Ny,Nx])
    for k in range(0,Nr):
      hFacMnSz = np.max([hFacMin, np.min([hFacMinDr*recip_drF[k],1])])

#     Calculate lopping factor hFacC :
      hFac_loc = (rF[k]-depth)*recip_drF[k]
      hFac_loc = np.minimum(np.maximum(hFac_loc, 0 ) , 1)
#     o Impose minimum fraction and/or size (dimensional)
      hFac_loc[hFac_loc < hFacMnSz/2]=np.nan
      hFac_loc[depth >= 0]=np.nan
      hFac_loc = np.maximum(hFac_loc, hFacMnSz)
      hFac_loc[np.isnan(hFac_loc)]=0
      hFacC[k,:,:] = hFac_loc
      hFacW[k,:,1:Nx] = np.minimum(hFacC[k,:,1:Nx],hFacC[k,:,0:Nx-1])
      hFacS[k,1:Ny,:] = np.minimum(hFacC[k,1:Ny,:],hFacC[k,0:Ny-1,:])

    hfac_dic = {}
    hfac_dic['C'] = hFacC
    hfac_dic['S'] = hFacS
    hfac_dic['W'] = hFacW

    return tuple(hfac_dic[i] for i in htype)

def readbin(fname, ndims, dataprec='float32', machineformat='b'):
    """
    Read meta-data files as written by MITgcm.

    Parameters
    ----------
    fname : string
        name of file to read
    ndims : int
        dimension of the file
    dataprec : string
        precision of resulting file ('float32' or 'float64')
    machineformat : string
        endianness ('b' or 'l', default 'b')

    Returns
    -------
    arr : array_like
          numpy array of the data read

    Usage
    -----
    >>> arr=readbin('bathy.bin',[Y,X])
    """

    tp = _typeprefixes[machineformat]
    try:
        tp = tp + _typesuffixes[dataprec]
    except KeyError:
        raise ValueError("dataprec must be 'float32' or 'float64'.")

    f = open(fname)
    arr = np.fromfile(f, tp, count=np.prod(ndims)).reshape(ndims)
    f.close()

    return arr

def tilecmap(arr, sNx, sNy, tilen=None, sel_zoom=5, fill_value=0):
    """
    Pseudocolor plot of land mask with tiles superimposed, optionally
    showing the values of arr for a single tile.

    Parameters
    ----------
    arr        : 2D array_like
                 values to plot, land mask is taken as arr==fill_value
    sNx        : int
                 number of x points in each tile
    sNy        : int
                 number of y points in each tile
    tilen      : int or None
                 plot a specific tile, default None
    sel_zoom   : int
                 zooming range, default 5
    fill_value : float
                 default 0

    Returns
    -------
    figure     : matplotlib figure
                 Plot of land mask, tiles and arr values

    Usage
    -----
    >>> [fig]=tilecmap(bathy, 5, 5)
    >>> [fig]=tilecmap(bathy, 5, 5, 66, sel_zoom=4)

    Example
    -------
    >>> eg_tilemap()


    """
    #Check dimensions of arr

    assert arr.ndim==2,'check_stp: array must be 2D'

    [Ny,Nx]=arr.shape

    nTx = Nx//sNx
    nTy = Ny//sNy

    mland = np.copy(arr)
    mland[mland!=fill_value] = 1
    mland[mland==fill_value] = np.nan
    arr = arr*mland

    tile_order = np.zeros([nTy, nTx], dtype=int)

    for n in range(0, nTy):
      for m in range(0, nTx):
        tile_order[n, m] = int(n*nTx+m+1)

    [cn_x, cn_y] = np.meshgrid(np.arange(sNx//2, Nx, sNx),
                                np.arange(sNy//2, Ny, sNy))

    major_xticks = np.arange(0, Nx+sNx, sNx)
    major_yticks = np.arange(0, Ny+sNy, sNy)


    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.pcolormesh(mland,cmap=cmap_lm)

    for a, b, c in zip(cn_x.flat, cn_y.flat, tile_order.flat):
        ax.annotate(str(c), (a, b), color='black',
                    ha='center', va='center')

    if tilen is not None:

     [Tind]=np.argwhere(tile_order==tilen)

     #Select position for the zoom inseting
     if (Tind[0]>nTy/2):
       if(Tind[1]>nTx//2):locz=2; locm1=1; locm2=4; cbar_pos=-0.1
       else:locz=1; locm1=2; locm2=3; cbar_pos=1.05
     else:
       if(Tind[1]>nTx//2):locz=1; locm1=3; locm2=4; cbar_pos=1.05
       else:locz=2; locm1=3; locm2=4; cbar_pos=-0.1

     #Select colorbar range for zoom
     Tix = Tind[1]*sNx;  Tiy = Tind[0]*sNy
     arrmin = np.nanmin(arr[Tiy:Tiy+sNy,Tix:Tix+sNx])
     arrmax = np.nanmax(arr[Tiy:Tiy+sNy,Tix:Tix+sNx])

     ax2 = zoomed_inset_axes(ax, zoom=sel_zoom, loc=locz, borderpad=-1)
     pc=ax2.pcolormesh(arr,vmin=arrmin,vmax=arrmax,cmap=plt.cm.jet)

     ax2.set_xlim([major_xticks[Tind[1]],major_xticks[Tind[1]]+sNx])
     ax2.set_ylim([major_yticks[Tind[0]],major_xticks[Tind[0]]+sNy])
     mark_inset(ax, ax2, loc1=locm1, loc2=locm2, fc="none", lw=1.5, ec='0')
     plt.xticks(visible=False)
     plt.yticks(visible=False)
     cax = inset_axes(ax2,
                      width="5%",
                      height="100%",
                      loc="lower left",
                      bbox_to_anchor=(cbar_pos,0,1,1),
                      bbox_transform=ax2.transAxes,
                      borderpad=0,
                      )
     cbar=plt.colorbar(pc,cax=cax, orientation='vertical')
     if cbar_pos<0:
      cax.yaxis.tick_left()
    ax.set_xticks(major_xticks)
    ax.set_yticks(major_yticks)
    ax.set_aspect('equal', adjustable='box')
    ax.set_xlim([0,Nx])
    ax.set_ylim([0,Ny])
    ax.set_xlabel('x')
    ax.set_ylabel('y')
    ax.grid()

    return fig



def writebin(fname, arr, dataprec='float32', machineformat='b'):
    '''Write an array to a bin format for MITgcm

    Parameters
    ----------
    fbase : string
            Name of file to write
    arr : array_like
          Numpy array to write
    dataprec : string
               precision of resulting file ('float32' or 'float64')
               ('float32' by default)
    machineformat : string
                    'b' or 'l' for big or little endian
                    ('b' by default)  
    Usage
    -----
    >>> writebin('data.bin',arr)
    '''

    tp = _typeprefixes[machineformat]
    try:
        tp = tp + _typesuffixes[dataprec]
    except KeyError:
        raise ValueError("dataprec must be 'float32' or 'float64'.")

    arr.astype(tp).tofile(fname)
