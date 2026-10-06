#!/usr/bin/env python
# -*- coding: iso-8859-15 -*-
######################## -*- coding: utf-8 -*-
"""Usage: get_grdchk_location.py
purpose: find location of grdchk points from "data.grdchk" settings
"""

#
# this is the python version of the matlab script get_grdchk_location.py
#
import numpy as np
import sys
sys.path.append('../../../utils/python/MITgcmutils')
import MITgcmutils as mit

# from a model run
hFacC=mit.rdmds('hFacC')
nr,ny,nx = hFacC.shape

# from SIZE.h :
sNx, sNy = nx//2, ny//2

# from data.grdchk :
iGloPos  = 31
jGloPos  = 7
kGloPos  = 1
iGloTile = 2
jGloTile = 2

# from data.grdchk :
# some parameters to test:
nsteps = [450, 560, 553]
nend = 1700

# count number of wet-points in this tile(iGloTile,jGloTile):
nSx, nSy = nx//sNx, ny//sNy

msk=np.ceil(hFacC).reshape((nr,nSy,sNy,nSx,sNx))
mskTile=np.squeeze(msk[:,iGloTile-1,:,jGloTile-1,:])

cumMsk=np.cumsum(mskTile.reshape((sNx*sNy*nr,)))

# find location:
ic1,jc1,kc1 = iGloPos,jGloPos,kGloPos

ijk1=ic1+(jc1-1)*sNx+(kc1-1)*sNx*sNy
yy1=cumMsk[ijk1-1]

for nstep in nsteps:
    #-- find the location of all grdchk points:
    print('In tile (%i,%i), nstep = %4i , (ic,jc,kc) = ( %2i , %2i , %2i ) :'%(
        iGloTile,jGloTile, nstep,ic1,jc1,kc1))
    for n in np.arange(0,nend+1,nstep):
        I=np.where(cumMsk==(yy1+n))[0]
        ii=I[0];               kg=1+np.floor(ii/(sNx*sNy))
        ii=np.mod(ii,sNx*sNy); jg=1+np.floor(ii/sNx)
        ii=np.mod(ii,sNx);     ig=1+ii
        print(' #%6i : i,j,k = %2i , %2i , %2i'%(n,ig,jg,kg))

    print(' ------------')
