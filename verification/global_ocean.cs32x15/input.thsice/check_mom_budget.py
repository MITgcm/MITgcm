#!/usr/bin/env python
# -*- coding: iso-8859-15 -*-
######################## -*- coding: utf-8 -*-
"""Usage: ./check_mom_budget.py
"""

import sys, os
import matplotlib.pyplot as plt
import numpy as np
try:
    from MITgcmutils import rmds
except:
    # this hack to make sure that MITgcmutils.rdmds is available assumes
    # that we are somewhere within the MITgcm/verification directory
    import re
    cwdstr = os.getcwd()
    dirpath = cwdstr[:re.search("verification",cwdstr).start()]
    sys.path.append(os.path.join(dirpath,'utils/python/MITgcmutils') )
    from MITgcmutils import rdmds

rDir, nit, deltaT   = "../tr_run.thsice/", 36010, 86400
rDir, nit, deltaT   = "../tr_run.viscA4/", 86405,  3600
namF, namFs = 'momDiag', 'srfDiag'

class structtype():
    pass

def load_grid(gDir):

    grd = structtype()
    
    grd.xC = rdmds(os.path.join(gDir,'XC'))
    grd.yC = rdmds(os.path.join(gDir,'YC'))
    grd.xG = rdmds(os.path.join(gDir,'XG'))
    grd.yG = rdmds(os.path.join(gDir,'YG'))

    grd.dXc=rdmds(os.path.join(gDir,'DXC'))
    grd.dYc=rdmds(os.path.join(gDir,'DYC'))
    grd.dXg=rdmds(os.path.join(gDir,'DXG'))
    grd.dYg=rdmds(os.path.join(gDir,'DYG'))

    grd.dRf=np.squeeze(rdmds(os.path.join(gDir,'DRF')))

    grd.rAc=rdmds(os.path.join(gDir,'RAC'))
    grd.rAw=rdmds(os.path.join(gDir,'RAW'))
    grd.rAs=rdmds(os.path.join(gDir,'RAS'))
    grd.rAz=rdmds(os.path.join(gDir,'RAZ'))

    grd.hFacC=rdmds(os.path.join(gDir,'hFacC'))
    grd.hFacW=rdmds(os.path.join(gDir,'hFacW'))
    grd.hFacS=rdmds(os.path.join(gDir,'hFacS'))
    grd.depth=rdmds(os.path.join(gDir,'Depth'))

    return grd

def readdiags(fname,nit):
    v,iter,M=rdmds(os.path.join(rDir,fname), nit, returnmeta = True);
    fList=M['fldlist']
    n=len(fList)
    return v, n, fList

def split_C_cub(v3d):
    """ [v6t] = split_C_cub(v3d)
    --------------------------------------------
    split 2d/3d arrays V, center, to 2d/3d x 6 faces
    and add 1 column + 1 row <== at the begining !!!
    => output is v6t[nr,ny+1,ny+1,6]
    """
    kad=1
    dims = v3d.shape
    nx,ny = dims[-1],dims[-2]
    if len(dims) == 2: nr = 1
    else: nr = np.prod(dims[:-2])
    nyp=ny+1; n2p=ny+2; nye=ny+kad

    v = v3d.reshape((nr,ny,nx))
    v6t = np.zeros((nr,nye,nye,6))

    for n in range(6):
        v6t[:,1:,1:,n]=v[:,:,n*ny:(n+1)*ny]

    v6t[:,1:,0, 0]=v6t[:,-1,:0:-1,4]
    v6t[:,1:,0, 2]=v6t[:,-1,:0:-1,0]
    v6t[:,1:,0, 4]=v6t[:,-1,:0:-1,2]
    v6t[:,1:,0, 1]=v6t[:,1:,-1,0]
    v6t[:,1:,0, 3]=v6t[:,1:,-1,2]
    v6t[:,1:,0, 5]=v6t[:,1:,-1,4]
   
    v6t[:,0, :, 0]=v6t[:,-1,:,5]
    v6t[:,0, :, 2]=v6t[:,-1,:,1]
    v6t[:,0, :, 4]=v6t[:,-1,:,3]
    v6t[:,0,1:, 1]=v6t[:,:0:-1,-1,5]
    v6t[:,0,1:, 3]=v6t[:,:0:-1,-1,1]
    v6t[:,0,1:, 5]=v6t[:,:0:-1,-1,3]

    v6t[:,0,0,1]=v6t[:,1,-1,0]
    v6t[:,0,0,3]=v6t[:,1,-1,3]
    v6t[:,0,0,5]=v6t[:,1,-1,5]

    return v6t

def calc_grad(fld,dx,dy):
    """ calculate gradient of 6-tiled fields fld[nr,ny+1,ny+1,6] and return
    as dfx[nr,ny,ncx]
    """
    nnr, np1 = fld.shape[:2]
    nc = np1-1
    myshape = (1,G.dXc.shape[0],G.dXc.shape[1])
    dfx = (fld[:,:,1:,:]-fld[:,:,:-1,:])[:,1:,:,:] \
          .reshape((nnr,nc,nc*6),order='F') \
          /np.tile(dx.reshape(myshape),(nnr,1,1))
    dfy = (fld[:,1:,:,:]-fld[:,:-1,:,:])[:,:,1:,:] \
          .reshape((nnr,nc,nc*6),order='F') \
          /np.tile(dy.reshape(myshape),(nnr,1,1))
    return dfx, dfy
    
def getListIndex(diagName,fldLst):
    """ Usuage getListIndex(diagName,fldLst)
    retrieve index of diagName in fldlst (list of diagnostics names); 
    if diagName is not in fldlist return -1
    """
    if diagName in str(fldLst):
        j = fldLst.index(diagName)
    else:
        j = -1
    return j

def printstats(var,titv):
    fmt='Var = %8s : Min,Max,Avr,StD= %12.5e %12.5e %12.5e %12.5e'
    print(fmt % (titv,var.min(), var.max(), var.mean(), var.std()))
    return

def printsum(var,res):
    fmt = '   Sum Tend: Avr,StD= %12.5e %12.5e ; Residual= %12.5e %12.5e +';
    print(fmt % (var.mean(), var.std(), res.mean(), res.std()))
    return

def printStatsAndSum(fldLst,dtot,gtot):
    for fldName in fldLst:
        j = getListIndex(fldName,fldList)
        if j > -1: 
            var = np.copy(np.squeeze(v4d[j,:,:,:]))
        elif fldName == fldLst[3]:
            # U/Vm_ImpD was not found. Now we have to do something different
            print(fldLst[3]+" was not found, trying alternative",end = " ")
            if 'Um' in fldLst[3]:
                if juNz>-1: j, var = juNz, gUnuZ
            elif 'Vm' in fldLst[3]:
                if jvNz>-1: j, var = jvNz, gVnuZ
            if j==-1: print("... unsuccessfull")

        if j>-1:
            printstats(var,fldList[j])
            gtot=gtot+var
            printsum(gtot,dtot-gtot)
        else:
            print('... cannot use '+fldName)
    return
  
# let's go

G = load_grid(rDir)

nr, nc = G.hFacC.shape[:2]
nPxy = G.hFacC.shape[2]*nc
nPp2 = nPxy+2;
ncx  = 6*nc
np1  = nc+1;

# not needed:
# globArea=G.rAc.sum()
# yg2=np.zeros(nPp2); yg2[:nPxy]=G.yG[:].reshape((nPxy))
# xg2=np.zeros(nPp2); xg2[:nPxy]=G.xG[:].reshape((nPxy))
# rAz2=np.zeros(nPp2); rAz2[:nPxy]=G.rAz[:].reshape((nPxy))
# #-- cubed sphere special: add missing corners
# xg2[nPxy]   = xg2[0];    yg2[nPxy]   = yg2[2*nc]; rAz2[nPxy]   = rAz2[0]
# xg2[nPxy+1] = xg2[3*nc]; yg2[nPxy+1] = yg2[0];    rAz2[nPxy+1] = rAz2[0]

mskW=np.minimum(np.ceil(G.hFacW),1); mskS=np.minimum(np.ceil(G.hFacS),1);

# set constant: gravity, rhoConst
rhoConst=1035.
gravity =9.81

# Read in 2-D diagnostics file "namFs" and 3-D diagnostics file "namF":
v3d,nV2d,f2dList = readdiags(namFs,nit)
v4d,nV,  fldList = readdiags(namF,nit)

if nV2d == 0: f2dList=fldList

if 'PHI_SURF' in str(f2dList):
    jdps = f2dList.index('PHI_SURF')
    var = v3d[jdps,:,:]
elif 'ETAN' in str(f2dList):
    jdps = f2dList.index('ETAN')
    var = gravity*np.copy(v3d[jdps,:,:])
else:
    jdps = -1
    
if jdps != -1:
    dpx, dpy = calc_grad(- split_C_cub(var),G.dXc,G.dYc)

jnh = -1
fileName='%s.%10.10i.%s' % (os.path.join(rDir,'pnhDiag'),nit+1,'data')
if os.path.isfile(fileName):
    print('  -- loading file: %s ...' % fileName)
    var=rdmds(os.path.join(rDir,'pnhDiag'),nit+1)
    print(' done')
elif 'PHI_NH' in str(fldList):
    jnh = fldList.index('PHI_NH')
    var=np.copy(v4d[jnh,:,:,:])

if jnh > -1:
    dpNHx, dpNHy = calc_grad(- split_C_cub(var),G.dXc,G.dYc)
    dpNHx=dpNHx*mskW
    dpNHy=dpNHy*mskS

if 'ETAN' in str(f2dList):
    jeta = f2dList.index('ETAN')
    v6t = split_C_cub(v3d[jeta,:,:]*G.rAc)
    vbx = 0.5*(v6t[:,:,1:,:]+v6t[:,:,:-1,:])[:,1:,:,:] \
           .reshape((1,nc,nc*6),order='F')/G.rAw
    vby = 0.5*(v6t[:,1:,:,:]+v6t[:,:-1,:,:])[:,:,1:,:] \
           .reshape((1,nc,nc*6),order='F')/G.rAs
    d6t = split_C_cub(G.depth)
    hhx = np.minimum(d6t[:,:,1:,:],d6t[:,:,:-1,:])[:,1:,:,:] \
            .reshape((1,nc,nc*6),order='F')
    hhy = np.minimum(d6t[:,1:,:,:],d6t[:,:-1,:,:])[:,:,1:,:] \
            .reshape((1,nc,nc*6),order='F')
    # when using z* with  older output, need to account for
    # column vertical streaching in computation of vertical
    # viscosity tendency form vertical viscous flux 'VISrI_Um'
    rFacW=hhx; rFacW[hhx==0.]=-1; rFacW=vbx/rFacW
    rFacW[hhx==0.]=0
    rFacW=rFacW+np.ones((nc,ncx))
    rFacS=hhy; rFacS[hhy==0.]=-1; rFacS=vby/rFacS
    rFacS[hhy==0.]=0
    rFacS=rFacS+np.ones((nc,ncx))
else:
    jdps = -1


#-------------------------------------------------------------------------------

gUdp=np.zeros((nr,nc,ncx)); gVdp=gUdp; titUdp=' ? '; titVdp=' ? '
j1,j2,jdph=-1,-1,-1
j1=getListIndex('Um_dPhiX',fldList)
j2=getListIndex('Vm_dPhiY',fldList)
if j1==-1 & j2==-1:
    jdph=-1;
    j1=getListIndex('Um_dPHdx',fldList)
    j2=getListIndex('Vm_dPHdy',fldList)
    if   j1>-1: jdph=j1
    elif j2>-1: jdph=j2
    if jdph > -1 & jdps > -1:
        gUdp=dpx.repmat((nr,1,1))*mskW;
        gVdp=dpy.repmat((nr,1,1))*mskS;
    if jnh > -1: gUdp=gUdp+dpNHx; gVdp=gVdp+dpNHy
else:
  if j1==-1: jdph=j2
  else: jdph=j1

if jdph > -1:
  if j1 > -1:
      gUdp=gUdp+np.squeeze(v4d[j1,:,:,:])
      titUdp=fldList[j1]
  if j2 > -1:
      gVdp=gVdp+np.squeeze(v4d[j2,:,:,:])
      titVdp=fldList[j2]
  if jdps > -1:
    titUdp=titUdp[:-1]+titUdp[-1].upper()
    titVdp=titVdp[:-1]+titVdp[-1].upper()
    print(' titUdp: >%s< ; titVdp: >%s<\n' %(titUdp,titVdp))

#-- Tendencies from implicit vertical viscous fluxes
# Note: will be used to close momentum budget
#   a) if using older output (since 'Um_ImplD' was not there);
#   b) and using implicit viscosity but without implicit bottom friction
# In the latest case (selectImplicitDrag=2,) cannot close the budget
# using older output
print('  --  Tendencies from vertically visc. fluxes --')
juNz = getListIndex('VISrI_Um',fldList)
if juNz>-1:
    var=np.copy(np.squeeze(v4d[juNz,:,:,:]))
    # compute tendency from minus div of vert. fluxes:
    div=np.copy(var); div[:-1,:,:]=div[:-1,:,:]-var[1:,:,:]
    ddz=G.hFacW*np.tile(G.dRf.reshape((nr,1,1)),(1,nc,ncx))
    rdz=np.copy(ddz); rdz[ddz!=0]=1./rdz[ddz!=0]
    gUnuZ= - div*rdz/np.tile(G.rAw*rFacW,(nr,1,1))
    printstats(gUnuZ,fldList[juNz])
    #--
    jj = getListIndex('Um_ImplD',fldList)
    if jj > -1:
        var=np.copy(np.squeeze(v4d[jj,:,:,:]))
        printstats(var,fldList[jj])
        var = var - gUnuZ
        printstats(var,'Diff:2-1')

    print()

jvNz = getListIndex('VISrI_Vm',fldList)
if jvNz>-1:
    var=np.copy(np.squeeze(v4d[jvNz,:,:,:]))
    # compute tendency from minus div of vert. fluxes:
    div=np.copy(var); div[:-1,:,:]=div[:-1,:,:]-var[1:,:,:]
    ddz=G.hFacS*np.tile(G.dRf.reshape((nr,1,1)),(1,nc,ncx))
    rdz=np.copy(ddz); rdz[ddz!=0]=1./rdz[ddz!=0]
    gVnuZ= - div*rdz/np.tile(G.rAs*rFacS,(nr,1,1))
    printstats(gVnuZ,fldList[jvNz])
    #--
    jj = getListIndex('Vm_ImplD',fldList)
    if jj !=-1:
        var=np.copy(np.squeeze(v4d[jj,:,:,:]))
        printstats(var,fldList[jj])
        var = var - gVnuZ
        printstats(var,'Diff:2-1')

    print()

# Here we check that vertical integral of implicit vertical viscous tendency
# match either bottom drag (if using implicit bottom drag) or simply zero.
j1 = getListIndex('Um_ImplD',fldList)
j2 = getListIndex('botTauX' ,f2dList)
if j1>-1 & j2>-1:
    print('  --  Vertically integrated tendencies --');
    bTauX = np.copy(v3d[j2,:,:])
    printstats(bTauX,f2dList[j2])
    var=np.copy(np.squeeze(v4d[j1,:,:,:]))
    # vertical integration:
    ddz=G.hFacW*np.tile(G.dRf.reshape((nr,1,1)),(1,nc,ncx))
    var=rhoConst*((var*ddz).sum(axis=0))*rFacW
    printstats(var,fldList[j1])
    printstats(var-bTauX,'Diff:2-1')
    print()

j1 = getListIndex('Vm_ImplD',fldList)
j2 = getListIndex('botTauY' ,f2dList)
if j1>-1 & j2>-1:
    bTauY = np.copy(v3d[j2,:,:])
    printstats(bTauY,f2dList[j2])
    var=np.copy(np.squeeze(v4d[j1,:,:,:]))
    # vertical integration:
    ddz=G.hFacS*np.tile(G.dRf.reshape((nr,1,1)),(1,nc,ncx))
    var=rhoConst*((var*ddz).sum(axis=0))*rFacS
    printstats(var,fldList[j1])
    printstats(var-bTauY,'Diff:2-1')
    print()

print('  --  Check Mom budget, exp: %s, files: %s & %s, it= %i'
      % (rDir,namF,namFs,nit))

j = getListIndex('TOTUTEND',fldList)
if j > -1:
    dUtot=np.copy(np.squeeze(v4d[j,:,:,:]))/86400.
    printstats(dUtot,fldList[j])
    
j = getListIndex('Um_dPhiX',fldList)
if j == -1: getListIndex('Um_dPHdx',fldList)
if j >-1:
  printstats(gUdp,titUdp)
  gUtot=gUdp

# For each of the following terms
#  a) print some stats of this term
#  b) add to other tendency and print stats of the sum
#  c) substract the sum from total tendency (-> residual) and print stats

printStatsAndSum(('Um_Advec','Um_Ext','Um_Diss','Um_ImplD','AB_gU'),dUtot,gUtot)

print()

j = getListIndex('TOTVTEND',fldList)
if j > -1:
    dVtot=np.copy(np.squeeze(v4d[j,:,:,:]))/86400.
    printstats(dVtot,fldList[j])

j = getListIndex('Vm_dPhiY',fldList)
if j == -1: getListIndex('Vm_dPHdy',fldList)
if j > -1:
  printstats(gVdp,titVdp)
  gVtot=gVdp

printStatsAndSum(('Vm_Advec','Vm_Ext','Vm_Diss','Vm_ImplD','AB_gV'),dVtot,gVtot)
