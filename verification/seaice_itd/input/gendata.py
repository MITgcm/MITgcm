# simple python script to generate input data
# this script is very similar to $ROOTDIR/verification/offline_exf_seaice/input/gendata.m
#
import numpy as np

kwr, kprt =1, 0
nx, ny, nr, nt = 80, 42, 3, 1

xc = np.arange(nx)
xc = xc - xc.mean()
yc = np.arange(ny)+0.5

# ------------------------------------------------------

def writefield(fname,data):
    import sys
    print 'write to file: '+fname
    if sys.byteorder == 'little': data.byteswap(True)
    fid = open(fname,"wb")
    data.tofile(fid)
    fid.close()


windx =   10.
H0    = -100. 

# pure channel
namf='channel.bin'
depth=H0*np.ones((ny,nx),dtype='float64'); depth[0,:] = 0.
#if kwr > 0: writefield(namf,depth)

# triangular obstacle
namf='bathy_3c.bin';
depth=H0*np.ones((ny,nx),dtype='float64'); depth[0,:]=0.;
msk=np.tile(np.abs(xc),(ny,1)) + np.tile(yc,(nx,1)).T
depth[msk < 24]=0.;
y2d=np.tile(yc,(nx,1)).T
depth[y2d > ny/2.]=H0;
if kwr > 0: writefield(namf,depth)

# wind field
namf='windx.bin';
wnd=windx*np.ones((nt,ny,nx),dtype='float64')
if kwr > 0: writefield(namf,wnd)

#- file name convention: "const_{xx}.bin" <-> uniform value = xx (in percent)
namf='const_00.bin';
fld=np.zeros((nt,ny,nx),dtype='float64')
if kwr > 0: writefield(namf,fld)

namf='const100.bin'; w0=1.;
var=w0*np.ones((nt,ny,nx),dtype='float64')
if kwr > 0: writefield(namf,var)

namf='const+20.bin'; w0=0.2;
var=w0*np.ones((nt,ny,nx),dtype='float64')
#if kwr > 0: writefield(namf,var)

namf='heff_quartic.bin'
hf_y = 8.*(yc/ny)**4
hf_y.resize((1,ny,1))
hf=np.tile(hf_y,(nt,1,nx))
if kwr > 0: writefield(namf,hf)

#------------------------------------------------------

namf='ice0_area.bin'; iceC0=1.;
iceConc=iceC0*np.ones((ny,nx),dtype='float64'); iceConc[0,:]=0;
iceConc[1,:] =0.00*iceC0;
iceConc[2,:] =0.10*iceC0;
iceConc[-1,:]=0.00*iceC0;
iceConc[-2,:]=0.01*iceC0;
#if kwr > 0: writefield(namf,iceConc)

namf='ice0_heff.bin'; iceH0=0.2;
iceVol=iceConc*iceH0;
#if kwr > 0: writefield(namf,iceVol)

#------------------------------------------------------

dsw0=100;
namf='dsw_'+str(dsw0)+'.bin'
fld=dsw0*np.ones((nt,ny,nx),dtype='float64')
if kwr > 0: writefield(namf,fld)

dlw0=250;
namf='dlw_'+str(dlw0)+'.bin'
fld=dlw0*np.ones((nt,ny,nx),dtype='float64')
if kwr > 0: writefield(namf,fld)

cel2K=273.15; dtx=4; #- dtx = amplitude of air temp variations in X-dir
ta_x=cel2K + dtx*np.sin(np.pi*(1+2*xc/nx));
ta=np.tile(ta_x,(nt,ny,1))
namf='tair_'+str(dtx)+'x.bin'
if kwr > 0: writefield(namf,ta)

cvapor_fac     =   640380.000
cvapor_exp     =     5107.400
atmrho         =        1.200
rh=70; #- specific humid <--> 70.% relative humid
tmpbulk = cvapor_fac*np.exp(-cvapor_exp/ta_x);
qa_x = (rh/100.)*tmpbulk/atmrho
qa=np.tile(qa_x,(nt,ny,1))
namf='qa'+str(rh)+'_'+str(dtx)+'x.bin'
if kwr > 0: writefield(namf,qa)

#- salinity
sCst=30
so=sCst*np.ones((nt,ny,nx),dtype='float64')
namf='socn.bin'
#if kwr > 0: writefield(namf,so)

muTf = 5.4e-2;
tfreeze=-muTf*sCst;
print 'T-freeze = {0:10.6f}'.format(tfreeze)
#- parabolic profile in Y, max @ j=4, min @ j=ny, amplitude=1.K
to_y=(yc-3.5)/(ny-4)
to_y=tfreeze+0.5-to_y*to_y
mnV=to_y.min(); MxV=to_y.max(); Avr=to_y[1:].mean()
print ' SST* av,mn,Mx: {0:9.6f} , {1:9.6f} , {2:9.6f} , {3:9.6f}'.format(Avr,mnV,MxV,MxV-mnV)
to_y.resize((1,ny,1))
to=np.tile(to_y,(nt,1,nx))
namf='tocn.bin';
if kwr > 0: writefield(namf,to)

#-- make some plots to check: ----------------

import matplotlib.pyplot as plt
hScal=np.asarray([-1.1, 0.1])*np.abs(H0);
xg = xc-.5
yg = yc-.5
plt.figure(1); plt.clf()
plt.subplot(211)
var=depth
plt.pcolormesh(xg,yg,var)
plt.colorbar()
plt.title('Depth [m]')

plt.subplot(413)
var=depth
j1=1
j2=ny/2-1
j3=j2+1
plt.plot(xc,var[j1,:],'k-',label=str(j1))
plt.plot(xc,var[j2,:],'ro-',label=str(j2))
plt.plot(xc,var[j3,:],'b-',label=str(j3))
plt.axis([-nx/2, nx/2, hScal[0], hScal[1]])
plt.grid()
plt.legend()
plt.title('Depth @ j= cst');

plt.subplot(414);
i=nx/2-1
plt.plot(yc,var[:,i],'k-')
plt.axis([0,ny,H0*1.1,-H0*.1]);
plt.grid()
plt.title(['Depth @ i=',str(i)]);

#--
dewPt=(qa_x*atmrho)/cvapor_fac;
dewPt=-cvapor_exp/np.log(dewPt);

plt.figure(2);plt.clf();
plt.subplot(311)
plt.plot(xc,ta_x-cel2K,'r-',label='ta',linewidth = 1)
plt.plot(xc,dewPt-cel2K,'b-',label='dew',linewidth = 1)
plt.plot(xc,tfreeze*np.ones((nx,1)),'k-',linewidth = 1)
AA=plt.axis()
plt.axis([-nx/2, nx/2, AA[2], AA[3]]);
plt.legend()
plt.grid()
plt.xlabel('X')
plt.title('Air Temp ($^\circ$C): del-Temp-X = '+str(dtx)+' , RH= '+str(rh));
plt.subplot(312)
plt.plot(yc,to_y.flatten(),'b-',linewidth = 1)
plt.plot(yc,tfreeze*np.ones((ny,1)),'k-',linewidth = 1)
AA=plt.axis()
plt.plot([1,1],AA[2:],'k',linewidth=2.)
plt.axis([0, ny, AA[2], AA[3]])
plt.grid()
plt.xlabel('Y')
plt.title('Ocean Temp $^\circ$C');

plt.subplot(313)
var=iceConc[:,0]
plt.semilogy(yc,var,'b-x',linewidth = 1,label='iceC')
var=iceVol[:,0]
plt.semilogy(yc,var,'r-x',linewidth = 1,label='hEff')
AA=plt.axis()
plt.plot([1,1],AA[2:],'k',linewidth=2.)
plt.axis([0, ny, 0, 2*iceC0])
plt.grid()
plt.xlabel('Y')
plt.legend(loc='lower center')
plt.title('Initial ice in Channel : y-section');
#
if kprt == 1:
    f=2
    namfig='forcing_{0:02g}'.format(f)
    print ' print fig= {0:2g} to file: '.format(f) + namfig
    print(f,'-depsc2',namfig); fprintf('\n');

    
plt.figure(3);plt.clf();
plt.subplot(311)
plt.pcolormesh(xg,yg,iceConc,vmin=-0.1, vmax=1.2)
plt.colorbar()
plt.title('Ice Concentration in Channel');

plt.subplot(312)
plt.pcolormesh(xg,yg,iceVol,vmin=-1./50., vmax=12./50.)
plt.colorbar()
plt.title('Effective ice thickness in Channel');

plt.subplot(313)
plt.semilogy(yc,iceConc[:,0],'b-x',label='iceC')
plt.semilogy(yc,iceVol[:,0],'r-x',label='hEff')
AA=plt.axis(); plt.axis([0,ny,0,2*iceC0]);
plt.grid()
plt.legend(loc='lower center')
plt.title('Initial ice in Channel : y-section');

plt.show()
