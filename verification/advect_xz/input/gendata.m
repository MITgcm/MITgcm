% This is a matlab script that generates the input data
prec='real*8';
ieee='b';
w2file=1; %- do write to file (1=y/0=no)

%- Dimensions of grid
nx=20;
ny=1;
nz=20;

%- Horizontal & vertical resolution (m):
dx=1.e4; dy=dx; dz=1.e2;
% full size of the domain:
Lx=nx*dx;
H=nz*dz ;

%- grid point coordinate :
xf=[0:nx]*dx;
xc=(xf(1:nx)+xf(2:nx+1))/2;
zc=-dz/2:-dz:-H;
zf=0:-dz:-H;

wf=' write file: ';
%- bathymetry :
zb=-H*ones(nx,1);
zb=-H*(1.-xc'/Lx/2);

if w2file,
 bname='bathy_slope.bin';
 fid=fopen(bname,'w',ieee); fwrite(fid,zb,prec); fclose(fid); 
 fprintf([wf,bname,'\n']);
end

%- partial cell filling (hFac):
hFac=ones(nx,1)*zf(1,[2:nz+1]);
hFac=max(hFac,zb*ones(1,nz));
hFac=ones(nx,1)*zf(1,[1:nz])-hFac;
hFac=hFac/dz; hFac=max(0,hFac);

rhFc=reshape(hFac,[nx*nz 1]); 
[IK]=find(rhFc > 0); rhFc(IK)=1./rhFc(IK);
rhFc=reshape(rhFc,[nx nz]);

%- horizontal flow field is defined from stream-function psi :
psi=zeros(nx+1,nz+1);
psi1=sin(pi*xf'/Lx)*ones(1,nz+1);
zu=zeros(nx+1,1); zu(2:nx)=max(zb(2:nx),zb(1:nx-1));
rHu=zeros(nx+1,1); rHu(2:nx)=-1./zu(2:nx);
psi2=max( ones(nx+1,1)*zf, zu*ones(1,nz+1) );
psi2=psi2.*(rHu*ones(1,nz+1));
psi2=sin(pi*psi2);
psi=H*psi1.*psi2;

uTrans=psi(:,[1:nz])-psi(:,[2:nz+1]);
u0=uTrans(1:nx,:).*rhFc/dz;

%- add small, vertically uniform, divergent component:
ud=psi1(:,1).*rHu;
ud=ud*H/160.;
u1=ud(1:nx,1)*ones(1,nz);
u1(find(hFac==0.))=0.;

if w2file,
 uname='Uvel.bin';
 fid=fopen(uname,'w',ieee); fwrite(fid,u0,prec); fclose(fid); 
 fprintf([wf,uname,'\n']);
 uname='Udiv.bin';
 fid=fopen(uname,'w',ieee); fwrite(fid,u0+u1,prec); fclose(fid); 
 fprintf([wf,uname,'\n']);
end

%- Initial Temperature : Gaussian bump :
t0=zeros(nx,nz);
%  position of the center
xM=xf(5); zM=zf(6); 
%  size of the patch  
dX=2*dx; dH=2*dz; 
%  amplitude:
tA=exp(-0.5*(35/20)^2);
%
r1=(xc'-xM)/dX; r2=(zc-zM)/dH;
r1=r1.*r1; r2=r2.*r2;
rD=r1*ones(1,nz)+ones(nx,1)*r2;
t0=tA*exp(-rD/2);

if w2file,
 tname='Tini_G.bin';
 fid=fopen(tname,'w',ieee); fwrite(fid,t0,prec); fclose(fid); 
 fprintf([wf,tname,'\n']);
end

%return

%- make plots to check:
figure(1);clf;
 subplot(211)
%imagesc(xf,zf,psi1'); set(gca,'YDir','normal'); colorbar;
 imagesc(xf,zf,psi'); set(gca,'YDir','normal'); colorbar;
 hold on ; plot(xc,zb,'b-'); hold off ; grid
 title('Stream-Function [m^2/s]');
 subplot(212)
%imagesc(xf(1:nx),zc,u1'); set(gca,'YDir','normal'); colorbar;
 imagesc(xf(1:nx),zc,u0'); set(gca,'YDir','normal'); colorbar;
 hold on ; plot(xc,zb,'r-'); hold off ; grid
 title('U velocity [m/s]');

figure(2);clf;
 subplot(211)
 imagesc(xc,zc,rhFc'); set(gca,'YDir','normal'); colorbar;
 hold on ; plot(xc,zb,'k-'); hold off ; grid
 title('Recip hFac');
 subplot(212)
 imagesc(xc,zc,t0'); set(gca,'YDir','normal'); colorbar;
 hold on ; plot(xc,zb,'r-'); hold off ; grid
 title('Initial Theta');

return
