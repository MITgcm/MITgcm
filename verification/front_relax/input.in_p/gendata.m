% This is a matlab script that generates the input data
prec='real*8';
ieee='b';
kwr=1;

%- pick simple number (easier to compare P & Z coords):
gravity=10; rhoC=1000.;
% conversion from dz (in m) to dp (in Pa)
conv2p=gravity*rhoC;

% Dimensions/resolution of grid (resolution/depth in m)
nx=1; dx=10e3;
ny=32; dy=10e3;
%- set original level thickness:
dz=[50 50 55 60 65 70 80 95 120 155 200 260 320 400 480];

nz=prod(size(dz));
%- add 10 more levels:
nr=nz+10;

% Nominal depth of model (meters)
Ho=sum(dz);
pBot=conv2p*Ho;

% Vertical Coordinates
zf=[0 -cumsum(dz)];
zc=(zf(1:end-1)+zf(2:end))/2;
dp=conv2p*flip(dz,2); drF=dp;
pf=pBot-[0 cumsum(dp)];
pc=(pf(1:end-1)+pf(2:end))/2;

%- after defining centered position (pc), get vertical spacing in between (to use if
%  we want the interface to be @ the middle beetween 2 center --> specifying delRc=)
delRc=zeros(1,nz+1);
delRc(2:nz)=pc(1:nz-1)-pc(2:nz);
delRc(1)=pf(1)-pc(1); delRc(nz+1)=pc(nz)-pf(nz+1);
% need to adjust first (=bottom) value since we will add more levels there:
delRc(1)=2*delRc(1);
fprintf(' delRc='); fprintf(' %4.1f,',delRc*1.e-4); fprintf('\n');
%- set "pfc" assuming interface are @ middle:
pfc=pf; pfc(2:nz)=(pc(1:nz-1)+pc(2:nz))*0.5;
drF=pfc(1:nz)-pfc(2:nz+1);

% Topography (channel), converted to P:
H=pBot*ones(nx,ny);
H(:,end)=0;  % Solid wall in North
namf='bathy_inP.bin';
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w',ieee); fwrite(fid,H,prec); fclose(fid);
end

% Size of domain (m)
Lx=dx*nx;
Ly=dy*(ny-1);  % Solid wall in North

% Variable resolution
y=(1:ny)/(ny-0)-0.5;
dyF=1-0.3*exp( -(5*y).^2 );
%dyF=ones(1,ny);              % Constant resolution
dyF=dyF/sum(dyF(1:ny-1))*Ly;
namf='dy.bin';
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w',ieee); fwrite(fid,dyF,prec); fclose(fid);
end

% Coordinates
xc=((1:nx)-0.5)*dx;
yf=-Ly/2+[0 cumsum(dyF)];
yc=(yf(1:end-1)+yf(2:end))/2;
[X,Y]=ndgrid(xc,yc);

% Stratification
fo=1e-4;
alpha=2e-4;
%gravity=9.81; has been set @ the top
N_over_f=20;
N2=(N_over_f*fo)^2;
dTdz=N2/(alpha*gravity);

slope=1e-3;
dTdy=-slope*dTdz;

for k=1:nz,
 T(:,:,k)=20 +dTdz*zc(k) ...
             +dTdy*Ly*sin(pi*Y/Ly)...
             *exp(-(3*zc(k)/Ho)^2);
end
T(:,end,:)=0;
%- flip k and add 10 more levels:
t2d=zeros(nx,ny,nr); t2d(:,:,[1+nr-nz:nr])=flip(T,3);
namf='Tini_flip.bin';
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w',ieee); fwrite(fid,t2d,prec); fclose(fid);
end

% Salt (passive tracer), only fct of Y:
S=zeros(nx,ny,nz);
for k=1:nz,
%S(:,:,k)=ones(nx,ny)+zc(k)/Ho;  % Linear with z
%S(:,:,k)=exp(2*zc(k)/Ho);       % Exponential with z
%S(1,:,k)=exp(2*yc/Ly);           % Exponential with y
 S(1,:,k)=exp(-(2*yc/Ly).^2);     % Exponential with y
end
S(:,end,:)=0;
%- flip k and add 10 more levels:
s2d1=zeros(nx,ny,nr); s2d1(:,:,[1+nr-nz:nr])=flip(S,3);
namf='Sini_Ydir.bin';
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w',ieee); fwrite(fid,s2d1,prec); fclose(fid);
end

% Salt (passive tracer), centered patch:
% centered location: (yS,zS) ; patch width: (Lh,Lv)
S=zeros(nx,ny,nz);
 yS=yc(16); Lh=Ly/8;
 zS=zc(10); Lv=500;
for k=1:nz,
 S(1,:,k)=exp(-((yc-yS)/Lh).^2)*exp(-((zc(k)-zS)/Lv).^2);  % Exponential with y^2 + z^2
end
S(:,end,:)=0;
%- flip k and add 10 more levels:
s2d2=zeros(nx,ny,nr); s2d2(:,:,[1+nr-nz:nr])=flip(S,3);
namf='Sini_Patch.bin';
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w',ieee); fwrite(fid,s2d2,prec); fclose(fid);
end

figure(1);clf; colormap('jet'); ccB=[0 0];
 subplot(211);
 yax=yc*1.e-3; %- in km
 var=squeeze(T); zax=zc; ccB=[15 21]*1; cInt=[15:0.5:21];
 var=squeeze(t2d(:,:,11:nr)); zax=pc;
%imagesc(yax,zax,var'); if zax(12) < 0, set(gca,'YDir','normal'); end
%pcolor(yax,zax,var'); if zax(12) > 0, set(gca,'YDir','reverse'); end
 contourf(yax,zax,var',cInt); if zax(12) > 0, set(gca,'YDir','reverse'); end
 if ccB(2) > ccB(1), caxis(ccB); end
 colorbar;

 subplot(212);
 var=squeeze(S); zax=zc; ccB=[-1 10]*0.1; cInt=[0:1:20]*5.e-2;
%var=squeeze(s2d2); zax=[[10:-1:1]+pBot pc];
%var=squeeze(s2d1(:,:,11:nr)); zax=pc;
 var=squeeze(s2d2(:,:,11:nr)); zax=pc;
%yax=yf(1:ny)*1.e-3; %- in km
%var=squeeze(uCg); zax=zc; ccB=[-1 10]*8.e-3; cInt=[0:1:20]*4.e-3;
%ccB=[0 0];
%var(:,:)=NaN; var(1:ny-1,2:nz)=log10(locN2); var(1:ny-1,2:nz)=locN2;
%imagesc(yax,zax,var'); if zax(12) < 0, set(gca,'YDir','normal'); end
%pcolor(yax,zax,var'); if zax(12) > 0, set(gca,'YDir','reverse'); end
 contourf(yax,zax,var',cInt); if zax(12) > 0, set(gca,'YDir','reverse'); end
 if ccB(2) > ccB(1), caxis(ccB); end
 colorbar;

figure(2); clf;
 yax=[1:ny]; yax=yax-mean(yax);
 subplot(311);
 plot(yax,dyF,'b-');
 AA=axis; AA(1:2)=[-ny/2 ny/2]; axis(AA);
 grid on;

 subplot(613);
 plot(yax,H,'b-');
 AA=axis; AA(1:2)=[-ny/2 ny/2]; axis(AA);
 grid on;

 subplot(223);
 zax=[1:nz];
 plot(pc,zax,'kx');
 zax=0.5+[0:nz];
 hold on;
 plot(pfc,zax,'b-');
 plot(pf,zax,'r-');
 hold off;
 grid
 title('pc , pf , pfc');

%- plot relative difference:
 var=(pfc-pf)./delRc;
 subplot(224);
 plot(var,zax,'b-');
 grid
 title(' (pfc - pf)/dpC');

