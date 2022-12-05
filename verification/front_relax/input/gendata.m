% This is a matlab script that generates the input data
prec='real*8';
ieee='b';
kwr=1;

%- pick simple number (easier to compare P & Z coords):
gravity=10; rhoC=1000.;

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

% Vertical Coordinates
zf=[0 -cumsum(dz)];
zc=(zf(1:end-1)+zf(2:end))/2;
drF=dz;
%- after defining centered position (zc), get vertical spacing in between (to use if
%  we want the interface to be @ the middle beetween 2 center --> specifying delRc=)
delRc=zeros(1,nz+1);
delRc(2:nz)=zc(1:nz-1)-zc(2:nz);
delRc(1)=zf(1)-zc(1); delRc(nz+1)=zc(nz)-zf(nz+1);
% need to adjust nz+1 value since we will add more levels there:
 delRc(nz+1)=2*delRc(nz+1);
fprintf(' delRc='); fprintf(' %4.1f,',delRc); fprintf('\n');
%- set "zfc" assuming interface are @ middle:
zfc=zf; zfc(2:nz)=(zc(1:nz-1)+zc(2:nz))*0.5;
drF=zfc(1:nz)-zfc(2:nz+1);

% Topography (channel):
H=-Ho*ones(nx,ny);
H(:,end)=0;  % Solid wall in North
namf='bathy_inZ.bin';
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
dTy=(T(1,[2:ny],:)-T(1,[1:ny-1],:));
T(:,end,:)=0;
%- add 10 more levels:
t2d=zeros(nx,ny,nr); t2d(:,:,[1:nz])=T;
namf='Tini_+10l.bin';
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w',ieee); fwrite(fid,t2d,prec); fclose(fid);
end

%- compute local N2:
dTz=(T(1,:,[1:nz-1])-T(1,:,[2:nz]));
dTz=squeeze(dTz)./repmat(delRc(2:nz),[ny 1]);
locN2=dTz(1:ny-1,:)*(alpha*gravity); MxV=max(locN2(:));
fprintf(' max(N2) = %6.4e ; max(c1) = %6.4f\n',MxV,Ho*sqrt(MxV)/pi);

%- Thermal wind
dyC=zeros(ny+1,1); dyC(1)=dyF(1); dyC(ny+1)=dyF(ny);
dyC(2:ny,1)=(dyF(2:ny)+dyF(1:ny-1))*0.5;
dbdy=zeros(ny+1,nz);
dbdy(2:ny,:)=gravity*alpha*squeeze(dTy);
dbdy=dbdy./repmat(dyC,[1 nz]);
dbdy(1,:)=dbdy(2,:); dbdy(ny+1,:)=dbdy(ny,:);
ug=zeros(ny+1,nz+1);
for k=nz:-1:1;
 ug(:,k) = ug(:,k+1) - dbdy(:,k)*drF(k)/fo ;
end
uCg=ug([1:ny],[1:nz])+ug([2:ny+1],[2:nz+1])  ...
   +ug([2:ny+1],[1:nz])+ug([1:ny],[2:nz+1]);
uCg=uCg/4; uCg(end,:)=0;
%- add 10 more levels:
u2d=zeros(nx,ny,nr); u2d(:,:,[1:nz])=reshape(uCg,[1 ny nz]);
namf='Uini_geos.bin';
if kwr > 1,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w',ieee); fwrite(fid,u2d,prec); fclose(fid);
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
%- add 10 more levels:
s2d1=zeros(nx,ny,nr); s2d1(:,:,[1:nz])=S;
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
%- add 10 more levels:
s2d2=zeros(nx,ny,nr); s2d2(:,:,[1:nz])=S;
namf='Sini_Patch.bin';
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w',ieee); fwrite(fid,s2d2,prec); fclose(fid);
end

figure(1);clf; colormap('jet'); ccB=[0 0];
 subplot(211);
 yax=yc*1.e-3; %- in km
 var=squeeze(T); zax=zc; ccB=[15 21]*1; cInt=[15:0.5:21];
 var=squeeze(t2d(:,:,1:nz)); zax=zc;
%imagesc(yax,zax,var'); if zax(12) < 0, set(gca,'YDir','normal'); end
%pcolor(yax,zax,var'); if zax(12) > 0, set(gca,'YDir','reverse'); end
 contourf(yax,zax,var',cInt); if zax(12) > 0, set(gca,'YDir','reverse'); end
 if ccB(2) > ccB(1), caxis(ccB); end
 colorbar;

 subplot(212);
%var=squeeze(S); zax=zc; ccB=[-1 11]*0.1; cInt=[0:1:10]*0.1;
%var=squeeze(s2d1(:,:,1:nz)); ccB=[-1 10]*0.1; cInt=[0:1:20]*5.e-2;
 var=squeeze(s2d2(:,:,1:nz)); ccB=[-1 10]*0.1; cInt=[0:1:20]*5.e-2;
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
 title('dyF');

 subplot(613);
 plot(yax,H,'b-');
 AA=axis; AA(1:2)=[-ny/2 ny/2]; axis(AA);
 grid on;
 title('H');

 subplot(223);
 zax=-[1:nz];
 plot(zc,zax,'kx');
 zax=0.5-[1:nz+1];
 hold on;
 plot(zf,zax,'b-');
 plot(zfc,zax,'r-');
 hold off;
 grid
 title('zc , zf , zfc');

%- plot relative difference:
 var=(zfc-zf)./delRc;
 subplot(224);
 plot(var,zax,'b-');
 grid
 title(' (zfc - zf)/dzC');

