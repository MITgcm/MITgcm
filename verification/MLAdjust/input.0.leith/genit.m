%function geninit()
%  This function generates initial condition files for the mitgcm.
% if init_vel=1, then the front problem is used
% if init_vel=0, then resting (other than small symmetry-breaking
% random noise) initial conditions are used.

nx=50
ny=nx/2+1
nz=40
init_vel=1;
dxspacing=1000
dyspacing=dxspacing

Lx=dxspacing*nx
Ly=dyspacing*ny

%-- Params
g=9.81;
tAlpha=-2e-4;
f0=7.29e-5;
rho=1035
day=24*60^2;
prec='real*8';
ieee='b';

H=200;            %Max depth

%-- Grid: x
dx=ones(1,nx);                                  % uniform resolution
dx=dx*Lx/sum(dx); 
xf=cumsum([0 dx]); % Face x points
xc=(xf(1:end-1)+xf(2:end))/2; % Centered x points

%-- Grid: y
dy=ones(1,ny);                                  % uniform resolution
dy=dy*Ly/sum(dy); 
yf=cumsum([0 dy]);  % Face y-points
yc=(yf(1:end-1)+yf(2:end))/2;  %Centered y-points
L=yc(end)-yc(1);	% this takes into account the wall of topography!!!

%-- Grid: z
dh=H/nz*ones(1,nz);
zf=-round(cumsum([0 dh])/1)*1;   % Face z points
dh=-diff(zf);
zc=(zf(1:end-1)+zf(2:end))/2;  % centered z points
nz=length(dh);
H=sum(dh)

[XT,YT,ZT]=ndgrid(xc,yc,zc); % This is the centered, temperature grid.
[XU,YU,ZU]=ndgrid(xf,yc,zc); % This is the U grid.
[XV,YV,ZV]=ndgrid(xc,yf,zc); % This is the V grid.
[XW,YW,ZW]=ndgrid(xc,yc,zf); % This is the W grid.
[XB,YB]=ndgrid(xc,yc); % This is the Bathymetry grid.

% Stratification Params
Dml=200;             % Mixed-layer Depth
D=Dml;
Lf=2*1e3;          % Half-Width of Front
y0=yc(round(length(yc)/2));     % centered location of Front

Ms=(2e-4)^2     %db/dy  2e-4 is about 1 K/50 km
Ns=9*Ms^2/f0^2  %db/dz

deltatheta=Lf*Ms/g/tAlpha
T0=17
dthetadz=-Ns/g/tAlpha

%-- Bathymetry
% Bathymetry is on Xc, Yc
hh=ones(size(XB));
hh(:,end)=0*hh(:,end);
hh=-H*hh;

figure(1)
subplot(221)
pcolor(XB/1e3,YB/1e3,hh)
axis equal

subplot(223)
plot(xc/1e3,dx/1e3,'.'); xlabel('X (km)');ylabel('\Delta x (km)')
title('\Delta x')

subplot(222)
plot(dy/1e3,yc/1e3,'.'); ylabel('Y (km)');xlabel('\Delta y (km)')
title('\Delta y')

subplot(224)
plot(dh,zc,'.'); ylabel('Z (m)');xlabel('\Delta z (m)')
title('\Delta z')

fid=fopen('topo_sl.bin','w',ieee); fwrite(fid,hh,prec); fclose(fid);

% Initial Temp Profile
figure(2)
subplot(4,2,1)
theta=T0+dthetadz*(ZT-ZT(1,1,end))+deltatheta*tanh((YT-y0)/Lf)/2;

% Impose a strong initial and restoring mixed layer to Dml
i=1;
iml=1;

while ((ZT(1,1,iml)>-Dml)&(iml<length(ZT(1,1,:))))
  iml=iml+1;
end

while (i<iml)
  theta(:,:,i)=theta(:,:,iml);
  i=i+1;
end

[ZT(1,1,1) theta(1,1,1) theta(1,end,1) ;ZT(1,1,i) theta(1,1,i) theta(1,end,i);ZT(1,1,end) theta(1,1,end) theta(1,end,end)]


subplot(4,2,1)
[h,c]=contourf(squeeze(YT(1,:,:))/1e3,squeeze(ZT(1,:,:)),squeeze(theta(1,:,:)));
axis([(yc(round(length(yc)/2))-1.5*Lf)/1e3 (yc(round(length(yc)/2))+1.5*Lf)/1e3 -Dml 0])
colorbar
title('Potl Temp')
xlabel('y (km)');ylabel('z (m)')

subplot(4,2,2)
[h,c]=contourf(squeeze(YT(1,:,:))/1e3,squeeze(ZT(1,:,:)),squeeze(theta(1,:,:)));
colorbar
title('Potl Temp')
xlabel('y (km)');ylabel('z (m)')


subplot(4,2,3)
dens=theta*rho*tAlpha+rho;
[h,c]=contour(squeeze(YT(1,:,:))/1e3,squeeze(ZT(1,:,:)),squeeze(dens(1,:,:)));
title('Density')
xlabel('y (km)');ylabel('z (m)')
axis([(yc(round(length(yc)/2))-1.5*Lf)/1e3 (yc(round(length(yc)/2))+1.5*Lf)/1e3 -Dml 0])

%Spice

x1=xc(round(length(xc)/4));     % centered location of Front #1
x2=xc(round(3*length(xc)/4));     % centered location of Front #2
spice=T0+dthetadz*(ZT-ZT(1,1,end))+deltatheta*(tanh((XT-x1)/Lf)-tanh((XT-x2)/Lf)-1)/2;

i=1
while (i<iml)
  spice(:,:,i)=spice(:,:,iml);
  i=i+1;
end


subplot(4,2,8)
[h,c]=contourf(squeeze(XT(:,1,:))/1e3,squeeze(ZT(:,1,:)),squeeze(spice(:,1,:)));
title('Spice')
xlabel('x (km)');ylabel('z (m)')

subplot(4,2,4)
[h,c]=contour(squeeze(YT(1,:,:))/1e3,squeeze(ZT(1,:,:)),squeeze(dens(1,:,:)));
title('Density')
xlabel('y (km)');ylabel('z (m)')

subplot(4,2,7)
decay=dyspacing;

figure(2)
plot(YB(1,:)/1e3,hh(1,:))
title('topography')
xlabel('x (km)');ylabel('value')

%-- Perturb Initial temperature
pert=rand(size(theta(:,:,1)));
pert=1e-5*(pert-0.5);
for i=1:length(theta(1,1,:))
  theta(:,:,i)=theta(:,:,i)+pert;
end

fid=fopen('thetaInitial.bin','w',ieee);
fwrite(fid,theta,prec); fclose(fid);

fid=fopen('spiceInitial.bin','w',ieee);
fwrite(fid,spice,prec); fclose(fid);


