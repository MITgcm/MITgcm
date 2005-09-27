%function geninit()
%  This function generates initial condition files for the mitgcm.
% if init_vel=1, then the front problem is used
% if init_vel=0, then resting (other than small symmetry-breaking
% random noise) initial conditions are used.

nx=50
ny=nx/2+1
nz=40
init_vel=1;
dxspacing=1e3
dyspacing=dxspacing

Lx=dxspacing*nx
Ly=dyspacing*ny 

%-- Params
g=9.81;
tAlpha=-2e-4;
f0=7.29e-5;
rho=1035
day=24*60^2;
time_scale=Inf %day/24/4
surf_time_scale=Inf
tau0=0.0           % Wind forcing magnitude...
daylength=day/3;    % Length of daytime heating
qconst=0          % Surface steady heat flux W/m^2 (positive for ocean cool)

H=2000;            %Max depth

%-- Grid: x
dx_ratio=30; dx_trans=.05; dx_min=100;
xn=(0.5:nx)/nx;
dx=ones(1,nx);                                  % uniform resolution
dx=dx*Lx/sum(dx); 
xf=cumsum([0 dx]); % Face x points
xc=(xf(1:end-1)+xf(2:end))/2; % Centered x points

%-- Grid: y
dy_ratio=30; dy_trans=.02; dy_min=100;
yn=(0.5:ny)/ny;
dy=ones(1,ny);                                  % uniform resolution
dy=dy*Ly/sum(dy); 
yf=cumsum([0 dy]);  % Face y-points
yc=(yf(1:end-1)+yf(2:end))/2;  %Centered y-points
L=yc(end)-yc(1);	% this takes into account the wall of topography!!!

%-- Grid: z

dh=H/nz*ones(1,nz);
%for i=21:38
%  dh(i)=dh(i-1)*1.1917;
%end

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
Lf=64*1e3;          % Half-Width of Front
%Lf=50*dyspacing;          % Half-Width of Front
y0=yc(round(length(yc)/2));     % centered location of Front


Ms=(2*f0)^2     %db/dy  2e-4 is about 1 K/50 km
Nsml=0*(f0)^2     %db/dz
Nsint=(32*f0)^2      %db/dz in interior

Ri=Nsml*f0^2/Ms^2;
Ld=max(Nsml^.5/f0*Dml, Ms/f0^2*Dml)
Ls=(f0^2/Ms/Dml*sqrt(5/2/(1+max(Ri,1))))^-1*2*pi

Riint=Nsint*f0^2/Ms^2;
Ldint=max(Nsint^.5/f0*H, Ms/f0^2*H)
Lsint=(f0^2/Ms/H*sqrt(5/2/(1+max(Riint,1))))^-1*2*pi

Resfacs=[Lf/Ld Lf/Ls Ls/dxspacing Ld/dxspacing]

dxspacing
optdxspacing=round(Ls*0.05)

deltatheta=2*Lf*Ms/g/tAlpha
T0=17
dthetadzml=-Nsml/g/tAlpha
dthetadzint=-Nsint/g/tAlpha

%-- Bathymetry
% Bathymetry is on Xc, Yc
hh=ones(size(XB));

% Variable Bathymetry
% hh(:,:)=hh(:,:)-2*max(0,abs(YB/L-1/2)-3/8);

hh(:,end)=0*hh(:,end);


hh=-H*hh;

Ltop=L/2;

figure(1)
subplot(221)
pcolor(XB/1e3,YB/1e3,hh)


subplot(223)
plot(xc/1e3,dx/1e3,'.'); xlabel('X (km)');ylabel('\Delta x (km)')
title('\Delta x')

subplot(222)
plot(dy/1e3,yc/1e3,'.'); ylabel('Y (km)');xlabel('\Delta y (km)')
title('\Delta y')

subplot(224)
plot(dh,zc,'.'); ylabel('Z (m)');xlabel('\Delta z (m)')
title('\Delta z')

% Surface maximum diurnal heat flux (gets multiplied by cos(2pi*t/day)

figure(3)
deltaT=3600;
t=deltaT:deltaT:86400*2;

q1=sum(max(cos(2*pi*t/day),cos(pi*daylength/day))-cos(pi*daylength/day))/length(t);

qdiur=-qconst/q1;

q1=qconst+qdiur*(max(cos(2*pi*t/day),cos(pi*daylength/day))-cos(pi*daylength/day));

plot(t,q1)

sprintf('Set data param fuconst to %0.5g',tau0)
sprintf('Set data param daylength to %0.5g',daylength)
sprintf('Set data param qdiur to %0.5g',qdiur)
sprintf('Set data param qconst to %0.5g',qconst)

xlabel('t (days)'); ylabel('Q Flux (pos=ocean cool)');title('Heat flux')

fid=fopen('dx_variable.bin','w','b'); fwrite(fid,dx,'real*8'); fclose(fid);
fid=fopen('dy_variable.bin','w','b'); fwrite(fid,dy,'real*8'); fclose(fid);
fid=fopen('topo_sl.bin','w','b'); fwrite(fid,hh,'real*8'); fclose(fid);

% Initial Temp Profile

figure(2)
subplot(4,2,1)

theta=T0+dthetadzint*(ZT+Dml)+deltatheta*tanh((YT-y0)/Lf)/2;

% Impose a strong Dml
i=1;
iml=1;

while ((ZT(1,1,iml)>-Dml)&(iml<length(ZT(1,1,:))))
  iml=iml+1;
end

while (i<iml)
  theta(:,:,i)=T0+dthetadzml*(ZT(:,:,i)+Dml)+deltatheta*tanh((YT(:,:,i)-y0)/Lf)/2;
  i=i+1;
end

[ZT(1,1,1) theta(1,1,1) theta(1,end,1) ;ZT(1,1,i) theta(1,1,i) theta(1,end,i);ZT(1,1,end) theta(1,1,end) theta(1,end,end)]

Tvec=15:.1:18;

subplot(4,2,1)
[h,c]=contourf(squeeze(XT(:,1,:))/1e3,squeeze(ZT(:,1,:)),squeeze(theta(:,1,:)),Tvec);
title('Potl Temp')
xlabel('x (km)');ylabel('z (m)')
fid=fopen('thetaRestoreFile.bin','w','b'); fwrite(fid,theta,'real*8'); fclose(fid);

subplot(4,2,2)
[h,c]=contourf(squeeze(YT(1,:,:))/1e3,squeeze(ZT(1,:,:)),squeeze(theta(1,:,:)),Tvec);
colorbar
title('Potl Temp')
xlabel('y (km)');ylabel('z (m)')
fid=fopen('thetaRestoreFile.bin','w','b'); fwrite(fid,theta,'real*8'); fclose(fid);

Rvec=1031:.05:1032;

subplot(4,2,3)
dens=theta*rho*tAlpha+rho;
[h,c]=contour(squeeze(XT(:,1,:))/1e3,squeeze(ZT(:,1,:)),squeeze(dens(:,1,:)),Rvec);
title('Density')
xlabel('x (km)');ylabel('z (m)')

subplot(4,2,4)
[h,c]=contour(squeeze(YT(1,:,:))/1e3,squeeze(ZT(1,:,:)),squeeze(dens(1,:,:)),Rvec);
title('Density')
colorbar
xlabel('y (km)');ylabel('z (m)')

%Spice

x1=xc(round(length(xc)/4));     % centered location of Front #1
x2=xc(round(3*length(xc)/4));     % centered location of Front #2

%spice=((SL+SR)+(SR-SL)*tanh((ZT-Dml/2)/Dml))/2

spice=T0+dthetadzint*(ZT+Dml)+deltatheta*(tanh((XT-x1)/Lf)-tanh((XT-x2)/Lf)-1)/2;

i=1

while (i<iml)
  spice(:,:,i)=T0+dthetadzml*(ZT(:,:,i)+Dml)+deltatheta*(tanh((XT(:,:,i)-x1)/Lf)-tanh((XT(:,:,i)-x2)/Lf)-1)/2;
  i=i+1;
end

fid=fopen('spiceRestoreFile.bin','w','b'); fwrite(fid,spice,'real*8'); fclose(fid);

subplot(4,2,8)
[h,c]=contourf(squeeze(XT(:,1,:))/1e3,squeeze(ZT(:,1,:)),squeeze(spice(:,1,:)),Tvec);
%clabel(h,c)
title('Spice')
xlabel('x (km)');ylabel('z (m)')

%clabel(h,c);

subplot(4,2,7)
invtime=0*YT;
decay=dyspacing;
invtime=1/time_scale*(exp(-YT/decay+YT(1,1,1)/decay)+exp(YT/decay-YT(1,end-1,1)/decay));
% Surface Restoring
invtime(:,:,1)=invtime(:,:,1)+1/surf_time_scale;

figure(4)
subplot(2,1,1)
plot(squeeze(YT(1,1:end-1,:)/1e3),squeeze(invtime(1,1:end-1,:)*86400))
ylabel('inverse restoring time (1/day)') 
xlabel('Y (km)')

subplot(2,1,2)
plot(squeeze(invtime(1,1:end-1,:)*86400)',squeeze(ZT(1,1:end-1,:))')
xlabel('inverse restoring time (1/day)') 
ylabel('depth')

figure(2)
plot(YB(1,:)/1e3,hh(1,:))
title('topography')
xlabel('x (km)');ylabel('value')

fid=fopen('invtimeRestoreFile.bin','w','b'); fwrite(fid,invtime,'real*8'); fclose(fid);

%-- Perturb Initial temperature

pert=rand(size(theta(:,:,1)));

pert=1e-4*(pert-0.5);

for i=1:length(theta(1,1,:))
  theta(:,:,i)=theta(:,:,i)+pert;
end

fid=fopen('thetaInitial.bin','w','b');
fwrite(fid,theta,'real*8'); fclose(fid);

fid=fopen('spiceInitial.bin','w','b');
fwrite(fid,spice,'real*8'); fclose(fid);

%-- Thermal wind
U = 0*XU;

for k=(nz-1):-1:1
  U(1:end-1,1:end-1,k) = U(1:end-1,1:end-1,k+1)+dh(k)*g/f0/rho*diff(dens(:,:,k),1,2)./diff(YU(1:end-1,:,k+1),1,2);
  U(:,end,k) = 0;      % Get endpoint
  U(end,:,k) = U(1,:,k);             % In the wall
end

% Make initial state Momentumless (at least boussinesq, good approx)

[imax,jmax,kmax]=size(U);

for i=1:imax
 for j=1:jmax
  U(i,j,:)=U(i,j,:)-mean(U(i,j,:));
 end
end

timestep=dxspacing/max(abs(U(:)))/3.

subplot(4,2,5)
[h,c]=contour(squeeze(XU(:,1,:))/1e3,squeeze(ZU(:,1,:)),squeeze(U(:,1,:)),-1:.02:1);
title('U');xlabel('x (km)');ylabel('z (m)')

subplot(4,2,6)
[h,c]=contour(squeeze(YU(1,:,:))/1e3,squeeze(ZU(1,:,:)),squeeze(U(1,:,:)),-1:.02:1);

title('U');xlabel('x (km)');ylabel('z (m)')

U2=(U([1:end-1],:,:)+U([2:end],:,:))/2;

fid=fopen('uInitial.bin','w','b');
fwrite(fid,U2,'real*8'); fclose(fid);


%-- Wind stress

%tau=tau0*tanh((((squeeze(YU(:,:,1))-y0-Lf))/Lf));

% Constant Wind Stress
%tau=tau0*ones(size(squeeze(YU(:,:,1))));

%subplot(4,2,8)
%quiver(squeeze(XU(:,:,1))/1e3,squeeze(YU(:,:,1))/1e3,tau,0*tau)
%title('Taux');xlabel('x (km)');ylabel('y (km)')
%fid=fopen('taux_sl.bin','w','b');
%fwrite(fid,tau,'real*8'); fclose(fid);

%save 'zf' zf

