% This is a matlab script that generates the input data
% variable x resolution
prec='real*8';
ieee='b';

% Dimensions of grid
%nx=50;
nx=200;
ny=45;
nz=25;
% Nominal depth of model (meters)
H=3600.0;
% Size of domain
Ly=600.0e3;
%Lx=500.0e3;
Lx=2000.0e3;

% Horizontal resolution (m)

dx=zeros(nx,1);
for i=1:nx
dx(i) = Lx/(nx);
end

dy=zeros(ny,1);
%for i=1:ny
%dy(i) = Ly/(ny);
%end

% y-resolution ramps from 10km to 40km in southern basin:
y = [0:600];
y0 = 420; yw = 40; r1 = 10; r2 = 40;
dy = (r1+r2)/2 - (r1-r2)/2*tanh((y-y0)/yw);

yg = 0; ii = 1; dely = dy(1);
while yg(ii)<=max(y)-dely,
  dely = interp1(y,dy,yg(ii));
  yg(ii+1) = yg(ii) + dely;
  ii = ii+1;
end
yg(ii) = max(y);

%plot(yg,mean(dy),'b+'); hold on;
%plot(0.5*(yg(1:end-1)+yg(2:end)),diff(yg),'rx-');
%plot(y,dy,'c.-'); grid on
%pause
dy = fliplr(diff(yg)*1e3);

dz=zeros(nz,1);
for i=1:nz
dz(i)=H/nz;
end
%sprintf('delZ = %d * %7.6g,',nz,dz)

z=zeros(nz,1);
z(1) = -dz(1)/2.0;
for i=2:nz
z(i)=z(i-1) - dz(i);
end

% Stratification
gravity=9.81;
talpha=2.0e-4;
rho0=1030.0;
rhotop=1030.0;
%rhobot=1030.0;
rhobot=1032.0;

%gravity=10;
%talpha=1.0e-4;
%rho0=1000.0;
%rhotop=1000.0;
%rhobot=1000.0;
N2=((rhobot-rhotop)/H)/rho0*gravity
Tz=N2/(gravity*talpha) ;

% Temperature profile. tRef is a single reference temperature, Tbg is
% a background profile, t is a three-dimensional initial stratification.
tRef = 0;
%Tref=Tz*(z+H) + (rhobot-rho0)/(rho0*talpha) + 1.0;
%Tref=Tz*(z+H) + (rhobot-rho0)/(rho0*talpha)
Tbg =Tz*(z+H) - (rhobot-rho0)/(rho0*talpha) + tRef;
[sprintf('Tref =') sprintf(' %8.6g,',Tbg)]
%t=0.00001*rand([nx,ny,nz]);
t=0*rand([nx,ny,nz]);
for k=1:nz
t(:,:,k) = t(:,:,k) + Tbg(k);
end
fid=fopen('T.init','w',ieee); fwrite(fid,t,prec); fclose(fid);

% this could be done more elegantly...
x=zeros(nx,1);
x(1) = 0.5*dx(1); % center of grid cell
for i=2:nx
x(i)=x(i-1) + 0.5*dx(i-1) + 0.5*dx(i);
end

y=zeros(ny,1);
y(1) = dy(1); % north edge of grid cell
for i=2:ny
y(i) = y(i-1) + dy(i);
end

% Linear slope and embayment
slope = 0.01;
%slope = 0.005;
lembay=50.0e3; % length of embayment
eastwall = 1; % make a wall on the eastern boundary
westwall = 0; % make a wall on the western boundary

% These three parameters should match with Width, Dmax and Xcenter
% (respectively) in data.obcs or else funny stuff will happen...
wembay=100.0e3; % width of embayment
dembay=600.0; % depth of embayment 
embay_ctr=1700.0e3; % x-coordinate of embayment center

%xembay=Lx-150.0e3-wembay; %x location of start of embayment
xembay=embay_ctr-wembay/2;
d=0.0*rand([nx,ny]);
for i=1:nx
d(:,1) = 0.0;
for j=2:ny
yneg=Ly-y(j);
if yneg < lembay 
if x(i) > xembay 
if x(i) < xembay+wembay
d(i,j) = -dembay;
else
d(i,j) = 0.0;
end
else
d(i,j) = 0.0;
end
else
d(i,j) = -slope*(yneg - lembay) - dembay;
end
if d(i,j) < -H
d(i,j) = -H;
end
end
end
if eastwall
  d(nx,:) = 0.0; % east wall
end
if westwall
  d(1,:) = 0.0; % west wall
end
fid=fopen('topog.slope','w',ieee); fwrite(fid,d,prec); fclose(fid);
hold off;
plot(x,d(:,2),'.-'); grid on
%plot(y,d(2,:),'.-'); grid on

%fid=fopen('delXvar','w',ieee); fwrite(fid,dx,prec); fclose(fid);
fid=fopen('delYvar','w',ieee); fwrite(fid,dy,prec); fclose(fid);
