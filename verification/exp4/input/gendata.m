% This is a matlab script that generates the input data

% Dimensions of grid
nx=80;
ny=42;
nz=8;
% Nominal depth of model (meters)
H=4500;
% Scale of bump (m)
L=25e3;
% Height of bump (m)
dh=0.90*H;
% Horizontal resolution (m)
dx=5e3;
% Rotation
f=1e-4;
% Stratification
N=1.5 * f*L/H;

% Gravity
g=9.81;
% E.O.S.
alpha=2.e-4;

Tz=N^2/(g*alpha)

dz=H/nz;
sprintf('delZ = %d * %7.6g,',nz,dz)

x=(1:nx)*dx;x=x-mean(x);
y=(1:ny)*dx;y=y-mean(y);
z=-dz/2:-dz:-H;

[Y,X]=meshgrid(y,x);

% Temperature profile
[sprintf('Tref =') sprintf(' %8.6g,',Tz*z-mean(Tz*z))]

ieee='b';
accuracy='real*8';

% Gaussian bump
h=-H+dh*exp( -(X.^2+Y.^2)/(2*(L^2)) );
fid=fopen('topog.bump','w',ieee); fwrite(fid,h,accuracy); fclose(fid);

% Side walls + bump
h(:,1)=0;
h(:,ny)=0;
fid=fopen('topog.bumpchannel','w',ieee); fwrite(fid,h,accuracy); fclose(fid);

% Simple channel
h(:,1)=0;
h(:,2:ny-1)=-H;
h(:,ny)=0;
fid=fopen('topog.channel','w',ieee); fwrite(fid,h,accuracy); fclose(fid);
