% This is a matlab script that generates the input data

% Dimensions of grid
nx=64;
ny=64;
nz=20;
% Nominal depth of model (meters)
H=2000;
% Size of domain
Lx=32e3;
% Scale of disk (m)
L=8e3;
% Horizontal resolution (m)
dx=Lx/nx;
% Rotation
f=1e-4;
% Stratification
N=0. * f*L/H;
% Flux
Qo=1;

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

% Gaussian bump
Q=Qo*(1+0.01*rand([nx,ny]));
r=sqrt(X.^2+Y.^2);
%% Q( find(r>L) )=0;
fid=fopen('Qnet.circle','w','b'); fwrite(fid,Q,'real*8'); fclose(fid);
