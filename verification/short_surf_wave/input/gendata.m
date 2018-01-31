% This is a matlab script that generates the input data
prec='real*8';
ieee='b';

% Dimensions of grid
nx=52;
ny=1;
nz=50;
% Nominal depth of model (meters)
H=10.;
% Size of interior domain
Lx=10.;

% Horizontal resolution (m)
dx=Lx/(nx-2);
dy=dx;
x=[1:nx]*dx; x=x-mean(x)+Lx/2;
fprintf('dx = %d ; x 1,2 .. nx-1,nx = %7.4g %7.4g %7.4g %7.4g\n', ...
     dx,x(1),x(2),x(nx-1),x(nx));

%gravity=9.81;
gravity=10.;

dz=H/nz;
fprintf('delZ = %d * %7.6g\n',nz,dz)

%- initial free-surface:
Ampli=0.15;
et=x*pi/Lx; et=Ampli*cos(et); 
et(1)=0; et(nx)=0;

%- closed bassin:
h0=ones(nx,1); h0(1)=0; h0(nx)=0;
h0=-H*h0;

fid=fopen('topo_flat.bin','w',ieee); fwrite(fid,h0,prec); fclose(fid);
fid=fopen('Eta_ini.bin','w',ieee); fwrite(fid,et,prec); fclose(fid);

figure(1);clf;
plot(x,et,'k-');
axis([-0.2 10.2 [-1 1]*0.16]); grid
