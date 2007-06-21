% This is a matlab script that generates the input data
prec='real*8';
ieee='b';

% Dimensions/resolution of grid (resolution/depth in m)
nx=1; dx=10e3;
ny=32; dy=10e3;
dz=[50 50 55 60 65 70 80 95 120 155 200 260 320 400 480];
nz=prod(size(dz));

% Nominal depth of model (meters)
Ho=sum(dz);

% Topography (channel)
H=-Ho*ones(nx,ny);
H(:,end)=0;  % Solid wall in North
fid=fopen('topog.bin','w',ieee); fwrite(fid,H,prec); fclose(fid);

% Size of domain (m)
Lx=dx*nx;
Ly=dy*(ny-1);  % Solid wall in North

% Variable resolution
y=(1:ny)/(ny-0)-0.5;
dy=1-0.3*exp( -(5*y).^2 );
%dy=ones(1,ny);              % Constant resolution
dy=dy/sum(dy(1:ny-1))*Ly;
fid=fopen('dy.bin','w',ieee); fwrite(fid,dy,prec); fclose(fid);

% Coordinates
xc=((1:nx)-0.5)*dx;
yf=-Ly/2+[0 cumsum(dy)];
yc=(yf(1:end-1)+yf(2:end))/2;
zf=[0 -cumsum(dz)];
zc=(zf(1:end-1)+zf(2:end))/2;
[X,Y]=ndgrid(xc,yc);

% Stratification
fo=1e-4;
alpha=2e-4;
gravity=9.81;
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
%- add 10 more levels:
t25=zeros(nx,ny,nz+10); t25(:,:,[1:nz]=T;
fid=fopen('theta.bin','w',ieee); fwrite(fid,t25,prec); fclose(fid);

% Salt (passive tracer)
for k=1:nz,
%S(:,:,k)=ones(nx,ny)+zc(k)/Ho;  % Linear with z
%S(:,:,k)=exp(2*zc(k)/Ho);       % Exponential with z
%S(:,:,k)=exp(2*Y/Ly);           % Exponential with y
 S(:,:,k)=exp(-(2*Y/Ly).^2);           % Exponential with y
end
S(:,end,:)=0;
%- add 10 more levels:
s25=zeros(nx,ny,nz+10); s25(:,:,[1:nz]=S;
fid=fopen('salt.bin','w',ieee); fwrite(fid,s25,prec); fclose(fid);
