% This is a matlab script that generates the input data
% variable x resolution
prec='real*8';
ieee='b';


% Dimensions of grid
nx=60;
ny=1;
nz=20;
% Nominal depth of model (meters)
H=200.0;
% Size of domain
Lx=13.3e3;

% Horizontal resolution (m)
dx=zeros(nx,1);
for i=1:nx
dx(i) = Lx/(nx+1);
end

dy = Lx/nx
% Stratification
gravity=9.81;
talpha=2.0e-4;
N2=1e-6;

Tz=N2/(gravity*talpha);

dz=H/nz;
sprintf('delZ = %d * %7.6g,',nz,dz)

x=zeros(nx,1);
x(1) = dx(1);
for i=2:nx
x(i)=x(i-1) + dx(i);
end
z=-dz/2:-dz:-H;

%[Y,X]=meshgrid(y,x);

% Temperature profile
Tref=Tz*z-mean(Tz*z);
[sprintf('Tref =') sprintf(' %8.6g,',Tref)]
t=0.0*rand([nx,ny,nz]);
for k=1:nz
t(:,:,k) = t(:,:,k) + Tref(k);
end
fid=fopen('T.init','w',ieee); fwrite(fid,t,prec); fclose(fid);

% Sloping channel 
slope=0.03
offset=2.5e3;
dmax=-40.0;
d=0.0*rand([nx,ny]);
for i=1:nx
for j=1:ny
d(i,j) = -H;
d(i,j) = d(i,j) + slope*(x(i) - offset);
if d(i,j) < -H ;
d(i,j) = -H;
end
if d(i,j) > dmax;
d(i,j) = dmax;
end
end
end
d(nx,:)=0.0;
fid=fopen('topog.slope','w',ieee); fwrite(fid,d,prec); fclose(fid);
plot(x,d(:,1))

fid=fopen('delXvar','w',ieee); fwrite(fid,dx,prec); fclose(fid);

%convex slope
nxslope=(dmax + H)/(slope)
d1=zeros(nx,ny);
hamp=(H-dmax)/5.0
pi=4.0*atan(1.0)
for i=1:nx
for j=1:ny
if x(i) < (offset + nxslope)
if x(i) < offset
d1(i,j) = d(i,j);
else
d1(i,j) = -H;
d1(i,j) = d(i,j) + hamp*sin(pi*(x(i)-offset)/nxslope);
if d1(i,j) < -H ;
d1(i,j) = -H;
end
if d1(i,j) > dmax;
d1(i,j) = dmax;
end
end
else
d1(i,j) = d(i,j);
end
end
end
%d1(end-1:end,:)=d1(1:2,:); % debug by aja
fid=fopen('topog.convex','w',ieee); fwrite(fid,d1,prec); fclose(fid);
plot(x,d1(:,1),'g')

%convex slope
d2=zeros(nx,ny);
for i=1:nx
for j=1:ny
if x(i) < (offset + nxslope)
if x(i) < offset
d2(i,j) = d(i,j);
else
d2(i,j) = -H;
d2(i,j) = d(i,j) - hamp*sin(pi*(x(i)-offset)/nxslope);
if d2(i,j) < -H ;
d2(i,j) = -H;
end
if d2(i,j) > dmax;
d2(i,j) = dmax;
end
end
else
d2(i,j) = d(i,j);
end
end
end
%d2(end-1:end,:)=d2(1:2,:); % debug by aja
fid=fopen('topog.concave','w',ieee); fwrite(fid,d2,prec); fclose(fid);
hold on
plot(x,d2(:,1),'r')
hold off


fid=fopen('delXvar','w',ieee); fwrite(fid,dx,prec); fclose(fid);
