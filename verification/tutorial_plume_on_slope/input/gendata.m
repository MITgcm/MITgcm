% This is a matlab script that generates the input data
% variable x resolution

prec='real*8';
ieee='b';

% Dimensions of grid
%nx=80;
nx=320;
ny=1;
nz=60;
% Nominal depth of model (meters)
H=200.0;
% Size of domain
Lx=6.40e3;

% Horizontal resolution (m)
%Variable resolution
res1=2*Lx/(3*nx);
L1=Lx/2;
L2=Lx - L1;
n2=nx - (L1/res1);
res2=L2/n2;
A=res2 - res1;
iswitch1=L1/res1;
width=40;
dx=zeros(nx,1);
for i=1:nx
dx(i) = res1 + 0.5*A*( tanh( (i-iswitch1)/width) + 1);
%dx(i) = Lx/nx;
end

dy = Lx/nx

% Flux
Qo=200;

% Stratification
gravity=9.81;
talpha=2.0e-4;
N2=0.0;
Tz=N2/(gravity*talpha);

dz=H/nz;
sprintf('delZ = %d * %7.6g,',nz,dz)

x=zeros(nx,1);
x(1) = dx(1);
for i=2:nx
x(i)=x(i-1) + dx(i);
end
z=-dz/2:-dz:-H;

%Tanh function for cooling
xswitch = 2.50e3 + Lx/2.0;
qwidth = 0.1e3;
Q=0.0*rand([nx,ny]);
for i=1:nx
Q(i,:) = Q(i,:) + Qo*0.5*(tanh((Lx-x(i)-xswitch)/qwidth) + 1);
%Q(i,:) = Q(i,:) + Qo*0.5*(tanh((x(i)-xswitch)/qwidth) + 1);
end
fid=fopen('Qnet.forcing','w',ieee); fwrite(fid,Q,prec); fclose(fid);

% Temperature profile
Tref=Tz*z-mean(Tz*z);
[sprintf('Tref =') sprintf(' %8.6g,',Tref)]
%t = 0.25*rand([nx,ny,nz]);
t=0.01*rand([nx,ny,nz]);
for k=1:nz
 t(:,:,k) = t(:,:,k) + Tref(k);
end
fid=fopen('T.init','w',ieee); fwrite(fid,t,prec); fclose(fid);

% Sloping channel
% tanh function for slope
slope=0.15
offset=1.5e3 + Lx/2.0;
dmax=-40.0;
h1=dmax;
h2=-H;
hdiff=(h1-h2);
xwidth=hdiff/(2.0*slope);
d=0.0*rand([nx,ny]);
for i=1:nx
for j=1:ny
%d(i,j) = hdiff/2*( exp((x(i)-offset)/xwidth) - exp(-(x(i)-offset)/xwidth))/ ( exp((x(i)-offset)/xwidth) + exp(-(x(i)-offset)/xwidth)) + hdiff/2 -H; 
d(i,j) = hdiff/2*(tanh((Lx-x(i)-offset)/xwidth) + 1) - H;
end
end
d(1,:)=0.0;
fid=fopen('topog.slope','w',ieee); fwrite(fid,d,prec); fclose(fid);
plot(x,d(:,1))
fid=fopen('dx.bin','w',ieee); fwrite(fid,dx,prec); fclose(fid);

