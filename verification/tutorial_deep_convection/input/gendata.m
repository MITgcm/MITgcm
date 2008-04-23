% This is a matlab script that generates the input data

% Dimensions of grid
nx=100;
ny=100;
nz=50;
% Nominal depth of model (meters)
H=1000;
% Size of domain
Lx=2.0e3;
% Radius of cooling disk (m)
Rc=600.;
% Horizontal resolution (m)
dx=Lx/nx;
% Rotation
f=1.e-4;
% Stratification
N=0.0*(f*Rc/H);
% surface temperature
Ts=20.;
% Flux : Cooling disk & noise added to cooling
Qo=800; Q1=10;

% Gravity
g=10.;
% E.O.S.
alpha=2.e-4;

Tz=N^2/(g*alpha)

dz=H/nz;
sprintf('delZ = %d * %7.6g,',nz,dz)

x=(1:nx)*dx;x=x-mean(x);
y=(1:ny)*dx;y=y-mean(y);
z=-dz/2:-dz:-H;

% Temperature profile
Tref=Ts+Tz*z-mean(Tz*z);
[sprintf('Tref =') sprintf(' %8.6g,',Tref)]

% Surface heat flux : refine the grid (by 3 x 3) to assign mean heat flux
Q=Qo+Q1*(0.5+rand([nx,ny]));
Qc=zeros(nx,ny);
xc=x'*ones(1,ny); yc=ones(nx,1)*y; 
for j=-1:1, for i=-1:1,
 xs=xc+dx*i/3 ; ys=yc+dx*j/3; r2=xs.*xs+ys.*ys;
 qs=Q/9; qs( find(r2 > Rc*Rc) )=0.;
 Qc=Qc+qs;
end ; end
fid=fopen('Qnet.bin','w','b'); fwrite(fid,Qc,'real*8'); fclose(fid);

var=2*pi*[0:1000]/1000; xl=Rc*cos(var); yl=Rc*sin(var);
figure(1);clf;
var=Qc; var(find(var==0))=NaN;
imagesc(xc,yc,var'); set(gca,'YDir','normal');
caxis([-15 820]);
%change_colmap(-1);
colorbar
grid;
hold on
L=line(xl,yl);
set(L,'color',[0 0 0]);
hold off ;
