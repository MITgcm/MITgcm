% This is a matlab script that generates the input data

% $Header: /u/gcmpack/MITgcm/verification/exp4/input/gendata.m,v 1.8 2011/05/19 00:03:06 jmc Exp $
% $Name:  $

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

Tz=N^2/(g*alpha);
fprintf(' Tz= %e ;',Tz);

dz=H/nz;
fprintf(' delZ = %d * %7.6g\n',nz,dz);

x=(1:nx)*dx;x=x-mean(x);
y=(1:ny)*dx;y=y-mean(y);
z=-dz/2:-dz:-H;

[Y,X]=meshgrid(y,x);

% Temperature profile
fprintf('Tref ='); fprintf(' %8.6g,',Tz*z-mean(Tz*z)); fprintf('\n');

ieee='b';
prec='real*8';

% Gaussian bump
h=-H+dh*exp( -(X.^2+Y.^2)/(2*(L^2)) );
fid=fopen('topog.bump','w',ieee); fwrite(fid,h,prec); fclose(fid);

% $$$ % Side walls + bump
% $$$ h(:,1)=0;
% $$$ h(:,ny)=0;
% $$$ fid=fopen('topog.bumpchannel','w',ieee); fwrite(fid,h,prec); fclose(fid);

% $$$ % Simple channel
% $$$ h(:,1)=0;
% $$$ h(:,2:ny-1)=-H;
% $$$ h(:,ny)=0;
% $$$ fid=fopen('topog.channel','w',ieee); fwrite(fid,h,prec); fclose(fid);

% initial fields for salinity
si = 35;
%fid=fopen('S.init','w',ieee); fwrite(fid,si*ones(nx,ny,nz),prec); fclose(fid);

% open boundary conditions;
u0 = .25;
s0 = si+1;
w0= 1.e-3;

% create two time slabs for testing
uMerid = cat(3,u0*ones(nx,nz),u0*ones(nx,nz));
uZonal = cat(3,u0*ones(ny,nz),  zeros(ny,nz));
sZonal = cat(3,s0*ones(ny,nz),s0*ones(ny,nz));

%- time varying fraction = 1 % of full velocity
du=u0*0.01;
uWest = cat(3,(u0+du)*ones(ny,nz),(u0-du)*ones(ny,nz));
uEast = cat(3,(u0-du)*ones(ny,nz),(u0+du)*ones(ny,nz));

% to test Non-Hydrostatic OBCS:
w1=[0:nz-1]*pi/nz; w1=-w0*sin(w1);
wZonal = cat (3, ones(ny,1)*w1, zeros(ny,nz));

% to test prescribing Eta in NonLin-FreeSurf formulation:
et1=0.1;
etWest = cat(2,+et1*ones(ny,1),-et1*ones(ny,1));
etEast = cat(2,-et1*ones(ny,1),+et1*ones(ny,1));

 fid=fopen('OBmeridU.bin','w',ieee); fwrite(fid,uMerid,prec); fclose(fid);
%fid=fopen('OBzonalU.bin','w',ieee); fwrite(fid,uZonal,prec); fclose(fid);
 fid=fopen('OB_WestU.bin','w',ieee); fwrite(fid,uWest ,prec); fclose(fid);
 fid=fopen('OB_EastU.bin','w',ieee); fwrite(fid,uEast ,prec); fclose(fid);
 fid=fopen('OBzonalS.bin','w',ieee); fwrite(fid,sZonal,prec); fclose(fid);
 fid=fopen('OBzonalW.bin','w',ieee); fwrite(fid,wZonal,prec); fclose(fid);
 fid=fopen('OB_WestH.bin','w',ieee); fwrite(fid,etWest,prec); fclose(fid);
 fid=fopen('OB_EastH.bin','w',ieee); fwrite(fid,etEast,prec); fclose(fid);

%- rbcs mask & restauring tracer field:
msk=ones(nx,ny,nz);
xMx=max(x);
shapeX=(x-xMx)/dx;
shapeX=exp(shapeX*2/3);

[I]=find(shapeX < 5.e-3); fprintf('zero out rbc-mask up to i= %i\n',max(I));
shapeX(I)=0.;
var=shapeX'*ones(1,ny*nz);
fid=fopen('rbcs_mask.bin','w',ieee); fwrite(fid,var,prec); fclose(fid);

tr1=(si+s0)/2;
var=tr1*ones(nx,ny,nz);
fid=fopen('rbcs_Tr1_fld.bin','w',ieee); fwrite(fid,var,prec); fclose(fid);

