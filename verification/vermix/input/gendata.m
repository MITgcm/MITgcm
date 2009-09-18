% This is a matlab script that generates the input data
% variable x resolution

prec='real*8';
ieee='b';

% Dimensions of grid
%nx=80;
nx=3;
ny=1;
nz=20;
% Nominal depth of model (meters)
H=1000.0;
% Size of domain
Lx=15e3;

dx = Lx/nx;
dz = H/nz;
% Horizontal resolution (m)
%Variable resolution

dy = dx;

% Flux
Qo=200;

% Stratification
gravity=9.81;
talpha=2.0e-4;
N2=2.56e-4;
Tz=N2/(gravity*talpha);
Sz=0.05;

sprintf('delR = %d * %7.6g,',nz,dz)

z=-dz/2:-dz:-H;
Tref=Tz*z/(H/20)+2;
Sref=34.025-Sz*z/(H/20);
[sprintf('Tref =') sprintf(' %8.6g,',Tref)]
[sprintf('Sref =') sprintf(' %8.6g,',Sref)]

Q=zeros([nx,ny]);
Q(1) = 0;
Q(2) = 200;
Q(3) = 0;
fid=fopen('Qnet.forcing','w',ieee); fwrite(fid,Q,prec); fclose(fid);

s=zeros([nx,ny,nz]);
for k=1:nz
 s(:,:,k) = s(:,:,k) + Sref(k);
end
fid=fopen('S.init','w',ieee); fwrite(fid,s,prec); fclose(fid);

t0=1*ones([nx,ny,nz]);
t=t0;
fid=fopen('T.init','w',ieee); fwrite(fid,t,prec); fclose(fid);

t=t0;
t(2,1,1) = .5;
fid=fopen('Tinstable.init','w',ieee); fwrite(fid,t,prec); fclose(fid);

t=t0;
for k=1:nz
 t(:,:,k) = t(:,:,k) + Tref(k);
end
fid=fopen('Tstable.init','w',ieee); fwrite(fid,t,prec); fclose(fid);

t=t0;
t(2,1,1) = -.5;
for k=1:nz
 t(:,:,k) = t(:,:,k) + Tref(k);
end
fid=fopen('Tstable2.init','w',ieee); fwrite(fid,t,prec); fclose(fid);

taux = 0*ones([nx,ny]);
taux(2:3,:) = 1;
fid=fopen('taux.forcing','w',ieee); fwrite(fid,taux,prec); fclose(fid);

tke = 1e-6*ones([nx,ny,nz]);
tke(2,1,1) = 1e-3;
fid=fopen('TKE.init','w',ieee); fwrite(fid,tke,prec); fclose(fid);

% double diffusive initial conditions: 
% salt fingers in upper layers (dt>0,ds>0,Rrho=talpha*dt/sbeta*ds > 1)
% diffusive convection in lower layers (dt<0,ds<0,Rrho=talpha*dt/sbeta*ds < 1)
sbeta = 7.e-4;
tdd = -min(Tref,2-Tref);
Rrho = 0*tdd+1.1;
Rrho(8:end) = 0.5;
Sz2=talpha*diff(tdd)/sbeta./Rrho(1:end-1);
sdd = cumsum([34,Sz2]);

t=zeros([nx,ny,nz]);
for k=1:nz
 t(:,:,k) = tdd(k);
end
fid=fopen('T.doublediff','w',ieee); fwrite(fid,t,prec); fclose(fid);

s=zeros([nx,ny,nz]);
for k=1:nz
 s(:,:,k) = sdd(k);
end
fid=fopen('S.doublediff','w',ieee); fwrite(fid,s,prec); fclose(fid);


