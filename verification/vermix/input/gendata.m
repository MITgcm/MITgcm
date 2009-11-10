% This is a matlab script that generates the input data
% variable x resolution

prec='real*8';
ieee='b';

% Dimensions of grid
nx=1;
ny=1;
nz=26;
% Vertical grid (meters)
dz =[10 10 10 10 10 12 12 15 15 18 18 22 22 26 32 38 46 46 46 46 46 46 46 46 46 46];
sprintf('delR = %d * %7.6g,',nz,dz)
zi = [0 cumsum(dz)];
z = 0.5*(zi(1:end-1)+zi(2:end));

% Initial temperature
gravity=9.81;
talpha=2.0e-4;
N2=2.e-5;
Tz=N2/(gravity*talpha);

Tref=-Tz*(z - zi(end))+2;
[sprintf('Tref =') sprintf(' %8.6g,',Tref)];
fid=fopen('T_26.init','w',ieee); fwrite(fid,Tref,prec); fclose(fid);

% Flux
Q=zeros(72,1);
Q(1:18) = 350;
Q(25:66) = -350*3/7;
fid=fopen('Qnet_72.forcing','w',ieee); fwrite(fid,Q,prec); fclose(fid);

%
taux = 0.1*ones([nx,ny],72);
fid=fopen('taux_72.forcing','w',ieee); fwrite(fid,taux,prec); fclose(fid);

%%%%%
tke = 1e-6*ones(1,nz);
tke(1,1) = 1e-3;
fid=fopen('TKE.init','w',ieee); fwrite(fid,tke,prec); fclose(fid);

% double diffusive initial conditions: 
% salt fingers in upper layers (dt>0,ds>0,Rrho=talpha*dt/sbeta*ds > 1)
% diffusive convection in lower layers (dt<0,ds<0,Rrho=talpha*dt/sbeta*ds < 1)
sbeta = 7.e-4;
tdd = max(Tref,8-Tref);
Rrho = 0*tdd+1.1;
Rrho(8:end) = 0.5;
Sz2=talpha*diff(tdd)/sbeta./Rrho(1:end-1);
sdd = cumsum([34,Sz2]);

fid=fopen('T.doublediff','w',ieee); fwrite(fid,tdd,prec); fclose(fid);

fid=fopen('S.doublediff','w',ieee); fwrite(fid,sdd,prec); fclose(fid);
