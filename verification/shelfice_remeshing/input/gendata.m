%Verion of gendata.m modified by DNG
%This is a matlab script that generates the input data


% the configuation approximately the ISOMIP experiment no. 1
% require matlab functions for equation of state


% Dimensions of grid
nx=3; 
ny=200;
nz=90;
delz = 10;

hfacMin = 0.2;
%mwct = 3;

dlat = 0.125/32; dy=dlat;
dlon = 0.125/4; dx=dlon;

%eos = 'linear';
eos = 'jmd95z';
% eos = 'mdjwf';

acc = 'real*8';

long = [-105.5:dlon:-105.5];
lonc = long+dlon/2;
latg = [-75.4457:dlat:-73.8809-dlat];
latc = latg+dlat/2;
size(latc);



dz = delz*ones(1,nz);
zgp1 = [0,cumsum(dz)];
zc = .5*(zgp1(1:end-1)+zgp1(2:end));
zg = zgp1(1:end-1);
dz = diff(zgp1);
% sprintf('delZ = %d * %7.6g,',nz,dz)

%%%%%%%%% stratification %%%%%%%%%%%%%%%%

T_sfc = -1.9;
T_bottom = 1.2;
del_T = (T_bottom - T_sfc)/(59*delz);

for iz = 1:nz;


    tref(iz) = T_sfc + del_T*((iz-30)*delz);
    if iz<=30;
        tref(iz)=-1.9;
    end
    if iz>=90
        tref(iz) =2;
    end
end

S_sfc = 34.2;
S_bottom = 34.7;
del_S = (S_bottom - S_sfc)/(59*delz);

for iz = 1:nz;


    sref(iz) = S_sfc + del_S*((iz-30)*delz);
    if iz<=30;
        sref(iz)=34.2;
    end
    if iz>=90
        sref(iz) =34.7;
    end
end

%%%%%%%%%%% density %%%%%%%%%%%%%%%%%

% Gravity
gravity=9.81;
rhoConst = 1000;
k=1;
dzm = abs([zg(1)-zc(1) .5*diff(zc)]);
dzp = abs([.5*diff(zc) zc(end)-zg(end)]);
p = abs(zc)*gravity*rhoConst*1e-4;
dp = p;
kp = 0;

Rho = zeros(nz,1);

while rms(dp) > 1e-13
  phiHydF(k) = 0;
  p0 = p;
  kp = kp+1;
  for k = 1:nz
    switch eos
     case 'linear'

     case 'jmd95z'
      drho = densjmd95(sref(k),tref(k),p(k))-rhoConst;
     case 'mdjwf'
      drho = densmdjwf(sref(k),tref(k),p(k))-rhoConst;
     otherwise
      error(sprintf('unknown EOS: %s',eos))
    end
    Rho(k) = drho+rhoConst;
    phiHydC(k)   = phiHydF(k) + dzm(k)*gravity*drho/rhoConst;
    phiHydF(k+1) = phiHydC(k) + dzp(k)*gravity*drho/rhoConst;
  end
  switch eos
   case 'mdjwf'
    p = (gravity*rhoConst*abs(zc) + phiHydC*rhoConst)/gravity/rhoConst;
  end
  dp = p-p0;
end

shelficemass = binread('h0.bin',3,200) * 917;

%phi0surf = zeros(nx,ny);
topo = zeros(nx,ny);

for ix=1:nx
  for iy=1:ny
%    k=max(find(abs(zg)<abs(topo(ix,iy))));
%    if isempty(k)
%      k=0;
%    end
%    if k>0
      
%      dr = -zg(k) - topo(ix,iy);
      
%      if (dr<delz/2)
%          phi0surf(ix,iy) = phiHydF(k) + (delz/2-dr) * (phiHydC(k)-phiHydF(k))/(delz/2);
%      else
%          phi0surf(ix,iy) = phiHydC(k) + (dr-delz/2) * (phiHydF(k+1)-phiHydC(k))/(delz/2);
%      end
      
%    end

     mass = shelficemass (ix,iy);
     massFuncC = rhoConst * (phiHydC/gravity + zc);
     massFuncF = rhoConst * (phiHydF/gravity + zgp1);

     k = max (find ( massFuncF < mass ));
     if (isempty(k))
         k=0;
     end
     if (k>0)
     if (mass < massFuncC(k))
      topo(ix,iy) = -zg(k) - (mass-massFuncF(k)) * delz/2 / (massFuncC(k)-massFuncF(k));
     else
      topo(ix,iy) = -zc(k) - (mass-massFuncC(k)) * delz/2 / (massFuncF(k+1)-massFuncC(k));
     end
     end

  end
end

%mass = rhoConst * (phi0surf / gravity - topo);
%mass(:,1:100) = mass(:,1:100) + 917 * 10;
%topo(:,1:100) = bathy(:,1:100)+mwct;

etainit = zeros(size(topo));

% new topography: icetopo rounded to the nearest k * deltaZ
%                 eta_init set to make difference

icetopo2 = topo;

for ix=1:nx
  for iy=1:ny
    k=max(find(abs(zg)<abs(icetopo2(ix,iy))));
    if isempty(k)
      k=0;
    else
      
      dr = 1-(-zg(k) - icetopo2(ix,iy))/delz;
      if (dr > .25)
          % bring Ro_surf *up* to closest grid face & make etainit negative
          % to compensate
          icetopo2(ix,iy) = -zg(k);
          etainit(ix,iy) = (dr-1)*delz;
      else
          % bring Ro_surf *down* to closest grid face & make etainit pos
          % to compensate
          icetopo2(ix,iy) = -zg(k+1);
          etainit(ix,iy) = (dr)*delz;
      end
       
    end
  end
end

etainit(:,1)=0;
icetopo2(:,1)=0;
bathy(:,1)=0;

fid = fopen('shelftopo.round.bin','w','b'); fwrite(fid,icetopo2,'real*8'); fclose(fid);
fid = fopen('etainit.round.bin','w','b'); fwrite(fid,etainit,'real*8'); fclose(fid);
fid = fopen('shelficemassinit.bin','w','b'); fwrite(fid,shelficemass,'real*8'); fclose(fid);
%fid = fopen('bathy_step.bin','w','b'); fwrite(fid,bathy,'real*8'); fclose(fid);

