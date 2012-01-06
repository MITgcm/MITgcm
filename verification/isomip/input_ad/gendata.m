% This is a matlab script that generates the input data

% the configuation approximately the ISOMIP experiment no. 1
% require matlab functions for equation of state

% Dimensions of grid
nx=50; nxi=20;
ny=100;
nz=30;
deltaZ = 30;

dlat = 0.1; dy=dlat;
dlon = 0.3; dx=dlon;

%eos = 'linear';
eos = 'jmd95z';
%eos = 'mdjwf';

acc = 'real*8';

long = [0:dlon:10-dlon];
lonc = long+dlon/2;
latg = [-80:dlat:-70-dlat];
latc = latg+dlat/2;

% Nominal depth of model (meters)
H = -900;
Hmin = -700; % deepest point of cavern
Hmax = -200; % shallowest point of cavern
dHdx = (Hmax-Hmin)/4;

bathy = ones(nx,ny)*H;
bathy(1,:) = 0;
bathy(:,1) = 0;
fid=fopen('bathy.box','w','b'); fwrite(fid,bathy,acc);fclose(fid);


dz = deltaZ*ones(1,nz);
zgp1 = [0,cumsum(dz)];
zc = .5*(zgp1(1:end-1)+zgp1(2:end));
zg = zgp1(1:end-1);
dz = diff(zgp1);
sprintf('delZ = %d * %7.6g,',nz,dz)

% Gravity
gravity=9.81;
rhoConst = 1030;
% compute potential field underneath ice shelf
talpha = 2e-4;
sbeta  = 7.4e-4;
tref = -1.9*ones(nz,1);
t    = tref;
sref = 34.4*ones(nz,1);
s    = sref;
gravity = 9.81;
k=1;
dzm = abs([zg(1)-zc(1) .5*diff(zc)]);
dzp = abs([.5*diff(zc) zc(end)-zg(end)]);
p = abs(zc)*gravity*rhoConst*1e-4;
dp = p;
kp = 0;
while rms(dp) > 1e-13
  phiHydF(k) = 0;
  p0 = p;
  kp = kp+1
  for k = 1:nz
    switch eos
     case 'linear'
      drho = rhoConst*(1-talpha*(t(k)-tref(k))+sbeta*(s(k)-sref(k)))-rhoConst;
     case 'jmd95z'
      drho = densjmd95(s(k),t(k),p(k))-rhoConst;
     case 'mdjwf'
      drho = densmdjwf(s(k),t(k),p(k))-rhoConst;
     otherwise
      error(sprintf('unknown EOS: %s',eos))
    end
    phiHydC(k)   = phiHydF(k) + dzm(k)*gravity*drho/rhoConst;
    phiHydF(k+1) = phiHydC(k) + dzp(k)*gravity*drho/rhoConst;
  end
  switch eos
   case 'mdjwf'
    p = (gravity*rhoConst*abs(zc) + phiHydC*rhoConst)/gravity/rhoConst;
  end
  dp = p-p0;
end

icetopo = ones(nx,1)*min(Hmax,Hmin + dHdx*(latc-latg(1)));
fid=fopen('icetopo.exp1','w','b'); fwrite(fid,icetopo,acc);fclose(fid);
fid=fopen('pload.exp1','w','b'); fwrite(fid,-icetopo,acc);fclose(fid);
phi0surf = zeros(nx,ny);
for ix=1:nx
  for iy=1:ny
    k=max(find(abs(zg)<abs(icetopo(ix,iy))));
    if isempty(k)
      k=0;
    end
    if k>0
      phi0surf(ix,iy) = phiHydF(k)*rhoConst;
    end
  end
end
fid=fopen(['phi0surf.exp1.' eos],'w','b'); fwrite(fid,phi0surf,acc);fclose(fid);

