
%-- Generate initial condition (U,V,Eta) for a solid-body rotation flow field
%   U-zon = Ueq * cos(Lat) over a sphere in rotation (Omega = 2*pi/rotPeriod).
%   Read-in few output grid-files (from dir "gDir") which are easy to get by
%   running the MITgcm for 1 time-step with default (=zero) initial conditions.

%- sphere radius [m]:
%rSphere=6370.e+3; %- Earth value
 rSphere=5500.4e+3; %- this test
%- rotation period [s] of the solid Earth:
%rotationPeriod= 86400.; % = 1 day
 rotationPeriod=108000.; % = 30.h <-- slower rotation rate
%- air density:
 rhoConst=1.;
%- Flow velocity (relative to solid Earth) at the Equator [m/s]:
%  this relative velocity plus sphere-rotation gives back 24.h rotation period
 Ueq=80.;

%- set other constants:
rad=pi/180.;
Omega=2*pi/rotationPeriod; Vsurf=rSphere*Omega;
flowRot=Ueq/rSphere; flowPer= 2*pi/flowRot;

fprintf('Sphere rotation: Omega= %12.6e 1/s ; Period= %6.2f h ; Vsurf= %8.3f m/s\n', ...
 Omega, rotationPeriod/3600., Vsurf);
fprintf(' Flow  rotation: omega= %12.6e 1/s ; period= %6.2f h ; U.eq = %8.3f m/s\n', ...
 flowRot, flowPer/3600., Ueq);

%-- load output grid files fron dir "gDir":
gDir='../run/'; %gDir='./';
xC=rdmds([gDir,'XC']);
yC=rdmds([gDir,'YC']);
yG=rdmds([gDir,'YG']);
dXg=rdmds([gDir,'DXG']);
dYg=rdmds([gDir,'DYG']);

%- put yG into long-vector format and add the 2 missing corners:
n1h=size(dXg,1); n2h=size(dXg,2); nPg=n1h*n2h; nPp2=nPg+2;
yZ=zeros(nPp2,1);
if n1h == 6*n2h,
  nc=n2h; %- this the "old-format", all facets stacked along first dimension
  yZ(1:nPg,1)=reshape(permute(reshape(yG,[nc 6 nc]),[1 3 2]),[nPg 1]);
  yZ(nPg+1)=yG(1+nc*2,1); %- take lat of SW corner of face 3
else
  nc=n1h; %- this the "compact-format", all facets stacked along 2nd dimension
  yZ(1:nPg,1)=reshape(yG,[nPg 1]);
  yZ(nPg+1)=yG(1,1+nc*2); %- take lat of SW corner of face 3
end
  yZ(nPg+2)=yG(1,1);    %- take lat of SW corner of face 1
%- split latitude array in 6 faces:
 y6t=split_Z_cub(yZ);

%- define flow field from stream-function:
 psi=rSphere*Ueq*sin(rad*y6t);
 ut=zeros(nc,nc,6); vt=ut; ncp=nc+1;
for n=1:6,
 ut(:,:,n)=psi(1:nc,2:ncp,n)-psi(1:nc,1:nc,n);
 vt(:,:,n)=psi(1:nc,1:nc,n)-psi(2:ncp,1:nc,n);
end
if n1h == 6*n2h,
  ut=permute(ut,[1 3 2]); vt=permute(vt,[1 3 2]);
end
 ut=reshape(ut,[n1h n2h]); vt=reshape(vt,[n1h n2h]);
 u=ut./dYg; v=vt./dXg;

%- define surface pressure anomaly from the norm of velocity at grid-cell center
%  using analytical expression: cos^2(lat) x fac with:
 fac=rhoConst*(Omega*rSphere + Ueq/2)*Ueq;
%- and since shifting by const has no effect on grad(P), substract 2/3(*fac) (= analytical mean)
%  to get closer to zero global-mean:
 ps1=cos(rad*yC); ps1=ps1.*ps1-2./3.; ps1=fac*ps1;

 psAvr=mean(ps1(:));
 fprintf('Ps stats: %8.2f , %13.6e , %8.2f\n',min(ps1(:)),psAvr,max(ps1(:)));

%- write initial conditions:
kwr=1; prec='real*8';
%  change kwr below to =2 to write (U,V,Eta) initial cond. or to =0 to skip all writing
%kwr=0;
if kwr > 1,
 fnam='U_init.bin'; var=u;
 fprintf(' writing file: %s ...',fnam);
 fid=fopen(fnam,'w','b');
 fwrite(fid,var,prec); fclose(fid);
 fprintf(' done\n');

 fnam='V_init.bin'; var=v;
 fprintf(' writing file: %s ...',fnam);
 fid=fopen(fnam,'w','b');
 fwrite(fid,var,prec); fclose(fid);
 fprintf(' done\n');
end
if kwr > 1,
 fnam='Eta_ini.bin'; var=ps1;
 fprintf(' writing file: %s ...',fnam);
 fid=fopen(fnam,'w','b');
 fwrite(fid,var,prec); fclose(fid);
 fprintf(' done\n');
end

%return

%- define passive tracer (use salt here) patch:
 lon=xC*rad; lat=yC*rad;
 X=cos(lat).*sin(lon); Y=-cos(lat).*cos(lon); Z=sin(lat);
 clear lat lon

 lon0=-90  *rad;
 lat0=45   *rad;
 xo=cos(lat0).*sin(lon0); yo=-cos(lat0).*cos(lon0); zo=sin(lat0);
 ro=0.3;
 R=sqrt( (X-xo).^2 + (Y-yo).^2 + (Z-zo).^2 );
 s=(1+cos( pi*min(R/ro,1+0*R) ))/2;

if kwr > 0,
 fnam='S_init.bin'; var=s;
 fprintf(' writing file: %s ...',fnam);
 fid=fopen(fnam,'w','b');
 fwrite(fid,var,prec); fclose(fid);
 fprintf(' done\n');
end

