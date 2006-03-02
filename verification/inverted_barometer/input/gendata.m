ieee='b';
accuracy='real*8';

Ho=2000;
nx=60;
ny=60;

% Flat bottom at z=-Ho
h=-Ho*ones(nx,ny);
% Walls
h(end,:)=0;
h(:,end)=0;

% center point
xc = nx/2;
yc = ny/2;
[x y]  = meshgrid([1:nx]',[1:ny]');
r0 = nx/2;
r = sqrt((x-xc).^2+(y-yc).^2);
h(find(r.^2>r0.^2)) = 0;
% coordinate transformation
phi = acos((x-xc)./r);
phi(find(r==0))=0;

kr = 2*pi/r0;

%mask = change(h,'~=',0,1);
mask=ones(nx,ny); mask(find(h==0))=0;

fid=fopen('topog.box','w',ieee); fwrite(fid,h,accuracy); fclose(fid);

% Atmospheric pressure in (Pa)
pMean=0e5;
pMax = 1e4;
nu = 2;
slp = pMean + pMax*besselj(nu,kr*r).*cos(nu*phi).*mask;

% remove mean
slp = (slp-sum(slp(:))/sum(mask(:))).*mask;
fid=fopen('pLoad.bin','w',ieee); fwrite(fid,slp,accuracy); fclose(fid);
