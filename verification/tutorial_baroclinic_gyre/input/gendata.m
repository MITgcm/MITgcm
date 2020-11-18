ieee='b';
accuracy='real*4';

Ho=1800;  % depth of ocean
nx=62;    % gridpoints in x
ny=62;    % gridpoints in y
xo=0; yo=15;  % origin in x,y for ocean
dx=1;dy=1;    % grid spacing in x, y

% Flat bottom at z=-Ho
h = -Ho*ones(nx,ny);
% create a border ring of walls around edge of domain
h([1 end],:)=0;
h(:,[1 end])=0;
fid=fopen('bathy.bin','w',ieee); fwrite(fid,h,accuracy); fclose(fid);

% Zonal wind-stress
tauMax=0.1;
x = (xo-1):dx:(xo+(nx-2)*dx); 
y = (yo-dy/2):dy:(yo+(ny-2)*dy+dy/2); 
[X,Y]=ndgrid(x,y);  % zonal wind-stress on (XG,YC) points
tau = -tauMax*cos(2*pi*((Y-yo)/(ny-2)/dy)); % ny-2 accounts for walls at N,S boundaries
fid=fopen('windx_cosy.bin','w',ieee); fwrite(fid,tau,accuracy); fclose(fid);

% Restoring temperature
Tmax=30;Tmin=0;
Trest = (Tmax-Tmin)/(ny-2)*((yo+dy*(ny-2)-Y));
fid=fopen('SST_relax.bin','w',ieee); fwrite(fid,Trest,accuracy); fclose(fid);
