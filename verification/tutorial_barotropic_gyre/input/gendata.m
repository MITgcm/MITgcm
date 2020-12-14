ieee='b'; % big endian format
accuracy='real*4'; % this is single precision

Ho=5000;  % ocean depth in meters
nx=62; % number of gridpoints in x-direction
ny=62; % number of gridpoints in y-direction

% Flat bottom at z=-Ho
h=-Ho*ones(nx,ny);

% Walls (surrounding domain) - generate bathymetry file
h([1 end],:)=0;
h(:,[1 end])=0;
fid=fopen('bathy.bin','w',ieee); fwrite(fid,h,accuracy); fclose(fid);

% Wind-stress
tauMax=0.1; % wind stress maximum
x=((0:nx-1)-0.5)/(nx-2); % x-coordinate, centered at grid u-point
y=((0:ny-1)-0.5)/(ny-2); % y-coordinate, centered at grid v-point
[X,Y]=ndgrid(x,y);

tau=-tauMax*cos(pi*Y); % generate file for -cos(y) profile between 0-1200km
fid=fopen('windx_cosy.bin','w',ieee); fwrite(fid,tau,accuracy); fclose(fid);
tau=tauMax*sin(pi*Y); % generate file for sin(y) profile between 0-1200km
fid=fopen('windx_siny.bin','w',ieee); fwrite(fid,tau,accuracy); fclose(fid);
