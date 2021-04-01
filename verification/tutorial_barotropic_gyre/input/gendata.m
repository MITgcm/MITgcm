ieee='b'; % big endian format
accuracy='real*4'; % this is single precision

Ho=5000;  % ocean depth in meters
nx=62;    % number of gridpoints in x-direction
ny=62;    % number of gridpoints in y-direction

% Flat bottom at z=-Ho
h = -Ho*ones(nx,ny);

% Walls (surrounding domain) - generate bathymetry file
h([1 end],:)=0;
h(:,[1 end])=0;
fid=fopen('bathy.bin','w',ieee); fwrite(fid,h,accuracy); fclose(fid);

% ocean domain extends from (xo,yo) to (xo+dx*(nx-2),yo+dy*(ny-2))
% (i.e. the ocean spans nx-2, ny-2 grid cells)
% out-of-box-config: xo=yo=0, dx=dy=20 km, ocean extent (0,0)-(1200,1200) km
% model domain includes a land cell surrounding the ocean domain
% The full model domain cell centers are located at:
%    XC(:,1)=-10,+10,...,+1210
%    YC(1,:)=-10,+10,...,+1210
% and full model domain cell corners are located at:
%    XG(:,1)=-20,0,...,1200[,1220]
%    YG(1,:)=-20,0,...,1200[,1220]
% where the last value in brackets is not included in the MITgcm grid variable
% but reflects the eastern and northern edge of the model domain respectively
% see section 2.11.4 of the MITgcm users manual

% Zonal wind-stress, located at u-points
% here we non-dimensional: xo=yo=0 to 1.0 at eastern and northern ocean boundary
% for the purpose of applying sinusoidal-shaped wind stress curve
tauMax=0.1; % wind stress maximum
x = (-1:nx-2)/(nx-2); % x-coordinate, located at XG points
y = ((0:ny-1)-0.5)/(ny-2); % y-coordinate, located at YC points
[X,Y]=ndgrid(x,y);

tau = -tauMax*cos(pi*Y); % generate file for -cos(y) profile between 0-1200km
fid=fopen('windx_cosy.bin','w',ieee); fwrite(fid,tau,accuracy); fclose(fid);
tau =  tauMax*sin(pi*Y); % generate file for sin(y) profile between 0-1200km
fid=fopen('windx_siny.bin','w',ieee); fwrite(fid,tau,accuracy); fclose(fid);

% Meridional wind-stress, if desired, would be located at v-points (XC, YG)
