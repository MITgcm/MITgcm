ieee = 'b';           % big-endian format
accuracy = 'float32'; % this is single-precision (='real*4')

Ho = 5000;  % ocean depth in meters
nx = 62;    % number of gridpoints in x-direction
ny = 62;    % number of gridpoints in y-direction
xo = 0;     % origin in x,y for ocean domain
yo = 0;     % (i.e. southwestern corner of ocean domain)
dx = 20;    % grid spacing in x (km)
dy = 20;    % grid spacing in y (km)
xeast  = xo + (nx-2)*dx;  % eastern extent of ocean domain
ynorth = yo + (ny-2)*dy;  % northern extent of ocean domain

% Flat bottom at z=-Ho
h = -Ho * ones(nx, ny);

% Walls (surrounding domain); generate bathymetry file
h([1 end], :) = 0;   % set ocean depth to zero at east and west walls
h(:, [1 end]) = 0;   % set ocean depth to zero at south and north walls
fid = fopen('bathy.bin', 'w', ieee);
fwrite(fid, h, accuracy);
fclose(fid);

% Ocean domain extends from (xo,yo) to (xeast,ynorth)
% (i.e. the ocean spans nx-2, ny-2 grid cells)
% out-of-box-config: xo=yo=0, dx=dy=20 km, ocean extent (0,0)-(1200,1200) km
% model domain includes a land cell surrounding the ocean domain
% The full model domain cell centers are located at:
%    XC(:,1) = -10, +10, ..., +1210 (km)
%    YC(1,:) = -10, +10, ..., +1210 (km)
% and full model domain cell corners are located at:
%    XG(:,1) = -20, 0, ..., 1200 [, 1220] (km)
%    YG(1,:) = -20, 0, ..., 1200 [, 1220] (km)
% where the last value in brackets is not included in the MITgcm grid variable
% and reflects the eastern and northern edge of the model domain respectively.
% See section 2.11.4 of the MITgcm users manual.

% Zonal wind-stress, located at u-points (see section 2.11.4)
% here we non-dimensionalize: 0 at southern and western ocean boundary 
% to 1.0 at eastern and northern ocean boundary
% for the purpose of applying sinusoidal-shaped wind stress curve
tauMax = 0.1;  % wind stress maximum
x = (-1:nx-2) / (nx-2);       % non-dim x-coordinate, located at XG points
y = ((0:ny-1)-0.5) / (ny-2);  % non-dim y-coordinate, located at YC points
[X,Y] = ndgrid(x, y);

% generate file for -cos(y) profile
tau = -tauMax * cos(pi*Y);
fid = fopen('windx_cosy.bin', 'w', ieee);
fwrite(fid, tau, accuracy);
fclose(fid);

% generate file for sin(y) profile
tau = tauMax * sin(pi*Y); 
fid = fopen('windx_siny.bin', 'w', ieee);
fwrite(fid, tau, accuracy);
fclose(fid);

% Meridional wind-stress, if desired, would be located at v-points (XC, YG)
