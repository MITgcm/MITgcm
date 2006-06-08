ieee='b';
accuracy='real*8';

Ho=5000;
nx=5;
ny=5;

% Flat bottom at z=-Ho
h=-Ho*ones(nx,ny);
% walls, no topgraphy
h(:,1) = 0;
fid=fopen('walls.box','w',ieee); fwrite(fid,h,accuracy); fclose(fid);
% walls, small ridge
h(3,2:ny) = -4000;
fid=fopen('walls.smallRidge','w',ieee); fwrite(fid,h,accuracy); fclose(fid);
% walls, big ridge
h(3,2:ny) = -2000;
fid=fopen('walls.bigRidge','w',ieee); fwrite(fid,h,accuracy); fclose(fid);
% no walls, giant ridge
h(3,2:ny) = -500;
fid=fopen('walls.giantRidge','w',ieee); fwrite(fid,h,accuracy); fclose(fid);
% no walls, big ridge
h=-Ho*ones(nx,ny);
h(3,:) = -2000;
fid=fopen('noWalls.bigRidge','w',ieee); fwrite(fid,h,accuracy); fclose(fid);
% no walls, small ridge
h(3,:) = -4000;
fid=fopen('noWalls.smallRidge','w',ieee); fwrite(fid,h,accuracy); fclose(fid);
% no walls, giant ridge
h(3,:) = -500;
fid=fopen('noWalls.giantRidge','w',ieee); fwrite(fid,h,accuracy); fclose(fid);

% box, big ridge
h=-Ho*ones(nx,ny);
h(3,2:ny) = -2000;
h(:,[1 end]) = 0; h([1 end],:) = 0;
%h([1 end],:) = 0;
fid=fopen('box.bigRidge','w',ieee); fwrite(fid,h,accuracy); fclose(fid);

% channel, big ridge
h=-Ho*ones(nx,ny);
h(3,2:ny) = -2000;
h(:,[1 5]) = 0;
fid=fopen('channel.bigRidge','w',ieee); fwrite(fid,h,accuracy); fclose(fid);

h=-Ho*ones(nx,ny);
h(1:nx,:) = -2000;
h([1 5],:) = 0;
fid=fopen('meridchannel.bigRidge','w',ieee); fwrite(fid,h,accuracy); fclose(fid);

% shallow, no walls, big ridge
h=-500*ones(nx,ny);
h(3,:) = -200;
fid=fopen('shallow.noWalls.bigRidge','w',ieee); fwrite(fid,h,accuracy); fclose(fid);


% Wind-stress
tauMax=0.1;
x=((1:nx)-.5)/(nx);
y=((1:ny)-1)/(ny);
[X,Y]=ndgrid(x,y);
tau=tauMax*ones(nx,ny);
fid=fopen('windx.sin_y','w',ieee); fwrite(fid,tau,accuracy); fclose(fid);
