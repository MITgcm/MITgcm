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
fid=fopen('topog.box','w',ieee); fwrite(fid,h,accuracy); fclose(fid);

% Wind-stress
tauMax=0.1;
x=((1:nx)-0.5)/(nx-1); % nx-1 accounts for a solid wall
y=((1:ny)-0.5)/(ny-1); % ny-1 accounts for a solid wall
[X,Y]=ndgrid(x,y);
tau=tauMax*sin(pi*Y);
fid=fopen('windx.sin_y','w',ieee); fwrite(fid,tau,accuracy); fclose(fid);
