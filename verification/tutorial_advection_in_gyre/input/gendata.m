ieee='b';
accuracy='real*8';

Ho=5000;
nx=60;
ny=60;

% Flat bottom at z=-Ho
h=-Ho*ones(nx,ny);
% Walls
h(end,:)=0;
h(:,end)=0;
fid=fopen('topog.box5000','w',ieee); fwrite(fid,h,accuracy); fclose(fid);

% Wind-stress
tauMax=0.1;
x=((1:nx)-0.5)/(nx-1); % nx-1 accounts for a solid wall
y=((1:ny)-0.5)/(ny-1); % ny-1 accounts for a solid wall
[X,Y]=ndgrid(x,y);
tau=-tauMax*cos(2*pi*Y);
fid=fopen('windx.m01cos2y','w',ieee); fwrite(fid,tau,accuracy); fclose(fid);

% dye
dye=zeros(60,60,1);
dye(2,30,1)=1;
fid=fopen('dye.bin','w',ieee); fwrite(fid,dye,accuracy); fclose(fid);
