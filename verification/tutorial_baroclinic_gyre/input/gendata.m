ieee='b';
accuracy='real*4';

Ho=1800;
nx=62;
ny=62;
xo=0; yo=15;
dx=1;dy=1;

% Flat bottom at z=-Ho
h=-Ho*ones(nx,ny);
% Walls
h(1,:)=0;h(end,:)=0;
h(:,1)=0;h(:,end)=0;
fid=fopen('bathy.bin','w',ieee); fwrite(fid,h,accuracy); fclose(fid);

% Wind-stress
tauMax=0.1;
x=(xo-dx/2):dx:(xo+(nx-2)*dx+dx/2); 
y=(yo-dy/2):dy:(yo+(ny-2)*dy+dy/2); 
[X,Y]=ndgrid(x,y);
tau=-tauMax*cos(2*pi*((Y-yo)/(ny-2)/dy)); %ny-2 accounts for walls at N,S boundaries
fid=fopen('windx_cosy.bin','w',ieee); fwrite(fid,tau,accuracy); fclose(fid);

%restoring temperature
Tmax=30;Tmin=0;
Trest=(Tmax-Tmin)/(ny-2)*((yo+dy*(ny-2)-Y));
fid=fopen('Trest.bin','w',ieee); fwrite(fid,Trest,accuracy); fclose(fid);
