Ho=5000;
nx=31;
ny=31;

% Flat bottom at z=-Ho
h=-Ho*ones(nx,ny);
% Walls
h(end,:)=0;
h(:,end)=0;
for j=1:5
  h(end-6+j,end-j:end)=0;
end
fid=fopen('topog.box','w','ieee-be'); fwrite(fid,h,'real*4'); fclose(fid);

% Wind-stress
tauMax=0.5;
x=((1:nx)-0.5)/(nx-1); % nx-1 accounts for a solid wall
y=((1:ny)-0.5)/(ny-1); % ny-1 accounts for a solid wall
[X,Y]=ndgrid(x,y);
%tau=tauMax*sin(pi*Y);
tau=-tauMax*cos(pi*Y);
fid=fopen('windx.cos_y','w','ieee-be'); fwrite(fid,tau,'real*4'); fclose(fid);
