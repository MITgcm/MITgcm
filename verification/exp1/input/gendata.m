Ho=2000;
nx=60;
ny=60;

% Flat bottom at z=-Ho
h=-Ho*ones(nx,ny);
% Walls
h(end,:)=0;
h(:,end)=0;
f77write('topog.box',h,'real*8','b');

% Wind-stress
tauMax=0.1;
x=((1:nx)-0.5)/(nx-1); % nx-1 accounts for a solid wall
y=((1:ny)-0.5)/(ny-1); % ny-1 accounts for a solid wall
[X,Y]=ndgrid(x,y);
tau=tauMax*sin(pi*Y);
f77write('windx.sin_y',tau,'real*8','b');
