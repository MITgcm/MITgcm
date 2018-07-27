ieee='b';
accuracy='real*4';

nx=60;
ny=60;

h=zeros(nx,ny);h(1,:)=1;
fid=fopen('Wwall.box32','w',ieee); fwrite(fid,h,accuracy); fclose(fid);
h=zeros(nx,ny);h(:,1)=1;
fid=fopen('Swall.box32','w',ieee); fwrite(fid,h,accuracy); fclose(fid);

% Wind-stress
tauMax=0.1;
x=((1:nx)-0.5)/nx; 
y=((1:ny)-0.5)/ny; 
[X,Y]=ndgrid(x,y);
tau=-tauMax*cos(pi*Y);
fid=fopen('windx.cos_y32','w',ieee); fwrite(fid,tau,accuracy); fclose(fid);
tau=tauMax*sin(pi*Y);
fid=fopen('windx.sin_y32','w',ieee); fwrite(fid,tau,accuracy); fclose(fid);
