% This is a matlab script that generates the input data

nx=32;ny=nx;ntx=6;

h=zeros(nx,ntx,ny);

x=0.25*((0.5:nx)/nx-0.5);
[X,Y]=ndgrid(x,x);
R=sqrt(X.^2+Y.^2);


tileno=1;

o=1;
i=round(nx*1/2);
j=round(ny*1/2);
h(i:i+o,tileno,j:j+o)=1;

h(:,1,:)=0.5+0.5*cos(   pi*min(R,0*R+0.04)/0.04 );

%- PS anomaly = 100.mb (10^4 Pa):
h=h*1.e+4;

fid=fopen('ps100mb.bin','w','b');
fwrite(fid,h,'real*8');
fclose(fid);
