nx=80;ny=1;nz=1;
Ho=500
frac=0/2;

x=(0.5:nx)'/nx;
y=(0.5:ny)/ny;
[X,Y]=ndgrid(x,y);

H=-Ho*(1-frac*exp( -((x-0.3)/0.1).^2));
H([1 end],:)=0;

fid=fopen('topog.bin','w','b');
fwrite(fid,H,'real*8');
fclose(fid)
