nx=32;ny=32;o=3;nt=6;
N=[nx+2*o ny+2*o nt];
rtype='real*8';
endian='l';

fid=fopen('fort.10','r',endian);
% Centered
xc=f77read(fid,N,rtype);
yc=f77read(fid,N,rtype);
dxf=f77read(fid,N,rtype);
dyf=f77read(fid,N,rtype);
ra=f77read(fid,N,rtype);
% Corners
xg=f77read(fid,N,rtype);
yg=f77read(fid,N,rtype);
dxv=f77read(fid,N,rtype);
dyu=f77read(fid,N,rtype);
raz=f77read(fid,N,rtype);
% Staggered
dxc=f77read(fid,N,rtype);
dyc=f77read(fid,N,rtype);
raw=f77read(fid,N,rtype);
ras=f77read(fid,N,rtype);
dxg=f77read(fid,N,rtype);
dyg=f77read(fid,N,rtype);
aW=f77read(fid,N,rtype);
aS=f77read(fid,N,rtype);
pC=f77read(fid,N,rtype);
pW=f77read(fid,N,rtype);
pS=f77read(fid,N,rtype);
fclose(fid);
