function [z] = cube2latlon(x,y,c,xi,yi)
% z=cube2latlon(x,y,c,xi,yi);
%
% Re-grids model output on expanded spherical cube to lat-lon grid.
%  x,y   are 2-D arrays of the cell-centered coordinates 
%  c     is a 2-D or 3-D scalar field
%  xi,yi are vectors of the new regular lat-lon grid to interpolate to.
%  z     is the interpolated data with dimensions of size(xi) by size(yi).
%
% e.g.
% >> x=rdmds('XC');
% >> y=rdmds('YC');
% >> t=rdmds('Ttave.0000513360');
% >> xi=-179:2:180;yi=-89:2:90;
% >> ti=cube2latlon(x,y,t,xi,yi);
%
% $Header: /u/gcmpack/MITgcm/utils/matlab/Attic/cube2latlon.m,v 1.3 2001/08/28 17:58:03 adcroft Exp $

NN=size(c);
[nx ny nz]=size(c);

for k=1:nz;
X=x;
Y=y;
C=c(:,:,k);

i=3*ny+(1:ny);j=floor(ny/2);
X(end+1,:)=X(i,j)'-360; Y(end+1,:)=Y(i,j)'; C(end+1,:)=C(i,j)';
i=3*ny+(1:ny);j=floor(ny/2)+1;
X(end+1,:)=X(i,j)'+360; Y(end+1,:)=Y(i,j)'; C(end+1,:)=C(i,j)';
i=5*ny+round(ny/2);j=1:floor(ny/2);
X(end+1,j)=X(i,j)+360;
Y(end+1,j)=Y(i,j);
C(end+1,j)=C(i,j);
i=5*ny+round(ny/2)+1;j=1:floor(ny/2);
X(end,j+ny/2)=X(i,j)-360;
Y(end,j+ny/2)=Y(i,j);
C(end,j+ny/2)=C(i,j);
i=2*32+(ny/2+1:ny);j=floor(ny/2);
X(end+1,1:ny/2)=X(i,j)'-360;
Y(end+1,1:ny/2)=Y(i,j)';
C(end+1,1:ny/2)=C(i,j)';
i=2*32+(ny/2+1:ny);j=floor(ny/2)+1;
X(end,ny/2+1:ny)=X(i,j)'+360;
Y(end,ny/2+1:ny)=Y(i,j)';
C(end,ny/2+1:ny)=C(i,j)';

z(:,:,k)=griddata(Y,X,C,yi,xi');
end % k

if size(NN,2)>2
z=reshape(z,[size(z,1) size(z,2) NN(3:end)]);
end
