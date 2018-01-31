function [z] = cube2latlon(x,y,c,xi,yi,varargin)
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
% Written by adcroft@.mit.edu, 2001.
NN=size(c);
[nx ny nz]=size(c);

X=reshape(x,[1 nx*ny]);
Y=reshape(y,[1 nx*ny]);
ig=find(X>90);
il=find(X<-90);
del=griddata_preprocess([Y Y(il) Y(ig)],[X X(il)+360 X(ig)-360],yi,xi',varargin{:});

for k=1:nz;
 C=reshape(c(:,:,k),[1 nx*ny]);
%z(:,:,k)=griddata([Y Y(il) Y(ig)],[X X(il)+360 X(ig)-360],[C C(il) C(ig)],yi,xi',varargin{:});
 z(:,:,k)=griddata_fast(del,[C C(il) C(ig)],varargin{:});
end % k

% Split vertical and time dimensions
if size(NN,2)>2
z=reshape(z,[size(z,1) size(z,2) NN(3:end)]);
end
