function [] = drawedges(xg,yg,varargin)
% Draws edges of cube on plot
% >> drawedges(xg,yg)
%
% Written by adcroft@.mit.edu, 2001.
[nx ny nt]=size(xg);
hnx=ceil(nx/2);
hny=ceil(ny/2);

if ~isempty(varargin)
 col=varargin{1};
else
 col='k';
end

if ndims(xg)==2 & size(xg,1)==6*size(xg,2)
 [nx ny nt]=size(xg);
 x=permute( reshape(xg,[nx/6 6 ny]),[1 3 2]);
 y=permute( reshape(yg,[nx/6 6 ny]),[1 3 2]);
 [nx ny nt]=size(xg);
elseif ndims(xg)==3 & size(xg,2)==6
 x=permute( X,[1 3 2]);
 y=permute( Y,[1 3 2]);
 [nx ny nt]=size(xg);
elseif ndims(xg)==3 & size(xg,3)==6
 [nx ny nt]=size(xg);
 x=xg;y=yg;
else
 size(xg)
 size(yg)
 error('Dimensions should be 2 or 3 dimensions: NxNx6, 6NxN or Nx6xN');
end


for sh=-180:90:180;
 for k=1:6;
  h=line( longitude(x(:,1,k))+sh, y(:,1,k));set(h,'Color',col);
  h=line( longitude(x(1,:,k))+sh, y(1,:,k));set(h,'Color',col);
 end
end
