function [del] = cube2latlon_preprocess(x,y,xi,yi,varargin)
% del=cube2latlon_preprocess(x,y,xi,yi);
%
% Calculates the data required for fast interpolation from the cube to a
% latitude-longitude grid
%  x,y   are 2-D arrays of the cell-centered coordinates 
%  xi,yi are vectors of the new regular lat-lon grid to interpolate to.
%  del   is the transformation data used by cube2latlon_fast
%
% e.g.
% >> x=rdmds('XC');
% >> y=rdmds('YC');
% >> t=rdmds('Ttave.0000513360');
% >> xi=-179:2:180;yi=-89:2:90;
% >> del=cube2latlon_preprocess(x,y,xi,yi);
% >> ti=cube2latlon_fast(del,t);
%
% Written by adcroft@.mit.edu, 2004.
NN=size(x);
[nx ny nz]=size(x);

X=reshape(x,[1 nx*ny]);
Y=reshape(y,[1 nx*ny]);
xMid=mean(X,2);
ig=find(X>xMid+90);
il=find(X<xMid-90);
del=griddata_preprocess([Y Y(il) Y(ig)],[X X(il)+360 X(ig)-360],yi,xi',varargin{:});

del.ig=ig;
del.il=il;
