function [z] = cube2latlon_fast(del,c,varargin)
% z=cube2latlon_fast(del,c);
%
% A fast version of cube2latlon that uses pre-processed transformation data
%  del  is the preprocessed data returned by cube2latlon_preprocess
%  c    is a 2-D or 3-D scalar field
%  z    is the interpolated data
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
NN=size(c);
[nx ny nz]=size(c);

for k=1:nz;
C=reshape(c(:,:,k),[1 nx*ny]);

z(:,:,k)=griddata_fast(del,[C C(del.il) C(del.ig)],varargin{:});

end % k

% Split vertical and time dimensions
if size(NN,2)>2
z=reshape(z,[size(z,1) size(z,2) NN(3:end)]);
end
