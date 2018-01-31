function zi = griddata_fast(delau,z,method)
%GRIDDATA_FAST Data gridding and surface fitting.
%   ZI = GRIDDATA_FAST(DEL,Z)
%
%   See also GRIDDATA_PREPROCESS

%   Based on
%   Clay M. Thompson 8-21-95
%   Copyright 1984-2001 The MathWorks, Inc. 

error(nargchk(2,3,nargin))

if nargin<3, method = 'linear'; end
if ~isstr(method), 
  error('METHOD must be one of ''linear'',''cubic'',''nearest'', or ''v4''.');
end


% Sort x and y so duplicate points can be averaged before passing to delaunay

switch lower(method),
  case 'linear'
    zi = linear(delau,z);
% case 'cubic'
%   zi = cubic(x,y,z,xi,yi);
% case 'nearest'
%   zi = nearest(x,y,z,xi,yi);
% case {'invdist','v4'}
%   zi = gdatav4(x,y,z,xi,yi);
  otherwise
    error('Unknown method.');
end
  
if nargout<=1, xi = zi; end


%------------------------------------------------------------
function zi = linear(del,z)
%LINEAR Triangle-based linear interpolation

%   Reference: David F. Watson, "Contouring: A guide
%   to the analysis and display of spacial data", Pergamon, 1994.

z = z(:).'; % Treat z as a row so that code below involving
            % z(tri) works even when tri is 1-by-3.
zi = sum(z(del.tri) .* del.w,2);

zi = reshape(zi,del.siz);

if ~isempty(del.out), zi(del.out) = NaN; end

%------------------------------------------------------------
