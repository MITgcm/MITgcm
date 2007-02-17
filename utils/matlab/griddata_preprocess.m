function [del] = griddata_preprocess(x,y,xi,yi,method)
%GRIDDATA_PREPROCESS Pre-calculate Delaunay triangulation for use
%   with GRIDDATA_FAST.
%
%   DEL = GRIDDATA_PREPROCESS(X,Y,XI,YI)

%   Based on
%   Clay M. Thompson 8-21-95
%   Copyright 1984-2001 The MathWorks, Inc. 
%   $Revision: 1.2 $  $Date: 2007/02/17 23:49:43 $

% $Header: /u/gcmpack/MITgcm/utils/matlab/griddata_preprocess.m,v 1.2 2007/02/17 23:49:43 jmc Exp $
% $Name:  $

error(nargchk(4,5,nargin))

if prod(size(xi)) ~= prod(size(yi))
 [yi,xi]=ndgrid(yi,xi);
end

if nargin<6, method = 'linear'; end
if ~isstr(method), 
  error('METHOD must be one of ''linear'',''cubic'',''nearest'', or ''v4''.');
end


switch lower(method),
  case 'linear'
    del = linear(x,y,xi,yi);
% case 'cubic'
%   zi = cubic(x,y,z,xi,yi);
% case 'nearest'
%   zi = nearest(x,y,z,xi,yi);
% case {'invdist','v4'}
%   zi = gdatav4(x,y,z,xi,yi);
  otherwise
    error('Unknown method.');
end
  


%------------------------------------------------------------
function delau = linear(x,y,xi,yi)
%LINEAR Triangle-based linear interpolation

%   Reference: David F. Watson, "Contouring: A guide
%   to the analysis and display of spacial data", Pergamon, 1994.

siz = size(xi);
xi = xi(:); yi = yi(:); % Treat these as columns
x = x(:); y = y(:); % Treat these as columns

% Triangularize the data
tri = delaunayn([x y]);
if isempty(tri),
  warning('Data cannot be triangulated.');
  return
end

% Find the nearest triangle (t)
t = tsearch(x,y,tri,xi,yi);

% Only keep the relevant triangles.
out = find(isnan(t));
if ~isempty(out), t(out) = ones(size(out)); end
tri = tri(t,:);

% Compute Barycentric coordinates (w).  P. 78 in Watson.
del = (x(tri(:,2))-x(tri(:,1))) .* (y(tri(:,3))-y(tri(:,1))) - ...
      (x(tri(:,3))-x(tri(:,1))) .* (y(tri(:,2))-y(tri(:,1)));
w(:,3) = ((x(tri(:,1))-xi).*(y(tri(:,2))-yi) - ...
          (x(tri(:,2))-xi).*(y(tri(:,1))-yi)) ./ del;
w(:,2) = ((x(tri(:,3))-xi).*(y(tri(:,1))-yi) - ...
          (x(tri(:,1))-xi).*(y(tri(:,3))-yi)) ./ del;
w(:,1) = ((x(tri(:,2))-xi).*(y(tri(:,3))-yi) - ...
          (x(tri(:,3))-xi).*(y(tri(:,2))-yi)) ./ del;
w(out,:) = zeros(length(out),3);

delau.tri=tri;
delau.w=w;
delau.siz=siz;
delau.out=out;

%------------------------------------------------------------
