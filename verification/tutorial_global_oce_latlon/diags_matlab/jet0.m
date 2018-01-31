function J = jet0(m)
%JET0    Variant of JET.
%   JET0(M), a variant of JET(M), is the colormap used with the
%   NCSA fluid jet image, but with the central colors set to white.
%   JET0, by itself, is the same length as the current colormap.
%   Use COLORMAP(JET0).
%
%   See also JET, HSV, HOT, PINK, FLAG, COLORMAP, RGBPLOT.

% taken from jet.m, Martin Losch, 3-8-02  
%   C. B. Moler, 5-10-91, 8-19-92.
%   Copyright 1984-2001 The MathWorks, Inc. 
if nargin < 1, m = size(get(gcf,'colormap'),1); end
n = max(round(m/4),1);
x = (1:n)'/n;
y = (n/2:n)'/n;
e = ones(length(x),1);
r = [0*y; 0*e; x; e; flipud(y)];
g = [0*y; x; e; flipud(x); 0*y];
b = [y; e; flipud(x); 0*e; 0*y];
J = [r g b];
while size(J,1) > m
   J(1,:) = [];
   if size(J,1) > m, J(size(J,1),:) = []; end
end
% set central colors to white
if mod(m,2)
  ic = ceil(m/2);
  J(ic-1:ic+1,:) = ones(3,3);
else
  J(m/2:m/2+1,:) = ones(2,3);
end
