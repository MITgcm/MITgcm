function hh = myquiver(arg1,arg2,arg3,arg4,arg5,arg6)
%MYQUIVER Quiver (or velocity) plot.
%	MYQUIVER(X,Y,U,V) plots the velocity vectors with components
%	(u,v) at the points (x,y).  The matrices X,Y,U,V must all be
%	the same size and contain the cooresponding position and
%	velocity components (X and Y can also be vectors to specify a
%	uniform grid).  MYQUIVER automatically scales the velocity
%	vectors to fit within the grid.
%
%	MYQUIVER(U,V) plots the velocity vectors at equally spaced
%	points in the x-y plane.
%
%	MYQUIVER(X,Y,S) or MYQUIVER(X,Y,U,V,S,...) automatically scales
%	the velocity vectors to fit within the grid and then multiplies
%	them by S.  Use S=0 to plot the velocity vectors without the
%	automatic scaling.
%
%	MYQUIVER(...,STYLE) uses the plot linestyle specified by the string
%	STYLE for the velocity vectors.  See PLOT for other linestyles.
%
%	H = MYQUIVER(...) returns a vector of line handles.
%
%	Example:
%	   [x,y] = meshgrid(-2:.2:2,-1:.15:1);
%	   z = x .* exp(-x.^2 - y.^2); [px,py] = gradient(z,.2,.15);
%	   contour(x,y,z), hold on
%	   myquiver(x,y,px,py), hold off, axis image
%
%	See also: FEATHER, PLOT, QUIVER

%	Clay M. Thompson 3-3-94
%	Copyright (c) 1994 by The MathWorks, Inc.
%	$Revision: 1.4 $

% modified D Menemenlis 28 mar 95
% modified from QUIVER for more consistent scaling
% search for "modified" in text for details

error(nargchk(2,6,nargin));

% Arrow head parameters
alpha = 0.33; % Size of arrow head relative to the length of the vector
beta = 0.33;  % Width of the base of the arrow head relative to the length
autoscale = 1; % Autoscale if ~= 0 then scale by this.

% Check numeric input arguments
if nargin<4 
  [msg,x,y,u,v] = xyzchk(arg1,arg2);
elseif nargin==4
  if isstr(arg4)
    [msg,x,y,u,v] = xyzchk(arg1,arg2);
  else
    [msg,x,y,u,v] = xyzchk(arg1,arg2,arg3,arg4);
  end
else
  [msg,x,y,u,v] = xyzchk(arg1,arg2,arg3,arg4);
end
if ~isempty(msg), error(msg); end

if nargin==2, % myquiver(u,v)
  lo = get(gca,'LineStyleOrder'); sym = lo(1,:);
elseif nargin==3, % myquiver(u,v,s) or myquiver(u,v,'style')
  if isstr(arg3),
    sym = arg3;
  else
    autoscale = arg3;
    lo = get(gca,'LineStyleOrder'); sym = lo(1,:);
  end
elseif nargin==4,  % myquiver(x,y,u,v) or myquiver(x,y,s,'style')
  if isstr(arg4),
    autoscale = arg3;
    sym = arg4;
  else
    lo = get(gca,'LineStyleOrder'); sym = lo(1,:);
  end
elseif nargin==5, % myquiver(x,y,u,v,s) or myquiver(x,y,u,v,'style')
  if isstr(arg5),
    sym = arg5;
  else
    autoscale = arg5;
    lo = get(gca,'LineStyleOrder'); sym = lo(1,:);
  end
elseif nargin==6, % myquiver(x,y,u,v,s,style)
  autoscale = arg5;
  sym = arg6;
end

if autoscale, 
  % Base autoscale value on average spacing in the x and y
  % directions.  Estimate number of points in each direction as
  % either the size of the input arrays or the effective square
  % spacing if x and y are vectors.
  if min(size(x))==1, n=sqrt(prod(size(x))); m=n; else [m,n]=size(x); end

% modified D Menemenlis 28 mar 95
% replaced "n" and "m" by "(n-1)" and "(m-1)" for more consistent scaling as
% size of domain is increased
  delx = diff([min(x(find(~isnan(x)))) max(x(find(~isnan(x))))])/(n-1); 
  dely = diff([min(y(find(~isnan(y)))) max(y(find(~isnan(y))))])/(m-1);

  len = sqrt((u/delx).^2 + (v/dely).^2);
  autoscale = autoscale*0.9 / max(len(find(~isnan(len))));
  u = u*autoscale; v = v*autoscale;
end

ax = newplot;
next = lower(get(ax,'NextPlot'));
hold_state = ishold;

% Make velocity vectors
x = x(:).'; y = y(:).';
u = u(:).'; v = v(:).';

% modified D Menemenlis 28 mar 95
% added "+0*u" and "+0*v" to remove dots on plot for nans

uu = [x+0*u;x+u;NaN*ones(size(u))];
vv = [y+0*v;y+v;NaN*ones(size(u))];

h = plot(uu(:),vv(:),sym);

% Make arrow heads and plot them
hu = [x+u-alpha*(u+beta*(v+eps));x+u; ...
      x+u-alpha*(u-beta*(v+eps));NaN*ones(size(u))];
hv = [y+v-alpha*(v-beta*(u+eps));y+v; ...
      y+v-alpha*(v+beta*(u+eps));NaN*ones(size(v))];
hold on
h = [h;plot(hu(:),hv(:),sym)];

if ~hold_state, hold off, view(2); set(ax,'NextPlot',next); end

if nargout>0, hh = h; end
