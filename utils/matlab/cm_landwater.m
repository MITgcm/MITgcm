function [c] = cm_landwater(n,varargin)
% [c] = cm_landwater(n)
% [c] = cm_landwater(n,f)
%
%  n - is size of color map
%  f is scalar fraction that should be water (default=0.5)
%    or vector [hmin hmax] from which f will be calculated
%
% e.g.
% >> cm_landwater(64,caxis);
%
% Green-yellor land/blue water colormap

if nargin==1
 f=0.5;
else
 f=varargin{1};
end

if prod(size(f))==2
 hmin=f(1);hmax=f(2);
 f=-hmin/(hmax-hmin);
 f=max(0,f);f=min(1,f);
end

if mod(n,2)==0
 nb=round(n*f(1));
 ng=n-nb;
 nw=0;
else
 nb=floor(n*f(1));
 nw=1;
 ng=n-nb-nw;
end
b=bluewater(nb);
g=greenland(ng);
w=ones(nw,1)*[1 1 0.55];

c=[b' w' g']';
if nargout==0
 colormap(c);
end

%subplot(211)
%plot((0:(n-1))/(n-1),c(:,3:-1:1))
%subplot(212)
%pcol([-1:.1/n:1])
%axis off

% ----------------------------------------------------
function [c] = bluewater(n)
% Blue colormap

x=(0:n-1)'/(n-1);
c=((.7-.7*x)*[1 1 0]+max(0,(1-.65*x))*[0 0 1]);
c=c(end:-1:1,:);
%colormap(c);
% ----------------------------------------------------
function [c] = greenland(n)
% Green land colormap

x=(0:n-1)'/(n-1);
r=max(0,(-.25+1.8*x));
g=.4*(1+2.5*x);
b=0.5*max(0,(-.25+2.0*x));
i=find(r>1);
r(i)=max( 1.7-1*x(i), b(i));
i=find(g>1);
g(i)=max( 1.5-1*x(i), b(i));
c=r*[1 0 0]+g*[0 1 0]+b*[0 0 1];
c=min(max(0,c),1);
%colormap(c);
% ----------------------------------------------------
