% Similar to pcolor except that pcol() doesn't drop the last column
% or row of data (ie. doesn't interpolated). It uses the shading flat
% method by default.
%
% See also  PCOLOR, IMAGESC
function [hh] = pcol(varargin);

%cmap=colormap;
%if cmap(size(cmap,1),:)==[0 0 0]
%else
% if sum(sum(isnan(data)))~=0
%  colormap( [colormap' [0 0 0]']')
% end
%end

% Scale data to fit colormap
%clim=[min(min(data)) max(max(data))];
%data=(data-clim(1))/(clim(2)-clim(1))*(size(colormap,1)-1)+1;
%data(find(isnan(data)==1))=size(colormap,1)+1;

if nargin == 1
%hh=imagesc(data);
 data=varargin{1};
 pcolor(data([1:end 1],[1:end 1]))
% $$$  xtick = get(gca,'XTick')';
% $$$  ytick = get(gca,'YTick')';
% $$$  xt=xtick+.5;
% $$$  yt=ytick+.5;
else
%hh=imagesc(varargin{1:2},data);
 x=varargin{1}(:);
 y=varargin{2}(:);
 data=varargin{3};
 pcolor([x' 2*x(end)-x(end-1)],...
        [y' 2*y(end)-y(end-1)],...
        data([1:end 1],[1:end 1]))
% $$$  dx = diff(x);
% $$$  dy = diff(y);
% $$$  xtick = get(gca,'XTick'); %[x(1:end-1)+.5*dx; x(end)+.5*dx(end)];
% $$$  ytick = get(gca,'YTick'); %[y(1:end-1)+.5*dy; y(end)+.5*dy(end)];
% $$$  xt = interp1(x,[x(1:end-1)+.5*dx; x(end)+.5*dx(end)],xtick);
% $$$  yt = interp1(y,[y(1:end-1)+.5*dy; y(end)+.5*dy(end)],ytick);
end
%set(gca,'YDir','normal')
% fix tickmarks to emulate imagesc behavior
% $$$ dx = diff(x);
% $$$ dy = diff(y);
% $$$ set(gca,'XTick',xt,'XTickLabel',xtick)
% $$$ set(gca,'YTick',yt,'YTickLabel',ytick)
set(gca,'Layer','top')
shading flat;
