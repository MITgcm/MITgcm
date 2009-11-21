function crossmap(TS,cx)

% Function crossmap(fld,cx)
% plot a cube sphere field as a cross
%
% INPUTS
% fld   input cube sphere field (n,6,n) where n is the face dimension
% cx    two element vector [cmin cmax] sets scaling for caxis

if nargin<2
cx=[min(TS(:)) max(TS(:))];
cx=[-1 1]*min(abs(cx));
end

a1=squeeze(TS(:,1,:));
a2=squeeze(TS(:,2,:));
a3=squeeze(TS(:,3,:));
a4=squeeze(TS(:,4,:));
a5=squeeze(TS(:,5,:));
a6=squeeze(TS(:,6,:));

tmp=cat(1,a2,rot90(a4,-1),rot90(a5,-1),a1);

subplot('position',[.05 .35 .9 .3])
h1=pcolor(tmp');shading flat
set(gca,'xtick',[],'ytick',[])
pos=get(gca,'position');

ax3=axes('position',[pos(1)+pos(3)/4*2 pos(2)+pos(4) pos(3)/4 pos(4)]);
h31=pcolor(rot90(a3',2));shading flat
set(gca,'xtick',[],'ytick',[])
ax6=axes('position',[pos(1)+pos(3)/4*2 pos(2)-pos(4) pos(3)/4 pos(4)]);
h61=pcolor(rot90(a6'));shading flat
set(gca,'xtick',[],'ytick',[])
box off

axx=findobj(gcf,'type','axes');
set(axx,'clim',cx)

%old versions
%ax=colorbar('position',[pos(1)+pos(3)/4/2/2 pos(2)+pos(4)*1.5 pos(3)/4*1.5 pos(4)/5]);
%colorbar(ax)

%new for R2009
ax=colorbar('location','southoutside');
set(ax,'position',[pos(1)+pos(3)/4/2/2 pos(2)+pos(4)*1.5 pos(3)/4*1.5 pos(4)/5]);
