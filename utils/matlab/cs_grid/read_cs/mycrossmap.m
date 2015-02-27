function mycrossmap(fld,cx,label,mask,col)

% Function mycrossmap(fld,cx,label,mask,col)
% plot a cube sphere field as a cross
% fixes a discontinuity at edges of cube faces
% requires mypcolor.m
%
% INPUTS
% fld   input cube sphere field (n,6,n) where n is the face dimension
% cx    two element vector [cmin cmax] sets scaling for caxis
% label colorbar label
% mask  landmask: 0 is land, 1 is ocean

clf reset

for f=1:6
  eval(['a' int2str(f) '=squeeze(fld(:,' int2str(f) ',:));']);
end

if nargin<2
  cx=[min(fld(:)) max(fld(:))];
end

if nargin<3
  label='';
end

cm=colormap;
if nargin>3
  if nargin<5
    col=[1 1 1]*.5;
  end
  fld(find(fld<cx(1)))=cx(1);
  fld(find(fld>cx(2)))=cx(2);
  cm=[col; cm; col];
  colormap(cm)
  fld(find(mask==0))=nan;
end

c1=cx(1)-2*(cx(2)-cx(1))/length(cm);
c2=cx(2)+2*(cx(2)-cx(1))/length(cm);  

if nargin>3
  for f=1:6
    eval(['in=find(isnan(a' int2str(f) '));']);
    eval(['a' int2str(f) '(in(1))=c1;']);
    eval(['a' int2str(f) '(in(2:end))=c2;']);
  end
end

tmp=cat(1,a2,rot90(a4,-1),rot90(a5,-1),a1);

subplot('position',[.05 .35 .9 .3])
h1=mypcolor(tmp');
pos=get(gca,'position');
set(gca,'visible','off')

ax3=axes('position',[pos(1)+pos(3)/4*2 pos(2)+pos(4) pos(3)/4 pos(4)]);
h31=mypcolor(rot90(a3',2));
set(gca,'visible','off')

ax6=axes('position',[pos(1)+pos(3)/4*2 pos(2)-pos(4) pos(3)/4 pos(4)]);
h61=mypcolor(rot90(a6'));
set(gca,'visible','off')

axx=findobj(gcf,'type','axes');
set(axx,'clim',[c1 c2])

%old versions
%ax=colorbar('position',[pos(1)+pos(3)/4/2/2 pos(2)+pos(4)*1.5 pos(3)/4*1.5 pos(4)/5]);
%colorbar(ax)

%new for R2009
ax=colorbar('location','southoutside');
set(ax,'position',[pos(1)+pos(3)/4/2/2 pos(2)+pos(4)*1.5 pos(3)/4*1.5 pos(4)/5]);
set(get(ax,'xlabel'),'string',label)
set(ax,'XLim',cx)
