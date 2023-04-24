function grph_CS_6t(var,c1,c2,nc,titv,Cgrd)
% grph_CS_6t(var,c1,c2,[nc],[titv]) : plot 1 field on the 6 faces of the CS-grid
% c1 < c2 = min & max for the color graph
% c1 > c2 = scale with min,max of the field, + c1/100 and + c2/100
% nc = nb of grid pts (interior only) in 1 direction for 1 face of the cube
%      (default: nc=32); if halo are included => size(var) > nc*nc*6
% titv = title
% Cgrd : C-grid position: 0 = center ; 1 = U-vel ; 2 = V-vel ; 3 = Z-point

if nargin < 6, Cgrd=0; end
if nargin < 5, ktit=0; titv=' '; else ktit=1; end
if nargin < 4, nc=32; end

dim=length(size(var));
if dim==2,
 n1h=size(var,1); n2h=size(var,2);
 if n1h == 6*n2h, nx=n2h;
  var=permute(reshape(var,[nx 6 nx]),[1 3 2]);
 elseif n1h*6 == n2h, nx=n1h;
  var=reshape(var,[nx nx 6]);
 else
  error([' var size: ',int2str(n1h),' x ',int2str(n2h),' does not fit regular cube !']);
 end
 ny=nx;
else
 nx=size(var,1); ny=size(var,2);
end

 olx=nx-nc; oly=ny-nc;
if rem(olx,2) == 0,
 %- symmetric overlap in X:
 olx=olx/2;
else
 %- non-symmetric: if centered, larger overlap at the beginning; if edge, at the end :
 if rem(Cgrd,2) == 0, olx=ceil(olx/2);
 else olx=floor(olx/2); end
end
if rem(oly,2) == 0,
 %- symmetric overlap in U:
 oly=oly/2;
else
 %- non-symmetric: if centered, larger overlap at the beginning; if edge, at the end :
 if Cgrd < 2, oly=ceil(oly/2);
 else oly=floor(oly/2); end
end
 xax=[1:nx]-.5-olx; yax=[1:ny]-.5-oly;
 if rem(Cgrd,2) == 1, xax=xax-.5; end
 if Cgrd > 1, yax=yax-.5; end
fprintf(' nx,ny= %i, %i ; olx,oly= %4.1f,%4.1f ; xax range: %5.1f,%5.1f ; yax: %5.1f,%5.1f\n', ...
          nx,ny,olx,oly,xax(1),xax(end),yax(1),yax(end));
j1=0; j2=nc;

mn=min(var(:)); mx=max(var(:));
if c1 >= c2
  mb=(mx-mn)*0.01;
  c1=mn+mb*c1;
  c2=mx+mb*c2;
end
 fprintf(' min,max %8.3e %8.3e ; Cmin,max %8.3e %8.3e \n',mn,mx,c1,c2)
 [xyP]=def_subP(6);
%------
 for n=1:6
% subplot(320+n);
  axes('position',xyP(n,:));
  v1t=var(:,:,n);
  imagesc(xax,yax,v1t') ; caxis([c1 c2]);set(gca,'YDir','normal') ;
  grid;
  if nx > nc || ny > nc,
    hold on;
    [L]=line([j1 j2 j2 j1 j1],[j1 j1 j2 j2 j1]);
    hold off; set(L,'color',[0 0 0]);
  end
  title(['Face Nb : ',int2str(n)]);
 end
%------
%- matlab 6 :
%pp=get(gca,'position');
%H=colorbar('vertical');
%set(gca,'position',pp);
%set(H,'position',[0.13 0.03 0.64  0.031]);
%set(H,'position',[0.49 0.036 0.02  0.26]);
%text(22.,-5.,sprintf('min= %9.5g ', mn));
%text(22.,-8.,sprintf('Max= %9.5g ', mx));
%- matlab 7 :
H=colorbar('WestOutside');
set(H,'position',[0.512 0.036 0.008 0.26]);
text(22.,-5.,sprintf('min= %9.5g ', mn));
text(22.,-8.,sprintf('Max= %9.5g ', mx));
if ktit == 1,
 axes('position',[.01,.01,.99,.99],'Visible','off');
 [T]=text(0.5,0.97,titv);
 set(T,'HorizontalAlignment','center','FontSize',12);
end
return
