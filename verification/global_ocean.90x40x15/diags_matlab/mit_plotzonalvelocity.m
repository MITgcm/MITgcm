% m-file: mit_plotzonalvelocity.m

% select timestep
k=kmax;

up = u(:,:,:,k).*grd.pacific_hfacw;
upzm = mit_zonalmean(up,grd.pacific_hfacw,grd.dxg);
ua = u(:,:,:,k).*grd.atlantic_hfacw;
uazm = mit_zonalmean(ua,grd.atlantic_hfacw,grd.dxg);

caxup = [min(upzm(:)) max(upzm(:))];
uplev = -1:.1:1;
if max(abs(caxup)) < .1
  uplev = .5*uplev;
end
if max(abs(caxup)) < .2
  uplev = .2*uplev;
end

caxua = [min(uazm(:)) max(uazm(:))];
ualev = -1:.1:1;
if max(abs(caxua)) < .1
  ualev = .5*ualev;
end
if max(abs(caxua)) < .2
  ualev = .2*ualev;
end

ixpw = 41;
ixpc = 53;
ixpe = 65;

ixaw = 83;
ixac = 85;
ixae = 2;

zaxis = -grd.zc;
zaxis = -grd.zg;
yaxis = grd.latg;
%zaxis = -[1:grd.nz];

figure('PaperPosition',[0.25 0.621429 8 9.75714])
sh = subplot(4,2,1);
pcol(yaxis,zaxis,upzm')
title('Pacific Ocean: zonal average') 
sh(3) = subplot(4,2,3);
pcol(yaxis,zaxis,sq(up(ixpw,:,:))')
title(['section at ' num2str(grd.long(ixpw)) '^{\circ}E'])
sh(5) = subplot(4,2,5);
pcol(yaxis,zaxis,sq(up(ixpc,:,:))')
title(['section at ' num2str(grd.long(ixpc)) '^{\circ}E'])
sh(7) = subplot(4,2,7);
pcol(yaxis,zaxis,sq(up(ixpe,:,:))')
title(['section at ' num2str(grd.long(ixpe)) '^{\circ}E'])
sh(2) = subplot(4,2,2);
pcol(yaxis,zaxis,uazm',ualev)
title('Atlantic Ocean: zonal average') 
sh(4) = subplot(4,2,4);
pcol(yaxis,zaxis,sq(ua(ixaw,:,:))')
title(['section at ' num2str(grd.long(ixaw)) '^{\circ}E'])
sh(6) = subplot(4,2,6);
pcol(yaxis,zaxis,sq(ua(ixac,:,:))')
title(['section at ' num2str(grd.long(ixac)) '^{\circ}E'])
sh(8) = subplot(4,2,8);
pcol(yaxis,zaxis,sq(ua(ixae,:,:))')
title(['section at ' num2str(grd.long(ixae)) '^{\circ}E'])

set(sh,'xlim',[-1 1]*30,'ylim',[-400 0])
set(sh,'layer','top')
set(sh(1:2:end),'clim',[uplev(1) uplev(end)])
set(sh(2:2:end),'clim',[ualev(1) ualev(end)])
set(gcf,'currentAxes',sh(end-1));colorbar('h')
set(gcf,'currentAxes',sh(end));colorbar('h')

suptitle(['experiment ' grd.dname ...
	  ', timestep = ' num2str(timesteps(k)) ...
	   ', ' tuname ' = ' num2str(tim(k)) ', zonal velocity [m/s]'])
