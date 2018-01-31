% m-file: mit_plotstreamfunctions.m

% select timestep
k=kmax;

if iscell(u)
  uk = u{k};
else
  uk = squeeze(u(:,:,:,k));
end
if iscell(v)
  vk = v{k};
else
  vk = squeeze(v(:,:,:,k));
end


addlayer = 1;

clear global_psi atlantic_psi baro_psi
% global overturning
global_psi = mit_overturning(vk,grd.hfacs,grd.dxg,grd.dz,addlayer);

% atlantic overturning
atlantic_psi = mit_overturning(vk,grd.atlantic_hfacs,grd.dxg,grd.dz,addlayer);
% pacific overturning
pacific_psi = mit_overturning(vk,grd.pacific_hfacs,grd.dxg,grd.dz,addlayer);

clear addlayer

% global barotropic stream function
baro_psi = mit_barostream(uk,grd.umask,grd.dyg,grd.dz);

% plot stream functions
figure('PaperPosition',[0.31 0.25 10.5 7.88],'PaperOrientation','landscape')
clear sh
sh(1) = subplot(2,2,1);
otlev = [-60:2:60];
contourf(grd.latg,-grd.zgpsi,global_psi'*1e-6,otlev); 
hold on; 
[cs h1] = contour(grd.latg,-grd.zgpsi,global_psi'*1e-6,[0 0]); 
clh1 = clabel(cs);
hold off
caxis([-1 1]*max(abs(global_psi(:)))*1.e-6); colorbar('h')
psimin = min(min(global_psi(:,5:end)));
[iy iz] = find(abs(global_psi(:,:)-psimin)<=1e-4);
text(grd.latg(iy),-grd.zgpsi(iz), ...
     ['\leftarrow ' num2str(psimin*1e-6,'%5.1f')], ...
     'horizontalalignment','left')
title('global overturning streamfunction [Sv]')
sh(2) = subplot(2,2,2);
contourf(grd.latg,-grd.zgpsi,atlantic_psi'*1e-6,otlev); 
hold on; 
[cs h2] = contour(grd.latg,-grd.zgpsi,atlantic_psi'*1e-6,[0 0]); 
clh2 = clabel(cs);
hold off
caxis([-1 1]*max(abs(atlantic_psi(:)))*1.e-6); colorbar('h');
psimax = max(atlantic_psi(13,5:end));
iz = find(abs(atlantic_psi(13,:)-psimax)<=1e-4);
text(grd.latg(13),-grd.zgpsi(iz), ...
     [num2str(psimax*1e-6,'%5.1f') ' \rightarrow'], ...
     'horizontalalignment','right')
psimin = min(min(atlantic_psi(1:35,5:end)));
[iymin,izmin] = find(abs(atlantic_psi(:,:)-psimin)<=1e-4);
text(grd.latg(iymin),-grd.zgpsi(izmin), ...
     [num2str(psimin*1e-6,'%5.1f') ' \rightarrow'], ...
     'horizontalalignment','right')
title('atlantic overturning streamfunction [Sv]')
%
sh(3) = subplot(2,2,3);
contourf(grd.latg,-grd.zgpsi,pacific_psi'*1e-6,otlev); 
hold on; 
[cs h3] = contour(grd.latg,-grd.zgpsi,pacific_psi'*1e-6,[0 0]); 
clh3 = clabel(cs);
hold off
caxis([-1 1]*max(abs(pacific_psi(:)))*1.e-6); colorbar('h');
title('pacific overturning streamfunction [Sv]')
if ~isempty([h1;h2;h3])
  set([h1;h2;h3],'LineWidth',2,'EdgeColor','k');
end
clh = [clh1;clh2;clh3];
if ~isempty(clh)
  set(clh(2:2:end),'FontSize',8);
end
% $$$ [cs h] = contourf(grd.long,grd.latg,baro_psi'*1e-6,20); 
% $$$ if ~isempty(h);
% $$$   set(h,'edgecolor','none'); 
% $$$ end; 
% $$$ axis image; 
% $$$ caxis([-1 1]*max(abs(baro_psi(:)))*1.e-6); colorbar('h');
% $$$ title('global barotropic stream function [Sv]')
bstlev = [-200:20:200];

sh(4) = subplot(2,2,4);
imagesc(grd.long,grd.latg,baro_psi'*1e-6); 
hold on;
[cs h ]=contour(grd.long,grd.latg,baro_psi'*1e-6,bstlev);
set(h,'edgecolor','k')
if ~isempty(h); 
  clh = clabel(cs,h); 
  set(clh,'Fontsize',8);
end
hold off
axis image, axis xy; 
caxis([-1 1]*max(abs(baro_psi(:)))*1.e-6); colorbar('h');
title('global barotropic stream function [Sv]')
suptitle(['experiment ' dname ', timestep = ' num2str(timesteps(k)) ...
	  ', ' tuname ' = ' num2str(tim(k))])
set(sh,'layer','top')

clear addlayer
