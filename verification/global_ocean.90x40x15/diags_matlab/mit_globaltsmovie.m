figure('PaperPosition',[0.31 0.25 10.5 7.88],'PaperOrientation','landscape');
colormap('default'); cmap = colormap; colormap([.75*ones(1,3); cmap]);
for k=pkt;
  subplot(2,1,1),imagesc(grd.lonc,grd.latc,t(:,:,iz,k)'); 
  axis xy; axis image; %caxis([-1.5 6]); 
  title(['Temperature, depth = ' num2str(grd.zc(iz))]) 
  subplot(2,1,2),imagesc(grd.lonc,grd.latc,s(:,:,iz,k)'); 
%  axis xy; axis image; caxis([30 40]);
  axis xy; axis image; caxis([32 37]);
  title(['Salinity, depth = ' num2str(grd.zc(iz))])
  suptitle(['experiment ' dname ', timestep = ' num2str(timesteps(k)) ...
	   ', ' tuname ' = ' num2str(tim(k))])
  drawnow; pause(delay); 
end
