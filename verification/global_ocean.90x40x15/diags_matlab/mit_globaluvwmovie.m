% plot velocity fields
figure
colormap(jet0);
for k=pkt;
  clear uc vc
  uc(1:grd.nx-1,:) = .5*(u(1:grd.nx-1,:,iz,k)+u(2:grd.nx,:,iz,k));
  uc(grd.nx,:)     = .5*(u(1,:,iz,k)+u(grd.nx,:,iz,k));
  uc = uc.*grd.cmask(:,:,iz);
  vc(:,1:grd.ny-1) = .5*(v(:,1:grd.ny-1,iz,k)+v(:,2:grd.ny,iz,k));
  vc(:,grd.ny)     = .5*v(:,grd.ny,iz,k);
  vc = vc.*grd.cmask(:,:,iz);
  imagesc(grd.lonc,grd.latc,w(:,:,iz,k)'); caxis([-1 1]*max(abs(caxis)));
  hold on,NaNquiver(grd.xc,grd.yc,uc,vc); hold off
  axis xy; axis image
  set(gca,'clipping','off')
  set(gca,'xlim',[0 360])
%  colorbar('h')
  title(['velocity field in 3D, depth = ' num2str(grd.zc(iz)) ]) 
  suptitle(['experiment ' dname ', timestep = ' num2str(timesteps(k)) ...
	   ', ' tuname ' = ' num2str(tim(k))])
  drawnow; pause(delay);
end

