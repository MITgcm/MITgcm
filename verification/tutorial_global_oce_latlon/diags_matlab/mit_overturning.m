function [psi, psimask] = mit_overturning(v,vmask,dx,dz,addlayer);
%function [psi, psimask] = mit_overturning(v,vmask,dx,dz);
% overturning stream function for mitgcm model, time slabs are handled as 
% cell objects, integration from bottom to top.

  if nargin < 5
    addlayer = 0;
  elseif (nargin > 5 & nargin < 4)
    error('needs 4 or 5 arguments')
  end
  [nx ny nz] = size(vmask);
  for kz=1:nz
    dxdzs(:,:,kz) = (vmask(:,:,kz).*dx)*dz(kz);
  end

  % mask for stream function
  pmask = change(squeeze(nanmean(vmask)),'~=',NaN,1);
  % add another psi-point at the bottom of each column
  for ky=1:ny;
    iz = min(find(isnan(pmask(ky,:))));
    if ~isempty(iz) & iz > 1
      pmask(ky,iz) = 1;
    end
  end
  if iscell(v)
    nt = length(v);
    psi = cell(size(v));
    for k=1:nt
      vdxdz  = v{k}(:,:,:).*dxdzs; %(dxdzs.*vmask);
      zave   = fliplr(change(squeeze(nansum(vdxdz)),'==',NaN,0));
      psi{k} = -fliplr(cumsum(zave,2)).*pmask;
    end
  else
    nt = size(v,4);
    for k=1:nt
      vdxdz = squeeze(v(:,:,:,k)).*dxdzs; %(dxdzs.*vmask);
      zave  = fliplr(squeeze(sum(change(vdxdz,'==',NaN,0),1)));
      psi(:,:,k)   = -fliplr(cumsum(zave,2)).*pmask;
    end
  end
  % add another layer at the bottom; psi is zero in the layer by definition
  if addlayer
    disp('mit_overturning: adding a layer with psi = 0 at the bottom')
    pmask = [pmask pmask(:,end)];
    if iscell(psi)
      nt = length(psi);
      for k=1:nt
	psi{k} = [psi{k} change(psi{k}(:,end),'~=',NaN,0)];
      end
    else
      psi = cat(2,psi,change(psi(:,end,:),'~=',NaN,0));
    end
  end
  if nargout == 2
    psimask = pmask;
  end
    
  return




  
