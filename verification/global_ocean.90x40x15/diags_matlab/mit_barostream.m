function [psi, psimask] = mit_barostream(u,varargin)
%function [psi, psimask] = mit_barostream(u,umask,dy,dz);
% or
%function [psi, psimask] = mit_barostream(u,gridinformation);
% global barotropic stream function for mitgcm model, time slabs are
% handled as cell objects, integration from north to the south (by
% convention). 

  if nargin == 2
    g = varargin{1};
    umask = g.umask;
    dy = g.dyg;
    dz = g.dz;
  elseif nargin == 4
    umask = varargin{1};
    dy = varargin{2};
    dz = varargin{3};
  else
    error('need 2 (one of which is the grid structure) or 4 arguments')
  end
  
  [nx ny nz] = size(umask);
  for kz=1:nz
    dydzs(:,:,kz) = (umask(:,:,kz).*dy)*dz(kz);
  end

  % mask for stream function
  pmask = change(squeeze(umask(:,:,1)),'==',NaN,0);
  % add psi-point to the north of all wet points
  pmask(1:nx,2:ny) = pmask(1:nx,2:ny)+pmask(1:nx,1:ny-1);
  pmask = change(pmask,'==',0,NaN);
  pmask = change(pmask,'~=',NaN,1);
  % integrate from the north to the south (by convention), change
  % integration direction by flipping the array ubar (transposed because
  % of MITgcm conventions)
  if iscell(u)
    nt = length(u);
    psi = cell(size(u));
    for k=1:nt
      udxdz = change(u{k}.*dydzs,'==',NaN,0);
      ubar = squeeze(sum(udxdz,3));
      psi{k} = fliplr(cumsum(fliplr(ubar),2)).*pmask;
    end
  else
    nt = size(u,4);
    psi = repmat(NaN,[nx ny nt]);
    udxdz = change(u.*repmat(dydzs,[1 1 1 nt]),'==',NaN,0);
    ubar = squeeze(sum(udxdz,3));
    for kt = 1:nt
      psi(:,:,kt) = fliplr(cumsum(fliplr(squeeze(ubar(:,:,kt))),2)).*pmask;
    end
  end
  if nargout == 2
    psimask = pmask;
  end
    
  return
