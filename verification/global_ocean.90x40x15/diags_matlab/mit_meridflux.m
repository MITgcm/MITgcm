function T = mit_meridflux(surface_flux,dx,dy)
% function T = mit_meridflux(surface_flux,dx,dy)
  
  if nargin == 2
    area = dx;
  elseif nargin == 3
    area = dx.*dy;
  else
    error('need two or three input parameters')
  end
  [nx ny nt] = size(surface_flux);
  for kt=1:nt
    fdxdy(:,:,kt) = change(surface_flux(:,:,kt),'==',NaN,0).*area;
  end

  T = squeeze(sum(cumsum(fdxdy,2),1));
  
  return
