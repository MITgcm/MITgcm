function [mdt, mdtt, mdtstd] = mit_globalmean(dt,rac3d)
% function [mdt, mdtt, mdtstd] = mit_globalmean(dt,rac3d)
  
  denom = nansum(nansum(rac3d));
  mdt    = squeeze(nansum(nansum(dt.*rac3d))./denom);
  mdtt   = squeeze(nansum(nansum(abs(dt).*rac3d))./denom);
  mdtstd = sqrt(squeeze(nansum(nansum(dt.^2.*rac3d))./denom));

  return
