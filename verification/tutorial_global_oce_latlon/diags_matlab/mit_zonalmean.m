function meanfield = mit_zonalmean(field,hfac,dx);
%function meanfield = mit_zonalmean(field,hfac,dx);

  [nx ny nz] = size(field);

  area = hfac.*repmat(dx,[1 1 nz]);
  meanfield = squeeze(nanmean(field.*area)./nanmean(area));
  
  knil = find(meanfield == 0);
  meanfield(knil) = NaN;
  
  return
