function meanfield = mit_zonalmean(field,hfac,dx);
%function meanfield = mit_zonalmean(field,hfac,dx);

% $Header: /u/gcmpack/MITgcm/verification/tutorial_global_oce_latlon/diags_matlab/mit_zonalmean.m,v 1.1 2006/08/12 19:37:26 jmc Exp $
% $Name:  $

  [nx ny nz] = size(field);

  area = hfac.*repmat(dx,[1 1 nz]);
  meanfield = squeeze(nanmean(field.*area)./nanmean(area));
  
  knil = find(meanfield == 0);
  meanfield(knil) = NaN;
  
  return
