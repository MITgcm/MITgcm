% m-file: mit_globalmovie.m
% plot temperature and salinity fields at that depth

% $Header: /u/gcmpack/MITgcm/verification/tutorial_global_oce_latlon/diags_matlab/mit_globalmovie.m,v 1.3 2006/08/12 20:25:12 jmc Exp $
% $Name:  $

iz = 1;
delay = .5;
disp(['depth = ' num2str(grd.zc(iz))])

if meanfields
  pkt = nt;
else
  pkt = kt(1:kmax);
end

mit_globaltsmovie;
mit_globaluvwmovie;


