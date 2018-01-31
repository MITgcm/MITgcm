% m-file: mit_globalmovie.m
% plot temperature and salinity fields at that depth

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


