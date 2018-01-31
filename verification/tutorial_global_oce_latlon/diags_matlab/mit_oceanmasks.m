function grid = mit_oceanmasks(grid,plotit)
% function grid = mit_oceanmasks(grid)
% create masks for the 4x4 degree run for the individual oceans
% hfacc/s/w have to be available for this
  
  if grid.nx ~= 90 | grid.ny ~= 40 
    error(sprintf( ...
	'%s\n%s\n%s\n%s', ...
	'Your horizontal grid has dimensions different from 90x40.', ...
	'mit_oceanmasks creates masks for the individual ocean basins,', ...
	'therefore it needs to be customized if you change the', ...
	'horizontal dimensions.'))
  end
    
  % masks for the zonal velocities:
  % hfacw
  
  % Atlantic mask
  grid.atlantic_hfacw = grid.hfacw;
  ixsow = 6:73; % southern ocean and most of pacific
  iysow = 1:24; 
  grid.atlantic_hfacw(ixsow,iysow,:) = NaN;
  ixnpw = 15:67; % northern pacific
  iynpw = 25:37;
  grid.atlantic_hfacw(ixnpw,iynpw,:) = NaN;
  % Pacific mask
  grid.pacific_hfacw = grid.hfacw;
  ixaw = 74:90; % most of the atlantic
  iyaw = 1:40;
  grid.pacific_hfacw(ixaw,iyaw,:) = NaN;
  ixnaw = 68:73; % north atlantic and gulf of Mexico
  iynaw = 27:40;
  grid.pacific_hfacw(ixnaw,iynaw,:) = NaN;
  ixcaw = 70:73; % carribean
  iycaw = 24:26;
  grid.pacific_hfacw(ixcaw,iycaw,:) = NaN;
  ixiw = 1:27; % most of the indic
  iyiw = 1:40;
  grid.pacific_hfacw(ixiw,iyiw,:) = NaN;
  ixauw = 28:35; % water south and west of Australia
  iyauw = 1:20;
  grid.pacific_hfacw(ixauw,iyauw,:) = NaN;
  
  
  % masks for the meridional overturning stream functions:
  % hfacs
  
  % Atlantic mask
  grid.atlantic_hfacs = grid.hfacs;
  ixso = 5:73; % southern ocean
  iyso = 1:24;
  grid.atlantic_hfacs(ixso,iyso,:) = NaN;
  ixnp = 14:65; % northern pacific
  iynp = 25:36; 
  grid.atlantic_hfacs(ixnp,iynp,:) = NaN;
  ixsoc = 1:90; % complete southern ocean
  iysoc = 1:12; % south of latg(12) = -34 degN;
  grid.atlantic_hfacs(ixsoc,iysoc,:) = NaN;
  
  % Pacific mask
  grid.pacific_hfacs = grid.hfacs;
  ixatl = [1:26 74:90]; % most of altantic and indian ocean
  iyatl = 1:grid.ny;
  grid.pacific_hfacs(ixatl,iyatl,:) = NaN;
  ixatlf = 73; % small fix
  iyatlf = 31;
  grid.pacific_hfacs(ixatlf,iyatlf,:) = NaN;
  ixgm = 66:73; % Gulf of Mexico
  iygm = 25:30;
  grid.pacific_hfacs(ixgm,iygm,:) = NaN;
  ixhb = 68:73; % Hudson Bay and Baffin Bay
  iyhb = 36:40;
  grid.pacific_hfacs(ixhb,iyhb,:) = NaN;
  ixsop = 1:grid.nx; % southern ocean
  iysop = 1:12;
  grid.pacific_hfacs(ixsop,iysop,:) = NaN;
  ixio = 27:32; % rest of indian ocean
  iyio = 13:19;
  grid.pacific_hfacs(ixio,iyio,:) = NaN;
  %pacific_hfacs(33:34,12,:) = NaN; % two points south of Australia
  
  % Indian ocean
  grid.indic_hfacs = grid.hfacs;
  ixsoi = 1:90; % southern ocean
  iysoi = 1:12;
  grid.indic_hfacs(ixsoi,iysoi,:) = NaN;
  ixap = [1:4 33:90]; % atlantic and pacific
  iyap = 1:40;
  grid.indic_hfacs(ixap,iyap,:) = NaN;
  iynps = 35:40; % north polar sea
  grid.indic_hfacs(:,iynps,:) = NaN;
  ixmeds = 1:10;  % mediterraenan
  iymeds = 29:31;
  grid.indic_hfacs(ixmeds,iymeds,:) = NaN;
  ixscs = 27:32; % south china sea
  iyscs = 21:29;
  grid.indic_hfacs(ixscs,iyscs,:) = NaN;
  grid.indic_hfacs(5,39,:) = NaN; % singular point near Murmansk
  
  % Southern Ocean
  grid.so_hfacs = change( ...
      change(grid.atlantic_hfacs,'==',NaN,-1) ...
      + change(grid.pacific_hfacs,'==',NaN,-1) ...
      + change(grid.indic_hfacs,'==',NaN,-1) ...
      + grid.hfacs-grid.hfacs,'==',-1,NaN);
  iso = find(~isnan(grid.so_hfacs));
  grid.so_hfacs(iso)= grid.hfacs(iso);

  if exist('plotit','var')
    figure;
    spy(isnan(grid.hfacs(:,:,1)'),'kx');
    hold on; 
    spy(~isnan(grid.atlantic_hfacs(:,:,1)')); 
    spy(~isnan(grid.indic_hfacs(:,:,1)'),'g.'); 
    spy(~isnan(grid.so_hfacs(:,:,1)'),'r.'); 
    spy(~isnan(grid.pacific_hfacs(:,:,1)'),'y.');
    title('hfacs')
    axis xy
  end
  
  % masks for zonal averages of C-properties:
  % hfacc
  
  % Atlantic mask
  grid.atlantic_hfacc = grid.hfacc;
  ixe = 6:65; % indian ocean
  iye = 1:28;
  grid.atlantic_hfacc(ixe,iye,:) = NaN;
  ixep = 26:65; % eastern pacific
  iyep = 1:grid.ny; 
  grid.atlantic_hfacc(ixep,iyep,:) = NaN;
  ixw = 6:73; % western pacific
  iyw = 1:23;
  grid.atlantic_hfacc(ixw,iyw,:) = NaN;
  ixgl = 66:68; % galapagos
  iygl = 24;
  grid.atlantic_hfacc(ixgl,iygl,:) = NaN;
  ixch = 73; % coast of Chile
  iych = 13:15;
  grid.atlantic_hfacc(ixch,iych,:) = NaN;
  
  % Pacific mask
  grid.pacific_hfacc = grid.hfacc;
  ixatl = [1:25 74:90]; % most of altantic and indian ocean
  iyatl = 1:grid.ny;
  grid.pacific_hfacc(ixatl,iyatl,:) = NaN;
  ixatlf = 73; % small fix
  iyatlf = 31;
  grid.pacific_hfacc(ixatlf,iyatlf,:) = NaN;
  ixgme = 69:73; % Gulf of Mexico
  iygme = 24:30;
  ixgmw = 66:69;
  iygmw = 26:28;
  grid.pacific_hfacc(ixgme,iygme,:) = NaN;
  grid.pacific_hfacc(ixgmw,iygmw,:) = NaN;
  ixhbb = 68:73; % Hudson and Baffin Bay
  iyhbb = 35:40;
  grid.pacific_hfacc(ixhbb,iyhbb,:) = NaN;
  ixio = 26:34; % rest of indian ocean
  iyio =  1:19;
  grid.pacific_hfacc(ixio,iyio,:) = NaN;
%  grid.pacific_hfacc(33:34,12,:) = NaN; % two points south of Australia
  
  % Indian ocean
  grid.indic_hfacc = ...
      change(change(grid.atlantic_hfacc,'==',NaN,0),'>',0,NaN) ...
      + change(change(grid.pacific_hfacc,'==',NaN,0),'>',0,NaN) ...
      + grid.hfacc;
  
  if exist('plotit','var')
    figure;
    spy(isnan(grid.hfacc(:,:,1)'),'kx'); axis xy
    hold on; 
    spy(~isnan(grid.atlantic_hfacc(:,:,1)')); 
    spy(~isnan(grid.pacific_hfacc(:,:,1)'),'r.');
    spy(~isnan(grid.indic_hfacc(:,:,1)'),'g.'); 
    axis xy
    title('hfacc');

  end
  
  return
