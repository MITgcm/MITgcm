function ncep2global_ocean
% function ncep2global_ocean

% read various fluxes, adds them up to have net heat flux and net
% freshwater flux (E-P only)
% interpolates the fluxes onto 90x40 grid of global_ocean.90x40x15

% constants
  ql_evap = 2.5e6; % latent heat due to evaporation
  rho_fresh = 1000; % density of fresh water
  onemonth = 60*60*24*30;
  % grid parameter
  grd = mit_loadgrid(DIRECTORY WHERE THE GRID INFORMATION IS HELD ...
		     (XC*, YC*, HFAC* etc.));
  lonc = grd.lonc;
  latc = grd.latc;
  cmask = grd.cmask; % 1=wet point, NaN = dry point
  rac = grd.rac;
  
  bdir = DIRECTORY WHERE THE NCEP DATA FILES ARE STORED;
  % taux
  ncload(fullfile(bdir,'ncep_taux_monthly.cdf'),'taux','X','Y','T')
  % tauy
  ncload(fullfile(bdir,'ncep_tauy_monthly.cdf'),'tauy')
  % there appears to be a different east west convention in the data
  taux = - taux;
  tauy = - tauy;
  
  % qnet
  ncload(fullfile(bdir,'ncep_netsolar_monthly.cdf'),'solr')
  ncload(fullfile(bdir,'ncep_netlw_monthly.cdf'),'lwflx') % long wave radiation
  ncload(fullfile(bdir,'ncep_latent_monthly.cdf'),'heat_flux');
  lhflx = heat_flux;
  ncload(fullfile(bdir,'ncep_sensible_monthly.cdf'),'heat_flux');
  shflx = heat_flux;
  
  % emp
  ncload(fullfile(bdir,'ncep_precip_monthly.cdf'),'prate')
  ncload(fullfile(bdir,'ncep_runoff_monthly.cdf'),'runoff')
  evap = lhflx/ql_evap/rho_fresh; % evaporation rate estimated from latent
				  % heat flux
  precip = prate/rho_fresh;       % change the units		       

  % add the fields 
  % net heat flux: sum of solar radiation, long wave flux, latent heat flux, 
  % and sensible heat flux
  qnet  = solr + lwflx + lhflx + shflx;
  % net fresh water flux: difference between evaporation and precipitation
  emp   = evap - precip;
  % substract the runoff (doesn't lead anywhere, unfortunately)
  empmr = emp - change(runoff,'==',NaN,0)/rho_fresh/onemonth;
  runoff(find(isnan(runoff)))=0;
  empmr = emp - runoff/rho_fresh/onemonth;
  
  % interpolate the three fields onto 90x40 grid
  ir = interp2global(X,Y,lonc,latc); % this sets up the
                                     % interpolator ir!
  for k = 1:length(T);
    tauxg(:,:,k)  = interp2global(X,Y,squeeze(taux(k,:,:)),lonc,latc,ir);
    tauyg(:,:,k)  = interp2global(X,Y,squeeze(tauy(k,:,:)),lonc,latc,ir);
    qnetg(:,:,k)  = interp2global(X,Y,squeeze(qnet(k,:,:)),lonc,latc,ir);
    empg(:,:,k)   = interp2global(X,Y,squeeze(emp(k,:,:)),lonc,latc,ir);
    empmrg(:,:,k) = interp2global(X,Y,squeeze(empmr(k,:,:)),lonc,latc,ir);
  end
  
  % apply landmask
  for k = 1:length(T);
    tauxg(:,:,k)  = tauxg(:,:,k).*cmask(:,:,1);
    tauyg(:,:,k)  = tauyg(:,:,k).*cmask(:,:,1);
    qnetg(:,:,k)  = qnetg(:,:,k).*cmask(:,:,1);
    empg(:,:,k)   = empg(:,:,k).*cmask(:,:,1);
    empmrg(:,:,k) = empmrg(:,:,k).*cmask(:,:,1);
  end
  % balance the fluxes
  effrac = rac.*cmask(:,:,1);
  mqnet  = mit_mean(qnetg,effrac);
  qnetb  = qnetg-mean(mqnet);
  
  memp   = mit_mean(empg,effrac);
  empb   = empg-mean(memp);
  mempmr = mit_mean(empmrg,effrac);
  empmrb = empmrg-mean(mempmr);
  
  % apply the landmasks one more time (because we have added something
  % to what should have been zero == land)
  for k = 1:length(T);
    qnetb(:,:,k)  = qnetb(:,:,k).*smask;
    empb(:,:,k)   = empb(:,:,k).*smask;
    empmrb(:,:,k) = empmrb(:,:,k).*smask;
  end

  % save the fields
  acc = 'real*4';
  mit_writefield('ncep_taux.bin',change(tauxg,'==',NaN,0),acc);
  mit_writefield('ncep_tauy.bin',change(tauyg,'==',NaN,0),acc);
  mit_writefield('ncep_qnet.bin',change(qnetb,'==',NaN,0),acc);
  mit_writefield('ncep_emp.bin',change(empb,'==',NaN,0),acc);
  mit_writefield('ncep_empmr.bin',change(empmrb,'==',NaN,0),acc);
  
  return
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

function Y = mit_mean(X,area);
%function Y = mit_mean(X,area);
  
  na = ndims(area);
  n = ndims(X);
  full_area = nansum(area(:));
  if n == 2 & na == 2
    [nx ny] = size(X);
    Xa = X.*area;
    Y = nansum(Xa(:))/full_area;
  elseif n == 3 & na == 2
    [nx ny nt] = size(X);
    Xa = X.*repmat(area,[1 1 nt]);
    for k=1:nt
      Y(k) = nansum(nansum(Xa(:,:,k)))/full_area;
    end
  elseif n == 3 & na == 3
    [nx ny nz] = size(X);
    Xa = X.*area;
    Y = nansum(Xa(:))/full_area;
  else
    error('X and area have to have consistent dimensions')
  end
  
  return
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
  
function count = mit_writefield(fname,h,accuracy)
%function count = mit_writefield(fname,h,accuracy)
  
  ieee='ieee-be';

  [fid message] = fopen(fname,'w',ieee);
  if fid <= 0
    error([message ', filename: ', [fname]])
  end

  clear count
  dims = size(h);
  if length(dims) == 2
    count = fwrite(fid,h,accuracy);
  elseif length(dims) == 3
    for k=1:dims(3);
      count(k) = fwrite(fid,squeeze(h(:,:,k)),accuracy);
    end
  elseif length(dims) == 4
    for kt=1:dims(4)
      for k=1:dims(3)
      count(k,kt) = fwrite(fid,squeeze(h(:,:,k,kt)),accuracy);
      end
    end
  end
    
  fclose(fid);
  
  return

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

function [zi] = interp2global(varargin);
%function [zi] = interp2global(x,y,z,xi,yi,ir);
  
  if nargin == 4
    [xx yy] = meshgrid(varargin{1},varargin{2});
    xx = reshape(xx,prod(size(xx)),1);
    yy = reshape(yy,prod(size(yy)),1);
    [xxi yyi] = meshgrid(varargin{3},varargin{4});
    xxv = reshape(xxi,prod(size(xxi)),1);
    yyv = reshape(yyi,prod(size(yyi)),1);
    % create map
    if ~exist('ir','var');
      r = 230e3; %m
      for k = 1:length(xxv);
	% find all points within radius r
	radius = lonlat2dist(xxv(k),yyv(k),xx,yy);
	ir{k} = find(radius<=r);
      end
    end
    zi = ir;
  else
%  zi = interp2_global(x,y,z,ix,iy','spline')';
    z  = varargin{3};
    xi = varargin{4};
    yi = varargin{5};
    [xxi yyi] = meshgrid(varargin{4},varargin{5});
    ir = varargin{6};
    zzv = repmat(NaN,length(ir),1);
    % interpolate
    for k = 1:length(ir);
      % find all points within radius r
      zzv(k) = mean(z(ir{k}));
    end
    zi = reshape(zzv,size(xxi))';
  end
  
  return

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

function r = lonlat2dist(lon,lat,lons,lats)
%function r = lonlat2dist(lon,lat,lons,lats)
%
% returns distance (in meters) between postion (lon,lat) and positions
% (lons,lats) on the earth (sphere). length(r) = length(lons)
  
  earth=6371000;
  deg2rad=pi/180;
  nx=length(lon);
  lt = deg2rad*lat;
  ln = deg2rad*lon;
  lts = deg2rad*lats;
  lns = deg2rad*lons;
  alpha = ...
     acos( ( cos(lt)*cos(lts) ).*cos(ln-lns) ...
	 + ( sin(lt)*sin(lts) )           );
  r = earth*abs(alpha');

  return

