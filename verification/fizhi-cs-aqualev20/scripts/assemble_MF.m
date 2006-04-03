%
%  $Id: assemble_MF.m,v 1.1 2006/04/03 20:55:28 molod Exp $
%
%  Ed Hill
%
%  Generate the APE MF fields:
%
%    Read all variables:
%    Convert units as necessary:
%    Regrid onto lat-lon:
%    Write netCDF output with all attributes:
%


%======================================================================
%
%  Define the connections between the APE "ML" variables and the
%  MITgcm diagnostics output
%
oname = { 'MF_fields.nc' };
title = 'Averages variance quantities as 3D fields';
nl  =  64;
np  =  17;


%  Get the following from:
%    ! (cd ../../ape_data_specs/ ; ./mf_parse_v2.sh)
vars = {};
vars{1} = {'MF01','mf_uu','u2totz','zonal_wind_variance','m2 s-2'};
vars{2} = {'MF02','mf_vv','v2totz','meridional_wind_variance','m2 s-2'};
vars{3} = {'MF03','mf_tt','potempsqz','temperature_variance','K2'};
vars{4} = {'MF04','mf_omom','wvelsqz','omega_variance','Pa2 s-2'};
vars{5} = {'MF05','mf_phiphi','phihydsqz','geopotential_variance','(m2 s-2)2'};
vars{6} = {'MF06','mf_qq','saltsqz','specific_humidity_variance','(kg kg-1)2'};
vars{7} = {'MF07','mf_uv','uvtotz','poleward_zonal_momentum_flux','m2 s-2'};
vars{8} = {'MF08','mf_uom','wuEz','vertical_zonal_momentum_flux','m Pa s-2'};
vars{9} = {'MF09','mf_vom','wvNz','vertical_meridional_momentum_flux','m Pa s-2'};
vars{10} = {'MF10','mf_vt','vNthz','poleward_temperature_flux','m s-1 K'};
vars{11} = {'MF11','mf_omt','wvelthz','vertical_temperature_flux','Pa s-1 K'};
vars{12} = {'MF12','mf_vq','vNsltz','poleward_moisture_flux','m s-1 kg kg-1'};
vars{13} = {'MF13','mf_omq','wvelsltz','vertical_moisture_flux','Pa s-1 kg kg-1'};
vars{14} = {'MF14','mf_vphi','vNphiz','poleward_geopotential_flux','m3 s-3'};
vars{15} = {'MF15','mf_sm_uu','smusq','zonal_wind_variance_stationary_mean','m2 s-2'};
vars{16} = {'MF16','mf_sm_vv','smvsq','meridional_wind_variance_stationary_mean','m2 s-2'};
vars{17} = {'MF17','mf_sm_tt','smtsq','temperature_variance_stationary_mean','K2'};
vars{18} = {'MF18','mf_sm_omom','smwsq','omega_variance_stationary_mean','Pa2 s-2'};
vars{19} = {'MF19','mf_sm_phiphi','smphisq','geopotential_variance_stationary_mean','(m2 s-2)2'};
vars{20} = {'MF20','mf_sm_qq','smqsq','specific_humidity_variance_stationary_mean','(kg kg-1)2'};
vars{21} = {'MF21','mf_sm_uv','smuv','poleward_zonal_momentum_flux_stationary_mean','m2 s-2'};
vars{22} = {'MF22','mf_sm_uom','smuw','vertical_zonal_momentum_flux_stationary_mean','m Pa s-2'};
vars{23} = {'MF23','mf_sm_vom','smvw','vertical_meridional_momentum_flux_stationary_mean','m Pa s-2'};
vars{24} = {'MF24','mf_sm_vt','smvt','poleward_temperature_flux_stationary_mean','m s-1 K'};
vars{25} = {'MF25','mf_sm_omt','smwt','vertical_temperature_flux_stationary_mean','Pa s-1 K'};
vars{26} = {'MF26','mf_sm_vq','smvq','poleward_moisture_flux_stationary_mean','m s-1 kg kg-1'};
vars{27} = {'MF27','mf_sm_omq','smwq','vertical_moisture_flux_stationary_mean','Pa s-1 kg kg-1'};
vars{28} = {'MF28','mf_sm_vphi','smvphi','poleward_geopotential_flux_stationary_mean','m3 s-3'};
vars{29} = {'MF29','mf_se_uu','seusq','zonal_wind_variance_stationary_eddy','m2 s-2'};
vars{30} = {'MF30','mf_se_vv','sevsq','meridional_wind_variance__stationary_eddy','m2 s-2'};
vars{31} = {'MF31','mf_se_tt','setsq','temperature_variance_stationary_eddy','K2'};
vars{32} = {'MF32','mf_se_omom','sewsq','omega_variance_stationary_eddy','Pa2 s-2'};
vars{33} = {'MF33','mf_se_phiphi','sephisq','geopotential_variance_stationary_eddy','(m2 s-2)2'};
vars{34} = {'MF34','mf_se_qq','seqsq','specific_humidity_variance_stationary_eddy','(kg kg-1)2'};
vars{35} = {'MF35','mf_se_uv','seuv','poleward_zonal_momentum_flux_stationary_eddy','m2 s-2'};
vars{36} = {'MF36','mf_se_uom','seuw','vertical_zonal_momentum_flux_stationary_eddy','m Pa s-2'};
vars{37} = {'MF37','mf_se_vom','sevw','vertical_meridional_momentum_flux_stationary_eddy','m Pa s-2'};
vars{38} = {'MF38','mf_se_vt','sevt','poleward_temperature_flux_stationary_eddy','m s-1 K'};
vars{39} = {'MF39','mf_se_omt','sewt','vertical_temperature_flux_stationary_eddy','Pa s-1 K'};
vars{40} = {'MF40','mf_se_vq','sevq','poleward_moisture_flux_stationary_eddy','m s-1 kg kg-1'};
vars{41} = {'MF41','mf_se_omq','sewq','vertical_moisture_flux_stationary_eddy','Pa s-1 kg kg-1'};
vars{42} = {'MF42','mf_se_vphi','sevphi','poleward_geopotential_flux_stationary_eddy','m3 s-3'};
vars{43} = {'MF43','mf_tm_uu','tmusq','zonal_wind_variance_transient_mean_merdional','m2 s-2'};
vars{44} = {'MF44','mf_tm_vv','tmvsq','meridional_wind_variance_transient_mean_merdional','m2 s-2'};
vars{45} = {'MF45','mf_tm_tt','tmtsq','temperature_variance_transient_mean_merdional','K2'};
vars{46} = {'MF46','mf_tm_omom','tmwsq','omega_variance_transient_mean_merdional','Pa2 s-2'};
vars{47} = {'MF47','mf_tm_phiphi','tmphisq','geopotential_variance_transient_mean_merdional','(m2 s-2)2'};
vars{48} = {'MF48','mf_tm_qq','tmqsq','specific_humidity_variance_transient_mean_merdional','(kg kg-1)2'};
vars{49} = {'MF49','mf_tm_uv','tmuv','poleward_zonal_momentum_flux_transient_mean_merdional','m2 s-2'};
vars{50} = {'MF50','mf_tm_uom','tmuw','vertical_zonal_momentum_flux_transient_mean_merdional','m Pa s-2'};
vars{51} = {'MF51','mf_tm_vom','tmvw','vertical_meridional_momentum_flux_transient_mean_merdional','m Pa s-2'};
vars{52} = {'MF52','mf_tm_vt','tmvt','poleward_temperature_flux_transient_mean_merdional','m s-1 K'};
vars{53} = {'MF53','mf_tm_omt','tmwt','vertical_temperature_flux_transient_mean_merdional','Pa s-1 K'};
vars{54} = {'MF54','mf_tm_vq','tmvq','poleward_moisture_flux_transient_mean_merdional','m s-1 kg kg-1'};
vars{55} = {'MF55','mf_tm_omq','tmwq','vertical_moisture_flux_transient_mean_merdional','Pa s-1 kg kg-1'};
vars{56} = {'MF56','mf_tm_vphi','tmvphi','poleward_geopotential_flux_transient_mean_merdional','m3 s-3'};
vars{57} = {'MF57','mf_te_uu','teusq','zonal_wind_variance_transient_eddy','m2 s-2'};
vars{58} = {'MF58','mf_te_vv','tevsq','meridional_wind_variance_transient_eddy','m2 s-2'};
vars{59} = {'MF59','mf_te_tt','tetsq','temperature_variance_transient_eddy','K2'};
vars{60} = {'MF60','mf_te_omom','tewsq','omega_variance_transient_eddy','Pa2 s-2'};
vars{61} = {'MF61','mf_te_phiphi','tephisq','geopotential_variance_transient_eddy','(m2 s-2)2'};
vars{62} = {'MF62','mf_te_qq','teqsq','specific_humidity_variance_transient_eddy','(kg kg-1)2'};
vars{63} = {'MF63','mf_te_uv','teuv','poleward_zonal_momentum_flux_transient_eddy','m2 s-2'};
vars{64} = {'MF64','mf_te_uom','teuw','vertical_zonal_momentum_flux_transient_eddy','m Pa s-2'};
vars{65} = {'MF65','mf_te_vom','tevw','vertical_meridional_momentum_flux_transient_eddy','m Pa s-2'};
vars{66} = {'MF66','mf_te_vt','tevt','poleward_temperature_flux_transient_eddy','m s-1 K'};
vars{67} = {'MF67','mf_te_omt','tewt','vertical_temperature_flux_transient_eddy','Pa s-1 K'};
vars{68} = {'MF68','mf_te_vq','tevq','poleward_moisture_flux_transient_eddy','m s-1 kg kg-1'};
vars{69} = {'MF69','mf_te_omq','tewq','vertical_moisture_flux_transient_eddy','Pa s-1 kg kg-1'};
vars{70} = {'MF70','mf_te_vphi','tevphi','poleward_geopotential_flux_transient_eddy','m3 s-3'};



%======================================================================
%
%  Read and save the fields
%
disp('Reading fields')

Pref = [ 1 2 3 5 7 10 15 20 25 30 40 50 60 70 85 92.5 100 ] * 10; 
Pref = Pref(length(Pref):-1:1);
pfac = (Pref./1000).^0.286;

ave = struct([]);
i = 1;
for i = 1:length(vars)
  
  if strcmp(vars{i}{3},'--')
    continue
  end
  
  fid = fopen([ 'data/' vars{i}{3} '.bin' ],'r','ieee-be');
  tmp = fread(fid,nl*np,'real*8');
  fclose(fid);
  ia = length(ave) + 1;
  ave(ia).dat  = reshape(tmp,[nl np]);
  ave(ia).ivar = i;

  %=================================
  %  unit conversions
  
  %  theta ==> T
  if strcmp(vars{i}{2}((end-1):end),'tt')
    disp(['    Converting units th^2 ==> T^2 for: "' vars{i}{2} '"'])
    for il = 1:nl
      ave(ia).dat(il,:) = ave(ia).dat(il,:) .* (pfac.^2);
    end
  elseif strcmp(vars{i}{2}(end:end),'t')
    disp(['    Converting units th ==> T     for: "' vars{i}{2} '"'])
    for il = 1:nl
      ave(ia).dat(il,:) = ave(ia).dat(il,:) .* pfac;
    end
  end
  
  %  geopotential
  %   if length(vars{i}{2}) > 6
  %     if strcmp(vars{i}{2}((end-5):end),'phiphi')
  %       disp(['    Converting units phiphi/g^2   for: "' vars{i}{2} '"'])
  %       ave(ia).dat(:,:) = ave(ia).dat(:,:) ./ (9.81^2);
  %     elseif strcmp(vars{i}{2}((end-2):end),'phi')
  %       disp(['    Converting units phi/g        for: "' vars{i}{2} '"'])
  %       ave(ia).dat(:,:) = ave(ia).dat(:,:) ./ 9.81;
  %     end
  %   end
    
end

%======================================================================
%
%  Grid Info
%
%    JMC suggested an evenly spaced Lat-Lon at: 128x64
%

og = [];
og.nlat = 64;
og.latcell = 180/og.nlat;
og.lat = linspace(-90+og.latcell/2, 90-og.latcell/2, og.nlat);
og.latbnd(:,1) = og.lat - og.latcell/2.0;
og.latbnd(:,2) = og.lat + og.latcell/2.0;

Pref = [ 1 2 3 5 7 10 15 20 25 30 40 50 60 70 85 92.5 100 ] * 10; 
Pref = Pref(length(Pref):-1:1);


%======================================================================
%
%  Write netCDF output
%
disp('Writing netCDF output')

nc = netcdf(oname{1}, 'clobber');
nc.title = [ 'Aqua Planet: ' ...
             title ]; 
nc.institution = 'MIT Dept. of EAPS, Cambridge, MA, USA';
nc.source = [ 'MITgcm: ' ];
nc.Conventions = 'CF-1.0';
%nc.history = [ 'Original data produced: ' '2002/08/20' ];

nc('pres') = length(Pref);
nc('lat') = og.nlat;
nc('bnd') = 2;

nc{'pres'} = ncdouble('pres');
nc{'pres'}.standard_name = 'air_pressure';
nc{'pres'}.units = 'Pa';
nc{'pres'}(:) = Pref*100;

nc{'lat'} = ncdouble('lat');
nc{'lat'}.standard_name = 'latitude';
nc{'lat'}.units = 'degrees_north';
nc{'lat'}.bounds = 'lat_bnds';
nc{'lat'}(:) = og.lat;

nc{'lat_bnds'} = ncdouble('lat','bnd');
nc{'lat_bnds'}.ape_name = 'latitude cell bounds';
nc{'lat_bnds'}.units = 'degrees_north';
nc{'lat_bnds'}(:) = og.latbnd;

for ii = 1:length(ave)
  
  iv = ave(ii).ivar;
  nc{ vars{iv}{2} } = ncfloat( 'pres', 'lat' );
  nc{ vars{iv}{2} }.ape_name = vars{iv}{4};
  nc{ vars{iv}{2} }.units = vars{iv}{5};
  nc{ vars{iv}{2} }.FillValue_ = 1.0e20;
  
  %  handle missing values
  indf = find( ave(ii).dat >= 1.0e14 );
  ave(ii).dat(indf) = 1.0e20;

  nc{ vars{iv}{2} }(:) = ave(ii).dat';

end

nc = close(nc);


