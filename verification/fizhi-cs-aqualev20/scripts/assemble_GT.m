%
%  $Id: assemble_GT.m,v 1.1 2006/04/03 20:55:28 molod Exp $
%
%  Ed Hill
%
%  Generate the APE GT fields:
%
%    For all time steps:
%      For all variables:
%    Convert units as necessary:
%    Write netCDF output with all attributes:
%

%======================================================================
%
%  Define the connections between the APE "GT" variables and the
%  MITgcm diagnostics output
%
months.i_start = 7;
months.i_end   = 42;
bname = 'data/GTall.nc';
oname = { 'GT_fields.nc' };

%  Get the following from:
%    ! (cd ../../ape_data_specs/ ; ./gt_parse.sh)
vars = {};
vars{1} = {'GT01','gt_sw_toai','RADSWT_ave', ...
           'toa_incoming_shortwave_flux','W m-2','-','1',''};
vars{2} = {'GT02','gt_sw_toar','OSR_ave', ...
           'toa_outgoing_shortwave_flux','W m-2','-','-1',''};
vars{3} = {'GT04','gt_lw_toa','OLR_ave', ...
           'toa_outgoing_longwave_flux','W m-2','-','1',''};
vars{4} = {'GT07','gt_cld_frac','CLDFRC_ave', ...
           'cloud_area_fraction','1','-','1',''};
vars{5} = {'GT08','gt_cldw','---', ...
           'atmosphere_cloud_condensed_water_co','kg m-2','-','1',''};
vars{6} = {'GT09','gt_cldi','---', ...
           'atmosphere_cloud_ice_content','kg m-2','-','1',''};
vars{7} = {'GT11','gt_cppn','PRECON_ave', ...
           'convective_precipitation_flux','kg m-2 s-1','-',...
           '0.000011574074','units = PRECON / (24*3600)'};
vars{8} = {'GT12','gt_dppn','GT12', ...
           'large_scale_precipitation_flux','kg m-2 s-1','-', ...
           '0.000011574074','GT12 = PREACC - PRECON'};
vars{9} = {'GT13','gt_evap','EVAP_ave', ...
           'evaporation_flux','kg m-2 s-1','-', ...
           '0.000011574074','units = EVAP / (24*3600)'};
vars{10} = {'GT15','gt_sswi','GT15', ...
            'surface_downwelling_shortwave_flux','W m-2','-', ...
            '1','GT15 = RADSWG/(1 - (ALBNIRDF + ALBVISDF)/2)'};
vars{11} = {'GT16','gt_sswr','GT16', ...
            'surface_upwelling_shortwave_flux','W m-2','-','-1', ...
            'GT16 = RADSWG*(1/(1 - (ALBNIRDF + ALBVISDF)/2) - 1)'};
vars{12} = {'GT18','gt_slwd','LWGDOWN_ave', ...
            'surface_downwelling_longwave_flux','W m-2','-','-1',''};
vars{13} = {'GT19','gt_slwu','LWGUP_ave', ...
            'surface_upwelling_longwave_flux','W m-2','-','1',''};
vars{14} = {'GT21','gt_slh','EFLUX_ave', ...
            'surface_upward_latent_heat_flux','W m-2','-','1',''};
vars{15} = {'GT22','gt_ssh','HFLUX_ave', ...
            'surface_upward_sensible_heat_flux','W m-2','-','1',''};
vars{16} = {'GT24','gt_ts2m','T2M_ave', ...
            'surface_air_temperature','K','-','1',''};
vars{17} = {'GT25','gt_ps','PS_ave', ...
            'surface_air_pressure','Pa','-','100',''};


%======================================================================
%
%  Open the input files and get the (minimum) number of time steps
%  available across all the files
%
disp('Finding available time steps')

ncl = {};
nTmin = 0;
ifacet = 1;
% for ifacet = 1:nfacets
  fname = bname;
  nc = netcdf(fname, 'nowrite');
  ncl{ifacet} = nc;
  T = nc{'T'}(:);
  nT = length(T);
  if (ifacet == 1)
    nTmin = nT;
  else
    nTmin = min(nTmin, nT);
  end
  nc = [];
% end
disp(sprintf('  total iterations found = %d',nTmin));


%======================================================================
%
%  Open the input files and get the (minimum) number of time steps
%  available across all the files
%
disp('Reading the data')

nc = ncl{1};
time = nc{ 'T' }(:);

for iv = 1:length(vars)
      
  %  cull unknown variables 
  if ( vars{iv}{3}(1) == '-' )
    continue
  end
  
  %  skip over computed quantities
  if strcmp( vars{iv}{1} , vars{iv}{3} )
    continue
  end

  %  WARN about missing variables
  nc = ncl{ifacet};
  if prod(size(nc{vars{iv}{3}})) == 0
    if it == 1
      str = 'var "%s" does not exist in file "%s"';
      disp(sprintf(['  Warning: ' str], vars{iv}{3}, name(nc)));
    end
    continue
  end
      
  %  get the data
  t0 = squeeze( nc{ vars{iv}{3} }(:) );
  
  dat(iv,ifacet).n = 1;
  dat(iv,ifacet).a = t0;

end

%  fill in the derived fields
for iv = 1:length(vars)
  tmp = [];
  nc = ncl{ifacet};
      
  if strncmp(vars{iv}{1},'GT12',4)
    %  GT12 = PREACC_ave - PRECON_ave
    tmp = squeeze( nc{ 'PREACC_ave' }(:) ) ...
          - squeeze( nc{ 'PRECON_ave' }(:) );
  end
  
  if  strncmp(vars{iv}{1},'GT15',4) || strncmp(vars{iv}{1},'GT16',4)
    albedo = ( squeeze( nc{ 'ALBNIRDF_ave' }(:)) ...
               + squeeze( nc{ 'ALBVISDF_ave' }(:)) ) ./ 2.0;
    if vars{iv}{1} == 'GT15'
      %  GT15 = RADSWG_ave/(1 - ALBEDO_ave)
      tmp = squeeze( nc{ 'RADSWG_ave' }(:) ) ...
            ./ (1.0 - albedo);
    end
    if strncmp(vars{iv}{1},'GT16',4)
      %  GT16 = RADSWG_ave*(1/(1 - ALBEDO_ave) - 1)
      tmp = squeeze( nc{ 'RADSWG_ave' }(:) ) ...
            .* (1.0./(1.0 - albedo) - 1.0);
    end
  end
  
  if length(tmp) > 0
    dat(iv,ifacet).n = 1;
    dat(iv,ifacet).a = tmp;
  end
  
end

ave = [];
for iv = 1:length(vars)
  ind = length(ave) + 1;
  ifacet = 1;
  if length(dat(iv,ifacet).n) == 0
    continue
  end
  ave(ind).v(:,ifacet) = dat(iv,ifacet).a;
  ave(ind).ivar = iv;
end

%  close the input files
nc = close( ncl{1} );
clear ncl;


%======================================================================
%
%  Convert units
%
disp('Converting units')

for ia = 1:length(ave)
  if strcmp(vars{ ave(ia).ivar }{7}, '-') ~= 1
    eval(['fac = ' vars{ ave(ia).ivar }{7} ';']);
    if fac ~= 1
      ave(ia).v = fac .* ave(ia).v;
    end
  end
end


%======================================================================
%
%  Write netCDF output
%
disp('Writing netCDF output')

nc = netcdf(oname{1}, 'clobber');
%nc.title = [ 'Aqua Planet: ' ...
%             'Single-Level 2-D Means from "Example" Experiment' ]; 
nc.institution = 'MIT Dept. of EAPS, Cambridge, MA, USA';
nc.source = [ 'MITgcm: ' ];
nc.Conventions = 'CF-1.0';
%nc.history = [ 'Original data produced: ' '2002/08/20' ];

nc('time') = length( ave(1).v );
nc('bnd') = 2;

nc{'time'} = ncdouble('time');
nc{'time'}.standard_name = 'time';
nc{'time'}.units = 'days since 0000-01-01';
nc{'time'}.bounds = 'time_bnds';
nc{'time'}(:) = time / (24*3600);

%float time_bnds(time, bnd) ;
%time_bnds:long_name = "time interval endpoints" ;
nc{'time_bnds'} = ncdouble('time','bnd');
nc{'time_bnds'}.ape_name = 'time interval endpoints';
nc{'time_bnds'}.units = 'days since 0000-01-01';
tbds(1,:) = (time - 24*3600)/(24*3600);
tbds(2,:) = (time)/(24*3600);
nc{'time_bnds'}(:) = tbds';


for ii = 1:length(ave)
  
  iv = ave(ii).ivar;
  nc{ vars{iv}{2} } = ncfloat( 'time' );
  nc{ vars{iv}{2} }.ape_name = vars{iv}{4};
  nc{ vars{iv}{2} }.units = vars{iv}{5};
  nc{ vars{iv}{2} }.FillValue_ = 1.0e20;
  
  nc{ vars{iv}{2} }(:) = ave(ii).v;

end

nc = close(nc);
