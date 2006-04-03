%
%  $Id: assemble_SH.m,v 1.1 2006/04/03 20:55:29 molod Exp $
%
%  Ed Hill
%
%  Generate the APE SH01 -- SH30 fields:
%
%    For all time steps:
%      For all variables:
%        For all facets:
%          compute the "derived" fields:
%          sum:
%    Compute the averages from the sums:
%    Rotate vector components as necessary:
%    Convert units as necessary:
%    Regrid onto lat-lon:
%    Write netCDF output with all attributes:
%

%======================================================================
%
%  Define the connections between the APE "SH" variables and the
%  MITgcm diagnostics output
%
nfacets = 6;
months.i_start = 7;
months.i_end   = 42;
bname = 'data/SHave.t%03d.nc';
gname = 'data/grid.t%03d.nc';
r_con_name = 'scrip_regrid/rmp_CS32_to_LL128x64_conserv.nc';
r_dwt_name = 'scrip_regrid/rmp_CS32_to_LL128x64_distwgt.nc';
oname = { 'SH_fields.nc' };

flags.regrid_type_for_vec = 2;
flags.debug               = 0;

%  Get the following from:
%    ! (cd ../../ape_data_specs/ ; ./sh_parse.sh)
vars = {};
vars{1} = {'SH01','sh_sw_toai','RADSWT','toa_incoming_shortwave_flux','W m-2','-','1'};
vars{2} = {'SH02','sh_sw_toar','OSR','toa_outgoing_shortwave_flux','W m-2','-','-1'};
vars{3} = {'SH04','sh_lw_toa','OLR','toa_outgoing_longwave_flux','W m-2','-','1'};
vars{4} = {'SH07','sh_cld_frac','CLDFRC','cloud_area_fraction','1','-','1'};
vars{5} = {'SH08','sh_cldw','---','atmosphere_cloud_condensed_water_co','kg m-2','-','1'};
vars{6} = {'SH09','sh_cldi','---','atmosphere_cloud_ice_content','kg m-2','-','1'};
vars{7} = {'SH11','sh_cppn','PRECON','convective_precipitation_flux','kg m-2 s-1','-','0.000011574074'};
vars{8} = {'SH12','sh_dppn','SH12','large_scale_precipitation_flux','kg m-2 s-1','-','0.000011574074'};
vars{9} = {'SH13','sh_evap','EVAP','evaporation_flux','kg m-2 s-1','-','0.000011574074'};
vars{10} = {'SH15','sh_sswi','SH15','surface_downwelling_shortwave_flux','W m-2','-','1'};
vars{11} = {'SH16','sh_sswr','SH16','surface_upwelling_shortwave_flux','W m-2','-','-1'};
vars{12} = {'SH18','sh_slwd','LWGDOWN','surface_downwelling_longwave_flux','W m-2','-','-1'};
vars{13} = {'SH19','sh_slwu','LWGUP','surface_upwelling_longwave_flux','W m-2','-','1'};
vars{14} = {'SH21','sh_slh','EFLUX','surface_upward_latent_heat_flux','W m-2','-','1'};
vars{15} = {'SH22','sh_ssh','HFLUX','surface_upward_sensible_heat_flux','W m-2','-','1'};
vars{16} = {'SH24','sh_ts2m','T2M','surface_air_temperature','K','-','1'};
vars{17} = {'SH25','sh_q2m','Q2M','specific_humidity','1','-','0.001'};
vars{18} = {'SH26','sh_tauu','UFLUX','surface_downward_eastward_stress','Pa','u','-1'};
vars{19} = {'SH27','sh_tauv','VFLUX','surface_downward_northward_stress','Pa','v','-1'};
vars{20} = {'SH28','sh_u10m','U10M','eastward_wind','m s-1','u','1'};
vars{21} = {'SH29','sh_v10m','V10M','northward_wind','m s-1','v','1'};
vars{22} = {'SH30','sh_ps','PS','surface_air_pressure','Pa','-','100'};



%======================================================================
%
%  Open the input files and get the (minimum) number of time steps
%  available across all the files
%
disp('Finding available time steps')

ncl = {};
nTmin = 0;
ifacet = 1;
for ifacet = 1:nfacets
  fname = sprintf(bname,ifacet);
  nc = netcdf(fname, 'nowrite');
  ncl{ifacet} = nc;
  T = nc{'T'}(:);
  nT = length(T);
  if (ifacet == 1)
    nTmin = nT;
  else
    nTmin = min(nTmin,nT);
  end
  nc = [];
end
disp(sprintf('  total iterations found = %d',nTmin));


%======================================================================
%
%  Compute the derived fields and the temporal sums of all fields on
%  the original MITgcm grid
%
disp('Computing Sums')

dat = [];
iz = 1;
it = 1;
iv = 1;
ifacet = 1;
for it = [ 1 ]  % months.i_start:min(nTmin,months.i_end)
  disp(sprintf('  iteration = %d',it));
  for iv = 1:length(vars)
    for ifacet = 1:nfacets
      
      %  cull unknown variables 
      if ( vars{iv}{3}(1) == '-' )
        continue
      end
      %  disp(sprintf('%d, %d',iv,ifacet));
      
      %  skip over computed quantities
      if strcmp( vars{iv}{1} , vars{iv}{3} )
        continue
      end

      %  WARN about missing variables
      nc = ncl{ifacet};
      if prod(size(nc{vars{iv}{3}})) == 0
        if it == months.i_start
          str = 'var "%s" does not exist in file "%s"';
          disp(sprintf(['  Warning: ' str], vars{iv}{3}, name(nc)));
        end
        continue
      end
      
      if it == 1
        dat(iv,ifacet).n = 1;
        dat(iv,ifacet).a = squeeze( nc{ vars{iv}{3} }(it,iz,:,:) );
      else
        dat(iv,ifacet).n = dat(iv,ifacet).n + 1;
        dat(iv,ifacet).a = dat(iv,ifacet).a + ...
            squeeze( nc{ vars{iv}{3} }(it,iz,:,:) );
      end
      
    end
  end
  
  %  fill in the derived fields
  for iv = 1:length(vars)
    for ifacet = 1:nfacets
      tmp = [];
      nc = ncl{ifacet};
      
      if strncmp(vars{iv}{1},'SH12',4)
        %  SH12 = PREACC - PRECON
        tmp = squeeze( nc{ 'PREACC' }(it,iz,:,:) ) ...
              - squeeze( nc{ 'PRECON' }(it,iz,:,:) );
      end
      
      if  strncmp(vars{iv}{1},'SH15',4) || strncmp(vars{iv}{1},'SH16',4)
        albedo = ( squeeze( nc{ 'ALBNIRDF' }(it,iz,:,:)) ...
                   + squeeze( nc{ 'ALBVISDF' }(it,iz,:,:)) ) ./ 2.0;
        if vars{iv}{1} == 'SH15'
          %  SH15 = RADSWG/(1 - ALBEDO)
          tmp = squeeze( nc{ 'RADSWG' }(it,iz,:,:) ) ...
              ./ (1.0 - albedo);
        end
        if strncmp(vars{iv}{1},'SH16',4)
          %  SH16 = RADSWG*(1/(1 - ALBEDO) - 1)
          tmp = squeeze( nc{ 'RADSWG' }(it,iz,:,:) ) ...
                .* (1.0./(1.0 - albedo) - 1.0);
        end
      end

      if prod(size(tmp)) > 0
        if it == 1
          dat(iv,ifacet).n = 1;
          dat(iv,ifacet).a = tmp;
        else
          dat(iv,ifacet).n = dat(iv,ifacet).n + 1;
          dat(iv,ifacet).a = dat(iv,ifacet).a + tmp;
        end
      end
      
    end
  end
end
clear tmp

%  close the input files
for ifacet = 1:nfacets
  nc = close( ncl{ifacet} );
end
clear ncl;

%======================================================================
%
%  Compute the averages from the temporal sums
%
disp('Computing Averages from Sums')

ave = [];
for iv = 1:length(vars)
  ind = length(ave) + 1;
  for ifacet = 1:nfacets
    if length(dat(iv,ifacet).n) == 0
      continue
    end
    ave(ind).v(:,:,ifacet) = dat(iv,ifacet).a ./ dat(iv,ifacet).n;
    ave(ind).ivar = iv;
  end
end

clear dat;

%======================================================================
%
%  Rotate vector components & convert units
%
clear ig;
gvars = { 'XC','YC','dxF','dyF','rA','XG','YG','dxV', ...
          'dyU','rAz','dxC','dyC','rAw','rAs','dxG','dyG' };
for iface = 1:6
  fname = sprintf(gname, iface);
  nc = netcdf(fname, 'nowrite');
  if iface == 1
    ig.ne = length(nc('X'));
  end
  for jj = 1:length(gvars)
    comm = [ 'ig.' gvars{jj} '(:,:,iface) = nc{''' ...
             gvars{jj} '''}(:);'];
    eval(comm);
  end
  nc = close(nc);
end

ne = ig.ne;
n1 = ne - 1;
dux = zeros(size(ig.XC));
duy = zeros(size(ig.XC));
dvx = zeros(size(ig.XC));
dvy = zeros(size(ig.XC));
% dux(:,:,:) = diff(ig.XG(:,1:ne,:),1,1);
% dvx(:,:,:) = diff(ig.XG(1:ne,:,:),1,2);
% duy(:,:,:) = diff(ig.YG(:,1:ne,:),1,1);
% dvy(:,:,:) = diff(ig.YG(1:ne,:,:),1,2);
%
%  Note the first two dimensions below are permuted relative to the
%  ordering in the *.mitgrid files (commented-out above).
dux(:,:,:) = diff(ig.XG(1:ne,:,:),1,2);  
dvx(:,:,:) = diff(ig.XG(:,1:ne,:),1,1);
duy(:,:,:) = diff(ig.YG(1:ne,:,:),1,2);
dvy(:,:,:) = diff(ig.YG(:,1:ne,:),1,1);
dux = dux + 360*double(dux < 180);
dux = dux - 360*double(dux > 180);  %  squeeze([ min(min(dux)) max(max(dux)) ])
duy = duy + 360*double(duy < 180);
duy = duy - 360*double(duy > 180);  %  squeeze([ min(min(duy)) max(max(duy)) ])
dvx = dvx + 360*double(dvx < 180);
dvx = dvx - 360*double(dvx > 180);  %  squeeze([ min(min(dvx)) max(max(dvx)) ])
dvy = dvy + 360*double(dvy < 180);
dvy = dvy - 360*double(dvy > 180);  %  squeeze([ min(min(dvy)) max(max(dvy)) ])
dut = sqrt(dux.^2 + duy.^2);  %  squeeze([ min(min(dut)) max(max(dut)) ])
dvt = sqrt(dvx.^2 + dvy.^2);  %  squeeze([ min(min(dvt)) max(max(dvt)) ])
ig.llux = dux ./ dut;  %  squeeze([ min(min(ig.llux)) max(max(ig.llux)) ])
ig.lluy = duy ./ dut;  %  squeeze([ min(min(ig.lluy)) max(max(ig.lluy)) ])
ig.llvx = dvx ./ dvt;  %  squeeze([ min(min(ig.llvx)) max(max(ig.llvx)) ])
ig.llvy = dvy ./ dvt;  %  squeeze([ min(min(ig.llvy)) max(max(ig.llvy)) ])
clear dux duy dvx dvy dut dvt ne n1

if flags.regrid_type_for_vec ~= 2
  %  Vector rotation
  for ia = 1:length(ave)
    if strcmp(vars{ ave(ia).ivar }{6}, 'u')
      %  llu = u .* llux  +  v .* llvx;
      %  llv = u .* lluy  +  v .* llvy;
      llu = ave(ia).v .* ig.llux  +  ave(ia+1).v .* ig.llvx;
      llv = ave(ia).v .* ig.lluy  +  ave(ia+1).v .* ig.llvy;
      ave(ia).v   = llu;
      ave(ia+1).v = llv;
    end
  end
end
clear llu llv

%  Units conversion
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
%  Regrid
%
%    JMC suggested an evenly spaced Lat-Lon at: 128x64
%
disp('Regridding')

og = [];
og.nlat = 64;
og.nlon = 128;
og.latcell = 180/og.nlat;
og.loncell = 360/og.nlon;
og.lat = linspace(-90+og.latcell/2, 90-og.latcell/2, og.nlat);
og.lon = linspace(0+og.loncell/2, 360-og.loncell/2, og.nlon);
og.latbnd(:,1) = og.lat - og.latcell/2.0;
og.latbnd(:,2) = og.lat + og.latcell/2.0;
og.lonbnd(:,1) = og.lon - og.loncell/2.0;
og.lonbnd(:,2) = og.lon + og.loncell/2.0;

rgvar = { 'src_address', 'dst_address', 'remap_matrix' };
rtype = { { 'con', r_con_name}, {'dwt', r_dwt_name } };
for kk = 1:length(rtype)
  nc = netcdf(rtype{kk}{2}, 'nowrite');
  for jj = 1:length(rgvar)
    comm = sprintf('rgrid.%s.%s = nc{''%s''}(:);',...
                   rtype{kk}{1},rgvar{jj},rgvar{jj});
    eval(comm);
    %  disp(comm);
  end
  nc = close(nc);
end

if ( flags.regrid_type_for_vec > 0 )
  % >> x=rdmds('XC');
  % >> y=rdmds('YC');
  % >> t=rdmds('Ttave.0000513360');
  % >> xi=-179:2:180;yi=-89:2:90;
  % >> del=cube2latlon_preprocess(x,y,xi,yi);
  % >> ti=cube2latlon_fast(del,t);
  aa_regrid.x = rdmds('aa_regrid/XC');
  aa_regrid.y = rdmds('aa_regrid/YC');
  %  u = rdmds('aa_regrid/U.0000000000');
  %  v = rdmds('aa_regrid/V.0000000000');
  %  del = cube2latlon_preprocess(LON,LAT,xc,yc);
  %  
  %  aa_regrid.del = ...
  %      cube2latlon_preprocess(aa_regrid.x,aa_regrid.y, ...
  %                             og.lon,og.lat);
end

%  save  zzz
%  zzz

ii = 1;
for ii = 1:length(ave)
  
  dcp = zeros(prod(size( ave(ii).v )),1);
  nn = 0;
  for kk = 1:size( ave(ii).v, 3 )
    for jk = 1:size( ave(ii).v, 2 )
      for ik = 1:size( ave(ii).v, 1 )
        nn = nn + 1;
        dcp(nn) = ave(ii).v(ik,jk,kk);
      end
    end
  end
  lld = zeros(og.nlat*og.nlon,1);
  
  if ( ( flags.regrid_type_for_vec > 0 )  ...
       && ( vars{ave(ii).ivar}{6} == 'u'  ...
            || vars{ave(ii).ivar}{6} == 'v' ) )
    
    if flags.regrid_type_for_vec == 1

      %  use distributed weights for vectors
      for jj = 1:length(rgrid.dwt.src_address)
        lld(rgrid.dwt.dst_address(jj)) = ...
            lld(rgrid.dwt.dst_address(jj)) ...
            + ( dcp(rgrid.dwt.src_address(jj)) ...
                * rgrid.dwt.remap_matrix(jj,1) );
      end
      ave(ii).r = reshape(lld',og.nlat,og.nlon);
    
    elseif ( flags.regrid_type_for_vec == 2  ...
             &&  vars{ave(ii).ivar}{6} == 'u' )
      
      %  use Alistair's uvcube2latlon() for vectors
      nsiz = size(ave(ii).v);
      by6u = zeros([ nsiz(1)*nsiz(3) nsiz(2) ]);
      by6v = zeros([ nsiz(1)*nsiz(3) nsiz(2) ]);
      for kk = 1:size( ave(ii).v, 3 )
        is = 1 + (kk-1)*nsiz(1);
        ie = is + nsiz(1) - 1;
        by6u(is:ie,:) = ave(ii  ).v(:,:,kk)';
        by6v(is:ie,:) = ave(ii+1).v(:,:,kk)';
      end
      %  xi=-179:2:180;  yi=-89:2:90;
      xi = og.lon;
      ind = find(xi > 179.5);
      xi(ind) = xi(ind) - 360;
      yi = og.lat;
      [ul,vl] = uvacube2latlon(aa_regrid.x,aa_regrid.y, ...
                               by6u,by6v, xi,yi);
      ave(ii  ).r = ul';
      ave(ii+1).r = vl';
      
      if flags.debug > 0
        figure(1), subplot(1,1,1)
        subplot(2,1,1), surf(ul'), axis equal, view(2), shading flat
        subplot(2,1,2), surf(vl'), axis equal, view(2), shading flat
      end
            
    elseif ( vars{ave(ii).ivar}{6} == 'v' )
      
      disp(sprintf( '  %s was computed along with %s', ...
                    vars{ave(ii).ivar}{2}, vars{ave(ii-1).ivar}{2} ));
      
    elseif ( vars{ave(ii).ivar}{6} == 'u' )
      
      disp('Error: please specify regrid type for vectors');
      disp('  using "flags.regrid_type_for_vec"');
      
    end
  else
    %  conservative for scalars 
    for jj = 1:length(rgrid.con.src_address)
      lld(rgrid.con.dst_address(jj)) = ...
          lld(rgrid.con.dst_address(jj)) ...
          + ( dcp(rgrid.con.src_address(jj)) ...
              * rgrid.con.remap_matrix(jj,1) );
    end
    ave(ii).r = reshape(lld',og.nlat,og.nlon);
  end
    
  if flags.debug > 0
    figure(2)
    surf(og.lon,og.lat,ll128x64), view(2), shading flat
  end
  
end


%======================================================================
%
%  Write netCDF output
%
disp('Writing netCDF output for SH')

nc = netcdf(oname{1}, 'clobber');
nc.title = [ 'Aqua Planet: ' ...
             'Single-Level 2-D Means from "Example" Experiment' ]; 
nc.institution = 'MIT Dept. of EAPS, Cambridge, MA, USA';
nc.source = [ 'MITgcm: ' ];
nc.Conventions = 'CF-1.0';
nc.history = [ 'Original data produced: ' '2002/08/20' ];

nc('lon') = og.nlon;
nc('lat') = og.nlat;
nc('bnd') = 2;

nc{'lon'} = ncdouble('lon');
nc{'lon'}.standard_name = 'longitude';
nc{'lon'}.units = 'degrees_east';
nc{'lon'}.bounds = 'lon_bnds';
nc{'lon'}(:) = og.lon;

nc{'lat'} = ncdouble('lat');
nc{'lat'}.standard_name = 'latitude';
nc{'lat'}.units = 'degrees_north';
nc{'lat'}.bounds = 'lat_bnds';
nc{'lat'}(:) = og.lat;

nc{'lon_bnds'} = ncdouble('lon','bnd');
nc{'lon_bnds'}.ape_name = 'longitude cell bounds';
nc{'lon_bnds'}.units = 'degrees_east';
nc{'lon_bnds'}(:) = og.lonbnd;

nc{'lat_bnds'} = ncdouble('lat','bnd');
nc{'lat_bnds'}.ape_name = 'latitude cell bounds';
nc{'lat_bnds'}.units = 'degrees_north';
nc{'lat_bnds'}(:) = og.latbnd;

for ii = 1:length(ave)
  
  iv = ave(ii).ivar;
  nc{ vars{iv}{2} } = ncfloat( 'lat', 'lon' );
  nc{ vars{iv}{2} }.ape_name = vars{iv}{4};
  nc{ vars{iv}{2} }.units = vars{iv}{5};
  nc{ vars{iv}{2} }.FillValue_ = 1.0e20;
  
  %  deal with missing values
  ind = find(isnan( ave(ii).r ));
  ave(ii).r(ind) = 1.0e20;
  
  nc{ vars{iv}{2} }(:) = ave(ii).r;

end

nc = close(nc);

