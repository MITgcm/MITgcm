%
%  $Id: assemble_ML.m,v 1.1 2006/04/03 20:55:29 molod Exp $
%
%  Ed Hill
%
%  Generate the APE ML01--ML07 and ME01--ME07 fields:
%
%    For all time steps:
%      For all variables:
%        For all facets:
%          interpolate to the P17 pressure levels:
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
%  Define the connections between the APE "ML" variables and the
%  MITgcm diagnostics output
%
nfacets = 6;
%  bname = 'data/MLave.t%03d.nc';
bname = 'data/ML_ave_%d.nc';
gname = 'data/grid.t%03d.nc';
r_con_name = 'scrip_regrid/rmp_CS32_to_LL128x64_conserv.nc';
r_dwt_name = 'scrip_regrid/rmp_CS32_to_LL128x64_distwgt.nc';
oname = { 'ML_fields.nc' };

flags.regrid_type_for_vec =  2;
flags.debug_lev           =  0;
flags.punits_conv         =  0.01;

%  Get the following from:
%    ! (cd ../../ape_data_specs/ ; ./ml_parse.sh)
vars = {};
vars{1} = {'ML01','ml_u','UVEL','eastward_wind','m s-1','u','1','ee'};
vars{2} = {'ML02','ml_v','VVEL','northward_wind','m s-1','v','1','ee'};
vars{3} = {'ML03','ml_t','ML03','air_temperature','K','-','1','ec'};
vars{4} = {'ML04','ml_om','WVEL','omega','Pa s-1','-','1','ee'};
vars{5} = {'ML05','ml_phi','ML05','geopotential_height','m','-','1','ee'};
vars{6} = {'ML06','ml_q','SALT','specific_humidity','kg kg-1','-','1','cc'};
vars{7} = {'ML07','ml_rh','RELHUM','relative_humidity','percent','-','1','cc'};
vars{8} = {'ML08','ml_th','THETA','potential_temperature','K','-','1','ec'};

%  Geopotential reference values
%  load geopot_20 ;  geopot = geopot_20(:,2);  clear geopot_20
phiref = ...
    [ -0.739878953  646.302002  1338.38696  2894.67627  4099.63135 ...
      5484.93359 7116.62549  9115.08008  10321.4688  11741.3574  13494.4102 ...
      15862.4404 17833.5605  19667.8887  22136.1934  23854.2266  26366.9375 ];

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
  nT = length( nc('T') );
  if (ifacet == 1)
    nTmin = nT;
  else
    nTmin = min(nTmin, nT);
  end
  nc = [];
end
disp(sprintf('  total iterations found = %d',nTmin));


%======================================================================
%
%  Interpolate to the reference pressure levels and then compute the
%  derived fields and the temporal sums of all fields on the original
%  MITgcm grid
%
disp('Computing Sums')

Pref = [ 1 2 3 5 7 10 15 20 25 30 40 50 60 70 85 92.5 100 ] * 10; 
Pref = Pref(length(Pref):-1:1);

dat = [];
it = 1;
iv = 1;
ifacet = 1;
it0 = 1;
for it = [ 1 ] % months.i_start:min(nTmin,months.i_end)
  disp(sprintf('  iteration = %d',it));
  
  for iv = 1:length(vars)
    for ifacet = 1:nfacets

      t0 = [];
      t1 = [];
      vtmp = [];

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
      t0 = squeeze( nc{ vars{iv}{3} }(it,:,:,:) );
      
      %  horizontally interpolate to u,v to mass points
      %
      %  this has changed so that now the "+1" points are garbage
      %  and must be ignored
      switch vars{iv}{6}
       case 'u'
        t1 = t0(:,:,1:32);
       case 'v'
        t1 = t0(:,1:32,:);
       otherwise
        t1 = t0;
      end

      vtmp = t1;
      
      %  sum for averages
      if it0 == 1
        dat(iv,ifacet).n = 1;
        dat(iv,ifacet).a = vtmp;
      else
        dat(iv,ifacet).n = dat(iv,ifacet).n + 1;
        dat(iv,ifacet).a = dat(iv,ifacet).a + vtmp;
      end
      
    end
  end
  
  %  fill in the derived fields
  for iv = 1:length(vars)
    for ifacet = 1:nfacets

      tmp  = [];
      vtmp = [];
      nc = ncl{ifacet};
      
      if strcmp(vars{iv}{1},'ML03')
        %  ML03 = THETA * (P/P0)^0.286
        tmp = squeeze( nc{ 'THETA' }(it,:,:,:) );
        for ii = 1:32
          for jj = 1:32
            tmp(:,ii,jj) = tmp(:,ii,jj) .* ( Pref' ./ 1000 ).^0.286;
          end
        end
      end

      if strcmp(vars{iv}{1},'ML05')
        %  ML05 = (PHIHYD/9.81) + ref
        tmp = squeeze( nc{ 'PHIHYD' }(it,:,:,:) );
        nn = size(tmp);
        for ii = 1:32
          for jj = 1:32
            tmp(:,ii,jj) = squeeze(tmp(:,ii,jj))/9.81 + phiref'; % + geopot;
          end
        end
      end

      if prod(size(tmp)) > 0

        if it0 == 1
          dat(iv,ifacet).n = 1;
          dat(iv,ifacet).a = tmp;
        else
          dat(iv,ifacet).n = dat(iv,ifacet).n + 1;
          dat(iv,ifacet).a = dat(iv,ifacet).a + tmp;
        end
      end
      
    end
  end

  it0 = 0;
  
end
clear tmp

%  close the input files
for ifacet = 1:nfacets
  nc = close( ncl{ifacet} );
end
clear ncl;

if 1 == 0
for iv = 1:length(vars)
  for ifacet = 1:nfacets
    for iz = 1:5:17
      aa = squeeze( dat(iv,ifacet).a(iz,:,:) );
      surf(aa), view(2), shading flat, colorbar
      disp([iv ifacet iz]);
      pause(1)
    end
  end
end
end

%  interpolate u,v to mass points
nc  = 32;
ncp = nc + 1;

for iv = 1:length(vars)
  switch vars{iv}{6}
   case 'u'
    
    nr = size(dat(iv,1).a,1);
    u3d = zeros(nc,nc,nr,6);
    v3d = zeros(nc,nc,nr,6);
    for fi = 1:6
      u3d(:,:,:,fi) = permute( dat(iv,  fi).a, [3 2 1] );
      v3d(:,:,:,fi) = permute( dat(iv+1,fi).a, [3 2 1] );
    end
    
    u6t=zeros(ncp,nc,nr,6);
    v6t=zeros(nc,ncp,nr,6);
    u6t([1:nc],:,:,:)=u3d(:,:,:,:);
    v6t(:,[1:nc],:,:)=v3d(:,:,:,:);
    
    u6t(ncp,[1:nc],:,1)=u3d(1,[1:nc],:,2);
    u6t(ncp,[1:nc],:,2)=v3d([nc:-1:1],1,:,4);
    u6t(ncp,[1:nc],:,3)=u3d(1,[1:nc],:,4);
    u6t(ncp,[1:nc],:,4)=v3d([nc:-1:1],1,:,6);
    u6t(ncp,[1:nc],:,5)=u3d(1,[1:nc],:,6);
    u6t(ncp,[1:nc],:,6)=v3d([nc:-1:1],1,:,2);
    
    v6t([1:nc],ncp,:,1)=u3d(1,[nc:-1:1],:,3);
    v6t([1:nc],ncp,:,2)=v3d([1:nc],1,:,3);
    v6t([1:nc],ncp,:,3)=u3d(1,[nc:-1:1],:,5);
    v6t([1:nc],ncp,:,4)=v3d([1:nc],1,:,5);
    v6t([1:nc],ncp,:,5)=u3d(1,[nc:-1:1],:,1);
    v6t([1:nc],ncp,:,6)=v3d([1:nc],1,:,1);
    
    for fi = 1:6
      dat(iv,  fi).a = permute( ...
          0.5*( u6t([1:nc],:,:,fi) + u6t([2:ncp],:,:,fi) ), [3 2 1] );
      dat(iv+1,fi).a = permute( ...
          0.5*( v6t(:,[1:nc],:,fi) + v6t(:,[2:ncp],:,fi) ), [3 2 1] );
    end
    
  end
end

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
    ave(ind).v(:,:,:,ifacet) = dat(iv,ifacet).a ./ dat(iv,ifacet).n;
    ave(ind).ivar = iv;
  end

  if 1 == 0
    aa = squeeze(ave(ind).v(1,:,:,1));
    surf(aa), view(2), shading interp, colorbar, pause(3)
    close all
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
      % llu = u .* llux  +  v .* llvx;
      % llv = u .* lluy  +  v .* llvy;
      for iz = 1:size(ave(ia).v,1)
        llu = squeeze( ave(ia).v(iz,:,:,:) ) .* ig.llux  ...
              +  squeeze( ave(ia+1).v(iz,:,:,:) ) .* ig.llvx;
        llv = squeeze( ave(ia).v(iz,:,:,:) ) .* ig.lluy  ...
              +  squeeze( ave(ia+1).v(iz,:,:,:) ) .* ig.llvy;
        ave(ia).v(iz,:,:,:)   = llu;
        ave(ia+1).v(iz,:,:,:) = llv;
      end
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
  else
    
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

%  save  ML_ave

ii = 1;
for ii = 1:length(ave)

  for iz = 1:size( ave(ii).v, 1 )

    siz = size( ave(ii).v );
    dcp = zeros(prod(siz(2:4)),1);
    nn = 0;
    for fk = 1:size( ave(ii).v, 4 )
      for jk = 1:size( ave(ii).v, 3 )
        for ik = 1:size( ave(ii).v, 2 )
          nn = nn + 1;
          dcp(nn) = ave(ii).v(iz,ik,jk,fk);
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
        ave(ii).r(iz,:,:) = reshape(lld',og.nlat,og.nlon);
        
      elseif ( flags.regrid_type_for_vec == 2  ...
               &&  vars{ave(ii).ivar}{6} == 'u' )

        %  use Alistair's uvcube2latlon() for vectors
        nsiz = size(ave(ii).v);
        by6u = zeros([ nsiz(2)*nsiz(4) nsiz(3) ]);
        by6v = zeros([ nsiz(2)*nsiz(4) nsiz(3) ]);
        for kk = 1:size( ave(ii).v, 4 )
          is = 1 + (kk-1)*nsiz(2);
          ie = is + nsiz(2) - 1;
          by6u(is:ie,:) = squeeze( ave(ii  ).v(iz,:,:,kk) )';
          by6v(is:ie,:) = squeeze( ave(ii+1).v(iz,:,:,kk) )';
        end
        %  xi=-179:2:180;  yi=-89:2:90;
        xi = og.lon;
        ind = find(xi > 179.5);
        xi(ind) = xi(ind) - 360;
        yi = og.lat;
        [ul,vl] = uvacube2latlon(aa_regrid.x,aa_regrid.y, ...
                                 by6u,by6v, xi,yi);
        ave(ii  ).r(iz,:,:) = ul';
        ave(ii+1).r(iz,:,:) = vl';

        if flags.debug_lev > 0
          figure(1), subplot(1,1,1)
          subplot(2,1,1), surf(ul'), axis equal, view(2), shading flat
          subplot(2,1,2), surf(vl'), axis equal, view(2), shading flat
        end

      elseif ( vars{ave(ii).ivar}{6} == 'v' )
        
        if iz == 1
          disp(sprintf( '  %s was computed along with %s', ...
                        vars{ave(ii).ivar}{2}, vars{ave(ii-1).ivar}{2} ));
        end
      
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
      ave(ii).r(iz,:,:) = reshape(lld',og.nlat,og.nlon);
    end

  end

end


%======================================================================
%
%  Write netCDF output
%
disp('Writing netCDF output')

nc = netcdf(oname{1}, 'clobber');
nc.title = [ 'Aqua Planet: ' ...
             'Single-Level 2-D Means from "Example" Experiment' ]; 
nc.institution = 'MIT Dept. of EAPS, Cambridge, MA, USA';
nc.source = [ 'MITgcm: ' ];
nc.Conventions = 'CF-1.0';
nc.history = [ 'Original data produced: ' '2002/08/20' ];

nc('pres') = length(Pref);
nc('lon') = og.nlon;
nc('lat') = og.nlat;
nc('bnd') = 2;

nc{'pres'} = ncdouble('pres');
nc{'pres'}.standard_name = 'air_pressure';
nc{'pres'}.units = 'Pa';
nc{'pres'}(:) = Pref*100;

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
  nc{ vars{iv}{2} } = ncfloat( 'pres', 'lat', 'lon' );
  nc{ vars{iv}{2} }.ape_name = vars{iv}{4};
  nc{ vars{iv}{2} }.units = vars{iv}{5};
  nc{ vars{iv}{2} }.FillValue_ = 1.0e20;

  nc{ vars{iv}{2} }(:) = ave(ii).r;

end

nc = close(nc);
