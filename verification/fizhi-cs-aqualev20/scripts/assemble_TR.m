%
%  $Id: assemble_TR.m,v 1.1 2006/04/03 20:55:29 molod Exp $
%
%  Ed Hill
%
%  Generate the APE TR fields:
%

%======================================================================
%
%  Define the connections between the APE "ML" variables and the
%  MITgcm diagnostics output
%
nfacets = 6;
months.i_start = 7;
months.i_end   = 42;
bname = 'data/TRall.t%03d.nc';
gname = 'data/grid.t%03d.nc';
r_con_name = 'scrip_regrid/rmp_CS32_to_LL128x64_conserv.nc';
r_dwt_name = 'scrip_regrid/rmp_CS32_to_LL128x64_distwgt.nc';
oname = { 'TR_fields.nc' };

flags.regrid_type_for_vec =  2;
flags.debug_lev           =  0;
flags.punits_conv         =  0.01;

%  Get the following from:
%    ! (cd ../../ape_data_specs/ ; ./tr_parse.sh)
vars = {};
vars{1} = {'TR01','tr_tppn','PREACC','precipitation_flux','kg m-2 s-1','-','0.000011574074','ee'};
vars{2} = {'TR02','tr_lw_toa','OLR','toa_outgoing_longwave_flux','W m-2','-','1','cc'};
vars{3} = {'TR03','tr_om500','WVEL','omega','Pa s-1','-','1','ec'};
vars{4} = {'TR04','tr_u250','UVEL','eastward_wind','m s-1','u','1','ee'};
vars{5} = {'TR05','tr_v250','VVEL','northward_wind','m s-1','v','1','ee'};
vars{6} = {'TR06','tr_mslp','RSURF','air_pressure_at_sea_level','Pa','-','1','cc'};


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
    nTmin = min(nTmin, nT);
  end
  nc = [];
end
disp(sprintf('  total iterations found = %d',nTmin));


%======================================================================
%
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

aa_regrid.x = rdmds('aa_regrid/XC');
aa_regrid.y = rdmds('aa_regrid/YC');

disp('Reading -- Regridding -- Writing')

nc_outf = netcdf(oname{1}, 'clobber');
nc_outf.title = [ 'Aqua Planet: ' ...
             'TR Fields' ]; 
nc_outf.institution = 'MIT Dept. of EAPS, Cambridge, MA, USA';
nc_outf.source = [ 'MITgcm: ' ];
nc_outf.Conventions = 'CF-1.0';
nc_outf.history = [ 'Original data produced: ' '' ];

nc_outf('time') = 0;   %  record dim
nc_outf('lon') = og.nlon;
nc_outf('lat') = og.nlat;
nc_outf('bnd') = 2;

nc_outf{'time'} = ncdouble('time');
nc_outf{'time'}.standard_name = 'time';
nc_outf{'time'}.units = 'days since 0000-01-01 00:00';
nc_outf{'time'}.bounds = 'time_bnds';
%  nc_outf{'time'}(:) = og.lon;

nc_outf{'lon'} = ncdouble('lon');
nc_outf{'lon'}.standard_name = 'longitude';
nc_outf{'lon'}.units = 'degrees_east';
nc_outf{'lon'}.bounds = 'lon_bnds';
nc_outf{'lon'}(:) = og.lon;

nc_outf{'lat'} = ncdouble('lat');
nc_outf{'lat'}.standard_name = 'latitude';
nc_outf{'lat'}.units = 'degrees_north';
nc_outf{'lat'}.bounds = 'lat_bnds';
nc_outf{'lat'}(:) = og.lat;

nc_outf{'time_bnds'} = ncdouble('time','bnd');
nc_outf{'time_bnds'}.ape_name = 'time interval endpoints';
%  nc_outf{'time_bnds'}(:) = og.lonbnd;

nc_outf{'lon_bnds'} = ncdouble('lon','bnd');
nc_outf{'lon_bnds'}.ape_name = 'longitude cell bounds';
nc_outf{'lon_bnds'}.units = 'degrees_east';
nc_outf{'lon_bnds'}(:) = og.lonbnd;

nc_outf{'lat_bnds'} = ncdouble('lat','bnd');
nc_outf{'lat_bnds'}.ape_name = 'latitude cell bounds';
nc_outf{'lat_bnds'}.units = 'degrees_north';
nc_outf{'lat_bnds'}(:) = og.latbnd;


for iv = 1:length(vars)
  nc_outf{ vars{iv}{2} } = ncfloat( 'time', 'lat', 'lon' );
  nc_outf{ vars{iv}{2} }.ape_name = vars{iv}{4};
  nc_outf{ vars{iv}{2} }.units = vars{iv}{5};
  nc_outf{ vars{iv}{2} }.FillValue_ = 1.0e20;
end

ne = 32;

dat = [];
it = 1;
iv = 1;
ifacet = 1;
it0 = 2880;
for it = [ (it0+1):nTmin ]

  if mod(it-1,50) == 0
    disp(sprintf([ '  iteration = %6d  ' datestr(now) ],it));
  end
  
  for iv = 1:length(vars)
    for ifacet = 1:nfacets

      t0 = [];
      vtmp = [];

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
      t0 = nc{ vars{iv}{3} }(it,:,:);
      
      %  horizontally interpolate to u,v to mass points
      %
      %  this has changed so that now the "+1" points are garbage
      %  and must be ignored
      dat(iv,ifacet).a = zeros([1 32 32]);
      switch vars{iv}{6}
       case 'u'
        dat(iv,ifacet).a(1,:,:) = t0(:,1:ne);
       case 'v'
        dat(iv,ifacet).a(1,:,:) = t0(1:ne,:);
       otherwise
        dat(iv,ifacet).a(1,:,:) = t0;
      end
      
      dat(iv,ifacet).n = 1;
      
    end
  end
  
  %  interpolate u,v to mass points
  nc  = ne;
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
  
  %  Units conversion
  ave = [];
  for iv = 1:length(vars)
    ind = length(ave) + 1;
    for ifacet = 1:nfacets
      ave(ind).v(1,:,:,ifacet) = dat(iv,ifacet).a;
      ave(ind).ivar = iv;
    end
  end
  for ia = 1:length(ave)
    if strcmp(vars{ ave(ia).ivar }{7}, '-') ~= 1
      eval(['fac = ' vars{ ave(ia).ivar }{7} ';']);
      if fac ~= 1
        ave(ia).v = fac .* ave(ia).v;
      end
    end
  end

  %  regrid
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
          
          % if iz == 1
          %   disp(sprintf( '  %s was computed along with %s', ...
          %                 vars{ave(ii).ivar}{2}, vars{ave(ii-1).ivar}{2} ));
          % end
          
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

  
  %  write the regridded fields to netCDF
  tcurr =  0.25 * it;
  nc_outf{'time'}(it-it0) = tcurr;
  nc_outf{'time_bnds'}(it-it0,:) = [ tcurr tcurr+0.25 ];
  for ii = 1:length(ave)

    iv = ave(ii).ivar;
    nc_outf{ vars{iv}{2} }(it-it0,:,:) = ave(ii).r(1,:,:);
    
  end
  

end
clear tmp

%  close the input files
for ifacet = 1:nfacets
  nc = close( ncl{ifacet} );
end
clear ncl;

%  close output file
nc_outf = close(nc_outf);


