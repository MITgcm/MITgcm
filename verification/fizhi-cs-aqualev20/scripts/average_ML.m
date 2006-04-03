%
%  $Id: average_ML.m,v 1.1 2006/04/03 20:55:29 molod Exp $
%
%  Ed Hill
%
%  Generate the APE ML01--ML07 and ME01--ME07 fields:
%
%    For all facets:
%      For all time steps:
%        For all variables:
%          Create a mask based on THETA == 0.0
%          For all variables:
%            compute partial sum wrt mask
%            compute number contributing wrt mask
%      Compute the averages from the partial sums & num contrib
%      Write netCDF output for this facet:
%

%======================================================================

ML_files

vars = { ...
    {'u','UVEL','masku'} ...
    {'v','VVEL','maskv'} ...
    {'w','WVEL','mask'} ...
    {'phi','PHIHYD','mask'} ...
    {'q','SALT','mask'} ...
    {'rh','RELHUM','mask'} ...
    {'th','THETA','mask'} ...
       };

%======================================================================
%
%  Interpolate to the reference pressure levels and then compute the
%  derived fields and the temporal sums of all fields on the original
%  MITgcm grid
%
disp('Computing Sums')

itile = 1;
for itile = 1:6
  
  it0 = 0;
  for iv = 1:length(vars)
    eval(sprintf('clear %s', vars{iv}{1} ));
  end
  
  fi = 1;
  for fi = [ 1:length(files) ]

    fib = 0;
  
    fname = sprintf(files{fi}{1},itile);
    disp([ '  fname = "' fname '"' ]);
    nc = netcdf(fname, 'nowrite');
    times = nc{'T'}(:);

    it = 1;
    for it = 1:length(times)
      
      for iv = 1:length(vars)
        eval(sprintf('%s = squeeze(nc{ ''%s'' }(it,:,:,:));', ...
                     vars{iv}{1},vars{iv}{2}));
        eval(sprintf('mr = max(abs(%s(:)));', vars{iv}{1}));
        if mr > 1e10
          %  error('values heading out of range!!!');
          fib = 1;
        end
      end

      mask = double(th ~= 0.0);
      masku = ones(size(th) + [0 0 1]);
      masku(:,:,1:(end-1)) = masku(:,:,1:(end-1)) .* mask;
      masku(:,:,2:end    ) = masku(:,:,2:end    ) .* mask;
      maskv = ones(size(th) + [0 1 0]);
      maskv(:,1:(end-1),:) = maskv(:,1:(end-1),:) .* mask;
      maskv(:,2:end,    :) = maskv(:,2:end,    :) .* mask;
      
      if it0 == 0
        it0 = 1;
        for iv = 1:length(vars)
          eval(sprintf('%s_ave = zeros(size(%s));', ...
                       vars{iv}{1},vars{iv}{1}));
        end
        mask_sum = zeros(size(mask));
        masku_sum = zeros(size(masku));
        maskv_sum = zeros(size(maskv));
      end
      for iv = 1:length(vars)
        eval(sprintf('%s_ave = %s_ave + (%s .* %s);', ...
                     vars{iv}{1}, vars{iv}{1}, ...
                     vars{iv}{3}, vars{iv}{1}));
      end
      mask_sum = mask_sum + mask;
      masku_sum = masku_sum + masku;
      maskv_sum = maskv_sum + maskv;
      
    end
    
    if fib == 1
      disp([ 'BAD ::  "' fname '"' ]);
    end
    
    nc = close(nc);
    
  end

  for iv = 1:length(vars)
    eval(sprintf('%s_ave = %s_ave ./ %s_sum;', ...
                 vars{iv}{1}, vars{iv}{1}, vars{iv}{3} ));
  end
  
  nc = netcdf(sprintf('ML_ave_%d.nc',itile), 'clobber');
  
  nc('T') = 0;
  nc('Z') = 17;
  nc('X') = 32;   nc('Xp1') = 33;
  nc('Y') = 32;   nc('Yp1') = 33;
  
  for iv = 1:length(vars)
    switch vars{iv}{3}
     case 'mask'
      nc{ vars{iv}{2} } = ncdouble( 'T', 'Z', 'Y', 'X' );
     case 'masku'
      nc{ vars{iv}{2} } = ncdouble( 'T', 'Z', 'Y', 'Xp1' );
     case 'maskv'
      nc{ vars{iv}{2} } = ncdouble( 'T', 'Z', 'Yp1', 'X' );
    end
    eval(sprintf('nc{ vars{iv}{2} }(1,:,:,:) = %s_ave;',vars{iv}{1}));
  end
  nc{ 'mask_sum' } = ncdouble( 'T', 'Z', 'Y', 'X' );
  nc{ 'mask_sum' }(1,:,:,:) = mask_sum;
  nc{ 'masku_sum' } = ncdouble( 'T', 'Z', 'Y', 'Xp1' );
  nc{ 'masku_sum' }(1,:,:,:) = masku_sum;
  nc{ 'maskv_sum' } = ncdouble( 'T', 'Z', 'Yp1', 'X' );
  nc{ 'maskv_sum' }(1,:,:,:) = maskv_sum;
  nc = close(nc);

end

