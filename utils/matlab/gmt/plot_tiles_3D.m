function [rv] = plot_tiles_3D(tdat,vname,tvals,iz, opts)

% Function [rv] = rdnctiles(res,vname,tvals,iz, opts)
%
% INPUTS
%   tdat       tile data as a struct array where:
%                tdat.gtn is the global tile number
%                tdat.var is the per-tile variable information
%   vname      string with variable name
%   tvals      vector of time ('T') values or [] for all
%   iz         depth index
%
%   opts       struct with the following optional fields
%      { 'fignum'      'figure' }  ...
%      { 'tilenums'    '[]'     }  ...
%      { 'tvname'      '''T'''  }  ...
%      { 'dblev'       '0'      }  ...
%      { 'delay'       '0'      }  ...
%      { 'radius'      '1'      }  ...
%      { 'caxis'       '[]'     }  ...
%      { 'colorbar'    '1'      }
%
% OUTPUTS
%   rv         return value (= number of tiles plotted)
%
% EXAMPLES
%
%   tl = rdnctiles({'state.*.nc' 'grid*'},[],[],[],20)
%   plot_tiles_3D(tl,'Temp',[])
%
%
%  Ed Hill
%  For debugging:
if 1 == 2
  clear all ; close all
  [tdat,att] = rdnctiles({'state.*.nc' 'grid*'},[],[],'bytile');
  vname = 'Temp';
  tvals = [];
  plot_tiles_3D(tdat,vname,tvals)
end

%====================================================================
%  Set defaults
dlev = 0;
if nargin < 4
  error('There must be at least four arguments!');
end
if nargin < 5 || isempty(opts)
  opts = struct();
end

if not(isstruct(opts))
  error('The "opts" argument must be a struct!');
end
opts_content = { ...
    { 'fignum'      'figure' }  ...
    { 'tilenums'    '[]'     }  ...
    { 'tvname'      '''T'''  }  ...
    { 'dblev'       '0'      }  ...
    { 'delay'       '0'      }  ...
    { 'radius'      '1'      }  ...
    { 'caxis'       '[]'     }  ...
    { 'colorbar'    '1'      }  };
for i = 1:size(opts_content,2)
  if not(isfield(opts,opts_content{i}{1}))
    eval(sprintf( 'opts.%s = %s;',...
                  opts_content{i}{1}, opts_content{i}{2} ));
  end
end

%====================================================================
%  Check inputs
if isempty(vname)  ||  not(ischar(vname)) 
  error('"vname" must be a character string')
end
if not(isstruct(tdat))  ...
      || not(isfield(tdat,'gtn'))  ||  not(isfield(tdat,'var'))
  error(['"tdat" must be a struct array with fields ' ...
         '"gtn" and "var"'])
end
if not(isempty(opts.tilenums)) && not(isvector(opts.tilenums))
  error(['"opts.tilenums" must be empty or a vector of global' ...
         ' tile numbers']);
end
%  Get the valid tile numbers
if isempty(opts.tilenums)
  tnall = [ tdat.gtn ];
else
  tnall = intersect(opts.tilenums,[ tdat.gtn ]);
  if isempty(tnall)
    error(['none of the global tile numbers match the' ...
           ' specified "opts.tilenums" values']);
  end
end
%  Get the valid time values
have_times = 0;
if not(isempty(tvals)) && not(isvector(tvals))
  error(['"tvals" must be empty or a vector of time' ...
         ' values']);
end
alltimes = [];
for itile = 1:tnall
  if isfield(tdat(itile).var,opts.tvname)
    have_times = 1;
    alltimes = union(alltimes,tdat(1).var.(opts.tvname));
  end
end
tvals
alltimes
if isvector(tvals) && not(isempty(alltimes))
  timelist = intersect(tvals,alltimes);
else
  if isvector(tvals) && isempty(alltimes)
    error(['none of the specified "tvals" values ' ...
           'exist in "tdat"']);
  end
  if isempty(tvals) && not(isempty(alltimes))
    timelist = alltimes;
  end
end


%====================================================================
%  Plot the tiles one-at-a-time in 3D

itime = 1;
figure(opts.fignum)
for tval = 1%[ timelist ]
  for itile = [ tnall ]
    
    %itime = find(tval == tdat(itile).var.(opts.tvname));
    itime = 1;

    fac = pi/180;
    corn = zeros([ size(tdat(itile).var.XG) 3 ]);
    [ corn(:,:,1), corn(:,:,2), corn(:,:,3) ] = ...
        sph2cart( fac*tdat(itile).var.YG, fac*tdat(itile).var.XG, 1 );
    
    %  plot3( corn(:,:,1), corn(:,:,2), corn(:,:,3), '-o' ), axis equal
    %  plot( tdat(itile).var.XG, tdat(itile).var.YG, '-o' )
    
    cen = zeros([ size(tdat(itile).var.XC) 3 ]);
    [ cen(:,:,1), cen(:,:,2), cen(:,:,3) ] = ...
        sph2cart( fac*tdat(itile).var.XC, fac*tdat(itile).var.YC, 1 );
    
    surf( cen(:,:,1), cen(:,:,2), cen(:,:,3), ...
          squeeze(tdat(itile).var.YC(:,:,1,itime)) )
    if itile == 1
      hold on
      axis equal
      r = opts.radius;
      axis([ -r r -r r -r r ]);
      if not(isempty(opts.caxis))
        caxis(opts.caxis);
      end
      if opts.colorbar ~= 0
        colorbar;
      end
    end
    
  end
  % disp(sprintf('  plotting tile %d',itile));
  %pause(opts.delay);

end
hold off

