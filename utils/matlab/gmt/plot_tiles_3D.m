function [rv] = plot_tiles_3D(tdat,vname,tvals, fignum,tilenums,tvname,dblev)

% Function [rv] = rdnctiles(res,vname, tilenums,dblev)
%
% INPUTS
%   tdat       tile data as a struct array where:
%                tdat.gtn is the global tile number
%                tdat.var is the per-tile variable information
%   vname      string with variable name
%
%   fignum     figure number (DEF: net)
%   tilenums   vector of global tile numbers to be plotted 
%                (empty = all)
%   tvname     name for the time variable [def='T']
%   dblev      debug level [def: 0]
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
%  $Id: plot_tiles_3D.m,v 1.1 2005/11/27 03:12:40 edhill Exp $


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
if nargin < 3
  error('There must be at least three arguments!');
end
if nargin < 4 || isempty(fignum)
  fignum = figure;
end
if nargin < 5
  tilenums = [];
end
if nargin < 5
  tvname = 'T';
end
if nargin > 5
  dlev = dblev;
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
if not(isempty(tilenums)) && not(isvector(tilenums))
  error(['"tilenums" must be empty or a vector of global' ...
         ' tile numbers']);
end
%  Get the valid tile numbers
if isempty(tilenums)
  tnall = [ tdat.gtn ];
else
  tnall = intersect(tilenums,[ tdat.gtn ]);
  if isempty(tnall)
    error(['none of the global tile numbers match the' ...
           ' specified "tilenums" values']);
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
  if isfield(tdat(itile).var,tvname)
    have_times = 1;
    alltimes = union(alltimes,tdat(1).var.(tvname));
  end
end

if isvector(tvals) && not(isempty(alltimes))
  timelist = intersection(tvals,alltimes);
else
  if isvector(tvals) && isempty(alltimes)
    error(['none of the specified "tvals" values ' ...
           'exist in "tdat"']);
  end
  if isempty(tvals) && not(isempty(alltimes))
    tvals = alltimes;
  end
end


%====================================================================
%  Plot the tiles one-at-a-time in 3D
alltimes
tnall
figure(fignum)
for it = 1:tnall

  fac = pi/180;
  corn = zeros([ size(tdat(it).var.XG) 3 ]);
  [ corn(:,:,1), corn(:,:,2), corn(:,:,3) ] = ...
      sph2cart( fac*tdat(it).var.XG, fac*tdat(it).var.YG, 1 );
  %  plot3( corn(:,:,1), corn(:,:,2), corn(:,:,3) ), axis equal
  if it > 1
    hold on
  end
  plot3()
  
end
hold off
