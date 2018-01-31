function [res,att] = rdnctiles(fpat,vnames,tlev, flag,dblev)

% Function [res,att] = rdnctiles(fpat,vnames,tlev, flag,dblev)
%
% INPUTS
%   fpat     either a string containing a file pattern
%              (eg. 'state.*.nc) or a cell array of file patterns
%   vnames   either a single variable name as a string or a
%              cell array of variable names
%   tlev     empty or a struct specifying:
%              tdname :  name of the "time" dimension
%              tvname :  name of the "time" coord variable
%              tvals  :  vector of "time" values 
%
%   flag     one of:  'oldflat' [def]
%                     'compact' 'bytile' 'byface'
%   dblev    debug level [def: 0]
%
% OUTPUTS
%   res      results
%   att      attribute values
%
% EXAMPLES
%   tlist = rdnctiles('state.*',[1000:100:2000]);
%   tlist = rdnctiles('state.*.nc','Temp','S',[1000:100:2000]);
%
%   tlist = rdnctiles('state.*.nc','Temp','S',[1000:100:2000]);
%
%   tl = rdnctiles({'state.*.nc' 'grid*'},[],[],[],20)
%   tl = rdnctiles({'state.*.nc' 'grid*'},[],[],'bytile',20)
%   tl = rdnctiles({'state.*.nc' 'grid*'},[],[24 72],[],20)
%   tl = rdnctiles({'state.*.nc' 'grid*'},[],[24 72],'bytile',20)
%
%
%  Ed Hill
%  Set defaults
dlev = 0;
if nargin < 1
  error('There must be at least one argument!');
end
if nargin < 2
  vnames = {};
end
if nargin < 3
  tlev = {};
end
if nargin < 4 || isempty(flag)
  flag = 'oldflat';
end
if nargin > 4
  dlev = dblev;
end

switch lower(flag)
 case 'oldflat'
  % error(['the ''oldflat'' format is not yet implemented.']);
 case 'compact'
  error(['the ''compact'' format is not yet implemented.']);
 case 'bytile'
 case 'byface'
  error(['the ''byface'' format is not yet implemented.']);
 otherwise
  error(['the flag ''' flag ''' is unknown']);
end

% Get all the files
fall = find_files_grid_first(fpat);
if isempty(fall)
  error(['No files matching could be found!']);
end
if dlev > 2
  disp(sprintf('Total files found:  %d',length(fall)));
  for i = 1:length(fall)
    disp(['  ' fall{i}]);
  end
end

%  Set defaults and do basic sanity checks for vnames
if isempty(vnames)
  vnames = {};
elseif ischar(vnames)
  tmp = vnames;
  vnames = { tmp };
elseif iscell(vnames)
else
  error(['"vnames" must be a cell array or a string!']);
end

%  Set defaults and do basic sanity checks for tlev
if isvector(tlev)
  tmp = tlev;
  clear tlev;
  tlev.tvals = tmp;
end
if isempty(tlev)
  vit = find_all_iters_per_var(fall,vnames,[]);
elseif isstruct(tlev)
  if isfield(tlev,'tdname')
    vit.tdname = tlev.tdname;
  end
  if isfield(tlev,'tvname')
    vit.tvname = tlev.tvname;
  end
  if isfield(tlev,'tvals')
    if isvector(tlev.tvals)
      vit = find_all_iters_per_var(fall,vnames,tlev);
      allvars = fieldnames(vit.vars);
      insufft = {};
      for i = 1:length(allvars)
        if isempty(vit.vars.(allvars{i}))
          continue
        end
        v = intersect(vit.vars.(allvars{i}),tlev.tvals);
        if length(v) < length(tlev.tvals)
          insufft{end+1} = allvars{i};
        end
        vit.vars.(allvars{i}) = tlev.tvals;
      end
      if not(isempty(insufft))
        disp(['WARNING: some of the specified times are not ' ...
              'available for the following variables :']);
        fprintf(1,['   ']);
        for i = 1:length(insufft)
          fprintf(1,[' ' insufft{i} ]);
        end
      end
    else
      error(['if tlev.tvals is defined, it *must* be a vector ' ...
             'of time values']);
    end
  else
      vit = find_all_iters_per_var(fall,vnames,[]);
  end
else
  error(['the "tlev" input is not in the right format']);
end

res = [];
att = [];
switch lower(flag)
 case 'oldflat'
  res = rdnctiles_oldflat(fall,vit,dlev);
 
 case 'compact'
  
 case 'bytile'
  res = rdnctiles_bytile(fall,vit,dlev);
 
 case 'byface'
  
end

