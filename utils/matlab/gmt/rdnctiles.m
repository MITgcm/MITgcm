function [res,att] = rdnctiles(fpat,vnames,tlev, flag,dblev)

% Function [res,att] = rdnctiles(fpat,vnames,tlev, flag,dblev)
%
% INPUTS
%   fpat     either a string containing a file pattern
%              (eg. 'state.*.nc) or a cell array of file patterns
%   vnames   either a single variable name as a string or a
%              cell array of variable names
%   tlev    vector of iteration values or struct of either 
%              iterations values or model times
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
%   t.iter = [1000:100:2000];
%   tlist = rdnctiles('state.*.nc','Temp','S',[1000:100:2000]);
%   tl = rdnctiles({'state.*.nc' 'grid*'},[],[],'bytile',20)
%
%
%  Ed Hill
%  $Id: rdnctiles.m,v 1.4 2005/10/23 06:50:03 edhill Exp $


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
  flag = 'oldflat'
end
if nargin > 4
  dlev = dblev;
end

switch lower(flag)
 case 'oldflat'
  error(['the ''oldflat'' format is not yet implemented.']);
 case 'compact'
  error(['the ''compact'' format is not yet implemented.']);
 case 'bytile'
 case 'byface'
  error(['the ''byface'' format is not yet implemented.']);
 otherwise
  error(['the flag ''' flag ''' is unknown']);
end

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

%  Get variable names
if isempty(vnames)
  clear vnames;
  vnames = {};
elseif ischar(vnames)
  tmp = vnames;
  vnames = {};
  vnames = { tmp };
else
  error(['"vnames" must be a cell array or a string!']);
end

%  Get iterations or model times
if isempty(tlev)
  tlev = find_all_iters(fall);
elseif isstruct(tlev) 
  if isfield(tlev,'iter')
    %  tlev.iter = iter.iter;
  elseif isfield(tlev,'time')
    %  tlev.time = iter.time;
  else
    error(['If tlev is a struct, either "iter" or ' ...
           '"time" must be members']);
  end
  if not(isfield(tlev,'iter'))
    tlev.iter = [];
  end
    if not(isfield(tlev,'time'))
    tlev.time = [];
  end
elseif isvector(tlev)
  tmp = tlev;
  clear tlev;
  tlev.iter = tmp;
  tlev.time = [];
end

res = [];
switch lower(flag)
 case 'oldflat'
  res = rdnctiles_oldflat(fall,vnames,tlev,dlev);
 
 case 'compact'
  
 case 'bytile'
  res = rdnctiles_bytile(fall,vnames,tlev,dlev);
 
 case 'byface'
  
end

