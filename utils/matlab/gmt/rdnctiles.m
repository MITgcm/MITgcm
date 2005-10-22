function [tlist] = rdnctiles(fpat,vnames,times, dblev)

% Function [tlist] = rdnctiles(fpat,vnames,times, dblev)
%
% INPUTS
%   fpat     either a string containing a file pattern
%              (eg. 'state.*.nc) or a cell array of file patterns
%   vnames   either a single variable name as a string or a
%              cell array of variable names
%   times    vector of iteration values or struct of either 
%              iterations values or model times
%
%   dblev    debug level [def: 0]
%
% OUTPUTS
%   tlist    cell array of tile structs
%
% EXAMPLES
%   tlist = rdnctiles('state.*',[1000:100:2000]);
%   tlist = rdnctiles('state.*.nc','Temp','S',[1000:100:2000]);
%   t.iter = [1000:100:2000];
%   tlist = rdnctiles('state.*.nc','Temp','S',[1000:100:2000]);
%   
%  Ed Hill
%  $Id: rdnctiles.m,v 1.2 2005/10/22 02:07:36 edhill Exp $

dlev = 0;
if nargin < 1
  error('There must be at least one argument!');
end
if nargin < 2
  vnames = {};
end
if nargin < 3
  times = {};
end
if nargin > 3
  dlev = dblev;
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
if isstruct(times) 
  if isfield(times,'iter')
    %  times.iter = iter.iter;
  elseif isfield(times,'model')
    %  times.model = iter.model;
  else
    error(['If times is a struct, either "iter" or ' ...
           '"model" must be members']);
  end
  if not(isfield(times,'iter'))
    times.iter = [];
  end
    if not(isfield(times,'model'))
    times.model = [];
  end
elseif isvector(times)
  tmp = times;
  clear times;
  times.iter = tmp;
  times.model = [];
end

% tlist = struct('gtn',[],'att',{},'var',{});
tlist = struct('gtn',{});
for fi = 1:length(fall)
  if dlev > 10
    disp(['  Opening : ' char(fall{fi}) ]);
  end
  nc = netcdf(fall{fi},'read');

  %  Get the global tile number
  if isempty(nc.tile_number) || not(isscalar(nc.tile_number(:)))
    error(['No scalar "tile_number" global attribute was found' ...
           ' in file "' fall{fi} '"']);
  end
  gti = nc.tile_number(:);
  it = find([tlist(:).gtn] == gti);
  % tlist
  % [tlist(:).gtn]
  % pause
  if isempty(it)
    it = length(tlist) + 1;
    tlist(it).gtn = gti;
  end

  %  Get all the global attributes
  allatt = ncnames(att(nc));
  if not(isfield(tlist(it),'att'))
    tlist(it).att = {};
  end
  if ~isempty(allatt)
    for attr = allatt;
      %  Don't get the attribute again if we already have it from
      %  reading a previous tile
      if not(isfield(tlist(it).att,(char(attr))))
        tlist(it).att.(char(attr)) = nc.(char(attr))(:);
      end
    end
  end
  
  %  Get all the variables
  if isempty(vnames)
    vread = ncnames(var(nc));
  else
    vread = vnames;
  end
  if dlev > 10
    mess = sprintf('    Variables to read :');
    for i = 1:length(vread)
      mess = [ mess ' ' vread{i}];
    end
    disp(mess);
  end
  for iv = 1:length(vread)
    if dlev > 10
      disp(['    reading : ' char(vread{iv}) ]);
    end
    if isempty(nc{char(vread{i})})
      disp(['    warning: no var "',vread{iv},'" in "',fall{fi},'"']);
      continue
    end
    
    tmpv =  nc{vread{iv}}(:);
    sz = size(tmpv);
    nd = length(sz);
    tlist(it).var.(char(vread{iv})) = permute(tmpv,[nd:-1:1]);
  end
  
  nc = close(nc);
end

