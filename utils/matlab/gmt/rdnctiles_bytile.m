function [tlist] = rdnctiles_bytile(fall,vnames,tvals,dlev)

% Function [tlist] = rdnctiles_bytile(fall,vnames,tvals,dlev)
%
% INPUTS
%   fpat     cell array of file names
%   vnames   cell array of variable names
%   tvals    struct of iteration values or model times
%              tvals.iters 
%              tvals.times 
%   dlev     debug level
%
% OUTPUTS
%   tlist    struct array of tile data
%
%
%  This function has no input checking since it is meant to be
%  called by a wrapper function that ensures proper inputs.
%
%  Ed Hill
%  $Id: rdnctiles_bytile.m,v 1.2 2005/10/23 06:50:03 edhill Exp $


tlist = struct('gtn',{});
for fi = 1:length(fall)
  if dlev > 10
    disp(['  Opening : ' char(fall{fi}) ]);
  end
  nc = netcdf(fall{fi},'read');

  % Get the global tile number
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
  
  % Get all valid times based upon either the model iteration numbers or
  % the model time values.
  fit = [];
  git = [];
  fvars = ncnames(var(nc));
  if isfield(tvals,'iters') ...
        &&  not(isempty( intersect(fvars,{'iter'}) ))
    fiters = nc{'iter'}(:);
    [v,fit,git] = intersect(fiters,tvals.iters);
  end
  if isfield(tvals,'times') ...
        &&  not(isempty( intersect(fvars,{'T'}) ))
    ftimes = nc{'T'}(:);
    [v,fit,git]  = intersect(ftimes,tvals.times);
  end
  %  allit = union(inti,intt);

  % Get all the global attributes
  allatt = ncnames(att(nc));
  if not(isfield(tlist(it),'att'))
    tlist(it).att = {};
  end
  if ~isempty(allatt)
    for attr = allatt;
      % Don't get the attribute again if we already have it from reading a
      % previous tile
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
    fprintf(1,'    reading : ');
  end
  for iv = 1:length(vread)
    if dlev > 10
      fprintf(1,[' ' char(vread{iv}) ]);
    end
    if isempty(nc{char(vread{iv})})
      disp(['\n    warning: no var "',vread{iv},'" in "',fall{fi},'"']);
      continue
    end
    
    if isempty(fit) || isempty(git)
      % Read all of the variable at once including all time levels
      tmpv =  nc{vread{iv}}(:);
      sz = size(tmpv);
      nd = length(sz);
      tlist(it).var.(char(vread{iv})) = permute(tmpv,[nd:-1:1]);
    else
      % Get the rank of the time dimension (if used) and then only
      % read the desired time values
      tind = 0;
      dnames = ncnames(dim(nc{vread{iv}}));
      if strcmp( ncnames(recdim(nc)), 'T' )
        m = regexp(dnames, '^T$');
        for i = 1:length(m)
          if not(isempty(m{i}))
            tind = i;
            break
          end
        end
      end
      indstr = '';
      for i = 1:length(dnames)
        if i > 1
         indstr = [ indstr ',' ];
      end
      if i == tind
        indstr = [ indstr 'kt' ];
      else
        indstr = [ indstr ':' ];
      end
      end
      rindstr = fliplr(indstr);
      for jj = 1:length(fit)
        kt = fit(jj);
        eval([ 'tmpv =  nc{vread{iv}}(' indstr ');' ]);
        sz = size(tmpv);
        nd = length(sz);
        tk = git(jj);
        comm = [ 'tlist(it).var.(char(vread{iv}))(' ...
                 rindstr ') = permute(tmpv,[nd:-1:1]);' ];
        eval(comm);
      end
    end
  end
  if dlev > 10
    fprintf(1,'\n');
  end
  
  nc = close(nc);
end

