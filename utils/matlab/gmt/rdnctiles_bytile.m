function [tlist] = rdnctiles_bytile(fall,vit, dlev)

% Function [tlist] = rdnctiles_bytile(fall,vit, dlev)
%
% INPUTS
%   fall   cell array of file names
%   vit    struct containing variable and time information
%            vit.tdname : "time" dim name (DEF: 'T')
%            vit.tdname : "time" coord var name (DEF: 'iter')
%            vit.vars.(vname) : "time" values for each var
%
%   dlev   debug level
%
% OUTPUTS
%   tlist  struct array of tile data
%
%
%  This function has minimal input checking since it is meant to be
%  called by a wrapper function that ensures proper inputs.
%
%  Ed Hill
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
  itile = find([tlist(:).gtn] == gti);
  % tlist
  % [tlist(:).gtn]
  % pause
  if isempty(itile)
    itile = length(tlist) + 1;
    tlist(itile).gtn = gti;
  end
  
  %  Get all the variables
  if dlev > 10
    fprintf(1,'    reading : ');
  end
  vread = fields(vit.vars);
  for iv = 1:length(vread)
    if isempty(nc{char(vread{iv})})
      % disp(['    warning: no var "',vread{iv},'" in "',fall{fi},'"']);
      continue
    end
    if dlev > 10
      fprintf(1,[' ' char(vread{iv}) ]);
    end
    
    if isempty(vit.vars.(vread{iv}))
      % Read all of the variable at once since no time levels are used
      tmpv =  nc{vread{iv}}(:);
      sz = size(tmpv);
      nd = length(sz);
      tlist(itile).var.(char(vread{iv})) = permute(tmpv,[nd:-1:1]);
    else
      % Get the rank of the time dimension 
      trank = 0;
      dnames = ncnames(dim(nc{vread{iv}}));
      if strcmp( ncnames(recdim(nc)), vit.tdname )
        m = regexp(dnames, [ '^' vit.tdname '$'] );
        for i = 1:length(m)
          if not(isempty(m{i}))
            trank = i;
            break
          end
        end
      end
      if trank == 0
        error(['no time dim was found for variable ''' ...
               vread{iv} '''']);
      end
      
      % get the corresponding file-local indicies and global-assembly
      % indicies along the time dimension
      loc_times = nc{vit.tvname}(:);
      [v,ind1,ind2] = intersect( vit.vars.(vread{iv}), loc_times );

      % only read the desired time values based on:
      %   the local  "kt" indicies and
      %   the global "tk" indicies
      indstr = '';
      for i = 1:length(dnames)
        if i > 1
         indstr = [ indstr ',' ];
        end
        if i == trank
          indstr = [ indstr 'kt' ];
        else
          indstr = [ indstr ':' ];
        end
      end
      rindstr = fliplr(indstr);
      for jj = 1:length(ind1)
        kt = ind2(jj);
        eval([ 'tmpv =  nc{vread{iv}}(' indstr ');' ]);
        sz = size(tmpv);
        nd = length(sz);
        tk = ind1(jj);
        comm = [ 'tlist(itile).var.(char(vread{iv}))(' ...
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

