function [vit] = find_all_iters_per_var(fall,vnames,vit)

% Function [vit] = find_all_iters_per_var(fall)
%
% INPUTS
%   fall     cell array of file names
%   vanmes   cell array list of variable names
%   vit      variable iteration/time information
%              struct with fields:  'tdname', 'tvname', 'vars'
%
% OUTPUTS
%   vit      ...see above...
%
%
%  Ed Hill
if isempty(vit) || not(isfield(vit,'tdname')) || isempty(vit.tdname)
  vit.tdname = 'T';
end
if isempty(vit) || not(isfield(vit,'tvname')) || isempty(vit.tvname)
  vit.tvname = 'iter';
end

for ii = 1:length(fall)
  %  ii
  nc = netcdf(fall{ii},'read');
  
  if isempty(vnames)
    allv = ncnames(var(nc));
  else
    allv = vnames;
  end
  if isempty(intersect( ncnames(dim(nc)), vit.tdname ))
    % The "time" dimension does not exist in this file so no variable can
    % use it
    for jj = 1:length(allv)
      vit.vars.(allv{jj}) = [];
    end
  else
    if isempty(intersect( ncnames(var(nc)), vit.tvname ))
      % Here, the time dimension does exist but the coordinate varible
      % doesn't so theres no clear way to determine how the variable
      % should be stitched together based solely on this coordinate
      % variable.
      disp(['WARNING: the "time" dimension ''' vit.tdname ...
            ''' exists in file '''  fall{ii} ...
            ''' but the coordinate variable ''' vit.tvname ...
            ''' doesn''t so theres no way to determine ordering ' ...
            'across files!']);
      for jj = 1:length(allv)
        vit.vars.(allv{jj}) = [];
      end
    else
      %  allv
      for jj = 1:length(allv)
        %  allv{jj}
        %  nc{allv{jj}}
        if isempty(nc{allv{jj}})
          vit.vars.(allv{jj}) = [];
          continue;
        end
        if isempty(intersect( ncnames(dim(nc{allv{jj}})), vit.tdname ))
          vit.vars.(allv{jj}) = [];
        else
          if not(isfield(vit,'vars')) ...
                || not(isfield(vit.vars,allv{jj}))
            vit.vars.(allv{jj}) = nc{vit.tvname}(:);
          else
            vit.vars.(allv{jj}) = ...
                union( vit.vars.(allv{jj}), nc{vit.tvname}(:) );
          end
        end
      end
    end
  end
  
  nc = close(nc);
end
