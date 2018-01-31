function [res] = rdnctiles_oldflat(fall,vit,dlev)

% Function [res] = rdnctiles_oldflat(fall,vit,dlev)
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
%   res    struct containing results
%
%
%  Ed Hill
fi = 1;
nc = netcdf(fall{fi},'read');
allatts = ncnames(att(nc));
if not(isempty( intersect(allatts,'exch3_ver') ))
  res.att.exch_ver = 3;
elseif not(isempty( intersect(allatts,'exch2_tbasex') ))
  res.att.exch_ver = 2;
else
  res.att.exch_ver = 1;
end
res.att.Nx = nc.('Nx')(:);
res.att.Ny = nc.('Ny')(:);
res.att.Nr = nc.('Nr')(:);
res.att.Ne = min(res.att.Nx, res.att.Ny);
nc = close(nc);
for fi = 2:length(fall)
  nc = netcdf(fall{fi},'read');
  % Verify that all files belong to a domain that has the same overall
  % shape
  if ( (res.att.Nx ~= nc.('Nx')(:))  ...
       || (res.att.Ny ~= nc.('Ny')(:))  ...
       || (res.att.Nr ~= nc.('Nr')(:)) )
    error(['file ''' fall{fi} ''' does not belong to a domain ' ...
           'that has the same overall shape (Nx,Ny,Nr) as the ' ...
           'other files']);
  end
  nc = close(nc);
end

for fi = 1:length(fall)
  if dlev > 10
    disp(['  Opening : ' char(fall{fi}) ]);
  end
  nc = netcdf(fall{fi},'read');

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
    
    % get the rank of the X,Y dims
    xrank = 0;
    yrank = 0;
    for i = 1:length(dnames)
      if strcmp(dnames{i},'X') || strcmp(dnames{i},'Xp1')
        xrank = i;
      end
      if strcmp(dnames{i},'Y') || strcmp(dnames{i},'Yp1')
        yrank = i;
      end
    end

    % get the corresponding file-local indicies and global-assembly
    % indicies along the time dimension
    loc_times = nc{vit.tvname}(:);
    [v,ind1,ind2] = intersect( vit.vars.(vread{iv}), loc_times );
    
    % only read the desired time values based on:
    %   the local  "kt" indicies and
    %   the global "tk" indicies
    vdims = dim(nc{vread{iv}});
    sz_glob = [];
    indstr = '';
    for i = 1:length(dnames)
      if i > 1
        indstr = [ indstr ',' ];
      end
      if i == trank
        indstr = [ indstr 'kt' ];
        sz_glob = [ sz_glob length(vit.vars.(vread{iv})) ];
      elseif i == xrank
        indstr = [ indstr 'kx' ];
        sz_glob = [ sz_glob res.att.Nx ];
      elseif i == yrank
        indstr = [ indstr 'ky' ];
        sz_glob = [ sz_glob res.att.Ny ];
      else
        indstr = [ indstr ':' ];
        sz_glob = [ sz_glob vdims{i}(:) ];
      end
    end
    rindstr = fliplr(indstr);
    sz_glob = fliplr(sz_glob);

    % If the global variable doesn't exist, then declare it and reserve
    % the necessary space
    if not(isfield(res,'var')) || not(isfield(res.var,vread{iv}))
      if length(sz_glob) == 1
        res.var.(vread{iv}) = zeros(sz_glob,1);
      else
        res.var.(vread{iv}) = zeros(sz_glob);
      end
    end
    
    % file-local X,Y indicies
    kx = [ 1:nc.('sNx')(:) ];
    ky = [ 1:nc.('sNy')(:) ];
    % global X,Y indicies
    if res.att.exch_ver == 2
      xk = nc.('exch2_txglobalo')(:) - 1 + kx;
      yk = nc.('exch2_tyglobalo')(:) - 1 + ky;
    elseif res.att.exch_ver == 1
      xk = (nc.('bi')(:) - 1)*nc.('sNx')(:) + kx;
      yk = (nc.('bj')(:) - 1)*nc.('sNy')(:) + ky;
    end
    if length(ind1) > 0
      for jj = 1:length(ind1)
        kt = ind2(jj);
        eval([ 'tmpv =  nc{vread{iv}}(' indstr ');' ]);
        sz = size(tmpv);
        nd = length(sz);
        tk = ind1(jj);
        comm = [ 'res.var.(char(vread{iv}))(' ...
                 rindstr ') = permute(tmpv,[nd:-1:1]);' ];
        eval(comm);
      end
    else
      eval([ 'tmpv =  nc{vread{iv}}(' indstr ');' ]);
      sz = size(tmpv);
      nd = length(sz);
      comm = [ 'res.var.(char(vread{iv}))(' ...
               rindstr ') = permute(tmpv,[nd:-1:1]);' ];
      eval(comm);
    end
  end
  if dlev > 10
    fprintf(1,'\n');
  end

  nc = close(nc);
end

