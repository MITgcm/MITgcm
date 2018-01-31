function [nt,nf] = mnc_assembly(fpat,vars, fout,fsize)

% Function [nt,nf] = mnc_assembly(fpat,vars, fout,fsize)
%
% INPUTS
% fpat   string containing the file pattern
% vars   structure array of variable names
%
% fout   output file pattern (DEF: "all.%05d.nc")
% fsize  max output file size (DEF: 2.0e+9 = +/-2GB)
%
% OUTPUTS
% nt     number of usable tiles found
% nf     number of output files written
%
% This function "assembles" MNC output.  It finds all the per-tile
% NetCDF files that match the input pattern, does some basic "sanity"
% tests to determine whether the files have compatible sizes, and
% then assembles all of the requested data (all of the variables)
% into one or more "global" NetCDF files.  The global files have
% the following dimension conventions:
%
%   "exch 1": all values are within a global horizontal grid
%             and indicies are (X,Y,Z,T)
%
%   "exch 2": all values are within one of up to six "faces" 
%             of a global cube with indicies (Xf,Yf,F,Z,T)
%
% where "X,Y.Z,T" are global space/time indicies, "Xf,Yf" are local
% per-face spatial indicies, and "F" is a face index.
%
% An example of how to use this script is:
% 
%   vars = struct([]);
%   vars(1).name = 'iter';
%   vars(2).name = 'U';
%   vars(3).name = 'Unk';
%   vars(4).name = 'V';
%   vars(5).name = 'Temp';
%   vars(6).name = 'S';
%   fpat = 'exp0_20041126_0001/state.0000.%06d.nc';
%   [nt,nf] = mnc_assembly(fpat,vars);
%
% and the resutlt is written as "all.00000.nc"

%=====  Argument checking and defaults  =====

if nargin < 2
  disp('Error: there must be at least 2 arguments!');
  return
end

if nargin < 3
  fout = 'all.%05d.nc';
end
if nargin < 4
  fsize = 2.0e+9;
end


%=====  Find and open all the matching files  =====

nt      = 0;
nf      = 0;
all_ncf = struct([]);

%  Find all of the files
exch2_msg = 0;
tmax = 200;
frdone = 0;
it = 0;
while frdone == 0

  it = it + 1;
  fnm = sprintf(fpat,it);
  % disp(fnm);

  %  Check that the file exists
  fid = fopen(fnm, 'r');
  if fid < 0
    if it >= tmax
      frdone = 1;
    end
    continue;
  end
  
  %  Open the NetCDF file
  fnc = netcdf(fnm, 'nowrite');
  if length(fnc) == 0
    continue;
  end

  %  Check for exch1/exch2 grid
  exch = 1;
  exch2_myFace = fnc.exch2_myFace(:);
  if length(exch2_myFace) ~= 0
    exch = 2;
    if exch2_msg == 0
      exch2_msg = 1;
      disp('  Grid type appears to be: "exch2"');
    end
  end
  
  n = length(all_ncf) + 1;
  all_ncf(n).name           = fnm;
  all_ncf(n).nc             = {fnc};
  all_ncf(n).exch           = exch;
  all_ncf(n).tile_number    = fnc.tile_number(1);
  all_ncf(n).bi             = fnc.bi(1);
  all_ncf(n).bj             = fnc.bj(1);
  all_ncf(n).sNx            = fnc.sNx(1);
  all_ncf(n).sNy            = fnc.sNy(1);
  all_ncf(n).Nx             = fnc.Nx(1);
  all_ncf(n).Ny             = fnc.Ny(1);
  all_ncf(n).Z              = fnc.Z(1);

  if exch == 2
    all_ncf(n).exch2_myFace = exch2_myFace;
    all_ncf(n).exch2_tbasex = fnc.exch2_tbasex(1);
    all_ncf(n).exch2_tbasey = fnc.exch2_tbasex(1);
  end
  
  clear fnc
end


%=====  Do some basic sanity checks =====

%  check for number of files/tiles found
if length(all_ncf) == 0
  disp('Error: no tiles found--no need to do any assembly!');
  return
elseif length(all_ncf) == 1
  disp('Error: one tile found--no need to do any assembly!');
  return
else
  disp(sprintf('  Found %d files matching the pattern:  "%s"', ...
               length(all_ncf), fpat ));
end

%  check for consistent "exch" version
if prod(double([all_ncf.exch] == all_ncf(1).exch)) ~= 1
  disp('Error: not all the "exch" types of the files match.');
  return;
end

%  check for consistent sNx,sNy
if (prod(double([all_ncf.sNx] == all_ncf(1).sNx)) ~= 1) ...
      | (prod(double([all_ncf.sNy] == all_ncf(1).sNy)) ~= 1)
  disp('Error: the "sNx,sNy" values for all the tiles are not');
  disp('   uniform.  Future versions of this function will be');
  disp('   able to handle non-uniform grid sizes but this');
  disp('   feature is not yet implemented.');
  return;
end

%  check for redundant tiles and "time series" output
if length(all_ncf) ~= length(unique([all_ncf.tile_number]))
  disp('Error: redundant tiles were found.  Please check that');
  disp('   the file pattern does not specify output spanning');
  disp('   multiple model runs or even multiple time series');
  disp('   within a single model run.  For multi-time-series');
  disp('   data sets, EACH "LEVEL" IN THE OUTPUT SERIES MUST');
  disp('   BE ASSEMBLED SEPARATERLY.');
  return
end


%=====  Get the dims/vars associations  =====

mydims = struct('names', {}, 'lens', {});
myvars = struct([]);
clear tncf;
for ivar = 1:length(vars)
  mydim_names = {};
  mydim_sizes = {};
  myatt.names = {};
  myatt.types = {};
  myatt.data  = {};
  myname = vars(ivar).name;
  disp(['  Looking for variable:   ' myname]);
  
  itile = 1;
  tncf = all_ncf(itile).nc{1};
  ncv = tncf{myname};
  len = length(ncv);
  if length(ncv) == 0
    warns = ['    Warning: variable "%s" is not defined in "%s"\n' ...
             '      so it will be ignored.'];
    disp(sprintf(warns,myname,all_ncf(itile).name));
    continue
  end
  mytype = datatype(ncv);
  tmpdims = dim(ncv);
  for inm = 1:length(tmpdims)
    mydim_names{inm} = name(tmpdims{inm});
    mydim_sizes{inm} = tmpdims{inm}(:);
  end
  for iat = 1:length(att(ncv))
    aaa = att(ncv);
    myatt.names(iat) = { name(aaa{iat}) };
    myatt.types(iat) = { datatype(aaa{iat}) };
    aab = aaa{iat};
    myatt.data(iat)  = { aab(:) };
  end
  
  %  confirm: vars have same dim names across all files
  ierr = 0;
  for itile = 2:length(all_ncf)
    tncf = all_ncf(itile).nc{1};
    ncv = tncf{myname};
    len = length(ncv);
    if length(ncv) == 0
      warns = ['    Warning: variable "%s" is not defined in "%s"\n' ...
               '      so it will be ignored.'];
      disp(sprintf(warns,myname,all_ncf(itile).name));
      continue
    end
    tmpdims = dim(ncv);
    for inm = 1:length(tmpdims)
      if mydim_names{inm} ~= name(tmpdims{inm})
        warns = ...
            ['    Warning: variable "%s" is not CONSISTENTLY defined.\n' ...
             '      It has different dimensions in different files so\n' ...
             '      so it will be ignored.'];
        disp(sprintf(warns,myname));
        ierr = 1;
        break
      end
      mydim_sizes{inm} = max([ tmpdims{inm}(:) mydim_sizes{inm} ]);
    end

  end
  
  if ierr == 0 
    %  check: does the variable have a "horizontal" component
    has_horiz   = 0;
    horiz_names = { 'X' 'Y' 'Xp1' 'Yp1' };
    for id = 1:length(mydim_names)
      if length([intersect(horiz_names,mydim_names{id})]) > 0
        has_horiz = 1;
      end
    end
    % disp([ '     ' myname ' ' sprintf('%d',has_horiz) ]);
    
    imy = length(myvars) + 1;
    myvars(imy).name       = myname;
    myvars(imy).type       = mytype;
    myvars(imy).dim_names  = mydim_names;
    myvars(imy).dim_sizes  = mydim_sizes;
    myvars(imy).atts       = myatt;
    myvars(imy).has_horiz  = has_horiz;
    
    % this is necessary to make it work with Matlab 6.5
    if isempty([mydims.names])
      addl = mydim_names;
    else
      addl = setdiff(mydim_names,[mydims.names]);
    end
    for iaddl = 1:length(addl)
      np1 = length(mydims) + 1;
      mydims(np1).names = addl(iaddl);
      mydims(np1).lens  = mydim_sizes(find(strcmp(addl(iaddl),mydim_names)));
    end
    
  end
end

%  For exch == 2, we need to add a "face" dimension
if all_ncf(1).exch == 2
  np1 = length(mydims) + 1;
  mydims(np1).names = { 'iface' };
  mydims(np1).lens  = { length(unique([all_ncf.exch2_myFace])) };
end

% myvars.name
% myvars.dim_names
% myvars.dim_sizes
% myvars(2).dim_names
% myvars(2).dim_names(4)

% mydims
% length(mydims)
% [ mydims.names ]
% [ mydims.lens ]


%=====  Assemble!  =====


if all_ncf(1).exch == 1

  %  exch "1":
  
% $$$   bi_max = max([all_ncf.bi]);
% $$$   bj_max = max([all_ncf.bj]);
% $$$   Xmax = bi_max * all_ncf(1).sNx;
% $$$   Ymax = bj_max * all_ncf(1).sNy;
  Xmax = all_ncf(1).Nx;
  Ymax = all_ncf(1).Ny;
  % at this point I have to make some assumptions about the domain
  % decomposition 
  bi_max = Xmax/all_ncf(1).sNx;
  bj_max = Ymax/all_ncf(1).sNy;
  itile = 0;
  for bj=1:bj_max
    for bi=1:bi_max
      itile = itile+1;
      all_ncf(itile).bi=bi;
      all_ncf(itile).bj=bj;
    end
  end

  horzdim = struct('names',{},'lens',{});
  horzdim(1).names = { 'X' };  horzdim(1).lens = { Xmax     };
  horzdim(2).names = {'Xp1'};  horzdim(2).lens = { Xmax + 1 };
  horzdim(3).names = { 'Y' };  horzdim(3).lens = { Ymax     };
  horzdim(4).names = {'Yp1'};  horzdim(4).lens = { Ymax + 1 };
  horzdim(5).names = { 'T' };  horzdim(5).lens = { 0        };
  
  iseq = 0;
  foutnm = sprintf(fout, iseq);
  fonc = netcdf(foutnm,'clobber');  % Should append-or-create!
  
  for idim = 1:length(mydims)
    dname = mydims(idim).names{1};
    ind = find(strcmp(dname,[horzdim.names]));
    if length(ind) ~= 0
      dlen = horzdim(ind).lens{1};
    else
      dlen = mydims(idim).lens{1};
    end
    comm = sprintf('fonc(''%s'') = %d;',dname,dlen);
    eval(comm);
  end
  
  for ivar = 1:length(myvars)
    comm = sprintf('fonc{''%s''} = nc%s( ',myvars(ivar).name,myvars(ivar).type);
    id = 1;
    comm = [ comm sprintf('''%s''',myvars(ivar).dim_names{id}) ];
    for id = 2:length(myvars(ivar).dim_names)
      comm = [ comm sprintf(',''%s''',myvars(ivar).dim_names{id}) ];
    end
    comm = [ comm ' );' ];
    eval(comm);
    for iat = 1:length(myvars(ivar).atts.names)
      comm = sprintf( ...
          'fonc{''%s''}.%s = nc%s( myvars(ivar).atts.data{iat} );', ...
          myvars(ivar).name, ...
          myvars(ivar).atts.names{iat}, ...
          myvars(ivar).atts.types{iat} );
      eval(comm);
    end
  end
  
  % for itime = 1:Tmax
  
  %  Here is where we need to check the output file size and start
  %  another file in the sequence, if necessary.
  
  for ivar = 1:length(myvars)
    disp(sprintf('  Copying variable:   %s',myvars(ivar).name))
    for itile = 1:length(all_ncf)

      if (myvars(ivar).has_horiz == 1) | (itile == 1)
        
        clear nct;
        nct = all_ncf(itile).nc{1};
        ox_off = (all_ncf(itile).bi - 1)*all_ncf(itile).sNx;
        oy_off = (all_ncf(itile).bj - 1)*all_ncf(itile).sNy;
        diml_in  = '';
        diml_out = '';
        for jj = 1:length(myvars(ivar).dim_names)
          doff = 1;
          if jj > 1
            diml_in  = sprintf('%s,',diml_in);
            diml_out = sprintf('%s,',diml_out);
          end
          dlen = myvars(ivar).dim_sizes{jj};
          diml_in  = sprintf('%s%s',diml_in, ':');
          fchar = myvars(ivar).dim_names{jj}(1);
          % disp(['       fchar = ' fchar '  ' myvars(ivar).dim_names{jj}]);
          if strcmp(myvars(ivar).dim_names{jj}(1),'X') == 1
            doff = ox_off + doff;
            dlen = ox_off + dlen;
          end
          if strcmp(myvars(ivar).dim_names{jj}(1),'Y') == 1
            doff = oy_off + doff;
            dlen = oy_off + dlen;
          end
          diml_out = sprintf('%s%d%s%d',diml_out,doff,':',dlen);
        end
        
        comm = sprintf( ...
            'fonc{''%s''}(%s) = nct{''%s''}(%s);', ...
            myvars(ivar).name, diml_out, myvars(ivar).name, diml_in );
        % disp([ '     comm:  ' comm ]);
        eval(comm);
        
      end
    
    end
  end
  % end
  
  fonc = close(fonc);
  
elseif all_ncf(1).exch == 2
  
  %  exch "2":
  Xmax = 0;
  Ymax = 0;
  for ii = 1:length(all_ncf)
    Xmax = max(Xmax, (all_ncf(ii).exch2_tbasex + all_ncf(ii).sNx));
    Ymax = max(Ymax, (all_ncf(ii).exch2_tbasey + all_ncf(ii).sNy));
  end
  
  horzdim = struct('names',{},'lens',{});
  horzdim(1).names = { 'X' };  horzdim(1).lens = { Xmax     };
  horzdim(2).names = {'Xp1'};  horzdim(2).lens = { Xmax + 1 };
  horzdim(3).names = { 'Y' };  horzdim(3).lens = { Ymax     };
  horzdim(4).names = {'Yp1'};  horzdim(4).lens = { Ymax + 1 };
  horzdim(5).names = { 'T' };  horzdim(5).lens = { 0        };

  iseq = 0;
  foutnm = sprintf(fout, iseq);
  fonc = netcdf(foutnm,'clobber');  % Should append-or-create!
  
  for idim = 1:length(mydims)
    dname = mydims(idim).names{1};
    ind = find(strcmp(dname,[horzdim.names]));
    if length(ind) ~= 0
      dlen = horzdim(ind).lens{1};
    else
      dlen = mydims(idim).lens{1};
    end
    comm = sprintf('fonc(''%s'') = %d;',dname,dlen);
    eval(comm);
  end

  for ivar = 1:length(myvars)
    comm = sprintf('fonc{''%s''} = nc%s( ',myvars(ivar).name,myvars(ivar).type);
    id = 1;
    comm = [ comm sprintf('''%s''',myvars(ivar).dim_names{id}) ];
    for id = 2:length(myvars(ivar).dim_names)
      dname = myvars(ivar).dim_names{id};
      if (dname(1) == 'Y') & (myvars(ivar).has_horiz == 1)
        comm = [ comm sprintf(',''%s''','iface') ];
      end
      comm = [ comm sprintf(',''%s''',dname) ];
    end
    comm = [ comm ' );' ];
    eval(comm);
    for iat = 1:length(myvars(ivar).atts.names)
      comm = sprintf( ...
          'fonc{''%s''}.%s = nc%s( myvars(ivar).atts.data{iat} );', ...
          myvars(ivar).name, ...
          myvars(ivar).atts.names{iat}, ...
          myvars(ivar).atts.types{iat} );
      eval(comm);
    end
  end

  %  Here is where we need to check the output file size and start
  %  another file in the sequence, if necessary.
  
  for ivar = 1:length(myvars)
    disp(sprintf('  Copying variable:   %s',myvars(ivar).name))
    for itile = 1:length(all_ncf)

      if (myvars(ivar).has_horiz == 1) | (itile == 1)
        
        clear nct;
        nct = all_ncf(itile).nc{1};
        ox_off = all_ncf(itile).exch2_tbasex;
        oy_off = all_ncf(itile).exch2_tbasey;
        diml_tin = '';
        diml_res = '';
        diml_in  = '';
        diml_out = '';
        if length(myvars(ivar).dim_names) < 2
          comm = sprintf( ...
              'fonc{''%s''}(%s%d) = nct{''%s''}(:);', ...
              myvars(ivar).name, '1:', myvars(ivar).dim_sizes{1}, ...
              myvars(ivar).name );
          % disp([ '    ' comm ]);
          eval(comm);
        else
          for jj = 1:length(myvars(ivar).dim_names)
            doff = 1;
            if jj > 1
              diml_tin = sprintf('%s,',diml_tin);
              diml_res = sprintf('%s,',diml_res);
              diml_in  = sprintf('%s,',diml_in);
              diml_out = sprintf('%s,',diml_out);
            end
            dnam = myvars(ivar).dim_names{jj};
            dlen = myvars(ivar).dim_sizes{jj};
            dlenr = dlen;
            fchar = myvars(ivar).dim_names{jj}(1);
            % disp(['       fchar = ' fchar '  ' myvars(ivar).dim_names{jj}]);
            if strcmp(dnam(1),'X') == 1
              doff = ox_off + doff;
              dlen = ox_off + dlen;
            end
            if strcmp(dnam(1),'Y') == 1
              diml_res = sprintf('%s%s',diml_res, '[],');
              diml_in  = sprintf('%s%s',diml_in, ':,');
              diml_out = sprintf('%s%d%s',diml_out,all_ncf(itile).exch2_myFace,',');
              doff = oy_off + doff;
              dlen = oy_off + dlen;
            end
            diml_tin = sprintf('%s%s',diml_tin, ':');
            diml_res = sprintf('%s%d',diml_res, dlenr);
            diml_in  = sprintf('%s%s',diml_in, ':');
            diml_out = sprintf('%s%d%s%d',diml_out,doff,':',dlen);
          end
          
          comm = sprintf( ...
              'tmp = reshape(nct{''%s''}(%s), %s); fonc{''%s''}(%s) = tmp(%s);', ...
              myvars(ivar).name, diml_tin, diml_res, myvars(ivar).name, ...
              diml_out, diml_in );
          % disp([ '    ' comm ]);
          eval(comm);
        end
      
      end
    
    end
  end
  % end

  fonc = close(fonc);

end

