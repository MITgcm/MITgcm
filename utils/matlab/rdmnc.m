function [S] = rdmnc(varargin)

% Usage:
%   S=RDMNC(FILE1,FILE2,...)
%   S=RDMNC(FILE1,...,ITER)
%   S=RDMNC(FILE1,...,'VAR1','VAR2',...)
%   S=RDMNC(FILE1,...,'VAR1','VAR2',...,ITER)
%
% Input:
%   FILE1   Either a single file name (e.g. 'state.nc') or a wild-card
%           strings expanding to a group of file names (e.g. 'state.*.nc').
%           There are no assumptions about suffices (e.g. 'state.*' works).
%   VAR1    Model variable names as written in the MNC/netcdf file.
%   ITER    Vector of iterations in the MNC/netcdf files, not model time.
%
% Output:
%   S       Structure with fields corresponding to 'VAR1', 'VAR2', ...
%
% Description:
%   This function is a rudimentary wrapper for joining and reading netcdf
%   files produced by MITgcm.  It does not give the same flexibility as
%   opening the netcdf files directly using netcdf(), but is useful for
%   quick loading of entire model fields which are distributed in multiple
%   netcdf files.
%
% Example:
%   >> S=rdmnd('state.*','XC','YC','T');
%   >> imagesc( S.XC, S.YC, S.T(:,:,1)' );
%
%  Author:  Alistair Adcroft
%  Modifications:  Daniel Enderton

% Initializations
dBug=0;
file={};
filepaths={};
files={};
varlist={};
iters=[];

% find out if matlab's generic netcdf API is available
% if not assume that Chuck Denham's netcdf toolbox is installed
usemexcdf = isempty(which('netcdf.open'));

% Process function arguments
for iarg=1:nargin;
    arg=varargin{iarg};
    if ischar(arg)
        if isempty(dir(char(arg)))
            varlist{end+1}=arg;
        else
            file{end+1}=arg;
        end
    else
        if isempty(iters)
            iters=arg;
        else
            error(['The only allowed numeric argument is iterations',...
                   ' to read in as a vector for the last argument']);
        end
    end
end
if isempty(file)
    if isempty(varlist),
       fprintf( 'No file name in argument list\n');
    else
       fprintf(['No file in argument list:\n ==> ',char(varlist(1))]);
       for i=2:size(varlist,2), fprintf([' , ',char(varlist(i))]); end
       fprintf(' <==\n');
    end
    error(' check argument list !!!');
end

% Create list of filenames
for eachfile=file
	filepathtemp=eachfile{:};
	indecies = find(filepathtemp==filesep);
	if ~isempty(indecies)
        filepathtemp = filepathtemp(1:indecies(end));
	else
        filepathtemp = '';
	end
    expandedEachFile=dir(char(eachfile{1}));
    for i=1:size(expandedEachFile,1);
        if expandedEachFile(i).isdir==0
            files{end+1}=expandedEachFile(i).name;
            filepaths{end+1}=filepathtemp;
        end
    end
end


% If iterations unspecified, open all the files and make list of all the
% iterations that appear, use this iterations list for data extraction.
if isempty(iters)
    iters = [];
    for ieachfile=1:length(files)
        eachfile = [filepaths{ieachfile},files{ieachfile}];
        if usemexcdf
          nc=netcdf(char(eachfile),'read');
          nciters = nc{'iter'}(:);
          if isempty(nciters), nciters = nc{'T'}(:); end
          close(nc);
        else
	  % the parser complains about "netcdf.open" when the matlab netcdf
          % API is not available, even when it is not used so we have to
          % avoid the use of "netcdf.open", etc in this function
          nciters = ncgetvar(char(eachfile),'iter');
          if isempty(nciters), nciters = ncgetvar(char(eachfile),'T'); end
        end
        iters = [iters,nciters'];
    end
    iters = unique(iters');
end

% Cycle through files for data extraction.
S.attributes=[];
for ieachfile=1:length(files)
    eachfile = [filepaths{ieachfile},files{ieachfile}];
    if dBug > 0, fprintf([' open: ',eachfile]); end
    if usemexcdf
      nc=netcdf(char(eachfile),'read');
      S=rdmnc_local(nc,varlist,iters,S,dBug);
      close(nc);
    else
      % the parser complains about "netcdf.open" when the matlab netcdf
      % API is not available, even when it is not used so we have to
      % avoid the use of "netcdf.open", etc in this function
      S=rdmnc_local_matlabAPI(char(eachfile),varlist,iters,S,dBug);
    end
end

return

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                             Local functions                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [A] = read_att(nc);
    allatt=ncnames(att(nc));
    if ~isempty(allatt)
        for attr=allatt;
            A.(char(attr))=nc.(char(attr))(:);
        end
    else
        A = 'none';
    end

function [i0,j0,fn] = findTileOffset(S);
    fn=0;
    if isfield(S,'attributes') & isfield(S.attributes,'global')
        G=S.attributes.global;
        tn=G.tile_number;
        snx=G.sNx; sny=G.sNy; nsx=G.nSx; nsy=G.nSy; npx=G.nPx; npy=G.nPy;
        ntx=nsx*npx;nty=nsy*npy;
        gbi=mod(tn-1,ntx); gbj=(tn-gbi-1)/ntx;
        i0=snx*gbi; j0=sny*gbj;
        if isfield(S.attributes.global,'exch2_myFace')
            fn=G.exch2_myFace;
            i0=G.exch2_txGlobalo -1; j0=G.exch2_tyGlobalo -1;
        end
    else
        i0=0;j0=0;
    end
    %[snx,sny,nsx,nsy,npx,npy,ntx,nty,i0,j0,fn];

function [S] = rdmnc_local(nc,varlist,iters,S,dBug)

  fiter = nc{'iter'}(:);                               % File iterations present
  if isempty(fiter), fiter = nc{'T'}(:); end
  if isinf(iters); iters = fiter(end); end
  if isnan(iters); iters = fiter; end
  [fii,dii] = ismember(fiter,iters);  fii = find(fii); % File iteration index
  dii = dii(find(dii ~= 0));                           % Data interation index
  if dBug > 0,
    fprintf(' ; fii='); fprintf(' %i',fii);
    fprintf(' ; dii='); fprintf(' %i',dii); fprintf(' \n');
  end

  % No variables specified? Default to all
  if isempty(varlist), varlist=ncnames(var(nc)); end

  % Attributes for structure
  if iters>0; S.iters_from_file=iters; end
  S.attributes.global=read_att(nc);
  [pstr,netcdf_fname,ext] = fileparts(name(nc));
  if strcmp(netcdf_fname(end-3:end),'glob')
    % assume it is a global file produced by gluemnc and change some
    % attributes
    S.attributes.global.sNx = S.attributes.global.Nx;
    S.attributes.global.sNy = S.attributes.global.Ny;
    S.attributes.global.nPx = 1;
    S.attributes.global.nSx = 1;
    S.attributes.global.nPy = 1;
    S.attributes.global.nSy = 1;
    S.attributes.global.tile_number = 1;
    S.attributes.global.nco_openmp_thread_number = 1;
  end

  % Read variable data
  for ivar=1:size(varlist,2)

    cvar=char(varlist{ivar});
    if isempty(nc{cvar})
      disp(['No such variable ''',cvar,''' in MNC file ',name(nc)]);
      continue
    end
    % code by Bruno Deremble: if you do not want to read all tiles these
    % modifications make the output field smaller, let us see, if it is
    % robust
    if (isfield(S,cvar) == 0); firstiter = 1; else firstiter = 0; end
    % end code by Bruno Deremble

    dims = ncnames(dim(nc{cvar}));        % Dimensions
    sizVar = size(nc{cvar}); nDims=length(sizVar);
    if dims{1} == 'T'
      if isempty(find(fii)), error('Iters not found'); end
      it = length(dims);
      tmpdata = nc{cvar}(fii,:);
      % leading unity dimensions get lost; add them back:
      tmpdata=reshape(tmpdata,[length(fii) sizVar(2:end)]);
    else
      it = 0;
      tmpdata = nc{cvar}(:);
      % leading unity dimensions get lost; add them back:
      tmpdata=reshape(tmpdata,sizVar);
    end

    if dBug > 1,
      fprintf(['  var:',cvar,': nDims=%i ('],nDims);fprintf(' %i',size(nc{cvar}));
      fprintf('):%i,nD=%i,it=%i ;',length(size(tmpdata)),length(dims),it);
    end
    if length(dims) > 1,
      tmpdata=permute(tmpdata,[nDims:-1:1]);
    end
    if dBug > 1,
%         fprintf('(tmpdata:');fprintf(' %i',size(tmpdata)); fprintf(')');
    end
    [ni nj nk nm nn no np]=size(tmpdata);

    [i0,j0,fn]=findTileOffset(S);
    cdim=dims{end}; if cdim(1)~='X'; i0=0; end
    cdim=dims{end}; if cdim(1)=='Y'; i0=j0; j0=0; end
    if length(dims)>1;
      cdim=dims{end-1}; if cdim(1)~='Y'; j0=0; end
    else
      j0=0;
    end
    if dBug > 1,
      fprintf(' i0,ni= %i %i; j,nj= %i %i; nk=%i :',i0,ni,j0,nj,nk);
    end
    % code by Bruno Deremble: if you do not want to read all tiles these
    % modifications make the output field smaller, let us see, if it is
    % robust
    if (firstiter)
      S.attributes.i_first.(cvar) = i0;
      S.attributes.j_first.(cvar) = j0;
    end
    i0 = i0 - S.attributes.i_first.(cvar);
    j0 = j0 - S.attributes.j_first.(cvar);
    % end code by Bruno Deremble

    Sstr = '';
    for istr = 1:max(nDims,length(dims)),
      if     istr == it,  Sstr = [Sstr,'dii,'];
      elseif istr == 1,   Sstr = [Sstr,'i0+(1:ni),'];
      elseif istr == 2,   Sstr = [Sstr,'j0+(1:nj),'];
      elseif istr == 3,   Sstr = [Sstr,'(1:nk),'];
      elseif istr == 4,   Sstr = [Sstr,'(1:nm),'];
      elseif istr == 5,   Sstr = [Sstr,'(1:nn),'];
      elseif istr == 6,   Sstr = [Sstr,'(1:no),'];
      elseif istr == 7,   Sstr = [Sstr,'(1:np),'];
      else, error('Can''t handle this many dimensions!');
      end
    end
    eval(['S.(cvar)(',Sstr(1:end-1),')=tmpdata;'])
    %S.(cvar)(i0+(1:ni),j0+(1:nj),(1:nk),(1:nm),(1:nn),(1:no),(1:np))=tmpdata;
    if dBug > 1, fprintf(' %i',size(S.(cvar))); fprintf('\n'); end

    S.attributes.(cvar)=read_att(nc{cvar});
    % replace missing or FillValues with NaN
    if isfield(S.attributes.(cvar),'missing_value');
      misval = S.attributes.(cvar).missing_value;
      S.(cvar)(S.(cvar) == misval) = NaN;
    end
    if isfield(S.attributes.(cvar),'FillValue_');
      misval = S.attributes.(cvar).FillValue_;
      S.(cvar)(S.(cvar) == misval) = NaN;
    end

  end % for ivar

  if isempty(S)
    error('Something didn''t work!!!');
  end

  return

function [S] = rdmnc_local_matlabAPI(fname,varlist,iters,S,dBug)

  fiter = ncgetvar(fname,'iter');                     % File iterations present
  if isempty(fiter), fiter = ncgetvar(fname,'T'); end
  if isinf(iters); iters = fiter(end); end
  if isnan(iters); iters = fiter; end
  [fii,dii] = ismember(fiter,iters);  fii = find(fii); % File iteration index
  dii = dii(find(dii ~= 0));                           % Data interation index
  if dBug > 0,
    fprintf(' ; fii='); fprintf(' %i',fii);
    fprintf(' ; dii='); fprintf(' %i',dii); fprintf(' \n');
  end

  % now open the file for reading
  nc = netcdf.open(fname,'NC_NOWRITE');
  % get basic information about netcdf file
  [ndims nvars natts recdim] = netcdf.inq(nc);

  % No variables specified? Default to all
  if isempty(varlist),
    for k=0:nvars-1
      varlist{k+1} = netcdf.inqVar(nc,k);
    end
  end

  % Attributes for structure
  if iters>0; S.iters_from_file=iters; end
  S.attributes.global=ncgetatt(nc,'global');
  [pstr,netcdf_fname,ext] = fileparts(fname);
  if strcmp(netcdf_fname(end-3:end),'glob')
    % assume it is a global file produced by gluemnc and change some
    % attributes
    S.attributes.global.sNx = S.attributes.global.Nx;
    S.attributes.global.sNy = S.attributes.global.Ny;
    S.attributes.global.nPx = 1;
    S.attributes.global.nSx = 1;
    S.attributes.global.nPy = 1;
    S.attributes.global.nSy = 1;
    S.attributes.global.tile_number = 1;
    S.attributes.global.nco_openmp_thread_number = 1;
  end

  % Read variable data
  for ivar=1:size(varlist,2)

    cvar=char(varlist{ivar});
    varid=ncfindvarid(nc,cvar);
    if isempty(varid)
      disp(['No such variable ''',cvar,''' in MNC file ',fname]);
      continue
    end
    % code by Bruno Deremble: if you do not want to read all tiles these
    % modifications make the output field smaller, let us see, if it is
    % robust
    if (isfield(S,cvar) == 0); firstiter = 1; else firstiter = 0; end
    % end code by Bruno Deremble

    [varname,xtype,dimids,natts] = netcdf.inqVar(nc,varid);
    % does this variable contain a record (unlimited) dimension?
    [isrecvar,recpos] = ismember(recdim,dimids);

    % Dimensions
    clear sizVar dims
    for k=1:length(dimids)
      [dims{k}, sizVar(k)] = netcdf.inqDim(nc,dimids(k));
    end
    nDims=length(sizVar);
    if isrecvar
      if isempty(find(fii)), error('Iters not found'); end
      it = length(dims);
      if length(dimids) == 1
        % just a time or iteration variable, this will result in a vector
        % and requires special treatment
        icount=1;
        tmpdata = zeros(length(fii),1);
        for k=1:length(fii)
          istart = fii(k)-1;
          tmpdata(k) = netcdf.getVar(nc,varid,istart,icount,'double');
        end
      else
        % from now on we assume that the record dimension is always the
        % last dimension. This may not always be the case
        if recpos ~= nDims
          error(sprintf('%s\n%s\n%s%s%i%s%i', ...
                        ['The current code assumes that the record ' ...
                         'dimension is the last dimension,'], ...
                        'this is not the case for variable', cvar, ...
                        ': nDims = ', nDims,  ...
                        ', position of recDim = ', recpos))
        end
        istart = zeros(1,it); % indexing starts a 0
        icount = sizVar;
        % we always want to get only on time slice at a time
        icount(recpos) = 1;
        % make your life simpler by putting the time dimension first
        tmpdata = zeros([length(fii) sizVar(1:end-1)]);
        for k=1:length(fii)
          istart(recpos) = fii(k)-1; % indexing starts at 0
          tmp = netcdf.getVar(nc,varid,istart,icount,'double');
          tmpdata(k,:) = tmp(:);
        end
        % move time dimension to the end ...
        tmpdata = shiftdim(tmpdata,1);
        % ... and restore original shape
        tmpdata = reshape(tmpdata,[sizVar(1:end-1) length(fii)]);
      end
    else
      it = 0;
      tmpdata = netcdf.getVar(nc,varid,'double');
    end
    %
    if dBug > 1,
      fprintf(['  var:',cvar,': nDims=%i ('],nDims);fprintf(' %i',sizVar);
      fprintf('):%i,nD=%i,it=%i ;',length(size(tmpdata)),length(dims),it);
    end
    [ni nj nk nm nn no np]=size(tmpdata);
    %
    [i0,j0,fn]=findTileOffset(S);
    cdim=dims{1}; if cdim(1)~='X'; i0=0; end
    cdim=dims{1}; if cdim(1)=='Y'; i0=j0; j0=0; end
    if length(dims)>1;
      cdim=dims{2}; if cdim(1)~='Y'; j0=0; end
    else
      j0=0;
    end
    if dBug > 1,
      fprintf(' i0,ni= %i %i; j,nj= %i %i; nk=%i :',i0,ni,j0,nj,nk);
    end
    % code by Bruno Deremble: if you do not want to read all tiles these
    % modifications make the output field smaller, let us see, if it is
    % robust
    if (firstiter)
      S.attributes.i_first.(cvar) = i0;
      S.attributes.j_first.(cvar) = j0;
    end
    i0 = i0 - S.attributes.i_first.(cvar);
    j0 = j0 - S.attributes.j_first.(cvar);
    % end code by Bruno Deremble

    Sstr = '';
    for istr = 1:max(nDims,length(dims)),
      if     istr == it,  Sstr = [Sstr,'dii,'];
      elseif istr == 1,   Sstr = [Sstr,'i0+(1:ni),'];
      elseif istr == 2,   Sstr = [Sstr,'j0+(1:nj),'];
      elseif istr == 3,   Sstr = [Sstr,'(1:nk),'];
      elseif istr == 4,   Sstr = [Sstr,'(1:nm),'];
      elseif istr == 5,   Sstr = [Sstr,'(1:nn),'];
      elseif istr == 6,   Sstr = [Sstr,'(1:no),'];
      elseif istr == 7,   Sstr = [Sstr,'(1:np),'];
      else, error('Can''t handle this many dimensions!');
      end
    end
    eval(['S.(cvar)(',Sstr(1:end-1),')=tmpdata;'])
    %S.(cvar)(i0+(1:ni),j0+(1:nj),(1:nk),(1:nm),(1:nn),(1:no),(1:np))=tmpdata;
    if dBug > 1, fprintf(' %i',size(S.(cvar))); fprintf('\n'); end
    %
    S.attributes.(cvar)=ncgetatt(nc,cvar);
    % replace missing or FillValues with NaN
    if isfield(S.attributes.(cvar),'missing_value');
      misval = S.attributes.(cvar).missing_value;
      S.(cvar)(S.(cvar) == misval) = NaN;
    end
    if isfield(S.attributes.(cvar),'FillValue_');
      misval = S.attributes.(cvar).FillValue_;
      S.(cvar)(S.(cvar) == misval) = NaN;
    end

  end % for ivar

  % close the file
  netcdf.close(nc);

  if isempty(S)
    error('Something didn''t work!!!');
  end

  return

function vf = ncgetvar(fname,varname)
% read a netcdf variable

  nc=netcdf.open(fname,'NC_NOWRITE');
  % find out basics about the files
  [ndims nvars natts dimm] = netcdf.inq(nc);
  vf = [];
  varid = [];
  for k=0:nvars-1
    if strcmp(netcdf.inqVar(nc,k),varname)
      varid = netcdf.inqVarID(nc,varname);
    end
  end
  if ~isempty(varid);
    [varn,xtype,dimids,natts] = netcdf.inqVar(nc,varid);
    % get data
    vf = double(netcdf.getVar(nc,varid));
  else
    % do nothing
  end
  netcdf.close(nc);

  return

function misval = ncgetmisval(nc,varid)

  [varname,xtype,dimids,natts] = netcdf.inqVar(nc,varid);
  misval = [];
  for k=0:natts-1
    attn = netcdf.inqAttName(nc,varid,k);
    if strcmp(attn,'missing_value') | strcmp(attn,'_FillValue')
      misval = double(netcdf.getAtt(nc,varid,attname));
    end
  end

function A = ncgetatt(nc,varname)
% get all attributes and put them into a struct

% 1. get global properties of file
  [ndims nvars natts dimm] = netcdf.inq(nc);

  % get variable ID and properties
  if strcmp('global',varname)
    % "variable" is global
    varid = netcdf.getConstant('NC_GLOBAL');
  else
    % find variable ID and properties
    varid = ncfindvarid(nc,varname);
    if ~isempty(varid)
      [varn,xtype,dimids,natts] = netcdf.inqVar(nc,varid);
    else
      warning(sprintf('variable %s not found',varname))
    end
  end

  if natts > 1
    for k=0:natts-1
      attn = netcdf.inqAttName(nc,varid,k);
      [xtype attlen]=netcdf.inqAtt(nc,varid,attn);
      attval = netcdf.getAtt(nc,varid,attn);
      if ~ischar(attval)
        attval = double(attval);
      end
      if strcmp(attn,'_FillValue')
        % matlab does not allow variable names to begin with an
        % underscore ("_"), so we have to do change the name of this
        % obsolete attribute.
        A.FillValue_=attval;
      else
        A.(char(attn))=attval;
      end
    end
  else
      A = 'none';
  end

  return


function varid = ncfindvarid(nc,varname)

  [ndims nvars natts dimm] = netcdf.inq(nc);
  varid=[];
  for k=0:nvars-1
    if strcmp(netcdf.inqVar(nc,k),varname);
      varid = netcdf.inqVarID(nc,varname);
      break
    end
  end

  return
