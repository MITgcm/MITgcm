function [S] = rdmnc(varargin)
% S=RDMNC(FILE1,FILE2,...)
% S=RDMNC(FILE1,...,ITER)
% S=RDMNC(FILE1,...,'VAR1','VAR2',...)
% S=RDMNC(FILE1,...,'VAR1','VAR2',...,ITER)
%
%  FILE1 ... is either a single file name (e.g. 'state.nc') or a wild-card
%        strings expanding to a group of file names (e.g. 'state.*.nc').
%        There are no assumptions about suffices (e.g. 'state.*' works)
%  VAR1 ... are model variable names as written in the MNC/netcdf file
%  ITER  is a vector of time levels (consecutive indices referring to snap-shot
%        in the MNC/netcdf file i.e. 1,2,3,... and not model time)
%  S     is a structure with mutliple fields
%
% rdmnc() is a rudimentary wrapper for joining and reading netcdf files
% produced by MITgcm.  It does not give the same flexibility as opening the
% netcdf files directly using netcdf(). It is useful for quick loading of
% entire model fields which are distributed in multiple netcdf files.
% rdmnc() also reads non-MITgcm generated netcdf files.
%
% e.g. To plot the surface temperature in last time-level written to file
% >> S=rdmnd('state.*','XC','YC','RC','T',Inf);
% >> imagesc( S.XC, S.YC, S.T(:,:,1)' );
%
% $Header: /u/gcmpack/MITgcm/utils/matlab/rdmnc.m,v 1.2 2005/02/02 10:31:22 mlosch Exp $

%Defaults
global verbose
file={};
files={};
verbose=0;
varlist={};
timelevels=[];

% Process function arguments
for iarg=1:nargin;
 arg=varargin{iarg};
 if ischar(arg)
   switch char(arg)
     case 'verbose',
       verbose=1;
     otherwise,
       if isempty( dir(char(arg)) )
        varlist{end+1}={arg};
       else
        file{end+1}={arg};
       end
   end
 else
   if isempty(timelevels)
     timelevels=arg;
   else
     error('Specify the time levels in a vector and not as multiple numeric arguments')
   end
 end
end

if verbose; disp('Verbose mode enabled'); end

% Create list of filenames
for eachfile=file
 expandedEachFile=dir(char(eachfile{1}));
 for i=1:size(expandedEachFile,1);
  if expandedEachFile(i).isdir==0; files{end+1}=expandedEachFile(i).name; end
 end
end

% Open each file
S.attributes=[];
for eachfile={files{1:end}}
 nc=netcdf(char(eachfile),'read');
 if ismncfile(nc)
  S=rdmnc_local(nc,varlist,timelevels,S);
 else
  S=rdnetcdf_local(nc,varlist,S);
 end
end

% -----------------------------------------------------------------------------
function [result] = ismncfile(nc);
result=~isempty(nc.MITgcm_mnc_ver);
%MLresult=~isempty(nc.('MITgcm_mnc_ver'));
% -----------------------------------------------------------------------------
function [A] = read_att(nc);
global verbose
allatt=ncnames(att(nc)); if verbose; allatt, end
A='none';
for attr=allatt;
 tmp = char(attr);
 eval(['A.' tmp '= nc.' tmp '(:);'])
%ML A.(char(attr))=nc.(char(attr))(:);
end
% -----------------------------------------------------------------------------
function [i0,j0,fn] = findTileOffset(S);
global verbose
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
 end
else
 i0=0;j0=0;
end
% -----------------------------------------------------------------------------
function [S] = rdnetcdf_local(nc,varlist,S)
% Read all attributes and variable data from a netcdf file
% with special operations for MNC style data
global verbose

% No variables specified? Default to all
if isempty(varlist)
 varlist=ncnames(var(nc)); if verbose; varlist, end
end

% Attributes for structure
S.attributes=read_att(nc);

% Read variable data
for ivar=1:size(varlist,2);
 cvar=char(varlist{ivar});
 tmpdata=nc{cvar}(:);
 if isempty(tmpdata)
  disp(['No such variable ''' cvar ''' in netcdf file' name(nc)])
 else
  tmpdata=squeeze(permute(tmpdata,[9:-1:1]));
  eval(['S.' cvar '=tmpdata;'])
  eval(['S.attributes.' cvar '=read_att(nc{' cvar '});'])
%ML  S.(cvar)=tmpdata;
%ML  S.attributes.(cvar)=read_att(nc{cvar});
 end
end
% -----------------------------------------------------------------------------
function [S] = rdmnc_local(nc,varlist,timelevels,S)
% Read all attributes and variable data from a netcdf file
% with special operations for MNC style data
global verbose

nt=size( nc('T'), 1); if verbose; nt, end

% No variables specified? Default to all
if isempty(varlist)
 varlist=ncnames(var(nc)); if verbose; varlist, end
end

% No iterations specified? Default to all
if isempty(timelevels) | isnan(timelevels)
 timelevels=1:nt;
elseif timelevels == Inf
 timelevels=nt;
end

% Sanity checks
if max( find(timelevels > nt) )
 error('Requested time level is beyond time dimension in netcdf file')
end

% Attributes for structure
if timelevels>0; S.timelevels_read_from_file=timelevels; end
S.attributes.global=read_att(nc);

% Read variable data
for ivar=1:size(varlist,2);
 cvar=char(varlist{ivar}); if verbose; cvar, end
 if isempty(nc{cvar})
  disp(['No such variable ''' cvar ''' in MNC file ' name(nc)])
  continue
 end
 dims=ncnames(dim(nc{cvar}));
 if dims{1}=='T'
  if verbose; disp(['Reading variable ''' cvar ''' with record dimension']); end
  tmpdata=nc{cvar}(timelevels,:);
 else
  if verbose; disp(['Reading variable ''' cvar '''']); end
  tmpdata=nc{cvar}(:);
 end
 if isempty(tmpdata)
  error(['No such variable ''' cvar ''' in MNC file ' name(nc)])
 else
  tmpdata=squeeze(permute(tmpdata,[9:-1:1]));
  [ni nj nk nm nn no np]=size(tmpdata);
  if np~=1; error('Wow! This is a very high dimension variable...'); end
  [i0,j0,fn]=findTileOffset(S);
  cdim=dims{end}; if cdim(1)~='X'; i0=0; end
  cdim=dims{end}; if cdim(1)=='Y'; i0=j0; j0=0; end
  if length(dims)>1;
   cdim=dims{end-1}; if cdim(1)~='Y'; j0=0; end
  else
   j0=0;
  end
  eval(['S.' cvar ...
	'(i0+(1:ni),j0+(1:nj),(1:nk),(1:nm),(1:nn),(1:no),(1:np))=tmpdata;'])
  eval(['S.attributes.' cvar ' =read_att(nc{''' cvar '''});'])
%ML  S.(cvar)(i0+(1:ni),j0+(1:nj),(1:nk),(1:nm),(1:nn),(1:no),(1:np))=tmpdata;
%ML  S.attributes.(cvar)=read_att(nc{cvar});
 end
end

if isempty(S)
 error('Something didn''t work!!!')
end
% -----------------------------------------------------------------------------
