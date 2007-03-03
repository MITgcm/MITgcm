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

% $Header: /u/gcmpack/MITgcm/utils/matlab/rdmnc.m,v 1.15 2007/03/03 21:40:45 jmc Exp $
% $Name:  $

% Initializations
dBug=0;
file={};
filepaths={};
files={};
varlist={};
iters=[];

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
	indecies = find(filepathtemp=='/');
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
        nc=netcdf(char(eachfile),'read');
        nciters = nc{'iter'}(:);
        if isempty(nciters), nciters = nc{'T'}(:); end
        iters = [iters,nciters'];
        close(nc);
    end
    iters = unique(iters');
end

% Cycle through files for data extraction.
S.attributes=[];
for ieachfile=1:length(files)
    eachfile = [filepaths{ieachfile},files{ieachfile}];
    if dBug > 0, fprintf([' open: ',eachfile]); end
    nc=netcdf(char(eachfile),'read');
    S=rdmnc_local(nc,varlist,iters,S,dBug);
    close(nc);
end


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
    
	% Read variable data
	for ivar=1:size(varlist,2)
        
        cvar=char(varlist{ivar});
        if isempty(nc{cvar})
            disp(['No such variable ''',cvar,''' in MNC file ',name(nc)]);
            continue
        end
	
        dims = ncnames(dim(nc{cvar}));        % Dimensions
        sizVar = size(nc{cvar}); nDims=length(sizVar);
        if dims{1} == 'T'
            if isempty(find(fii)), error('Iters not found'); end
            it = length(dims);
            tmpdata = nc{cvar}(fii,:);
%-      leading unity dimensions get lost; add them back:
            tmpdata=reshape(tmpdata,[length(fii) sizVar(2:end)]);
        else
            it = 0;
            tmpdata = nc{cvar}(:);
%-      leading unity dimensions get lost; add them back:
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
	end

if isempty(S)
    error('Something didn''t work!!!');
end
