function [fld,tme,nPx,nPy]=readgcm(fnam,t,Nx,Ny,Nz,Ix,Iy,Iz,prec,nPx,nPy)

% Function [fld,tme,nPx,nPy]=readgcm(fnam,t,Nx,Ny,Nz,Ix,Iy,Iz,prec,nPx,nPy)
% read and output model field from file fnam time step t
%
% INPUTS
% fnam      input path and file name
% t         indices of time steps to read (0 means read all, the default)
% Nx*Ny*Nz  grid dimension (default 360*224*46)
% Ix,Iy,Iz  subsample indices (default Ix=1:Nx, Iy=1:Ny, and Iz=1:Nz)
% prec      numeric precision (default 'real*4')
%
% INPUT/OUTPUT
% nPx, nPy  number of processes in x and y direction;
%           when not specified as INPUTS, nPx and nPy are determined based
%           on the following file naming convention:
%             U_06_04.00060_00120_10 is
%             U for nPx=6, nPy=4, hour 60 to 120 at 10-hour intervals
%           (defaults to 1 when filename contains no underscores)
%
% OUTPUTS
% fld   Nx*Ny*Nz*Nt output array
%          e.g., to contour level k, use "contourf(fld(:,:,k,1)'), colorbar"
% tme   model integration times in days
%
% The following are valid calling forms:
%    readgcm(fnam,t,Nx,Ny,Nz,Ix,Iy,Iz,prec,nPx,nPy)
%    readgcm(fnam, ...                            )
%    readgcm(fnam, ...               ,prec, ...   )
% where "..." represents 0 or more ordered arguments.
%
% SEE ALSO
% writegcm

% "prec" is set to value of second string argument if it exists
nprec=9;
if exist('t') , if ischar(t)
    prec=t;  nprec=2;
    if exist('Nx'), nPx=Nx; end
    if exist('Ny'), nPy=Ny; end
end, end
if exist('Nx'), if ischar(Nx)
    prec=Nx; nprec=3;
    if exist('Ny'), nPx=Ny; end
    if exist('Nz'), nPy=Nz; end
end, end
if exist('Ny'), if ischar(Ny)
    prec=Ny; nprec=4;
    if exist('Nz'), nPx=Nz; end
    if exist('Ix'), nPy=Ix; end
end, end
if exist('Nz'), if ischar(Nz)
    prec=Nz; nprec=5;
    if exist('Ix'), nPx=Ix; end
    if exist('Iy'), nPy=Iy; end
end, end
if exist('Ix'), if ischar(Ix)
    prec=Ix; nprec=6;
    if exist('Iy'), nPx=Iy; end
    if exist('Iz'), nPy=Iz; end
end, end
if exist('Iy'), if ischar(Iy)
    prec=Iy; nprec=7;
    if exist('Iz'), nPx=Iz; end
    if exist('prec'), nPy=prec; end
end, end
if exist('Iz'), if ischar(Iz)
    prec=Iz; nprec=8;
    if exist('prec'), nPx=prec; end
    if exist('nPx'), nPy=nPx; end
end, end

% set default argument values
if nargin < 9 & nprec==9, prec='real*4'; end
if nargin < 5 | nprec <= 5
  if isempty(findstr(fnam,'KPPhbl')) & ...
        isempty(findstr(fnam,'H')) & ...
        isempty(findstr(fnam,'SF')) & ...
        isempty(findstr(fnam,'Pbottom')) & ...
        isempty(findstr(fnam,'Pbottom')) & ...
        isempty(findstr(fnam,'BU')) & ...
        isempty(findstr(fnam,'BV'))
    Nz=46;
  else 
    Nz=1;
  end
end
if nargin < 4 | nprec <= 4, Ny=224; end
if nargin < 3 | nprec <= 3, Nx=360; end
if nargin < 8 | nprec <= 8, Iz=1:Nz; end
if nargin < 7 | nprec <= 7, Iy=1:Ny; end
if nargin < 6 | nprec <= 6, Ix=1:Nx; end
if nargin < 2 | nprec <= 2, t=0; end
if nargin < 1, error('please specify input file name'); end
if nargin < nprec+1
  % locate beginning filename character in path
  slash_loc=findstr('/',fnam);
  if isempty(slash_loc), slash_loc=0; end
  slash_loc=max(slash_loc);
  bar_loc=findstr('_',fnam((slash_loc+1):length(fnam)));
  bar_loc=min(bar_loc);
  if isempty(bar_loc)
    nPx=1; nPy=1;
  elseif length(fnam)<slash_loc+bar_loc+5
    nPx=1; nPy=1;
  else
    nPx=str2num(fnam(slash_loc+bar_loc+(1:2)));  % number of processes in X
    nPy=str2num(fnam(slash_loc+bar_loc+(4:5)));  % number of processes in Y
    if isempty(nPx) | isempty(nPy), nPx=1; nPy=1; end
  end
elseif nargin < nprec+2
  nPy=nPx;                                  % number of processes in X and Y
end

% compute number of sub-grids
sNx=Nx/nPx;                                 % number of X points in sub-grid
sNy=Ny/nPy;                                 % number of Y points in sub-grid
if (sNx-floor(sNx))~=0 | (sNy-floor(sNy))~=0
  error('Nx/nPx and Ny/nPy must be integer')
end

% compute file record length
switch prec
  case {'float32', 'real*4'}
    rlength=(sNx*sNy*Nz+1)*4;
  case {'float64', 'real*8'}
    rlength=(sNx*sNy*Nz+1)*8;
end

% compute total number of time steps
fid=fopen(fnam,'r','ieee-be');
tmp=fseek(fid,0,'eof');
tmp=ftell(fid);
tmp=tmp/(rlength*nPx*nPy);
if t==0 | tmp<max(t)
  t=1:tmp;
end

% read model file
fld=zeros(length(Ix),length(Iy),length(Iz),length(t));
tme=zeros(1,length(t));
disp(['Reading ' int2str(length(t)) ' time steps:'])

if length(Ix)==Nx & length(Iy)==Ny & length(Iz)==Nz

  for k=1:length(t)
    fprintf(1,'\b\b\b%0.0f',k)
    tmp=fseek(fid,nPx*nPy*(t(k)-1)*rlength,'bof');
    for j=1:nPy
      jx=((j-1)*sNy+1):(j*sNy);
      for i=1:nPx
        ix=((i-1)*sNx+1):(i*sNx);
        tm=fread(fid,1,prec);
        [tmp count]=fread(fid,[sNx,sNy*Nz],prec);
        tme(k)=tm;
        fld(ix,jx,:,k)=reshape(tmp,sNx,sNy,Nz);
      end
    end
  end
  
elseif length(Ix)<Nx | length(Iy)<Ny

  % If only part of the file needs to be read, determine
  % which tiles contain the desired data.
  nPx_id=zeros(Nx,Ny);
  nPy_id=zeros(Nx,Ny);
  for j=1:nPy
    jx=((j-1)*sNy+1):(j*sNy);
    for i=1:nPx
      ix=((i-1)*sNx+1):(i*sNx);
      nPx_id(ix,jx)=i;
      nPy_id(ix,jx)=j;
    end
  end
  nPx_id=nPx_id(Ix,Iy);
  nPy_id=nPy_id(Ix,Iy);
  nPxy_id=[nPx_id(:) nPy_id(:)];
  if length(nPx_id)>1
    nPxy_id=sortrows(nPxy_id);
    nPxy_id(find(sum(abs(diff(nPxy_id)'))'==0)+1,:)=[];
  end

  fld_tmp=zeros(Nx,Ny,Nz);
  for k=1:length(t)
    fprintf(1,'\b\b\b%0.0f',k)
    for l=1:size(nPxy_id,1)
      i=nPxy_id(l,1);
      j=nPxy_id(l,2);
      rnumber=nPx*nPy*(t(k)-1)+(j-1)*nPx+i-1;
      tmp=fseek(fid,rnumber*rlength,'bof');
      ix=((i-1)*sNx+1):(i*sNx);
      jx=((j-1)*sNy+1):(j*sNy);
      tm=fread(fid,1,prec);
      [tmp count]=fread(fid,[sNx,sNy*Nz],prec);
      tme(k)=tm;
      fld_tmp(ix,jx,:)=reshape(tmp,sNx,sNy,Nz);
    end
    fld(:,:,:,k)=fld_tmp(Ix,Iy,Iz);
  end
  
elseif length(Iz)<Nz

  fld_tmp=zeros(Nx,Ny,Nz);
  for k=1:length(t)
    fprintf(1,'\b\b\b%0.0f',k)
    tmp=fseek(fid,nPx*nPy*(t(k)-1)*rlength,'bof');
    for j=1:nPy
      jx=((j-1)*sNy+1):(j*sNy);
      for i=1:nPx
        ix=((i-1)*sNx+1):(i*sNx);
        tm=fread(fid,1,prec);
        [tmp count]=fread(fid,[sNx,sNy*Nz],prec);
        tme(k)=tm;
        fld_tmp(ix,jx,:)=reshape(tmp,sNx,sNy,Nz);
      end
    end
    fld(:,:,:,k)=fld_tmp(:,:,Iz);
  end

end

fid=fclose(fid);
fprintf(1,'\b\b\b',k)

