function writegcm(fnam,fld,tme,nPx,nPy,prec,oflag)

% Function writegcm(fnam,fld,tme,nPx,nPy,prec,oflag)
% write array fld in MIT GCM UV format
%
% INPUTS
% fnam      output file name
% fld       Nx*Ny*Nz*Nt input array
% tme       model integration times in days has dimension Nt
% nPx, nPy  number of processes in x and y direction (default 1,1)
% prec      numeric precision (default 'real*4')
% oflag     overwrite flag specifies field to be overwritten
%           oflag=0 means create new file (the default)
%           oflag>0 must have dimension Nt
%
% SEE ALSO
% readgcm

if nargin < 2, error('please specify array and output file name'); end
[Nx Ny Nz Nt]=size(fld);
if nargin < 7, oflag=0; end
if oflag > 0
  if length(oflag) ~= Nt, error('length(oflag) must equal Nt'); end
end
if nargin < 6, prec='real*4'; end
if nargin < 5, nPy=1; end
if nargin < 4, nPx=1; end
if nargin < 3, tme=1:Nt; end
if length(tme) ~= Nt, error('length(tme) must equal Nt'); end

sNx=Nx/nPx;                                 % number of X points in sub-grid
sNy=Ny/nPy;                                 % number of Y points in sub-grid
if (sNx-floor(sNx))~=0 | (sNy-floor(sNy))~=0
  error('Nx/nPx and Ny/nPy must be integer')
end

switch prec
  case {'float32', 'real*4'}
    rlength=(sNx*sNy*Nz+1)*4;
  case {'float64', 'real*8'}
    rlength=(sNx*sNy*Nz+1)*8;
end

if oflag == 0

  fid=fopen(fnam,'w','ieee-be');
  for t=1:Nt
    for j=1:nPy
      jx=((j-1)*sNy+1):(j*sNy);
      for i=1:nPx
        ix=((i-1)*sNx+1):(i*sNx);
        fwrite(fid,tme(t),prec);
        fwrite(fid,fld(ix,jx,:,t),prec);
      end
    end
  end

else

  fid=fopen(fnam,'r+','ieee-be');
  for t=1:Nt
    if fseek(fid,nPx*nPy*(oflag(t)-1)*rlength,'bof')
      error('unexpected end of file reached')
    end
    for j=1:nPy
      jx=((j-1)*sNy+1):(j*sNy);
      for i=1:nPx
        ix=((i-1)*sNx+1):(i*sNx);
        fwrite(fid,tme(t),prec);
        fwrite(fid,fld(ix,jx,:,t),prec);
      end
    end
  end

end

fid=fclose(fid);
