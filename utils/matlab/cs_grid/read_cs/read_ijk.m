function fld=read_ijk(fnam,ix,jx,kx,nx,ny,nz,prec)

% Function fld=read_ijk(fnam,ix,jx,kx,nx,ny,nz,prec)
% read in specific indices (ix,jx,kx) from a 3D
% binary file of dimension(nx,ny,nz)
%
% INPUTS
% fnam  input path and file name
% ix    i-indices, 1 to nx, to read (default 1)
% jx    j-indices, 1 to ny, to read (default 1)
% kx    k-indices, 1 to nz, to read (e.g., 1:50, default 1)
% nx    x-dimension of binary file (default 1440)
% ny    y-dimension of binary file (default 640)
% nz    z-dimension of binary file (default 50)
% prec  numeric precision (see fread; default 'real*4')
%
% OUTPUTS
% fld    output array of dimension length(ix)*length(jx)*length(kx)
%
% SEE ALSO
% readbin read_ijkt writebin

if nargin < 8, prec='real*4'; end
if nargin < 7, nz=50; end
if nargin < 6, ny=640; end
if nargin < 5, nx=1440; end
if nargin < 4, kx=1; end
if nargin < 3, jx=1; end
if nargin < 2, ix=1; end
if nargin < 1, error('please specify input file name'); end

fld=zeros(length(ix),length(jx),length(kx));
fid=fopen(fnam,'r','ieee-be');

switch prec
  case {'int8','integer*1'}
    preclength=1;
  case {'int16','integer*2','uint16','integer*2'}
    preclength=2;
  case {'int32','integer*4','uint32','single','real*4','float32'}
    preclength=4;
  case {'int64','integer*8','uint64','double','real*8','float64'}
    preclength=8;
end

for k=1:length(kx)
  for j=1:length(jx)
    for i=1:length(ix)
      skip = (kx(k)-1)*ny*nx + (jx(j)-1)*nx + ix(i)-1;
      if(fseek(fid,skip*preclength,'bof')<0), error('past end of file'); end
      fld(i,j,k)=fread(fid,1,prec);
    end
  end
end

fid=fclose(fid);
fld=squeeze(fld);
