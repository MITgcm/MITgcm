function fld=read_ijkt(fnam,ix,jx,kx,tx,nx,ny,nz,nt,prec)

% Function fld=read_ijkt(fnam,ix,jx,kx,tx,nx,ny,nz,nt,prec)
% read in specific indices (ix,jx,kx,tx) from a 4D
% binary file of dimension(nx,ny,nz,nt)
%
% INPUTS
% fnam  input path and file name
% ix    i-indices, 1 to nx, to read (default 1)
% jx    j-indices, 1 to ny, to read (default 1)
% kx    k-indices, 1 to nz, to read (e.g., 1:50, default 1)
% tx    t-indices, 1 to nt, to read (default 1)
% nx    x-dimension of binary file (default 1440)
% ny    y-dimension of binary file (default 640)
% nz    z-dimension of binary file (default 50)
% nz    t-dimension of binary file (default 1)
% prec  numeric precision (see fread; default 'real*4')
%
% OUTPUTS
% fld    output array of dimension
%        length(ix)*length(jx)*length(kx)*length(kt)
%
% SEE ALSO
% readbin read_ijk writebin

if nargin < 10, prec='real*4'; end
if nargin < 9, nt=1; end
if nargin < 8, nz=50; end
if nargin < 7, ny=640; end
if nargin < 6, nx=1440; end
if nargin < 5, tx=1; end
if nargin < 4, kx=1; end
if nargin < 3, jx=1; end
if nargin < 2, ix=1; end
if nargin < 1, error('please specify input file name'); end

fld=zeros(length(ix),length(jx),length(kx),length(tx));
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

for t=1:length(tx)
  for k=1:length(kx)
    for j=1:length(jx)
      for i=1:length(ix)
        skip = (tx(t)-1)*nz*ny*nx + (kx(k)-1)*ny*nx + (jx(j)-1)*nx + ix(i)-1;
        if(fseek(fid,skip*preclength,'bof')<0), error('past end of file'); end
        fld(i,j,k,t)=fread(fid,1,prec);
      end
    end
  end
end

fid=fclose(fid);
fld=squeeze(fld);
