function fld=read_llc(fnam,nx,kx,prec);

% Function read_llc(fnam,nx,kx,prec);
% read lat-lon-cap field
%
% INPUTS
% fnam  input path and file name
% nx    face dimension (default 270)
% kx    vertical indices to read, e.g., 1:50 (default 1)
% prec  numeric precision (see fread; default 'real*4')
%
% OUTPUTS
% fld  output array of dimension nx*nx*13*length(kx)

if nargin < 4, prec='real*4'; end
if nargin < 3, kx=1; end
if nargin < 2, nx=270; end
if nargin < 1, error('please specify input file name'); end

fld=zeros(nx,nx*13,length(kx));
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
  skip=(kx(k)-1)*nx*nx*13;
  if(fseek(fid,skip*preclength,'bof')<0), error('past end of file'); end
  fld(:,:,k)=reshape(fread(fid,nx*nx*13,prec),nx,nx*13);
end

fid=fclose(fid);
