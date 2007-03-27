function fld=read_cs_bin(fnam,kx,prec,cx)

% Function fld=read_cs_bin(fnam,kx,prec,cx)
% read in cube sphere binary output
%
% INPUTS
% fnam  input path and file name
% kx    vertical indices to read, e.g., 1:50 (default 1)
% prec  numeric precision (see fread; default 'real*4')
% cx    cube face size (default 510)
%
% OUTPUTS
% fld    output array of dimension cx*6*cx*length(kx)
%
% SEE ALSO
% readbin, read_cs_face, read_cs_ifjk, read_cs_index

if nargin < 4, cx=510; end
if nargin < 3, prec='real*4'; end
if nargin < 2, kx=1; end
if nargin < 1, error('please specify input file name'); end

fld=zeros(cx,6,cx,length(kx));
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
  skip=(kx(k)-1)*cx*6*cx;
  if(fseek(fid,skip*preclength,'bof')<0), error('past end of file'); end
  fld(:,:,:,k)=reshape(fread(fid,cx*6*cx,prec),cx,6,cx);
end

fid=fclose(fid);
