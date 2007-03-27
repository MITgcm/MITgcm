function fld=read_cs_index(fnam,ij_indices,kx,prec,cx)

% Function fld=read_cs_index(fnam,ij_indices,kx,prec,cx)
% read in  horizontal-slice indices for cube sphere configuration
%
% INPUTS
% fnam        input path and file name
% ij_indices  cube sphere horizontal-slice indices (default 1)
% kx          vertical indices to read, e.g., 1:50 (default 1)
% prec        numeric precision (see fread; default 'real*4')
% cx          cube face size (default 510)
%
% OUTPUTS
% fld    output array of dimension length(ij_indices)*length(kx)
%
% SEE ALSO
% readbin, read_cs_bin, read_cs_face, read_cs_ifjk

if nargin < 5, cx=510; end
if nargin < 4, prec='real*4'; end
if nargin < 3, kx=1; end
if nargin < 2, ij_indices=1; end
if nargin < 1, error('please specify input file name'); end

fld=zeros(length(ij_indices),length(kx));
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
  for i=1:length(ij_indices)
    skip = (kx(k)-1)*cx*6*cx + ij_indices(i)-1;
    if(fseek(fid,skip*preclength,'bof')<0), error('past end of file'); end
    fld(i,k)=fread(fid,1,prec);
  end
end

fid=fclose(fid);
