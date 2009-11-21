function fld=read_cs_ifjk(fnam,ix,face,jx,kx,prec,cx)

% Function fld=read_cs_ifjk(fnam,ix,face,jx,kx,prec,cx)
% read in specific indices for cube sphere configuration
%
% INPUTS
% fnam  input path and file name
% ix    i-indices, 1 to cx, to read (default 1)
% face  face number, 1 to 6, to read (default 1)
% jx    j-indices, 1 to cx, to read (default 1)
% kx    vertical indices to read, e.g., 1:50 (default 1)
% prec  numeric precision (see fread; default 'real*4')
% cx    cube face size (default 510)
%
% OUTPUTS
% fld    output array of dimension length(ix)*length(face)*length(jx)*length(kx)
%
% SEE ALSO
% readbin, read_cs_bin, read_cs_face, read_cs_index

if nargin < 7, cx=510; end
if nargin < 6, prec='real*4'; end
if nargin < 5, kx=1; end
if nargin < 4, jx=1; end
if nargin < 3, face=1; end
if nargin < 2, ix=1; end
if nargin < 1, error('please specify input file name'); end

fld=zeros(length(ix),length(face),length(jx),length(kx));
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
    for f=1:length(face)
      for i=1:length(ix)
        skip = (kx(k)-1)*cx*6*cx + (jx(j)-1)*cx*6 + (face(f)-1)*cx + ix(i)-1;
        if(fseek(fid,skip*preclength,'bof')<0), error('past end of file'); end
        fld(i,f,j,k)=fread(fid,1,prec);
      end
    end
  end
end

fid=fclose(fid);
fld=squeeze(fld);
