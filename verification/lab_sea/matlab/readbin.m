function fld=readbin(fnam,siz,typ,prec,skip)

% Function fld=readbin(fnam,siz,typ,prec,skip)
% read in N-D binary field
%
% INPUTS
% fnam  input path and file name
% siz   grid dimension (default [360 224 46])
% typ   0: sequential FORTRAN (default);  1: plain binary
% prec  numeric precision (default 'real*4')
% skip  records to skip before reading (default 0)
%
% OUTPUTS
% fld    output array of dimension siz
%
% SEE ALSO
% writebin

if nargin < 5, skip=0; end
if nargin < 4, prec='real*4'; end
if nargin < 3, typ=0; end
if nargin < 2, siz=[360 224 46]; end
if nargin < 1, t=1; end
if nargin < 0, error('please specify input file name'); end

fid=fopen(fnam,'r','ieee-be');

if skip>0
  if typ==0
    error('feature not implemented yet');
  else
    switch prec
      case {'int8','integer*1'}
        reclength=prod(siz);
      case {'int16','integer*2','uint16','integer*2'}
        reclength=2*prod(siz);
      case {'int32','integer*4','uint32','single','real*4','float32'}
        reclength=4*prod(siz);
      case {'int64','integer*8','uint64','double','real*8','float64'}
        reclength=8*prod(siz);
    end
  end
  fseek(fid,skip*reclength,'bof');
end

switch typ
  case 0
    tmp=read_record(fid,prec);
  case 1
    tmp=fread(fid,[siz(1),prod(siz(2:length(siz)))],prec);
end
fid=fclose(fid);

switch length(siz)
  case 2
    fld=reshape(tmp,siz(1),siz(2));
  case 3
    fld=reshape(tmp,siz(1),siz(2),siz(3));
  case 4
    fld=reshape(tmp,siz(1),siz(2),siz(3),siz(4));
  case 5
    fld=reshape(tmp,siz(1),siz(2),siz(3),siz(4),siz(5));
  otherwise
    fld=tmp;
end
