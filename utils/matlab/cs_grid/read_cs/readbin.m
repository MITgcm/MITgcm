function fld=readbin(fnam,siz,typ,prec,skip,mform)

% Function fld=readbin(fnam,siz,typ,prec,skip,mform)
% read in N-D binary field
%
% INPUTS
% fnam  input path and file name
% siz   grid dimension (default [360 224 46])
% typ   0: sequential FORTRAN;  1: plain binary (the default)
% prec  numeric precision (see fread; default: 'real*4')
% skip  records to skip before reading (default 0)
% mform machine format (see fopen; default: 'ieee-be')
%
% OUTPUTS
% fld    output array of dimension siz
%
% SEE ALSO
% read_ijk read_ijkt writebin

if nargin < 6, mform='ieee-be'; end
if nargin < 5, skip=0; end
if nargin < 4, prec='real*4'; end
if nargin < 3, typ=1; end
if nargin < 2, siz=[360 224 46]; end
if nargin < 1, error('please specify input file name'); end

if ~exist(fnam)
    error(['File ' fnam ' does not exist.'])
end

fid=fopen(fnam,'r',mform);

if skip>0
  if typ==0
    for n=1:skip
      tmp=read_record(fid,prec);
    end
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
    if(fseek(fid,skip*reclength,'bof')<0), error('past end of file'); end
  end
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
