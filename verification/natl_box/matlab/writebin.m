function writebin(fnam,fld,typ,prec)

% Function writebin(fnam,fld,typ,prec)
% write N-D binary field
%
% INPUTS
% fnam  input path and file name
% fld   input array
% typ   0: sequential FORTRAN (default);  1: plain binary
% prec  numeric precision (default 'real*4')
%
% SEE ALSO
% readbin

if nargin < 4, prec='real*4'; end
if nargin < 3, typ=0; end
if nargin < 2, error('please specify array and output file name'); end

fid=fopen(fnam,'w','ieee-be');
switch typ
  case 0
    write_record(fid,fld,prec);
  case 1
    fwrite(fid,fld,prec);
end
fid=fclose(fid);
