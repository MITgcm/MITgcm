function fld=readbin(fnam,siz,typ,prec)

% Function fld=readbin(fnam,siz,typ,prec)
% read in N-D binary field
%
% INPUTS
% fnam   input path and file name
% siz    grid dimension (default [360 224 46])
% typ    0: sequential FORTRAN (default);  1: plain binary
% prec   numeric precision (default 'real*4')
%
% OUTPUTS
% fld    output array of dimension siz
%
% SEE ALSO
% writebin

if nargin<4, prec='real*4'; end
if nargin<3, typ=0; end
if nargin<2, siz=[360 224 46]; end
if nargin<1, t=1; end
if nargin<0, error('please specify input file name'); end

fid=fopen(fnam,'r','ieee-be');
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
