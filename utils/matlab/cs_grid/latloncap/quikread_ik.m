function fld=quikread_ik(fnam,ix,kx,nx,nz,prec)

% Function fld=quikread_ik(fnam,ix,kx,nx,nz,prec)
% read in specific indices (ix,kx) from a 2D
% binary file of dimension(nx,nz)
%
% INPUTS
% fnam  input path and file name
% ix    i-indices, 1 to nx, to read (default 1)
% kx    k-indices, 1 to nz, to read (e.g., 1:50, default 1)
% nx    x-dimension of binary file (default 1440)
% nz    z-dimension of binary file (default 50)
% prec  numeric precision (see fread; default 'single')
%
% OUTPUTS
% fld    output array of dimension length(ix)*length(kx)
%
% SEE ALSO
% readbin read_ijk read_ijkt writebin

if nargin < 6, prec='single'; end
if nargin < 5, nz=50; end
if nargin < 4, nx=1440; end
if nargin < 3, kx=1; end
if nargin < 2, ix=1; end
if nargin < 1, error('please specify input file name'); end

m=memmapfile(fnam,'Format',{prec [nx,nz] 'fld'});
fld=swapbytes(m.Data.fld(ix,kx));
