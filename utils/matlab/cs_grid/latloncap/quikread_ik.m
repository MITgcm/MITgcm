function fld=quikread_ik(fnam,ix,kx,nx,nz,prec,mform)

% Function fld=quikread_ik(fnam,ix,kx,nx,nz,prec,mform)
% read in specific indices (ix,kx) from a 2D
% binary file of dimension(nx,nz)
%
% INPUTS
% fnam  input path and file name
% ix    i-indices, 1 to nx, to read (default 1)
% kx    k-indices, 1 to nz, to read (e.g., 1:50, default 1)
% nx    x-dimension of binary file (default 1440)
% nz    z-dimension of binary file (default 50)
% prec  numeric precision (see fread; default: 'real*4')
% mform machine format (see fopen; default: 'ieee-be')
%
% OUTPUTS
% fld    output array of dimension length(ix)*length(kx)
%
% SEE ALSO
% readbin read_ijk read_ijkt writebin

if nargin < 7, mform='ieee-be'; end
if nargin < 6, prec='real*4'; end
if nargin < 5, nz=50; end
if nargin < 4, nx=1440; end
if nargin < 3, kx=1; end
if nargin < 2, ix=1; end
if nargin < 1, error('please specify input file name'); end

if ~exist(fnam)
    error(['File ' fnam ' does not exist.'])
end

dx=min(diff(ix(:)));

if dx ~= max(diff(ix(:)))
    % Use memory mapping.  Can handle arbitrary i-indices but
    % can be slow and require a lot of memory for large files.
    
    switch prec
      case {'integer*1'}
        prec='int8';
      case {'integer*2'}
        prec='int16';
      case {'integer*4'}
        prec='int32';
      case {'real*4','float32'}
        prec='single';
      case {'integer*8'}
        prec='int64';
      case {'real*8','float64'}
        prec='double';
    end

    m=memmapfile(fnam,'Format',{prec [nx,nz] 'fld'});
    fld=swapbytes(m.Data.fld(ix,kx));

else
    % Use fread SKIP capability.  Much faster and memory efficient
    % for large files but requires that i-indices be equidistant.

    if isempty(dx), dx=0;, end
    
    switch prec
      case {'int8','integer*1'}
        reclength=1;
      case {'int16','integer*2','uint16'}
        reclength=2;
      case {'int32','integer*4','uint32','single','real*4','float32'}
        reclength=4;
      case {'int64','integer*8','uint64','double','real*8','float64'}
        reclength=8;
    end

    fid=fopen(fnam,'r',mform);
    fld=zeros(length(ix),length(kx));
    for k=1:length(kx)
        skip=(kx(k)-1)*nx+ix(1)-1;
        if(fseek(fid,skip*reclength,'bof')<0), error('past end of file'); end
        fld(:,k)=fread(fid,length(ix),prec,(dx-1)*reclength,mform);
    end
    fid=fclose(fid);

end
