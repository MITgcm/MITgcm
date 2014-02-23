function fld=quikread_llc(fnam,nx,kx,prec,tiles);

% Function quikread_llc(fnam,nx,kx,prec,tiles);
% read lat-lon-cap field
%
% INPUTS
% fnam  input path and file name
% nx    tile dimension (default 270)
% kx    vertical indices to read, e.g., 1:50 (default 1)
% prec  numeric precision (see fread; default 'real*4')
% tiles llc tiles to read (default 1:13)
%
% OUTPUTS
% fld  output array of dimension nx*nx*length(tiles)*length(kx)
%
% EXAMPLES
% fld=quikread_llc('Depth.data',270); quikplot_llc(fld)
% fld=quikread_llc('Depth.data',270,1,'real*4',4:7); quikpcolor(fld')
% fld=quikread_llc('Depth.data',270,1,'real*4',8:10); quikpcolor(fld')
%
% SEE ALSO
% read_llc_fkij quilplot_llc quikpcolor

if nargin < 5, tiles=1:13; end
if nargin < 4, prec='real*4'; end
if nargin < 3, kx=1; end
if nargin < 2, nx=270; end
if nargin < 1, error('please specify input file name'); end

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

if preclength<=4
    % if input is single precision,
    % output single precision to save space
    fld=zeros(nx,nx*length(tiles),length(kx),'single');
else
    fld=zeros(length(ix),length(jx),length(kx));
end


if length(tiles)==13
    fid=fopen(fnam,'r','ieee-be');
    if preclength<=4
        % if input is single precision,
        % read single precision to save space
        prec=[prec '=>' prec];
    end    
    for k=1:length(kx)
        skip=(kx(k)-1)*nx*nx*13;
        if(fseek(fid,skip*preclength,'bof')<0), error('past end of file'); end
        fld(:,:,k)=reshape(fread(fid,nx*nx*13,prec),nx,nx*13);
    end    
    fid=fclose(fid);
else
    ix=1:nx;
    for k=1:length(kx)
        for tl=1:length(tiles)
            switch tiles(tl)
              case {1,2,3}
                face=1;
              case {4,5,6}
                face=2;
              case 7
                face=3;
              case {8,9,10}
                face=4;
              case {11,12,13}
                face=5;
            end
            switch tiles(tl)
              case {1,4,7,8,11}
                jx=1:nx;
              case {2,5,9,12}
                jx=(nx+1):(2*nx);
              case {3,6,10,13}
                jx=(2*nx+1):(3*nx);
            end
            fld(:,(nx*(tl-1)+1):(nx*tl),k)=read_llc_fkij(fnam,nx,face,kx(k),ix,jx,prec);
        end
    end
end
