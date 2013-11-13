function fld=quikread_llc(fnam,nx,kx,prec,tiles);

% Function quikread_llc(fnam,nx,kx,prec);
% read lat-lon-cap field
%
% INPUTS
% fnam  input path and file name
% nx    face dimension (default 270)
% kx    vertical indices to read, e.g., 1:50 (default 1)
% prec  numeric precision (see fread; default 'real*4')
% tiles llc tiles to read (default 1:13)
%
% OUTPUTS
% fld  output array of dimension nx*nx*length(tiles)*length(kx)
%
% EXAMPLES
% fld=quikread_llc('Depth.data',270,1,'real*4',4:7);
% quikpcolor(fld')
% fld=quikread_llc('Depth.data',270,1,'real*4',8:10);
% quikpcolor(rot90(fld,2)');

if nargin < 5, tiles=1:13; end
if nargin < 4, prec='real*4'; end
if nargin < 3, kx=1; end
if nargin < 2, nx=270; end
if nargin < 1, error('please specify input file name'); end

fld=zeros(nx,nx*length(tiles),length(kx));
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

if length(tiles)==13
    for k=1:length(kx)
        skip=(kx(k)-1)*nx*nx*13;
        if(fseek(fid,skip*preclength,'bof')<0), error('past end of file'); end
        fld(:,:,k)=reshape(fread(fid,nx*nx*13,prec),nx,nx*13);
    end
else
    for k=1:length(kx)
        sk=(kx(k)-1)*nx*nx*13;
        for tl=1:length(tiles)
            switch tiles(tl)
              case {1,2,3,4,5,6,7}
                skip=sk+(tiles(tl)-1)*nx*nx;
                if(fseek(fid,skip*preclength,'bof')<0)
                    error(['past end of file']);
                end
                tmp=fread(fid,nx*nx,prec);
                tmp=reshape(tmp,nx,nx);
              case {8,9,10}
                tmp=zeros(nx);
                for j=1:nx
                    skip=sk+7*nx*nx+nx*(tiles(tl)-8)+(j-1)*nx*3;
                    if(fseek(fid,skip*preclength,'bof')<0)
                        error(['past end of file']);
                    end
                    tmp(:,j)=fread(fid,nx,prec);
                end
                tmp=rot90(tmp,1);
              case {11,12,13}
                tmp=zeros(nx);
                for j=1:nx
                    skip=sk+10*nx*nx+nx*(tiles(tl)-11)+(j-1)*nx*3;
                    if(fseek(fid,skip*preclength,'bof')<0)
                        error(['past end of file']);
                    end
                    tmp(:,j)=fread(fid,nx,prec);
                end
                tmp=rot90(tmp,1);
            end
            fld(:,(nx*(tl-1)+1):(nx*tl),k)=tmp;
        end
    end
end

fid=fclose(fid);
