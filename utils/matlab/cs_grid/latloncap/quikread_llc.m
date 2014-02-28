function fld=quikread_llc(fnam,nx,kx,prec,pname,minlat,maxlat,minlon,maxlon);

% Function quikread_llc(fnam,nx,kx,prec,pname,minlat,maxlat,minlon,maxlon);
% Read lat-lon-cap field
% If there is less than 5 input arguments: read the complete field.
% If there is more than 5 input arguments: read a region.
%
% INPUTS
% fnam   input path and file name
% nx     tile dimension (default 270)
% kx     vertical indices to read, e.g., 1:50 (default 1)
% prec   numeric precision (see fread; default 'real*4')
% pname  directory path name that contains XC.data and YC.data
% minlat minimum latitude of region to extract
% maxlat maximum latitude of region to extract
% minlon minimum longitude of region to extract
% maxlon maximum longitude of region to extract
%
% OUTPUTS
% fld  output array
%
% SEE ALSO
% read_llc_fkij quilplot_llc quikpcolor
%
% EXAMPLES
%
% % read and plot complete field
% fld=quikread_llc('Depth.data',270);
% quikplot_llc(fld)
%
% % read and plot the region 120W to 40W and 80S and 60N
% fld=quikread_llc('Depth.data',270,1,'real*4','',-80,60,-120,-40);
% quikpcolor(fld')

if nargin < 9, maxlon=180; end
if nargin < 8, minlon=-180; end
if nargin < 7, maxlat=90; end
if nargin < 6, minlat=-90; end
if nargin < 5, pname=''; end
if nargin < 4, prec='real*4'; end
if nargin < 3, kx=1; end
if nargin < 2, nx=270; end
if nargin < 1, error('please specify input file name'); end

if minlon < -180
    error('minlon<-180 not yet implemented: please email menemenlis@jpl.nasa.gov')
end
if maxlon > 180
    error('maxlon>180 not yet implemented: please email menemenlis@jpl.nasa.gov')
end
if minlat >= maxlat
    error('maxlat must be greater than minlat')
end
if minlon >= maxlon
    error('maxlon must be greater than minlon')
end

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

if nargin < 6
    if preclength <= 4
        % if input is single precision,
        % output single precision to save space
        fld=zeros(nx,nx*13,length(kx),'single');
    else
        fld=zeros(nx,nx*13,length(kx));
    end
    fid=fopen(fnam,'r','ieee-be');
    if preclength <= 4
        % if input is single precision,
        % read single precision to save space
        prec=[prec '=>' prec];
    end    
    for k=1:length(kx)
        if kx(k) > 1
            skip=(kx(k)-1)*nx*nx*13;
            if(fseek(fid,skip*preclength,'bof')<0)
                error('past end of file');
            end
        end
        fld(:,:,k)=reshape(fread(fid,nx*nx*13,prec),nx,nx*13);
    end
    fid=fclose(fid);
else
    fc=[];
    for f=1:5
        yc=read_llc_fkij([pname 'YC.data'],nx,f);
        xc=read_llc_fkij([pname 'XC.data'],nx,f);
        [i j]=find(yc>=minlat&yc<=maxlat&xc>=minlon&xc<=maxlon);
        if ~isempty(i)
            if isempty(fc)
                fc=f;
                ix=min(i):max(i);
                jx=min(j):max(j);
            else
                error(['multiface output not yet implemented: please email menemenlis@jpl.nasa.gov'])
            end
        end
    end
    if preclength <= 4
        % if input is single precision,
        % output single precision to save space
        fld=zeros(length(ix),length(jx),length(kx),'single');
    else
        fld=zeros(length(ix),length(jx),length(kx));
    end
    for k=1:length(kx)
        fld(:,:,k)=read_llc_fkij(fnam,nx,fc,kx(k),ix,jx,prec);
    end
end
