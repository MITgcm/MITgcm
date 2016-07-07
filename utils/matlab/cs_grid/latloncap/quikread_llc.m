function [fld fc ix jx]=quikread_llc(fnam,nx,kx,prec,gdir,minlat,maxlat,minlon,maxlon);

% Function [fld fc ix jx]=quikread_llc(fnam,nx,kx,prec, ...
%                                      gdir,minlat,maxlat,minlon,maxlon);
% Read lat-lon-cap field
% If there is less than 5 input arguments: read the complete field.
% If there is more than 5 input arguments: read a region.
%
% INPUTS
% fnam   input path and file name
% nx     tile dimension (default 270)
% kx     vertical indices to read, e.g., 1:50 (default 1)
% prec   numeric precision (see fread; default 'real*4')
% gdir   directory path name that contains grid files XC.data and YC.data
% minlat minimum latitude of region to extract
% maxlat maximum latitude of region to extract
% minlon minimum longitude of region to extract
% maxlon maximum longitude of region to extract
% (valid longitude range is -180 to 180 or 0 to 360)
%
% OUTPUTS
% fld  output array
% fc   faces that contain requested region
% ix   i-indices for requested region
% jx   j-indices for requested region
% (note that if length(fc)>1, fld, ix, and jx are matlab
%  cell arrays that can be accessed as fld{fc(1)}, etc.)
%
% NOTES
%  For faces 4 and 5, southwest velocity is:
%   u_East (i,j) =   v_Model(i,j)
%   v_North(i,j) = - u_Model(i,j-1)
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
%
% SEE ALSO
% read_llc_fkij quilplot_llc quikpcolor

if nargin < 9, maxlon=180; end
if nargin < 8, minlon=-180; end
if nargin < 7, maxlat=90; end
if nargin < 6, minlat=-90; end
if nargin < 5, gdir=''; end
if nargin < 4, prec='real*4'; end
if nargin < 3, kx=1; end
if nargin < 2, nx=270; end
if nargin < 1, error('please specify input file name'); end

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

switch prec
 case {'int8'}
  preclength=1;
 case {'int16','uint16'}
  preclength=2;
 case {'int32','uint32','single'}
  preclength=4;
 case {'int64','uint64','double'}
  preclength=8;
end

if nargin < 6
    fld=zeros(nx,nx*13,length(kx),prec);
    fid=fopen(fnam,'r','ieee-be');
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
    if minlon < -180
        error('minlon<-180 not yet implemented: please email menemenlis@jpl.nasa.gov')
    end
    if maxlon > 360
        error('maxlon>360 not yet implemented: please email menemenlis@jpl.nasa.gov')
    end    
    if minlat >= maxlat
        error('maxlat must be greater than minlat')
    end
    if minlon >= maxlon
        error('maxlon must be greater than minlon')
    end    
    fld=[];
    fc =[];
    ix =[];
    jx =[];
    for f=1:5
        yc=read_llc_fkij([gdir 'YC.data'],nx,f);
        xc=read_llc_fkij([gdir 'XC.data'],nx,f);
        if maxlon>180
            in=find(xc<0);
            xc(in)=xc(in)+360;
        end
        [i j]=find(yc>=minlat&yc<=maxlat&xc>=minlon&xc<=maxlon);
        if ~isempty(i)
            fc=[fc f];
            ix{f}=min(i):max(i);
            jx{f}=min(j):max(j);
            fld{f}=zeros(length(ix{f}),length(jx{f}),length(kx),prec);
            for k=1:length(kx)
                fld{f}(:,:,k)=read_llc_fkij(fnam,nx,f,kx(k),ix{f},jx{f},prec);
            end
        end
    end
    if length(fc) == 1
        fld=fld{fc};
        ix = ix{fc};
        jx = jx{fc};
    end
end
