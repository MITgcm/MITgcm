function G = load_grid(varargin);
% function G = load_grid(dirName,option,nyAxis,nxAxis);
%
% load the MITgcm output grid-files into one structure array
% arguments:
%     dirName = directory where grid-files are
% optional arguments:
%      ncdf = last digit of integer "option":
%        ncdf = 0,2 : read binary (MDSIO) files using rdmds (default: ncdf=0)
%        ncdf = 1,3 : read NetCDF (MNC)   files using rdmnc
%        ncdf = 2,3 : as 0,1 + print elapsed-time for loading files
%      option >= 10 : only read in Horiz.Grid spacing (without hFac)
%      option >= 20 : only read in Verti.Grid spacing (without hFac)
%      nxAxis = number of spacing in 1.D x-axis xAxC (default: nxAxis = grid-Nx)

if nargin == 0
  rDir = '.';
else
  rDir = varargin{1};
end
if nargin < 2,
 ktyp=0;
 ncdf=0;
else
 ncdf= varargin{2};
 ktyp=floor(ncdf/10); ncdf=rem(ncdf,10);
end

if ncdf > 1, TimeT0=clock; end

if rem(ncdf,2) == 0,
%- load MDSIO grid files :
if ktyp < 2,
 xC=rdmds(fullfile(rDir,'XC'));
 yC=rdmds(fullfile(rDir,'YC'));
 xG=rdmds(fullfile(rDir,'XG'));
 yG=rdmds(fullfile(rDir,'YG'));

 dXc=rdmds(fullfile(rDir,'DXC'));
 dYc=rdmds(fullfile(rDir,'DYC'));
 dXg=rdmds(fullfile(rDir,'DXG'));
 dYg=rdmds(fullfile(rDir,'DYG'));

 rAc=rdmds(fullfile(rDir,'RAC'));
 rAw=rdmds(fullfile(rDir,'RAW'));
 rAs=rdmds(fullfile(rDir,'RAS'));
 rAz=rdmds(fullfile(rDir,'RAZ'));

%- load grid orientation relative to West-East / South-North dir:
 if isempty(dir(fullfile(rDir,'AngleCS.*'))),
  fprintf(' no grid orientation (Cos & Sin) file to load ');
  csAngle= ones(size(rAc));
  snAngle=zeros(size(rAc));
  fprintf(' => set COS=1,SIN=0\n');
 else
  csAngle=rdmds(fullfile(rDir,'AngleCS'));
  snAngle=rdmds(fullfile(rDir,'AngleSN'));
 end
 dims = size(rAc);
end

if rem(ktyp,2) == 0,
 rC=rdmds(fullfile(rDir,'RC')); rC=squeeze(rC);
 rF=rdmds(fullfile(rDir,'RF')); rF=squeeze(rF);
 dRc=rdmds(fullfile(rDir,'DRC')); dRc=squeeze(dRc);
 dRf=rdmds(fullfile(rDir,'DRF')); dRf=squeeze(dRf);
 dims = size(rC);
end

if ktyp == 0,
%- define domain:
 hFacC=rdmds(fullfile(rDir,'hFacC'));
 hFacW=rdmds(fullfile(rDir,'hFacW'));
 hFacS=rdmds(fullfile(rDir,'hFacS'));
 depth=rdmds(fullfile(rDir,'Depth'));
 dims = size(hFacC);
end

 if ncdf > 1, TimeT1=clock; end

else
%- load NetCDF grid files :
%S=rdmnc([rDir,'grid.*.nc']);
 S=rdmnc(fullfile(rDir,'grid.*.nc'));
 if ncdf > 1, TimeT1=clock; end

%-- 
 xC=S.XC;
 yC=S.YC;
 xG=S.XG(1:end-1,1:end-1);
 yG=S.YG(1:end-1,1:end-1);
 rC=S.RC';
 rF=S.RF';
 dXc=S.dxC(1:end-1,:);
 dYc=S.dyC(:,1:end-1);
 dXg=S.dxG(:,1:end-1);
 dYg=S.dyG(1:end-1,:);
 dRc=S.drC';
 dRf=S.drF';
 rAc=S.rA;
 rAw=S.rAw(1:end-1,:);
 rAs=S.rAs(:,1:end-1);
 rAz=S.rAz(1:end-1,1:end-1);
 if isfield(S,'AngleCS') & isfield(S,'AngleSN'), 
   csAngle=S.AngleCS;
   snAngle=S.AngleSN;
 else
   fprintf(' no grid orientation (Cos & Sin) in grid file ');
   csAngle= ones(size(rAc));
   snAngle=zeros(size(rAc));
   fprintf(' => set COS=1,SIN=0\n');
 end
 hFacC=S.HFacC;
 hFacW=S.HFacW(1:end-1,:,:);
 hFacS=S.HFacS(:,1:end-1,:);
 depth=S.Depth;
 dims = size(hFacC);
end

if ncdf > 1, fprintf(' (took %6.4f s)\n',etime(TimeT1,TimeT0)); end

if length(dims) == 1, dims(2)=1; end
if length(dims) == 2, dims(3)=1; end

%- 1-D axis:
if nargin < 3,
%- yAxis
  ny=dims(2);
  yAxC=yC(1,:)';
  if rem(ncdf,2) == 0,
    yAxV=[yG(1,:) 2*yC(1,end)-yG(1,end)]';
  else
    yAxV=S.YG(1,:)';
  end
else
  ny=varargin{3};
  dyAx=(max(yG(:))-min(yG(:)))/ny;
  yAxV=min(yG(:))+[0:ny]'*dyAx;
  yAxC=(yAxV(1:ny)+yAxV(2:ny+1))/2;
end
if nargin < 4,
%- xAxis
  nx=dims(1);
  xAxC=xC(:,1);
  if rem(ncdf,2) == 0,
    xAxU=[xG(:,1)' 2*xC(end,1)-xG(end,1)]';
  else
    xAxU=S.XG(:,1);
  end
else
  nx=varargin{4};
  dxAx=max(max(xG(:)),max(xC(:)))-min(min(xG(:)),min(xC(:)));
  if dxAx > 360*(1-1/nx), dxAx=360; end
  dxAx=dxAx/nx;
  xAxU=min(xG(:))+[0:nx]'*dxAx;
  xAxC=(xAxU(1:nx)+xAxU(2:nx+1))/2;
end

%- volume:
if rem(ncdf,2) == 1 | ktyp == 0,
%fprintf(' rAc:'); fprintf(' %i',size(rAc)); fprintf('\n');
%fprintf('dims:'); fprintf(' %i',dims); fprintf('\n');
%fprintf(' dRf:'); fprintf(' %i',size(dRf)); fprintf('\n');
%vol=repmat(rAc,[1 1 dims(3)]).*hFacC;
 vol=reshape(rAc,[dims(1)*dims(2) 1])*dRf'; vol=reshape(vol,dims).*hFacC;
end

% create the structure

if rem(ncdf,2) == 1 | ktyp == 0,
G = struct('dims',dims, ...
    'nx',nx,'ny',ny,'xAxC',xAxC,'yAxC',yAxC,'xAxU',xAxU,'yAxV',yAxV, ...
    'xC',xC,'yC',yC,'xG',xG,'yG',yG,'rC',rC,'rF',rF, ...
    'dXc',dXc,'dYc',dYc,'dXg',dXg,'dYg',dYg,'dRc',dRc,'dRf',dRf, ...
    'rAc',rAc,'rAw',rAw,'rAs',rAs,'rAz',rAz, ...
    'csAngle',csAngle,'snAngle',snAngle, ...
    'hFacC',hFacC,'hFacW',hFacW,'hFacS',hFacS,'depth',depth,'vol',vol);
elseif ktyp == 1,
G = struct('dims',dims, ...
    'nx',nx,'ny',ny,'xAxC',xAxC,'yAxC',yAxC,'xAxU',xAxU,'yAxV',yAxV, ...
    'xC',xC,'yC',yC,'xG',xG,'yG',yG, ...
    'dXc',dXc,'dYc',dYc,'dXg',dXg,'dYg',dYg, ...
    'rAc',rAc,'rAw',rAw,'rAs',rAs,'rAz',rAz, ...
    'csAngle',csAngle,'snAngle',snAngle);
else
G = struct('dims',dims, ...
    'rC',rC,'rF',rF, ...
    'dRc',dRc,'dRf',dRf);
end

return
