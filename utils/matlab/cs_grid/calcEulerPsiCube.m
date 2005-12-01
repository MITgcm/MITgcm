function [psi,mskG,ylat] = calcEulerPsiCube(varargin);

% [psi,mskG,ylat] = calcEulerPsiCube(d,g,flu,rstar,blkFile,[optional]);
% [psi,mskG,ylat] = calcEulerPsiCube(d,g,flu,rstar,blkFile,'grav',9.81);
%
% Input arguements:  
%   The incoming field data (d) and grid data (g) must be in a structured
%   array format (which is the format that comes from rdmnc):
%       d  [Field data]  hUtave,hVtave OR uVeltave,vVeltave
%       g  [Grid data ]  drF,dxG,dyG,HFacW,HFacS
%   Other input parameters:
%       flu      (str)  'O' or 'A' for ocean or atmosphere
%       rstar    (str)  0 or 1 if you are using r* coordinates or not
%       blkFile  (str)  Broken line file ('isoLat_cs32_59.mat')
%   Optional parameters:
%       'grav'   (num)  Acceleration due to gravity (default 9.81);
%
% Output fields:
%   psi     Overturning (eg [61,6,nt])
%   mskG    Land mask (eg [60,5])
%   ylat    Latitude coordinate of psi (eg [61,1])
%
% Description:
%   Caculates overturning stream function (psi).  For the atmosphere, data
%   is must be in p-coordinates and the output is the mass transport psi
%   [10^9 kg/s].  For the ocean, data should be in z-coordinates and the
%   output is the volume transport psi [10^6 m^3/s = Sv].  If the rstar
%   parameters is on, hu and hv are used, if off, the hfacw*.u and hfacs*.v
%   are used (the multiplication being done inside the function).
%
%   'psi' is tabulated on broken lines at the interface between cells in
%   the vertical.  'mskG' is for the area between broken lines and between
%   the cell interfaces in the vertical.
%
% Original Author:  Jean-Michel Campin
% Modifications:  Daniel Enderton

% Defaults that can be overriden.
grav = 9.81;

% Read input parameters.
d = varargin{1};
g = varargin{2};
flu = varargin{3};
rstar = varargin{4};
blkFile = varargin{5};
for ii = 6:2:length(varargin)
    temp = varargin{ii+1}; eval([varargin{ii},' = temp;']);
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     Prepare / reform  incoming data                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nc = size(g.XC,2);
nr = length(g.drF);

delM = g.drF;
hw = reshape(g.HFacW(1:6*nc,1:nc,1:nr),[6*nc*nc,nr]);
hs = reshape(g.HFacS(1:6*nc,1:nc,1:nr),[6*nc*nc,nr]);
dxg = reshape(g.dxG(1:6*nc,1:nc),[6*nc*nc,1]).*1e-6;
dyg = reshape(g.dyG(1:6*nc,1:nc),[6*nc*nc,1]).*1e-6;
if rstar
    nt = size(d.hUtave,4);
    hu = reshape(d.hUtave(1:6*nc,1:nc,1:nr,1:nt),[6*nc*nc,nr,nt]);
    hv = reshape(d.hVtave(1:6*nc,1:nc,1:nr,1:nt),[6*nc*nc,nr,nt]);
else
    nt = size(d.uVeltave,4);
    hu = reshape(d.uVeltave(1:6*nc,1:nc,1:nr,1:nt),[6*nc*nc,nr,nt]);
    hv = reshape(d.vVeltave(1:6*nc,1:nc,1:nr,1:nt),[6*nc*nc,nr,nt]);
    for it = 1:nt
        hu(:,:,it) = hw.*hu(:,:,it);
        hv(:,:,it) = hs.*hv(:,:,it);
    end
end

% Load broken information.
% I looked at calc_psiH_CS.m, and did not find it very clear.
% May be you can try to see what is in 
% MITgcm/utils/matlab/cs_grid/bk_line/use_psiLine.m
% it's shorter, and slightly better.
load(blkFile);
ydim = length(bkl_Ylat);
ylat = [-90,bkl_Ylat,90];

% For the volume transport through a cell face, the area of the face times
% the velocity through the face is calculated.  For the ocean, the area is
% calculated simply by the vertical cell length times the horizontal cell
% length.  In the atmosphere, pressure coordinates are used.  Using the
% hydrostatic relationship, delM = rho*dz = -dp/g.  by leaving the rho on
% the side of the dz and using this for the vertical cell length the volume
% transport is converted to a mass transport as desired.  Note the the
% additional factor of 1e-3 is the difference between the 10^6 and 10^9
% used for the ocean and atmosphere, respectively.  The 1e-6 is applied at
% the end for the final conversion into the desired units.
if isequal(flu,'A'), delM = -(1e-3).*delM./grav; end

% kMsep=1;
% if (nargin < 6), kfac=0;
% else kfac=1; end;
nBas=0;

% Load basin information.
% if nBas > 0,
%     % Load ocean basin mask (& separation line):
%     mskBw=rdda([rac,'maskW_bas.bin'],[6*nc*nc 3],1,'real*4','b');
%     mskBs=rdda([rac,'maskS_bas.bin'],[6*nc*nc 3],1,'real*4','b');
%     if nBas==2, 
%         mskBw(:,2)=mskBw(:,2)+mskBw(:,3);
%         mskBs(:,2)=mskBs(:,2)+mskBs(:,3);
%         mskBw=min(1,mskBw); mskBs=min(1,mskBs);
%     end
%     %- load: np_Sep, ij_Sep, tp_Sep:
%     sep_lineF=[rac,'sepBas_cs32_60'];
%     load(sep_lineF);
% end

% Prepare arrays.
psi = zeros(ydim+2,nr+1,1+nBas,nt); 
mskZ = zeros(ydim+2,nr+1,1+nBas);  % Mask of psi
mskV = zeros(ydim+2,nr,1+nBas);    % Mask of the merid. transport
mskG = zeros(ydim+1,nr,1+nBas);    % Mask of the ground

% The variable "bkl_Flg" is -1/1 if edge (on a given broken) has a u point
% and -2/2 if it has a v point.  Positive/negative values contribute
% positively/negatively to northward heat transport (this depends on the
% oreientation of the cell).  A zero value indicates an end of edges that
% contribute to a broken line.  The u and v information is parced into two
% seperate fields, ufac and vfac (-2/2 are reduced to -1/1 for vfac).
ufac = zeros([size(bkl_Flg),1+nBas]);
vfac = zeros([size(bkl_Flg),1+nBas]);
ufac(:,:,1) = rem(bkl_Flg,2);
vfac(:,:,1) = fix(bkl_Flg/2);
% for jl=1:ydim,
%     ie=bkl_Npts(jl);
%     for b=1:nBas, 
%         ufac(1:ie,jl,1+b)=mskBw(bkl_IJuv(1:ie,jl),b).*ufac(1:ie,jl,1);
%         vfac(1:ie,jl,1+b)=mskBs(bkl_IJuv(1:ie,jl),b).*vfac(1:ie,jl,1);
%     end;
% end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   Compute mass/volume stream function                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Compute volume transport through broken lines a hence psi.  ut/vt is the
%  velocity times the edge length it is passing through.  The sum of this
%  quantity along a broken line (vz) times the cell height is the volume
%  transport through broken line at one layer (delM(k)*vz).  psi is then
%  the value of the volume transport through the level above subtracted
%  from the value of psi above.
for it = 1:nt
    for k = nr:-1:1
        ut = dyg.*hu(:,k,it);
        vt = dxg.*hv(:,k,it); 
        for jl = 1:ydim
            ie = bkl_Npts(jl);
            for b = 1:1+nBas
                vz = sum(   ufac(1:ie,jl,b).*ut(bkl_IJuv(1:ie,jl)) ...
                          + vfac(1:ie,jl,b).*vt(bkl_IJuv(1:ie,jl)) );
                psi(jl+1,k,b,it) = psi(jl+1,k+1,b,it) - delM(k)*vz;
            end
        end
    end
end

% % Compute the mask :
% if kfac == 1, 
%     ufac=abs(ufac) ; vfac=abs(vfac);
%     for jl=1:ydim, 
%         ie=bkl_Npts(jl); 
%         hw=zeros(ie,nr); hs=zeros(ie,nr);
%         hw=hw(bkl_IJuv(1:ie,jl),:); % Would need correction!
%         hs=hs(bkl_IJuv(1:ie,jl),:);
%         for b=1:1+nBas,
%             for k=1:nr,
%                 %   for ii=1:bkl_Npts(jl);
%                 %    ij=bkl_IJuv(ii,jl);
%                 %    mskV(jl+1,k,b)=mskV(jl+1,k,b)+ufac(ii,jl,b)*hw(ij,k)+vfac(ii,jl,b)*hs(ij,k);
%                 %   end ;
%                  tmpv=ufac(1:ie,jl,b).*hw(:,k)+vfac(1:ie,jl,b).*hs(:,k);
%                  mskV(jl+1,k,b)=mskV(jl+1,k,b)+max(tmpv);
%             end
%         end
%     end
%     mskV=ceil(mskV); mskV=min(1,mskV);
%     %- build the real mask (=mskG, ground) used to draw the continent with "surf":
%     %   position=centered , dim= ydim+1 x nr
%     mskG=mskV(1:ydim+1,:,:)+mskV(2:ydim+2,:,:); mskG=min(1,mskG);
% 
%     if kMsep & nBas > 0,
%         mskW=1+min(1,ceil(hw));
%         mskS=1+min(1,ceil(hs));
%         for b=1:nBas,
%             bs=b; b1=1+bs; b2=2+rem(bs,nBas);
%             if nBas == 2, bs=b+b-1; b1=2; b2=3 ; end
%             for j=1:ydim+1,
%                 for i=1:np_Sep(bs,j),
%                     ij=ij_Sep(bs,j,i); typ=abs(tp_Sep(bs,j,i));
%                     if typ == 1, 
%                         mskG(j,:,b1)=mskG(j,:,b1).*mskW(ij,:);
%                         mskG(j,:,b2)=mskG(j,:,b2).*mskW(ij,:);
%                     elseif typ == 2,
%                         mskG(j,:,b1)=mskG(j,:,b1).*mskS(ij,:);
%                         mskG(j,:,b2)=mskG(j,:,b2).*mskS(ij,:);
%                     end
%                 end
%             end
%         end
%         mskG=min(2,mskG);
%     end
% 
%     %- to keep psi=0 on top & bottom
%     mskZ(:,[2:nr+1],:)=mskV; 
%     mskZ(:,[1:nr],:)=mskZ(:,[1:nr],:)+mskV;
%     %- to keep psi=0 on lateral boundaries :
%     mskZ([1:ydim],:,:)=mskZ([1:ydim],:,:)+mskZ([2:ydim+1],:,:);
%     mskZ([2:ydim+1],:,:)=mskZ([2:ydim+1],:,:)+mskZ([3:ydim+2],:,:);
%     mskZ=ceil(mskZ); mskZ=min(1,mskZ);
%     if kMsep & nBas > 0,
%         mskM=zeros(ydim+2,nr,1+nBas); mskM(2:ydim+2,:,:)=min(2-mskG,1); 
%         mskM(1:ydim+1,:,:)=mskM(1:ydim+1,:,:)+mskM(2:ydim+2,:,:);
%         mskZ(:,1:nr,:)=min(mskZ(:,1:nr,:),mskM); 
%     end
%     %- apply the mask (and remove dim = 1) :
%     if nt == 1,
%         psi=squeeze(psi); mskV=squeeze(mskV); mskZ=squeeze(mskZ);
%         psi( find(mskZ==0) )=NaN ;
%     else
%         for nt=1:nt,
%             psi1=psi(:,:,:,nt); psi1( find(mskZ==0) )=NaN ; psi(:,:,:,nt)=psi1;
%         end
%         if nBas < 1, psi=squeeze(psi); mskV=squeeze(mskV); mskZ=squeeze(mskZ); end
%     end
% else
%     if nBas < 1 | nt == 1, psi=squeeze(psi); end
% end

psi = squeeze(psi);
