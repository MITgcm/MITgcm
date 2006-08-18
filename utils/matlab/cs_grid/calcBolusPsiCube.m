function [PsiB,ylat]=calcBolusPsiCube(d,g,GMform,blkFile);

% [PsiB,ylat]=calcBolusPsiCube(d,g,GMform,blkFile);
%
% Compute bolus streamfunction from GM scheme
%
% Input arguments:  
%   The incoming field data (d) and grid data (g) must be in a structured
%   array format (which is the format that comes from rdmnc):
%       d       [Field data]  Kwx,Kwy
%       g       [Grid data ]  drF,rA,dxC,dyC,dxG,dyG,HFacW,HFacS
%       GMform  [string]      GM form 'Skew' or 'Advc'
%       blkFile [file name]   Broken line file  
% Output arguments:
%       PsiB : bolus streamfunction at interface level (in Sv) 
%       ylat : meridional coordinate of PsiB
%
% Comments:
%   -For Skew-flux form:
%        PsiB computed from Kwx & Kwy divided by 2. 
%        first average Kwx and Kwy at u- and v-points:
%        psiX=(rAc*Kwx)_i / (dXc*dYg) ; psiY=(rAc*Kwy)_j / dYc ;
%        and then "zonally" average along broken lines
%   -For Advective form:
%        PsiB computed from PsiX and PsiY
%        just need to "zonally" average along broken lines
%
%---------------------------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     Prepare grid stuff                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nc = size(g.XC,2);
nr = length(g.drF);
nt = size(d.GM_Kwx,4);

%--- areas :
ra  = g.rA;
dxc = reshape(g.dxC(1:6*nc,1:nc),[6*nc*nc,1]);
dyc = reshape(g.dyC(1:6*nc,1:nc),[6*nc*nc,1]);
dxg = reshape(g.dxG(1:6*nc,1:nc),[6*nc*nc,1]);
dyg = reshape(g.dyG(1:6*nc,1:nc),[6*nc*nc,1]);

rAu=dxc.*dyg;
rAv=dyc.*dxg;

%--- masks :
hw = reshape(g.HFacW(1:6*nc,1:nc,1:nr),[6*nc*nc,nr]);
hs = reshape(g.HFacS(1:6*nc,1:nc,1:nr),[6*nc*nc,nr]);
mskw=ceil(hw); mskw=min(1,mskw);
msks=ceil(hs); msks=min(1,msks);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     Read/Prepare GM fields                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
psiX_all = zeros(6*nc*nc,nr,nt);
psiY_all = zeros(6*nc*nc,nr,nt);

switch GMform
case 'Skew'

kwx_all = 0.5*d.GM_Kwx;
kwy_all = 0.5*d.GM_Kwy;

for it = 1:nt
    kwx = kwx_all(:,:,:,it);
    kwy = kwy_all(:,:,:,it);

    %-- K*ra + add 1 overlap :
    kwx = repmat(ra,[1 1 nr]).*kwx;
    kwy = repmat(ra,[1 1 nr]).*kwy;
    v6X = split_C_cub(kwx,1);
    v6Y = split_C_cub(kwy,1);
    k6x = v6X(:,[2:nc+1],:,:);
    k6y = v6Y([2:nc+1],:,:,:);

    %----------------- 
    clear v6X v6Y
    v6X = (k6x([2:nc+1],:,:,:) + k6x([1:nc],:,:,:))/2;
    v6Y = (k6y(:,[2:nc+1],:,:) + k6y(:,[1:nc],:,:))/2;

    psiX = zeros(6*nc,nc,nr);
    psiY = zeros(6*nc,nc,nr);

    for n = 1:6
        is = 1+nc*(n-1);ie=nc*n;
        psiX([is:ie],[1:nc],[1:nr]) = v6X(:,:,:,n);
        psiY([is:ie],[1:nc],[1:nr]) = v6Y(:,:,:,n);
    end

    psiX = reshape(psiX,6*nc*nc,nr);
    psiY = reshape(psiY,6*nc*nc,nr);

    psiX_all(:,:,it) = mskw .* psiX ./ repmat(rAu,[1,nr]);
    psiY_all(:,:,it) = msks .* psiY ./ repmat(rAv,[1,nr]);

end

case 'Advc'

psiX_all = reshape(d.GM_PsiX(1:6*nc,:,:,1:nt),6*nc*nc,nr,nt);
psiY_all = reshape(d.GM_PsiY(:,1:nc,:,1:nt)  ,6*nc*nc,nr,nt);

otherwise 
    disp(['C est Portnawak: GMform should be Skew or Advc: ',GMform])
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     Zonally integrate along broken lines                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load(blkFile);
ydim = length(bkl_Ylat);
ylat = [-90,bkl_Ylat,90];
ufac= rem(bkl_Flg,2);
vfac= fix(bkl_Flg/2);

PsiB= zeros(ydim+2,nr+1,nt);

for it = 1:nt
    for k = 1:nr
        psixt=dyg.*psiX_all(:,k,it);
        psiyt=dxg.*psiY_all(:,k,it);
        for jl = 1:ydim
            ie = bkl_Npts(jl);
            PsiB(jl+1,k,it) = sum( ufac(1:ie,jl).*psixt(bkl_IJuv(1:ie,jl)) ...
                      + vfac(1:ie,jl).*psiyt(bkl_IJuv(1:ie,jl)) );
        end
    end
end
PsiB = 1e-6*PsiB;
